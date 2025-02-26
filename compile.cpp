#include "compile.h"
#include "basics.h"
#include "parse.h"
#include <format>

AstDecl *find_decl(AstBlock *block, std::string_view name)
{
    for (auto statement : block->statements)
    {
        if (statement->kind != AST_DECL)
        {
            continue;
        }

        auto decl = static_cast<AstDecl *>(statement);
        if (text_of(&decl->ident) == name)
        {
            return decl;
        }
    }

    if (block->parent_block != nullptr)
    {
        return find_decl(block->parent_block, name);
    }

    return nullptr;
}

// TODO: We have to differentiate if we want to include the node itself
// if it is a block...
std::vector<AstBlock *> get_child_blocks(AstNode *node)
{
    switch (node->kind)
    {
        case AST_BLOCK:
        {
            auto block = static_cast<AstBlock *>(node);
            return {block};
        }

        // TODO: Handle AST_IF, AST_WHILE etc. once they are implemented
        default: return {};
    }
}

void allocate_locals(AstBlock *block)
{
    assert(block->size == 0);
    assert(block->is_global() == false);

    for (auto statement : block->statements)
    {
        if (statement->kind != AST_DECL)
        {
            continue;
        }

        auto decl = static_cast<AstDecl *>(statement);

        auto size_of_decl = 8;  // TODO
        decl->address     = block->offset_from_parent_block + block->size;
        block->size += size_of_decl;
    }

    // Calculate sizes of child blocks and get the largest child block size
    int64_t max_child_block_size = 0;
    for (auto statement : block->statements)
    {
        auto child_blocks = get_child_blocks(statement);
        for (auto child_block : child_blocks)
        {
            child_block->offset_from_parent_block = block->offset_from_parent_block + block->size;

            allocate_locals(child_block);

            if (child_block->size > max_child_block_size)
            {
                max_child_block_size = child_block->size;
            }
        }
    }

    block->size += max_child_block_size;
}

void write(uint8_t *data, size_t len, BytecodeWriter *w)
{
    w->bytecode.insert(w->bytecode.end(), data, data + len);
}

void _write8(uint8_t value, BytecodeWriter *w)
{
    write(&value, sizeof(value), w);
}

void _write64(int64_t value, BytecodeWriter *w)
{
    write((uint8_t *)&value, sizeof(value), w);
}

void write_op(OpCode op, BytecodeWriter *w)
{
    if (w->generate_asm)
    {
        w->asm_source += std::format("{}\n", to_string(op));
        if (w->also_generate_bytecode == false)
        {
            return;
        }
    }

    _write8(op, w);
}

void write_op_8(OpCode op, uint8_t value, BytecodeWriter *w)
{
    if (w->generate_asm)
    {
        w->asm_source += std::format("{} {}\n", to_string(op), value);
        if (w->also_generate_bytecode == false)
        {
            return;
        }
    }

    _write8(op, w);
    _write8(value, w);
}

void write_op_64(OpCode op, int64_t value, BytecodeWriter *w)
{
    if (w->generate_asm)
    {
        w->asm_source += std::format("{} {}\n", to_string(op), value);
        if (w->also_generate_bytecode == false)
        {
            return;
        }
    }

    _write8(op, w);
    _write64(value, w);
}

void compile_error(BytecodeWriter *w, std::string_view message)
{
    printf("Compiler error: %s\n", message.data());
}

bool is_expression(AstKind kind)
{
    return kind == AST_BIN_OP || kind == AST_IDENT || kind == AST_LITERAL || kind == AST_PROC_CALL;
}

[[nodiscard]] bool generate_code(AstNode *node, BytecodeWriter *w);

[[nodiscard]] bool generate_expr(AstNode *node, BytecodeWriter *w)
{
    if (is_expression(node->kind) == false)
    {
        return false;
    }

    return generate_code(node, w);
}

[[nodiscard]] bool generate_code(AstNode *node, BytecodeWriter *w)
{
    switch (node->kind)
    {
        case AST_BIN_OP:
        {
            auto binop = static_cast<AstBinOp *>(node);

            if (generate_expr(binop->lhs, w) == false || generate_expr(binop->rhs, w) == false)
            {
                return false;
            }

            switch (binop->binop)
            {
                case TOK_ASTERISK: write_op(MUL, w); break;
                case TOK_SLASH:    write_op(DIV, w); break;
                case TOK_MOD:      write_op(MOD, w); break;
                case TOK_PLUS:     write_op(ADD, w); break;
                case TOK_MINUS:    write_op(SUB, w); break;
                default:           assert(false); break;
            }

            return true;
        }

        case AST_IDENT:
        {
            auto ident = static_cast<AstIdent *>(node);

            auto decl = find_decl(w->current_block, text_of(&ident->ident));
            if (decl == nullptr)
            {
                compile_error(w, std::format("Declaration of {} not found", text_of(&ident->ident)));
                return false;
            }

            auto address_relative_to_rsp = decl->address - decl->proc->body.size;
            write_op_64(LOADR, address_relative_to_rsp, w);
            /* write64(decl->, w); */  // TODO

            return true;
        }

        case AST_BLOCK:
        {
            auto block = static_cast<AstBlock *>(node);

            auto prev_block  = w->current_block;
            w->current_block = block;
            defer
            {
                w->current_block = prev_block;
            };

            auto got_return = false;
            for (auto statement : block->statements)
            {
                if (generate_code(statement, w) == false)
                {
                    return false;
                }

                if (got_return == false && statement->kind == AST_RETURN)
                {
                    got_return = true;
                }
            }

            if (block->is_proc_body && got_return == false)
            {
                // Write implicit return - NOTE: will cause stack corruption
                // for functions that are supposed to return a value but don't
                write_op(RET, w);
            }

            return true;
        }

        case AST_DECL:
        {
            auto decl = static_cast<AstDecl *>(node);

            assert(decl->proc == nullptr);
            decl->proc = w->current_proc;

            /* assert((decl->is_global == false) == (decl->address < 0)); */

            if (w->current_block->is_global())
            {
                auto address = w->bytecode.size();
                if (generate_code(decl->init_expr, w) == false)
                {
                    return false;
                }

                decl->address = address;
            }
            else
            {
                if (generate_expr(decl->init_expr, w) == false)
                {
                    return false;
                }

                auto address_relative_to_rsp = decl->address - decl->proc->body.size;
                write_op_64(STORER, address_relative_to_rsp, w);
            }

            return true;
        }

        case AST_LITERAL:
        {
            auto literal = static_cast<AstLiteral *>(node);

            assert(literal->type == LIT_INT);  // TODO
            write_op_64(PUSHC, literal->int_value, w);

            return true;
        }

        case AST_ARG:
        {
            assert(false);
        }

        case AST_PROC:
        {
            auto proc = static_cast<AstProc *>(node);

            auto prev_proc  = w->current_proc;
            w->current_proc = proc;
            defer
            {
                w->current_proc = prev_proc;
            };

            allocate_locals(&proc->body);

            // Make stack space for the locals
            write_op_64(ADDRSP, proc->body.size, w);

            return generate_code(&proc->body, w);
        }

        case AST_PROC_CALL:
        {
            assert(false);
        }

        case AST_PROC_SIGNATURE:
        {
            assert(false);
        }

        case AST_PROGRAM:
        {
            auto program = static_cast<AstProgram *>(node);

            for (auto node : program->block.statements)
            {
                if (node->kind != AST_DECL)
                {
                    compile_error(w, "Expected declaration");
                    return false;
                }

                return generate_code(&program->block, w);
            }

            return true;
        }

        case AST_RETURN:
        {
            auto ret = static_cast<AstReturn *>(node);

            if (generate_expr(ret->expr, w) == false)
            {
                return false;
            }

            if (w->current_block->size != 0)
            {
                // Deallocate the locals stack space
                write_op_64(ADDRSP, -w->current_block->size, w);
            }

            write_op(RET, w);

            return true;
        }

        default: assert(false);
    }

    return false;
}
