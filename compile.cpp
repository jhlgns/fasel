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

std::vector<AstBlock *> get_child_blocks(AstNode *node)
{
    switch (node->kind)
    {
        case AST_BLOCK: return {static_cast<AstBlock *>(node)};
        // TODO: Handle AST_IF, AST_WHILE etc. once they are implemented
        default: return {};
    }
}

void calculate_allocations(AstBlock *block, int64_t root_block_size = -1)
{
    // TODO: Offset by block offset from parent block

    /*
    a: int32 = 1  // RSP - 20 | 0
    b: int64 = 2  // RSP - 16 | 4
    c: int16 = 3  // RSP - 8  | 12
    {  // base = 12
        d: int32 = 4  // RSP - 4 | 16
    }
    {  // base = 12
        e: int32 = 5  // RSP - 4 | 16
    }
    RSP = 20
    */

    // Calculate addresses of this block's declarations
    int64_t cursor = 0;
    for (auto statement : block->statements)
    {
        if (statement->kind != AST_DECL)
        {
            continue;
        }

        auto decl = static_cast<AstDecl *>(statement);

        decl->address = block->offset_from_parent_block + cursor;

        auto size_of_decl = 8;  // TODO
        cursor += size_of_decl;
    }

    assert(block->size >= cursor);

    if (root_block_size == -1)
    {
        root_block_size = block->size;
    }

    // Calculate allocations of child blocks
    for (auto statement : block->statements)
    {
        auto child_blocks = get_child_blocks(statement);
        for (auto child_block : child_blocks)
        {
            child_block->offset_from_parent_block = cursor;
            calculate_allocations(child_block, root_block_size);
        }
    }

    // Now make the addresses of the declarations relative to the stack pointer
    for (auto statement : block->statements)
    {
        if (statement->kind != AST_DECL)
        {
            continue;
        }

        auto decl = static_cast<AstDecl *>(statement);

        decl->address -= root_block_size;
    }
}

void calculate_size(AstBlock *block)
{
    assert(block->size == 0);

    auto is_global = block->parent_block == nullptr;
    assert(is_global == false);

    // Calculate the total size of all local declarations
    for (auto statement : block->statements)
    {
        if (statement->kind != AST_DECL)
        {
            continue;
        }

        auto decl = static_cast<AstDecl *>(statement);
        assert(decl->is_global == is_global);

        auto size_of_decl = 8;  // TODO
        block->size += size_of_decl;
    }

    // Calculate sizes of child blocks and get the largest child block size
    int64_t max_child_block_size = 0;
    for (auto statement : block->statements)
    {
        auto child_blocks = get_child_blocks(statement);
        for (auto child_block : child_blocks)
        {
            calculate_size(child_block);

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
            /*
            main := proc() {
                // a has ofset -16 from RSP while calling main
                a := 0

                // b has offset -8 from RSP
                b := 123

                if true {
                    c := 532

                    // a found in parent block
                    a = 8
                } else {
                    a := 8
                }
            }
            */

            auto ident = static_cast<AstIdent *>(node);

            auto decl = find_decl(w->current_block, text_of(&ident->ident));
            if (decl == nullptr)
            {
                compile_error(w, std::format("Declaration of {} not found", text_of(&ident->ident)));
                return false;
            }

            assert(decl->address <= 0);  // TODO: This is not right for proc calls and for global variables

            write_op_64(LOADR, decl->address, w);
            /* write64(decl->, w); */  // TODO

            return true;
        }

        case AST_BLOCK:
        {
            auto block = static_cast<AstBlock *>(node);

            auto is_global = block->parent_block == nullptr;

            // Allocate locals
            if (is_global == false)  // Only if we are not compiling the top level program pseudo-block
            {
                calculate_size(block);
                calculate_allocations(block);
                write_op_64(ADDRSP, block->size, w);
            }

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

                got_return = statement->kind == AST_RETURN;
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

            /* assert((decl->is_global == false) == (decl->address < 0)); */

            if (decl->is_global)
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

                write_op_64(STORER, decl->address, w);
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
            // TODO!
            auto proc = static_cast<AstProc *>(node);
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
