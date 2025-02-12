#include "compile.h"
#include "basics.h"
#include "parse.h"
#include <format>

AstDecl *find_decl(AstBlock *block, std::string_view name)
{
    for (auto statement : block->statements)
    {
        if (statement->kind != AST_DECL)
            continue;

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

/* size_t absolute_address(AstBlock *block) { */
/*     auto result = block->base_address; */

/*     if (block->parent_block != nullptr) { */
/*         result += absolute_address(block->parent_block); */
/*     } */

/*     return result; */
/* } */

/* size_t absolute_address(AstDecl *decl) { */
/*     assert(decl->block != nullptr); */

/*     auto result = absolute_address(decl->block); */
/*     result += decl->block_offset; */

/*     return result; */
/* } */

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
        }
            return true;

        case AST_BLOCK:
        {
            auto block = static_cast<AstBlock *>(node);

            auto is_global = block->parent_block == nullptr;

            // TODO: Nested blocks for if-statements and raw blocks etc.

            /*
            a: int32 = 1  // -14 | 0
            b: int64 = 2  // -10 | 4
            c: int16 = 3  // -2  | 12
            RSP = 14
            */

            // Allocate locals
            if (is_global == false)  // Only if we are not compiling the top level program pseudo-block
            {
                int64_t cursor = 0;
                for (auto i = 0; i < block->statements.size(); ++i)
                {
                    auto statement = block->statements[block->statements.size() - 1 - i];
                    if (statement->kind != AST_DECL)
                        continue;

                    auto decl = static_cast<AstDecl *>(statement);

                    assert(decl->is_global == is_global);

                    auto size_of_decl = 8;  // TODO
                    cursor -= size_of_decl;

                    decl->address = cursor;
                }

                write_op_64(ADDRSP, -cursor, w);
            }

            auto prev_block  = w->current_block;
            w->current_block = block;
            defer
            {
                w->current_block = prev_block;
            };

            for (auto statement : block->statements)
            {
                if (generate_code(statement, w) == false)
                {
                    return false;
                }
            }

            return true;
        }

        case AST_DECL:
        {
            auto decl = static_cast<AstDecl *>(node);

            assert((decl->is_global == false) == (decl->address < 0));

        /* assert(decl->is_global == (w->current_block == nullptr)); */

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

            if (is_expression(ret->expr->kind) == false)
            {
                assert(false);
            }

            if (generate_code(ret->expr, w) == false)
            {
                return false;
            }

            write_op(RET, w);

            return true;
        }

        default: assert(false);
    }

    return false;
}
