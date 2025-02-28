#include "compile.h"
#include "basics.h"
#include "parse.h"
#include <format>

std::vector<AstBlock *> get_statement_child_blocks(AstNode *node)
{
    if (auto block = ast_cast<AstBlock>(node))
    {
        return {block};
    }

    if (auto if_ = ast_cast<AstIf>(node))
    {
        return {&if_->true_block, &if_->false_block};
    }

    // TODO: Handle AST_IF, AST_WHILE etc. once they are implemented
    return {};
}

void allocate_locals(AstBlock *block)
{
    assert(block->size == 0);
    assert(block->is_global() == false);

    for (auto statement : block->statements)
    {
        if (auto decl = ast_cast<AstDecl>(statement))
        {
            auto size_of_decl = 8;  // TODO
            decl->address     = block->offset_from_parent_block + block->size;
            // for (auto current = block->parent_block; current != nullptr; current = current->parent_block)
            // {
            //     decl->address += current->offset_from_parent_block;
            // }

            block->size += size_of_decl;
        }
    }

    // Calculate sizes of child blocks and get the largest child block size
    int64_t max_child_block_size = 0;
    for (auto statement : block->statements)
    {
        auto child_blocks = get_statement_child_blocks(statement);
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
    if (w->pos + len > w->bytecode.size())
    {
        w->bytecode.resize(w->pos + len);
    }

    memcpy(&w->bytecode[w->pos], data, len);
    w->pos += len;
}

void _write8(uint8_t value, BytecodeWriter *w)
{
    write(&value, sizeof(value), w);
}

void _write64(int64_t value, BytecodeWriter *w)
{
    write((uint8_t *)&value, sizeof(value), w);
}

int64_t write_op(OpCode op, BytecodeWriter *w)
{
    auto addr = w->pos;

    if (w->generate_asm)
    {
        w->asm_source += std::format("{}\n", to_string(op));
        if (w->also_generate_bytecode == false)
        {
            return addr;
        }
    }

    _write8(op, w);

    return addr;
}

int64_t write_op_8(OpCode op, uint8_t value, BytecodeWriter *w)
{
    auto addr = w->pos;

    if (w->generate_asm)
    {
        w->asm_source += std::format("{} {}\n", to_string(op), value);
        if (w->also_generate_bytecode == false)
        {
            return addr;
        }
    }

    _write8(op, w);
    _write8(value, w);

    return addr;
}

int64_t write_op_64(OpCode op, int64_t value, BytecodeWriter *w)
{
    auto addr = w->pos;

    if (w->generate_asm)
    {
        w->asm_source += std::format("{} {}\n", to_string(op), value);
        if (w->also_generate_bytecode == false)
        {
            return addr;
        }
    }

    _write8(op, w);
    _write64(value, w);

    return addr;
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
    if (auto bin_op = ast_cast<AstBinOp>(node))
    {
        if (bin_op->type == TOK_AND)
        {
            if (generate_expr(bin_op->lhs, w) == false)
            {
                return false;
            }

            auto jmp0_false_pos_1 = w->pos;
            write_op_64(JMP0, 333, w);

            if (generate_expr(bin_op->rhs, w) == false)
            {
                return false;
            }

            auto jmp0_false_pos_2 = w->pos;
            write_op_64(JMP0, 333, w);

            write_op_64(PUSHC, 1, w);
            auto jmp_done_pos = w->pos;
            write_op_64(JMP, 333, w);

            auto false_label = w->pos;
            write_op_64(PUSHC, 0, w);
            auto done_label = w->pos;

            w->pos = jmp0_false_pos_1;
            write_op_64(JMP0, false_label, w);
            w->pos = jmp0_false_pos_2;
            write_op_64(JMP0, false_label, w);
            w->pos = jmp_done_pos;
            write_op_64(JMP, done_label, w);

            w->pos = done_label;

            return true;
        }

        if (bin_op->type == TOK_OR)
        {
            if (generate_expr(bin_op->lhs, w) == false)
            {
                return false;
            }

            auto jmp1_true_pos_1 = w->pos;
            write_op_64(JMP1, 333, w);

            if (generate_expr(bin_op->rhs, w) == false)
            {
                return false;
            }

            auto jmp1_true_pos_2 = w->pos;
            write_op_64(JMP1, 333, w);

            write_op_64(PUSHC, 0, w);
            auto jmp_done_pos = w->pos;
            write_op_64(JMP, 333, w);

            auto true_label = w->pos;
            write_op_64(PUSHC, 1, w);
            auto done_label = w->pos;

            w->pos = jmp1_true_pos_1;
            write_op_64(JMP1, true_label, w);
            w->pos = jmp1_true_pos_2;
            write_op_64(JMP1, true_label, w);
            w->pos = jmp_done_pos;
            write_op_64(JMP, done_label, w);

            w->pos = done_label;

            return true;
        }

        if (generate_expr(bin_op->lhs, w) == false || generate_expr(bin_op->rhs, w) == false)
        {
            return false;
        }

        switch (bin_op->type)
        {
            case TOK_ASTERISK: write_op(MUL, w); break;
            case TOK_SLASH:    write_op(DIV, w); break;
            case TOK_MOD:      write_op(MOD, w); break;
            case TOK_PLUS:     write_op(ADD, w); break;
            case TOK_MINUS:    write_op(SUB, w); break;

            case TOK_BIT_AND: write_op(BITAND, w); break;
            case TOK_BIT_OR:  write_op(BITOR, w); break;
            case TOK_BIT_XOR: write_op(BITXOR, w); break;

            case TOK_LEFTSHIFT:  write_op(LSH, w); break;
            case TOK_RIGHTSHIFT: write_op(RSH, w); break;
            case TOK_EQ:         write_op(CMPEQ, w); break;
            case TOK_NE:         write_op(CMPNE, w); break;
            case TOK_GE:         write_op(CMPGE, w); break;
            case TOK_GT:         write_op(CMPGT, w); break;
            case TOK_LE:         write_op(CMPLE, w); break;
            case TOK_LT:         write_op(CMPLT, w); break;

            default: assert(false); break;
        }

        return true;
    }

    if (auto ident = ast_cast<AstIdent>(node))
    {
        auto decl = w->current_block->find_decl(text_of(&ident->ident));
        if (decl == nullptr)
        {
            compile_error(w, std::format("Declaration of {} not found", text_of(&ident->ident)));
            return false;
        }

        if (decl->enclosing_proc == nullptr)
        {
            write_op_64(PUSHC, decl->address, w);
        }
        else
        {
            auto relative_address = decl->address - decl->enclosing_proc->body.size;
            write_op_64(LOADR, relative_address, w);
        }

        return true;
    }

    if (auto block = ast_cast<AstBlock>(node))
    {
        auto prev_block  = w->current_block;
        w->current_block = block;
        defer
        {
            w->current_block = prev_block;
        };

        auto got_return = false;  // Just do this in the AstProc case and remove the AstBlock case completely?
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

    if (auto decl = ast_cast<AstDecl>(node))
    {
        assert(decl->enclosing_proc == nullptr);
        decl->enclosing_proc = w->current_proc;

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

            auto relative_address = decl->address - decl->enclosing_proc->body.size;
            write_op_64(STORER, relative_address, w);
        }

        return true;
    }

    if (auto literal = ast_cast<AstLiteral>(node))
    {
        assert(literal->type == LIT_INT);  // TODO
        write_op_64(PUSHC, literal->int_value, w);
        return true;
    }

    if (auto proc_call = ast_cast<AstProcCall>(node))
    {
        if (generate_expr(proc_call->proc, w) == false)
        {
            return false;
        }

        write_op(CALL, w);
        return true;
    }

    if (auto proc = ast_cast<AstProc>(node))
    {
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

    if (auto program = ast_cast<AstProgram>(node))
    {
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

    if (auto if_ = ast_cast<AstIf>(node))
    {
        if (generate_expr(if_->condition, w) == false)
        {
            return false;
        }

        auto jmp0_false_pos = w->pos;
        write_op_64(JMP0, 333, w);

        if (generate_code(&if_->true_block, w) == false)
        {
            return false;
        }

        auto false_label = w->pos;
        w->pos           = jmp0_false_pos;
        write_op_64(JMP0, false_label, w);

        w->pos = false_label;

        return true;
    }

    if (auto ret = ast_cast<AstReturn>(node))
    {
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

    assert(false);
    return false;
}
