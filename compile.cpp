#include "compile.h"
#include "basics.h"
#include "disasm.h"
#include "parse.h"
#include <format>
#include <span>

std::string BytecodeWriter::disassemble()
{
    return ::disassemble(std::span{this->bytecode.begin(), this->bytecode.end()});
}

std::vector<AstBlock *> get_statement_child_blocks(AstNode *node)
{
    if (auto block = ast_cast<AstBlock>(node))
    {
        return {block};
    }

    if (auto yf = ast_cast<AstIf>(node))
    {
        return {&yf->then_block, &yf->else_block};
    }

    if (auto proc = ast_cast<AstProcedure>(node))
    {
        return {&proc->body};
    }

    // TODO: Handle AstIf, AstWhile etc. once they are implemented
    return {};
}

void fix_block_hierarchy(AstBlock *root_block)
{
    // TODO: We need a way to break out of the recursion
    visit(
        root_block,
        [root_block](AstNode *node)
        {
            if (auto block = ast_cast<AstBlock>(node))
            {
                if (block != root_block)
                {
                    block->parent_block = root_block;
                    fix_block_hierarchy(block);
                }
            }
        });
    /*

    program.block {
        decl.init {
            proc.body {
               if.then_block {
               }
            }
        }
    }

    */

    // TODO

    for (auto statement : root_block->statements)
    {
        auto child_blocks = get_statement_child_blocks(statement);
        for (auto child_block : child_blocks)
        {
            child_block->parent_block = root_block;

            fix_block_hierarchy(child_block);
        }
    }
}

void allocate_locals(AstBlock *block)
{
    assert(block->size == 0);
    assert(block->is_global() == false);

    for (auto statement : block->statements)
    {
        if (auto decl = ast_cast<AstDeclaration>(statement))
        {
            auto size_of_decl = 8;  // TODO
            decl->address     = block->offset_from_parent_block + block->size;

            block->size += size_of_decl;
            if (decl->is_proc_arg)
            {
                // The return address is at the top of the stack
                // TODO: This is smelly... Must think about this, but works for now.
                decl->address -= 8;
                block->size_of_args += size_of_decl;
            }
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
    _write8(op, w);

    return addr;
}

int64_t write_op_8(OpCode op, uint8_t value, BytecodeWriter *w)
{
    auto addr = w->pos;
    _write8(op, w);
    _write8(value, w);

    return addr;
}

// TODO: Use varint encoding
int64_t write_op_64(OpCode op, int64_t value, BytecodeWriter *w)
{
    auto addr = w->pos;
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
    return kind == AstKind::binary_operator || kind == AstKind::identifier || kind == AstKind::literal ||
           kind == AstKind::procedure_call;
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
    if (auto bin_op = ast_cast<AstBinaryOperator>(node))
    {
        if (bin_op->type == Tt::logical_and)
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

        if (bin_op->type == Tt::logical_or)
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
            case Tt::asterisk: write_op(MUL, w); break;
            case Tt::slash:    write_op(DIV, w); break;
            case Tt::mod:      write_op(MOD, w); break;
            case Tt::plus:     write_op(ADD, w); break;
            case Tt::minus:    write_op(SUB, w); break;

            case Tt::bit_and: write_op(BITAND, w); break;
            case Tt::bit_or:  write_op(BITOR, w); break;
            case Tt::bit_xor: write_op(BITXOR, w); break;

            case Tt::left_shift:            write_op(LSH, w); break;
            case Tt::right_shift:           write_op(RSH, w); break;
            case Tt::equal:                 write_op(CMPEQ, w); break;
            case Tt::inequal:               write_op(CMPNE, w); break;
            case Tt::greater_than_or_equal: write_op(CMPGE, w); break;
            case Tt::greater_than:          write_op(CMPGT, w); break;
            case Tt::less_than_or_equal:    write_op(CMPLE, w); break;
            case Tt::less_than:             write_op(CMPLT, w); break;

            default: assert(false); break;
        }

        return true;
    }

    if (auto ident = ast_cast<AstIdentifier>(node))
    {
        auto decl = w->current_block->find_declaration(ident->identifier.text());
        if (decl == nullptr)
        {
            compile_error(w, std::format("Declaration of {} not found", ident->identifier.text()));
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

            if (got_return == false && statement->kind == AstKind::return_statement)
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

    if (auto decl = ast_cast<AstDeclaration>(node))
    {
        assert(decl->enclosing_proc == nullptr);
        decl->enclosing_proc = w->current_proc;

        if (w->current_block->is_global())
        {
            decl->address = w->bytecode.size();

            if (decl->init_expression != nullptr && generate_code(decl->init_expression, w) == false)
            {
                return false;
            }
        }
        else if (decl->init_expression != nullptr)
        {
            if (generate_expr(decl->init_expression, w) == false)
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
        assert(literal->type == LiteralType::integer);  // TODO
        write_op_64(PUSHC, literal->int_value, w);
        return true;
    }

    if (auto proc_call = ast_cast<AstProcedureCall>(node))
    {
        for (auto i = 0; i < proc_call->arguments.size(); ++i)
        {
            // auto arg = proc_call->arguments[proc_call->arguments.size() - i - 1];
            auto arg = proc_call->arguments[i];
            if (generate_code(arg, w) == false)
            {
                return false;
            }
        }

        if (generate_expr(proc_call->procedure, w) == false)
        {
            return false;
        }

        write_op(CALL, w);
        return true;
    }

    if (auto proc = ast_cast<AstProcedure>(node))
    {
        // TODO: Pop the arguments TODO: No, why, they would just get pushed again either way, right? But their
        // addresses must be set.

        auto prev_proc  = w->current_proc;
        w->current_proc = proc;
        defer
        {
            w->current_proc = prev_proc;
        };

        std::vector<AstDeclaration *> arg_decls;
        for (auto arg : proc->signature.arguments)
        {
            if (proc->body.find_declaration(arg.identifier.text()) != nullptr)
            {
                compile_error(w, "TODO");
                return false;
            }

            // TODO Copy type
            auto decl         = new AstDeclaration{};
            decl->identifier  = arg.identifier;
            decl->is_proc_arg = true;
            // decl->enclosing_proc = proc;

            arg_decls.push_back(decl);
        }

        proc->body.statements.insert(proc->body.statements.begin(), arg_decls.begin(), arg_decls.end());

        allocate_locals(&proc->body);

        // Make stack space for the locals
        write_op_64(ADDRSP, proc->body.size - proc->body.size_of_args, w);

        return generate_code(&proc->body, w);
    }

    if (auto program = ast_cast<AstProgram>(node))
    {
        fix_block_hierarchy(&program->block);

        for (auto node : program->block.statements)
        {
            if (node->kind != AstKind::declaration)
            {
                compile_error(w, "Expected declaration");
                return false;
            }

            return generate_code(&program->block, w);
        }

        return true;
    }

    if (auto yf = ast_cast<AstIf>(node))
    {
        if (generate_expr(yf->condition, w) == false)
        {
            return false;
        }

        auto jmp0_false_pos = w->pos;
        write_op_64(JMP0, 333, w);

        if (generate_code(&yf->then_block, w) == false)
        {
            return false;
        }

        auto false_label = w->pos;
        w->pos           = jmp0_false_pos;
        write_op_64(JMP0, false_label, w);

        // TODO: Else block

        w->pos = false_label;

        return true;
    }

    if (auto retyrn = ast_cast<AstReturn>(node))
    {
        if (generate_expr(retyrn->expression, w) == false)
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

    UNREACHED;

    return false;
}
