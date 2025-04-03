#include "desugar.h"

#include "parse.h"

AstNode *desugar(MemoryPool &pool, AstNode *ast)
{
    if (ast == nullptr)
    {
        return nullptr;
    }

    // NOTE: Do not modify the original AST node!
    // NOTE: Only allocate and return new nodes if the desugared node is not equal to the original node

    switch (ast->kind)
    {
        case AstKind::for_loop:
        {
            /*
            for <identifier> <range_begin>:<operator><range_end>:<step> <block>
            ->
            {
                <identifier> := range_begin
                while <identifier> <operator> <range_end> {
                    <...block>
                    <identifier> += <step>
                }
            }
            */
            auto foa = static_cast<AstForLoop *>(ast);

            auto decl        = new (pool) AstDeclaration{};
            decl->identifier = foa->identifier;

            if (foa->range_begin != nullptr)
            {
                decl->init_expression = foa->range_begin;
            }
            else
            {
                auto zero = new (pool) AstLiteral{};
                zero->value.emplace<uint64_t>(0);
                decl->init_expression = zero;
            }

            auto counter_identifier        = new (pool) AstIdentifier{};
            counter_identifier->identifier = foa->identifier;

            auto condition           = new (pool) AstBinaryOperator{};
            condition->lhs           = counter_identifier;
            condition->operator_type = foa->comparison_operator;
            condition->rhs           = foa->range_end;

            auto next_value = new (pool) AstBinaryOperator{};

            switch (foa->comparison_operator)
            {
                case Tt::less_than:
                case Tt::less_than_or_equal:
                {
                    next_value->operator_type = Tt::plus;
                    break;
                }

                case Tt::greater_than:
                case Tt::greater_than_or_equal:
                {
                    next_value->operator_type = Tt::minus;
                    break;
                }

                default: UNREACHED;
            }
            next_value->lhs = counter_identifier;

            if (foa->step != nullptr)
            {
                next_value->rhs = foa->step;
            }
            else
            {
                auto one = new (pool) AstLiteral{};
                one->value.emplace<uint64_t>(1);
                next_value->rhs = one;
            }

            auto prologue           = new (pool) AstBinaryOperator{};
            prologue->operator_type = Tt::assign;
            prologue->lhs           = counter_identifier;
            prologue->rhs           = next_value;

            auto whyle       = new (pool) AstWhileLoop{};
            whyle->condition = condition;
            whyle->block     = foa->block;
            whyle->prologue  = prologue;

            auto block = new (pool) AstBlock{};
            block->statements.push_back(decl);
            block->statements.push_back(whyle);

            return desugar(pool, block);
        }

        case AstKind::array_type:
        {
            auto array_type = static_cast<AstArrayType *>(ast);

            AstArrayType desugared{*array_type};
            desugared.length_expression = desugar(pool, array_type->length_expression);
            desugared.element_type      = desugar(pool, array_type->element_type);

            if (desugared != *array_type)
            {
                return new (pool) AstArrayType{std::move(desugared)};
            }

            return array_type;
        }

        case AstKind::binary_operator:
        {
            auto bin_op = static_cast<AstBinaryOperator *>(ast);

            AstBinaryOperator desugared{*bin_op};
            desugared.lhs = desugar(pool, bin_op->lhs);
            desugared.rhs = desugar(pool, bin_op->rhs);

            if (desugared != *bin_op)
            {
                return new (pool) AstBinaryOperator{std::move(desugared)};
            }

            return bin_op;
        }

        case AstKind::block:
        {
            auto block = static_cast<AstBlock *>(ast);

            AstBlock desugared{*block};

            for (auto &statement : desugared.statements)
            {
                statement = desugar(pool, statement);
            }

            if (desugared != *block)
            {
                return new (pool) AstBlock{std::move(desugared)};
            }

            return block;
        }

        case AstKind::break_statement:
        {
            return ast;
        }

        case AstKind::continue_statement:
        {
            return ast;
        }

        case AstKind::declaration:
        {
            auto decl = static_cast<AstDeclaration *>(ast);

            AstDeclaration desugared{*decl};
            desugared.type            = desugar(pool, decl->type);
            desugared.init_expression = desugar(pool, decl->init_expression);

            if (desugared != *decl)
            {
                return new (pool) AstDeclaration{std::move(desugared)};
            }

            return decl;
        }

        case AstKind::goto_statement:
        {
            return ast;
        }

        case AstKind::identifier:
        {
            return ast;
        }

        case AstKind::if_statement:
        {
            auto yf = static_cast<AstIfStatement *>(ast);

            AstIfStatement desugared{*yf};
            desugared.condition  = desugar(pool, yf->condition);
            desugared.then_block = ast_cast<AstBlock, true>(desugar(pool, yf->then_block));

            if (yf->else_block != nullptr)
            {
                desugared.else_block = ast_cast<AstBlock, true>(desugar(pool, yf->else_block));
            }

            if (desugared != *yf)
            {
                return new (pool) AstIfStatement{std::move(desugared)};
            }

            return yf;
        }

        case AstKind::label:
        {
            return ast;
        }

        case AstKind::literal:
        {
            return ast;
        }

        case AstKind::module:
        {
            auto module = static_cast<AstModule *>(ast);

            AstModule desugared{*module};
            desugared.block = ast_cast<AstBlock, true>(desugar(pool, module->block));

            if (desugared != *module)
            {
                return new (pool) AstModule{std::move(desugared)};
            }

            return module;
        }

        case AstKind::pointer_type:
        {
            auto pointer_type = static_cast<AstPointerType *>(ast);

            AstPointerType desugared{*pointer_type};
            desugared.target_type = desugar(pool, pointer_type->target_type);

            if (desugared != *pointer_type)
            {
                return new (pool) AstPointerType{std::move(desugared)};
            }

            return pointer_type;
        }

        case AstKind::procedure:
        {
            auto proc = static_cast<AstProcedure *>(ast);

            AstProcedure desugared{*proc};
            desugared.signature = ast_cast<AstProcedureSignature, true>(desugar(pool, proc->signature));
            desugared.body      = ast_cast<AstBlock, false>(desugar(pool, proc->body));

            if (desugared != *proc)
            {
                return new (pool) AstProcedure{std::move(desugared)};
            }

            return proc;
        }

        case AstKind::procedure_call:
        {
            auto call = static_cast<AstProcedureCall *>(ast);

            AstProcedureCall desugared{*call};
            desugared.procedure = desugar(pool, call->procedure);

            for (auto &arg : desugared.arguments)
            {
                arg = desugar(pool, arg);
            }

            if (desugared != *call)
            {
                return new (pool) AstProcedureCall{std::move(desugared)};
            }

            return call;
        }

        case AstKind::procedure_signature:
        {
            auto signature = static_cast<AstProcedureSignature *>(ast);

            AstProcedureSignature desugared{*signature};
            for (auto &arg : desugared.arguments)
            {
                arg = ast_cast<AstDeclaration, true>(desugar(pool, arg));
            }

            if (desugared != *signature)
            {
                return new (pool) AstProcedureSignature{std::move(desugared)};
            }

            return signature;
        }

        case AstKind::return_statement:
        {
            auto retyrn = static_cast<AstReturnStatement *>(ast);

            AstReturnStatement desugared{*retyrn};
            desugared.expression = desugar(pool, retyrn->expression);

            if (desugared != *retyrn)
            {
                return new (pool) AstReturnStatement{std::move(desugared)};
            }

            return retyrn;
        }

        case AstKind::type_identifier:
        {
            return ast;
        }

        case AstKind::while_loop:
        {
            auto whyle = static_cast<AstWhileLoop *>(ast);

            AstWhileLoop desugared{*whyle};
            desugared.condition = desugar(pool, whyle->condition);
            desugared.block     = ast_cast<AstBlock, true>(desugar(pool, whyle->block));

            if (desugared != *whyle)
            {
                return new (pool) AstWhileLoop{std::move(desugared)};
            }

            return whyle;
        }
    }

    UNREACHED;

    /*
    match <expr> {
        <match-pattern-1>: <block-1>
        <match-pattern-2>: <block-2>
        ...
        default: <block-n>
    }
    ->
    e := <expr>
    if e is <match-pattern-1> <block-1>
    else if e is <match-pattern-1> <block-1>
    ...
    else <block-n>
    */
}
