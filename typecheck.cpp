#include "typecheck.h"
#include "parse.h"

void type_error(AstNode *ast, std::string_view message)
{
    // TODO
    FATAL(message);
}

DeclarationNode *BlockNode::find_declaration(std::string_view name)
{
    for (auto statement : this->statements)
    {
        if (auto decl = node_cast<DeclarationNode>(statement))
        {
            if (decl->identifier == name)
            {
                return decl;
            }
        }
    }

    if (this->parent_block != nullptr)
    {
        return this->parent_block->find_declaration(name);
    }

    return nullptr;
}

Node *typecheck(BlockNode *containing_block, AstNode *ast)
{
    // TODO: Error messages
    // TODO: Check for duplicate declarations

    switch (ast->kind)
    {
        case AstKind::binary_operator:
        {
            auto bin_op = static_cast<AstBinaryOperator *>(ast);

            auto result = new BinaryOperatorNode{};

            result->operator_type = bin_op->type;

            if (!(result->lhs = typecheck(containing_block, bin_op->lhs)))
            {
                return nullptr;
            }

            if (result->lhs->type.is_numerical() == false)
            {
                return nullptr;
            }

            if (!(result->rhs = typecheck(containing_block, bin_op->rhs)))
            {
                return nullptr;
            }

            if (result->rhs->type.is_numerical() == false)
            {
                return nullptr;
            }

            if (result->lhs->type.is_signed != result->rhs->type.is_signed)
            {
                // We don't do automatic lossy conversion of signedness to avoid errors
                return nullptr;
            }

            auto is_float = result->lhs->type.kind == Type::Kind::floatingpoint ||
                            result->rhs->type.kind == Type::Kind::floatingpoint;
            auto max_size = std::max(result->lhs->type.size, result->rhs->type.size);

            assert(max_size == 1 || max_size == 2 || max_size == 4 || max_size == 8);

            result->type = Type{
                .kind = is_float ? Type::Kind::floatingpoint : Type::Kind::integer,
                .size = max_size,
            };

            return result;
        }

        case AstKind::block:
        {
            auto block = static_cast<AstBlock *>(ast);

            auto result          = new BlockNode{};
            result->parent_block = containing_block;

            for (auto statement : block->statements)
            {
                auto statement_node = typecheck(result, statement);
                result->statements.push_back(statement_node);
            }

            result->type = BuiltinTypes::voyd;

            return result;
        }

        case AstKind::declaration:
        {
            auto decl = static_cast<AstDeclaration *>(ast);

            auto result             = new DeclarationNode{};
            result->identifier      = decl->identifier.text();
            result->init_expression = typecheck(containing_block, decl->init_expression);

            result->type = BuiltinTypes::voyd;

            return result;
        }

        case AstKind::identifier:
        {
            auto ident = static_cast<AstIdentifier *>(ast);

            auto result        = new IdentifierNode{};
            result->identifier = ident->identifier.text();

            auto decl = containing_block->find_declaration(result->identifier);
            if (decl == nullptr)
            {
                type_error(ident, std::format("Declaration of '{}' not found", result->identifier));
                return nullptr;
            }

            assert(decl->init_expression->type.is_valid());
            result->type = decl->init_expression->type;

            return result;
        }

        case AstKind::if_branch:
        {
            auto yf = static_cast<AstIf *>(ast);

            auto result        = new IfNode{};
            result->condition  = typecheck(containing_block, yf->condition);
            result->then_block = node_cast<BlockNode>(typecheck(containing_block, &yf->then_block));
            result->else_block = node_cast<BlockNode>(typecheck(containing_block, yf->else_block));

            assert(result->then_block != nullptr && ((result->else_block == nullptr) == (yf->else_block == nullptr)));

            result->type = BuiltinTypes::voyd;

            return result;
        }

        case AstKind::literal:
        {
            auto literal = static_cast<AstLiteral *>(ast);

            if (literal->type != LiteralType::signed_integer)
            {
                // TODO
                type_error(literal, "TODO: Currently only signed integer literals are supported in the type checker");
                return nullptr;
            }

            auto result                    = new LiteralNode{};
            result->signed_integer_value   = literal->signed_integer_value;
            result->unsigned_integer_value = literal->unsigned_integer_value;
            result->float_value            = literal->float_value;
            result->double_value           = literal->double_value;

            result->type = BuiltinTypes::i64;

            return result;
        }

        case AstKind::procedure:
            // TODO LEFT OFF HERE

        case AstKind::procedure_call:
        case AstKind::procedure_signature:
        case AstKind::program:
        case AstKind::return_statement:
        case AstKind::simple_type:         break;
    }

    UNREACHED
}
