#include "typecheck.h"
#include "parse.h"

enum class BinaryOperatorCategory
{
    arithmetic,
    bitwise_operation,
    boolean,
    assignment,
};

BinaryOperatorCategory get_category(TokenType type)
{
    switch (type)
    {
        case Tt::assign:
        {
            return BinaryOperatorCategory::assignment;
        }

        case Tt::asterisk:
        case Tt::slash:
        case Tt::mod:
        case Tt::plus:
        case Tt::minus:
        {
            return BinaryOperatorCategory::arithmetic;
        }

        case Tt::left_shift:
        case Tt::right_shift:
        case Tt::bit_and:
        case Tt::bit_xor:
        case Tt::bit_or:
        {
            return BinaryOperatorCategory::bitwise_operation;
        }


        case Tt::less_than:
        case Tt::greater_than:
        case Tt::less_than_or_equal:
        case Tt::greater_than_or_equal:
        case Tt::equal:
        case Tt::inequal:
        {
            return BinaryOperatorCategory::boolean;
        }

        default: UNREACHED
    }
}

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

            auto result           = new BinaryOperatorNode{};
            result->operator_type = bin_op->type;

            if (!(result->lhs = typecheck(containing_block, bin_op->lhs)))
            {
                return nullptr;
            }

            if (!(result->rhs = typecheck(containing_block, bin_op->rhs)))
            {
                return nullptr;
            }

            auto category = get_category(bin_op->type);
            switch (category)
            {
                case BinaryOperatorCategory::arithmetic:
                {
                    if (result->lhs->type.is_numerical() == false || result->rhs->type.is_numerical() == false)
                    {
                        type_error(bin_op, "Binary operators require numerical expressions on both sides");
                        return nullptr;
                    }

                    if (result->lhs->type.is_signed != result->rhs->type.is_signed)
                    {
                        type_error(
                            bin_op,
                            "Arithmetic on different signedness is not supported. You must cast the operands to the same type.");
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

                    break;
                }

                case BinaryOperatorCategory::bitwise_operation:
                {
                    // TODO
                    type_error(bin_op, "TODO: Bitwise operations are not yet supported in the type checker");
                    return nullptr;
                }

                case BinaryOperatorCategory::boolean:
                {
                    if (result->lhs->type.kind != Type::Kind::boolean)
                    {
                        type_error(bin_op, "Left side of the expression is not a boolean expression");
                        return nullptr;
                    }

                    if (result->rhs->type.kind != Type::Kind::boolean)
                    {
                        type_error(bin_op, "Right side of the expression is not a boolean expression");
                        return nullptr;
                    }

                    result->type = BuiltinTypes::boolean;

                    break;
                }

                case BinaryOperatorCategory::assignment:
                {
                    if (result->lhs->type != result->rhs->type)
                    {
                        type_error(bin_op, "An assignment expression must have the same type as the variable");
                        return nullptr;
                    }

                    result->type = result->lhs->type;

                    break;
                }

                default: UNREACHED;
            }

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
            UNREACHED;  // TODO

        case AstKind::procedure_call:
        {
            auto call = static_cast<AstProcedureCall *>(ast);
        }

        case AstKind::procedure_signature:
        case AstKind::program:
        case AstKind::return_statement:
        case AstKind::simple_type:         break;
    }

    UNREACHED
}
