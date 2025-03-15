#include "typecheck.h"
#include "parse.h"

enum class BinaryOperatorCategory
{
    arithmetic,
    bitwise_operation,
    comparison,
    short_circuit_boolean,
    assignment,
};

BinaryOperatorCategory bin_op_category(TokenType type)
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
            return BinaryOperatorCategory::comparison;
        }

        case Tt::logical_or:
        case Tt::logical_and:
        {
            return BinaryOperatorCategory::short_circuit_boolean;
        }

        default: UNREACHED
    }
}

void type_error(Node *node, std::string_view message)
{
    // TODO
    std::cout << "Typechecking error: " << message << std::endl;
}

DeclarationNode *BlockNode::find_declaration(std::string_view name) const
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

    if (this->containing_block != nullptr)
    {
        return this->containing_block->find_declaration(name);
    }

    return nullptr;
}

Node *make_node_internal(BlockNode *containing_block, AstNode *ast)
{
    switch (ast->kind)
    {
        case AstKind::binary_operator:
        {
            auto bin_op = static_cast<AstBinaryOperator *>(ast);

            auto lhs = make_node(containing_block, bin_op->lhs);
            if (lhs == nullptr)
            {
                return nullptr;
            }

            auto rhs = make_node(containing_block, bin_op->rhs);
            if (rhs == nullptr)
            {
                return nullptr;
            }

            auto result           = new BinaryOperatorNode{};
            result->lhs           = lhs;
            result->rhs           = rhs;
            result->operator_type = bin_op->type;

            return result;
        }

        case AstKind::block:
        {
            auto block = static_cast<AstBlock *>(ast);

            auto result = new BlockNode{};

            for (auto statement : block->statements)
            {
                auto statement_node = make_node(result, statement);
                result->statements.push_back(statement_node);
            }

            return result;
        }

        case AstKind::declaration:
        {
            auto decl = static_cast<AstDeclaration *>(ast);

            auto result        = new DeclarationNode{};
            result->identifier = decl->identifier.text();

            if (decl->type == nullptr)
            {
                auto nop               = new NopNode{};
                nop->containing_block  = containing_block;
                result->specified_type = nop;
            }
            else
            {
                result->specified_type = make_node(containing_block, decl->type);
            }

            if (decl->init_expression == nullptr)
            {
                auto nop                = new NopNode{};
                nop->containing_block   = containing_block;
                result->init_expression = nop;
            }
            else
            {
                result->init_expression = make_node(containing_block, decl->init_expression);
            }

            return result;
        }

        case AstKind::identifier:
        {
            auto ident = static_cast<AstIdentifier *>(ast);

            auto result        = new IdentifierNode{};
            result->identifier = ident->identifier.text();

            return result;
        }

        case AstKind::if_statement:
        {
            auto yf = static_cast<AstIf *>(ast);

            auto result        = new IfNode{};
            result->condition  = make_node(containing_block, yf->condition);
            result->then_block = node_cast<BlockNode, true>(make_node(containing_block, &yf->then_block));
            result->else_block = node_cast<BlockNode, true>(make_node(containing_block, yf->else_block));

            return result;
        }

        case AstKind::literal:
        {
            auto literal = static_cast<AstLiteral *>(ast);

            auto result    = new LiteralNode{};
            result->value  = literal->value;
            result->suffix = literal->suffix;

            return result;
        }

        case AstKind::procedure:
        {
            auto proc = static_cast<AstProcedure *>(ast);

            auto result       = new ProcedureNode{};
            result->signature = node_cast<ProcedureSignatureNode, true>(make_node(containing_block, &proc->signature));
            result->body      = node_cast<BlockNode, true>(make_node(containing_block, &proc->body));

            assert(result->signature != nullptr);
            assert(result->body != nullptr);

            return result;
        }

        case AstKind::procedure_call:
        {
            auto call = static_cast<AstProcedureCall *>(ast);

            auto result       = new ProcedureCallNode{};
            result->procedure = make_node(containing_block, call->procedure);

            for (auto argument : call->arguments)
            {
                result->arguments.push_back(make_node(containing_block, argument));
            }

            return result;
        }

        case AstKind::procedure_signature:
        {
            auto signature = static_cast<AstProcedureSignature *>(ast);

            auto result = new ProcedureSignatureNode{};

            for (auto argument : signature->arguments)
            {
                auto argument_node = node_cast<DeclarationNode, true>(make_node(containing_block, &argument));
                result->arguments.push_back(argument_node);
            }

            result->return_type = make_node(containing_block, signature->return_type);

            return result;
        }

        case AstKind::program:
        {
            UNREACHED;
        }

        case AstKind::return_statement:
        {
            auto retyrn = static_cast<AstReturn *>(ast);

            auto result        = new ReturnNode{};
            result->expression = make_node(containing_block, retyrn->expression);

            return result;
        }

        case AstKind::simple_type:
        {
            auto simple_type = static_cast<AstSimpleType *>(ast);

            std::tuple<std::string_view, Node *> types_by_name[] = {
                {"void", const_cast<SimpleTypeNode *>(&BuiltinTypes::voyd)},
                {"i64", const_cast<SimpleTypeNode *>(&BuiltinTypes::i64)},
                {"i32", const_cast<SimpleTypeNode *>(&BuiltinTypes::i32)},
                {"i16", const_cast<SimpleTypeNode *>(&BuiltinTypes::i16)},
                {"i8", const_cast<SimpleTypeNode *>(&BuiltinTypes::i8)},
                {"u64", const_cast<SimpleTypeNode *>(&BuiltinTypes::u64)},
                {"u32", const_cast<SimpleTypeNode *>(&BuiltinTypes::u32)},
                {"u16", const_cast<SimpleTypeNode *>(&BuiltinTypes::u16)},
                {"u8", const_cast<SimpleTypeNode *>(&BuiltinTypes::u8)},
                {"bool", const_cast<SimpleTypeNode *>(&BuiltinTypes::boolean)},
                {"type", const_cast<SimpleTypeNode *>(&BuiltinTypes::type)},
            };

            auto found = false;
            for (auto [name, type] : types_by_name)
            {
                if (simple_type->identifier.text() == name)
                {
                    return type;
                }
            }

            // TODO: Error reporting
            std::cout << "Custom type names are not implemented yet" << std::endl;

            return nullptr;
        }

        case AstKind::pointer_type:
        {
            auto pointer = static_cast<AstPointerType *>(ast);

            auto result         = new PointerTypeNode{};
            result->target_type = make_node(containing_block, pointer->target_type);

            return result;
        }

        case AstKind::array_type:
        {
            auto array = static_cast<AstArrayType *>(ast);

            auto result               = new ArrayTypeNode{};
            result->length_expression = make_node(containing_block, array->length_expression);
            result->element_type      = make_node(containing_block, array->element_type);

            return result;
        }
    }

    UNREACHED
}

Node *make_node(BlockNode *containing_block, AstNode *ast)
{
    auto result              = make_node_internal(containing_block, ast);
    result->containing_block = containing_block;

    return result;
}

bool typecheck(Node *node)
{
    // NOTE: Only set the type if the node is an expression, do not assign void to statements

    assert(node != nullptr);

    switch (node->kind)
    {
        case NodeKind::binary_operator:
        {
            auto bin_op = static_cast<BinaryOperatorNode *>(node);

            if (typecheck(bin_op->lhs) == false)
            {
                return false;
            }

            if (typecheck(bin_op->rhs) == false)
            {
                return false;
            }

            auto lhs_simple = node_cast<const SimpleTypeNode>(bin_op->lhs->type);
            auto rhs_simple = node_cast<const SimpleTypeNode>(bin_op->rhs->type);

            auto category = bin_op_category(bin_op->operator_type);
            switch (category)
            {
                case BinaryOperatorCategory::arithmetic:
                {
                    if (lhs_simple == nullptr)
                    {
                        type_error(bin_op, "Invalid kind of type for left operand");
                        return false;
                    }

                    if (rhs_simple == nullptr)
                    {
                        type_error(bin_op, "Invalid kind of type for right operand");
                        return false;
                    }

                    if (lhs_simple->is_numerical() == false || rhs_simple->is_numerical() == false)
                    {
                        type_error(bin_op, "Arithmetic binary operators require numerical expressions on both sides");
                        return false;
                    }

                    auto invalid_integer_types = lhs_simple->type_kind == SimpleTypeNode::Kind::signed_integer &&
                                                     rhs_simple->type_kind == SimpleTypeNode::Kind::unsigned_integer ||
                                                 lhs_simple->type_kind == SimpleTypeNode::Kind::unsigned_integer &&
                                                     rhs_simple->type_kind == SimpleTypeNode::Kind::signed_integer;
                    if (invalid_integer_types)
                    {
                        type_error(
                            bin_op,
                            "Arithmetic on different signedness is not supported. You must cast the operands to the same type.");
                        return false;
                    }

                    auto is_float = lhs_simple->type_kind == SimpleTypeNode::Kind::floatingpoint ||
                                    rhs_simple->type_kind == SimpleTypeNode::Kind::floatingpoint;
                    auto max_size = std::max(lhs_simple->size, rhs_simple->size);

                    assert(max_size == 1 || max_size == 2 || max_size == 4 || max_size == 8);

                    auto type = new SimpleTypeNode{
                        is_float ? SimpleTypeNode::Kind::floatingpoint : lhs_simple->type_kind,
                        max_size};

                    bin_op->type = type;

                    return true;
                }

                case BinaryOperatorCategory::bitwise_operation:
                {
                    if (lhs_simple == nullptr)
                    {
                        type_error(bin_op, "Invalid kind of type for left operand");
                        return false;
                    }

                    if (rhs_simple == nullptr)
                    {
                        type_error(bin_op, "Invalid kind of type for right operand");
                        return false;
                    }

                    if (lhs_simple->type_kind != SimpleTypeNode::Kind::unsigned_integer ||
                        rhs_simple->type_kind != SimpleTypeNode::Kind::unsigned_integer)
                    {
                        type_error(bin_op, "Bitwise binary operators require unsigned integer operands on both sides");
                        return false;
                    }

                    auto max_size = std::max(lhs_simple->size, rhs_simple->size);
                    assert(max_size == 1 || max_size == 2 || max_size == 4 || max_size == 8);

                    auto type = new SimpleTypeNode{SimpleTypeNode::Kind::unsigned_integer, max_size};

                    bin_op->type = type;

                    return true;
                }

                case BinaryOperatorCategory::comparison:
                {
                    // TODO

                    if (types_equal(bin_op->lhs->type, bin_op->rhs->type) == false)
                    {
                        type_error(bin_op, "Only expressions of the same time can be compared");
                        return false;
                    }

                    bin_op->type = &BuiltinTypes::boolean;

                    return true;
                }

                case BinaryOperatorCategory::short_circuit_boolean:
                {
                    if (lhs_simple == nullptr)
                    {
                        type_error(bin_op, "Invalid type for left operand");
                        return false;
                    }

                    if (rhs_simple == nullptr)
                    {
                        type_error(bin_op, "Invalid type for right operand");
                        return false;
                    }

                    if (lhs_simple->type_kind != SimpleTypeNode::Kind::boolean)
                    {
                        type_error(bin_op, "Left side is not a boolean expression");
                        return false;
                    }

                    if (lhs_simple->type_kind != SimpleTypeNode::Kind::boolean)
                    {
                        type_error(bin_op, "Right side is not a boolean expression");
                        return false;
                    }

                    bin_op->type = &BuiltinTypes::boolean;

                    return true;
                }

                case BinaryOperatorCategory::assignment:
                {
                    if (types_equal(bin_op->lhs->type, bin_op->rhs->type) == false)
                    {
                        type_error(bin_op, "An assignment expression must have the same type as the variable");
                        return false;
                    }

                    bin_op->type = bin_op->lhs->type;

                    return true;
                }

                default: UNREACHED;
            }

            return true;
        }

        case NodeKind::block:
        {
            auto block = static_cast<BlockNode *>(node);

            for (auto statement : block->statements)
            {
                if (typecheck(statement) == false)
                {
                    return false;
                }
            }

            return true;
        }

        case NodeKind::declaration:
        {
            auto decl = static_cast<DeclarationNode *>(node);

            if (typecheck(decl->init_expression) == false)
            {
                return false;
            }

            if (decl->specified_type->kind != NodeKind::nop)
            {
                if (decl->init_expression->kind == NodeKind::nop)
                {
                    assert(decl->specified_type != nullptr);
                    decl->init_expression->type = decl->specified_type;
                }

                if (types_equal(decl->specified_type, decl->init_expression->type) == false)
                {
                    type_error(
                        decl,
                        "The declaration init expression does not have the same inferred type as the specified type on the declaration");
                    return false;
                }
            }

            return true;
        }

        case NodeKind::identifier:
        {
            auto ident = static_cast<IdentifierNode *>(node);
            auto decl  = ident->containing_block->find_declaration(ident->identifier);

            if (decl == nullptr)
            {
                type_error(ident, std::format("Could not find the declaration of identifier '{}'", ident->identifier));
                return false;
            }

            // TODO: Use before declare should lead to this error, which should be caught
            assert(decl->init_expression->type != nullptr);

            ident->type = decl->init_expression->type;

            return true;
        }

        case NodeKind::if_statement:
        {
            auto yf = static_cast<IfNode *>(node);

            if (typecheck(yf->condition) == false)
            {
                return false;
            }

            return true;
        }

        case NodeKind::literal:
        {
            auto literal = static_cast<LiteralNode *>(node);

            if (std::holds_alternative<uint64_t>(literal->value))
            {
                if (literal->suffix == 'u')
                {
                    literal->type = &BuiltinTypes::u64;
                }
                else
                {
                    literal->type = &BuiltinTypes::i64;
                }

                return true;
            }

            if (std::holds_alternative<float>(literal->value))
            {
                literal->type = &BuiltinTypes::f32;
                return true;
            }

            if (std::holds_alternative<double>(literal->value))
            {
                literal->type = &BuiltinTypes::f64;
                return true;
            }

            if (std::holds_alternative<bool>(literal->value))
            {
                literal->type = &BuiltinTypes::boolean;
                return true;
            }

            // TODO
            type_error(literal, "TODO: Currently only boolean and integer literals are supported in the type checker");
            return false;
        }

        case NodeKind::procedure:
        {
            // TODO
            UNREACHED;
        }

        case NodeKind::procedure_call:
        {
            // TODO
            UNREACHED;
        }

        case NodeKind::procedure_signature:
        {
            // TODO
            UNREACHED;
        }

        case NodeKind::program:
        {
            // TODO
            UNREACHED;
        }

        case NodeKind::return_statement:
        {
            // TODO
            UNREACHED;
        }

        case NodeKind::simple_type:
        {
            // TODO
            UNREACHED;
        }

        case NodeKind::pointer_type:
        {
            // TODO
            UNREACHED;
        }

        case NodeKind::array_type:
        {
            // TODO
            UNREACHED;
        }

        case NodeKind::struct_type:
        {
            // TODO
            UNREACHED;
        }

        case NodeKind::nop:
        {
            node->type = node;
            return true;
        }
    }

    UNREACHED;

#if 0
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

            auto lhs_simple = node_cast<const SimpleTypeNode>(result->lhs->type);
            auto rhs_simple = node_cast<const SimpleTypeNode>(result->rhs->type);

            auto category = get_category(bin_op->type);
            switch (category)
            {
                case BinaryOperatorCategory::arithmetic:
                {
                    if (lhs_simple == nullptr)
                    {
                        type_error(bin_op, "Invalid kind of type for left operand");
                        return nullptr;
                    }

                    if (rhs_simple == nullptr)
                    {
                        type_error(bin_op, "Invalid kind of type for right operand");
                        return nullptr;
                    }

                    if (lhs_simple->is_numerical() == false || rhs_simple->is_numerical() == false)
                    {
                        type_error(bin_op, "Arithmetic binary operators require numerical expressions on both sides");
                        return nullptr;
                    }

                    if ((lhs_simple->type_kind == SimpleTypeNode::Kind::signed_integer) !=
                        (rhs_simple->type_kind == SimpleTypeNode::Kind::signed_integer))
                    {
                        type_error(
                            bin_op,
                            "Arithmetic on different signedness is not supported. You must cast the operands to the same type.");
                        return nullptr;
                    }

                    auto is_float = lhs_simple->type_kind == SimpleTypeNode::Kind::floatingpoint ||
                                    rhs_simple->type_kind == SimpleTypeNode::Kind::floatingpoint;
                    auto max_size = std::max(lhs_simple->size, rhs_simple->size);

                    assert(max_size == 1 || max_size == 2 || max_size == 4 || max_size == 8);

                    auto type = new SimpleTypeNode{
                        is_float ? SimpleTypeNode::Kind::floatingpoint : lhs_simple->type_kind,
                        max_size};

                    result->type = type;

                    break;
                }

                case BinaryOperatorCategory::bitwise_operation:
                {
                    if (lhs_simple == nullptr)
                    {
                        type_error(bin_op, "Invalid kind of type for left operand");
                        return nullptr;
                    }

                    if (rhs_simple == nullptr)
                    {
                        type_error(bin_op, "Invalid kind of type for right operand");
                        return nullptr;
                    }

                    if (lhs_simple->type_kind != SimpleTypeNode::Kind::unsigned_integer ||
                        lhs_simple->type_kind != SimpleTypeNode::Kind::unsigned_integer)
                    {
                        type_error(
                            bin_op,
                            "Bitwise binary operators require unsigned integer expressions on both sides");
                        return nullptr;
                    }

                    auto max_size = std::max(lhs_simple->size, rhs_simple->size);
                    assert(max_size == 1 || max_size == 2 || max_size == 4 || max_size == 8);

                    auto type = new SimpleTypeNode{SimpleTypeNode::Kind::unsigned_integer, max_size};

                    result->type = type;

                    break;
                }

                case BinaryOperatorCategory::comparison:
                {
                    // TODO

                    if (types_equal(result->lhs->type, result->rhs->type) == false)
                    {
                        type_error(bin_op, "Only expressions of the same time can be compared");
                        return nullptr;
                    }

                    result->type = &BuiltinTypes::boolean;

                    break;
                }

                case BinaryOperatorCategory::short_circuit_boolean:
                {
                    if (lhs_simple == nullptr)
                    {
                        type_error(bin_op, "Invalid type for left operand");
                        return nullptr;
                    }

                    if (rhs_simple == nullptr)
                    {
                        type_error(bin_op, "Invalid type for right operand");
                        return nullptr;
                    }

                    if (lhs_simple->type_kind != SimpleTypeNode::Kind::boolean)
                    {
                        type_error(bin_op, "Left side is not a boolean expression");
                        return nullptr;
                    }

                    if (lhs_simple->type_kind != SimpleTypeNode::Kind::boolean)
                    {
                        type_error(bin_op, "Right side is not a boolean expression");
                        return nullptr;
                    }

                    result->type = &BuiltinTypes::boolean;

                    break;
                }

                case BinaryOperatorCategory::assignment:
                {
                    if (types_equal(result->lhs->type, result->rhs->type) == false)
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

            result->type = &BuiltinTypes::voyd;

            return result;
        }

        case AstKind::declaration:
        {
            auto decl = static_cast<AstDeclaration *>(ast);

            auto result             = new DeclarationNode{};
            result->identifier      = decl->identifier.text();
            result->init_expression = typecheck(containing_block, decl->init_expression);

            result->type = &BuiltinTypes::voyd;

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

            assert(decl->init_expression->type != nullptr);
            result->type = decl->init_expression->type;

            return result;
        }

        case AstKind::if_statement:
        {
            auto yf = static_cast<AstIf *>(ast);

            auto result        = new IfNode{};
            result->condition  = typecheck(containing_block, yf->condition);
            result->then_block = node_cast<BlockNode>(typecheck(containing_block, &yf->then_block));
            result->else_block = node_cast<BlockNode>(typecheck(containing_block, yf->else_block));

            assert(result->then_block != nullptr && ((result->else_block == nullptr) == (yf->else_block == nullptr)));

            result->type = &BuiltinTypes::voyd;

            return result;
        }

        case AstKind::literal:
        {
            auto literal = static_cast<AstLiteral *>(ast);

            if (std::holds_alternative<uint64_t>(literal->value) == false)
            {
                // TODO
                type_error(literal, "TODO: Currently only signed integer literals are supported in the type checker");
                return nullptr;
            }

            auto result   = new LiteralNode{};
            result->value = literal->value;

            if (literal->suffix == 'u')
            {
                result->type = &BuiltinTypes::u64;
            }
            else
            {
                result->type = &BuiltinTypes::i64;
            }

            return result;
        }

        case AstKind::procedure:
            // TODO LEFT OFF HERE
            UNREACHED;  // TODO

        case AstKind::procedure_call:
        {
            auto call = static_cast<AstProcedureCall *>(ast);

            auto result = new ProcedureCallNode{};
            for (auto argument
        }

        case AstKind::procedure_signature:
        {
            auto signature = static_cast<AstProcedureSignature *>(ast);

            auto result = new ProcedureSignatureNode{};

            for (auto argument : signature->arguments)
            {
                result->arguments.push_back(make_node(containing_block, argument));
            }

            result->return_type = convert_type(signature->return_type);

            return result
        }

        case AstKind::program:
        case AstKind::return_statement:
        case AstKind::simple_type:
        case AstKind::pointer_type:
        case AstKind::array_type:       break;
    }

    UNREACHED
#endif
}

bool types_equal(const Node *lhs, const Node *rhs)
{
    assert(lhs != nullptr && rhs != nullptr);

    if (lhs->kind != rhs->kind)
    {
        return false;
    }

    switch (lhs->kind)
    {
        case NodeKind::simple_type:
        {
            auto lhs_simple = static_cast<const SimpleTypeNode *>(lhs);
            auto rhs_type   = static_cast<const SimpleTypeNode *>(rhs);

            return lhs_simple->type_kind == rhs_type->type_kind && lhs_simple->size == rhs_type->size;
        }

        case NodeKind::pointer_type:
        {
            auto lhs_pointer = static_cast<const PointerTypeNode *>(lhs);
            auto rhs_pointer = static_cast<const PointerTypeNode *>(rhs);

            return types_equal(lhs_pointer->target_type, rhs_pointer->target_type);
        }

        case NodeKind::array_type:
        {
            auto lhs_array = static_cast<const ArrayTypeNode *>(lhs);
            auto rhs_array = static_cast<const ArrayTypeNode *>(rhs);

            if (types_equal(lhs_array->element_type, rhs_array->element_type) == false)
            {
                return false;
            }

            // TODO
            auto lhs_length_literal = node_cast<LiteralNode>(lhs_array->length_expression);
            auto rhs_length_literal = node_cast<LiteralNode>(rhs_array->length_expression);

            if (lhs_length_literal == nullptr || rhs_length_literal == nullptr)
            {
                FATAL("Only literal array length expressions are supported for now");
            }

            if (std::holds_alternative<uint64_t>(lhs_length_literal->value) == false ||
                std::holds_alternative<uint64_t>(rhs_length_literal->value) == false)
            {
                FATAL("The length expression is not an integer literal value");
            }

            auto lhs_length = std::get<uint64_t>(lhs_length_literal->value);
            auto rhs_length = std::get<uint64_t>(rhs_length_literal->value);

            return lhs_length == rhs_length;
        }

        case NodeKind::struct_type:
        {
            // TODO
            UNREACHED;
        }

        default: UNREACHED;
    }
}

const SimpleTypeNode BuiltinTypes::voyd = SimpleTypeNode{SimpleTypeNode::Kind::voyd, -1};

const SimpleTypeNode BuiltinTypes::i64 = SimpleTypeNode{SimpleTypeNode::Kind::signed_integer, 8};
const SimpleTypeNode BuiltinTypes::i32 = SimpleTypeNode{SimpleTypeNode::Kind::signed_integer, 4};
const SimpleTypeNode BuiltinTypes::i16 = SimpleTypeNode{SimpleTypeNode::Kind::signed_integer, 2};
const SimpleTypeNode BuiltinTypes::i8  = SimpleTypeNode{SimpleTypeNode::Kind::signed_integer, 1};

const SimpleTypeNode BuiltinTypes::u64 = SimpleTypeNode{SimpleTypeNode::Kind::unsigned_integer, 8};
const SimpleTypeNode BuiltinTypes::u32 = SimpleTypeNode{SimpleTypeNode::Kind::unsigned_integer, 4};
const SimpleTypeNode BuiltinTypes::u16 = SimpleTypeNode{SimpleTypeNode::Kind::unsigned_integer, 2};
const SimpleTypeNode BuiltinTypes::u8  = SimpleTypeNode{SimpleTypeNode::Kind::unsigned_integer, 1};
const SimpleTypeNode BuiltinTypes::f32 = SimpleTypeNode{SimpleTypeNode::Kind::floatingpoint, 4};
const SimpleTypeNode BuiltinTypes::f64 = SimpleTypeNode{SimpleTypeNode::Kind::floatingpoint, 8};

const SimpleTypeNode BuiltinTypes::boolean = SimpleTypeNode{SimpleTypeNode::Kind::boolean, 1};
const SimpleTypeNode BuiltinTypes::type    = SimpleTypeNode{SimpleTypeNode::Kind::type, -1};
