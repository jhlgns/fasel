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

bool spread_poison(const Node *possibly_infected, Node *spread_to)
{
    if (possibly_infected->is_poisoned())
    {
        spread_to->set_inferred_type(&BuiltinTypes::poison);
        return true;
    }

    return false;
}

Node *NodeConverter::make_node(AstNode *ast)
{
    switch (ast->kind)
    {
        case AstKind::binary_operator:
        {
            auto bin_op = static_cast<AstBinaryOperator *>(ast);

            auto lhs = this->make_node(bin_op->lhs);
            auto rhs = this->make_node(bin_op->rhs);

            assert(lhs != nullptr && rhs != nullptr);

            return this->ctx.make_binary_operator(bin_op->type, lhs, rhs);
        }

        case AstKind::block:
        {
            auto block = static_cast<AstBlock *>(ast);

            auto result = this->ctx.make_block(this->current_block, {});

            SET_TEMPORARILY(this->current_block, result);

            for (auto statement : block->statements)
            {
                auto statement_node = this->make_node(statement);
                result->statements.push_back(statement_node);
            }

            return result;
        }

        case AstKind::declaration:
        {
            auto decl = static_cast<AstDeclaration *>(ast);

            Node *specified_type{};
            Node *init_expr{};

            if (decl->type == nullptr)
            {
                specified_type = this->ctx.make_nop();
            }
            else
            {
                specified_type = this->make_node(decl->type);
            }

            if (decl->init_expression == nullptr)
            {
                init_expr = this->ctx.make_nop();
            }
            else
            {
                init_expr = this->make_node(decl->init_expression);
            }

            return this->ctx
                .make_declaration(decl->identifier.text(), specified_type, init_expr, decl->is_procedure_argument);
        }

        case AstKind::identifier:
        {
            auto ident = static_cast<AstIdentifier *>(ast);
            return this->ctx.make_identifier(ident->identifier.text());
        }

        case AstKind::if_statement:
        {
            auto yf = static_cast<AstIfStatement *>(ast);

            auto condition  = this->make_node(yf->condition);
            auto then_block = node_cast<BlockNode, true>(this->make_node(&yf->then_block));

            BlockNode *else_block{};
            if (yf->else_block != nullptr)
            {
                else_block = node_cast<BlockNode, true>(this->make_node(yf->else_block));
            }

            return this->ctx.make_if(condition, then_block, else_block);
        }

        case AstKind::while_loop:
        {
            auto whyle = static_cast<AstWhileLoop *>(ast);

            auto condition = this->make_node(whyle->condition);
            auto block     = node_cast<BlockNode, true>(this->make_node(&whyle->block));

            return this->ctx.make_while(condition, block);
        }

        case AstKind::literal:
        {
            auto literal = static_cast<AstLiteral *>(ast);
            return this->ctx.make_literal(literal->value, literal->suffix);
        }

        case AstKind::procedure:
        {
            auto proc = static_cast<AstProcedure *>(ast);

            auto signature = node_cast<ProcedureSignatureNode, true>(this->make_node(&proc->signature));

            BlockNode *body{};
            if (proc->is_external == false)
            {
                body = node_cast<BlockNode, true>(this->make_node(&proc->body));
            }

            return this->ctx.make_procedure(signature, body, proc->is_external);
        }

        case AstKind::procedure_call:
        {
            auto call = static_cast<AstProcedureCall *>(ast);

            auto procedure = this->make_node(call->procedure);
            std::vector<Node *> arguments{};

            for (auto argument : call->arguments)
            {
                arguments.push_back(this->make_node(argument));
            }

            return this->ctx.make_procedure_call(procedure, std::move(arguments));
        }

        case AstKind::procedure_signature:
        {
            auto signature = static_cast<AstProcedureSignature *>(ast);
            std::vector<DeclarationNode *> arguments{};

            for (auto argument : signature->arguments)
            {
                auto argument_node = node_cast<DeclarationNode, true>(this->make_node(&argument));
                arguments.push_back(argument_node);
            }

            auto return_type = this->make_node(signature->return_type);

            return this->ctx.make_procedure_signature(std::move(arguments), signature->is_vararg, return_type);
        }

        case AstKind::return_statement:
        {
            auto retyrn = static_cast<AstReturnStatement *>(ast);

            Node *expression{};
            if (retyrn->expression == nullptr)
            {
                expression = this->ctx.make_nop();
            }
            else
            {
                expression = this->make_node(retyrn->expression);
            }

            return this->ctx.make_return(expression);
        }

        case AstKind::module:
        {
            auto module = static_cast<AstModule *>(ast);

            auto block = node_cast<BlockNode, true>(this->make_node(&module->block));

            return this->ctx.make_module(block);
        }

        case AstKind::type_identifier:
        {
            auto type_ident = static_cast<AstTypeIdentifier *>(ast);

            auto found = false;
            for (auto [type, name] : BuiltinTypes::type_names)
            {
                if (type_ident->identifier.text() == name)
                {
                    return type;
                }
            }

            // TODO: Error reporting
            std::cout << "Type '" << type_ident->identifier.text()
                      << "' not found (custom type names are not implemented yet)" << std::endl;

            return nullptr;
        }

        case AstKind::pointer_type:
        {
            auto pointer = static_cast<AstPointerType *>(ast);

            auto target_type = this->make_node(pointer->target_type);

            return this->ctx.make_pointer_type(target_type);
        }

        case AstKind::array_type:
        {
            auto array = static_cast<AstArrayType *>(ast);

            auto length       = this->make_node(array->length_expression);
            auto element_type = this->make_node(array->element_type);

            return this->ctx.make_array_type(length, element_type);
        }

        case AstKind::label:
        {
            auto label = static_cast<AstLabel *>(ast);
            return this->ctx.make_label(label->identifier);
        }

        case AstKind::goto_statement:
        {
            auto gotoo = static_cast<AstGotoStatement *>(ast);
            return this->ctx.make_goto(gotoo->label_identifier);
        }

        case AstKind::break_statement:
        {
            return this->ctx.make_break();
        }

        case AstKind::continue_statement:
        {
            return this->ctx.make_continue();
        }
    }

    UNREACHED
}

//
// DeclarationRegistrar
//

void DeclarationRegistrar::register_declarations(ModuleNode *node)
{
    ::visit(node, *this);
}

// Registers the declaration inside the current block
void DeclarationRegistrar::visit(DeclarationNode *declaration)
{
    declaration->containing_block = this->current_block;

    if (declaration->is_procedure_argument)
    {
        return;
    }

    if (declaration->init_expression->kind == NodeKind::procedure && this->current_block->is_global() == false)
    {
        this->error(declaration, "Procedures can only be defined at module scope");
        return;
    }

    auto [it, ok] = this->current_block->declarations.emplace(std::string{declaration->identifier}, declaration);
    if (ok == false)
    {
        this->error(
            declaration,
            std::format(
                "A declaration with identifier '{}' already exists in this procedure's body",
                declaration->identifier));
    }
}

// Registers the label as a declaration in the current block
void DeclarationRegistrar::visit(LabelNode *label)
{
    auto decl     = this->ctx.make_declaration(label->identifier, &BuiltinTypes::label, label, false);
    auto [it, ok] = this->current_procedure->body->declarations.emplace(std::string{label->identifier}, decl);
    if (ok == false)
    {
        this->error(
            label,
            std::format(
                "A declaration with identifier '{}' already exists in this procedure's body",
                label->identifier));
    }
}

// Registers the arguments of the procedure as declarations inside the current block
void DeclarationRegistrar::visit(ProcedureNode *procedure)
{
    if (procedure->is_external)
    {
        return;
    }

    for (auto arg : procedure->signature->arguments)
    {
        auto [it, ok] = procedure->body->declarations.emplace(std::string{arg->identifier}, arg);
        if (ok == false)
        {
            this->error(arg, std::format("Duplicate argument name '{}'", arg->identifier));
        }
    }
}

//
// TypeChecker
//

bool TypeChecker::do_implicit_cast_if_necessary(Node *&node, Node *type)
{
    assert(type->is_type());
    assert(Node::types_equal(type, &BuiltinTypes::poison) == false);
    assert(node->is_type() == false || node->kind == NodeKind::nop);

    // NOTE: Also set inferred type on the cast node

    if (Node::types_equal(node->inferred_type(), type))
    {
        return true;
    }

    auto can_cast = false;

    auto src_basic  = node_cast<BasicTypeNode>(node->inferred_type());
    auto dest_basic = node_cast<BasicTypeNode>(type);
    if (src_basic != nullptr && dest_basic != nullptr)
    {
        can_cast = src_basic->is_numerical() && dest_basic->is_numerical();
    }

    // TODO

    if (can_cast)
    {
        auto cast = this->ctx.make_type_cast(type, node);
        cast->set_inferred_type(type);
        node = cast;
    }

    return can_cast;
}

// Does not modify the binary operator. Possibly does implicit casts for its operands
// and returns the coerced type, such that the inferred types of both operands are
// equal to the coerced type.
// In case of an error, the binary operator is poisoned.
Node *TypeChecker::coerce_types(BinaryOperatorNode *bin_op)
{
    auto lhs_basic = node_cast<const BasicTypeNode>(bin_op->lhs->inferred_type());
    auto rhs_basic = node_cast<const BasicTypeNode>(bin_op->rhs->inferred_type());

    auto is_invalid_type =
        lhs_basic == nullptr || rhs_basic == nullptr || //
        (lhs_basic->is_numerical() == false && lhs_basic->type_kind != BasicTypeNode::Kind::boolean) ||
        (rhs_basic->is_numerical() == false && rhs_basic->type_kind != BasicTypeNode::Kind::boolean);

    if (is_invalid_type)
    {
        this->error(
            bin_op,
            true,
            std::format(
                "Invalid operand type for binary operator (left: {}, right: {})",
                Node::type_to_string(bin_op->lhs->inferred_type()),
                Node::type_to_string(bin_op->rhs->inferred_type())));
        return &BuiltinTypes::poison;
    }

    auto different_integer_signedness = //
        lhs_basic->type_kind == BasicTypeNode::Kind::signed_integer &&
            rhs_basic->type_kind == BasicTypeNode::Kind::unsigned_integer ||
        lhs_basic->type_kind == BasicTypeNode::Kind::unsigned_integer &&
            rhs_basic->type_kind == BasicTypeNode::Kind::signed_integer;
    if (different_integer_signedness)
    {
        this->error(
            bin_op,
            true,
            std::format(
                "Binary operator operands with different signedness are not supported (left: {}, right: {})",
                Node::type_to_string(bin_op->lhs->inferred_type()),
                Node::type_to_string(bin_op->rhs->inferred_type())));
        return &BuiltinTypes::poison;
    }

    auto is_float = lhs_basic->type_kind == BasicTypeNode::Kind::floatingpoint ||
                    rhs_basic->type_kind == BasicTypeNode::Kind::floatingpoint;
    auto max_size = std::max(lhs_basic->size, rhs_basic->size);

    assert(max_size == 1 || max_size == 2 || max_size == 4 || max_size == 8);

    auto type_kind = is_float ? BasicTypeNode::Kind::floatingpoint : lhs_basic->type_kind;
    BasicTypeNode *type{};
    for (auto [builtin_type, name] : BuiltinTypes::type_names)
    {
        if (builtin_type->type_kind == type_kind && builtin_type->size == max_size)
        {
            type = builtin_type;
        }
    }

    if (this->do_implicit_cast_if_necessary(bin_op->lhs, type) == false)
    {
        UNREACHED;
    }

    if (this->do_implicit_cast_if_necessary(bin_op->rhs, type) == false)
    {
        UNREACHED;
    }

    return type;
}

void TypeChecker::typecheck(Node *node)
{
    this->typecheck_internal(node);
    assert(node->inferred_type() != nullptr);
}

void TypeChecker::typecheck_internal(Node *node)
{
    assert(node != nullptr);

    if (auto decl = node_cast<DeclarationNode>(node);
        decl != nullptr && decl->init_expression->inferred_type() != nullptr)
    {
        // Has already been checked out of order because its identifier has been
        // accessed before the typechecker reached the declaration
        return;
    }

    assert(node->inferred_type() == nullptr);

    switch (node->kind)
    {
        case NodeKind::binary_operator:
        {
            auto bin_op = static_cast<BinaryOperatorNode *>(node);

            this->typecheck(bin_op->lhs);
            this->typecheck(bin_op->rhs);

            if (spread_poison(bin_op->lhs, bin_op))
            {
                return;
            }

            if (spread_poison(bin_op->rhs, bin_op))
            {
                return;
            }

            auto lhs_basic = node_cast<const BasicTypeNode>(bin_op->lhs->inferred_type());
            auto rhs_basic = node_cast<const BasicTypeNode>(bin_op->rhs->inferred_type());

            auto category = bin_op_category(bin_op->operator_kind);
            switch (category)
            {
                case BinaryOperatorCategory::arithmetic:
                {
                    auto coerced_type = this->coerce_types(bin_op);

                    if (Node::types_equal(coerced_type, &BuiltinTypes::poison) == false)
                    {
                        bin_op->set_inferred_type(coerced_type);
                    }

                    return;
                }

                case BinaryOperatorCategory::bitwise_operation:
                {
                    auto is_invalid_type = lhs_basic == nullptr || rhs_basic == nullptr ||
                                           lhs_basic->type_kind != BasicTypeNode::Kind::unsigned_integer ||
                                           rhs_basic->type_kind != BasicTypeNode::Kind::unsigned_integer;
                    if (is_invalid_type)
                    {
                        this->error(
                            bin_op,
                            true,
                            std::format(
                                "Invalid operand type for bitwise binary operator, expected unsigned integer operands (left: {}, right: {})",
                                Node::type_to_string(bin_op->lhs->inferred_type()),
                                Node::type_to_string(bin_op->rhs->inferred_type())));
                        return;
                    }

                    auto max_size = std::max(lhs_basic->size, rhs_basic->size);
                    assert(max_size == 1 || max_size == 2 || max_size == 4 || max_size == 8);

                    auto type = this->ctx.make_basic_type(BasicTypeNode::Kind::unsigned_integer, max_size);

                    bin_op->set_inferred_type(type);

                    if (this->do_implicit_cast_if_necessary(bin_op->lhs, type) == false)
                    {
                        UNREACHED;
                    }

                    if (this->do_implicit_cast_if_necessary(bin_op->rhs, type) == false)
                    {
                        UNREACHED;
                    }

                    return;
                }

                case BinaryOperatorCategory::comparison:
                {
                    auto coerced_type = this->coerce_types(bin_op);
                    // NOTE: Set to boolean regardless of the coercion result because
                    // comparison operators always infer to booleans
                    bin_op->set_inferred_type(&BuiltinTypes::boolean);

                    return;
                }

                case BinaryOperatorCategory::short_circuit_boolean:
                {
                    bin_op->set_inferred_type(&BuiltinTypes::boolean);

                    auto is_invalid_type = lhs_basic == nullptr || rhs_basic == nullptr ||
                                           lhs_basic->type_kind != BasicTypeNode::Kind::boolean ||
                                           rhs_basic->type_kind != BasicTypeNode::Kind::boolean;
                    if (is_invalid_type)
                    {
                        this->error(
                            bin_op,
                            false,
                            std::format(
                                "Invalid non-boolean operand type for boolean short circuit binary operator (left: {}, right: {})",
                                Node::type_to_string(bin_op->lhs->inferred_type()),
                                Node::type_to_string(bin_op->rhs->inferred_type())));
                        return;
                    }

                    return;
                }

                case BinaryOperatorCategory::assignment:
                {
                    bin_op->set_inferred_type(&BuiltinTypes::voyd);

                    if (this->do_implicit_cast_if_necessary(bin_op->rhs, bin_op->lhs->inferred_type()) == false)
                    {
                        this->error(
                            bin_op,
                            false,
                            std::format(
                                "Incompatible operand types for assignment operator (left: {}, right: {})",
                                Node::type_to_string(bin_op->lhs->inferred_type()),
                                Node::type_to_string(bin_op->rhs->inferred_type())));
                        return;
                    }

                    return;
                }

                default: UNREACHED;
            }

            return;
        }

        case NodeKind::block:
        {
            // TODO: Check somehow that the procedure contains a return statement

            auto block = static_cast<BlockNode *>(node);
            block->set_inferred_type(&BuiltinTypes::voyd);

            SET_TEMPORARILY(this->current_block, block);

            for (auto statement : block->statements)
            {
                this->typecheck(statement);
            }

            return;
        }

        case NodeKind::declaration:
        {
            auto decl = static_cast<DeclarationNode *>(node);

            decl->set_inferred_type(&BuiltinTypes::voyd);

            assert(this->current_block != nullptr);

            this->typecheck(decl->init_expression);
            if (decl->init_expression->is_poisoned())
            {
                return;
            }

            if (decl->identifier == "main" && decl->is_global())
            {
                auto proc = node_cast<ProcedureNode>(decl->init_expression);
                if (proc == nullptr || Node::types_equal(&BuiltinTypes::main_signature, proc->signature) == false)
                {
                    this->error(
                        decl,
                        false,
                        std::format(
                            "A global declaration called 'main' must be of type {}, received {}",
                            Node::type_to_string(&BuiltinTypes::main_signature),
                            Node::type_to_string(decl->init_expression->inferred_type())));
                    return;
                }
            }

            if (decl->specified_type->kind != NodeKind::nop)
            {
                if (decl->init_expression->kind == NodeKind::nop)
                {
                    assert(decl->specified_type != nullptr);
                    decl->init_expression->set_inferred_type(decl->specified_type);
                }

                if (this->do_implicit_cast_if_necessary(decl->init_expression, decl->specified_type) == false)
                {
                    this->error(
                        decl,
                        false,
                        std::format(
                            "The type of the declaration initialization expression is not implicitly convertible to the specified type (specified type: {}, init expression type: {})",
                            Node::type_to_string(decl->specified_type),
                            Node::type_to_string(decl->init_expression->inferred_type())));
                    return;
                }
            }

            return;
        }

        case NodeKind::identifier:
        {
            auto ident = static_cast<IdentifierNode *>(node);

            auto decl = this->current_block->find_declaration(ident->identifier);
            if (decl == nullptr)
            {
                this->error(
                    ident,
                    true,
                    std::format("Could not find the declaration of identifier '{}'", ident->identifier));
                return;
            }

            ident->declaration = decl;
            // TODO: Check that the identifier is accessed after the declaration

            // Identifier might be accessed before its declaration has been typechecked
            if (decl->init_expression->inferred_type() == nullptr)
            {
                // NOTE: current_procedure is not set to the according parent procedure,
                // which is fine as long as there can only be global procedures
                SET_TEMPORARILY(this->current_block, decl->containing_block);
                this->typecheck(decl);
            }

            assert(decl->init_expression->inferred_type() != nullptr);
            ident->set_inferred_type(decl->init_expression->inferred_type());

            return;
        }

        case NodeKind::if_statement:
        {
            auto yf = static_cast<IfStatementNode *>(node);
            yf->set_inferred_type(&BuiltinTypes::voyd);

            this->typecheck(yf->condition);
            if (Node::types_equal(yf->condition->inferred_type(), &BuiltinTypes::boolean) == false)
            {
                this->error(
                    yf,
                    false,
                    std::format(
                        "The condition of the if-statement must be of type bool, received {}",
                        Node::type_to_string(yf->condition->inferred_type())));
                return;
            }

            this->typecheck_and_spread_poison(yf->then_block, yf);

            if (yf->else_block != nullptr)
            {
                this->typecheck(yf->else_block);
            }

            return;
        }

        case NodeKind::while_loop:
        {
            auto whyle = static_cast<WhileLoopNode *>(node);
            whyle->set_inferred_type(&BuiltinTypes::voyd);

            this->typecheck(whyle->condition);
            if (Node::types_equal(whyle->condition->inferred_type(), &BuiltinTypes::boolean) == false)
            {
                this->error(
                    whyle,
                    false,
                    std::format(
                        "The condition of the while-loop must be of type bool, received {}",
                        Node::type_to_string(whyle->condition->inferred_type())));
                return;
            }

            this->typecheck(whyle->body);

            return;
        }

        case NodeKind::literal:
        {
            auto literal = static_cast<LiteralNode *>(node);

            if (std::holds_alternative<uint64_t>(literal->value))
            {
                if (literal->suffix == 'u')
                {
                    literal->set_inferred_type(&BuiltinTypes::u64);
                }
                else
                {
                    assert(literal->suffix == '\0');
                    literal->set_inferred_type(&BuiltinTypes::i64);
                }

                return;
            }

            if (std::holds_alternative<float>(literal->value))
            {
                assert(literal->suffix == 'f');
                literal->set_inferred_type(&BuiltinTypes::f32);
                return;
            }

            if (std::holds_alternative<double>(literal->value))
            {
                assert(literal->suffix == '\0');
                literal->set_inferred_type(&BuiltinTypes::f64);
                return;
            }

            if (std::holds_alternative<bool>(literal->value))
            {
                assert(literal->suffix == '\0');
                literal->set_inferred_type(&BuiltinTypes::boolean);
                return;
            }

            if (std::holds_alternative<std::string>(literal->value))
            {
                assert(literal->suffix == '\0');
                literal->set_inferred_type(&BuiltinTypes::string_literal);
                return;
            }

            UNREACHED;
        }

        case NodeKind::procedure:
        {
            auto proc = static_cast<ProcedureNode *>(node);

            assert((proc->body == nullptr) == proc->is_external);

            SET_TEMPORARILY(this->current_procedure, proc);

            this->typecheck_and_spread_poison(proc->signature, proc);

            // NOTE: Need to set the type before typechecking the body because the body might
            // call this procedure recursively
            if (proc->inferred_type() == nullptr)
            {
                proc->set_inferred_type(proc->signature);
            }

            if (proc->is_external == false)
            {
                this->typecheck(proc->body);

                auto got_return = false;
                for (auto statement : proc->body->statements)
                {
                    if (statement->kind == NodeKind::return_statement)
                    {
                        got_return = true;
                    }
                }

                if (got_return == false)
                {
                    if (Node::types_equal(proc->signature->return_type, &BuiltinTypes::voyd) == false)
                    {
                        this->error(proc, false, "Non-void procedure does not return a value");
                        return;
                    }

                // For void procedures, create an implicit return statement if it is missing
                    proc->body->statements.push_back(this->ctx.make_return(nullptr));
                }
            }

            return;
        }

        case NodeKind::procedure_call:
        {
            auto call = static_cast<ProcedureCallNode *>(node);

            if (this->typecheck_and_spread_poison(call->procedure, call))
            {
                return;
            }

            if (call->procedure->inferred_type()->kind != NodeKind::procedure_signature)
            {
                this->error(call, true, "The procedure expression is not callable");
                return;
            }

            auto signature = node_cast<ProcedureSignatureNode, true>(call->procedure->inferred_type());
            call->set_inferred_type(signature->return_type);

            if (signature->is_vararg)
            {
                if (call->arguments.size() < signature->arguments.size())
                {
                    this->error(
                        call,
                        false,
                        std::format(
                            "Expected at least {} arguments for call to variadic procedure (received {})",
                            signature->arguments.size(),
                            call->arguments.size()));
                    return;
                }
            }
            else if (call->arguments.size() != signature->arguments.size())
            {
                this->error(
                    call,
                    false,
                    std::format(
                        "The procedure call does not have the same number of arguments as the procedure signature (expected {}, received {})",
                        call->arguments.size(),
                        signature->arguments.size()));
                return;
            }

            for (auto i = 0; i < call->arguments.size(); ++i)
            {
                this->typecheck(call->arguments[i]);
                if (call->arguments[i]->is_poisoned())
                {
                    continue;
                }

                if (i < signature->arguments.size() &&
                    Node::types_equal(
                        call->arguments[i]->inferred_type(),
                        signature->arguments[i]->init_expression->inferred_type()) == false)
                {
                    this->error(
                        call,
                        false,
                        std::format(
                            "Wrong type passed for argument '{}' (expected: {}, received: {})",
                            signature->arguments[i]->identifier,
                            Node::type_to_string(signature->arguments[i]->init_expression->inferred_type()),
                            Node::type_to_string(call->arguments[i]->inferred_type())));
                }
            }

            return;
        }

        case NodeKind::procedure_signature:
        {
            auto signature = static_cast<ProcedureSignatureNode *>(node);

            for (auto arg : signature->arguments)
            {
                this->typecheck_and_spread_poison(arg, signature);
            }

            if (signature->inferred_type() == nullptr)
            {
                signature->set_inferred_type(&BuiltinTypes::type);
            }

            return;
        }

        case NodeKind::return_statement:
        {
            auto retyrn = static_cast<ReturnStatementNode *>(node);
            retyrn->set_inferred_type(&BuiltinTypes::voyd);

            this->typecheck(retyrn->expression);
            if (retyrn->expression->is_poisoned())
            {
                return;
            }

            if (Node::types_equal(this->current_procedure->signature->return_type, &BuiltinTypes::voyd) == false &&
                this->do_implicit_cast_if_necessary(
                    retyrn->expression,
                    this->current_procedure->signature->return_type) == false)
            {
                this->error(
                    retyrn,
                    false,
                    std::format(
                        "The type of the return expression is not implicitly convertible to the procedure return type (return type of procedure: {}, inferred type of return expression: {})",
                        Node::type_to_string(this->current_procedure->signature->return_type),
                        Node::type_to_string(retyrn->expression->inferred_type())));
                return;
            }

            return;
        }

        case NodeKind::module:
        {
            auto module = static_cast<ModuleNode *>(node);
            module->set_inferred_type(&BuiltinTypes::voyd);

            this->typecheck(module->block);

            return;
        }

        case NodeKind::basic_type:
        case NodeKind::pointer_type:
        case NodeKind::array_type:
        case NodeKind::struct_type:
        {
            node->set_inferred_type(&BuiltinTypes::type);

            return;
        }

        case NodeKind::nop:
        {
            node->set_inferred_type(node);

            return;
        }

        case NodeKind::type_cast:
        {
            auto cast = static_cast<TypeCastNode *>(node);
            cast->set_inferred_type(cast->target_type);

            assert(cast->target_type != nullptr);

            // TODO: Pointer casting etc.

            auto src_basic    = node_cast<BasicTypeNode, true>(cast->expression->inferred_type());
            auto target_basic = node_cast<BasicTypeNode, true>(cast->target_type);

            if (src_basic->is_numerical() == false || target_basic->is_numerical() == false)
            {
                this->error(cast, false, "TODO: Currently only casts between numerical types are implemented");
                return;
            }
        }

        case NodeKind::goto_statement:
        {
            // TODO: Check that the label is found

            node->set_inferred_type(&BuiltinTypes::voyd);

            return;
        }

        case NodeKind::break_statement:
        {
            // TODO: Check that we are in a loop/switch

            node->set_inferred_type(&BuiltinTypes::voyd);

            return;
        }

        case NodeKind::continue_statement:
        {
            // TODO: Check that we are in a loop

            node->set_inferred_type(&BuiltinTypes::voyd);

            return;
        }

        case NodeKind::label:
        {
            auto label = static_cast<LabelNode *>(node);

            label->set_inferred_type(&BuiltinTypes::voyd);

            return;
        }
    }

    UNREACHED;
}

bool TypeChecker::typecheck_and_spread_poison(Node *node, Node *parent)
{
    assert(node->inferred_type() == nullptr);
    // assert(
    //     (parent->inferred_type() == nullptr ||
    //      Node::types_equal(parent->inferred_type(), &BuiltinTypes::poison) == false) &&
    //     "Already poisoned");

    this->typecheck(node);
    if (spread_poison(node, parent))
    {
        return true;
    }

    return false;
}

void TypeChecker::error(Node *node, bool poison, std::string_view message)
{
    if (poison)
    {
        node->set_inferred_type(&BuiltinTypes::poison);
    }

    // TODO: Print a string representation of the node for context
    this->errors.push_back(std::format("Type error: {}", message));
}
