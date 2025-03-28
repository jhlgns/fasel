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

DeclarationNode *BlockNode::find_declaration(std::string_view name, bool recurse) const
{
    auto it = this->declarations.find(std::string{name});
    if (it != this->declarations.end())
    {
        return it->second;
    }

    if (recurse && this->parent_block != nullptr)
    {
        return this->parent_block->find_declaration(name);
    }

    return nullptr;
}

Node *TypeChecker::make_node(AstNode *ast)
{
    switch (ast->kind)
    {
        case AstKind::binary_operator:
        {
            auto bin_op = static_cast<AstBinaryOperator *>(ast);

            auto lhs = this->make_node(bin_op->lhs);
            if (lhs == nullptr)
            {
                return nullptr;
            }

            auto rhs = this->make_node(bin_op->rhs);
            if (rhs == nullptr)
            {
                return nullptr;
            }

            return this->ctx.make_binary_operator(bin_op->type, lhs, rhs);
        }

        case AstKind::block:
        {
            auto block = static_cast<AstBlock *>(ast);

            auto result = this->ctx.make_block(this->current_block, {});

            auto old_block      = this->current_block;
            this->current_block = result;
            defer
            {
                this->current_block = old_block;
            };

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

            return this->ctx.make_declaration(decl->identifier.text(), specified_type, init_expr);
        }

        case AstKind::identifier:
        {
            auto ident = static_cast<AstIdentifier *>(ast);
            return this->ctx.make_identifier(ident->identifier.text());
        }

        case AstKind::if_statement:
        {
            auto yf = static_cast<AstIf *>(ast);

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
            auto retyrn = static_cast<AstReturn *>(ast);

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
            auto gotoo = static_cast<AstGoto *>(ast);
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

bool TypeChecker::typecheck(Node *node)
{
    // NOTE: Only set the type if the node is an expression, do not assign void to statements

    // TODO: Better error messages - print the inferred types if they are wrong so the user knows what is going on

    assert(node != nullptr);
    assert(node->type == nullptr);

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

            auto lhs_basic = node_cast<const BasicTypeNode>(bin_op->lhs->type);
            auto rhs_basic = node_cast<const BasicTypeNode>(bin_op->rhs->type);

            auto category = bin_op_category(bin_op->operator_kind);
            switch (category)
            {
                case BinaryOperatorCategory::arithmetic:
                {
                    if (lhs_basic == nullptr)
                    {
                        this->error(bin_op, "Invalid kind of type for left operand");
                        return false;
                    }

                    if (rhs_basic == nullptr)
                    {
                        this->error(bin_op, "Invalid kind of type for right operand");
                        return false;
                    }

                    if (lhs_basic->is_numerical() == false || rhs_basic->is_numerical() == false)
                    {
                        this->error(bin_op, "Arithmetic binary operators require numerical expressions on both sides");
                        return false;
                    }

                    auto invalid_integer_types = lhs_basic->type_kind == BasicTypeNode::Kind::signed_integer &&
                                                     rhs_basic->type_kind == BasicTypeNode::Kind::unsigned_integer ||
                                                 lhs_basic->type_kind == BasicTypeNode::Kind::unsigned_integer &&
                                                     rhs_basic->type_kind == BasicTypeNode::Kind::signed_integer;
                    if (invalid_integer_types)
                    {
                        this->error(
                            bin_op,
                            "Arithmetic on different signedness is not supported. You must cast the operands to the same type.");
                        return false;
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

                    ENSURE(type != nullptr);

                    bin_op->type = type;

                    if (is_float)
                    {
                        if (types_equal(type, lhs_basic) == false)
                        {
                            bin_op->lhs = this->ctx.make_type_cast(type, bin_op->lhs);
                        }

                        if (types_equal(type, rhs_basic) == false)
                        {
                            bin_op->rhs = this->ctx.make_type_cast(type, bin_op->rhs);
                        }
                    }

                    return true;
                }

                case BinaryOperatorCategory::bitwise_operation:
                {
                    if (lhs_basic == nullptr)
                    {
                        this->error(bin_op, "Invalid kind of type for left operand");
                        return false;
                    }

                    if (rhs_basic == nullptr)
                    {
                        this->error(bin_op, "Invalid kind of type for right operand");
                        return false;
                    }

                    if (lhs_basic->type_kind != BasicTypeNode::Kind::unsigned_integer ||
                        rhs_basic->type_kind != BasicTypeNode::Kind::unsigned_integer)
                    {
                        this->error(bin_op, "Bitwise binary operators require unsigned integer operands on both sides");
                        return false;
                    }

                    auto max_size = std::max(lhs_basic->size, rhs_basic->size);
                    assert(max_size == 1 || max_size == 2 || max_size == 4 || max_size == 8);

                    auto type = new BasicTypeNode{BasicTypeNode::Kind::unsigned_integer, max_size};

                    bin_op->type = type;

                    return true;
                }

                case BinaryOperatorCategory::comparison:
                {
                    // TODO

                    if (types_equal(bin_op->lhs->type, bin_op->rhs->type) == false)
                    {
                        this->error(bin_op, "Only expressions of the same time can be compared");
                        return false;
                    }

                    bin_op->type = &BuiltinTypes::boolean;

                    return true;
                }

                case BinaryOperatorCategory::short_circuit_boolean:
                {
                    if (lhs_basic == nullptr)
                    {
                        this->error(bin_op, "Invalid type for left operand");
                        return false;
                    }

                    if (rhs_basic == nullptr)
                    {
                        this->error(bin_op, "Invalid type for right operand");
                        return false;
                    }

                    if (lhs_basic->type_kind != BasicTypeNode::Kind::boolean)
                    {
                        this->error(bin_op, "Left side is not a boolean expression");
                        return false;
                    }

                    if (lhs_basic->type_kind != BasicTypeNode::Kind::boolean)
                    {
                        this->error(bin_op, "Right side is not a boolean expression");
                        return false;
                    }

                    bin_op->type = &BuiltinTypes::boolean;

                    return true;
                }

                case BinaryOperatorCategory::assignment:
                {
                    if (types_equal(bin_op->lhs->type, bin_op->rhs->type) == false)
                    {
                        this->error(
                            bin_op,
                            std::format(
                                "Assignment type mismatch (left side {} vs. right side {})",
                                type_to_string(bin_op->lhs->type),
                                type_to_string(bin_op->rhs->type)));
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
            // TODO: Check somehow that the procedure contains a return statement

            auto block = static_cast<BlockNode *>(node);

            auto old_block      = this->current_block;
            this->current_block = block;
            defer
            {
                this->current_block = old_block;
            };

            for (auto statement : block->statements)
            {
                if (auto decl = node_cast<DeclarationNode>(statement); decl != nullptr)
                {
                    auto [it, ok] = this->current_block->declarations.emplace(std::string{decl->identifier}, decl);
                    if (ok == false)
                    {
                        this->error(decl, std::format("Duplicate declaration of '{}'", decl->identifier));
                        return false;
                    }
                }

                if (this->current_procedure != nullptr)
                {
                    // NOTE: While typechecking the module root block, current_procedure_body is nullptr
                    statement->time = ++this->current_procedure->body->current_time;
                }

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

            assert(this->current_block != nullptr);

            if (typecheck(decl->init_expression) == false)
            {
                return false;
            }

            if (decl->identifier == "main" && this->current_block->is_global())
            {
                auto proc = node_cast<ProcedureNode>(decl->init_expression);
                if (proc == nullptr || types_equal(&BuiltinTypes::main_signature, proc->signature) == false)
                {
                    this->error(
                        proc,
                        std::format(
                            "A global declaration called 'main' must be of type {}, received {}",
                            type_to_string(&BuiltinTypes::main_signature),
                            type_to_string(decl->init_expression->type)));
                    return false;
                }
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
                    this->error(
                        decl,
                        std::format(
                            "The declaration init expression does not have the same inferred type as the specified type on the declaration (specified type: {}; inferred type: {})",
                            type_to_string(decl->specified_type),
                            type_to_string(decl->init_expression->type)));
                    return false;
                }
            }

            return true;
        }

        case NodeKind::identifier:
        {
            auto ident = static_cast<IdentifierNode *>(node);
            auto decl  = this->current_block->find_declaration(ident->identifier);

            if (decl == nullptr)
            {
                this->error(ident, std::format("Could not find the declaration of identifier '{}'", ident->identifier));
                return false;
            }

            // TODO: The time checking does not work yet, because the time is not assigned
            // recursively in expressions - only the top level block statements get a time
            // assert(ident->time >= 0 && decl->time >= 0);
            //
            // if (ident->time < decl->time)
            // {
            //     this->error(ident, std::format("Identifier '{}' referenced before its declaration", ident->identifier));
            //     return false;
            // }

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

            if (types_equal(yf->condition->type, &BuiltinTypes::boolean) == false)
            {
                this->error(
                    yf,
                    std::format(
                        "The condition of the if-statement must be of type bool, received {}",
                        type_to_string(yf->condition->type)));
                return false;
            }

            if (typecheck(yf->then_block) == false)
            {
                return false;
            }

            if (yf->else_block != nullptr && typecheck(yf->else_block) == false)
            {
                return false;
            }

            return true;
        }

        case NodeKind::while_loop:
        {
            auto whyle = static_cast<WhileLoopNode *>(node);

            if (typecheck(whyle->condition) == false)
            {
                return false;
            }

            if (types_equal(whyle->condition->type, &BuiltinTypes::boolean) == false)
            {
                this->error(
                    whyle,
                    std::format(
                        "The condition of the while-loop must be of type bool, received {}",
                        type_to_string(whyle->condition->type)));
                return false;
            }

            if (typecheck(whyle->block) == false)
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
                    assert(literal->suffix == '\0');
                    literal->type = &BuiltinTypes::i64;
                }

                return true;
            }

            if (std::holds_alternative<float>(literal->value))
            {
                assert(literal->suffix == 'f');
                literal->type = &BuiltinTypes::f32;
                return true;
            }

            if (std::holds_alternative<double>(literal->value))
            {
                assert(literal->suffix == '\0');
                literal->type = &BuiltinTypes::f64;
                return true;
            }

            if (std::holds_alternative<bool>(literal->value))
            {
                assert(literal->suffix == '\0');
                literal->type = &BuiltinTypes::boolean;
                return true;
            }

            if (std::holds_alternative<std::string>(literal->value))
            {
                assert(literal->suffix == '\0');
                literal->type = &BuiltinTypes::string_literal;
                return true;
            }

            UNREACHED;
            return false;
        }

        case NodeKind::procedure:
        {
            auto proc = static_cast<ProcedureNode *>(node);

            assert((proc->body == nullptr) == proc->is_external);

            auto old_procedure      = this->current_procedure;
            this->current_procedure = proc;
            defer
            {
                this->current_procedure = old_procedure;
            };

            if (typecheck(proc->signature) == false)
            {
                return false;
            }

            // NOTE: Need to set the type before typechecking the body because the body might
            // call this procedure recursively
            proc->type = proc->signature;

            if (proc->is_external == false)
            {
                for (auto arg : proc->signature->arguments)
                {
                    auto [it, ok] = proc->body->declarations.emplace(std::string{arg->identifier}, arg);
                    if (ok == false)
                    {
                        this->error(arg, std::format("Duplicate argument name '{}'", arg->identifier));
                        return false;
                    }
                }

                if (typecheck(proc->body) == false)
                {
                    return false;
                }
            }

            return true;
        }

        case NodeKind::procedure_call:
        {
            auto call = static_cast<ProcedureCallNode *>(node);

            if (typecheck(call->procedure) == false)
            {
                return false;
            }

            if (call->procedure->type->kind != NodeKind::procedure_signature)
            {
                this->error(node, "The procedure expression is not callable");
                return false;
            }

            auto signature = node_cast<ProcedureSignatureNode, true>(call->procedure->type);

            if (signature->is_vararg)
            {
                if (call->arguments.size() < signature->arguments.size())
                {
                    this->error(
                        node,
                        std::format(
                            "Expected at least {} arguments for variadic procedure call",
                            signature->arguments.size()));
                    return false;
                }
            }
            else if (call->arguments.size() != signature->arguments.size())
            {
                this->error(
                    node,
                    "The procedure call does not have the same number of arguments as the procedure signature");
                return false;
            }

            for (auto i = 0; i < call->arguments.size(); ++i)
            {
                if (typecheck(call->arguments[i]) == false)
                {
                    return false;
                }

                if (i < signature->arguments.size() &&
                    types_equal(call->arguments[i]->type, signature->arguments[i]->init_expression->type) == false)
                {
                    this->error(
                        node,
                        std::format(
                            "Wrong type passed for argument '{}' (expected: {}, received: {})",
                            signature->arguments[i]->identifier,
                            type_to_string(signature->arguments[i]->init_expression->type),
                            type_to_string(call->arguments[i]->type)));
                    return false;
                }
            }

            call->type = signature->return_type;

            return true;
        }

        case NodeKind::procedure_signature:
        {
            auto signature = static_cast<ProcedureSignatureNode *>(node);

            for (auto arg : signature->arguments)
            {
                if (typecheck(arg) == false)
                {
                    return false;
                }
            }

            signature->type = &BuiltinTypes::type;

            return true;
        }

        case NodeKind::return_statement:
        {
            auto retyrn = static_cast<ReturnNode *>(node);

            if (retyrn->expression->kind == NodeKind::nop)
            {
                return true;
            }
            else if (typecheck(retyrn->expression) == false)
            {
                return false;
            }

            if (types_equal(retyrn->expression->type, this->current_procedure->signature->return_type) == false)
            {
                this->error(
                    retyrn,
                    std::format(
                        "The type of the return expression does not match the procedure return type (expected {}, received {})",
                        type_to_string(this->current_procedure->signature->return_type),
                        type_to_string(retyrn->expression->type)));
                return false;
            }

            return true;
        }

        case NodeKind::module:
        {
            auto module = static_cast<ModuleNode *>(node);

            if (typecheck(module->block) == false)
            {
                return false;
            }

            return true;
        }

        case NodeKind::basic_type:
        case NodeKind::pointer_type:
        case NodeKind::array_type:
        case NodeKind::struct_type:
        {
            node->type = &BuiltinTypes::type;

            return true;
        }

        case NodeKind::nop:
        {
            node->type = node;  // TODO: Smelly

            return true;
        }

        case NodeKind::type_cast:
        {
            auto cast = static_cast<TypeCastNode *>(node);

            assert(cast->type != nullptr);

            // TODO: Pointer casting etc.

            auto src_basic  = node_cast<BasicTypeNode, true>(cast->expression->type);
            auto dest_basic = node_cast<BasicTypeNode, true>(cast->type);

            if (src_basic->is_numerical() == false || dest_basic->is_numerical() == false)
            {
                this->error(cast, "TODO: Currently only casts between numerical types are implemented");
                return false;
            }

            // cast->type is already set
        }

        case NodeKind::goto_statement:
        {
            // TODO: Check that the label is found

            return true;
        }

        case NodeKind::break_statement:
        {
            // TODO: Check that we are in a loop/switch

            return true;
        }

        case NodeKind::continue_statement:
        {
            // TODO: Check that we are in a loop

            return true;
        }

        case NodeKind::label:
        {
            auto label = static_cast<LabelNode *>(node);

            auto decl     = this->ctx.make_declaration(label->identifier, &BuiltinTypes::label, label);
            auto [it, ok] = this->current_procedure->body->declarations.emplace(std::string{label->identifier}, decl);
            if (ok == false)
            {
                this->error(label, "A declaration with identifier '{}' already exists in this procedure's body");
                return false;
            }

            return true;
        }
    }

    UNREACHED;
}

void TypeChecker::error(const Node *node, std::string_view message)
{
    // TODO: Print a string representation of the node for context
    this->errors.push_back(std::format("Type error: {}", message));
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
        case NodeKind::basic_type:
        {
            auto lhs_basic = static_cast<const BasicTypeNode *>(lhs);
            auto rhs_basic = static_cast<const BasicTypeNode *>(rhs);

            return lhs_basic->type_kind == rhs_basic->type_kind && lhs_basic->size == rhs_basic->size;
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
            auto lhs_length_literal = node_cast<LiteralNode>(lhs_array->length);
            auto rhs_length_literal = node_cast<LiteralNode>(rhs_array->length);

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

        case NodeKind::procedure_signature:
        {
            auto lhs_signature = static_cast<const ProcedureSignatureNode *>(lhs);
            auto rhs_signature = static_cast<const ProcedureSignatureNode *>(rhs);

            if (lhs_signature->is_vararg != rhs_signature->is_vararg)
            {
                return false;
            }

            if (types_equal(lhs_signature->return_type, rhs_signature->return_type) == false)
            {
                return false;
            }

            if (lhs_signature->arguments.size() != rhs_signature->arguments.size())
            {
                return false;
            }

            for (auto i = 0; i < lhs_signature->arguments.size(); ++i)
            {
                auto lhs_type = lhs_signature->arguments[i]->init_expression->type;
                auto rhs_type = rhs_signature->arguments[i]->init_expression->type;

                if (types_equal(lhs_type, rhs_type) == false)
                {
                    return false;
                }
            }

            return true;
        }

        case NodeKind::struct_type:
        {
            // TODO
            UNREACHED;
        }

        default: UNREACHED;
    }
}

std::string type_to_string(const Node *type /*, BlockNode *block */)
{
    switch (type->kind)
    {
        case NodeKind::procedure_signature:
        {
            auto signature     = static_cast<const ProcedureSignatureNode *>(type);
            std::string result = "proc(";

            for (auto i = 0; i < signature->arguments.size(); ++i)
            {
                result += type_to_string(signature->arguments[i]->init_expression->type);

                if (i + 1 < signature->arguments.size())
                {
                    result += ", ";
                }
            }

            result += ") ";
            result += type_to_string(signature->return_type);

            return result;
        }

        case NodeKind::basic_type:
        {
            auto simple_type = static_cast<const BasicTypeNode *>(type);

            for (auto [builtin_type, name] : BuiltinTypes::type_names)
            {
                if (types_equal(builtin_type, type))
                {
                    return std::string{name};
                }
            }

            // TODO: Custom type names
            UNREACHED;
        }

        case NodeKind::pointer_type:
        {
            auto pointer_type = static_cast<const PointerTypeNode *>(type);
            return "*" + type_to_string(pointer_type->target_type);
        }

        case NodeKind::array_type:
        {
            auto array_type = static_cast<const ArrayTypeNode *>(type);
            // TODO
            return "[...]" + type_to_string(array_type->element_type);
        }

        case NodeKind::struct_type:
        {
            // TODO
            UNREACHED;
        }

        default: UNREACHED;
    }
}

BasicTypeNode BuiltinTypes::voyd    = BasicTypeNode{BasicTypeNode::Kind::voyd, -1};
BasicTypeNode BuiltinTypes::i64     = BasicTypeNode{BasicTypeNode::Kind::signed_integer, 8};
BasicTypeNode BuiltinTypes::i32     = BasicTypeNode{BasicTypeNode::Kind::signed_integer, 4};
BasicTypeNode BuiltinTypes::i16     = BasicTypeNode{BasicTypeNode::Kind::signed_integer, 2};
BasicTypeNode BuiltinTypes::i8      = BasicTypeNode{BasicTypeNode::Kind::signed_integer, 1};
BasicTypeNode BuiltinTypes::u64     = BasicTypeNode{BasicTypeNode::Kind::unsigned_integer, 8};
BasicTypeNode BuiltinTypes::u32     = BasicTypeNode{BasicTypeNode::Kind::unsigned_integer, 4};
BasicTypeNode BuiltinTypes::u16     = BasicTypeNode{BasicTypeNode::Kind::unsigned_integer, 2};
BasicTypeNode BuiltinTypes::u8      = BasicTypeNode{BasicTypeNode::Kind::unsigned_integer, 1};
BasicTypeNode BuiltinTypes::f32     = BasicTypeNode{BasicTypeNode::Kind::floatingpoint, 4};
BasicTypeNode BuiltinTypes::f64     = BasicTypeNode{BasicTypeNode::Kind::floatingpoint, 8};
BasicTypeNode BuiltinTypes::boolean = BasicTypeNode{BasicTypeNode::Kind::boolean, 1};
BasicTypeNode BuiltinTypes::type    = BasicTypeNode{BasicTypeNode::Kind::type, -1};
BasicTypeNode BuiltinTypes::label   = BasicTypeNode{BasicTypeNode::Kind::label, -1};

PointerTypeNode BuiltinTypes::string_literal = []
{
    PointerTypeNode result{};
    result.target_type = &BuiltinTypes::i8;
    return result;
}();

ProcedureSignatureNode BuiltinTypes::main_signature = []
{
    ProcedureSignatureNode result;
    result.return_type = &BuiltinTypes::voyd;
    return result;
}();

const std::vector<std::tuple<BasicTypeNode *, std::string_view>> BuiltinTypes::type_names = {
    std::make_tuple(&BuiltinTypes::voyd, "void"),
    std::make_tuple(&BuiltinTypes::i64, "i64"),
    std::make_tuple(&BuiltinTypes::i32, "i32"),
    std::make_tuple(&BuiltinTypes::i16, "i16"),
    std::make_tuple(&BuiltinTypes::i8, "i8"),
    std::make_tuple(&BuiltinTypes::u64, "u64"),
    std::make_tuple(&BuiltinTypes::u32, "u32"),
    std::make_tuple(&BuiltinTypes::u16, "u16"),
    std::make_tuple(&BuiltinTypes::u8, "u8"),
    std::make_tuple(&BuiltinTypes::f32, "f32"),
    std::make_tuple(&BuiltinTypes::f64, "f64"),
    std::make_tuple(&BuiltinTypes::boolean, "bool"),
    std::make_tuple(&BuiltinTypes::type, "type"),
    std::make_tuple(&BuiltinTypes::label, "label"),
};
