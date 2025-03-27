#include "context.h"

void *operator new(size_t size, Context &context)
{
    return context.pool.allocate(size);
}

// TODO: Do some assertions for the Node* arguments (is statement, type, ...)

BinaryOperatorNode *Context::make_binary_operator(TokenType operator_kind, Node *lhs, Node *rhs)
{
    assert(lhs != nullptr);
    assert(rhs != nullptr);

    auto result           = new (*this) BinaryOperatorNode{};
    result->operator_kind = operator_kind;
    result->lhs           = lhs;
    result->rhs           = rhs;
    return result;
}

BlockNode *Context::make_block(BlockNode *parent_block, std::vector<Node *> statements)
{
    auto result          = new (*this) BlockNode{};
    result->parent_block = parent_block;
    result->statements   = std::move(statements);
    return result;
}

DeclarationNode *Context::make_declaration(std::string_view identifier, Node *specified_type, Node *init_expression)
{
    assert(identifier.empty() == false);
    assert((specified_type != nullptr) || (init_expression != nullptr));

    if (specified_type == nullptr)
    {
        specified_type = this->make_nop();
    }

    if (init_expression == nullptr)
    {
        init_expression = this->make_nop();
    }

    auto result             = new (*this) DeclarationNode{};
    result->identifier      = identifier;
    result->specified_type  = specified_type;
    result->init_expression = init_expression;
    return result;
}

IdentifierNode *Context::make_identifier(std::string_view identifier)
{
    assert(identifier.empty() == false);

    auto result        = new (*this) IdentifierNode{};
    result->identifier = identifier;
    return result;
}

IfNode *Context::make_if(Node *condition, BlockNode *then_block, BlockNode *else_block)
{
    assert(condition != nullptr);
    assert(then_block != nullptr);

    auto result        = new (*this) IfNode{};
    result->condition  = condition;
    result->then_block = then_block;
    result->else_block = else_block;
    return result;
}

LiteralNode *Context::make_literal(std::variant<bool, uint64_t, float, double> value, char suffix)
{
    assert(std::holds_alternative<bool>(value) == false || suffix == '\0');
    assert(std::holds_alternative<float>(value) == false || suffix == 'f');
    assert(std::holds_alternative<double>(value) == false || suffix == '\0');

    auto result    = new (*this) LiteralNode{};
    result->value  = value;
    result->suffix = suffix;
    return result;
}

LiteralNode *Context::make_bool_literal(bool value)
{
    auto result   = new (*this) LiteralNode{};
    result->value = value;
    return result;
}

LiteralNode *Context::make_sint_literal(uint64_t value)
{
    auto result   = new (*this) LiteralNode{};
    result->value = value;
    return result;
}

LiteralNode *Context::make_uint_literal(uint64_t value)
{
    auto result    = new (*this) LiteralNode{};
    result->value  = value;
    result->suffix = 'u';
    return result;
}

LiteralNode *Context::make_float_literal(float value)
{
    auto result    = new (*this) LiteralNode{};
    result->value  = value;
    result->suffix = 'f';
    return result;
}

LiteralNode *Context::make_double_literal(double value)
{
    auto result   = new (*this) LiteralNode{};
    result->value = value;
    return result;
}

ModuleNode *Context::make_module(BlockNode *block)
{
    assert(block != nullptr);

    auto result   = new (*this) ModuleNode{};
    result->block = block;
    return result;
}

ModuleNode *Context::make_module(std::vector<DeclarationNode *> declarations)
{
    auto result = new (*this) ModuleNode{};

    std::vector<Node *> statements{};
    for (auto decl : declarations)
    {
        statements.push_back(decl);
    }

    result->block = this->make_block(nullptr, std::move(statements));
    return result;
}

ProcedureNode *Context::make_procedure(ProcedureSignatureNode *signature, BlockNode *body, bool is_external)
{
    assert(signature != nullptr);
    assert((body == nullptr) == is_external);

    auto result         = new (*this) ProcedureNode{};
    result->signature   = signature;
    result->body        = body;
    result->is_external = is_external;
    return result;
}

ProcedureCallNode *Context::make_procedure_call(Node *procedure, std::vector<Node *> arguments)
{
    assert(procedure != nullptr);

    auto result       = new (*this) ProcedureCallNode{};
    result->procedure = procedure;
    result->arguments = std::move(arguments);
    return result;
}

ProcedureSignatureNode *Context::make_procedure_signature(std::vector<DeclarationNode *> arguments, Node *return_type)
{
    assert(return_type != nullptr);

    auto result         = new (*this) ProcedureSignatureNode{};
    result->arguments   = std::move(arguments);
    result->return_type = return_type;
    return result;
}

ReturnNode *Context::make_return(Node *expression)
{
    if (expression == nullptr)
    {
        expression = this->make_nop();
    }

    auto result        = new (*this) ReturnNode{};
    result->expression = expression;
    return result;
}

TypeCastNode *Context::make_type_cast(Node *type, Node *expression)
{
    assert(type != nullptr);
    assert(expression != nullptr);

    auto result        = new (*this) TypeCastNode{};
    result->type       = type;
    result->expression = expression;
    return result;
}

BasicTypeNode *Context::make_basic_type(BasicTypeNode::Kind kind, int64_t size)
{
    auto result = new (*this) BasicTypeNode{kind, size};
    return result;
}

PointerTypeNode *Context::make_pointer_type(Node *target_type)
{
    assert(target_type != nullptr);

    auto result         = new (*this) PointerTypeNode{};
    result->target_type = target_type;
    return result;
}

ArrayTypeNode *Context::make_array_type(Node *length, Node *element_type)
{
    assert(length != nullptr);
    assert(element_type != nullptr);

    auto result          = new (*this) ArrayTypeNode{};
    result->length       = length;
    result->element_type = element_type;
    return result;
}

NopNode *Context::make_nop()
{
    auto result = new (*this) NopNode{};
    return result;
}
