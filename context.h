#pragma once

#include "memory_pool.h"
#include "node.h"

struct Context
{
    constexpr static size_t pool_size = 8 * 1024 * 1024;

    MemoryPool pool{pool_size};

    BinaryOperatorNode *make_binary_operator(TokenType operator_kind, Node *lhs, Node *rhs);
    BlockNode *make_block(BlockNode *parent_block, std::vector<Node *> statements);
    DeclarationNode *make_declaration(
        std::string_view identifier,
        Node *specified_type,
        Node *init_expression,
        bool is_procedure_argument);
    IdentifierNode *make_identifier(std::string_view identifier);
    IfStatementNode *make_if(Node *condition, BlockNode *then_block, BlockNode *else_block);
    WhileLoopNode *make_while(Node *condition, BlockNode *block);
    BreakStatementNode *make_break();
    ContinueStatementNode *make_continue();
    LiteralNode *make_literal(std::variant<bool, uint64_t, float, double, std::string> value, char suffix);
    LiteralNode *make_bool_literal(bool value);
    LiteralNode *make_sint_literal(uint64_t value);
    LiteralNode *make_uint_literal(uint64_t value);
    LiteralNode *make_float_literal(float value);
    LiteralNode *make_double_literal(double value);
    LiteralNode *make_string_literal(std::string value);
    ModuleNode *make_module(BlockNode *block);
    ModuleNode *make_module(std::vector<DeclarationNode *> declarations);
    ProcedureNode *make_procedure(ProcedureSignatureNode *signature, BlockNode *body, bool is_external);
    ProcedureCallNode *make_procedure_call(Node *procedure, std::vector<Node *> arguments);
    ProcedureSignatureNode *make_procedure_signature(
        std::vector<DeclarationNode *> arguments,
        bool is_vararg,
        Node *return_type);
    ReturnStatementNode *make_return(Node *expression);
    GotoStatementNode *make_goto(std::string_view label_identifier);
    LabelNode *make_label(std::string_view identifier);
    TypeCastNode *make_type_cast(Node *target_type, Node *expression);
    BasicTypeNode *make_basic_type(BasicTypeNode::Kind kind, int64_t size);
    PointerTypeNode *make_pointer_type(Node *target_type);
    ArrayTypeNode *make_array_type(Node *length, Node *element_type); // StructTypeNode *make_struct_type();
    NopNode *make_nop();
};

void *operator new(size_t size, Context &context);
