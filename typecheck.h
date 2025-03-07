#pragma once

#include "lex.h"
#include <cstddef>
#include <cstdint>
#include <vector>

enum class NodeKind
{
    binary_operator,
    block,
    declaration,
    identifier,
    if_branch,
    literal,
    procedure,
    procedure_call,
    procedure_signature,
    program,
    return_statement,
    simple_type,
};

struct Type
{
    enum class Kind
    {
        unchecked,
        voyd,
        boolean,
        integer,
        floatingpoint,
        strukt,
    };

    Kind kind{};
    int64_t size = -1;
    bool is_signed{};

    inline bool is_valid() const { return this->size > 0 || this->kind == Kind::voyd; }
    inline bool is_numerical() const { return this->kind == Kind::integer || this->kind == Kind::floatingpoint; }

    auto operator<=>(const Type &) const = default;
};

struct BuiltinTypes
{
    constexpr static Type voyd    = Type{.kind = Type::Kind::voyd};
    constexpr static Type i64     = Type{.kind = Type::Kind::integer, .size = 8, .is_signed = true};
    constexpr static Type i32     = Type{.kind = Type::Kind::integer, .size = 4, .is_signed = true};
    constexpr static Type i16     = Type{.kind = Type::Kind::integer, .size = 2, .is_signed = true};
    constexpr static Type i8      = Type{.kind = Type::Kind::integer, .size = 1, .is_signed = true};
    constexpr static Type u64     = Type{.kind = Type::Kind::integer, .size = 8, .is_signed = false};
    constexpr static Type u32     = Type{.kind = Type::Kind::integer, .size = 4, .is_signed = false};
    constexpr static Type u16     = Type{.kind = Type::Kind::integer, .size = 2, .is_signed = false};
    constexpr static Type u8      = Type{.kind = Type::Kind::integer, .size = 1, .is_signed = false};
    constexpr static Type boolean = Type{.kind = Type::Kind::boolean, .size = 1};
};

struct Node
{
    NodeKind kind{};
    Type type{};

    explicit Node(NodeKind kind)
        : kind{kind}
    {
    }

    virtual ~Node() = default;
};


template<typename TNode>
TNode *node_cast(Node *node)
{
    if (node == nullptr)
    {
        return nullptr;
    }

    if (node->kind != TNode::kind)
    {
        return nullptr;
    }

    return static_cast<TNode *>(node);
}

template<NodeKind the_kind>
struct NodeHelper : Node
{
    constexpr static NodeKind kind = the_kind;

    NodeHelper()
        : Node(the_kind)
    {
    }
};

struct BinaryOperatorNode : NodeHelper<NodeKind::binary_operator>
{
    TokenType operator_type{};
    Node *lhs{};
    Node *rhs{};
};

struct BlockNode : public NodeHelper<NodeKind::block>
{
    std::vector<Node *> statements{};
    BlockNode *parent_block{};
    int64_t offset_from_parent_block{};
    int64_t memory_size{};  // Size of all local variables
    // int64_t memory_size_of_args{};  // Size of all procedure arguments

    inline bool is_global() const { return this->parent_block == nullptr; }

    struct DeclarationNode *find_declaration(std::string_view name);
};

struct DeclarationNode : NodeHelper<NodeKind::declaration>
{
    std::string_view identifier;
    Node *init_expression;
};

struct IdentifierNode : NodeHelper<NodeKind::identifier>
{
    std::string_view identifier;
};

struct IfNode : NodeHelper<NodeKind::identifier>
{
    Node *condition;
    BlockNode *then_block;
    BlockNode *else_block;
};

struct LiteralNode : NodeHelper<NodeKind::literal>
{
    int64_t signed_integer_value{};
    uint64_t unsigned_integer_value{};  // TODO: Parse
    float float_value{};  // TODO: Parse
    double double_value{};  // TODO: Parse
};

Node *typecheck(BlockNode *containing_block, struct AstNode *ast);
