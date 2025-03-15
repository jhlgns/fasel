#pragma once

#include "basics.h"
#include "lex.h"
#include <cstddef>
#include <cstdint>
#include <vector>

struct AstNode;
struct BlockNode;

enum class NodeKind
{
    binary_operator,
    block,
    declaration,
    identifier,
    if_statement,
    literal,
    procedure,
    procedure_call,
    procedure_signature,
    program,
    return_statement,
    simple_type,
    pointer_type,
    array_type,
    struct_type,
    nop,
};

struct Node
{
    NodeKind kind{};
    const Node *type{};  // (type != nullptr) == (node is an expression)
    const BlockNode *containing_block{};

    explicit Node(NodeKind kind)
        : kind{kind}
    {
    }

    virtual ~Node() = default;
};


template<typename TNode, bool required = false>
TNode *node_cast(Node *node)
{
    if (node == nullptr)
    {
        if constexpr (required)
        {
            FATAL("node_cast passed nullptr");
        }

        return nullptr;
    }

    if (node->kind != TNode::kind)
    {
        if constexpr (required)
        {
            FATAL("node_cast passed invalid kind");
        }

        return nullptr;
    }

    return static_cast<TNode *>(node);
}

template<typename TNode, bool required = false>
const TNode *node_cast(const Node *node)
{
    if (node == nullptr)
    {
        if constexpr (required)
        {
            FATAL("node_cast passed nullptr");
        }

        return nullptr;
    }

    if (node->kind != TNode::kind)
    {
        if constexpr (required)
        {
            FATAL("node_cast passed invalid kind");
        }

        return nullptr;
    }

    return static_cast<const TNode *>(node);
}

template<NodeKind the_kind>
struct NodeOfKind : Node
{
    constexpr static NodeKind kind = the_kind;

    NodeOfKind()
        : Node(the_kind)
    {
    }
};

struct BinaryOperatorNode : NodeOfKind<NodeKind::binary_operator>
{
    TokenType operator_type{};
    Node *lhs{};
    Node *rhs{};
};

struct BlockNode : NodeOfKind<NodeKind::block>
{
    std::vector<Node *> statements{};
    int64_t offset_from_parent_block{};
    int64_t memory_size{};  // Size of all local variables
    // int64_t memory_size_of_args{};  // Size of all procedure arguments

    inline bool is_global() const { return this->containing_block == nullptr; }

    struct DeclarationNode *find_declaration(std::string_view name) const;
};

struct DeclarationNode : NodeOfKind<NodeKind::declaration>
{
    std::string_view identifier;
    Node *specified_type;
    Node *init_expression;
};

struct IdentifierNode : NodeOfKind<NodeKind::identifier>
{
    std::string_view identifier;
};

struct IfNode : NodeOfKind<NodeKind::identifier>
{
    Node *condition;
    BlockNode *then_block;
    BlockNode *else_block;
};

struct LiteralNode : NodeOfKind<NodeKind::literal>
{
    std::variant<uint64_t, float, double, bool> value;
    char suffix{};
};

struct ProcedureSignatureNode : NodeOfKind<NodeKind::procedure_signature>
{
    std::vector<DeclarationNode *> arguments{};
    Node *return_type;
};

struct ProcedureNode : NodeOfKind<NodeKind::procedure>
{
    ProcedureSignatureNode *signature{};
    BlockNode *body{};
};

struct ProcedureCallNode : NodeOfKind<NodeKind::procedure_call>
{
    Node *procedure{};
    std::vector<Node *> arguments{};
};

struct ReturnNode : NodeOfKind<NodeKind::return_statement>
{
    Node *expression;
};

struct SimpleTypeNode : NodeOfKind<NodeKind::simple_type>
{
    enum class Kind
    {
        invalid,
        voyd,
        boolean,
        signed_integer,
        unsigned_integer,
        floatingpoint,
        type,  // This is the kind of all types themselves
    };

    explicit SimpleTypeNode(Kind type_kind, int64_t size)
        : type_kind{type_kind}
        , size{size}
    {
    }

    Kind type_kind{};
    int64_t size = -1;

    inline bool is_numerical() const
    {
        return this->type_kind == Kind::signed_integer || this->type_kind == Kind::unsigned_integer ||
               this->type_kind == Kind::floatingpoint;
    }
};

struct PointerTypeNode : NodeOfKind<NodeKind::pointer_type>
{
    Node *target_type;
};

struct ArrayTypeNode : NodeOfKind<NodeKind::array_type>
{
    Node *length_expression;
    Node *element_type;
};

struct StructTypeNode : NodeOfKind<NodeKind::struct_type>
{
    // TODO
};

// This node is implicitly created for optional expressions that have been omitted in the program
// (like the init expression of a local variable)
struct NopNode : NodeOfKind<NodeKind::nop>
{
};

bool types_equal(const Node *lhs, const Node *rhs);

struct BuiltinTypes
{
    static const SimpleTypeNode voyd;
    static const SimpleTypeNode i64;
    static const SimpleTypeNode i32;
    static const SimpleTypeNode i16;
    static const SimpleTypeNode i8;
    static const SimpleTypeNode u64;
    static const SimpleTypeNode u32;
    static const SimpleTypeNode u16;
    static const SimpleTypeNode u8;
    static const SimpleTypeNode f32;
    static const SimpleTypeNode f64;
    static const SimpleTypeNode boolean;
    static const SimpleTypeNode type;
};

Node *make_node(BlockNode *containing_block, AstNode *ast);
[[nodiscard]] bool typecheck(Node *node);
