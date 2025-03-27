#pragma once

#include "basics.h"
#include "lex.h"

#include <cstdint>

struct BlockNode;

enum class NodeKind
{
    binary_operator,
    block,
    declaration,
    identifier,
    if_statement,
    literal,
    module,
    procedure,
    procedure_call,
    procedure_signature,
    return_statement,
    type_cast,

    basic_type,
    pointer_type,
    array_type,
    struct_type,

    nop,
};

struct Node
{
    NodeKind kind{};
    const Node *type{};  // (type != nullptr) <=> (node is an expression)
    int64_t time =
        -1;  // The index of this statement within its containing block + 1 (if this node is not a statement, this stays -1)

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
    TokenType operator_kind{};
    Node *lhs{};
    Node *rhs{};
};

namespace llvm
{
    class Value;
}

struct DeclarationNode : NodeOfKind<NodeKind::declaration>
{
    std::string_view identifier{};
    Node *specified_type{};
    Node *init_expression{};

    llvm::Value *named_value{};
};

struct BlockNode : NodeOfKind<NodeKind::block>
{
    std::vector<Node *> statements{};
    BlockNode *parent_block{};
    int64_t current_time{};  // TODO: Document

    std::unordered_map<std::string, DeclarationNode *> declarations{};

    inline bool is_global() const { return this->parent_block == nullptr; }

    DeclarationNode *find_declaration(std::string_view name, bool recurse = true) const;
};

struct IdentifierNode : NodeOfKind<NodeKind::identifier>
{
    std::string_view identifier{};
};

struct IfNode : NodeOfKind<NodeKind::if_statement>
{
    Node *condition{};
    BlockNode *then_block{};
    BlockNode *else_block{};
};

struct LiteralNode : NodeOfKind<NodeKind::literal>
{
    std::variant<bool, uint64_t, float, double, std::string> value{};
    char suffix{};
};

struct ProcedureSignatureNode : NodeOfKind<NodeKind::procedure_signature>
{
    std::vector<DeclarationNode *> arguments{};
    Node *return_type{};
};

struct ProcedureNode : NodeOfKind<NodeKind::procedure>
{
    ProcedureSignatureNode *signature{};
    BlockNode *body{};
    bool is_external{};
};

struct ProcedureCallNode : NodeOfKind<NodeKind::procedure_call>
{
    Node *procedure{};
    std::vector<Node *> arguments{};
};

struct ReturnNode : NodeOfKind<NodeKind::return_statement>
{
    Node *expression{};
};

struct TypeCastNode : NodeOfKind<NodeKind::type_cast>
{
    // NOTE: The requested type is set in Node::type... hmmm...
    Node *expression{};
};

struct ModuleNode : NodeOfKind<NodeKind::module>
{
    BlockNode *block{};
};

struct BasicTypeNode : NodeOfKind<NodeKind::basic_type>
{
    enum class Kind
    {
        invalid,
        voyd,
        boolean,
        signed_integer,
        unsigned_integer,
        floatingpoint,
        type,
    };

    explicit BasicTypeNode(Kind type_kind, int64_t size)
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
    Node *target_type{};
};

struct ArrayTypeNode : NodeOfKind<NodeKind::array_type>
{
    Node *length{};
    Node *element_type{};
};

struct StructTypeNode : NodeOfKind<NodeKind::struct_type>
{
    // TODO
};

// This node is implicitly created for optional expressions that have been omitted
// (like the init expression of a local variable)
struct NopNode : NodeOfKind<NodeKind::nop>
{
};
