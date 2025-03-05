#if 0
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
    int64_t size = -1;
};

struct Node
{
    NodeKind kind{};
    Type inferred_type{};

    explicit Node(NodeKind kind)
        : kind{kind}
    {
    }

    virtual ~Node() = default;
};

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
    TokenType type{};
    Node *lhs{};
    Node *rhs{};
};

struct Block : public NodeHelper<NodeKind::block>
{
    std::vector<Node *> statements{};
    Block *parent_block{};
    int64_t offset_from_parent_block{};
    int64_t memory_size{};
    int64_t memory_size_of_args{};

    bool is_global() const { return this->parent_block == nullptr; }

    inline AstDeclaration *find_declaration(std::string_view name)
    {
        for (auto statement : this->statements)
        {
            if (auto decl = ast_cast<AstDeclaration>(statement))
            {
                if (decl->identifier.text() == name)
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
};

Node *typecheck(struct AstNode *ast_node);
#endif
