#pragma once

#include "basics.h"
#include "lex.h"

#include <cstdint>

namespace llvm
{
    class Value;
    class BasicBlock;
}  // namespace llvm

struct BlockNode;

enum class NodeKind
{
    binary_operator,
    break_statement,
    block,
    continue_statement,
    declaration,
    goto_statement,
    identifier,
    if_statement,
    label,
    literal,
    module,
    procedure,
    procedure_call,
    procedure_signature,
    return_statement,
    type_cast,
    while_loop,

    basic_type,
    pointer_type,
    array_type,
    struct_type,

    nop,
};

struct Node
{
    NodeKind kind{};

    explicit Node(NodeKind kind)
        : kind{kind}
    {
    }

    virtual ~Node() = default;

    void set_inferred_type(Node *inferred_type);
    Node *inferred_type() const;
    bool is_type() const;
    bool is_poisoned() const;
    static bool types_equal(const Node *lhs, const Node *rhs);
    static std::string type_to_string(const Node *type);

private:
    Node *inferred_type_{};
};


template<typename TNode, bool required = false>
TNode *node_cast(Node *node)
{
    if (node == nullptr)
    {
        if constexpr (required)
        {
            FATAL("node_cast received nullptr");
        }

        return nullptr;
    }

    if (node->kind != TNode::kind)
    {
        if constexpr (required)
        {
            FATAL("node_cast received invalid kind");
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
            FATAL("node_cast received nullptr");
        }

        return nullptr;
    }

    if (node->kind != TNode::kind)
    {
        if constexpr (required)
        {
            FATAL("node_cast received invalid kind");
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

struct DeclarationNode : NodeOfKind<NodeKind::declaration>
{
    std::string_view identifier{};
    Node *specified_type{};
    Node *init_expression{};
    bool is_procedure_argument{};
    BlockNode *containing_block{};

    llvm::Value *named_value{};

    inline bool is_global() const;
};

struct BlockNode : NodeOfKind<NodeKind::block>
{
    std::vector<Node *> statements{};
    BlockNode *parent_block{};

    std::unordered_map<std::string, DeclarationNode *> declarations{};

    inline bool is_global() const { return this->parent_block == nullptr; }

    DeclarationNode *find_declaration(std::string_view name, bool recurse = true) const;
};

inline bool DeclarationNode::is_global() const
{
    return this->containing_block->is_global();
}

struct IdentifierNode : NodeOfKind<NodeKind::identifier>
{
    std::string_view identifier{};
    DeclarationNode *declaration{};
};

struct IfStatementNode : NodeOfKind<NodeKind::if_statement>
{
    Node *condition{};
    BlockNode *then_block{};
    BlockNode *else_block{};
};

struct WhileLoopNode : NodeOfKind<NodeKind::while_loop>
{
    Node *condition{};
    BlockNode *body{};
};

struct BreakStatementNode : NodeOfKind<NodeKind::break_statement>
{
};

struct ContinueStatementNode : NodeOfKind<NodeKind::continue_statement>
{
};

struct LiteralNode : NodeOfKind<NodeKind::literal>
{
    std::variant<bool, uint64_t, float, double, std::string> value{};
    char suffix{};
};

struct ProcedureSignatureNode : NodeOfKind<NodeKind::procedure_signature>
{
    std::vector<DeclarationNode *> arguments{};
    bool is_vararg{};
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

struct ReturnStatementNode : NodeOfKind<NodeKind::return_statement>
{
    Node *expression{};
};

struct TypeCastNode : NodeOfKind<NodeKind::type_cast>
{
    Node *target_type{};
    Node *expression{};
};

struct LabelNode : NodeOfKind<NodeKind::label>
{
    std::string_view identifier;
    llvm::BasicBlock *block{};
    llvm::BasicBlock *after{};
};

struct GotoStatementNode : NodeOfKind<NodeKind::goto_statement>
{
    std::string_view label_identifier;
};

struct ModuleNode : NodeOfKind<NodeKind::module>
{
    BlockNode *block{};
};

struct BasicTypeNode : NodeOfKind<NodeKind::basic_type>
{
    enum class Kind
    {
        unchecked,
        voyd,
        boolean,
        signed_integer,
        unsigned_integer,
        floatingpoint,
        type,
        label,
        poison,
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

struct BuiltinTypes
{
    static BasicTypeNode voyd;
    static BasicTypeNode i64;
    static BasicTypeNode i32;
    static BasicTypeNode i16;
    static BasicTypeNode i8;
    static BasicTypeNode u64;
    static BasicTypeNode u32;
    static BasicTypeNode u16;
    static BasicTypeNode u8;
    static BasicTypeNode f32;
    static BasicTypeNode f64;
    static BasicTypeNode boolean;
    static BasicTypeNode type;
    static BasicTypeNode label;
    static BasicTypeNode poison;
    static NopNode nop;
    static PointerTypeNode string_literal;
    static ProcedureSignatureNode main_signature;

    static const std::vector<std::tuple<BasicTypeNode *, std::string_view>> type_names;
};

struct NodeVisitorBase
{
    ProcedureNode *current_procedure{};
    BlockNode *current_block{};
    std::vector<std::string> errors{};

    inline void error(const Node *node, std::string_view message)
    {
        // TODO: Print a string representation of the node for context
        this->errors.push_back(std::format("Type error: {}", message));
    }

    inline bool has_error() const { return this->errors.empty() == false; }
    inline virtual bool is_done() const { return false; }

    inline virtual void visit(ArrayTypeNode *array_type) { }
    inline virtual void visit(BasicTypeNode *basic_type) { }
    inline virtual void visit(BinaryOperatorNode *binary_operator) { }
    inline virtual void visit(BlockNode *block) { }
    inline virtual void visit(BreakStatementNode *break_statement) { }
    inline virtual void visit(ContinueStatementNode *continue_statement) { }
    inline virtual void visit(DeclarationNode *declaration) { }
    inline virtual void visit(GotoStatementNode *goto_statement) { }
    inline virtual void visit(IdentifierNode *identifier) { }
    inline virtual void visit(IfStatementNode *if_statement) { }
    inline virtual void visit(LabelNode *label) { }
    inline virtual void visit(LiteralNode *literal) { }
    inline virtual void visit(ModuleNode *module) { }
    inline virtual void visit(NopNode *nop) { }
    inline virtual void visit(PointerTypeNode *pointer_type) { }
    inline virtual void visit(ProcedureCallNode *procedure_call) { }
    inline virtual void visit(ProcedureNode *procedure) { }
    inline virtual void visit(ProcedureSignatureNode *procedure_signature) { }
    inline virtual void visit(ReturnStatementNode *return_statement) { }
    inline virtual void visit(StructTypeNode *struct_type) { }
    inline virtual void visit(TypeCastNode *type_cast) { }
    inline virtual void visit(WhileLoopNode *while_loop) { }
};

enum class VisitOrder
{
    children_first,
    this_first,
};

inline void visit(Node *node, NodeVisitorBase &visitor)
{
    if (node == nullptr)
    {
        return;
    }

    if (auto array_type = node_cast<ArrayTypeNode>(node))
    {
        visitor.visit(array_type);
        if (visitor.is_done())
        {
            return;
        }

        visit(array_type->length, visitor);
        visit(array_type->element_type, visitor);

        return;
    }

    if (auto basic_type = node_cast<BasicTypeNode>(node))
    {
        visitor.visit(basic_type);
        if (visitor.is_done())
        {
            return;
        }

        return;
    }

    if (auto binary_operator = node_cast<BinaryOperatorNode>(node))
    {
        visitor.visit(binary_operator);
        if (visitor.is_done())
        {
            return;
        }

        visit(binary_operator->lhs, visitor);
        visit(binary_operator->rhs, visitor);

        return;
    }

    if (auto block = node_cast<BlockNode>(node))
    {
        visitor.visit(block);
        if (visitor.is_done())
        {
            return;
        }

        SET_TEMPORARILY(visitor.current_block, block);

        for (auto statement : block->statements)
        {
            visit(statement, visitor);
        }

        return;
    }

    if (auto break_statement = node_cast<BreakStatementNode>(node))
    {
        visitor.visit(break_statement);
        if (visitor.is_done())
        {
            return;
        }

        return;
    }

    if (auto continue_statement = node_cast<ContinueStatementNode>(node))
    {
        visitor.visit(continue_statement);
        if (visitor.is_done())
        {
            return;
        }

        return;
    }

    if (auto declaration = node_cast<DeclarationNode>(node))
    {
        visitor.visit(declaration);
        if (visitor.is_done())
        {
            return;
        }

        visit(declaration->specified_type, visitor);
        visit(declaration->init_expression, visitor);

        return;
    }

    if (auto goto_statement = node_cast<GotoStatementNode>(node))
    {
        visitor.visit(goto_statement);
        if (visitor.is_done())
        {
            return;
        }

        return;
    }

    if (auto identifier = node_cast<IdentifierNode>(node))
    {
        visitor.visit(identifier);
        if (visitor.is_done())
        {
            return;
        }

        return;
    }

    if (auto if_statement = node_cast<IfStatementNode>(node))
    {
        visitor.visit(if_statement);
        if (visitor.is_done())
        {
            return;
        }

        visit(if_statement->condition, visitor);
        visit(if_statement->then_block, visitor);
        visit(if_statement->else_block, visitor);

        return;
    }

    if (auto label = node_cast<LabelNode>(node))
    {
        visitor.visit(label);
        if (visitor.is_done())
        {
            return;
        }

        return;
    }

    if (auto literal = node_cast<LiteralNode>(node))
    {
        visitor.visit(literal);
        if (visitor.is_done())
        {
            return;
        }

        return;
    }

    if (auto module = node_cast<ModuleNode>(node))
    {
        visitor.visit(module);
        if (visitor.is_done())
        {
            return;
        }

        visit(module->block, visitor);

        return;
    }

    if (auto nop = node_cast<NopNode>(node))
    {
        visitor.visit(nop);
        if (visitor.is_done())
        {
            return;
        }

        return;
    }

    if (auto pointer_type = node_cast<PointerTypeNode>(node))
    {
        visitor.visit(pointer_type);
        if (visitor.is_done())
        {
            return;
        }

        visit(pointer_type->target_type, visitor);

        return;
    }

    if (auto procedure_call = node_cast<ProcedureCallNode>(node))
    {
        visitor.visit(procedure_call);
        if (visitor.is_done())
        {
            return;
        }

        visit(procedure_call->procedure, visitor);

        for (auto arg : procedure_call->arguments)
        {
            visit(arg, visitor);
        }

        return;
    }

    if (auto procedure = node_cast<ProcedureNode>(node))
    {
        assert(visitor.current_procedure == nullptr);
        assert((procedure->is_external) == (procedure->body == nullptr));

        // // NOTE: Set this here already instead of after visiting the procedure
        // // so that the arguments can be registered as declarations of the procedure's
        // // body.
        // SET_TEMPORARILY(visitor.current_procedure, procedure);
        // SET_TEMPORARILY(visitor.current_block, procedure->body);

        visitor.visit(procedure);
        if (visitor.is_done())
        {
            return;
        }

        SET_TEMPORARILY(visitor.current_procedure, procedure);

        visit(procedure->signature, visitor);
        visit(procedure->body, visitor);

        return;
    }

    if (auto procedure_signature = node_cast<ProcedureSignatureNode>(node))
    {
        visitor.visit(procedure_signature);
        if (visitor.is_done())
        {
            return;
        }

        for (auto arg : procedure_signature->arguments)
        {
            visit(arg, visitor);
        }

        visit(procedure_signature->return_type, visitor);

        return;
    }

    if (auto return_statement = node_cast<ReturnStatementNode>(node))
    {
        visitor.visit(return_statement);
        if (visitor.is_done())
        {
            return;
        }

        visit(return_statement->expression, visitor);

        return;
    }

    if (auto struct_type = node_cast<StructTypeNode>(node))
    {
        TODO;
        // f(struct_type);

        // visit(struct_type->xxx, f);

        // return;
    }

    if (auto type_cast = node_cast<TypeCastNode>(node))
    {
        visitor.visit(type_cast);
        if (visitor.is_done())
        {
            return;
        }

        visit(type_cast->target_type, visitor);
        visit(type_cast->expression, visitor);

        return;
    }

    if (auto while_loop = node_cast<WhileLoopNode>(node))
    {
        visitor.visit(while_loop);
        if (visitor.is_done())
        {
            return;
        }

        visit(while_loop->condition, visitor);
        visit(while_loop->body, visitor);

        return;
    }

    UNREACHED;
}
