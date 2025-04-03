#pragma once

#include "basics.h"
#include "lex.h"

#include <vector>

enum class AstKind
{
    array_type,
    binary_operator,
    block,
    break_statement,
    continue_statement,
    declaration,
    for_loop,
    goto_statement,
    identifier,
    if_statement,
    label,
    literal,
    module,
    pointer_type,
    procedure,
    procedure_call,
    procedure_signature,
    return_statement,
    type_identifier,
    while_loop,
};

inline std::string_view to_string(AstKind kind)
{
    switch (kind)
    {
        case AstKind::array_type:          return "array_type";
        case AstKind::binary_operator:     return "binary_operator";
        case AstKind::block:               return "block";
        case AstKind::break_statement:     return "break_statement";
        case AstKind::continue_statement:  return "continue_statement";
        case AstKind::declaration:         return "declaration";
        case AstKind::for_loop:            return "for_loop";
        case AstKind::goto_statement:      return "jumpto";
        case AstKind::identifier:          return "identifier";
        case AstKind::if_statement:        return "if_statement";
        case AstKind::label:               return "label";
        case AstKind::literal:             return "literal";
        case AstKind::module:              return "module";
        case AstKind::pointer_type:        return "pointer_type";
        case AstKind::procedure:           return "procedure";
        case AstKind::procedure_call:      return "procedure_call";
        case AstKind::procedure_signature: return "procedure_signature";
        case AstKind::return_statement:    return "return_statement";
        case AstKind::type_identifier:     return "type_identifier";
        case AstKind::while_loop:          return "while_loop";
    }

    UNREACHED;
}

struct AstNode
{
    AstKind kind{};

    AstNode() = delete;

    virtual ~AstNode() = default;

    explicit AstNode(AstKind kind)
        : kind{kind}
    {
    }

    auto operator<=>(const AstNode &) const = default;
};

template<AstKind the_kind, bool required = false>
struct AstOfKind : AstNode
{
    constexpr static AstKind kind = the_kind;

    AstOfKind()
        : AstNode(kind)
    {
    }

    auto operator<=>(const AstOfKind &) const = default;
};

template<typename TNode, bool required = false>
TNode *ast_cast(AstNode *node)
{
    if (node == nullptr)
    {
        if constexpr (required)
        {
            FATAL("ast_cast received nullptr");
        }

        return nullptr;
    }

    if (node->kind != TNode::kind)
    {
        if constexpr (required)
        {
            FATAL("ast_cast received nullptr");
        }

        return nullptr;
    }

    return static_cast<TNode *>(node);
}

// TODO: Remove this node and replace it with AstIdentifier
struct AstTypeIdentifier : AstOfKind<AstKind::type_identifier>
{
    Token identifier{};

    auto operator<=>(const AstTypeIdentifier &) const = default;
};

struct AstPointerType : AstOfKind<AstKind::pointer_type>
{
    AstNode *target_type{};

    auto operator<=>(const AstPointerType &) const = default;
};

struct AstArrayType : AstOfKind<AstKind::type_identifier>
{
    AstNode *length_expression{};
    AstNode *element_type{};

    auto operator<=>(const AstArrayType &) const = default;
};

struct AstDeclaration : AstOfKind<AstKind::declaration>
{
    Token identifier{};
    AstNode *type{};
    AstNode *init_expression{};
    bool is_procedure_argument{};

    auto operator<=>(const AstDeclaration &) const = default;
};

struct AstBinaryOperator : AstOfKind<AstKind::binary_operator>
{
    TokenType operator_type{};
    AstNode *lhs{};
    AstNode *rhs{};

    auto operator<=>(const AstBinaryOperator &) const = default;
};

struct AstLabel : AstOfKind<AstKind::label>
{
    std::string_view identifier{};

    auto operator<=>(const AstLabel &) const = default;
};

struct AstGotoStatement : AstOfKind<AstKind::goto_statement>
{
    std::string_view label_identifier{};

    auto operator<=>(const AstGotoStatement &) const = default;
};

struct AstLiteral : AstOfKind<AstKind::literal>
{
    Token token{};  // TODO: Remove
    char suffix{};  // 'u' or 'f'
    std::variant<bool, uint64_t, float, double, std::string> value{};

    auto operator<=>(const AstLiteral &) const = default;
};

struct AstProcedureSignature : AstOfKind<AstKind::procedure_signature>
{
    std::vector<AstDeclaration *> arguments{};
    bool is_vararg{};
    AstNode *return_type{};

    auto operator<=>(const AstProcedureSignature &) const = default;
};

struct AstBlock : AstOfKind<AstKind::block>
{
    enum CompilerErrorKind
    {
        none,
        declaration,
        typecheck,
    };

    std::vector<AstNode *> statements{};
    CompilerErrorKind expected_compiler_error_kind{};

    auto operator<=>(const AstBlock &) const = default;
};

struct AstIfStatement : AstOfKind<AstKind::if_statement>
{
    AstNode *condition{};
    AstBlock *then_block{};
    AstBlock *else_block{};

    auto operator<=>(const AstIfStatement &) const = default;
};

struct AstWhileLoop : AstOfKind<AstKind::while_loop>
{
    AstNode *condition{};
    AstBlock *block{};
    AstNode *prologue{};  // NOTE: Only used by desugared for loop at the moment

    auto operator<=>(const AstWhileLoop &) const = default;
};

struct AstForLoop : AstOfKind<AstKind::for_loop>
{
    Token identifier{};
    AstNode *range_begin{};  // TODO: Make it so that none means 0 for '<' and '<='?
    TokenType
        comparison_operator{};  // One of '>', '>=', '<', '<=' (I'm not decided yet on whether to include '==' and '!=')
    AstNode *range_end{};
    AstNode *step{};
    AstBlock *block{};

    auto operator<=>(const AstForLoop &) const = default;
};

struct AstBreakStatement : AstOfKind<AstKind::break_statement>
{
    auto operator<=>(const AstBreakStatement &) const = default;
};

struct AstContinueStatement : AstOfKind<AstKind::continue_statement>
{
    auto operator<=>(const AstContinueStatement &) const = default;
};

struct AstReturnStatement : AstOfKind<AstKind::return_statement>
{
    AstNode *expression{};

    auto operator<=>(const AstReturnStatement &) const = default;
};

struct AstIdentifier : AstOfKind<AstKind::identifier>
{
    Token identifier{};

    auto operator<=>(const AstIdentifier &) const = default;
};

struct AstProcedure : AstOfKind<AstKind::procedure>
{
    AstProcedureSignature *signature{};
    AstBlock *body{};
    bool is_external{};

    auto operator<=>(const AstProcedure &) const = default;
};

struct AstProcedureCall : AstOfKind<AstKind::procedure_call>
{
    AstNode *procedure{};
    std::vector<AstNode *> arguments{};

    auto operator<=>(const AstProcedureCall &) const = default;
};

struct AstModule : AstOfKind<AstKind::module>
{
    AstBlock *block{};

    auto operator<=>(const AstModule &) const = default;
};

AstModule *parse_module(std::string_view source);


#if 0
template<typename F>
void visit(AstNode *node, const F &f)
{
    if (node == nullptr)
    {
        return;
    }

    f(node);

    if (auto bin_op = ast_cast<AstBinaryOperator>(node))
    {
        visit(bin_op->lhs, f);
        visit(bin_op->rhs, f);
        return;
    }

    if (auto block = ast_cast<AstBlock>(node))
    {
        for (auto statement : block->statements)
        {
            visit(statement, f);
        }

        return;
    }

    if (auto decl = ast_cast<AstDeclaration>(node))
    {
        visit(decl->init_expression, f);
        return;
    }

    if (auto ident = ast_cast<AstIdentifier>(node))
    {
        return;
    }

    if (auto yf = ast_cast<AstIf>(node))
    {
        visit(yf->condition, f);
        visit(&yf->then_block, f);
        visit(yf->else_block, f);
        return;
    }

    if (auto literal = ast_cast<AstLiteral>(node))
    {
        return;
    }

    if (auto proc = ast_cast<AstProcedure>(node))
    {
        visit(&proc->signature, f);
        visit(&proc->body, f);
        return;
    }

    if (auto call = ast_cast<AstProcedureCall>(node))
    {
        visit(call->procedure, f);

        for (auto arg : call->arguments)
        {
            visit(arg, f);
        }

        return;
    }

    if (auto signature = ast_cast<AstProcedureSignature>(node))
    {
        for (auto arg : signature->arguments)
        {
            visit(&arg, f);
        }

        return;
    }

    if (auto module = ast_cast<AstModule>(node))
    {
        visit(&module->block, f);
        return;
    }

    if (auto retyrn = ast_cast<AstReturn>(node))
    {
        visit(retyrn->expression, f);
        return;
    }

    UNREACHED;
}
#endif
