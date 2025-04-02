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
};

template<AstKind the_kind>
struct AstOfKind : AstNode
{
    constexpr static AstKind kind = the_kind;

    AstOfKind()
        : AstNode(kind)
    {
    }
};

template<typename TNode>
TNode *ast_cast(AstNode *node)
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

// TODO: Remove this node and replace it with AstIdentifier
struct AstTypeIdentifier : AstOfKind<AstKind::type_identifier>
{
    Token identifier{};
};

struct AstPointerType : AstOfKind<AstKind::pointer_type>
{
    AstNode *target_type{};
};

struct AstArrayType : AstOfKind<AstKind::type_identifier>
{
    AstNode *length_expression{};
    AstNode *element_type{};
};

struct AstDeclaration : AstOfKind<AstKind::declaration>
{
    Token identifier{};
    AstNode *type{};
    AstNode *init_expression{};
    bool is_procedure_argument{};
};

struct AstBinaryOperator : AstOfKind<AstKind::binary_operator>
{
    TokenType type{};
    AstNode *lhs{};
    AstNode *rhs{};
};

struct AstLabel : AstOfKind<AstKind::label>
{
    std::string_view identifier{};
};

struct AstGotoStatement : AstOfKind<AstKind::goto_statement>
{
    std::string_view label_identifier{};
};

struct AstLiteral : AstOfKind<AstKind::literal>
{
    Token token{};
    char suffix{};  // 'u' or 'f'
    std::variant<bool, uint64_t, float, double, std::string> value{};
};

struct AstProcedureSignature : AstOfKind<AstKind::procedure_signature>
{
    std::vector<AstDeclaration> arguments{};
    bool is_vararg{};
    AstNode *return_type{};
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
};

struct AstIfStatement : AstOfKind<AstKind::if_statement>
{
    AstNode *condition{};
    AstBlock then_block{};
    AstBlock *else_block{};
};

struct AstWhileLoop : AstOfKind<AstKind::while_loop>
{
    AstNode *condition{};
    AstBlock block{};
};

struct AstBreakStatement : AstOfKind<AstKind::break_statement>
{
};

struct AstContinueStatement : AstOfKind<AstKind::continue_statement>
{
};

struct AstReturnStatement : AstOfKind<AstKind::return_statement>
{
    AstNode *expression{};
};

struct AstIdentifier : AstOfKind<AstKind::identifier>
{
    Token identifier{};
};

struct AstProcedure : AstOfKind<AstKind::procedure>
{
    AstProcedureSignature signature{};
    AstBlock body{};  // TODO: Make this an optional pointer!
    bool is_external{};
};

struct AstProcedureCall : AstOfKind<AstKind::procedure_call>
{
    AstNode *procedure{};
    std::vector<AstNode *> arguments{};
};

struct AstModule : AstOfKind<AstKind::module>
{
    AstBlock block{};
};

bool parse_module(std::string_view source, AstModule &out_module);


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
