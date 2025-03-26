#pragma once

#include "basics.h"
#include "lex.h"
#include <vector>

enum class AstKind
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
};

inline const char *to_string(AstKind kind)
{
    switch (kind)
    {
        case AstKind::binary_operator:     return "binary_operator";
        case AstKind::block:               return "block";
        case AstKind::declaration:         return "declaration";
        case AstKind::identifier:          return "identifier";
        case AstKind::if_statement:        return "if_statement";
        case AstKind::literal:             return "literal";
        case AstKind::procedure:           return "procedure";
        case AstKind::procedure_call:      return "procedure_call";
        case AstKind::procedure_signature: return "procedure_signature";
        case AstKind::program:             return "program";
        case AstKind::return_statement:    return "return_statement";
        case AstKind::simple_type:         return "simple_type";
        case AstKind::pointer_type:        return "pointer_type";
        case AstKind::array_type:          return "array_type";
    }

    assert(false);
}

// enum class LiteralType
// {
//     none,
//     integer,
//     float32,
//     float64,
//     string,
// };

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

struct AstSimpleType : AstOfKind<AstKind::simple_type>
{
    Token identifier{};
};

struct AstPointerType : AstOfKind<AstKind::pointer_type>
{
    AstNode *target_type{};
};

struct AstArrayType : AstOfKind<AstKind::simple_type>
{
    AstNode *length_expression{};
    AstNode *element_type{};
};

struct AstDeclaration : AstOfKind<AstKind::declaration>
{
    Token identifier{};
    AstNode *type{};
    AstNode *init_expression{};
};

struct AstBinaryOperator : AstOfKind<AstKind::binary_operator>
{
    TokenType type{};
    AstNode *lhs{};
    AstNode *rhs{};
};

struct AstLiteral : AstOfKind<AstKind::literal>
{
    Token token{};
    char suffix{};  // 'u' or 'f'
    std::variant<uint64_t, float, double, bool> value;
};

struct AstProcedureSignature : AstOfKind<AstKind::procedure_signature>
{
    std::vector<AstDeclaration> arguments{};
    AstNode *return_type{};
};

struct AstBlock : AstOfKind<AstKind::block>
{
    bool is_proc_body{};
    std::vector<AstNode *> statements{};
};

struct AstIf : AstOfKind<AstKind::if_statement>
{
    AstNode *condition{};
    AstBlock then_block{};
    AstBlock *else_block{};
};

struct AstReturn : AstOfKind<AstKind::return_statement>
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

// TODO: Rename to module
struct AstProgram : AstOfKind<AstKind::program>
{
    AstBlock block{};
};

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

    if (auto program = ast_cast<AstProgram>(node))
    {
        visit(&program->block, f);
        return;
    }

    if (auto retyrn = ast_cast<AstReturn>(node))
    {
        visit(retyrn->expression, f);
        return;
    }

    UNREACHED;
}

bool parse_program(std::string_view source, AstProgram &out_program);
