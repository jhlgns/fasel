#pragma once

#include "lex.h"
#include <vector>

enum AstKind
{
    AST_ARG,
    AST_BIN_OP,
    AST_BLOCK,
    AST_DECL,
    AST_IDENT,
    AST_IF,
    AST_LITERAL,
    AST_PROC,
    AST_PROC_CALL,
    AST_PROC_SIGNATURE,
    AST_PROGRAM,
    AST_RETURN,
};

inline const char *to_string(AstKind kind)
{
    switch (kind)
    {
        case AST_ARG:            return "AST_ARG";
        case AST_BIN_OP:         return "AST_BIN_OP";
        case AST_BLOCK:          return "AST_BLOCK";
        case AST_DECL:           return "AST_DECL";
        case AST_IDENT:          return "AST_IDENT";
        case AST_IF:             return "AST_IF";
        case AST_LITERAL:        return "AST_LITERAL";
        case AST_PROC:           return "AST_PROC";
        case AST_PROC_CALL:      return "AST_PROC_CALL";
        case AST_PROC_SIGNATURE: return "AST_PROC_SIGNATURE";
        case AST_PROGRAM:        return "AST_PROGRAM";
        case AST_RETURN:         return "AST_RETURN";
    }

    assert(false);
}

struct AstNode
{
    AstNode()          = delete;
    virtual ~AstNode() = default;
    explicit AstNode(AstKind kind)
        : kind{kind}
    {
    }

    AstKind kind{};
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

struct AstDecl : AstNode
{
    constexpr static AstKind kind = AST_DECL;

    AstDecl()
        : AstNode(AST_DECL)
    {
    }

    Token ident{};
    AstNode *init_expr{};

    // Compiler information

    bool is_proc_arg{};
    struct AstProc *enclosing_proc{};
    int64_t address{};  // Global declaration: address inside the program
                        // Local declaration (procedure): offset from procedure stack base
};

struct AstBinOp : AstNode
{
    constexpr static AstKind kind = AST_BIN_OP;

    AstBinOp()
        : AstNode(AST_BIN_OP)
    {
    }

    AstNode *lhs{};
    AstNode *rhs{};
    TokenType type{};
};

struct AstArg : AstNode
{
    constexpr static AstKind kind = AST_ARG;

    AstArg()
        : AstNode(AST_ARG)
    {
    }

    Token ident{};
    Token type{};
};

enum AstLiteralType
{
    LIT_NONE,
    LIT_INT,
    LIT_FLOAT,
    LIT_STRING,
};

struct AstLiteral : public AstNode
{
    constexpr static AstKind kind = AST_LITERAL;

    AstLiteral()
        : AstNode(AST_LITERAL)
    {
    }

    Token token{};
    AstLiteralType type{};
    int64_t int_value{};
};

struct AstProcSignature : public AstNode
{
    constexpr static AstKind kind = AST_PROC_SIGNATURE;

    AstProcSignature()
        : AstNode(AST_PROC_SIGNATURE)
    {
    }

    std::vector<AstArg> arguments{};
};

struct AstBlock : public AstNode
{
    constexpr static AstKind kind = AST_BLOCK;

    AstBlock()
        : AstNode(AST_BLOCK)
    {
    }

    bool is_proc_body{};
    std::vector<AstNode *> statements{};

    // Compiler information
    AstBlock *parent_block{};
    int64_t offset_from_parent_block{};
    int64_t size{};
    int64_t size_of_args{};

    bool is_global() const { return this->parent_block == nullptr; }

    AstDecl *find_decl(std::string_view name)
    {
        for (auto statement : this->statements)
        {
            if (auto decl = ast_cast<AstDecl>(statement))
            {
                if (text_of(&decl->ident) == name)
                {
                    return decl;
                }
            }
        }

        if (this->parent_block != nullptr)
        {
            return this->parent_block->find_decl(name);
        }

        return nullptr;
    }
};

struct AstIf : public AstNode
{
    constexpr static AstKind kind = AST_IF;

    AstIf()
        : AstNode(AST_IF)
    {
    }

    AstNode *condition{};
    AstBlock then_block{};
    AstBlock else_block{};
};

struct AstReturn : public AstNode
{
    constexpr static AstKind kind = AST_RETURN;

    AstReturn()
        : AstNode(AST_RETURN)
    {
    }

    AstNode *expr{};
};

struct AstIdent : public AstNode
{
    constexpr static AstKind kind = AST_IDENT;

    AstIdent()
        : AstNode(AST_IDENT)
    {
    }

    Token ident{};
};

struct AstProc : public AstNode
{
    constexpr static AstKind kind = AST_PROC;

    AstProc()
        : AstNode(AST_PROC)
    {
    }

    AstProcSignature signature{};
    AstBlock body{};
};

struct AstProcCall : public AstNode
{
    constexpr static AstKind kind = AST_PROC_CALL;

    AstProcCall()
        : AstNode(AST_PROC_CALL)
    {
    }

    AstNode *proc{};
    std::vector<AstNode *> arguments{};
};

struct AstProgram : public AstNode
{
    constexpr static AstKind kind = AST_PROGRAM;

    AstProgram()
        : AstNode(AST_PROGRAM)
    {
    }

    AstBlock block{};  // TODO: Just make this an array of AstDecls
};

bool parse_program(std::string_view source, AstProgram *prog);
