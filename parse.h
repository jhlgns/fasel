#pragma once

#include "lex.h"
#include <vector>

enum AstKind
{
    AST_BIN_OP,
    AST_BLOCK,
    AST_DECL,
    AST_IDENT,
    AST_LITERAL,
    AST_ARG,
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
        case AST_BIN_OP:         return "AST_BIN_OP";
        case AST_BLOCK:          return "AST_BLOCK";
        case AST_DECL:           return "AST_DECL";
        case AST_IDENT:          return "AST_IDENT";
        case AST_LITERAL:        return "AST_LITERAL";
        case AST_ARG:            return "AST_PARAM";
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
    AstNode() = delete;
    explicit AstNode(AstKind kind)
        : kind{kind}
    {
    }

    AstKind kind{};
};

struct AstDecl : AstNode
{
    AstDecl()
        : AstNode(AST_DECL)
    {
    }

    Token ident{};
    AstNode *init_expr{};

    // Compiler information

    struct AstBlock *block{};
    struct AstProc *proc{};
    /* bool is_global{}; */
    int64_t address{};  // Global declaration: address inside the program
                        // Local declaration (procedure): offset from procedure stack base
    /* int64_t root_block_size{};  // Local declaration: the size of the block that has the memory this declaration
     * lives in (in case of */
    /*                             // procedures, this is the procedure body), 0 for global declarations */
};

struct AstBinOp : AstNode
{
    AstBinOp()
        : AstNode(AST_BIN_OP)
    {
    }

    AstNode *lhs{};
    AstNode *rhs{};
    TokenType binop{};
};

struct AstArg : AstNode
{
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

struct AstLiteral : AstNode
{
    AstLiteral()
        : AstNode(AST_LITERAL)
    {
    }

    Token token{};
    AstLiteralType type{};
    int64_t int_value{};
};

struct AstProcSignature : AstNode
{
    AstProcSignature()
        : AstNode(AST_PROC_SIGNATURE)
    {
    }

    std::vector<AstArg> arguments{};
};

struct AstBlock : AstNode
{
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

    bool is_global() const { return this->parent_block == nullptr; }
};


struct AstReturn : AstNode
{
    AstReturn()
        : AstNode(AST_RETURN)
    {
    }

    AstNode *expr{};
};

struct AstIdent : AstNode
{
    AstIdent()
        : AstNode(AST_IDENT)
    {
    }

    Token ident{};
};

struct AstProc : AstNode
{
    AstProc()
        : AstNode(AST_PROC)
    {
    }

    AstProcSignature signature{};
    AstBlock body{};
};

struct AstProcCall : AstNode
{
    AstProcCall()
        : AstNode(AST_PROC_CALL)
    {
    }

    AstNode *proc{};
    std::vector<AstNode *> arguments{};
};

struct AstProgram : AstNode
{
    AstProgram()
        : AstNode(AST_PROGRAM)
    {
    }

    AstBlock block{};
};

bool parse_program(std::string_view source, AstProgram *prog);
