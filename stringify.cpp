#include "stringify.h"
#include <format>

template<typename... Args>
std::string indent(int indent_level, std::string_view text)
{
    std::string indent = std::string(indent_level * 4, ' ');
    return std::format("{}{}", indent, text);
}

std::string to_string(int indent_level, AstBinOp *node)
{
    std::string result;

    result += indent(indent_level, "BinOp(\n");
    result += std::format("{}\n", indent(indent_level + 1, std::string{(char)node->binop}));
    result += std::format("{}\n", dump_node(indent_level + 1, node->lhs));
    result += std::format("{}\n", dump_node(indent_level + 1, node->rhs));
    result += indent(indent_level, ")");

    return result;
}

std::string to_string(int indent_level, AstBlock *node)
{
    std::string result;

    result += indent(indent_level, "Block(\n");

    for (AstNode *statement : node->statements)
    {
        result += dump_node(indent_level + 1, statement);
        result += "\n";
    }

    result += indent(indent_level, ")");

    return result;
}

std::string to_string(int indent_level, AstDecl *node)
{
    std::string result;

    result += indent(indent_level, "Decl(\n");
    result += std::format("{}\n", indent(indent_level + 1, text_of(&node->ident)));
    result += std::format("{}\n", dump_node(indent_level + 1, node->init_expr));
    result += indent(indent_level, ")");

    return result;
}

std::string to_string(int indent_level, AstIdent *node)
{
    std::string result;

    result += indent(indent_level, std::format("Ident({})", text_of(&node->ident)));

    return result;
}

std::string to_string(int indent_level, AstLiteral *node)
{
    std::string result;

    result += indent(indent_level, std::format("Literal({})", text_of(&node->token)));

    return result;
}

std::string to_string(int indent_level, AstArg *node)
{
    std::string result;

    result += indent(indent_level, "Arg(\n");
    result += std::format("{}\n", indent(indent_level + 1, text_of(&node->ident)));
    result += std::format("{}\n", indent(indent_level + 1, text_of(&node->type)));
    result += indent(indent_level, ")");

    return result;
}

std::string to_string(int indent_level, AstProc *node)
{
    std::string result;

    result += indent(indent_level, "Proc(\n");
    result += std::format("{}\n", dump_node(indent_level + 1, &node->signature));
    result += std::format("{}\n", dump_node(indent_level + 1, &node->body));
    result += indent(indent_level, ")");

    return result;
}

std::string to_string(int indent_level, AstProcCall *node)
{
    std::string result;

    result += indent(indent_level, "ProcCall(\n");
    result += std::format("{}\n", indent(indent_level + 1, "Proc:"));
    result += std::format("{}\n", dump_node(indent_level + 2, node->proc));
    result += std::format("{}\n", indent(indent_level + 1, "Args:"));

    for (AstNode *arg : node->arguments)
    {
        result += std::format("{}\n", dump_node(indent_level + 2, arg));
    }

    result += indent(indent_level, ")");

    return result;
}

std::string to_string(int indent_level, AstProcSignature *node)
{
    std::string result;

    result += indent(indent_level, "ProcSignature(\n");

    for (AstArg &arg : node->arguments)
    {
        result += std::format("{}\n", dump_node(indent_level + 1, &arg));
    }

    result += indent(indent_level, ")");

    return result;
}

std::string to_string(int indent_level, AstProgram *node)
{
    std::string result;

    result += indent(indent_level, "Program(\n");

    for (AstNode *decl : node->block.statements)
    {
        result += dump_node(indent_level + 1, decl);
        result += "\n";
    }

    result += indent(indent_level, ")");

    return result;
}

std::string to_string(int indent_level, AstReturn *node)
{
    std::string result;

    result += indent(indent_level, "Return(\n");
    result += std::format("{}\n", dump_node(indent_level + 1, node->expr));
    result += indent(indent_level, ")");

    return result;
}

std::string dump_node(int indent_level, AstNode *node)
{
    switch (node->kind)
    {
        case AST_BIN_OP:         return to_string(indent_level, static_cast<AstBinOp *>(node));
        case AST_BLOCK:          return to_string(indent_level, static_cast<AstBlock *>(node));
        case AST_DECL:           return to_string(indent_level, static_cast<AstDecl *>(node));
        case AST_IDENT:          return to_string(indent_level, static_cast<AstIdent *>(node));
        case AST_LITERAL:        return to_string(indent_level, static_cast<AstLiteral *>(node));
        case AST_ARG:            return to_string(indent_level, static_cast<AstArg *>(node));
        case AST_PROC:           return to_string(indent_level, static_cast<AstProc *>(node));
        case AST_PROC_CALL:      return to_string(indent_level, static_cast<AstProcCall *>(node));
        case AST_PROC_SIGNATURE: return to_string(indent_level, static_cast<AstProcSignature *>(node));
        case AST_PROGRAM:        return to_string(indent_level, static_cast<AstProgram *>(node));
        case AST_RETURN:         return to_string(indent_level, static_cast<AstReturn *>(node));
        default:                 assert(false);
    }
}
