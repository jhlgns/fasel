#include "stringify.h"
#include <format>

template<typename... Args>
std::string indent(int indent_level, std::string_view text)
{
    std::string indent = std::string(indent_level * 4, ' ');
    return std::format("{}{}", indent, text);
}

std::string to_string(int indent_level, AstBinaryOperator *node)
{
    std::string result;

    result += indent(indent_level, "BinOp(\n");
    result += std::format("{}\n", indent(indent_level + 1, std::string{(char)node->type}));
    result += std::format("{}\n", dump_node(indent_level + 1, node->lhs));
    result += std::format("{}\n", dump_node(indent_level + 1, node->rhs));
    result += indent(indent_level, ")");

    return result;
}

std::string to_string(int indent_level, AstBlock *node)
{
    std::string result;

    result += indent(indent_level, "Block(\n");

    for (auto statement : node->statements)
    {
        result += dump_node(indent_level + 1, statement);
        result += "\n";
    }

    result += indent(indent_level, ")");

    return result;
}

std::string to_string(int indent_level, AstDeclaration *node)
{
    std::string result;

    result += indent(indent_level, "Decl(\n");
    result += std::format("{}\n", indent(indent_level + 1, node->identifier.text()));
    result += std::format("{}\n", dump_node(indent_level + 1, node->init_expression));
    result += indent(indent_level, ")");

    return result;
}

std::string to_string(int indent_level, AstIdentifier *node)
{
    std::string result;

    result += indent(indent_level, std::format("Ident({})", node->identifier.text()));

    return result;
}

std::string to_string(int indent_level, AstSimpleType *node)
{
    std::string result;

    result += indent(indent_level, std::format("SimpleType({})", node->identifier.text()));

    return result;
}

std::string to_string(int indent_level, AstIf *node)
{
    std::string result;

    // TODO
    result += std::format("{}\n", indent(indent_level, std::format("If(\n")));
    result += std::format("{}\n", indent(indent_level + 1, "Condition:\n"));
    result += std::format("{}\n", dump_node(indent_level + 2, node->condition));
    result += std::format("{}\n", indent(indent_level + 1, "Then:\n"));
    result += std::format("{}\n", dump_node(indent_level + 2, &node->then_block));
    result += std::format("{}\n", indent(indent_level + 1, "Else:\n"));
    result += std::format("{}\n", dump_node(indent_level + 2, node->else_block));
    result += indent(indent_level, std::format(")"));

    return result;
}

std::string to_string(int indent_level, AstLiteral *node)
{
    std::string result;

    result += indent(indent_level, std::format("Literal({})", node->token.text()));

    return result;
}

std::string to_string(int indent_level, AstProcedure *node)
{
    std::string result;

    result += indent(indent_level, "Proc(\n");
    result += std::format("{}\n", dump_node(indent_level + 1, &node->signature));
    result += std::format("{}\n", dump_node(indent_level + 1, &node->body));
    result += indent(indent_level, ")");

    return result;
}

std::string to_string(int indent_level, AstProcedureCall *node)
{
    std::string result;

    result += indent(indent_level, "ProcCall(\n");
    result += std::format("{}\n", indent(indent_level + 1, "Proc:"));
    result += std::format("{}\n", dump_node(indent_level + 2, node->procedure));
    result += std::format("{}\n", indent(indent_level + 1, "Args:"));

    for (AstNode *arg : node->arguments)
    {
        result += std::format("{}\n", dump_node(indent_level + 2, arg));
    }

    result += indent(indent_level, ")");

    return result;
}

std::string to_string(int indent_level, AstProcedureSignature *node)
{
    std::string result;

    result += indent(indent_level, "ProcSignature(\n");

    for (auto &arg : node->arguments)
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
    result += std::format("{}\n", dump_node(indent_level + 1, node->expression));
    result += indent(indent_level, ")");

    return result;
}

std::string dump_node(int indent_level, AstNode *node)
{
    if (node == nullptr)
    {
        return "NULL";
    }

    switch (node->kind)
    {
        case AstKind::binary_operator:     return to_string(indent_level, static_cast<AstBinaryOperator *>(node));
        case AstKind::block:               return to_string(indent_level, static_cast<AstBlock *>(node));
        case AstKind::declaration:         return to_string(indent_level, static_cast<AstDeclaration *>(node));
        case AstKind::if_statement:           return to_string(indent_level, static_cast<AstIf *>(node));
        case AstKind::identifier:          return to_string(indent_level, static_cast<AstIdentifier *>(node));
        case AstKind::literal:             return to_string(indent_level, static_cast<AstLiteral *>(node));
        case AstKind::procedure:           return to_string(indent_level, static_cast<AstProcedure *>(node));
        case AstKind::procedure_call:      return to_string(indent_level, static_cast<AstProcedureCall *>(node));
        case AstKind::procedure_signature: return to_string(indent_level, static_cast<AstProcedureSignature *>(node));
        case AstKind::program:             return to_string(indent_level, static_cast<AstProgram *>(node));
        case AstKind::return_statement:    return to_string(indent_level, static_cast<AstReturn *>(node));
        case AstKind::simple_type:         return to_string(indent_level, static_cast<AstSimpleType *>(node));
        default:                           assert(false);
    }
}
