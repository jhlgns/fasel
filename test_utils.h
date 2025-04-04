#pragma once

#include "parse.h"

inline AstBinaryOperator *make_binary_operator(TokenType type, AstNode *lhs, AstNode *rhs)
{
    auto result  = new AstBinaryOperator{};
    result->type = type;
    result->lhs  = lhs;
    result->rhs  = rhs;
    return result;
}

inline AstBlock *make_block(std::vector<AstNode *> &&statements)
{
    auto result        = new AstBlock{};
    result->statements = std::move(statements);
    return result;
}

inline AstDeclaration *make_declaration(std::string_view identifier, AstNode *specified_type, AstNode *init_expression)
{
    auto result        = new AstDeclaration{};
    result->identifier = Token{
        .type = Tt::identifier,
        .pos  = Cursor{.at = identifier.data(), .line = -1, .line_offset = -1},
        .length  = identifier.size(),
    };
    result->type            = specified_type;
    result->init_expression = init_expression;
    return result;
}

inline AstIdentifier *make_identifier(std::string_view identifier)
{
    auto result        = new AstIdentifier{};
    result->identifier = Token{
        .type = Tt::identifier,
        .pos  = Cursor{.at = identifier.data(), .line = -1, .line_offset = -1},
        .length  = identifier.size(),
    };
    return result;
}

inline AstIf *make_if(AstNode *condition, AstBlock *then_block, AstBlock *else_block)
{
    auto result        = new AstIf{};
    result->condition  = condition;
    result->then_block = std::move(*then_block);
    result->else_block = else_block;
    return result;
}

inline AstLiteral *make_int_literal(uint64_t value, char suffix = '\0')
{
    auto result = new AstLiteral{};
    result->value.emplace<uint64_t>(value);
    result->suffix = suffix;
    return result;
}

inline AstLiteral *make_float_literal(float value)
{
    auto result = new AstLiteral{};
    result->value.emplace<float>(value);
    result->suffix = 'f';
    return result;
}

inline AstLiteral *make_double_literal(double value)
{
    auto result = new AstLiteral{};
    result->value.emplace<double>(value);
    return result;
}

inline AstLiteral *make_bool_literal(bool value)
{
    auto result = new AstLiteral{};
    result->value.emplace<bool>(value);
    return result;
}

inline AstProcedure *make_procedure(AstProcedureSignature *signature, AstBlock *body)
{
    auto result       = new AstProcedure{};
    result->signature = std::move(*signature);
    result->body      = std::move(*body);
    return result;
}

inline AstProcedureCall *make_procedure_call(AstNode *procedure, std::vector<AstNode *> &&arguments)
{
    auto result       = new AstProcedureCall{};
    result->procedure = procedure;
    result->arguments = std::move(arguments);
    return result;
}

inline AstProcedureSignature *make_procedure_signature(std::vector<AstDeclaration> arguments)
{
    auto result       = new AstProcedureSignature{};
    result->arguments = std::move(arguments);
    return result;
}

inline AstModule *make_module(std::vector<AstDeclaration> declarations)
{
    auto result = new AstModule{};
    for (auto &&decl : declarations)
    {
        result->block.statements.push_back(new AstDeclaration{std::move(decl)});
    }
    return result;
}

inline AstReturn *make_return(AstNode *expression)
{
    auto result        = new AstReturn{};
    result->expression = expression;
    return result;
}

inline AstTypeIdentifier *make_simple_type(std::string_view identifier)
{
    auto result        = new AstTypeIdentifier{};
    result->identifier = Token{
        .type = Tt::identifier,
        .pos  = Cursor{.at = identifier.data(), .line = -1, .line_offset = -1},
        .length  = identifier.size(),
    };
    return result;
}

inline AstPointerType *make_pointer_type(AstNode *target_type)
{
    auto result         = new AstPointerType{};
    result->target_type = target_type;
    return result;
}

inline AstArrayType *make_array_type(AstNode *length_expression, AstNode *element_type)
{
    auto result               = new AstArrayType{};
    result->element_type      = element_type;
    result->length_expression = length_expression;
    return result;
}
