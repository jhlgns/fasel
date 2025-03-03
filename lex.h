#pragma once

#include <cassert>
#include <cstddef>
#include <string_view>

enum TokenType
{
    TOK_ASTERISK, // *
    TOK_SLASH, // /
    TOK_MOD, // %
    TOK_PLUS, // +
    TOK_MINUS, // -
    TOK_ASSIGN, // =
    TOK_BIT_AND, // &
    TOK_BIT_OR, // |
    TOK_BIT_XOR, // ^
    TOK_LEFTSHIFT,  // <<
    TOK_RIGHTSHIFT,  // >>
    TOK_EQ,  // ==
    TOK_NE,  // !=
    TOK_GE,  // >=
    TOK_GT,  // >
    TOK_LE,  // <=
    TOK_LT,  // <

    TOK_AND,  // &&
    TOK_OR,  // ||

    TOK_COMMA, // ,

    TOK_OPENPAREN, // (
    TOK_CLOSEPAREN, // )
    TOK_OPENBRACE, // {
    TOK_CLOSEBRACE, // }
    TOK_DECLASSIGN,  // :=
    TOK_IDENT,
    TOK_KEYWORD,
    TOK_NUM_LIT,

    TOK_EOF,
};

inline const char *to_string(TokenType t)
{
    switch (t)
    {
        case TOK_ASTERISK:   return "TOK_ASTERISK";
        case TOK_SLASH:      return "TOK_SLASH";
        case TOK_MOD:        return "TOK_MOD";
        case TOK_PLUS:       return "TOK_PLUS";
        case TOK_MINUS:      return "TOK_MINUS";
        case TOK_ASSIGN:     return "TOK_ASSIGN";
        case TOK_BIT_AND:    return "TOK_BIT_AND";
        case TOK_BIT_XOR:    return "TOK_BIT_XOR";
        case TOK_BIT_OR:     return "TOK_BIT_OR";
        case TOK_COMMA:      return "TOK_COMMA";
        case TOK_OPENPAREN:  return "TOK_OPENPAREN";
        case TOK_CLOSEPAREN: return "TOK_CLOSEPAREN";
        case TOK_OPENBRACE:  return "TOK_OPENBRACE";
        case TOK_CLOSEBRACE: return "TOK_CLOSEBRACE";
        case TOK_DECLASSIGN: return "TOK_DECLASSIGN";
        case TOK_LEFTSHIFT:  return "TOK_LEFTSHIFT";
        case TOK_RIGHTSHIFT: return "TOK_RIGHTSHIFT";
        case TOK_AND:        return "TOK_AND";
        case TOK_OR:         return "TOK_OR";
        case TOK_EQ:         return "TOK_EQ";
        case TOK_NE:         return "TOK_NE";
        case TOK_GE:         return "TOK_GE";
        case TOK_GT:         return "TOK_GT";
        case TOK_LE:         return "TOK_LE";
        case TOK_LT:         return "TOK_LT";
        case TOK_IDENT:      return "TOK_IDENT";
        case TOK_KEYWORD:    return "TOK_KEYWORD";
        case TOK_NUM_LIT:    return "TOK_NUM_LIT";
        default:             return "(unknown!)";
    }
}

// The higher the precedence, the stronger the operator binds its arguments.
// '*' has higher precedence than '+'.
inline int binop_prec(TokenType binop)
{
    switch (binop)
    {
        case TOK_ASTERISK: return 110;
        case TOK_SLASH:    return 110;
        case TOK_MOD:      return 110;

        case TOK_PLUS:  return 100;
        case TOK_MINUS: return 100;

        case TOK_LEFTSHIFT:  return 90;
        case TOK_RIGHTSHIFT: return 90;

        case TOK_LT: return 80;
        case TOK_GT: return 80;
        case TOK_LE: return 80;
        case TOK_GE: return 80;

        case TOK_EQ: return 70;
        case TOK_NE: return 70;

        case TOK_BIT_AND: return 50;

        case TOK_BIT_XOR: return 40;

        case TOK_BIT_OR: return 30;

        case TOK_AND: return 20;

        case TOK_OR: return 10;

        default: return 0;
    }
}

struct LexerPos
{
    const char *c;
    int line;
    int line_offset;
};

struct Token
{
    TokenType type;
    LexerPos pos;
    size_t len;
};

inline std::string_view text_of(Token *token)
{
    return std::string_view{token->pos.c, token->len};
}

struct Lexer
{
    std::string_view source;
    // LexerPos start;
    LexerPos at;
};

Token next_token(Lexer *l);
Token peek_token(const Lexer *l);
void reset(Lexer *l, Token token);
