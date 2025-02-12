#pragma once

#include <cassert>
#include <cstddef>
#include <string_view>

enum TokenType
{
    TOK_ASTERISK   = '*',
    TOK_SLASH      = '/',
    TOK_MOD        = '%',
    TOK_PLUS       = '+',
    TOK_MINUS      = '-',
    TOK_EQUALS     = '=',
    TOK_COMMA      = ',',
    TOK_OPENPAREN  = '(',
    TOK_CLOSEPAREN = ')',
    TOK_OPENBRACE  = '{',
    TOK_CLOSEBRACE = '}',
    /* TOK_OPENBRACKET = '[', */
    /* TOK_CLOSERACKET = ']', */
    TOK_IDENT = 255,
    TOK_KEYWORD,
    TOK_NUM_LIT,
    TOK_DECLASSIGN,  // :=
    TOK_EOF,
};

inline const char *to_string(TokenType t)
{
    switch (t)
    {
        case TOK_ASTERISK:   return "TOK_ASTERISK";
        case TOK_SLASH:      return "TOK_SLASH";
        case TOK_PLUS:       return "TOK_PLUS";
        case TOK_MINUS:      return "TOK_MINUS";
        case TOK_EQUALS:     return "TOK_EQUALS";
        case TOK_COMMA:      return "TOK_COMMA";
        case TOK_OPENPAREN:  return "TOK_OPENPAREN";
        case TOK_CLOSEPAREN: return "TOK_CLOSEPAREN";
        case TOK_OPENBRACE:  return "TOK_OPENBRACE";
        case TOK_CLOSEBRACE: return "TOK_CLOSEBRACE";
        case TOK_IDENT:      return "TOK_IDENT";
        case TOK_KEYWORD:    return "TOK_KEYWORD";
        case TOK_NUM_LIT:    return "TOK_NUM_LIT";
        case TOK_DECLASSIGN: return "TOK_DECLASSIGN";
        case TOK_EOF:        return "TOK_EOF";
        default:             return "(unknown!)";
    }
}

// The higher the precedence, the stronger the operator binds its arguments.
// '*' has higher precedence than '+'.
inline int binop_prec(TokenType binop)
{
    switch (binop)
    {
        case '*':
        case '/':
        case '%': return 100;
        case '+':
        case '-': return 90;
        case '=': return 80;
        /* case ',': return 15; */
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
    LexerPos start;
    LexerPos at;
};

Token next_token(Lexer *l);
void reset(Lexer *l, Token token);
