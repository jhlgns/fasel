#include "lex.h"

void next(LexerPos *p, int n = 1)
{
    for (int i = 0; i < n; ++i, ++p->c)
    {
        if (*p->c == '\0')
        {
            break;
        }

        if (*p->c == '\n')
        {
            ++p->line;
            p->line_offset = 0;
        }
        else
        {
            ++p->line_offset;
        }
    }
}

Token emit(Lexer *l, TokenType t)
{
    assert(t == TOK_EOF || l->at.c > l->start.c);

    return Token{
        .type = t,
        .pos  = l->start,
        .len  = static_cast<size_t>(l->at.c - l->start.c),
    };
}

bool eat_seq(Lexer *l, std::string_view txt)
{
    if (l->at.c - l->source.data() + txt.size() > l->source.size())
    {
        return 0;
    }

    for (int i = 0; i < txt.size(); ++i)
    {
        if (l->at.c[i] != txt[i])
        {
            return 0;
        }
    }

    next(&l->at, txt.size());
    return 1;
}

bool is_ident(char c, int i)
{
    return c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '_' || (i > 0 && (c >= '0' && c <= '9'));
}

bool is_white(char c)
{
    return c == ' ' || c == '\t' || c == '\n';
}

bool is_digit(char c)
{
    return c >= '0' && c <= '9';
}

bool is_hex_digit(char c)
{
    return c >= '0' && c <= '9' || c >= 'a' && c <= 'f' || c >= 'A' && c <= 'F';
}

Token next_token(Lexer *l)
{
    // TODO: Comments
    // TODO: Floating point literals

    for (; is_white(*l->at.c); next(&l->at))
    {
    }

    l->start = l->at;

    // clang-format off
    auto simple_tokens = {
        std::make_tuple("return", TOK_KEYWORD),
        std::make_tuple("while", TOK_KEYWORD),
        std::make_tuple("proc", TOK_KEYWORD),
        std::make_tuple("else", TOK_KEYWORD),
        std::make_tuple("if", TOK_KEYWORD),

        std::make_tuple(":=", TOK_DECLASSIGN),
        std::make_tuple("<<", TOK_LEFTSHIFT),
        std::make_tuple(">>", TOK_RIGHTSHIFT),
        std::make_tuple("&&", TOK_AND),
        std::make_tuple("||", TOK_OR),
        std::make_tuple("==", TOK_EQ),
        std::make_tuple("!=", TOK_NE),
        std::make_tuple(">=", TOK_GE),
        std::make_tuple("<=", TOK_LE),

        std::make_tuple("*", TOK_ASTERISK),
        std::make_tuple("/", TOK_SLASH),
        std::make_tuple("%", TOK_MOD),
        std::make_tuple("+", TOK_PLUS),
        std::make_tuple("-", TOK_MINUS),
        std::make_tuple("=", TOK_ASSIGN),
        std::make_tuple("&", TOK_BIT_AND),
        std::make_tuple("^", TOK_BIT_XOR),
        std::make_tuple("|", TOK_BIT_OR),
        std::make_tuple(",", TOK_COMMA),
        std::make_tuple("(", TOK_OPENPAREN),
        std::make_tuple(")", TOK_CLOSEPAREN),
        std::make_tuple("{", TOK_OPENBRACE),
        std::make_tuple("}", TOK_CLOSEBRACE),
        std::make_tuple(">", TOK_GT),
        std::make_tuple("<", TOK_LT),
    };
    // clang-format on

    for (auto [text, type] : simple_tokens)
    {
        if (eat_seq(l, text))
        {
            return emit(l, type);
        }
    }

    for (; is_ident(*l->at.c, l->at.c - l->start.c); next(&l->at))
    {
    }
    if (l->at.c > l->start.c)
    {
        return emit(l, TOK_IDENT);
    }

    if (is_digit(*l->at.c))
    {
        next(&l->at);
        if (*l->at.c == 'x')
        {
            next(&l->at);
        }

        for (; is_hex_digit(*l->at.c); next(&l->at))
        {
        }
    }
    if (l->at.c > l->start.c)
    {
        return emit(l, TOK_NUM_LIT);
    }

    if (*l->at.c == '\0')
    {
        return emit(l, TOK_EOF);
    }

    printf("Lexer error at %d:%d: unable to parse token.\n", l->at.line, l->at.line_offset);
    next(&l->at);

    return emit(l, TOK_EOF);
}

void reset(Lexer *l, Token token)
{
    l->start = token.pos;
    l->at    = token.pos;
}
