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

Token next_token(Lexer *l)
{
  // TODO: Comments
  // TODO: Floating point literals

    for (; is_white(*l->at.c); next(&l->at))
    {
    }

    l->start = l->at;

    const char *keywords[] = {"return", "proc", "if", "else", "while"};
    for (auto kw : keywords)
    {
        if (eat_seq(l, kw))
        {
            return emit(l, TOK_KEYWORD);
        }
    }

    for (; is_ident(*l->at.c, l->at.c - l->start.c); next(&l->at))
    {
    }
    if (l->at.c > l->start.c)
    {
        return emit(l, TOK_IDENT);
    }

    for (; is_digit(*l->at.c); next(&l->at))
    {
    }
    if (l->at.c > l->start.c)
    {
        return emit(l, TOK_NUM_LIT);
    }

    if (eat_seq(l, ":="))
    {
        return emit(l, TOK_DECLASSIGN);
    }

    char single_char_tokens[] = {'*', '/', '%', '+', '-', ',', '(', ')', '{', '}', '='};
    for (int i = 0; i < sizeof(single_char_tokens); ++i)
    {
        if (*l->at.c == single_char_tokens[i])
        {
            next(&l->at);
            return emit(l, static_cast<TokenType>(single_char_tokens[i]));
        }
    }

    if (*l->at.c == '\0')
    {
        return emit(l, TOK_EOF);
    }

    printf("Lexer error at %d:%d: unable to parse token.\n", l->at.line, l->at.line_offset);
    /* for (auto c = l->at.c; *c && (c - l->at.c) < 5; ++c) */
    /* { */
    /*     printf("%c", *c); */
    /* } */
    /* printf("...\n"); */
    next(&l->at);

    return emit(l, TOK_EOF);
}

void reset(Lexer *l, Token token)
{
    l->start = token.pos;
    l->at    = token.pos;
}
