#include "parse.h"
#include "arena.h"
#include "basics.h"
#include <charconv>
#include <span>

struct Parser
{
    Lexer lexer;
    std::span<char *> errors;
    struct AstBlock *current_block;
};

Token next_token(Parser *parser)
{
    return next_token(&parser->lexer);
}

// Attaches the errors of new to the errors of bak, returns bak.
Parser with_error(Parser bak, Parser *errored, const char *format, ...)
{
    assert(bak.errors.size() == 0);

    char fmt[4096] = {0};

    auto next        = next_token(&bak);
    auto line_start = next.pos.c;
    auto line_end   = next.pos.c;
    while (line_start + 1 > bak.lexer.source && *(line_start - 1) != '\n')
        --line_start;
    while (*line_start != '\n' && (*line_start == ' ' || *line_start == '\t'))
        ++line_start;
    while (*line_end != '\n' && (line_end - bak.lexer.source.data()) <= bak.lexer.source.size())
        ++line_end;
    char line[1024] = {0};
    strncpy(line, line_start, line_end - line_start);  // TODO: Max 1024

    snprintf(
        fmt,
        sizeof(fmt),
        "Parser error at %d:%d (line: '%s'): %s",
        (int)bak.lexer.at.line,
        (int)bak.lexer.at.line_offset,
        line,
        format);

    va_list va;
    va_start(va, format);
    char msg[4096] = {0};
    vsnprintf(msg, sizeof(msg), fmt, va);
    va_end(va);

    if (errored != nullptr && errored->errors.size() > 0)
    {
        size_t errors_len = 1 + errored->errors.size();
        auto es           = static_cast<char **>(arena_alloc(sizeof(char *) * errors_len));
        bak.errors        = std::span{es, errors_len};
        bak.errors[0]     = static_cast<char *>(arena_alloc(strlen(msg) + 1));
        strcpy(bak.errors[0], msg);

        for (int i = 0; i < errored->errors.size(); ++i)
        {
            bak.errors[1 + i] = (char *)arena_alloc(strlen(errored->errors[i]) + 1);
            strcpy(bak.errors[1 + i], errored->errors[i]);
        }
    }
    else
    {
        size_t errors_len = 1;
        auto es           = static_cast<char **>(arena_alloc(sizeof(char *) * errors_len));
        bak.errors        = std::span{es, errors_len};
        bak.errors[0]     = static_cast<char *>(arena_alloc(strlen(msg) + 1));
        strcpy(bak.errors[0], msg);
    }

    return bak;
}

Parser eat_token(Parser p, TokenType type, Token *token)
{
    auto bak = p;
    auto t   = next_token(&p);
    if (t.type != type)
    {
        char tkn[1024];
        size_t len = sizeof(tkn);
        if (t.len < len)
            len = t.len;
        strncpy(tkn, t.pos.c, len);
        return with_error(bak, nullptr, "Expected %s, got %s (%s)", to_string(type), to_string(t.type), tkn);
    }

    if (token != nullptr)
    {
        *token = t;
    }

    return p;
}

Parser eat_token(Parser p, char type, Token *token)
{
    return eat_token(p, static_cast<TokenType>(type), token);
}

Parser eat_token(Parser p, char type)
{
    return eat_token(p, static_cast<TokenType>(type), nullptr);
}

Parser eat_token(Parser p, TokenType type)
{
    return eat_token(p, type, nullptr);
}

Parser eat_keyword(Parser p, const char *kw)
{
    auto bak = p;
    auto t   = next_token(&p);
    if (t.type != TOK_KEYWORD)
    {
        return with_error(bak, nullptr, "Expected keyword '%s', got %s", kw, to_string(t.type));
    }

    auto len = strlen(kw);
    if (t.len < len)
    {
        len = t.len;
    }
    if (strncmp(t.pos.c, kw, len) != 0)
    {
        return with_error(bak, nullptr, "Expected keyword '%s', got %s", kw, to_string(t.type));
    }

    return p;
}

bool advance(Parser *p, Parser parsed, bool require)
{
    assert(p->errors.size() == 0);

    if (parsed.errors.size() != 0 || parsed.lexer.at.c <= p->lexer.at.c)
    {
        if (require)
        {
            p->errors = parsed.errors;
        }

        return false;
    }

    *p = parsed;
    return true;
}

bool must(Parser *p, Parser parsed)
{
    return advance(p, parsed, true);
}

bool maybe(Parser *p, Parser parsed)
{
    return advance(p, parsed, false);
}

Parser parse_expr(Parser p, AstNode **out_expr);
Parser parse_decl(Parser p, AstDecl *out_decl);

Parser parse_statement(Parser p, AstNode **out_statement)
{
    auto bak        = p;
    const char *err = "Failed to parse statement";

    AstDecl decl{};
    if (maybe(&p, parse_decl(p, &decl)))
    {
        auto result    = new AstDecl{};
        *result        = decl;
        *out_statement = result;
        return p;
    }

    if (maybe(&p, eat_keyword(p, "return")))
    {
        auto ret = new AstReturn;
        if (must(&p, parse_expr(p, &ret->expr)) == false)
        {
            delete ret;
            return with_error(bak, &p, err);
        }

        *out_statement = ret;
        return p;
    }

    AstNode *expr;
    if (maybe(&p, parse_expr(p, &expr)))
    {
        *out_statement = expr;
        return p;
    }

    return with_error(bak, nullptr, err);
}

Parser parse_block(Parser p, AstBlock *out_block)
{
    auto bak        = p;
    const char *err = "Failed to parse block";

    out_block->parent_block = p.current_block;
    p.current_block         = out_block;
    defer
    {
        p.current_block = out_block->parent_block;
    };

    if (must(&p, eat_token(p, '{')) == false)
    {
        return with_error(bak, &p, err);
    }

    while (true)
    {
        if (maybe(&p, eat_token(p, '}')))
        {
            return p;
        }

        AstNode *stmt = nullptr;
        if (must(&p, parse_statement(p, &stmt)) == false)
        {
            return with_error(bak, &p, err);
        }

        out_block->statements.push_back(stmt);
    }

    return p;
}

Parser parse_primary_expr(Parser p, AstNode **out_primary_expr)
{
    auto bak        = p;
    const char *err = "Failed to parse primary expression";

    Token t;
    if (maybe(&p, eat_token(p, TOK_NUM_LIT, &t)))
    {
        AstLiteral *literal = new AstLiteral;
        literal->token      = t;
        literal->type       = LIT_INT;

        auto result = std::from_chars(t.pos.c, t.pos.c + t.len, literal->int_value);
        if (result.ec != std::errc{})
        {
            return with_error(bak, nullptr, "Failed to parse integer literal");
        }

        if (result.ptr != t.pos.c + t.len)
        {
            return with_error(bak, nullptr, "Failed to parse integer literal");
        }

        *out_primary_expr = literal;
        return p;
    }

    if (maybe(&p, eat_token(p, TOK_IDENT, &t)))
    {
        AstIdent *ident   = new AstIdent;
        ident->ident      = t;
        *out_primary_expr = ident;
        return p;
    }

    if (maybe(&p, eat_keyword(p, "proc")))
    {
        if (must(&p, eat_token(p, '(')) == false)
        {
            return with_error(bak, &p, err);
        }

        auto proc = new AstProc;
        while (true)
        {
            bool done = false;
            auto bak  = p;
            switch (next_token(&p).type)
            {
                case ',': break;  // TODO: Allows for ',' at beginning of parameter list
                case ')': done = 1; break;
                default:  p = bak; break;
            }

            if (done)
                break;

            AstArg arg = {};
            if (must(&p, eat_token(p, TOK_IDENT, &arg.ident)) == false)
            {
                return with_error(bak, &p, err);
            }

            if (must(&p, eat_token(p, TOK_IDENT, &arg.type)) == false)
            {
                return with_error(bak, &p, err);
            }

            proc->signature.arguments.push_back(arg);
        }

        if (must(&p, parse_block(p, &proc->body)) == false)
        {
            return with_error(bak, &p, err);
        }

        *out_primary_expr = proc;
        return p;
    }

    return with_error(bak, nullptr, err);
}

Parser parse_suffix_expr(Parser p, AstNode *lhs, AstNode **node)
{
    const char *err = "Failed to parse suffix expression";
    auto bak        = p;

    if (maybe(&p, eat_token(p, '(')))
    {
        auto call  = new AstProcCall;
        call->proc = lhs;

        auto is_end = false;
        while (true)
        {
            if (advance(&p, eat_token(p, ')'), is_end))
            {
                break;
            }
            else if (is_end)
            {
                return with_error(bak, &p, err);
            }

            AstNode *argument;
            if (must(&p, parse_expr(p, &argument)) == false)
            {
                return with_error(bak, &p, err);
            }

            call->arguments.push_back(argument);

            if (maybe(&p, eat_token(p, ',')) == false)
            {
                is_end = true;
            }
        }

        *node = call;
        return p;
    }

    return with_error(bak, nullptr, err);
}

Parser parse_binary_expr(Parser p, AstNode **node, int prev_prec)
{
    auto bak        = p;
    const char *err = "Failed to parse expression";

    AstNode *lhs;
    if (must(&p, parse_primary_expr(p, &lhs)) == false)
    {
        return with_error(bak, &p, err);
    }
    maybe(&p, parse_suffix_expr(p, lhs, &lhs));

    while (true)
    {
        auto before_op = p;
        auto op        = next_token(&p);
        auto prec      = binop_prec(op.type);

        if (prec == 0 || prec <= prev_prec)
        {
            *node = lhs;
            p     = before_op;
            return p;
        }

        AstNode *rhs;
        if (must(&p, parse_binary_expr(p, &rhs, prec)) == false)
        {
            return with_error(bak, &p, "Failed to parse binary operator right hand side");
        }

        auto bin_op   = new AstBinOp;
        bin_op->binop = op.type;
        bin_op->lhs   = lhs;
        bin_op->rhs   = rhs;

        lhs = bin_op;
    }

    *node = lhs;

    return p;
}

Parser parse_expr(Parser p, AstNode **node)
{
    return parse_binary_expr(p, node, -1);
}

Parser parse_decl(Parser p, AstDecl *decl)
{
    auto bak        = p;
    const char *err = "Failed to parse declaration";

    Token ident;
    if (must(&p, eat_token(p, TOK_IDENT, &ident)) == false)
    {
        return with_error(bak, &p, err);
    }

    if (must(&p, eat_token(p, TOK_DECLASSIGN)) == false)
    {
        return with_error(bak, &p, err);
    }

    AstNode *init_expr;
    if (must(&p, parse_expr(p, &init_expr)) == false)
    {
        return with_error(bak, &p, err);
    }

    decl->ident     = ident;
    decl->init_expr = init_expr;

    return p;
}

Parser parse_program(Parser p, AstProgram *prog)
{
    const char *err = "Failed to parse program";
    auto bak        = p;

    while (true)
    {
        auto decl = new AstDecl{};
        if (must(&p, parse_decl(p, decl)) == false)
        {
            return with_error(bak, &p, err);
        }

        decl->is_global = true;
        prog->block.statements.push_back(decl);

        reset_arena();

        Token next = next_token(&p);
        if (next.type == TOK_EOF)
        {
            return p;
        }

        reset(&p.lexer, next);
    }

    return p;
}

bool parse_program(std::string_view source, AstProgram *prog)
{
    Lexer l{
        .source = source,
        .start  = {.c = source.data()},
        .at     = {.c = source.data()},
    };
    Parser p{.lexer = l};

    if (must(&p, parse_program(p, prog)) == false)
    {
        printf("Failed to parse the program\n");

        for (int i = 0; i < p.errors.size(); ++i)
        {
            for (int j = 0; j < i * 4; ++j)
            {
                printf(" ");
            }

            printf("%s\n", p.errors[i]);
        }

        return false;
    }

    return true;
}
