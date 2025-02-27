#include "parse.h"
#include "arena.h"
#include "basics.h"
#include <charconv>
#include <format>
#include <iostream>

/*
TODO: Error reporting: at a certain point, we know that a syntax error happened without relying on the parent parse context handling it.
For example, if we parse the keyword 'proc', then we know we must parse a procedure.
The parent context might discard the error message and set its own onw...
*/

struct Parser
{
    Lexer lexer;
    std::string error;  // TODO: Make this an arena allocated string_view
    struct AstBlock *current_block;
};

Token next_token(Parser *parser)
{
    return next_token(&parser->lexer);
}

Parser copy_error(Parser bak, std::string_view error)
{
    bak.error = error;
    return bak;
}

Parser add_error(Parser bak, std::string_view error)
{
    auto next       = next_token(&bak);
    auto line_start = next.pos.c;
    while (line_start + 1 > bak.lexer.source && *(line_start - 1) != '\n')
    {
        --line_start;
    }

    while (*line_start != '\n' && (*line_start == ' ' || *line_start == '\t'))
    {
        ++line_start;
    }

    auto line_end = next.pos.c;
    while (*line_end != '\n' && line_end + 1 < bak.lexer.source.data() + bak.lexer.source.size())
    {
        ++line_end;
    }

    auto line = std::string_view{line_start, line_end};
    assert(line.find("\n") == std::string_view::npos);
    bak.error = std::format("Parser error at {}:{}\n{}\n{}", bak.lexer.at.line, bak.lexer.at.line_offset, line, error);

    return bak;
}

Parser eat_token(Parser p, TokenType type, Token *token)
{
    auto bak = p;
    auto t   = next_token(&p);
    if (t.type != type)
    {
        return add_error(bak, std::format("Expected {}, got {} ({})", to_string(type), to_string(t.type), text_of(&t)));
    }

    if (token != nullptr)
    {
        *token = t;
    }

    return p;
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
        return add_error(bak, std::format("Expected keyword '{}', got '{}'", kw, to_string(t.type)));
    }

    auto len = strlen(kw);
    if (t.len < len)
    {
        len = t.len;
    }
    if (strncmp(t.pos.c, kw, len) != 0)
    {
        return add_error(bak, std::format("Expected keyword '{}', got '{}'", kw, to_string(t.type)));
    }

    return p;
}

bool advance(Parser *p, Parser parsed, bool require)
{
    assert(p->error.empty());

    if (parsed.error.empty() == false || parsed.lexer.at.c <= p->lexer.at.c)
    {
        if (require)
        {
            p->error = parsed.error;
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
Parser parse_block(Parser p, AstBlock *out_block);

Parser parse_statement(Parser p, AstNode **out_statement)
{
    auto bak = p;

    AstDecl decl{};
    if (maybe(&p, parse_decl(p, &decl)))
    {
        auto result    = new AstDecl{};
        *result        = decl;
        *out_statement = result;
        return p;
    }

    if (maybe(&p, eat_keyword(p, "if")))
    {
        AstIf if_;
        if (must(&p, parse_expr(p, &if_.condition)) == false)
        {
            return copy_error(bak, p.error);
        }

        if (must(&p, parse_block(p, &if_.body)) == false)
        {
            return copy_error(bak, p.error);
        }

        auto result    = new AstIf{};
        *result        = std::move(if_);
        *out_statement = result;
        return p;
    }

    if (maybe(&p, eat_keyword(p, "return")))
    {
        AstReturn ret{};
        if (must(&p, parse_expr(p, &ret.expr)) == false)
        {
            return copy_error(bak, p.error);
        }

        *out_statement = new AstReturn{ret};
        return p;
    }

    AstNode *expr;
    if (maybe(&p, parse_expr(p, &expr)))
    {
        *out_statement = expr;
        return p;
    }

    AstBlock block{};
    if (maybe(&p, parse_block(p, &block)))
    {
        *out_statement = new AstBlock{std::move(block)};
        return p;
    }

    return add_error(bak, "Failed to parse statement");
}

Parser parse_block(Parser p, AstBlock *out_block)
{
    auto bak = p;

    out_block->parent_block = p.current_block;
    p.current_block         = out_block;
    defer
    {
        p.current_block = out_block->parent_block;
    };

    if (must(&p, eat_token(p, TOK_OPENBRACE)) == false)
    {
        return copy_error(bak, p.error);
    }

    while (true)
    {
        if (maybe(&p, eat_token(p, TOK_CLOSEBRACE)))
        {
            return p;
        }

        AstNode *stmt = nullptr;
        if (must(&p, parse_statement(p, &stmt)) == false)
        {
            return copy_error(bak, p.error);
        }

        out_block->statements.push_back(stmt);
    }

    return p;
}

Parser parse_proc(Parser p, AstProc *out_proc)
{
    auto bak = p;

    if (maybe(&p, eat_keyword(p, "proc")) == false)
    {
        return copy_error(bak, p.error);
    }
    if (must(&p, eat_token(p, TOK_OPENPAREN)) == false)
    {
        return copy_error(bak, p.error);
    }

    while (true)
    {
        bool done = false;
        auto bak  = p;
        switch (next_token(&p).type)
        {
            case TOK_COMMA: break;  // TODO: Allows for ',' at beginning of parameter list
            case TOK_CLOSEPAREN: done = 1; break;
            default:  p = bak; break;
        }

        if (done)
        {
            break;
        }

        AstArg arg{};
        if (must(&p, eat_token(p, TOK_IDENT, &arg.ident)) == false)
        {
            return copy_error(bak, p.error);
        }

        if (must(&p, eat_token(p, TOK_IDENT, &arg.type)) == false)
        {
            return copy_error(bak, p.error);
        }

        out_proc->signature.arguments.push_back(arg);
    }

    out_proc->body.is_proc_body = true;
    if (must(&p, parse_block(p, &out_proc->body)) == false)
    {
        return copy_error(bak, p.error);
    }

    return p;
}

Parser parse_primary_expr(Parser p, AstNode **out_primary_expr)
{
    auto bak = p;

    Token t;
    if (maybe(&p, eat_token(p, TOK_NUM_LIT, &t)))
    {
        auto literal   = new AstLiteral{};
        literal->token = t;
        literal->type  = LIT_INT;

        auto result = std::from_chars(t.pos.c, t.pos.c + t.len, literal->int_value);
        if (result.ec != std::errc{})
        {
            return add_error(bak, "Failed to parse integer literal");
        }

        if (result.ptr != t.pos.c + t.len)
        {
            return add_error(bak, "Failed to parse integer literal");
        }

        *out_primary_expr = literal;
        return p;
    }

    if (maybe(&p, eat_token(p, TOK_IDENT, &t)))
    {
        auto ident        = new AstIdent{};
        ident->ident      = t;
        *out_primary_expr = ident;
        return p;
    }

    AstProc proc{};
    if (maybe(&p, parse_proc(p, &proc)))
    {
        *out_primary_expr = new AstProc{proc};
        return p;
    }

    return add_error(bak, "Failed to parse primary expression");
}

Parser parse_suffix_expr(Parser p, AstNode *lhs, AstNode **node)
{
    auto bak = p;

    if (maybe(&p, eat_token(p, TOK_OPENPAREN)))
    {
        AstProcCall call{};
        call.proc = lhs;

        auto is_end = false;
        while (true)
        {
            if (advance(&p, eat_token(p, TOK_CLOSEPAREN), is_end))
            {
                break;
            }
            else if (is_end)
            {
                return copy_error(bak, p.error);
            }

            AstNode *argument;
            if (must(&p, parse_expr(p, &argument)) == false)
            {
                return copy_error(bak, p.error);
            }

            call.arguments.push_back(argument);

            if (maybe(&p, eat_token(p, TOK_COMMA)) == false)
            {
                is_end = true;
            }
        }

        *node = new AstProcCall{std::move(call)};
        return p;
    }

    return add_error(bak, "Failed to parse suffix expression");
}

Parser parse_binary_expr(Parser p, AstNode **node, int prev_prec)
{
    auto bak = p;

    AstNode *lhs;
    if (must(&p, parse_primary_expr(p, &lhs)) == false)
    {
        return copy_error(bak, p.error);
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
            return add_error(bak, "Failed to parse binary operator right hand side");
        }

        auto bin_op   = new AstBinOp{};
        bin_op->type = op.type;
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
    auto bak = p;

    Token ident;
    if (must(&p, eat_token(p, TOK_IDENT, &ident)) == false)
    {
        return copy_error(bak, p.error);
    }

    if (must(&p, eat_token(p, TOK_DECLASSIGN)) == false)
    {
        return copy_error(bak, p.error);
    }

    AstNode *init_expr;
    if (must(&p, parse_expr(p, &init_expr)) == false)
    {
        return copy_error(bak, p.error);
    }

    decl->ident     = ident;
    decl->init_expr = init_expr;

    return p;
}

Parser parse_program(Parser p, AstProgram *prog)
{
    auto bak = p;

    p.current_block = &prog->block;

    while (true)
    {
        auto decl = new AstDecl{};
        if (must(&p, parse_decl(p, decl)) == false)
        {
            return copy_error(bak, p.error);
        }

        prog->block.statements.push_back(decl);

        reset_arena();

        auto next = next_token(&p);
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
        std::cout << p.error << std::endl;

        return false;
    }

    return true;
}

/* Parser parse_brogram(Parser p, void *brogram) */
/* { */
/*     auto bak = p; */

/*     if (maybe(&p, eat_keyword(p, "")) == false) */
/*     { */
/*         // Error parsing brogram: expected keyword */
/*         return add_error(bak, p.error); */
/*     } */
/* } */
