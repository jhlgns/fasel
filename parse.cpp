#include "parse.h"
#include "arena.h"
#include <charconv>
#include <format>
#include <iostream>

// TODO: The parsing functions should return an invalid parser on critical error
// TODO: eat_identifier() etc. must return a parser. The advance method must take an parser to advance

struct Parser
{
    Lexer lexer;
    const char *error_context;
    // struct AstBlock *current_block;

    // Print out an error message with this context the next time error() is called on this parser
    void arm(const char *context) { this->error_context = context; }

    [[nodiscard]] Parser quiet()
    {
        auto result          = *this;
        result.error_context = nullptr;
        return result;
    }

    void error(const Parser *start, std::string_view error) const
    {
        if (this->error_context == nullptr)
        {
            return;
        }

        auto current_token = start->peek_token();
        auto line_start    = current_token.pos.c;
        while (line_start + 1 > start->lexer.source && *(line_start - 1) != '\n')
        {
            --line_start;
        }

        while (*line_start != '\n' && (*line_start == ' ' || *line_start == '\t'))
        {
            ++line_start;
        }

        auto line_end = current_token.pos.c;
        while (*line_end != '\n' && line_end + 1 < start->lexer.source.data() + start->lexer.source.size())
        {
            ++line_end;
        }

        auto line = std::string_view{line_start, line_end};
        assert(line.find("\n") == std::string_view::npos);

        std::cout << std::format(
                         "Parser error at {}:{}\n{}\n{}",
                         start->lexer.at.line,
                         start->lexer.at.line_offset,
                         line,
                         error)
                  << std::endl;
    }

    Parser expect_token(TokenType type, Token *token) const
    {
        auto start = *this;
        auto p     = *this;

        auto t = p.next_token();
        if (t.type != type)
        {
            this->error(
                &start,
                std::format("Expected {}, got {} ({})", to_string(type), to_string(t.type), text_of(&t)));

            return start;
        }

        if (token != nullptr)
        {
            *token = t;
        }

        return p;
    }

    Token peek_token() const
    {
        auto token = ::peek_token(&this->lexer);
        return token;
    }

    Token next_token()
    {
        auto token = ::next_token(&this->lexer);
        return token;
    }

    Parser parse_token(TokenType type) { return this->expect_token(type, nullptr); }

    Parser parse_keyword(const char *kw) const
    {
        auto start = *this;
        auto p     = *this;

        auto t = p.next_token();
        if (t.type != TOK_KEYWORD)
        {
            p.error(&start, std::format("Expected keyword '{}', got '{}'", kw, to_string(t.type)));

            return start;
        }

        auto len = strlen(kw);
        if (t.len < len)
        {
            len = t.len;
        }
        if (strncmp(t.pos.c, kw, len) != 0)
        {
            p.error(&start, std::format("Expected keyword '{}', got '{}'", kw, to_string(t.type)));

            return start;
        }

        return p;
    }

    bool advance(const Parser &parsed)
    {
        if (parsed.lexer.at.c <= this->lexer.at.c)
        {
            return false;
        }

        this->lexer = parsed.lexer;

        return true;
    }

    bool operator>>=(const Parser &other) { return this->advance(other); }
};

Parser parse_expr(Parser p, AstNode **out_expr);
Parser parse_decl(Parser p, AstDecl *out_decl);
Parser parse_block(Parser p, AstBlock *out_block);

Parser parse_statement(Parser p, AstNode **out_statement)
{
    auto start = p;

    AstDecl decl{};
    if (p >>= parse_decl(p.quiet(), &decl))
    {
        *out_statement = new AstDecl{std::move(decl)};
        return p;
    }

    if (p >>= p.quiet().parse_keyword("if"))
    {
        p.arm("parsing if statement");

        // TODO
        AstIf if_{};
        // auto if_ = new AstIf{};
        // if_->then_block.parent_block = p.current_block;
        // if_->else_block.parent_block = p.current_block;

        if (!(p >>= parse_expr(p, &if_.condition)))
        {
            return start;
        }

        // TODO: Allow simple statements instead of blocks for then and else

        if (!(p >>= parse_block(p, &if_.then_block)))
        {
            return start;
        }

        if (p >>= p.quiet().parse_keyword("else"))
        {
            if (!(p >>= parse_block(p, &if_.else_block)))
            {
                return start;
            }
        }

        *out_statement = new AstIf{std::move(if_)};
        return p;
    }

    if (p >>= p.quiet().parse_keyword("return"))
    {
        p.arm("parsing return statement");

        AstReturn ret{};
        if (!(p >>= parse_expr(p, &ret.expr)))
        {
            return start;
        }

        *out_statement = new AstReturn{std::move(ret)};
        return p;
    }

    AstNode *expr;
    if (p >>= parse_expr(p.quiet(), &expr))
    {
        *out_statement = expr;
        return p;
    }

    AstBlock block{};
    if (p >>= parse_block(p.quiet(), &block))
    {
        *out_statement = new AstBlock{std::move(block)};
        return p;
    }

    p.error(&start, "Failed to parse statement");

    return start;
}

Parser parse_block(Parser p, AstBlock *out_block)
{
    auto start = p;

    // out_block->parent_block = p.current_block;
    // p.current_block         = out_block;
    // defer
    // {
        // p.current_block = out_block->parent_block;
    // };

    if (!(p >>= p.parse_token(TOK_OPENBRACE)))
    {
        return start;
    }

    p.arm("parsing block");

    while (true)
    {
        if (p >>= p.quiet().parse_token(TOK_CLOSEBRACE))
        {
            return p;
        }

        AstNode *stmt = nullptr;
        if (!(p >>= parse_statement(p, &stmt)))
        {
            return start;
        }

        out_block->statements.push_back(stmt);
    }

    return p;
}

Parser parse_proc(Parser p, AstProc *out_proc)
{
    auto start = p;

    if (!(p >>= p.quiet().parse_keyword("proc")))
    {
        return start;
    }

    p.arm("parsing procedure");

    if (!(p >>= p.parse_token(TOK_OPENPAREN)))
    {
        return start;
    }

    while (true)
    {
        auto done = false;
        switch (p.peek_token().type)
        {
            case TOK_COMMA: // TODO: Allows for ',' at beginning of parameter list
                p.next_token();
                break;

            case TOK_CLOSEPAREN:
                p.next_token();
                done = true;
                break;

            default: break;
        }

        if (done)
        {
            break;
        }

        // TODO: Parse AstDecl here and remove AstArg completely
        AstArg arg{};
        if (!(p >>= p.expect_token(TOK_IDENT, &arg.ident)))
        {
            return start;
        }

        if (!(p >>= p.expect_token(TOK_IDENT, &arg.type)))
        {
            return start;
        }

        out_proc->signature.arguments.push_back(arg);
    }

    out_proc->body.is_proc_body = true;
    if (!(p >>= parse_block(p, &out_proc->body)))
    {
        return start;
    }

    return p;
}

Parser parse_primary_expr(Parser p, AstNode **out_primary_expr)
{
    auto start = p;

    Token t;
    if (p >>= p.quiet().expect_token(TOK_NUM_LIT, &t))
    {
        p.arm("parsing number literal");

        AstLiteral literal{};
        literal.token = t;
        literal.type  = LIT_INT;

        std::from_chars_result result;
        if (*t.pos.c == '0' && *(t.pos.c + 1) == 'x')
        {
            result = std::from_chars(t.pos.c + 2, t.pos.c + t.len, literal.int_value, 16);
        }
        else
        {
            result = std::from_chars(t.pos.c, t.pos.c + t.len, literal.int_value);
        }

        if (result.ec != std::errc{})
        {
            p.error(&start, "Failed to parse integer literal");
            return start;
        }

        if (result.ptr != t.pos.c + t.len)
        {
            p.error(&start, "Failed to parse integer literal");
            return start;
        }

        *out_primary_expr = new AstLiteral{std::move(literal)};
        return p;
    }

    if (p >>= p.quiet().expect_token(TOK_IDENT, &t))
    {
        auto ident        = new AstIdent{};
        ident->ident      = t;
        *out_primary_expr = ident;
        return p;
    }

    AstProc proc{};
    if (p >>= parse_proc(p, &proc))
    {
        *out_primary_expr = new AstProc{std::move(proc)};
        return p;
    }

    p.error(&start, "Failed to parse primary expression");

    return start;
}

Parser parse_suffix_expr(Parser p, AstNode *lhs, AstNode **node)
{
    auto start = p;

    if (p >>= p.quiet().parse_token(TOK_OPENPAREN))
    {
        p.arm("Parsing procedure call");

        AstProcCall call{};
        call.proc = lhs;

        auto is_end = false;
        while (true)
        {
            if (is_end)
            {
                if (p >>= p.parse_token(TOK_CLOSEPAREN))
                {
                    break;
                }

                return start;
            }

            AstNode *argument;
            if (!(p >>= parse_expr(p, &argument)))
            {
                return start;
            }

            call.arguments.push_back(argument);

            if (!(p >>= p.quiet().parse_token(TOK_COMMA)))
            {
                is_end = true;
            }
        }

        *node = new AstProcCall{std::move(call)};
        return p;
    }

    p.error(&start, "Failed to parse suffix expression");

    return start;
}

Parser parse_binary_expr(Parser p, AstNode **node, int prev_prec)
{
    auto start = p;

    AstNode *lhs;
    if (!(p >>= parse_primary_expr(p, &lhs)))
    {
        return start;
    }
    p >>= parse_suffix_expr(p.quiet(), lhs, &lhs);

    while (true)
    {
        auto op   = p.peek_token();
        auto prec = binop_prec(op.type);

        if (prec == 0 || prec <= prev_prec)
        {
            *node = lhs;
            return p;
        }

        p.next_token();
        p.arm("parsing binary operator right hand side");

        AstNode *rhs;
        if (!(p >>= parse_binary_expr(p, &rhs, prec)))
        {
            return start;
        }

        auto bin_op  = new AstBinOp{};
        bin_op->type = op.type;
        bin_op->lhs  = lhs;
        bin_op->rhs  = rhs;

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
    auto start = p;

    Token ident;
    if (!(p >>= p.expect_token(TOK_IDENT, &ident)))
    {
        return start;
    }

    if (!(p >>= p.parse_token(TOK_DECLASSIGN)))
    {
        return start;
    }

    AstNode *init_expr;
    if (!(p >>= parse_expr(p, &init_expr)))
    {
        return start;
    }

    // decl->block     = p.current_block;
    decl->ident     = ident;
    decl->init_expr = init_expr;

    return p;
}

Parser parse_program(Parser p, AstProgram *prog)
{
    auto start = p;

    // p.current_block = &prog->block;
    p.arm("parsing program");

    while (true)
    {
        // TODO: Arm???

        auto decl = new AstDecl{};
        if (!(p >>= parse_decl(p, decl)))
        {
            return start;
        }

        prog->block.statements.push_back(decl);

        reset_arena();

        if (p.peek_token().type == TOK_EOF)
        {
            return p;
        }
    }

    return p;
}

bool parse_program(std::string_view source, AstProgram *prog)
{
    Lexer l{
        .source = source,
        // .start  = {.c = source.data()},
        .at = {.c = source.data()},
    };
    Parser p{.lexer = l};

    if (!(p >>= parse_program(p, prog)))
    {
        // std::cout << p.error << std::endl;

        return false;
    }

    return true;
}
