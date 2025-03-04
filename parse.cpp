#include "parse.h"
#include "string_util.hpp"
#include <charconv>
#include <format>
#include <iostream>

// TODO: The parsing functions should return an invalid parser on critical errors so that all parsing is cancelled

struct Parser
{
    explicit Parser(const Lexer &lexer)
        : lexer{lexer}
    {
    }

    Lexer lexer;
    const char *error_context;

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

        auto line = get_line(start->lexer.source, current_token.pos.at);

        auto message = std::format(
            "Parser error at {}:{}\n{}\n{}",
            start->lexer.cursor.line,
            start->lexer.cursor.line_offset,
            line,
            error);

        std::cout << message << std::endl;
    }

    Token peek_token() const { return this->lexer.peek_token(); }

    Token next_token()
    {
        auto token = this->lexer.next_token();
        while (token.type == Tt::single_line_comment || token.type == Tt::multi_line_comment)
        {
            token = this->lexer.next_token();
        }

        return token;
    }

    Parser parse_token(TokenType type, Token *token) const
    {
        auto start = *this;
        auto p     = *this;

        auto t = p.next_token();
        if (t.type != type)
        {
            this->error(&start, std::format("Expected {}, got {} ({})", to_string(type), to_string(t.type), t.text()));
            return start;
        }

        if (token != nullptr)
        {
            *token = t;
        }

        return p;
    }

    Parser parse_token(TokenType type) const { return this->parse_token(type, nullptr); }

    Parser parse_keyword(std::string_view keyword) const
    {
        auto start = *this;
        auto p     = *this;

        auto t = p.next_token();
        if (t.type != Tt::keyword)
        {
            this->error(&start, std::format("Expected keyword '{}', got '{}'", keyword, to_string(t.type)));
            return start;
        }

        auto len = keyword.size();
        if (t.len < len)
        {
            len = t.len;
        }
        if (strncmp(t.pos.at, keyword.data(), len) != 0)
        {
            p.error(&start, std::format("Expected keyword '{}', got '{}'", keyword, to_string(t.type)));
            return start;
        }

        return p;
    }

    bool advance(const Parser &parsed)
    {
        if (parsed.lexer.cursor.at <= this->lexer.cursor.at)
        {
            return false;
        }

        this->lexer = parsed.lexer;

        return true;
    }

    bool operator>>=(const Parser &other) { return this->advance(other); }
};

Parser parse_expr(Parser p, AstNode **out_expr);
Parser parse_decl(Parser p, AstDeclaration *out_decl);
Parser parse_block(Parser p, AstBlock *out_block);

Parser parse_statement(Parser p, AstNode **out_statement)
{
    auto start = p;

    AstDeclaration decl{};
    if (p >>= parse_decl(p.quiet(), &decl))
    {
        *out_statement = new AstDeclaration{std::move(decl)};
        return p;
    }

    if (p >>= p.quiet().parse_keyword("if"))
    {
        p.arm("parsing if statement");

        AstIf yf{};

        if (!(p >>= parse_expr(p, &yf.condition)))
        {
            return start;
        }

        // TODO: Allow simple statements instead of blocks for then and else

        if (!(p >>= parse_block(p, &yf.then_block)))
        {
            return start;
        }

        if (p >>= p.quiet().parse_keyword("else"))
        {
            if (!(p >>= parse_block(p, &yf.else_block)))
            {
                return start;
            }
        }

        *out_statement = new AstIf{std::move(yf)};
        return p;
    }

    if (p >>= p.quiet().parse_keyword("return"))
    {
        p.arm("parsing return statement");

        AstReturn retyrn{};
        if (!(p >>= parse_expr(p, &retyrn.expression)))
        {
            return start;
        }

        *out_statement = new AstReturn{std::move(retyrn)};
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

    if (!(p >>= p.parse_token(Tt::brace_open)))
    {
        return start;
    }

    p.arm("parsing block");

    while (true)
    {
        if (p >>= p.quiet().parse_token(Tt::brace_close))
        {
            return p;
        }

        AstNode *stmt{};
        if (!(p >>= parse_statement(p, &stmt)))
        {
            return start;
        }

        out_block->statements.push_back(stmt);
    }

    return p;
}

Parser parse_proc(Parser p, AstProcedure *out_proc)
{
    auto start = p;

    if (!(p >>= p.quiet().parse_keyword("proc")))
    {
        return start;
    }

    p.arm("parsing procedure");

    if (!(p >>= p.parse_token(Tt::parenthesis_open)))
    {
        return start;
    }

    auto require_close = false;
    while (true)
    {
        if (require_close)
        {
            if (!(p >>= p.parse_token(Tt::parenthesis_close)))
            {
                return start;
            }

            break;
        }
        else if (p >>= p.quiet().parse_token(Tt::parenthesis_close))
        {
            break;
        }

        AstDeclaration arg{};
        if (!(p >>= parse_decl(p, &arg)))
        {
            return start;
        }

        out_proc->signature.arguments.push_back(std::move(arg));

        if (!(p >>= p.quiet().parse_token(Tt::comma)))
        {
            require_close = true;
        }
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
    if (p >>= p.quiet().parse_token(Tt::numerical_literal, &t))
    {
        p.arm("parsing number literal");

        AstLiteral literal{};
        literal.token = t;
        literal.type  = LiteralType::integer;

        std::from_chars_result result;
        if (t.pos.at[0] == '0' && t.pos.at[1] == 'x')
        {
            result = std::from_chars(t.pos.at + 2, t.pos.at + t.len, literal.int_value, 16);
        }
        else
        {
            result = std::from_chars(t.pos.at, t.pos.at + t.len, literal.int_value);
        }

        if (result.ec != std::errc{})
        {
            p.error(&start, "Failed to parse integer literal");
            return start;
        }

        if (result.ptr != t.pos.at + t.len)
        {
            p.error(&start, "Failed to parse integer literal");
            return start;
        }

        *out_primary_expr = new AstLiteral{std::move(literal)};
        return p;
    }

    if (p >>= p.quiet().parse_token(Tt::identifier, &t))
    {
        auto ident        = new AstIdentifier{};
        ident->identifier = t;
        *out_primary_expr = ident;
        return p;
    }

    AstProcedure proc{};
    if (p >>= parse_proc(p, &proc))
    {
        *out_primary_expr = new AstProcedure{std::move(proc)};
        return p;
    }

    p.error(&start, "Failed to parse primary expression");

    return start;
}

Parser parse_suffix_expr(Parser p, AstNode *lhs, AstNode **node)
{
    auto start = p;

    if (p >>= p.quiet().parse_token(Tt::parenthesis_open))
    {
        p.arm("parsing procedure call");

        AstProcedureCall call{};
        call.proc = lhs;

        auto require_close = false;
        while (true)
        {
            if (require_close)
            {
                if (!(p >>= p.parse_token(Tt::parenthesis_close)))
                {
                    return start;
                }

                break;
            }
            else if (p >>= p.quiet().parse_token(Tt::parenthesis_close))
            {
                break;
            }

            AstNode *argument;
            if (!(p >>= parse_expr(p, &argument)))
            {
                return start;
            }

            call.arguments.push_back(argument);

            if (!(p >>= p.quiet().parse_token(Tt::comma)))
            {
                require_close = true;
            }
        }

        *node = new AstProcedureCall{std::move(call)};
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
        auto prec = binary_operator_precedence(op.type);

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

        auto bin_op  = new AstBinaryOperator{};
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

Parser parse_type(Parser p, AstNode **out_type)
{
    auto start = p;

    Token identifier;
    if (p >>= p.parse_token(Tt::identifier, &identifier))
    {
        auto type        = new AstSimpleType{};
        type->identifier = identifier;

        *out_type = type;

        return p;
    }

    return start;
}

Parser parse_decl(Parser p, AstDeclaration *decl)
{
    auto start = p;

    if (!(p >>= p.parse_token(Tt::identifier, &decl->identifier)))
    {
        return start;
    }

    if (!(p >>= p.parse_token(Tt::colon)))
    {
        return start;
    }

    p.arm("parsing declaration");

    auto has_type = p >>= parse_type(p.quiet(), &decl->type);

    if (p >>= p.quiet().parse_token(Tt::assign))
    {
        if (!(p >>= parse_expr(p, &decl->init_expression)))
        {
            return start;
        }
    }
    else if (has_type == false)
    {
        p.error(&start, "Declaration without init expression needs a type");
    }

    return p;
}

Parser parse_program(Parser p, AstProgram *prog)
{
    auto start = p;

    // p.current_block = &prog->block;
    p.arm("parsing program");

    while (true)
    {
        auto decl = new AstDeclaration{};
        if (!(p >>= parse_decl(p, decl)))
        {
            return start;
        }

        prog->block.statements.push_back(decl);

        if (p.peek_token().type == Tt::eof)
        {
            return p;
        }
    }

    return p;
}

bool parse_program(std::string_view source, AstProgram *prog)
{
    Lexer lexer{source};
    Parser p{lexer};

    if (!(p >>= parse_program(p, prog)))
    {
        std::cout << "Compilation failed" << std::endl;

        return false;
    }

    return true;
}
