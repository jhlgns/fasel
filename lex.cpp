#include "lex.h"

#include <format>
#include <iostream>

void next(Cursor &cursor, int n = 1)
{
    for (int i = 0; i < n; ++i, ++cursor.at)
    {
        if (*cursor.at == '\0')
        {
            break;
        }

        if (*cursor.at == '\n')
        {
            ++cursor.line;
            cursor.line_offset = 0;
        }
        else
        {
            ++cursor.line_offset;
        }
    }
}

Token emit(Lexer &lexer, const Cursor &start, TokenType t)
{
    assert(t == Tt::eof || lexer.cursor.at > start.at);

    return Token{
        .type = t,
        .pos  = start,
        .len  = static_cast<size_t>(lexer.cursor.at - start.at),
    };
}

bool consume_sequence(Lexer &lexer, std::string_view prefix)
{
    if (lexer.cursor.at - lexer.source.data() + prefix.size() > lexer.source.size())
    {
        return false;
    }

    for (int i = 0; i < prefix.size(); ++i)
    {
        if (lexer.cursor.at[i] != prefix[i])
        {
            return false;
        }
    }

    next(lexer.cursor, prefix.size());
    return true;
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

Token Lexer::peek_token() const
{
    auto temp  = *this;
    auto token = temp.next_token();

    return token;
}

Token Lexer::next_token()
{
    // TODO: Comments
    // TODO: Floating point literals

    for (; is_white(*this->cursor.at); next(this->cursor))
    {
    }

    auto start = this->cursor;

    if (consume_sequence(*this, "//"))
    {
        for (; *this->cursor.at && *this->cursor.at != '\n'; next(this->cursor))
        {
        }

        return emit(*this, start, Tt::single_line_comment);
    }

    if (consume_sequence(*this, "/*"))
    {
        auto depth = 1;
        while (depth > 0)
        {
            if (consume_sequence(*this, "/*"))
            {
                ++depth;
                continue;
            }

            if (consume_sequence(*this, "*/"))
            {
                --depth;
                continue;
            }

            if (*this->cursor.at == '\0')
            {
                break;
            }

            next(this->cursor);
        }

        return emit(*this, start, Tt::multi_line_comment);
    }

    // clang-format off
    auto simple_tokens = {
        // std::make_tuple(":=", Tt::declaration_assignment),
        std::make_tuple("<<", Tt::left_shift),
        std::make_tuple(">>", Tt::right_shift),
        std::make_tuple("&&", Tt::logical_and),
        std::make_tuple("||", Tt::logical_or),
        std::make_tuple("==", Tt::equal),
        std::make_tuple("!=", Tt::inequal),
        std::make_tuple(">=", Tt::greater_than_or_equal),
        std::make_tuple("<=", Tt::less_than_or_equal),

        std::make_tuple("*", Tt::asterisk),
        std::make_tuple("/", Tt::slash),
        std::make_tuple("%", Tt::mod),
        std::make_tuple("+", Tt::plus),
        std::make_tuple("-", Tt::minus),
        std::make_tuple("=", Tt::assign),
        std::make_tuple("&", Tt::bit_and),
        std::make_tuple("^", Tt::bit_xor),
        std::make_tuple("|", Tt::bit_or),
        std::make_tuple(",", Tt::comma),
        std::make_tuple("(", Tt::parenthesis_open),
        std::make_tuple(")", Tt::parenthesis_close),
        std::make_tuple("{", Tt::brace_open),
        std::make_tuple("}", Tt::brace_close),
        std::make_tuple("[", Tt::bracket_open),
        std::make_tuple("]", Tt::bracket_close),
        std::make_tuple(">", Tt::greater_than),
        std::make_tuple("<", Tt::less_than),
        std::make_tuple(":", Tt::colon),
    };
    // clang-format on

    for (auto [text, type] : simple_tokens)
    {
        if (consume_sequence(*this, text))
        {
            return emit(*this, start, type);
        }
    }

    for (; is_ident(*this->cursor.at, this->cursor.at - start.at); next(this->cursor))
    {
    }
    if (this->cursor.at > start.at)
    {
        std::string_view text{start.at, this->cursor.at};
        auto is_keyword = text == "return" || text == "while" || text == "proc" || text == "else" || text == "if";
        if (is_keyword)
        {
            return emit(*this, start, Tt::keyword);
        }

        return emit(*this, start, Tt::identifier);
    }

    if (is_digit(*this->cursor.at))
    {
        next(this->cursor);
        if (*this->cursor.at == 'x')
        {
            next(this->cursor);
        }

        for (; is_hex_digit(*this->cursor.at); next(this->cursor))
        {
        }
    }
    if (this->cursor.at > start.at)
    {
        if (*this->cursor.at == 'u')
        {
            next(this->cursor);
        }

        return emit(*this, start, Tt::numerical_literal);
    }

    if (*this->cursor.at == '\0')
    {
        return emit(*this, start, Tt::eof);
    }

    std::cout << std::format("Lexer error at {}:{}: unable to parse token", this->cursor.line, this->cursor.line_offset)
              << std::endl;
    next(this->cursor);

    return emit(*this, start, Tt::eof);
}
