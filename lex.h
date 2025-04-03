#pragma once

#include "basics.h"

#include <cassert>
#include <cstddef>
#include <format>
#include <string>
#include <string_view>

enum class TokenType
{
    none,
    asterisk,  // *
    slash,  // /
    mod,  // %
    plus,  // +
    minus,  // -
    assign,  // =
    bit_and,  // &
    bit_or,  // |
    bit_xor,  // ^
    left_shift,  // <<
    right_shift,  // >>
    equal,  // ==
    inequal,  // !=
    greater_than_or_equal,  // >=
    greater_than,  // >
    less_than_or_equal,  // <=
    less_than,  // <

    logical_and,  // &&
    logical_or,  // ||

    colon,  // :
    comma,  // ,
    triple_dot,  // ...

    parenthesis_open,  // (
    parenthesis_close,  // )
    brace_open,  // {
    brace_close,  // }
    bracket_open,  // [
    bracket_close,  // ]

    identifier,
    keyword,
    number_literal,
    string_literal,

    single_line_comment,  // ...
    multi_line_comment,  // /* ... */

    eof,
};

using Tt = TokenType;

inline std::string_view to_string(TokenType type)
{
    switch (type)
    {
        case Tt::asterisk:              return "asterisk";
        case Tt::slash:                 return "slash";
        case Tt::mod:                   return "mod";
        case Tt::plus:                  return "plus";
        case Tt::minus:                 return "minus";
        case Tt::assign:                return "assign";
        case Tt::bit_and:               return "bit_and";
        case Tt::bit_xor:               return "bit_xor";
        case Tt::bit_or:                return "bit_or";
        case Tt::comma:                 return "comma";
        case Tt::triple_dot:            return "triple_dot";
        case Tt::parenthesis_open:      return "parenthesis_open";
        case Tt::parenthesis_close:     return "parenthesis_close";
        case Tt::brace_open:            return "brace_open";
        case Tt::brace_close:           return "brace_close";
        case Tt::bracket_open:          return "bracket_open";
        case Tt::bracket_close:         return "bracket_close";
        case Tt::colon:                 return "colon";
        case Tt::left_shift:            return "left_shift";
        case Tt::right_shift:           return "right_shift";
        case Tt::logical_and:           return "logical_and";
        case Tt::logical_or:            return "logical_or";
        case Tt::equal:                 return "equal";
        case Tt::inequal:               return "inequal";
        case Tt::greater_than_or_equal: return "greater_than_or_equal";
        case Tt::greater_than:          return "greater_than";
        case Tt::less_than_or_equal:    return "less_than_or_equal";
        case Tt::less_than:             return "less_than";
        case Tt::identifier:            return "identifier";
        case Tt::keyword:               return "keyword";
        case Tt::number_literal:        return "numerical_literal";
        case Tt::string_literal:        return "string_literal";
        case Tt::single_line_comment:   return "single_line_comment";
        case Tt::multi_line_comment:    return "multi_line_comment";
        case Tt::eof:                   return "eof";
        case Tt::none:                  return "(none)";
    }

    UNREACHED;
}

// The higher the precedence, the stronger the operator binds its arguments.
// '*' has higher precedence than '+'.
inline int binary_operator_precedence(TokenType binop)
{
    switch (binop)
    {
        case Tt::asterisk:              return 110;
        case Tt::slash:                 return 110;
        case Tt::mod:                   return 110;

        case Tt::plus:                  return 100;
        case Tt::minus:                 return 100;

        case Tt::left_shift:            return 90;
        case Tt::right_shift:           return 90;

        case Tt::less_than:             return 80;
        case Tt::greater_than:          return 80;
        case Tt::less_than_or_equal:    return 80;
        case Tt::greater_than_or_equal: return 80;

        case Tt::equal:                 return 70;
        case Tt::inequal:               return 70;

        case Tt::bit_and:               return 50;

        case Tt::bit_xor:               return 40;

        case Tt::bit_or:                return 30;

        case Tt::logical_and:           return 20;

        case Tt::logical_or:            return 10;

        case Tt::assign:                return 5;

        default:                        return 0;
    }
}

struct Cursor
{
    const char *at{};
    int line{};
    int line_offset{};

    auto operator<=>(const Cursor &) const = default;
};

struct Token
{
    TokenType type{};
    Cursor pos{};
    size_t length{};

    inline std::string_view text() const { return std::string_view{this->pos.at, this->length}; }

    inline std::string to_string() const
    {
        switch (this->type)
        {
            case TokenType::identifier:     return std::format("Identifier '{}'", this->text());
            case TokenType::keyword:        return std::format("Keyword '{}'", this->text());
            case TokenType::number_literal: return std::format("Number literal '{}'", this->text());
            case TokenType::string_literal:
                return std::format("String literal '{}'", this->text());  // TODO: Shorten string literals
            default: return std::string{::to_string(this->type)};
        }
    }

    auto operator<=>(const Token &) const = default;
};

struct Lexer
{
    explicit Lexer(std::string_view source)
        : source{source}
        , cursor{.at = source.data()}
    {
    }

    std::string_view source{};
    Cursor cursor{};

    Token peek_token() const;
    Token next_token();
};
