#include "lex.h"
#include <catch2/catch_test_macros.hpp>

void require_sequence(std::string_view source, std::vector<std::function<bool(Token)>> assertions)
{
    // TODO
}

TEST_CASE("Integer literals", "[lex]")
{
    // TODO: Is there even a point in writing lexer tests? I guess all that could go wrong while lexing would lead to
    // failures in the parser and the compiler tests...
    // -> It could for example make sense to test the emission of comment tokens

    require_sequence(
        "1",
        {
            [](Token token) { return token.type == Tt::numerical_literal && token.text() == "1"; },
        });
}
