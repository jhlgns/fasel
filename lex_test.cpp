#include "lex.h"
#include <catch2/catch_test_macros.hpp>

void require_sequence(std::string_view source, std::vector<std::function<bool(Token)>> assertions)
{
    // TODO
}

TEST_CASE("Integer literals", "[lexing]")
{
    require_sequence(
        "1",
        {
            [](Token token) { return token.type == TOK_NUM_LIT; },
        });
}
