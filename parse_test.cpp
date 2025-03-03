#include "parse.h"
#include "stringify.h"
#include <catch2/catch_test_macros.hpp>
#include <iostream>

void test_integer_literal(std::string_view literal_text, int64_t expected_value)
{
    AstProgram program;

    auto source = std::format("main := proc() {{ a := {} }}", literal_text);
    REQUIRE(parse_program(source, &program));

    auto main_decl = program.block.find_decl("main");
    REQUIRE(main_decl != nullptr);

    auto main = ast_cast<AstProc>(main_decl->init_expr);
    REQUIRE(main != nullptr);

    auto a_decl = ast_cast<AstDecl>(main->body.statements.front());
    REQUIRE(a_decl != nullptr);

    auto literal = ast_cast<AstLiteral>(a_decl->init_expr);
    REQUIRE(literal != nullptr);

    REQUIRE(literal->int_value == expected_value);
}

TEST_CASE("Integer literals", "[parsing]")
{
#define CASE(literal) test_integer_literal(#literal, literal);
    CASE(0);
    CASE(1);
    CASE(7);
    CASE(788);
    CASE(789237489234);
    CASE(9223372036854775807);  // int64_t max value
    // CASE(-9223372036854775807);  // int64_t min value

    CASE(0x0);
    CASE(0x00003);
    CASE(0xaaaaaaaaaa9900);
    CASE(0x0000000000000000);
    CASE(0x0000000000000001);
    CASE(0x7fffffffffffffff);
    CASE(0x78910fabc78eed81);
    CASE(0xAFFED00F);
    CASE(0xDec0dedFece5);
#undef CASE
}

// TODO: More individual tests
TEST_CASE("Program", "[parsing]")
{
    std::string_view source{R"(
f := proc(a int, b int) {
    if a == 1 {
        return a * b + 2 >> 2
    } else {
        return a / 400 * f(b | 2, a &0xff)
    }
}

main := proc() {
    x := f(3, 4)
}

)"};

    AstProgram program;
    REQUIRE(parse_program(source, &program));

    // TODO
    std::cout << dump_node(0, &program) << std::endl;
}

