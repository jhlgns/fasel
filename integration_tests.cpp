#include "compile.h"
#include "parse.h"
#include "vm.h"
#include <catch2/catch_test_macros.hpp>

void test_expression(std::string_view expression, int64_t expected_result)
{
    auto source = std::format("main := proc() {{ return {} }}", expression);

    AstProgram program;
    REQUIRE(parse_program(source, &program));

    BytecodeWriter w{.generate_asm = true, .also_generate_bytecode = true};
    REQUIRE(generate_code(&program, &w));

    Vm vm;
    run_main(&vm, &program, std::span{w.bytecode.begin(), w.bytecode.end()});

    auto result = pop_64(&vm);

    REQUIRE(result == expected_result);

    REQUIRE(vm.rsp == vm.stack_start());
}

TEST_CASE("Expression evaluation", "[integration]")
{
#define CASE(expr) test_expression(#expr, expr);
    CASE(1);
    CASE(1 + 2);
    CASE(1 + 2 * 3);
    CASE(1 * 2 + 3);
    CASE(1 * 2 + 3 / 4);
    CASE(1 / 2 - 3 * 4);
    CASE(1 * 2 * 3 % 4);
    CASE(0 + 0 - 0 + 0);
    CASE(0 + 0 * 0 + 0);
    CASE(100 * 23 / 34 + 56 * 32480);
    CASE(8 % 8 - 7348 % 23);

    CASE(0xff & 0x70);
    CASE(0xff & 0x00);
    CASE(0xff & 0x07);
    CASE(0xff | 0x70);
    CASE(0xff | 0x00);
    CASE(0xff | 0x07);
    CASE(0xff ^ 0x70);
    CASE(0xff ^ 0x00);
    CASE(0xff ^ 0x07);
    CASE(1 << 2 | 3 >> 4);
    CASE(1 | 2 | 3 | 16);
    CASE(1 & 2 | 3 & 16);
    CASE(1 | 2 & 3 & 16);

    CASE(100 == 1);
    CASE(1 == 1);
    CASE(1 == 100);
    CASE(1 > 1);
    CASE(1 > 100);
    CASE(100 < 1);
    CASE(1 < 1);
    CASE(1 < 100);
    CASE(100 <= 1);
    CASE(1 <= 1);
    CASE(1 <= 100);
    CASE(100 >= 1);
    CASE(1 >= 1);
    CASE(1 >= 100);

    CASE(1576 & 2 * 485 / 871 + 5923 % 7 | 16 & 3);
#undef CASE
}
