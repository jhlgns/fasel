#include <catch2/catch_test_macros.hpp>

#include "basics.h"

// #include "compile_ir.h"
// #include "jit.h"
// #include "parse.h"
// #include "typecheck.h"

TEST_CASE("TODO: Name", "[integration]")
{
    auto source = R"(
main := proc() {
}
)"sv;

    // AstProgram program;
    // REQUIRE(parse_program(source, program));

    // auto program_node = make_node(nullptr, &program);

    // TypeChecker type_checker{.print_errors = true};
    // REQUIRE(type_checker.typecheck(program_node));

    // auto compilation_result = compile_to_ir(program_node);

    // Jit jit{};
    // jit.add_module(std::move(compilation_result.context), std::move(compilation_result.module));
    // auto main_address = jit.get_symbol_address("main");
    // auto main         = reinterpret_cast<int64_t (*)()>(main_address);

    // auto result = main();
}

#if 0
#    include "compile.h"
#    include "parse.h"
#    include "vm.h"
#    include <catch2/catch_test_macros.hpp>

void test_expression(std::string_view expression, int64_t expected_result)
{
    auto source = std::format("main := proc() {{ return {} }}", expression);

    AstProgram program;
    REQUIRE(parse_program(source, program));

    BytecodeWriter w;
    REQUIRE(generate_code(&program, &w));

    Vm vm;
    run_main(&vm, &program, std::span{w.bytecode.begin(), w.bytecode.end()});

    auto result = pop_64(&vm);

    REQUIRE(result == expected_result);

    REQUIRE(vm.rsp == vm.stack_start());
}

TEST_CASE("Expression evaluation", "[integration]")
{
#    define CASE(expr) test_expression(#expr, expr);
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
    CASE(100 << 2 | 300 >> 4);
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
#    undef CASE
}

int64_t fib(int64_t n)
{
    if (n == 0)
    {
        return 0;
    }

    if (n == 1)
    {
        return 1;
    }

    return fib(n - 1) + fib(n - 2);
}

TEST_CASE("Fibonacci", "[integration]")
{
    auto source = std::string_view{R"(
fib := proc(n i64) {
    if n == 0 { return 0 }
    if n == 1 { return 1 }

    return fib(n - 1) + fib(n - 2)
}

main := proc() {
    return fib(3)
}
)"};

    // TODO: There seems to be something very weird going on.
    // n is not where it is expected to be because something is
    // happening with the stack that I don't understand yet.
    // I think this is a fundamental flaw of how the locals are allocated
    // and accessed that somehow is unearthed only now. We might need an RBP
    // or do something smarter with addressing relative to RSP.
    // TODO: Add a simple test to verify this
    AstProgram program;
    REQUIRE(parse_program(source, program));

    BytecodeWriter w;
    REQUIRE(generate_code(&program, &w));
    // auto test = w.disassemble();

    Vm vm;
    load_program(&vm, &w);
    start_proc_call(&vm, &program, "main");
    run_program(&vm);

    auto result   = pop_64(&vm);
    auto expected = fib(20);

    REQUIRE(result == expected);
}
#endif
