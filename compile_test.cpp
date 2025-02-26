#include "catch2/catch_test_macros.hpp"
#include "compile.h"
#include "parse.h"
#include "vm.h"
#include <string_view>

BytecodeWriter make_mock_writer()
{
    return BytecodeWriter{.generate_asm = true, .also_generate_bytecode = true};
}

void test_codegen(std::string_view source, BytecodeWriter *mock)
{
    AstProgram program;
    auto success = parse_program(source, &program);
    REQUIRE(success);

    BytecodeWriter w{.generate_asm = true, .also_generate_bytecode = true};
    success = generate_code(&program, &w);
    REQUIRE(success);

    printf("Source code:\n%s\n", source.data());
    printf("Compilation result:\n");
    printf("-> Got ASM:\n%s\n", w.asm_source.data());
    printf("Expected ASM:\n%s\n", mock->asm_source.data());

    REQUIRE(w.bytecode.size() == mock->bytecode.size());

    for (auto i = 0; i < w.bytecode.size(); ++i)
    {
        if (w.bytecode[i] != mock->bytecode[i])
        {
            /* printf("The program does not match at bytecode index %d\n", (int)i); */
        }

        REQUIRE(w.bytecode[i] == mock->bytecode[i]);

        return;
    }

    // TODO
    run_program(w.bytecode);

    printf("Test succeeded!\n");
}

TEST_CASE("basic locals")
{
    auto source = std::string_view{
        R"(
main := proc() {
    a := 1
    b := 2
    c := a + b * 2
    return c
}
)"};

    auto mock = make_mock_writer();

    auto a = -24;
    auto b = -16;
    auto c = -8;

    write_op_64(ADDRSP, 24, &mock);
    write_op_64(PUSHC, 1, &mock);
    write_op_64(STORER, a, &mock);
    write_op_64(PUSHC, 2, &mock);
    write_op_64(STORER, b, &mock);
    write_op_64(LOADR, a, &mock);
    write_op_64(LOADR, b, &mock);
    write_op_64(PUSHC, 2, &mock);
    write_op(MUL, &mock);
    write_op(ADD, &mock);
    write_op_64(STORER, c, &mock);
    write_op_64(LOADR, c, &mock);
    write_op_64(ADDRSP, -24, &mock);
    write_op(RET, &mock);

    test_codegen(source, &mock);
}

TEST_CASE("Implicit return")
{
    auto source = std::string_view{
        R"(
main := proc() {
    a := 1
}
)"};

    auto mock = make_mock_writer();

    write_op_64(ADDRSP, 8, &mock);
    write_op_64(PUSHC, 1, &mock);
    write_op_64(STORER, -8, &mock);
    write_op(RET, &mock);

    test_codegen(source, &mock);
}

TEST_CASE("Nested blocks 1")
{
    auto source = std::string_view{
        R"(
main := proc() {
    a := 1

    {
        b := 2
        c := 3
    }

    d := 4

    {
        e := 5
    }
}
)"};

    auto a = -32;
    auto b = -16;
    auto c = -8;
    auto d = -24;
    auto e = -16;

    auto mock = make_mock_writer();

    write_op_64(ADDRSP, 32, &mock);
    write_op_64(PUSHC, 1, &mock);
    write_op_64(STORER, a, &mock);
    write_op_64(PUSHC, 2, &mock);
    write_op_64(STORER, b, &mock);
    write_op_64(PUSHC, 3, &mock);
    write_op_64(STORER, c, &mock);
    write_op_64(PUSHC, 4, &mock);
    write_op_64(STORER, d, &mock);
    write_op_64(PUSHC, 5, &mock);
    write_op_64(STORER, d, &mock);
    write_op(RET, &mock);

    test_codegen(source, &mock);
}

TEST_CASE("Nested blocks 2")
{
    auto source = std::string_view{
        R"(
main := proc() {
    a := 1
    {
        b := 2
        {
            c := 3
            {
                d := 4
            }
        }
    }
}
)"};

    // TODO: For allocation tests, don't make this an integration test - create a helper function that takes an array of
    // identifier-address pairs and checks if they match based on the resulting AST

    auto total = 32;
    auto a     = 0 - total;
    auto b     = 8 - total;
    auto c     = 16 - total;
    auto d     = 24 - total;

    auto mock = make_mock_writer();

    write_op_64(ADDRSP, total, &mock);
    write_op_64(PUSHC, 1, &mock);
    write_op_64(STORER, a, &mock);
    write_op_64(PUSHC, 2, &mock);
    write_op_64(STORER, b, &mock);
    write_op_64(PUSHC, 3, &mock);
    write_op_64(STORER, c, &mock);
    write_op_64(PUSHC, 4, &mock);
    write_op_64(STORER, d, &mock);
    write_op(RET, &mock);

    test_codegen(source, &mock);
}

TEST_CASE("Nested blocks 3")
{
    auto source = std::string_view{
        R"(
main := proc() {
    a := 1

    {
        {
            b := 2
        }

        c := 3
    }

    d := 4
}
)"};

    // TODO: For allocation tests, don't make this an integration test - create a helper function that takes an array of
    // identifier-address pairs and checks if they match based on the resulting AST

    auto total = 32;
    auto a     = 0 - total;
    auto b     = 8 - total;
    auto c     = 8 - total;
    auto d     = 8 - total;

    auto mock = make_mock_writer();

    write_op_64(ADDRSP, total, &mock);
    write_op_64(PUSHC, 1, &mock);
    write_op_64(STORER, a, &mock);
    write_op_64(PUSHC, 2, &mock);
    write_op_64(STORER, b, &mock);
    write_op_64(PUSHC, 3, &mock);
    write_op_64(STORER, c, &mock);
    write_op_64(PUSHC, 4, &mock);
    write_op_64(STORER, d, &mock);
    write_op(RET, &mock);

    test_codegen(source, &mock);
}
