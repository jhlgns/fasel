#include "catch2/catch_test_macros.hpp"
#include "compile.h"
#include "parse.h"
#include <string_view>

void test_codegen(std::string_view source, BytecodeWriter *mock)
{
    AstProgram program;
    REQUIRE(parse_program(source, &program));

    BytecodeWriter w;
    REQUIRE(generate_code(&program, &w));

    // printf("Source code:\n%s\n", source.data());
    // printf("Compiled to ASM:\n%s\n", w.asm_source.data());
    // printf("Expected ASM:\n%s\n", mock->asm_source.data());

    REQUIRE(w.bytecode.size() == mock->bytecode.size());

    for (auto i = 0; i < w.bytecode.size(); ++i)
    {
        REQUIRE(static_cast<int>(w.bytecode[i]) == static_cast<int>(mock->bytecode[i]));
    }
}

void get_all_allocations(AstBlock *block, std::unordered_map<std::string_view, AstDeclaration *> *all_decls)
{
    for (auto statement : block->statements)
    {
        if (auto decl = ast_cast<AstDeclaration>(statement))
        {
            auto [it, ok] = all_decls->emplace(decl->identifier.text(), decl);
            REQUIRE(ok);
            continue;
        }

        auto child_blocks = get_statement_child_blocks(statement);
        for (auto child_block : child_blocks)
        {
            get_all_allocations(child_block, all_decls);
        }
    }
}

void require_allocation(
    std::string_view source,
    std::unordered_map<std::string_view, int64_t> expected_variable_locations)
{
    AstProgram program;
    REQUIRE(parse_program(source, &program));

    REQUIRE(program.block.statements.size() == 1);
    auto proc_decl = ast_cast<AstDeclaration>(program.block.statements.front());
    REQUIRE(proc_decl != nullptr);
    auto proc = ast_cast<AstProcedure>(proc_decl->init_expression);
    REQUIRE(proc != nullptr);

    // NOTE: We need to invoke the codegen here because it allocates the variables
    BytecodeWriter w;
    REQUIRE(generate_code(proc, &w));

    std::unordered_map<std::string_view, AstDeclaration *> all_decls;
    get_all_allocations(&proc->body, &all_decls);

    REQUIRE(all_decls.size() == expected_variable_locations.size());

    for (auto [name, expected_location] : expected_variable_locations)
    {
        SECTION(std::format("The address of {} must be {}", name, expected_location))
        {
            auto it = all_decls.find(name);
            REQUIRE(it != all_decls.end());

            auto decl = it->second;
            REQUIRE(decl->address == expected_location);
        };
    }
}

TEST_CASE("Locals get allocated and assigned to their init expression", "[compile]")
{
    auto source = std::string_view{
        R"(
main := proc() {
    a := 1
    b := 2
    {
        c := 3
        {
            d := 4
            e := 5
        }
        f := 6
    }
    {
        g := 7
    }
    h := a + b * 2

    return b
}
)"};

    BytecodeWriter mock{};

    auto total = 56;
    auto a     = 0 - total;
    auto b     = 8 - total;
    auto c     = 24 - total;
    auto d     = 40 - total;
    auto e     = 48 - total;
    auto f     = 32 - total;
    auto g     = 24 - total;
    auto h     = 16 - total;

    write_op_64(ADDRSP, total, &mock);
    write_op_64(PUSHC, 1, &mock);
    write_op_64(STORER, a, &mock);
    write_op_64(PUSHC, 2, &mock);
    write_op_64(STORER, b, &mock);
    write_op_64(PUSHC, 3, &mock);
    write_op_64(STORER, c, &mock);
    write_op_64(PUSHC, 4, &mock);
    write_op_64(STORER, d, &mock);
    write_op_64(PUSHC, 5, &mock);
    write_op_64(STORER, e, &mock);
    write_op_64(PUSHC, 6, &mock);
    write_op_64(STORER, f, &mock);
    write_op_64(PUSHC, 7, &mock);
    write_op_64(STORER, g, &mock);
    write_op_64(LOADR, a, &mock);
    write_op_64(LOADR, b, &mock);
    write_op_64(PUSHC, 2, &mock);
    write_op(MUL, &mock);
    write_op(ADD, &mock);
    write_op_64(STORER, h, &mock);
    write_op_64(LOADR, b, &mock);
    write_op_64(ADDRSP, -total, &mock);
    write_op(RET, &mock);

    test_codegen(source, &mock);
}

TEST_CASE("Local variable initialization with nested blocks", "[compile]")
{
    auto source = std::string_view{
        R"(
main := proc() {
    a := 1
    b := 2
    {
        c := a + b * 2
    }
    return b
}
)"};

    BytecodeWriter mock{};

    auto total = 24;
    auto a     = 0 - total;
    auto b     = 8 - total;
    auto c     = 16 - total;

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
    write_op_64(LOADR, b, &mock);
    write_op_64(ADDRSP, -24, &mock);
    write_op(RET, &mock);

    test_codegen(source, &mock);
}

TEST_CASE("An implicit return statement is generated if it missing in the source code", "[compile]")
{
    auto source = std::string_view{
        R"(
main := proc() {
    a := 1
}
)"};

    BytecodeWriter mock{};

    write_op_64(ADDRSP, 8, &mock);
    write_op_64(PUSHC, 1, &mock);
    write_op_64(STORER, -8, &mock);
    write_op(RET, &mock);

    test_codegen(source, &mock);
}

TEST_CASE("Local variable allocation 1", "[compile]")
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
    {
        f := 6
    }
}
)"};

    require_allocation(
        source,
        {
            {"a", 0},
            {"b", 16},
            {"c", 24},
            {"d", 8},
            {"e", 16},
            {"f", 16},
        });
}

TEST_CASE("Local variable allocation 2", "[compile]")
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

    require_allocation(
        source,
        {
            {"a", 0},
            {"b", 8},
            {"c", 16},
            {"d", 24},
        });
}

TEST_CASE("Local variable allocation 3", "[compile]")
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

    require_allocation(
        source,
        {
            {"a", 0},
            {"b", 24},
            {"c", 16},
            {"d", 8},
        });
}

TEST_CASE("If 1", "[compile]")
{
    auto source = std::string_view{
        R"(
main := proc() {
    a := 1

    if a == 1 {
        b := 2
    }
}
)"};

    // TODO: For allocation tests, don't make this an integration test - create a helper function that takes an array of
    // identifier-address pairs and checks if they match based on the resulting AST

    auto total = 16;
    auto a     = 0 - total;
    auto b     = 8 - total;

    BytecodeWriter mock{};

    write_op_64(ADDRSP, total, &mock);
    write_op_64(PUSHC, 1, &mock);
    write_op_64(STORER, a, &mock);
    write_op_64(LOADR, a, &mock);
    write_op_64(PUSHC, 1, &mock);
    write_op(CMPEQ, &mock);
    auto jmp0_false_pos = mock.pos;
    write_op_64(JMP0, 333, &mock);
    write_op_64(PUSHC, 2, &mock);
    write_op_64(STORER, b, &mock);
    auto false_label = mock.pos;

    mock.pos = jmp0_false_pos;
    write_op_64(JMP0, false_label, &mock);

    mock.pos = false_label;
    write_op(RET, &mock);

    test_codegen(source, &mock);
}

TEST_CASE("Basic procedure calling", "[compile]")
{
    auto source = std::string_view{
        R"(
f := proc() {
    return 456
}

main := proc() {
    return f()
}
)"};

    BytecodeWriter mock{};

    write_op_64(ADDRSP, 0, &mock);
    write_op_64(PUSHC, 456, &mock);
    write_op(RET, &mock);

    write_op_64(ADDRSP, 0, &mock);
    write_op_64(PUSHC, 0, &mock);
    write_op(CALL, &mock);
    write_op(RET, &mock);

    test_codegen(source, &mock);
}

TEST_CASE("Procedure calling with arguments", "[compile]")
{
    auto source = std::string_view{
        R"(
f := proc(a i64, b i64, c i64) {
    d := 5
    return a + b * c - d
}

main := proc() {
    return f(1, 2, 3)
}
)"};

    auto total = 32;
    auto a     = 0 - total;
    auto b     = 8 - total;
    auto c     = 16 - total;
    auto d     = 24 - total;

    BytecodeWriter mock{};

    write_op_64(ADDRSP, 8, &mock);
    write_op_64(PUSHC, 5, &mock);
    write_op_64(STORER, d, &mock);
    write_op_64(LOADR, a, &mock);
    write_op_64(LOADR, b, &mock);
    write_op_64(LOADR, c, &mock);
    write_op(MUL, &mock);
    write_op(ADD, &mock);
    write_op_64(LOADR, d, &mock);
    write_op(SUB, &mock);
    write_op_64(ADDRSP, -total, &mock);
    write_op(RET, &mock);

    write_op_64(ADDRSP, 0, &mock);
    write_op_64(PUSHC, 1, &mock);
    write_op_64(PUSHC, 2, &mock);
    write_op_64(PUSHC, 3, &mock);
    write_op_64(PUSHC, 0, &mock);
    write_op(CALL, &mock);
    write_op(RET, &mock);

    test_codegen(source, &mock);
}
