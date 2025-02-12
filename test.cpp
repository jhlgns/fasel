#include "compile.h"
#include "parse.h"
#include "vm.h"
#include <string_view>

void test_codegen(std::string_view source, BytecodeWriter *mock)
{
    AstProgram program;
    if (parse_program(source, &program) == false)
    {
        assert(false);
    }

    BytecodeWriter w{.generate_asm = true, .also_generate_bytecode = true};
    auto compile_success = generate_code(&program, &w);
    printf("Compilation result:\nSource code:\n%s\n-> ASM:\n%s\n", source.data(), w.asm_source.data());

    if (compile_success == false)
    {
        assert(false);
    }

    if (w.bytecode.size() != mock->bytecode.size())
    {
        assert(false);
    }

    for (auto i = 0; i < w.bytecode.size(); ++i)
    {
        if (w.bytecode[i] != mock->bytecode[i])
        {
            printf("The program does not match at bytecode index %d\n", (int)i);
            assert(false);
            return;
        }
    }

    // TODO
    run_program(w.bytecode);

    printf("Test succeeded!\n");
}

void basic_locals()
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

    BytecodeWriter mock;

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
    write_op(RET, &mock);

    test_codegen(source, &mock);
}

void test()
{
    basic_locals();
}
