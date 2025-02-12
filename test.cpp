#include "compile.h"
#include "parse.h"
#include <string_view>

void test()
{
    auto source = std::string_view{
        R"(
main := proc() {
    a := 1
    b := 2
    c := a + b
    return c
}
)"};

    BytecodeWriter mock;

    write_op_64(PUSHC, 1, &mock);  // Push a's initial value onto the stack
    write_op_64(STORER, -24, &mock);  // Store a at RSP - 8
    write_op_64(PUSHC, 2, &mock);  // Push b's initial value onto the stack
    write_op_64(STORER, -16, &mock);  // Store b at RSP - 16
    write_op_64(LOADR, -24, &mock);  // Push b
    write_op_64(LOADR, -16, &mock);  // Push a
    write_op(ADD, &mock);  // Pop a, pop b, push a + b
    write_op_64(STORER, -8, &mock);  // Store the top of the stack into c
    write_op_64(LOADR, -8, &mock);  // Store the top of the stack into c
    write_op(RET, &mock);  // Return

    AstProgram program;
    if (parse_program(source, &program) == false)
    {
        assert(false);
    }

    BytecodeWriter w{.generate_asm = false};
    if (generate_code(&program, &w) == false)
    {
        /* printf("ASM:\n%s\n", w.asm_source.data()); */
        assert(false);
    }

    /* printf("ASM:\n%s\n", w.asm_source.data()); */

    if (w.bytecode.size() != mock.bytecode.size())
    {
        assert(false);
    }

    for (auto i = 0; i < w.bytecode.size(); ++i)
    {
        if (w.bytecode[i] != mock.bytecode[i])
        {
            printf("The program does not match at bytecode index %d\n", (int)i);
            assert(false);
            return;
        }
    }

    printf("Test succeeded!\n");
}
