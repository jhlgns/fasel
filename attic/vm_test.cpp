#if 0
#include "catch2/catch_test_macros.hpp"
#include "compile.h"
#include "vm.h"
#include <span>

// TODO: Test CALL and the other instructions

TEST_CASE("PUSHC", "[vm]")
{
    BytecodeWriter w;
    write_op_64(PUSHC, 0, &w);
    write_op_64(PUSHC, 123, &w);
    write_op_64(PUSHC, 333, &w);
    write_op_64(PUSHC, 1337, &w);
    write_op_64(PUSHC, std::numeric_limits<int64_t>::min(), &w);
    write_op_64(PUSHC, std::numeric_limits<int64_t>::max(), &w);

    Vm vm;
    load_program(&vm, std::span{w.bytecode.begin(), w.bytecode.end()});
    run_program(&vm);

    REQUIRE(vm.rsp >= vm.stack_start());
    REQUIRE(vm.rsp < vm.stack_end());
    REQUIRE((vm.rsp - vm.stack_start()) % 8 == 0);
    // REQUIRE((vm.rsp - vm.stack_start()) / 8 == 3);

    REQUIRE(pop_64(&vm) == std::numeric_limits<int64_t>::max());
    REQUIRE(pop_64(&vm) == std::numeric_limits<int64_t>::min());
    REQUIRE(pop_64(&vm) == 1337);
    REQUIRE(pop_64(&vm) == 333);
    REQUIRE(pop_64(&vm) == 123);
    REQUIRE(pop_64(&vm) == 0);

    REQUIRE(vm.rsp == vm.stack_start());
}

struct Operator
{
    OpCode op;
    int64_t (*calculate)(int64_t, int64_t);
};

constexpr static auto all_operators = {
    Operator{.op = ADD, .calculate = [](int64_t a, int64_t b) { return a + b; }},
    Operator{.op = SUB, .calculate = [](int64_t a, int64_t b) { return a - b; }},
    Operator{.op = MUL, .calculate = [](int64_t a, int64_t b) { return a * b; }},
    Operator{.op = DIV, .calculate = [](int64_t a, int64_t b) { return a / b; }},
    Operator{.op = MOD, .calculate = [](int64_t a, int64_t b) { return a % b; }},
    Operator{.op = BITAND, .calculate = [](int64_t a, int64_t b) { return a & b; }},
    Operator{.op = BITOR, .calculate = [](int64_t a, int64_t b) { return a | b; }},
    Operator{.op = BITXOR, .calculate = [](int64_t a, int64_t b) { return a ^ b; }},
    Operator{.op = RSH, .calculate = [](int64_t a, int64_t b) { return a >> b; }},
    Operator{.op = LSH, .calculate = [](int64_t a, int64_t b) { return a << b; }},
    Operator{.op = CMPEQ, .calculate = [](int64_t a, int64_t b) { return static_cast<int64_t>(a == b); }},
    Operator{.op = CMPNE, .calculate = [](int64_t a, int64_t b) { return static_cast<int64_t>(a != b); }},
    Operator{.op = CMPGE, .calculate = [](int64_t a, int64_t b) { return static_cast<int64_t>(a >= b); }},
    Operator{.op = CMPGT, .calculate = [](int64_t a, int64_t b) { return static_cast<int64_t>(a > b); }},
    Operator{.op = CMPLE, .calculate = [](int64_t a, int64_t b) { return static_cast<int64_t>(a <= b); }},
    Operator{.op = CMPLT, .calculate = [](int64_t a, int64_t b) { return static_cast<int64_t>(a < b); }},
};

TEST_CASE("Integer math", "[vm]")
{
    auto values = std::vector<int64_t>{
        std::numeric_limits<int64_t>::min(),
        std::numeric_limits<int64_t>::min() + 1,
        -87923401,
        -8192,
        -10,
        -1,
        0,
        1,
        7,
        13,
        16,
        32,
        33,
        63,
        64,
        65,
        256,
        1000,
        708927389047,
        std::numeric_limits<int64_t>::max() - 1,
        std::numeric_limits<int64_t>::max(),
    };

    for (auto op : all_operators)
    {
        SECTION(std::format("Operator {}", to_string(op.op)))
        {
            for (auto a : values)
            {
                for (auto b : values)
                {
                    if ((op.op == DIV || op.op == MOD) &&
                        (a == std::numeric_limits<int64_t>::min() && b == -1 || b == 0))
                    {
                        continue;
                    }

                    BytecodeWriter w;
                    write_op_64(PUSHC, a, &w);
                    write_op_64(PUSHC, b, &w);
                    write_op(op.op, &w);

                    Vm vm;
                    load_program(&vm, std::span{w.bytecode.begin(), w.bytecode.end()});
                    run_program(&vm);

                    auto received = pop_64(&vm);
                    auto expected = op.calculate(a, b);
                    REQUIRE(received == expected);

                    REQUIRE(vm.rsp == vm.stack_start());
                }
            }
        }
    }
}

TEST_CASE("JMP, JMP0, JMP1", "[vm]")
{
    for (auto op : {JMP0, JMP1})
    {
        for (auto condition : {-1, 0, 1, 2})
        {
            BytecodeWriter w;

            write_op_64(PUSHC, condition, &w);
            auto jmpx_pos = w.pos;
            write_op_64(op, -1, &w);
            write_op_64(PUSHC, 111, &w);
            auto jmp_end_pos = w.pos;
            write_op_64(JMP, -1, &w);
            auto target_label = w.pos;
            write_op_64(PUSHC, 222, &w);
            auto end_label = w.pos;

            w.pos = jmpx_pos;
            write_op_64(op, target_label, &w);
            w.pos = jmp_end_pos;
            write_op_64(JMP, end_label, &w);

            Vm vm;
            load_program(&vm, std::span{w.bytecode.begin(), w.bytecode.end()});
            run_program(&vm);

            auto received = pop_64(&vm);
            int64_t expected;
            if (op == JMP0)
            {
                expected = condition == 0 ? 222 : 111;
            }
            else
            {
                expected = condition != 0 ? 222 : 111;
            }

            REQUIRE(expected == received);

            REQUIRE(vm.rsp == vm.stack_start());
        }
    }
}

TEST_CASE("CALL", "[vm]")
{
    BytecodeWriter w;

    auto f = w.pos;
    write_op_64(PUSHC, 3, &w);
    write_op_64(PUSHC, 3, &w);
    write_op(MUL, &w);
    write_op(RET, &w);

    auto main = w.pos;
    write_op_64(PUSHC, f, &w);
    write_op(CALL, &w);
    write_op(RET, &w);

    Vm vm;
    load_program(&vm, std::span<uint8_t>{w.bytecode.begin(), w.bytecode.end()});
    start_proc_call(&vm, main);
    run_program(&vm);

    REQUIRE(pop_64(&vm) == 9);
    REQUIRE(vm.rsp == vm.stack_start());
}
#endif
