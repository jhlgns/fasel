#include "disasm.h"
#include "op_code.h"
#include <format>

struct Instruction
{
    int64_t address;
    OpCode op;
    int64_t argument;
    bool has_argument;
};

std::string disassemble(std::span<uint8_t> program, int64_t mark_address)
{
    int64_t rip = 0;

    // std::vector<int64_t> jump_targets;
    std::vector<Instruction> instructions;

    while (rip < program.size())
    {
        Instruction insn{.address = rip};

        assert(rip + sizeof(OpCode) <= program.size());
        memcpy(&insn.op, &program[rip], sizeof(OpCode));
        rip += sizeof(OpCode);

        insn.has_argument = insn.op == PUSHC || insn.op == LOADR || insn.op == STORER || insn.op == ADDRSP ||
                            insn.op == JMP0 || insn.op == JMP1 || insn.op == JMP;
        // auto is_jump = insn.op == JMP0 || insn.op == JMP1 || insn.op == JMP;
        // assert(is_jump == false || insn.has_argument);

        if (insn.has_argument)
        {
            assert(rip + sizeof(int64_t) <= program.size());
            memcpy(&insn.argument, &program[rip], sizeof(int64_t));
            rip += sizeof(int64_t);

            // if (is_jump)
            // {
            //     jump_targets.push_back(insn.argument);
            // }
        }

        instructions.push_back(insn);
    }

    assert(rip == program.size());

    std::string result;

    for (auto insn : instructions)
    {
        // for (auto jump_target : jump_targets)
        {
            // if (jump_target == insn.address)
            {
                result += std::format("{:>7}: ", insn.address);
            }
        }

        result += to_string(insn.op);

        if (insn.has_argument)
        {
            result += std::format(" {}", insn.argument);
        }

        if (mark_address == insn.address)
        {
            result += " <<<";
        }

        result += "\n";
    }

    return result;
}
