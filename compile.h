#pragma once

#include <cassert>
#include <cstdint>
#include <vector>


/* struct SymbolAddressPlaceholder */
/* { */
/*     std::string symbol_name; */
/*     size_t bytecode_offset; */
/* }; */

struct BytecodeWriter
{
    std::vector<uint8_t> bytecode;
    struct AstBlock *current_block;
    /* std::vector<SymbolAddressPlaceholder> saps; */

    bool generate_asm;
    std::string asm_source;
};

enum OpCode : uint8_t
{
    // Memory operations
    PUSHC,  // value = read i64; push i64 value;
    LOADR,  // offset = read i64; push i64 *(RSP + offset);
    LOAD,   // addr = read i64; push i64 *addr;
    /* STORE,  // addr = read i64; value = pop i64; *addr = value; */
    STORER,  // offset = read i64; value = pop i64; *(RSP + offset) = value;

    // Integer arithmetic operations
    ADD,  // a = pop i64; b = pop i64; push i64 a + b
    SUB,  // a = pop i64; b = pop i64; push i64 a - b
    MUL,  // a = pop i64; b = pop i64; push i64 a * b
    DIV,  // a = pop i64; b = pop i64; push i64 a / b
    MOD,  // a = pop i64; b = pop i64; push i64 a % b

    // Jumping and branching
    CMP,   // a = pop i64; b = pop i64; ...compare a and b and set status registers
    JMP,   // dst = read i64; jmp dst;
    JEQ,   // dst = read i64; if previous CMP yielded a == b: jmp dst;
    JNE,   // dst = read i64; if previous CMP yielded a != b: jmp dst;
    JLT,   // dst = read i64; if previous CMP yielded a < b: jmp dst;
    JLE,   // dst = read i64; if previous CMP yielded a <= b: jmp dst;
    JGT,   // dst = read i64; if previous CMP yielded a > b: jmp dst;
    JGE,   // dst = read i64; if previous CMP yielded a >= b: jmp dst;
    CALL,  // push i64 rip; dst = read i64; jmp dst;
    RET,   // value = pop i64; ret = pop i64; push i64 value; jmp ret;
};

inline const char *to_string(OpCode op)
{
    switch (op)
    {
        case PUSHC:  return "PUSHC";
        case LOADR:  return "LOADR";
        case LOAD:   return "LOAD";
        case STORER: return "STORER";

        case ADD: return "ADD";
        case SUB: return "SUB";
        case MUL: return "MUL";
        case DIV: return "DIV";
        case MOD: return "MOD";

        case CMP:  return "CMP";
        case JMP:  return "JMP";
        case JEQ:  return "JEQ";
        case JNE:  return "JNE";
        case JLT:  return "JLT";
        case JLE:  return "JLE";
        case JGT:  return "JGT";
        case JGE:  return "JGE";
        case CALL: return "CALL";
        case RET:  return "RET";
    }

    assert(false);
};

void write_op(OpCode op, BytecodeWriter *w);
void write_op_8(OpCode op, uint8_t value, BytecodeWriter *w);
void write_op_64(OpCode op, int64_t value, BytecodeWriter *w);

[[nodiscard]] bool generate_code(struct AstNode *node, BytecodeWriter *w);

