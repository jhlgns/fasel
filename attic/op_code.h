#pragma once

#include "basics.h"
#include <cassert>
#include <cstdint>

/*
TODO:

x = 1 <= 2 && 3 != 4

With short circuit:
if (!(1 <= 2)) { JMP :false }
if (!(3 != 4)) { JMP :false }
PUSHC 1
JMP :done
:false
PUSHC 0
:done

->

PUSHC 2
PUSHC 1
CMPLE
JMP0 :false
PUSHC 4
PUSHC 3
CMPNE
JMP0 :false
PUSHC 1
JMP :done
:false
PUSHC 0
:done


x = 1 <= 2 || 3 != 4

With short circuit:
if (1 <= 2) { JMP :true }
if (3 != 4) { JMP :true }
PUSHC 0
JMP :done
:true
PUSHC 1
:done

->

PUSHC 2
PUSHC 1
CMPLE
JMP1 :true
PUSHC 4
PUSHC 3
CMPNE
JMP0 :true
PUSHC 0
JMP :done
:true
PUSHC 0
:done

*/

enum OpCode : uint8_t
{
    // Memory operations
    PUSHC,    // value = read i64; push i64 value;

    // Load relative from RSP (for stack local addressing without base pointer)
    LOADR1,    // offs = read i64; push i64 *(i8 *)(RSP + offs);
    LOADR2,    // offs = read i64; push i64 *(i16 *)(RSP + offs);
    LOADR4,    // offs = read i64; push i64 *(i32 *)(RSP + offs);
    LOADR8,    // offs = read i64; push i64 *(i64 *)(RSP + offs);

    // Load from hardcoded address
    LOAD1,    // addr = read i64; push i64 *(i8 *)addr;
    LOAD2,    // addr = read i64; push i64 *(i16 *)addr;
    LOAD4,    // addr = read i64; push i64 *(i32 *)addr;
    LOAD8,    // addr = read i64; push i64 *(i64 *)addr;

    // Store relative to RSP (for stack local addressing without base pointer)
    STORER1,   // offs = read i64; value = pop i64; *(i8 *)(RSP + offs) = value;
    STORER2,   // offs = read i64; value = pop i64; *(i16 *)(RSP + offs) = value;
    STORER4,   // offs = read i64; value = pop i64; *(i32 *)(RSP + offs) = value;
    STORER8,   // offs = read i64; value = pop i64; *(i64 *)(RSP + offs) = value;

    // Allocate stack space
    ADDRSP,   // offs = read i64; RSP += offs

    // Integer arithmetic operations
    ADD,  // a = pop i64; b = pop i64; push i64 a + b
    SUB,  // a = pop i64; b = pop i64; push i64 a - b
    MUL,  // a = pop i64; b = pop i64; push i64 a * b
    DIV,  // a = pop i64; b = pop i64; push i64 a / b
    MOD,  // a = pop i64; b = pop i64; push i64 a % b

    BITAND,  // a = pop i64; b = pop i64; push i64 a & b
    BITOR,   // a = pop i64; b = pop i64; push i64 a | b
    BITXOR,  // a = pop i64; b = pop i64; push i64 a ^ b

    LSH,  // a = pop i64; b = pop i64; push i64 a << b
    RSH,  // a = pop i64; b = pop i64; push i64 a >> b

    // (Conditional) jumping
    CMPEQ,  // dst = read i64; a = pop i64; b = pop i64; push a == b ? 1 : 0;
    CMPNE,  // dst = read i64; a = pop i64; b = pop i64; push a != b ? 1 : 0;
    CMPLT,  // dst = read i64; a = pop i64; b = pop i64; push a < b ? 1 : 0;
    CMPLE,  // dst = read i64; a = pop i64; b = pop i64; push a <= b ? 1 : 0;
    CMPGT,  // dst = read i64; a = pop i64; b = pop i64; push a > b ? 1 : 0;
    CMPGE,  // dst = read i64; a = pop i64; b = pop i64; push a >= b ? 1 : 0;
    JMP0,   // dst = read i64; a = pop i64; if a == 0 jmp dst;
    JMP1,   // dst = read i64; a = pop i64; if a != 0 jmp dst;
    JMP,    // dst = read i64; jmp dst;
    CALL,   // TODO: Make a version of this instruction with the target address baked in statically
    RET,    // value = pop i64; ret = pop i64; push i64 value; jmp ret;
};

inline const char *to_string(OpCode op)
{
    switch (op)
    {
        case OpCode::PUSHC:   return "PUSHC";
        case OpCode::LOADR1:  return "LOADR1";
        case OpCode::LOADR2:  return "LOADR2";
        case OpCode::LOADR4:  return "LOADR4";
        case OpCode::LOADR8:  return "LOADR8";
        case OpCode::LOAD1:   return "LOAD1";
        case OpCode::LOAD2:   return "LOAD2";
        case OpCode::LOAD4:   return "LOAD4";
        case OpCode::LOAD8:   return "LOAD8";
        case OpCode::STORER1: return "STORER1";
        case OpCode::STORER2: return "STORER2";
        case OpCode::STORER4: return "STORER4";
        case OpCode::STORER8: return "STORER8";
        case OpCode::ADDRSP:  return "ADDRSP";
        case OpCode::ADD:     return "ADD";
        case OpCode::SUB:     return "SUB";
        case OpCode::MUL:     return "MUL";
        case OpCode::DIV:     return "DIV";
        case OpCode::MOD:     return "MOD";
        case OpCode::BITAND:  return "BITAND";
        case OpCode::BITOR:   return "BITOR";
        case OpCode::BITXOR:  return "BITXOR";
        case OpCode::LSH:     return "LSH";
        case OpCode::RSH:     return "RSH";
        case OpCode::CMPEQ:   return "CMPEQ";
        case OpCode::CMPNE:   return "CMPNE";
        case OpCode::CMPLT:   return "CMPLT";
        case OpCode::CMPLE:   return "CMPLE";
        case OpCode::CMPGT:   return "CMPGT";
        case OpCode::CMPGE:   return "CMPGE";
        case OpCode::JMP0:    return "JMP0";
        case OpCode::JMP1:    return "JMP1";
        case OpCode::JMP:     return "JMP";
        case OpCode::CALL:    return "CALL";
        case OpCode::RET:     return "RET";
    }

    UNREACHED;
};
