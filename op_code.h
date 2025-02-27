#pragma once

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
    PUSHC,  // value = read i64; push i64 value;
    LOADR,  // offs = read i64; push i64 *(RSP + offs);
    LOAD,   // addr = read i64; push i64 *addr;
    STORER,  // offs = read i64; value = pop i64; *(RSP + offs) = value;
    ADDRSP,  // offs = read i64; RSP += offs

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
    RET,    // value = pop i64; ret = pop i64; push i64 value; jmp ret;
};

inline const char *to_string(OpCode op)
{
    switch (op)
    {
        case PUSHC:  return "PUSHC";
        case LOADR:  return "LOADR";
        case LOAD:   return "LOAD";
        case STORER: return "STORER";
        case ADDRSP: return "ADDRSP";
        case ADD:    return "ADD";
        case SUB:    return "SUB";
        case MUL:    return "MUL";
        case DIV:    return "DIV";
        case MOD:    return "MOD";
        case BITAND: return "BITAND";
        case BITOR:  return "BITOR";
        case BITXOR: return "BITXOR";
        case LSH:    return "LSH";
        case RSH:    return "RSH";
        case CMPEQ:  return "CMPEQ";
        case CMPNE:  return "CMPNE";
        case CMPLT:  return "CMPLT";
        case CMPLE:  return "CMPLE";
        case CMPGT:  return "CMPGT";
        case CMPGE:  return "CMPGE";
        case JMP0:   return "JMP0";
        case JMP1:   return "JMP1";
        case JMP:    return "JMP";
        case RET:    return "RET";
    }

    assert(false);
};
