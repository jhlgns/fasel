#pragma once

#include "op_code.h"
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
    std::vector<uint8_t> bytecode{};
    struct AstBlock *current_block{};
    /* std::vector<SymbolAddressPlaceholder> saps; */

    bool generate_asm{};
    bool also_generate_bytecode{};
    std::string asm_source{};
};


void write_op(OpCode op, BytecodeWriter *w);
void write_op_8(OpCode op, uint8_t value, BytecodeWriter *w);
void write_op_64(OpCode op, int64_t value, BytecodeWriter *w);

[[nodiscard]] bool generate_code(struct AstNode *node, BytecodeWriter *w);

