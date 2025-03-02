#pragma once

#include "op_code.h"
#include <cassert>
#include <cstdint>
#include <vector>

struct BytecodeWriter
{
    std::vector<uint8_t> bytecode{};
    size_t pos{};  // TODO: The post-fixups do not correct the ASM
    struct AstBlock *current_block{};
    struct AstProc *current_proc{};

    std::string disassemble();
};

std::vector<AstBlock *> get_statement_child_blocks(class AstNode *node);

int64_t write_op(OpCode op, BytecodeWriter *w);
int64_t write_op_8(OpCode op, uint8_t value, BytecodeWriter *w);
int64_t write_op_64(OpCode op, int64_t value, BytecodeWriter *w);

[[nodiscard]] bool generate_code(struct AstNode *node, BytecodeWriter *w);
