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
    struct AstProcedure *current_proc{};

    void write_data(const void *data, size_t length);
    int64_t write_op(OpCode op);
    int64_t write_op_8(OpCode op, uint8_t value);
    int64_t write_op_64(OpCode op, int64_t value);
};

std::vector<AstBlock *> get_statement_child_blocks(class AstNode *node);

[[nodiscard]] bool generate_code(struct AstNode *node, BytecodeWriter *w);

