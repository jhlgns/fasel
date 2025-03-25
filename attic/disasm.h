#pragma once

#include <span>
#include <string>

std::string disassemble(std::span<uint8_t> program, int64_t mark_address = -1);
