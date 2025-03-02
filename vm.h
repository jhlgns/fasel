#pragma once

#include <span>

struct Vm
{
    constexpr static size_t program_start = 1024;
    constexpr static size_t stack_size    = 64 * 1024;

    // 1024 bytes padding | program | stack
    std::vector<uint8_t> memory;
    size_t program_length;
    int64_t rip;
    int64_t rsp;

    size_t program_end() { return program_start + this->program_length; }

    size_t stack_start() { return this->program_end(); }
    size_t stack_end() { return this->stack_start() + this->stack_size; }
};

int64_t pop_64(Vm *vm);

// TODO: Make a version of load program that does not reset the RSP such that arguments can be passed by the user manaually
void load_program(Vm *vm, struct BytecodeWriter *w);
void load_program(Vm *vm, std::span<uint8_t> bytecode);
void start_proc_call(Vm *vm, struct AstProgram *program, std::string_view proc_name);
void start_proc_call(Vm *vm, int64_t address);
void run_program(Vm *vm);

void run_main(Vm *vm, AstProgram *program, std::span<uint8_t> bytecode);

std::vector<int64_t> DEBUG_dump_stack(Vm *vm);
