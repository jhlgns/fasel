#include "vm.h"
#include "compile_vm.h"
#include "op_code.h"
#include "parse.h"
#include <format>
#include <iostream>

void read_program(Vm *vm, void *data, size_t len)
{
    if (vm->rip < Vm::program_start || vm->rip + len > vm->program_end())
    {
        FATAL("Read outside of program bounds");
    }

    memcpy(data, &vm->memory[vm->rip], len);
    vm->rip += len;
}

OpCode read_op(Vm *vm)
{
    OpCode result;
    read_program(vm, &result, sizeof(result));

    return result;
}

int64_t read_arg_64(Vm *vm)
{
    int64_t result;
    read_program(vm, &result, sizeof(result));

    return result;
}

void write_memory(Vm *vm, int64_t address, const void *data, size_t len)
{
    if (address < vm->stack_start() || address + len > vm->stack_end())
    {
        FATAL("Segfault");
    }

    memcpy(&vm->memory[address], data, len);
}

void read_memory(Vm *vm, int64_t address, void *data, size_t len)
{
    if (address < vm->program_start || address + len > vm->stack_end())
    {
        FATAL("Segfault");
    }

    memcpy(data, &vm->memory[address], len);
}

void push(Vm *vm, const void *data, size_t len)
{
    if (vm->rsp + len > vm->stack_end())
    {
        FATAL("Stack overflow");
    }

    memcpy(&vm->memory[vm->rsp], data, len);
    vm->rsp += len;
}

void push_64(Vm *vm, int64_t value)
{
    push(vm, &value, sizeof(value));
}

void pop(Vm *vm, void *data, size_t len)
{
    if (vm->rsp < len || vm->rsp - len < vm->stack_start())
    {
        FATAL("Stack underflow");
    }

    vm->rsp -= len;
    memcpy(data, &vm->memory[vm->rsp], len);
}

int64_t pop_64(Vm *vm)
{
    int64_t result;
    pop(vm, &result, sizeof(result));

    return result;
}

void load_program(Vm *vm, BytecodeWriter *w)
{
    load_program(vm, std::span<uint8_t>{w->bytecode.begin(), w->bytecode.end()});
}

void load_program(Vm *vm, std::span<uint8_t> bytecode)
{
    vm->program_length = bytecode.size();
    vm->rip            = Vm::program_start;
    vm->rsp            = vm->stack_start();

    vm->memory.resize(Vm::program_start + bytecode.size() + Vm::stack_size);  // TODO: Alignment of stack start?
    memcpy(&vm->memory[Vm::program_start], bytecode.data(), bytecode.size());
}

void start_proc_call(Vm *vm, struct AstProgram *program, std::string_view proc_name)
{
    UNREACHED
    // auto proc_decl = program->block.find_declaration(proc_name);
    // if (proc_decl == nullptr)
    // {
    //     FATAL(std::format("Procedure '{}' not found", proc_name));
    // }

    // start_proc_call(vm, proc_decl->address);
}

void start_proc_call(Vm *vm, int64_t address)
{
    push_64(vm, vm->program_length);  // NOTE: The return address
    vm->rip = Vm::program_start + address;
}

void run_program(Vm *vm)
{
    auto debug_mode = strcmp("1", std::getenv("JANG_DEBUG")) == 0;

    while (vm->rip < vm->program_end())
    {
        if (debug_mode)
        {
            std::cout << "================[" << vm->rip - Vm::program_start << "]================" << std::endl;

            std::cout << "Stack:" << std::endl;

            auto stack = DEBUG_dump_stack(vm);
            for (auto i = 0; i < stack.size(); ++i)
            {
                std::cout << std::format("{:>7}: {}", i, stack[i]) << std::endl;
            }

            std::cout << "Program:" << std::endl;

            UNREACHED;
            // auto disasm =
            // disassemble(std::span{&vm->memory[Vm::program_start], vm->program_length}, vm->rip - Vm::program_start);
            // std::cout << disasm << std::endl;

            std::string command;
            std::getline(std::cin, command);
        }

        auto op = read_op(vm);
        switch (op)
        {
            case PUSHC:
            {
                auto constant = read_arg_64(vm);
                push_64(vm, constant);

                break;
            }

#define LOADR_CASE(bytes, cpp_type)                               \
    case LOADR##bytes:                                            \
    {                                                             \
        auto offset = read_arg_64(vm);                            \
        cpp_type value;                                           \
        read_memory(vm, vm->rsp + offset, &value, sizeof(value)); \
        push_64(vm, value);                                       \
                                                                  \
        break;                                                    \
    }
                LOADR_CASE(1, uint8_t)
                LOADR_CASE(2, uint16_t)
                LOADR_CASE(4, uint32_t)
                LOADR_CASE(8, uint64_t)
#undef LOADR_CASE

#define STORER_CASE(bytes)                                 \
    case STORER##bytes:                                    \
    {                                                      \
        auto offset = read_arg_64(vm);                     \
        auto value  = pop_64(vm);                          \
        write_memory(vm, vm->rsp + offset, &value, bytes); \
                                                           \
        break;                                             \
    }
                STORER_CASE(1)
                STORER_CASE(2)
                STORER_CASE(4)
                STORER_CASE(8)
#undef STORER_CASE

            case ADDRSP:
            {
                auto offset = read_arg_64(vm);
                vm->rsp += offset;

                break;
            }

            case JMP:
            {
                auto target = read_arg_64(vm);
                vm->rip     = Vm::program_start + target;

                break;
            }

            case JMP0:
            {
                auto target    = read_arg_64(vm);
                auto condition = pop_64(vm);

                if (condition == 0)
                {
                    vm->rip = Vm::program_start + target;
                }

                break;
            }

            case JMP1:
            {
                auto target    = read_arg_64(vm);
                auto condition = pop_64(vm);

                if (condition != 0)
                {
                    vm->rip = Vm::program_start + target;
                }

                break;
            }

            case CALL:
            {
                auto target = pop_64(vm);
                push_64(vm, vm->rip - Vm::program_start);
                vm->rip = Vm::program_start + target;

                break;
            }

            case RET:
            {
                auto return_value = pop_64(vm);
                auto return_addr  = pop_64(vm);
                push_64(vm, return_value);
                vm->rip = Vm::program_start + return_addr;

                break;
            }

#define BINOP_CASE(op_code, op) \
    case op_code:               \
    {                           \
        auto b = pop_64(vm);    \
        auto a = pop_64(vm);    \
        push_64(vm, a op b);    \
        break;                  \
    }
                BINOP_CASE(ADD, +)
                BINOP_CASE(SUB, -)
                BINOP_CASE(MUL, *)
                BINOP_CASE(DIV, /)
                BINOP_CASE(MOD, %)
                BINOP_CASE(BITAND, &)
                BINOP_CASE(BITOR, |)
                BINOP_CASE(BITXOR, ^)
                BINOP_CASE(RSH, >>)
                BINOP_CASE(LSH, <<)
                BINOP_CASE(CMPEQ, ==)
                BINOP_CASE(CMPNE, !=)
                BINOP_CASE(CMPGE, >=)
                BINOP_CASE(CMPGT, >)
                BINOP_CASE(CMPLE, <=)
                BINOP_CASE(CMPLT, <)
#undef BINOP_CASE

            default: FATAL("Unimplemented instruction");
        }
    }

    if (vm->rip != vm->program_end())
    {
        FATAL("Decoding error: RIP is not at the end of the program");
    }
}

void run_main(Vm *vm, AstProgram *program, std::span<uint8_t> bytecode)
{
    load_program(vm, bytecode);
    start_proc_call(vm, program, "main");
    run_program(vm);
}

std::vector<int64_t> DEBUG_dump_stack(Vm *vm)
{
    auto count = vm->rsp - vm->stack_start();
    assert(count % 8 == 0);
    count /= 8;

    std::vector<int64_t> result;
    result.resize(count);

    for (auto i = 0; i < count; ++i)
    {
        memcpy(&result[i], &vm->memory[vm->stack_start() + i * 8], 8);
    }

    return result;
}
