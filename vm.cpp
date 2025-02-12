#include "vm.h"
#include "op_code.h"

struct Vm
{
    std::span<uint8_t> program;
    std::vector<uint8_t> stack;
    std::vector<uint8_t> memory;
    int64_t rsp;
    int64_t rip;
};

void read(Vm *vm, void *data, size_t len)
{
    if (vm->rip + len > vm->program.size())
    {
        printf("Read outside of program bounds\n");
        assert(false);
    }

    memcpy(data, &vm->program[vm->rip], len);
    vm->rip += len;
}

OpCode read_op(Vm *vm)
{
    OpCode result;
    read(vm, &result, sizeof(result));

    return result;
}

int64_t read_64(Vm *vm)
{
    int64_t result;
    read(vm, &result, sizeof(result));

    return result;
}

void write_stack(Vm *vm, int64_t address, const void *data, size_t len)
{
    if (address + len > vm->rsp)
    {
        printf("Segfault\n");
        assert(false);
    }

    memcpy(&vm->stack[vm->rsp], data, len);
}

void read_stack(Vm *vm, int64_t address, void *data, size_t len)
{
    if (address + len > vm->rsp)
    {
        printf("Segfault\n");
        assert(false);
    }

    memcpy(data, &vm->stack[vm->rsp], len);
}

void push(Vm *vm, const void *data, size_t len)
{
    if (vm->rsp + len > vm->stack.size())
    {
        printf("Stack overflow\n");
        assert(false);
    }

    memcpy(&vm->stack[vm->rsp], data, len);
    vm->rsp += len;
}

void push_64(Vm *vm, int64_t value)
{
    push(vm, &value, sizeof(value));
}

void pop(Vm *vm, void *data, size_t len)
{
    if (vm->rsp < len)
    {
        printf("Stack underflow\n");
        assert(false);
    }

    vm->rsp -= len;
    memcpy(data, &vm->stack[vm->rsp], len);
}

int64_t pop_64(Vm *vm)
{
    int64_t result;
    pop(vm, &result, sizeof(result));

    return result;
}

void run_program(std::span<uint8_t> program)
{
    Vm vm{.program = program};
    vm.stack.resize(64 * 1024);
    // TODO: Stack and main memory could be one array like in a real address space, using a memory break
    vm.memory.resize(64 * 1024);

    while (vm.rip != vm.program.size())
    {
        switch (read_op(&vm))
        {
            case PUSHC:
            {
                auto constant = read_64(&vm);
                push_64(&vm, constant);

                break;
            }

            case LOADR:
            {
                auto offset = read_64(&vm);
                int64_t value;
                read_stack(&vm, vm.rsp + offset, &value, sizeof(value));
                push_64(&vm, value);

                break;
            }

            case LOAD:
            {
                break;
            }

            case STORER:
            {
                auto offset = read_64(&vm);
                auto value  = pop_64(&vm);
                write_stack(&vm, vm.rsp + offset, &value, sizeof(value));

                break;
            }

            case ADDRSP:
            {
                auto offset = read_64(&vm);

            // TODO: Validate bounds!
                vm.rsp += offset;
                break;
            }

            case ADD:
            {
                auto a = pop_64(&vm);
                auto b = pop_64(&vm);
                push_64(&vm, a + b);

                break;
            }

            case SUB:
            {
                auto a = pop_64(&vm);
                auto b = pop_64(&vm);
                push_64(&vm, a - b);

                break;
            }

            case MUL:
            {
                auto a = pop_64(&vm);
                auto b = pop_64(&vm);
                push_64(&vm, a * b);

                break;
            }

            case DIV:
            {
                auto a = pop_64(&vm);
                auto b = pop_64(&vm);
                push_64(&vm, a / b);

                break;
            }

            case MOD:
            {
                auto a = pop_64(&vm);
                auto b = pop_64(&vm);
                push_64(&vm, a % b);

                break;
            }

            case CMP:
            {
                assert(false);
                break;
            }

            case JMP:
            {
                assert(false);
                break;
            }

            case JEQ:
            {
                assert(false);
                break;
            }

            case JNE:
            {
                assert(false);
                break;
            }

            case JLT:
            {
                assert(false);
                break;
            }

            case JLE:
            {
                assert(false);
                break;
            }

            case JGT:
            {
                assert(false);
                break;
            }

            case JGE:
            {
                assert(false);
                break;
            }

            case CALL:
            {
                assert(false);
                break;
            }

            case RET:
            {
                // TODO
                // assert(false);
                break;
            }

            default: assert(false);
        }
    }

    printf("Program ran to last instruction\n");

    for (auto i = 0; i < vm.rsp / 8; ++i)
    {
        auto test = *(int64_t *)&vm.stack[i * 8];
        printf("Stack value %d: %ld\n", i, test);
    }
}
