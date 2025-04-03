#include "memory_pool.h"

#include "basics.h"

#include <cstdio>
#include <cstdlib>

MemoryPool::MemoryPool(size_t capacity)
    : capacity{capacity}
    , memory_start{static_cast<char *>(malloc(capacity))}
{
    this->cursor = this->memory_start;
}

void *MemoryPool::allocate(size_t bytes)
{
    if (this->cursor + bytes > this->memory_start + this->capacity)
    {
        FATAL("Memory pool out of memory");
    }

    auto result = this->cursor;
    this->cursor += bytes;

    // TODO: Alignment?
    // memset(result, 0x0, bytes);

    return result;
}

std::span<char> MemoryPool::allocate_span(size_t bytes)
{
    auto result = static_cast<char *>(this->allocate(bytes));
    return std::span<char>{result, bytes};
}

void MemoryPool::reset(char *where)
{
    if (where < this->memory_start || where > this->memory_start + this->capacity)
    {
        FATAL("Memory pool reset out of bounds");
    }

    this->cursor = where;
}

void *operator new(size_t size, MemoryPool &pool)
{
    return pool.allocate(size);
}
