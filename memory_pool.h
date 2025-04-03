#pragma once

#include <cstddef>
#include <span>

struct MemoryPool
{
    size_t capacity{};
    char *memory_start{};
    char *cursor{};

    explicit MemoryPool(size_t capacity);

    void *allocate(size_t bytes);
    std::span<char> allocate_span(size_t bytes);
    void reset(char *where);
};

void *operator new(size_t size, MemoryPool &pool);
