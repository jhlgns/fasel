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

// template<typename... Args>
// std::string_view tprint(size_t cap, std::format_string<Args...> format, Args &&...args)
// {
//     /1* assert(cap >= format.size()); *1/
//     auto buffer = arena_alloc_span(cap);
//     std::format_to_n(buffer, cap, format, std::forward<Args>(args)...);
// }
