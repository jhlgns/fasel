#pragma once

#include <cstddef>
#include <span>

void init_arena();
void *arena_alloc(size_t bytes);
std::span<char> arena_alloc_span(size_t bytes);
void reset_arena();

/* template<typename... Args> */
/* std::string_view tprint(size_t cap, std::format_string<Args...> format, Args &&...args) */
/* { */
/*     /1* assert(cap >= format.size()); *1/ */
/*     auto buffer = arena_alloc_span(cap); */
/*     std::format_to_n(buffer, cap, format, std::forward<Args>(args)...); */
/* } */
