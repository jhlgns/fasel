#pragma once

#include <cstddef>

void init_arena();
void *arena_alloc(size_t bytes);
void reset_arena();

