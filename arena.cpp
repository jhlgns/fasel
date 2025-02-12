#include "arena.h"

#include <cstdio>
#include <cstdlib>

constexpr size_t arena_size = 4 * 1024 * 1024;

struct
{
    char *memory_start;
    char *cursor;
} arena;

void init_arena()
{
    arena.memory_start = (char *)malloc(arena_size);
    arena.cursor       = arena.memory_start;
}

void *arena_alloc(size_t bytes)
{
    if ((arena.cursor - arena.memory_start) + bytes >= arena_size)
    {
        printf("Out of arena memory\n");
        abort();
    }

    char *result = arena.cursor;
    arena.cursor += bytes;

    // TODO: Alignment?
    /* memset(result, 0x0, bytes); */

    return result;
}

void reset_arena()
{
    arena.cursor = arena.memory_start;
}
