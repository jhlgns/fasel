#pragma once

#include <cassert>
#include <string_view>

inline std::string_view extract_line(std::string_view source, const char *offset)
{
    if (source.size() == 0)
    {
        return source;
    }

    auto source_start = source.data();
    auto source_end   = source.data() + source.size();

    assert(offset >= source_start && offset < source_end);

    auto line_start = offset;
    auto line_end   = offset;

    while (line_start > source_start && line_start[-1] != '\n')
    {
        --line_start;
    }

    while (line_start[0] && *line_start != '\n' && (*line_start == ' ' || *line_start == '\t'))
    {
        ++line_start;
    }

    while (line_end[1] && line_end[1] != '\n')
    {
        ++line_end;
    }

    while (line_end > source_start && (*line_end == ' ' || *line_end == '\t'))
    {
        --line_end;
    }

    auto line = std::string_view{line_start, line_end + 1};
    assert(line.find("\n") == std::string_view::npos);

    return line;
}
