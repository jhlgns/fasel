#include "string_util.h"

#include "basics.h"

#include <cassert>
#include <cstdio>
#include <ostream>
#include <string>
#include <string_view>

std::optional<std::string> read_file_as_string(std::string_view path)
{
    auto f = std::fopen(path.data(), "r");
    if (f == nullptr)
    {
        std::cerr << "Could not open file " << path << std::endl;
        return std::nullopt;
    }
    defer
    {
        fclose(f);
    };

    fseek(f, 0, SEEK_END);
    auto file_size = ftell(f);
    rewind(f);

    auto result = std::string{};
    result.resize(file_size + 1);
    result[file_size] = 0;

    size_t pos  = 0;
    size_t read = 0;
    for (size_t read = 0; (read = fread(&result[pos], 1, file_size - pos, f)); pos += read)
    {
    }

    return result;
}

std::string_view extract_line(std::string_view source, const char *offset)
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
