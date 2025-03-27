#pragma once

#include <string_view>

std::optional<std::string> read_file_as_string(std::string_view path);
std::string_view extract_line(std::string_view source, const char *offset);
