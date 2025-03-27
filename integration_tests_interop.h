#pragma once

#include <functional>

extern "C"
{
    void test_output(const char *format, ...);
}

void register_integration_tests_sink(std::function<void(std::string_view)> callback);
