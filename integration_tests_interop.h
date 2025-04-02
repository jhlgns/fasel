#pragma once

#include <functional>

extern "C"
{
    void test_fail(const char *message);
    void test_output(const char *format, ...);
}

struct OutputTestEvent
{
    std::string_view output{};
};

struct FailureTestEvent
{
};

using TestEvent = std::variant<OutputTestEvent, FailureTestEvent>;

void register_integration_tests_sink(std::function<void(TestEvent)> callback);
