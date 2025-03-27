#include <cstdarg>
#include <cstdio>
#include <functional>
#include <vector>

static std::vector<std::function<void(std::string_view)>> sinks{};

extern "C"
{
    void test_output(const char *format, ...)
    {
        // TODO: Dynamic buffer size
        constexpr size_t buffer_size = 8 * 1024 * 1024;
        static const auto buffer     = new char[buffer_size];

        va_list va_args;
        va_start(va_args, format);
        vsnprintf(buffer, buffer_size, format, va_args);
        va_end(va_args);

        for (const auto &sink : sinks)
        {
            sink(buffer);
        }
    }
}

void register_integration_tests_sink(std::function<void(std::string_view)> callback)
{
    sinks.push_back(std::move(callback));
}
