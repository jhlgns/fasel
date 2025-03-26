#include "catch2/catch_test_macros.hpp"
#include "string_util.hpp"

TEST_CASE("Line extraction", "[string_util]")
{
    std::string_view cases[][2] = {
        {"", ""},
        {"x", "x"},
        {"xyz", "xyz"},
        {" xyz", "xyz"},
        {"xyz ", "xyz"},
        {"\nxyz", "xyz"},
        {"xyz\n", "xyz"},
        {"\nxyz\n", "xyz"},
        {"\n xyz\n", "xyz"},
        {"\nxyz \n", "xyz"},
        {" \n xyz\n", "xyz"},
        {" \nxyz \n", "xyz"},
        {" \n xyz \n ", "xyz"},
        {" \n \txyz \t \n ", "xyz"},
    };

    for (const auto [source, expected_line] : cases)
    {
        auto offset = source.find('x');
        if (offset == std::string_view::npos)
        {
            offset = 0;
        }

        auto received_line = extract_line(source, source.data() + offset);
        REQUIRE(received_line == expected_line);
    }
}
