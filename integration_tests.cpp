#include "compile_ir.h"
#include "integration_tests_interop.h"
#include "jit.h"
#include "lex.h"
#include "parse.h"
#include "string_util.h"
#include "typecheck.h"

#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <llvm/IR/Module.h>

// TODO: Implement some way of converting the program to a C program and compare the output

namespace fs = std::filesystem;

static std::string current_test_output{};

static int _init = []
{
    register_integration_tests_sink(
        [](std::string_view message)
        {
            current_test_output += message;  //
        });
    return 1;
}();

TEST_CASE("Integration tests", "[integration]")
{
    for (auto it = fs::directory_iterator{"../integration-tests"}; it != fs::directory_iterator{}; ++it)
    {
        if (it->is_regular_file() == false)
        {
            continue;
        }

        SECTION(it->path().string())
        {
            defer
            {
                current_test_output.clear();
            };

            auto source = read_file_as_string(it->path().string());
            REQUIRE(source.has_value());

            Lexer lexer{source.value()};
            auto token = lexer.next_token();
            REQUIRE(token.type == Tt::multi_line_comment);

            auto comment = token.text();

            auto marker = "OUTPUT:\n"sv;
            auto begin  = comment.find(marker);
            REQUIRE(begin != std::string_view::npos);

            begin += marker.length();

            auto end = comment.rfind("*/");
            REQUIRE(end != std::string_view::npos);

            auto required_output = comment.substr(begin, end - begin);

            // 1. Parsing
            AstModule module{};
            if (parse_module(source.value(), module) == false)
            {
                std::cout << "Parsing failed" << std::endl;
                REQUIRE(false);
            }

            // 2. Typechecking
            Context ctx{};
            TypeChecker type_checker{ctx};
            auto module_node = node_cast<ModuleNode>(type_checker.make_node(&module));

            if (type_checker.typecheck(module_node) == false)
            {
                std::cout << "Typechecking failed:" << std::endl;
                for (const auto &error : type_checker.errors)
                {
                    std::cout << error << std::endl;
                }

                REQUIRE(false);
            }

            // 3. Compile to IR
            auto compilation_result = compile_to_ir(module_node);
            compilation_result.module->print(llvm::outs(), nullptr);

            // 4. Run JIT
            Jit jit{};
            jit.add_module(std::move(compilation_result.context), std::move(compilation_result.module));
            auto main_address = jit.get_symbol_address("main");
            auto main         = reinterpret_cast<void (*)()>(main_address);
            REQUIRE(main != nullptr);

            main();

            std::cout << "============================" << std::endl;
            std::cout << "Got test output: " << std::endl;
            std::cout << current_test_output << std::endl;
            std::cout << "============================" << std::endl;

            REQUIRE(current_test_output == required_output);
        }
    }
}
