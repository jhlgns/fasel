#include "compile_ir.h"
#include "desugar.h"
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
        [](TestEvent event)
        {
            if (std::holds_alternative<FailureTestEvent>(event))
            {
                auto failure = std::get<FailureTestEvent>(event);
                std::cout << "test_fail called: " << failure.message << std::endl;
                REQUIRE(false);
            }
            else if (std::holds_alternative<OutputTestEvent>(event))
            {
                auto output = std::get<OutputTestEvent>(event);
                current_test_output += output.output;
            }
            else
            {
                UNREACHED;
            }
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

        if (it->path().extension() != ".fsl")
        {
            continue;
        }

        // if (it->path().string().ends_with("000-basics.fsl") == false)
        // {
        //     continue;
        // }

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

            Context ctx{};

            auto module = parse_module(source.value());
            if (module == nullptr)
            {
                std::cout << "Parsing failed" << std::endl;
                REQUIRE(false);
            }

            module = ast_cast<AstModule, true>(desugar(ctx.pool, module));

            NodeConverter node_converter{ctx};
            auto module_node = node_cast<ModuleNode>(node_converter.make_node(module));

            DeclarationRegistrar registrar{ctx};
            registrar.register_declarations(module_node);
            if (registrar.has_error())
            {
                std::cout << "Semantic analysis failed:" << std::endl;  // TODO
                for (const auto &error : registrar.errors)
                {
                    std::cout << error << std::endl;
                }

                REQUIRE(false);
            }

            TypeChecker type_checker{ctx};
            type_checker.typecheck(module_node);
            if (type_checker.errors.empty() == false)
            {
                std::cout << "Typechecking failed:" << std::endl;
                for (const auto &error : type_checker.errors)
                {
                    std::cout << error << std::endl;
                }

                REQUIRE(false);
            }

            auto compilation_result = compile_to_ir(module_node);
            // compilation_result.module->print(llvm::outs(), nullptr);

            Jit jit{};
            jit.add_module(std::move(compilation_result.context), std::move(compilation_result.module));
            auto main_address = jit.get_symbol_address("main");
            auto main         = reinterpret_cast<void (*)()>(main_address);
            REQUIRE(main != nullptr);

            main();

            // std::cout << "============================" << std::endl;
            // std::cout << "Got test output: " << std::endl;
            // std::cout << current_test_output << std::endl;
            // std::cout << "============================" << std::endl;

            REQUIRE(current_test_output == required_output);
        }
    }
}
