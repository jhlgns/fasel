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

namespace fs = std::filesystem;

static std::string current_test_output{};

static int _init = []
{
    register_integration_tests_sink(
        [](std::string_view message)
        {
            current_test_output += message;//
        });
    return 1;
}();

TEST_CASE("Integration tests", "[integration]")
{
    for (auto it = fs::directory_iterator{"./integration-tests"}; it != fs::directory_iterator{}; ++it)
    {
        if (it->is_regular_file())
        {
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
            // std::cout << "Requires output: '" << comment.substr(begin, end - begin) << "'" << std::endl;


            // 1. Parsing
            AstModule module;
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

            main();

            std::cout << "Got test output: " << current_test_output << std::endl;

            REQUIRE(current_test_output == required_output);
            current_test_output.clear();
        }
    }
}

TEST_CASE("TODO: Name", "[integration]")
{
    auto source = R"(
main := proc() {
}
)"sv;

    // AstProgram program;
    // REQUIRE(parse_program(source, program));

    // auto program_node = make_node(nullptr, &program);

    // TypeChecker type_checker{.print_errors = true};
    // REQUIRE(type_checker.typecheck(program_node));

    // auto compilation_result = compile_to_ir(program_node);

    // Jit jit{};
    // jit.add_module(std::move(compilation_result.context), std::move(compilation_result.module));
    // auto main_address = jit.get_symbol_address("main");
    // auto main         = reinterpret_cast<int64_t (*)()>(main_address);

    // auto result = main();
}

#if 0
#    include "compile.h"
#    include "parse.h"
#    include "vm.h"

#    include <catch2/catch_test_macros.hpp>

void test_expression(std::string_view expression, int64_t expected_result)
{
    auto source = std::format("main := proc() {{ return {} }}", expression);

    AstProgram program;
    REQUIRE(parse_program(source, program));

    BytecodeWriter w;
    REQUIRE(generate_code(&program, &w));

    Vm vm;
    run_main(&vm, &program, std::span{w.bytecode.begin(), w.bytecode.end()});

    auto result = pop_64(&vm);

    REQUIRE(result == expected_result);

    REQUIRE(vm.rsp == vm.stack_start());
}

TEST_CASE("Expression evaluation", "[integration]")
{
#    define CASE(expr) test_expression(#expr, expr);
    CASE(1);
    CASE(1 + 2);
    CASE(1 + 2 * 3);
    CASE(1 * 2 + 3);
    CASE(1 * 2 + 3 / 4);
    CASE(1 / 2 - 3 * 4);
    CASE(1 * 2 * 3 % 4);
    CASE(0 + 0 - 0 + 0);
    CASE(0 + 0 * 0 + 0);
    CASE(100 * 23 / 34 + 56 * 32480);
    CASE(8 % 8 - 7348 % 23);

    CASE(0xff & 0x70);
    CASE(0xff & 0x00);
    CASE(0xff & 0x07);
    CASE(0xff | 0x70);
    CASE(0xff | 0x00);
    CASE(0xff | 0x07);
    CASE(0xff ^ 0x70);
    CASE(0xff ^ 0x00);
    CASE(0xff ^ 0x07);
    CASE(100 << 2 | 300 >> 4);
    CASE(1 | 2 | 3 | 16);
    CASE(1 & 2 | 3 & 16);
    CASE(1 | 2 & 3 & 16);

    CASE(100 == 1);
    CASE(1 == 1);
    CASE(1 == 100);
    CASE(1 > 1);
    CASE(1 > 100);
    CASE(100 < 1);
    CASE(1 < 1);
    CASE(1 < 100);
    CASE(100 <= 1);
    CASE(1 <= 1);
    CASE(1 <= 100);
    CASE(100 >= 1);
    CASE(1 >= 1);
    CASE(1 >= 100);

    CASE(1576 & 2 * 485 / 871 + 5923 % 7 | 16 & 3);
#    undef CASE
}

int64_t fib(int64_t n)
{
    if (n == 0)
    {
        return 0;
    }

    if (n == 1)
    {
        return 1;
    }

    return fib(n - 1) + fib(n - 2);
}

TEST_CASE("Fibonacci", "[integration]")
{
    auto source = std::string_view{R"(
fib := proc(n i64) {
    if n == 0 { return 0 }
    if n == 1 { return 1 }

    return fib(n - 1) + fib(n - 2)
}

main := proc() {
    return fib(3)
}
)"};

    // TODO: There seems to be something very weird going on.
    // n is not where it is expected to be because something is
    // happening with the stack that I don't understand yet.
    // I think this is a fundamental flaw of how the locals are allocated
    // and accessed that somehow is unearthed only now. We might need an RBP
    // or do something smarter with addressing relative to RSP.
    // TODO: Add a simple test to verify this
    AstProgram program;
    REQUIRE(parse_program(source, program));

    BytecodeWriter w;
    REQUIRE(generate_code(&program, &w));
    // auto test = w.disassemble();

    Vm vm;
    load_program(&vm, &w);
    start_proc_call(&vm, &program, "main");
    run_program(&vm);

    auto result   = pop_64(&vm);
    auto expected = fib(20);

    REQUIRE(result == expected);
}
#endif
