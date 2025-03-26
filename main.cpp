#include <cassert>
#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include "basics.h"
#include "compile_ir.h"
#include "jit.h"
#include "parse.h"
#include "typecheck.h"
#include "llvm/IR/Module.h"

int main(int argc, char **argv)
{
    std::cout << "This is the fasel compiler." << std::endl;

    if (argc != 2)
    {
        std::cerr << "Usage: fasel <main source file>" << std::endl;
        return 1;
    }

    auto path = argv[1];
    std::cout << "Compiling file: " << path << std::endl;

    auto f = fopen(path, "r");
    if (f == nullptr)
    {
        std::cerr << "Could not open file " << path << std::endl;
        return 1;
    }
    defer
    {
        fclose(f);
    };

    fseek(f, 0, SEEK_END);
    auto file_size = ftell(f);
    rewind(f);
    auto source       = std::make_unique<char[]>(file_size + 1);
    source[file_size] = 0;

    size_t pos  = 0;
    size_t read = 0;
    for (size_t read = 0; (read = fread(&source[pos], 1, file_size - pos, f)); pos += read)
    {
    }

    // 1. Parsing
    AstProgram program;
    if (parse_program(source.get(), program) == false)
    {
        std::cout << "Parsing failed" << std::endl;
        return 1;
    }

    // 2. Typechecking
    auto program_node = make_node(nullptr, &program);

    TypeChecker type_checker{.print_errors = true};
    if (type_checker.typecheck(program_node) == false)
    {
        std::cout << "Typechecking failed" << std::endl;
        return 1;
    }

    // 3. Compile to IR
    auto compilation_result = compile_to_ir(program_node);
    compilation_result.module->print(llvm::outs(), nullptr);

    // 4. Run JIT
    Jit jit{};
    jit.add_module(std::move(compilation_result.context), std::move(compilation_result.module));
    auto main_address = jit.get_symbol_address("main");
    auto main         = reinterpret_cast<void (*)()>(main_address);

    main();

    std::cout << "Done" << std::endl;

    return 0;
}
