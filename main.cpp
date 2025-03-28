#include "compile_ir.h"
#include "jit.h"
#include "parse.h"
#include "string_util.h"
#include "typecheck.h"

#include <cassert>
#include <cstdarg>
#include <cstdlib>
#include <cstring>
#include <llvm/IR/Module.h>

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

    auto source_file = read_file_as_string(path);
    if (source_file.has_value())
    {
        std::cerr << "Failed to read source file" << std::endl;
        return 1;
    }

    auto source = std::move(source_file.value());

#if 0
    Lexer lexer{source.get()};
    for (auto i = 0;; ++i)
    {
        auto token = lexer.next_token();
        std::cout << "Token " << i << ": " << token.to_string() << std::endl;
        if (token.type == Tt::eof)
        {
            break;
        }
    }
#endif

    // 1. Parsing
    AstModule module{};
    if (parse_module(source, module) == false)
    {
        std::cout << "Parsing failed" << std::endl;
        return 1;
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

        return 1;
    }

    // for (auto [name, decl] : module_node->block->declarations)
    // {
    //     std::cout << "XXX Found global symbol: " << name << " (" << type_to_string(decl->init_expression->type) << ")"
    //               << std::endl;
    // }

    // 3. Compile to IR
    auto compilation_result = compile_to_ir(module_node);
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
