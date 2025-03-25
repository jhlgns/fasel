#include "compile_ir.h"
#include "jit.h"
#include "parse.h"
#include "typecheck.h"
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>

// https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl03.html
// https://llvm.org/docs/ProgrammersManual.html
// https://github.com/mukul-rathi/bolt/blob/master/src/llvm-backend/deserialise_ir/expr_ir.cc

/*

main := proc()
{
    a := 1
    b := 2

    if a == 2
    {
        print("Hallo")
    }

    {
        c := 3
        b = 1

        while b > 10
        {
            f := 10
            print(b)
        }
    }
}


*/

using namespace llvm;

auto source = R"(
main := proc() i64 {
    i64_var := 123
    // TODO: Fix the store instruction emission (needs a pointer, gets something else currently)
    //i64_var = i64_var * 2

    float_var: f32 = 456f

    // TODO: Create implicit conversion node
    coerced := i64_var * float_var

    //return 2 + i64_var * 3
    return 2f * 3 / 1.5
}
)"sv;

using namespace llvm;

int main(int argc, char **argv)
{
    // 1. Parsing
    AstProgram program;
    if (parse_program(source, program) == false)
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
    compilation_result.module->print(outs(), nullptr);

    // 4. Run JIT
    run_main_jit(std::move(compilation_result.context), std::move(compilation_result.module));

    std::cout << "Done" << std::endl;
}
