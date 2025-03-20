#include "compile_ir.h"
#include "parse.h"
#include "typecheck.h"
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>

// https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl03.html
// https://llvm.org/docs/ProgrammersManual.html
// https://github.com/mukul-rathi/bolt/blob/master/src/llvm-backend/deserialise_ir/expr_ir.cc

using namespace llvm;

auto source = R"(
main := proc() void {
    a := 1
    return 2 + a * 3
}
)"sv;

using namespace llvm;

int main(int argc, char **argv)
{
    AstProgram program;
    if (parse_program(source, program) == false)
    {
        std::cout << "Parsing failed" << std::endl;
        return 1;
    }

    auto node = make_node(nullptr, &program);
    TypeChecker type_checker{.print_errors = true};
    if (type_checker.typecheck(node) == false)
    {
        std::cout << "Typechecking failed" << std::endl;
        return 1;
    }

    auto ir = compile_to_ir(node);

    std::cout << "Done" << std::endl;


    LLVMContext llvm_context;
    IRBuilder ir_builder{llvm_context};
    Module the_module{"janguage test module", llvm_context};

    auto function_type = FunctionType::get(ir_builder.getVoidTy(), false);
    auto function      = Function::Create(function_type, GlobalValue::LinkageTypes::InternalLinkage);
}
