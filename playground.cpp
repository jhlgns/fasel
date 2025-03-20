#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>

// https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl03.html
// https://llvm.org/docs/ProgrammersManual.html
// https://github.com/mukul-rathi/bolt/blob/master/src/llvm-backend/deserialise_ir/expr_ir.cc

using namespace llvm;

LLVMContext llvm_context;
IRBuilder ir_builder{llvm_context};
Module the_module{"janguage test module", llvm_context};

int main(int argc, char **argv)
{
}

