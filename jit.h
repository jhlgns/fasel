#pragma once

#include <memory>

namespace llvm
{
    class Module;
    class LLVMContext;
}  // namespace llvm

void run_main_jit(std::unique_ptr<llvm::LLVMContext> &&context, std::unique_ptr<llvm::Module> &&module);
