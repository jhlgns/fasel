#pragma once

#include <memory>

namespace llvm
{
    class Module;
    class LLVMContext;
}  // namespace llvm

struct IrCompilationResult
{
    inline explicit IrCompilationResult(
        std::unique_ptr<llvm::LLVMContext> context,
        std::unique_ptr<llvm::Module> module)
        : context(std::move(context))
        , module(std::move(module))
    {
    }
    ~IrCompilationResult();

    std::unique_ptr<llvm::LLVMContext> context;
    std::unique_ptr<llvm::Module> module;
};

IrCompilationResult compile_to_ir(struct Node *node);
