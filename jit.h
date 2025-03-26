#pragma once

#include <memory>
#include <string_view>

namespace llvm
{
    class Module;
    class LLVMContext;
}  // namespace llvm

struct Jit
{
    struct Impl;

    std::unique_ptr<Impl> impl;

    explicit Jit();
    ~Jit();
    void add_module(std::unique_ptr<llvm::LLVMContext> context, std::unique_ptr<llvm::Module> module);
    void *get_symbol_address(std::string_view name);
};

// void run_main_jit(std::unique_ptr<llvm::LLVMContext> &&context, std::unique_ptr<llvm::Module> &&module);
