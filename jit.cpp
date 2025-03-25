#include "jit.h"
#include "basics.h"

#include <iostream>
#include <llvm/ADT/StringRef.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/Orc/CompileUtils.h>
#include <llvm/ExecutionEngine/Orc/Core.h>
#include <llvm/ExecutionEngine/Orc/ExecutionUtils.h>
#include <llvm/ExecutionEngine/Orc/IRCompileLayer.h>
#include <llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h>
#include <llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/NoFolder.h>
#include <llvm/IR/Value.h>

using namespace llvm;
using namespace llvm::orc;

void run_main_jit(std::unique_ptr<llvm::LLVMContext> &&context, std::unique_ptr<llvm::Module> &&module)
{
    LLVMInitializeNativeAsmParser();
    LLVMInitializeNativeAsmPrinter();
    LLVMInitializeNativeDisassembler();
    LLVMInitializeNativeTarget();
    LLVMInitializeX86TargetInfo();
    LLVMInitializeX86TargetMC();

    auto executor_process_control = SelfExecutorProcessControl::Create();
    if (!executor_process_control)
    {
        std::cout << toString(executor_process_control.takeError()) << std::endl;
        TODO;
    }

    auto execution_session = std::make_unique<ExecutionSession>(std::move(*executor_process_control));

    auto jit_target_machine_builder = JITTargetMachineBuilder::detectHost();
    if (!jit_target_machine_builder)
    {
        std::cout << "JITTargetMachineBuilder::detectHost() failed: "
                  << toString(jit_target_machine_builder.takeError()) << std::endl;
        TODO;
    }

    auto data_layout = jit_target_machine_builder->getDefaultDataLayoutForTarget();
    if (!data_layout)
    {
        std::cout << "JITTargetMachineBuilder::getDefaultDataLayoutForTarget failed" << std::endl;
        std::cout << "Target triple: " << jit_target_machine_builder->getTargetTriple().getTriple() << std::endl;
        std::cout << toString(data_layout.takeError()) << std::endl;
        TODO;
    }

    MangleAndInterner mangle{*execution_session, *data_layout};

    RTDyldObjectLinkingLayer object_layer{
        *execution_session,
        []() { return std::make_unique<SectionMemoryManager>(); }};
    IRCompileLayer compile_layer{
        *execution_session,
        object_layer,
        std::make_unique<ConcurrentIRCompiler>(std::move(*jit_target_machine_builder))};
    // execution_session.getMainJITDylib().addGenerator(
    //                 cantFail(DynamicLibrarySearchGenerator::GetForCurrentProcess(DL.getGlobalPrefix())));

    auto &main_jit_dy_lib = execution_session->createBareJITDylib("<main>");

    auto error = compile_layer.add(main_jit_dy_lib, ThreadSafeModule{std::move(module), std::move(context)});
    if (error)
    {
        std::cout << "Failed to add the module to the compile layer: " << toString(std::move(error)) << std::endl;
        TODO;
    }

    // std::cout << "--------------------------" << std::endl;
    // std::cout << "Dump of execution session:" << std::endl;
    // execution_session->dump(outs());
    // std::cout << "--------------------------" << std::endl;

    auto main_symbol_def = execution_session->lookup({&main_jit_dy_lib}, mangle("main"));
    if (!main_symbol_def)
    {
        std::cout << "Error looking up main symbol definition: " << toString(main_symbol_def.takeError()) << std::endl;
        TODO;
    }

    auto main = main_symbol_def->getAddress().toPtr<double (*)()>();
    assert(main != nullptr);
    std::cout << "main() is at " << reinterpret_cast<void *>(main) << std::endl;

    auto result = main();

    std::cout << "main() returned: " << result << std::endl;

    if (auto error = execution_session->endSession(); error)
    {
        std::cout << "Failed to end the execution session: " << toString(std::move(error)) << std::endl;
        TODO;
    }
}
