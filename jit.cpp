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

static int _initted = []
{
    LLVMInitializeNativeAsmParser();
    LLVMInitializeNativeAsmPrinter();
    LLVMInitializeNativeDisassembler();
    LLVMInitializeNativeTarget();
    return 1;
}();

struct Jit::Impl
{
    std::optional<ExecutionSession> execution_session{};
    std::optional<RTDyldObjectLinkingLayer> object_layer{};
    std::optional<IRCompileLayer> compile_layer{};

    std::optional<MangleAndInterner> mangle{};
    std::optional<DataLayout> data_layout{};

    JITDylib *main_jit_dy_lib{};

    explicit Impl()
    {
        auto executor_process_control = SelfExecutorProcessControl::Create();
        if (!executor_process_control)
        {
            std::cout << "SelfExecutorProcessControl::Create() failed:"
                      << toString(executor_process_control.takeError()) << std::endl;
            FATAL("Failed to create JIT session");
        }

        this->execution_session.emplace(std::move(*executor_process_control));

        auto jit_target_machine_builder = JITTargetMachineBuilder::detectHost();
        if (!jit_target_machine_builder)
        {
            std::cout << "JITTargetMachineBuilder::detectHost() failed: "
                      << toString(jit_target_machine_builder.takeError()) << std::endl;
            FATAL("Failed to create JIT session");
        }

        auto data_layout = jit_target_machine_builder->getDefaultDataLayoutForTarget();
        if (!data_layout)
        {
            std::cout << "JITTargetMachineBuilder::getDefaultDataLayoutForTarget failed" << std::endl;
            std::cout << "Target triple: " << jit_target_machine_builder->getTargetTriple().getTriple() << std::endl;
            std::cout << toString(data_layout.takeError()) << std::endl;
            FATAL("Failed to create JIT session");
        }

        this->data_layout = std::move(*data_layout);

        this->mangle.emplace(this->execution_session.value(), this->data_layout.value());

        this->object_layer.emplace(*execution_session, []() { return std::make_unique<SectionMemoryManager>(); });

        this->compile_layer.emplace(
            this->execution_session.value(),
            this->object_layer.value(),
            std::make_unique<ConcurrentIRCompiler>(std::move(*jit_target_machine_builder)));

        this->main_jit_dy_lib = &this->execution_session->createBareJITDylib("<main>");

        this->main_jit_dy_lib->addGenerator(
            cantFail(DynamicLibrarySearchGenerator::GetForCurrentProcess(this->data_layout.value().getGlobalPrefix())));
    }
};

Jit::Jit()
    : impl{std::make_unique<Jit::Impl>()}
{
}

Jit::~Jit() = default;

void Jit::add_module(std::unique_ptr<llvm::LLVMContext> context, std::unique_ptr<llvm::Module> module)
{
    auto error = this->impl->compile_layer->add(
        *this->impl->main_jit_dy_lib,
        ThreadSafeModule{std::move(module), std::move(context)});
    if (error)
    {
        std::cout << "Failed to add the module to the compile layer: " << toString(std::move(error)) << std::endl;
        FATAL("Failed to add module to JIT session");
    }
}

void *Jit::get_symbol_address(std::string_view name)
{
    auto def = this->impl->execution_session->lookup({this->impl->main_jit_dy_lib}, (*this->impl->mangle)("main"));
    if (!def)
    {
        std::cout << "Error looking up main symbol definition: " << toString(def.takeError()) << std::endl;
        FATAL("Failed to lookup symbol definition");
    }

    return def->getAddress().toPtr<void *>();
}


#if 0
void run_main_jit(std::unique_ptr<llvm::LLVMContext> &&context, std::unique_ptr<llvm::Module> &&module)
{
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
#endif
