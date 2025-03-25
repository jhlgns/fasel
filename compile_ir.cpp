#include "compile_ir.h"
#include "typecheck.h"

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

// https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl03.html
// https://llvm.org/docs/ProgrammersManual.html
// https://github.com/mukul-rathi/bolt/blob/master/src/llvm-backend/deserialise_ir/expr_ir.cc

// TODO: For testing, use reverse compilation to C: https://github.com/lifting-bits/rellic

using namespace llvm;

struct IrCompiler
{
    explicit IrCompiler(LLVMContext &llvm_context)
        : llvm_context{llvm_context}
        , module{std::make_unique<Module>("JanguageTestModule", llvm_context)}
        , ir{llvm_context}
    {
    }

    LLVMContext &llvm_context;
    std::unique_ptr<Module> module;
    Function *function{};
    IRBuilder<llvm::NoFolder> ir;

    void allocate_locals(BlockNode *block)
    {
        for (auto statement : block->statements)
        {
            if (auto block = node_cast<BlockNode>(statement))
            {
                allocate_locals(block);
                continue;
            }

            if (auto yf = node_cast<IfNode>(statement))
            {
                allocate_locals(yf->then_block);

                if (yf->else_block != nullptr)
                {
                    allocate_locals(yf->else_block);
                }

                continue;
            }

            if (auto decl = node_cast<DeclarationNode>(statement))
            {
                auto it = block->locals.find(std::string{decl->identifier});
                assert(it != block->locals.end());

                // NOTE: This does not handle scope overlapping (phi allocations)

                auto type           = this->convert_type(decl->init_expression->type);
                it->second.location = this->ir.CreateAlloca(type, nullptr, decl->identifier);
            }
        }
    }

    Type *convert_type(const Node *node)
    {
        assert(node != nullptr);

        switch (node->kind)
        {
            case NodeKind::simple_type:
            {
                auto simple = static_cast<const SimpleTypeNode *>(node);

                switch (simple->type_kind)
                {
                    case SimpleTypeNode::Kind::voyd:             return this->ir.getVoidTy();
                    case SimpleTypeNode::Kind::boolean:          return this->ir.getInt1Ty();
                    case SimpleTypeNode::Kind::signed_integer:   return this->ir.getIntNTy(simple->size * 8);
                    case SimpleTypeNode::Kind::unsigned_integer: return this->ir.getIntNTy(simple->size * 8);

                    case SimpleTypeNode::Kind::floatingpoint:
                    {
                        switch (simple->size)
                        {
                            case 4:  return this->ir.getFloatTy();
                            case 8:  return this->ir.getDoubleTy();
                            default: UNREACHED;
                        }
                    }

                    default: UNREACHED;
                }
            }

            case NodeKind::pointer_type:
            {
                return this->ir.getPtrTy();
            }

            case NodeKind::array_type:
            {
                auto array = static_cast<const ArrayTypeNode *>(node);

                auto element_type = this->convert_type(array->element_type);

                auto length_literal = node_cast<LiteralNode>(array->length_expression);
                if (length_literal == nullptr)
                {
                    TODO;
                }

                if (std::holds_alternative<uint64_t>(length_literal->value) == false)
                {
                    TODO;
                }

                auto length = std::get<uint64_t>(length_literal->value);

                return ArrayType::get(element_type, length);
            }

            case NodeKind::struct_type:
            {
                auto strukt = static_cast<const StructTypeNode *>(node);

                TODO;
            }

            case NodeKind::procedure_signature:
            {
                auto signature = static_cast<const ProcedureSignatureNode *>(node);

                auto return_type = this->convert_type(signature->return_type);
                std::vector<Type *> argument_types;

                for (auto argument : signature->arguments)
                {
                    auto argument_type = this->convert_type(argument->init_expression->type);
                    argument_types.push_back(argument_type);
                }

                auto function_type = FunctionType::get(return_type, argument_types, false);

                return function_type;
            }

            default: UNREACHED;
        }
    }

    Value *generate_binary_operator_code(BinaryOperatorNode *bin_op)
    {
        auto lhs = this->generate_code(bin_op->lhs);
        if (lhs == nullptr)
        {
            UNREACHED;
        }

        auto rhs = this->generate_code(bin_op->rhs);
        if (rhs == nullptr)
        {
            UNREACHED;
        }

        assert(bin_op->type->kind == NodeKind::simple_type);
        auto type = node_cast<SimpleTypeNode, true>(bin_op->type);

        switch (type->type_kind)
        {
            case SimpleTypeNode::Kind::signed_integer:
            {
                switch (bin_op->operator_type)
                {
                    case Tt::asterisk:              return this->ir.CreateMul(lhs, rhs); break;
                    case Tt::slash:                 return this->ir.CreateSDiv(lhs, rhs); break;
                    case Tt::mod:                   return this->ir.CreateSRem(lhs, rhs); break;
                    case Tt::plus:                  return this->ir.CreateAdd(lhs, rhs); break;
                    case Tt::minus:                 return this->ir.CreateSub(lhs, rhs); break;
                    case Tt::assign:                return this->ir.CreateStore(rhs, lhs); break;  // TODO: Pointer to left hand side?
                    case Tt::bit_and:               return this->ir.CreateAnd(lhs, rhs); break;
                    case Tt::bit_or:                return this->ir.CreateOr(lhs, rhs); break;
                    case Tt::bit_xor:               return this->ir.CreateXor(lhs, rhs); break;
                    case Tt::left_shift:            return this->ir.CreateShl(lhs, rhs); break;
                    case Tt::right_shift:           return this->ir.CreateLShr(lhs, rhs); break;  // TODO: LShr or AShr?
                    case Tt::equal:                 return this->ir.CreateICmp(CmpInst::ICMP_EQ, lhs, rhs); break;
                    case Tt::inequal:               return this->ir.CreateICmp(CmpInst::ICMP_NE, lhs, rhs); break;
                    case Tt::greater_than_or_equal: return this->ir.CreateICmp(CmpInst::ICMP_SGE, lhs, rhs); break;
                    case Tt::greater_than:          return this->ir.CreateICmp(CmpInst::ICMP_SGT, lhs, rhs); break;
                    case Tt::less_than_or_equal:    return this->ir.CreateICmp(CmpInst::ICMP_SLE, lhs, rhs); break;
                    case Tt::less_than:             return this->ir.CreateICmp(CmpInst::ICMP_SLT, lhs, rhs); break;
                    case Tt::logical_and:           TODO;
                    case Tt::logical_or:            TODO;

                    default: UNREACHED;
                }

                break;
            }

            case SimpleTypeNode::Kind::unsigned_integer:
            {
                TODO;
            }

            case SimpleTypeNode::Kind::floatingpoint:
            {
                switch (bin_op->operator_type)
                {
                    case Tt::asterisk: return this->ir.CreateFMul(lhs, rhs); break;
                    case Tt::slash:    return this->ir.CreateFDiv(lhs, rhs); break;
                    case Tt::mod:      return this->ir.CreateFRem(lhs, rhs); break;
                    case Tt::plus:     return this->ir.CreateFAdd(lhs, rhs); break;
                    case Tt::minus:    return this->ir.CreateFSub(lhs, rhs); break;
                    case Tt::assign:   return this->ir.CreateStore(rhs, lhs);

                    // TODO: Read more about ordered and unordered floating point comparisons
                    case Tt::equal:                 return this->ir.CreateFCmp(CmpInst::FCMP_OEQ, lhs, rhs); break;
                    case Tt::inequal:               return this->ir.CreateFCmp(CmpInst::FCMP_ONE, lhs, rhs); break;
                    case Tt::greater_than_or_equal: return this->ir.CreateFCmp(CmpInst::FCMP_OGE, lhs, rhs); break;
                    case Tt::greater_than:          return this->ir.CreateFCmp(CmpInst::FCMP_OGT, lhs, rhs); break;
                    case Tt::less_than_or_equal:    return this->ir.CreateFCmp(CmpInst::FCMP_OLE, lhs, rhs); break;
                    case Tt::less_than:             return this->ir.CreateFCmp(CmpInst::FCMP_OLT, lhs, rhs); break;
                    default:                        UNREACHED;
                }
            }

            default: UNREACHED;
        }

        UNREACHED;
    }

    Value *generate_code(Node *node)
    {
        switch (node->kind)
        {
            case NodeKind::binary_operator:
            {
                auto bin_op = static_cast<BinaryOperatorNode *>(node);
                return generate_binary_operator_code(bin_op);
            }

            case NodeKind::block:
            {
                auto block = static_cast<BlockNode *>(node);

                for (auto statement : block->statements)
                {
                    this->generate_code(statement);
                }

                return nullptr;
            }

            case NodeKind::declaration:
            {
                auto decl = static_cast<DeclarationNode *>(node);

                switch (decl->init_expression->kind)
                {
                    case NodeKind::procedure:
                    {
                        assert(decl->is_global());  // TODO: Transform local procedure declarations to global ones
                        assert(this->function == nullptr);

                        auto procedure = node_cast<ProcedureNode, true>(decl->init_expression);

                        auto type          = this->convert_type(decl->init_expression->type);
                        auto function_type = cast<FunctionType>(type);
                        this->function     = Function::Create(
                            function_type,
                            GlobalValue::LinkageTypes::ExternalLinkage,
                            decl->identifier,
                            this->module.get());

                        auto i = 0;
                        for (auto &arg : this->function->args())
                        {
                            arg.setName(procedure->signature->arguments[i]->identifier);
                            ++i;
                        }

                        auto block =
                            BasicBlock::Create(this->llvm_context, std::format("{}_entry", decl->identifier), function);
                        this->ir.SetInsertPoint(block);  // TODO: Restore insert point when done?
                        this->generate_code(decl->init_expression);
                        // this->function->print(outs());
                        this->function = nullptr;

                        break;
                    }

                    default:
                    {
                        if (decl->is_global())
                        {
                            TODO;  // TODO: There are no global variables yet and they are not on the roadmap
                        }
                        else
                        {
                            auto value = this->generate_code(decl->init_expression);
                            auto local = decl->containing_block->find_local(decl->identifier);
                            assert(local != std::nullopt);

                            this->ir.CreateStore(value, local->location);
                        }

                        // TODO
                        break;
                    }
                }

                return nullptr;
            }

            case NodeKind::identifier:
            {
                auto ident = static_cast<IdentifierNode *>(node);

                auto type  = this->convert_type(node->type);
                auto local = ident->containing_block->find_local(ident->identifier).value();

                assert(local.location != nullptr);

                return this->ir.CreateLoad(type, local.location);
            }

            case NodeKind::if_statement:
            {
                TODO;
            }

            case NodeKind::literal:
            {
                auto literal = static_cast<LiteralNode *>(node);

                auto simple_type = node_cast<SimpleTypeNode, true>(literal->type);
                assert(simple_type != nullptr);

                switch (simple_type->type_kind)
                {
                    case SimpleTypeNode::Kind::boolean:
                    {
                        auto value = std::get<bool>(literal->value);
                        return ConstantInt::get(this->ir.getInt1Ty(), value);
                    }

                    case SimpleTypeNode::Kind::signed_integer:
                    case SimpleTypeNode::Kind::unsigned_integer:
                    {
                        auto value = std::get<uint64_t>(literal->value);
                        return ConstantInt::get(IntegerType::get(this->llvm_context, simple_type->size * 8), value);
                    }

                    case SimpleTypeNode::Kind::floatingpoint:
                    {
                        switch (simple_type->size)
                        {
                            case 4:
                            {
                                auto value = std::get<float>(literal->value);
                                return ConstantFP::get(this->llvm_context, APFloat{value});
                            }

                            case 8:
                            {
                                auto value = std::get<double>(literal->value);
                                return ConstantFP::get(this->ir.getDoubleTy(), value);
                            }

                            default: UNREACHED;
                        }
                    }

                    default: UNREACHED;
                }
            }

            case NodeKind::procedure:
            {
                auto procedure = static_cast<ProcedureNode *>(node);

                this->allocate_locals(procedure->body);
                this->generate_code(procedure->body);

                return nullptr;
            }

            case NodeKind::procedure_call:
            {
                UNREACHED;
            }

            case NodeKind::procedure_signature:
            {
                UNREACHED;
            }

            case NodeKind::return_statement:
            {
                auto retyrn = static_cast<ReturnNode *>(node);

                if (retyrn->expression->kind == NodeKind::nop)
                {
                    return this->ir.CreateRet(nullptr);
                }

                auto value = this->generate_code(retyrn->expression);
                return this->ir.CreateRet(value);
            }

            case NodeKind::program:
            {
                auto program = static_cast<ProgramNode *>(node);
                // generate_code(program->block);

                for (auto declaration : program->block->statements)
                {
                    assert(declaration->kind == NodeKind::declaration);
                    this->generate_code(declaration);
                }

                return nullptr;
            }

            case NodeKind::simple_type:  UNREACHED;
            case NodeKind::pointer_type: UNREACHED;
            case NodeKind::array_type:   UNREACHED;
            case NodeKind::struct_type:  UNREACHED;
            case NodeKind::nop:          UNREACHED;
        }

        UNREACHED;
    }
};

IrCompilationResult::~IrCompilationResult() = default;

IrCompilationResult compile_to_ir(struct Node *node)
{
    auto llvm_context = std::make_unique<LLVMContext>();

    IrCompiler ir_compiler{*llvm_context};
    ir_compiler.generate_code(node);

    return IrCompilationResult{std::move(llvm_context), std::move(ir_compiler.module)};
}
