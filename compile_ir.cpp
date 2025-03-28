#include "compile_ir.h"

#include "typecheck.h"

#include <llvm/ADT/StringRef.h>
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
        , module{std::make_unique<Module>("inmemory_temp_module", llvm_context)}
        , ir{llvm_context}
    {
    }

    LLVMContext &llvm_context;
    std::unique_ptr<Module> module;
    Function *current_function{};
    BlockNode *current_block{};
    IRBuilder<llvm::NoFolder> ir;
    // IRBuilder<> ir;

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
                auto type         = this->convert_type(decl->init_expression->type);
                decl->named_value = this->ir.CreateAlloca(type, nullptr, decl->identifier);
            }
        }
    }

    Type *convert_type(const Node *node)
    {
        assert(node != nullptr);

        switch (node->kind)
        {
            case NodeKind::basic_type:
            {
                auto basic = static_cast<const BasicTypeNode *>(node);

                switch (basic->type_kind)
                {
                    case BasicTypeNode::Kind::voyd:             return this->ir.getVoidTy();
                    case BasicTypeNode::Kind::boolean:          return this->ir.getInt1Ty();
                    case BasicTypeNode::Kind::signed_integer:   return this->ir.getIntNTy(basic->size * 8);
                    case BasicTypeNode::Kind::unsigned_integer: return this->ir.getIntNTy(basic->size * 8);

                    case BasicTypeNode::Kind::floatingpoint:
                    {
                        switch (basic->size)
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

                auto length_literal = node_cast<LiteralNode>(array->length);
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

                auto function_type = FunctionType::get(return_type, argument_types, signature->is_vararg);

                return function_type;
            }

            default: UNREACHED;
        }
    }

    Value *generate_code(BinaryOperatorNode *bin_op)
    {
        assert(types_equal(bin_op->lhs->type, bin_op->rhs->type));

        auto is_store = bin_op->operator_kind == Tt::assign;

        auto lhs = this->generate_code(bin_op->lhs, is_store);
        ENSURE(lhs != nullptr);

        auto rhs = this->generate_code(bin_op->rhs);
        ENSURE(rhs != nullptr);

        if (bin_op->operator_kind == Tt::assign)
        {
            return this->ir.CreateStore(rhs, lhs);
        }

        assert(bin_op->type->kind == NodeKind::basic_type);
        auto lhs_simple = node_cast<BasicTypeNode, true>(bin_op->lhs->type);
        auto rhs_simple = node_cast<BasicTypeNode, true>(bin_op->rhs->type);
        assert(lhs_simple->type_kind == rhs_simple->type_kind);

        switch (lhs_simple->type_kind)
        {
            case BasicTypeNode::Kind::boolean:
            case BasicTypeNode::Kind::signed_integer:
            {
                switch (bin_op->operator_kind)
                {
                    case Tt::asterisk:              return this->ir.CreateMul(lhs, rhs, "mul");
                    case Tt::slash:                 return this->ir.CreateSDiv(lhs, rhs, "div");
                    case Tt::mod:                   return this->ir.CreateSRem(lhs, rhs, "mod");
                    case Tt::plus:                  return this->ir.CreateAdd(lhs, rhs, "add");
                    case Tt::minus:                 return this->ir.CreateSub(lhs, rhs, "sub");
                    case Tt::bit_and:               return this->ir.CreateAnd(lhs, rhs, "and");
                    case Tt::bit_or:                return this->ir.CreateOr(lhs, rhs, "or");
                    case Tt::bit_xor:               return this->ir.CreateXor(lhs, rhs, "xor");
                    case Tt::left_shift:            return this->ir.CreateShl(lhs, rhs, "shl");
                    case Tt::right_shift:           return this->ir.CreateLShr(lhs, rhs, "lshr");
                    case Tt::equal:                 return this->ir.CreateICmp(CmpInst::ICMP_EQ, lhs, rhs, "ieq");
                    case Tt::inequal:               return this->ir.CreateICmp(CmpInst::ICMP_NE, lhs, rhs, "ine");
                    case Tt::greater_than_or_equal: return this->ir.CreateICmp(CmpInst::ICMP_SGE, lhs, rhs, "isge");
                    case Tt::greater_than:          return this->ir.CreateICmp(CmpInst::ICMP_SGT, lhs, rhs, "isgt");
                    case Tt::less_than_or_equal:    return this->ir.CreateICmp(CmpInst::ICMP_SLE, lhs, rhs, "isle");
                    case Tt::less_than:             return this->ir.CreateICmp(CmpInst::ICMP_SLT, lhs, rhs, "islt");
                    case Tt::logical_and:           TODO;
                    case Tt::logical_or:            TODO;

                    default: UNREACHED;
                }

                break;
            }

            case BasicTypeNode::Kind::unsigned_integer:
            {
                switch (bin_op->operator_kind)
                {
                    case Tt::asterisk:              return this->ir.CreateMul(lhs, rhs, "mul");
                    case Tt::slash:                 return this->ir.CreateUDiv(lhs, rhs, "udiv");
                    case Tt::mod:                   return this->ir.CreateURem(lhs, rhs, "urem");
                    case Tt::plus:                  return this->ir.CreateAdd(lhs, rhs, "add");
                    case Tt::minus:                 return this->ir.CreateSub(lhs, rhs, "sub");
                    case Tt::bit_and:               return this->ir.CreateAnd(lhs, rhs, "and");
                    case Tt::bit_or:                return this->ir.CreateOr(lhs, rhs, "or");
                    case Tt::bit_xor:               return this->ir.CreateXor(lhs, rhs, "xor");
                    case Tt::left_shift:            return this->ir.CreateShl(lhs, rhs, "shl");
                    case Tt::right_shift:           return this->ir.CreateLShr(lhs, rhs, "lshr");
                    case Tt::equal:                 return this->ir.CreateICmp(CmpInst::ICMP_EQ, lhs, rhs, "ieq");
                    case Tt::inequal:               return this->ir.CreateICmp(CmpInst::ICMP_NE, lhs, rhs, "ine");
                    case Tt::greater_than_or_equal: return this->ir.CreateICmp(CmpInst::ICMP_UGE, lhs, rhs, "iuge");
                    case Tt::greater_than:          return this->ir.CreateICmp(CmpInst::ICMP_UGT, lhs, rhs, "iugt");
                    case Tt::less_than_or_equal:    return this->ir.CreateICmp(CmpInst::ICMP_ULE, lhs, rhs, "iule");
                    case Tt::less_than:             return this->ir.CreateICmp(CmpInst::ICMP_ULT, lhs, rhs, "iult");
                    case Tt::logical_and:           TODO;  // TODO: This should be handled by desugaring
                    case Tt::logical_or:            TODO;

                    default: UNREACHED;
                }

                break;
            }

            case BasicTypeNode::Kind::floatingpoint:
            {
                switch (bin_op->operator_kind)
                {
                    case Tt::asterisk: return this->ir.CreateFMul(lhs, rhs, "fmul");
                    case Tt::slash:    return this->ir.CreateFDiv(lhs, rhs, "fdiv");
                    case Tt::mod:      return this->ir.CreateFRem(lhs, rhs, "frem");
                    case Tt::plus:     return this->ir.CreateFAdd(lhs, rhs, "fadd");
                    case Tt::minus:    return this->ir.CreateFSub(lhs, rhs, "fsub");

                    // TODO: Read more about ordered and unordered floating point comparisons
                    case Tt::equal:                 return this->ir.CreateFCmp(CmpInst::FCMP_OEQ, lhs, rhs, "foeq");
                    case Tt::inequal:               return this->ir.CreateFCmp(CmpInst::FCMP_ONE, lhs, rhs, "fone");
                    case Tt::greater_than_or_equal: return this->ir.CreateFCmp(CmpInst::FCMP_OGE, lhs, rhs, "foge");
                    case Tt::greater_than:          return this->ir.CreateFCmp(CmpInst::FCMP_OGT, lhs, rhs, "fogt");
                    case Tt::less_than_or_equal:    return this->ir.CreateFCmp(CmpInst::FCMP_OLE, lhs, rhs, "fole");
                    case Tt::less_than:             return this->ir.CreateFCmp(CmpInst::FCMP_OLT, lhs, rhs, "folt");
                    default:                        UNREACHED;
                }
            }

            default: UNREACHED;
        }

        UNREACHED;
    }

    Value *generate_code(BlockNode *block)
    {
        auto old_block      = this->current_block;
        this->current_block = block;
        defer
        {
            this->current_block = old_block;
        };

        for (auto statement : block->statements)
        {
            this->generate_code(statement);
        }

        return nullptr;
    }

    Value *generate_code(DeclarationNode *decl)
    {
        if (decl->init_expression->kind == NodeKind::procedure)
        {
            // TODO: Transform local procedure declarations to global ones
            assert(this->current_block->is_global());
            assert(this->current_function == nullptr);

            auto procedure = node_cast<ProcedureNode, true>(decl->init_expression);

            auto type              = this->convert_type(decl->init_expression->type);
            auto function_type     = cast<FunctionType>(type);
            this->current_function = Function::Create(
                function_type,
                GlobalValue::LinkageTypes::ExternalLinkage,
                decl->identifier,
                this->module.get());
            decl->named_value =
                this->current_function;  // NOTE: Must set the named_value before compiling the body to allow recursion

            auto i = 0;
            for (auto &arg : this->current_function->args())
            {
                arg.setName(procedure->signature->arguments[i]->identifier);
                ++i;
            }

            if (procedure->is_external == false)
            {
                auto block = BasicBlock::Create(this->llvm_context, "entry", this->current_function);
                this->ir.SetInsertPoint(block);  // TODO: Restore insert point when done?
                this->generate_code(decl->init_expression);
            }

            this->current_function = nullptr;

            return nullptr;
        }

        // TODO: There are no global variables yet and they are not on the roadmap
        ENSURE(this->current_block->is_global() == false);

        // Declaration assignment to init expresion
        auto value = this->generate_code(decl->init_expression);
        this->ir.CreateStore(value, decl->named_value);

        return nullptr;
    }

    Value *generate_code(IdentifierNode *ident, bool is_store = false)
    {
        auto type = this->convert_type(ident->type);
        auto decl = this->current_block->find_declaration(ident->identifier);

        assert(decl->named_value != nullptr);

        if (is_store)
        {
            return decl->named_value;
        }

        if (isa<Argument>(decl->named_value))
        {
            return decl->named_value;
        }

        return this->ir.CreateLoad(type, decl->named_value, "load");
    }

    Value *generate_code(IfNode *yf)
    {
        auto condition = this->generate_code(yf->condition);

        auto if_cond = this->ir.CreateICmpNE(condition, ConstantInt::get(this->ir.getInt1Ty(), 0), "ifcond");

        auto then_block  = BasicBlock::Create(this->llvm_context, "then", this->current_function);
        auto else_block  = BasicBlock::Create(this->llvm_context, "else");
        auto merge_block = BasicBlock::Create(this->llvm_context, "merge");

        this->ir.CreateCondBr(if_cond, then_block, else_block);

        this->ir.SetInsertPoint(then_block);
        this->generate_code(yf->then_block);
        this->ir.CreateBr(merge_block);
        then_block = this->ir.GetInsertBlock();

        this->current_function->insert(this->current_function->end(), else_block);
        this->ir.SetInsertPoint(else_block);
        if (yf->else_block != nullptr)
        {
            this->generate_code(yf->else_block);
        }
        this->ir.CreateBr(merge_block);
        else_block = this->ir.GetInsertBlock();

        this->current_function->insert(this->current_function->end(), merge_block);
        this->ir.SetInsertPoint(merge_block);

        // auto phi = this->ir.CreatePHI();   TODO: Continue here later for ternary

        return nullptr;
    }

    Value *generate_code(LiteralNode *literal)
    {
        auto basic_type = node_cast<BasicTypeNode>(literal->type);

        if (std::holds_alternative<uint64_t>(literal->value))
        {
            assert(basic_type != nullptr);
            auto value = std::get<uint64_t>(literal->value);
            return ConstantInt::get(IntegerType::get(this->llvm_context, basic_type->size * 8), value);
        }

        if (std::holds_alternative<float>(literal->value))
        {
            auto value = std::get<float>(literal->value);
            return ConstantFP::get(this->llvm_context, APFloat{value});
        }

        if (std::holds_alternative<double>(literal->value))
        {
            auto value = std::get<double>(literal->value);
            return ConstantFP::get(this->ir.getDoubleTy(), value);
        }

        if (std::holds_alternative<bool>(literal->value))
        {
            auto value = std::get<bool>(literal->value);
            return ConstantInt::get(this->ir.getInt1Ty(), value);
        }

        if (std::holds_alternative<std::string>(literal->value))
        {
            auto value = std::get<std::string>(literal->value);
            return this->ir.CreateGlobalString(value, "string");
        }

        UNREACHED;

#if 0
        auto basic_type = node_cast<BasicTypeNode, true>(literal->type);
        assert(basic_type != nullptr);  // TODO: Struct types will appear here

        switch (basic_type->type_kind)
        {
            case BasicTypeNode::Kind::boolean:
            {
                auto value = std::get<bool>(literal->value);
                return ConstantInt::get(this->ir.getInt1Ty(), value);
            }

            case BasicTypeNode::Kind::signed_integer:
            case BasicTypeNode::Kind::unsigned_integer:
            {
                auto value = std::get<uint64_t>(literal->value);
                return ConstantInt::get(IntegerType::get(this->llvm_context, basic_type->size * 8), value);
            }

            case BasicTypeNode::Kind::floatingpoint:
            {
                switch (basic_type->size)
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

            default: TODO;
        }
#endif
    }

    Value *generate_code(ProcedureNode *proc)
    {
        assert(proc->is_external == false);

        for (auto arg : proc->signature->arguments)
        {
            for (auto &the_arg : this->current_function->args())
            {
                if (StringRef{arg->identifier} == the_arg.getName())
                {
                    arg->named_value = &the_arg;
                }
            }

            assert(arg->named_value != nullptr);
        }

        this->allocate_locals(proc->body);
        this->generate_code(proc->body);

        return nullptr;
    }

    Value *generate_code(ProcedureCallNode *call)
    {
        assert(call->procedure->kind == NodeKind::identifier);  // TODO: Function pointer calling
        auto ident     = static_cast<IdentifierNode *>(call->procedure);
        auto proc_decl = this->current_block->find_declaration(ident->identifier);
        assert(
            proc_decl->named_value !=
            nullptr);  // TODO: Implement on-demand node compilation with something like ensure_compiled(Node *node)
        assert(proc_decl->init_expression->kind == NodeKind::procedure);
        auto proc = static_cast<ProcedureNode *>(proc_decl->init_expression);

        auto type = cast<FunctionType>(this->convert_type(proc->signature));

        std::vector<Value *> arguments{};
        for (auto argument : call->arguments)
        {
            auto argument_value = this->generate_code(argument);
            arguments.push_back(argument_value);
        }

        if (proc->signature->return_type->kind == NodeKind::basic_type)
        {
            auto return_basic = node_cast<BasicTypeNode>(proc->signature->return_type);
            if (return_basic->type_kind == BasicTypeNode::Kind::voyd)
            {
                this->ir.CreateCall(type, proc_decl->named_value, arguments);
                return nullptr;
            }
        }

        return this->ir.CreateCall(type, proc_decl->named_value, arguments, std::format("call_{}", ident->identifier));
    }

    Value *generate_code(ReturnNode *retyrn)
    {
        if (retyrn->expression->kind == NodeKind::nop)
        {
            return this->ir.CreateRet(nullptr);
        }

        auto value = this->generate_code(retyrn->expression);
        this->ir.CreateRet(value);

        return nullptr;
    }

    Value *generate_code(TypeCastNode *cast)
    {
        auto dest_basic_type = node_cast<BasicTypeNode, true>(cast->type);
        auto src_basic_type  = node_cast<BasicTypeNode, true>(cast->expression->type);

        assert(dest_basic_type->type_kind == BasicTypeNode::Kind::floatingpoint);

        auto expression_value = this->generate_code(cast->expression);
        auto dest_type        = this->convert_type(dest_basic_type);

        assert(dest_type->isFloatingPointTy());

        switch (src_basic_type->type_kind)
        {
            case BasicTypeNode::Kind::boolean: UNREACHED;

            case BasicTypeNode::Kind::signed_integer:
            {
                switch (dest_basic_type->type_kind)
                {
                    case BasicTypeNode::Kind::floatingpoint:
                    {
                        return this->ir.CreateSIToFP(expression_value, dest_type, "itof");
                    }

                    default: TODO;
                }
            }

            case BasicTypeNode::Kind::unsigned_integer:
            {
                switch (dest_basic_type->type_kind)
                {
                    case BasicTypeNode::Kind::floatingpoint:
                    {
                        return this->ir.CreateUIToFP(expression_value, dest_type, "utof");
                    }

                    default: TODO;
                }
            }

            case BasicTypeNode::Kind::floatingpoint:
            {
                switch (dest_basic_type->type_kind)
                {
                    case BasicTypeNode::Kind::floatingpoint:
                    {
                        switch (src_basic_type->size)
                        {
                            case 4:
                            {
                                assert(dest_basic_type->size == 8);
                                return this->ir.CreateFPExt(expression_value, dest_type, "ftod");
                            }

                            case 8:
                            {
                                assert(dest_basic_type->size == 4);
                                return this->ir.CreateFPTrunc(expression_value, dest_type, "dtof");
                            }

                            default: UNREACHED;
                        }
                    }

                    default: TODO;
                }
            }

            default: UNREACHED;
        }
    }

    Value *generate_code(Node *node, bool is_store = false)
    {
        switch (node->kind)
        {
            case NodeKind::binary_operator:  return this->generate_code(static_cast<BinaryOperatorNode *>(node));
            case NodeKind::block:            return this->generate_code(static_cast<BlockNode *>(node));
            case NodeKind::declaration:      return this->generate_code(static_cast<DeclarationNode *>(node));
            case NodeKind::identifier:       return this->generate_code(static_cast<IdentifierNode *>(node), is_store);
            case NodeKind::if_statement:     return this->generate_code(static_cast<IfNode *>(node));
            case NodeKind::literal:          return this->generate_code(static_cast<LiteralNode *>(node));
            case NodeKind::module:           return this->generate_code(static_cast<ModuleNode *>(node)->block);
            case NodeKind::procedure:        return this->generate_code(static_cast<ProcedureNode *>(node));
            case NodeKind::procedure_call:   return this->generate_code(static_cast<ProcedureCallNode *>(node));
            case NodeKind::return_statement: return this->generate_code(static_cast<ReturnNode *>(node));
            case NodeKind::type_cast:        return this->generate_code(static_cast<TypeCastNode *>(node));

            case NodeKind::array_type:          UNREACHED;
            case NodeKind::nop:                 UNREACHED;
            case NodeKind::pointer_type:        UNREACHED;
            case NodeKind::procedure_signature: UNREACHED;
            case NodeKind::basic_type:          UNREACHED;
            case NodeKind::struct_type:         UNREACHED;
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
