#include "compile_ir.h"

#include "node.h"

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
    explicit IrCompiler(LLVMContext &llvm_context, Module &module)
        : llvm_context{llvm_context}
        , module{module}
        , ir{llvm_context}
    {
    }

    LLVMContext &llvm_context;
    Module &module;
    IRBuilder<llvm::NoFolder> ir;
    // IRBuilder<> ir;
    BasicBlock *current_break_target{};
    BasicBlock *current_continue_target{};

    void allocate_locals(BlockNode *block)
    {
        for (auto statement : block->statements)
        {
            if (auto decl = node_cast<DeclarationNode>(statement))
            {
                auto type         = this->convert_type(decl->init_expression->inferred_type());
                decl->named_value = this->ir.CreateAlloca(type, nullptr, decl->identifier);
                continue;
            }

            if (auto label = node_cast<LabelNode>(statement))
            {
                label->block = BasicBlock::Create(this->llvm_context, std::format("{}_label_begin", label->identifier));
                label->after = BasicBlock::Create(this->llvm_context, std::format("{}_label_end", label->identifier));
                continue;
            }

            if (auto block = node_cast<BlockNode>(statement))
            {
                allocate_locals(block);
                continue;
            }

            if (auto yf = node_cast<IfStatementNode>(statement))
            {
                allocate_locals(yf->then_block);

                if (yf->else_block != nullptr)
                {
                    allocate_locals(yf->else_block);
                }

                continue;
            }

            if (auto whyle = node_cast<WhileLoopNode>(statement))
            {
                allocate_locals(whyle->body);
                continue;
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
                    auto argument_type = this->convert_type(argument->init_expression->inferred_type());
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
        assert(Node::types_equal(bin_op->lhs->inferred_type(), bin_op->rhs->inferred_type()));

        auto is_store = bin_op->operator_kind == Tt::assign;

        auto lhs = this->generate_code(bin_op->lhs, is_store);
        ENSURE(lhs != nullptr);

        auto rhs = this->generate_code(bin_op->rhs);
        ENSURE(rhs != nullptr);

        if (bin_op->operator_kind == Tt::assign)
        {
            return this->ir.CreateStore(rhs, lhs);
        }

        assert(bin_op->inferred_type()->kind == NodeKind::basic_type);
        auto lhs_simple = node_cast<BasicTypeNode, true>(bin_op->lhs->inferred_type());
        auto rhs_simple = node_cast<BasicTypeNode, true>(bin_op->rhs->inferred_type());
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

                    default:                        UNREACHED;
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

                    default:                        UNREACHED;
                }

                break;
            }

            case BasicTypeNode::Kind::floatingpoint:
            {
                switch (bin_op->operator_kind)
                {
                    case Tt::asterisk:              return this->ir.CreateFMul(lhs, rhs, "fmul");
                    case Tt::slash:                 return this->ir.CreateFDiv(lhs, rhs, "fdiv");
                    case Tt::mod:                   return this->ir.CreateFRem(lhs, rhs, "frem");
                    case Tt::plus:                  return this->ir.CreateFAdd(lhs, rhs, "fadd");
                    case Tt::minus:                 return this->ir.CreateFSub(lhs, rhs, "fsub");

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
        for (auto statement : block->statements)
        {
            this->generate_code(statement);

            auto is_terminator = statement->kind == NodeKind::break_statement ||
                                 statement->kind == NodeKind::continue_statement ||
                                 statement->kind == NodeKind::return_statement;
            if (is_terminator)
            {
                // TODO: It is weird that this has to be done, but the unconditional 'br'
                // instruction seems to be ignored if there are other instructions following,
                // for example after a loop break
                break;
            }
        }

        return nullptr;
    }

    Value *generate_code(DeclarationNode *decl)
    {
        if (decl->is_global() && decl->named_value != nullptr)
        {
            // Already compiled out of order
            return nullptr;
        }

        if (decl->init_expression->kind == NodeKind::procedure)
        {
            // TODO: Transform local procedure declarations to global ones
            assert(decl->is_global());
            // assert(this->ir.GetInsertBlock() == nullptr);

            auto procedure = node_cast<ProcedureNode, true>(decl->init_expression);

            auto type          = this->convert_type(decl->init_expression->inferred_type());
            auto function_type = cast<FunctionType>(type);
            auto function      = Function::Create(
                function_type,
                GlobalValue::LinkageTypes::ExternalLinkage,
                decl->identifier,
                this->module);
            decl->named_value =
                function;  // NOTE: Must set the named_value before compiling the body to allow recursion

            auto i = 0;
            for (auto &arg : function->args())
            {
                arg.setName(procedure->signature->arguments[i]->identifier);
                ++i;
            }

            if (procedure->is_external == false)
            {
                auto block = BasicBlock::Create(this->llvm_context, "entry", function);
                this->ir.SetInsertPoint(block);  // TODO: Restore insert point when done?
                this->generate_code(decl->init_expression);
            }

            return nullptr;
        }

        // TODO: There are no global variables yet and they are not on the roadmap
        ENSURE(decl->is_global() == false);

        // Declaration assignment to init expresion
        auto value = this->generate_code(decl->init_expression);
        this->ir.CreateStore(value, decl->named_value);

        return nullptr;
    }

    Value *generate_code(IdentifierNode *ident, bool is_store = false)
    {
        auto type = this->convert_type(ident->inferred_type());

        assert(ident->declaration->named_value != nullptr);

        if (is_store)
        {
            return ident->declaration->named_value;
        }

        if (isa<Argument>(ident->declaration->named_value))
        {
            return ident->declaration->named_value;
        }

        return this->ir.CreateLoad(type, ident->declaration->named_value, "load");
    }

    Value *generate_code(IfStatementNode *yf)
    {
        auto condition = this->generate_code(yf->condition);

        auto function = this->ir.GetInsertBlock()->getParent();

        auto if_cond = this->ir.CreateICmpNE(condition, ConstantInt::get(this->ir.getInt1Ty(), 0), "if_cond");

        auto then_block = BasicBlock::Create(this->llvm_context, "if_then", function);
        auto else_block = BasicBlock::Create(this->llvm_context, "if_else");
        auto done_block = BasicBlock::Create(this->llvm_context, "if_end");

        this->ir.CreateCondBr(if_cond, then_block, else_block);

        this->ir.SetInsertPoint(then_block);
        this->generate_code(yf->then_block);
        if (this->ir.GetInsertBlock()->getTerminator() == nullptr)
        {
            this->ir.CreateBr(done_block);
        }
        then_block = this->ir.GetInsertBlock();

        function->insert(function->end(), else_block);
        this->ir.SetInsertPoint(else_block);
        if (yf->else_block != nullptr)
        {
            this->generate_code(yf->else_block);
        }
        if (this->ir.GetInsertBlock()->getTerminator() == nullptr)
        {
            this->ir.CreateBr(done_block);
        }
        else_block = this->ir.GetInsertBlock();

        function->insert(function->end(), done_block);
        this->ir.SetInsertPoint(done_block);

        // auto phi = this->ir.CreatePHI();   TODO: Continue here later for ternary

        return nullptr;
    }

    Value *generate_code(WhileLoopNode *whyle)
    {
        auto function = this->ir.GetInsertBlock()->getParent();

        auto head_block = BasicBlock::Create(this->llvm_context, "while_head", function);
        auto body_block = BasicBlock::Create(this->llvm_context, "while_body", function);
        auto done_block = BasicBlock::Create(this->llvm_context, "while_done", function);

        SET_TEMPORARILY(this->current_break_target, done_block);
        SET_TEMPORARILY(this->current_continue_target, head_block);

        assert(this->ir.GetInsertBlock()->getTerminator() == nullptr);
        this->ir.CreateBr(head_block);

        this->ir.SetInsertPoint(head_block);

        auto condition  = this->generate_code(whyle->condition);
        auto while_cond = this->ir.CreateICmpNE(condition, ConstantInt::get(this->ir.getInt1Ty(), 0), "while_cond");
        this->ir.CreateCondBr(while_cond, body_block, done_block);

        this->ir.SetInsertPoint(body_block);
        this->generate_code(whyle->body);

        if (this->ir.GetInsertBlock()->getTerminator() == nullptr)
        {
            this->ir.CreateBr(head_block);
        }

        // this->current_function->insert(this->current_function->end(), done_block);
        this->ir.SetInsertPoint(done_block);

        return nullptr;
    }

    Value *generate_code(BreakStatementNode *node)
    {
        assert(this->current_break_target != nullptr);

        this->ir.CreateBr(this->current_break_target);

        return nullptr;
    }

    Value *generate_code(ContinueStatementNode *node)
    {
        assert(this->current_continue_target != nullptr);

        this->ir.CreateBr(this->current_continue_target);

        return nullptr;
    }

    Value *generate_code(LiteralNode *literal)
    {
        auto basic_type = node_cast<BasicTypeNode>(literal->inferred_type());

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
    }

    Value *generate_code(ProcedureNode *proc)
    {
        assert(proc->is_external == false);

        for (auto arg : proc->signature->arguments)
        {
            for (auto &the_arg : this->ir.GetInsertBlock()->getParent()->args())
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
        auto ident = static_cast<IdentifierNode *>(call->procedure);

        if (ident->declaration->named_value == nullptr)
        {
            // Compile on demand for out of order declarations
            IrCompiler ir_compiler{this->llvm_context, this->module};
            ir_compiler.generate_code(ident->declaration);
            assert(ident->declaration->named_value != nullptr);
        }

        assert(ident->declaration->init_expression->kind == NodeKind::procedure);
        auto proc = static_cast<ProcedureNode *>(ident->declaration->init_expression);

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
                this->ir.CreateCall(type, ident->declaration->named_value, arguments);
                return nullptr;
            }
        }

        return this->ir
            .CreateCall(type, ident->declaration->named_value, arguments, std::format("call_{}", ident->identifier));
    }

    Value *generate_code(ReturnStatementNode *retyrn)
    {
        if (retyrn->expression->kind == NodeKind::nop)
        {
            return this->ir.CreateRet(nullptr);
        }

        auto value = this->generate_code(retyrn->expression);
        this->ir.CreateRet(value);

        return nullptr;
    }

    Value *generate_code(LabelNode *label)
    {
        TODO;

        // this->current_function->insert(this->current_function->end(), label->block);
        // this->current_function->insert(this->current_function->end(), label->after);

        // this->ir.SetInsertPoint(label->block);
        // this->ir.CreateBr(label->after);
        // this->ir.SetInsertPoint(label->block->begin());

        // return nullptr;
    }

    Value *generate_code(GotoStatementNode *gotoo)
    {
        TODO;

        // auto decl = this->current_procedure->body->find_declaration(gotoo->label_identifier);
        // assert(decl != nullptr);

        // auto label = node_cast<LabelNode, true>(decl->init_expression);
        // assert(label->block != nullptr);

        // this->ir.CreateBr(label->block);

        // return nullptr;
    }

    Value *generate_code(TypeCastNode *cast)
    {
        auto dest_basic_type = node_cast<BasicTypeNode, true>(cast->inferred_type());
        auto src_basic_type  = node_cast<BasicTypeNode, true>(cast->expression->inferred_type());

        // assert(dest_basic_type->type_kind == BasicTypeNode::Kind::floatingpoint);

        auto expression_value = this->generate_code(cast->expression);
        auto dest_type        = this->convert_type(dest_basic_type);

        // assert(dest_type->isFloatingPointTy());

        switch (src_basic_type->type_kind)
        {
            case BasicTypeNode::Kind::boolean: UNREACHED;

            case BasicTypeNode::Kind::signed_integer:
            {
                switch (dest_basic_type->type_kind)
                {
                    case BasicTypeNode::Kind::signed_integer:
                    {
                        assert(src_basic_type->size != dest_basic_type->size);
                        return this->ir.CreateIntCast(expression_value, dest_type, true, "intcast");
                    }

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
            case NodeKind::binary_operator:     return this->generate_code(static_cast<BinaryOperatorNode *>(node));
            case NodeKind::block:               return this->generate_code(static_cast<BlockNode *>(node));
            case NodeKind::declaration:         return this->generate_code(static_cast<DeclarationNode *>(node));
            case NodeKind::identifier:          return this->generate_code(static_cast<IdentifierNode *>(node), is_store);
            case NodeKind::if_statement:        return this->generate_code(static_cast<IfStatementNode *>(node));
            case NodeKind::while_loop:          return this->generate_code(static_cast<WhileLoopNode *>(node));
            case NodeKind::break_statement:     return this->generate_code(static_cast<BreakStatementNode *>(node));
            case NodeKind::continue_statement:  return this->generate_code(static_cast<ContinueStatementNode *>(node));
            case NodeKind::literal:             return this->generate_code(static_cast<LiteralNode *>(node));
            case NodeKind::module:              return this->generate_code(static_cast<ModuleNode *>(node)->block);
            case NodeKind::procedure:           return this->generate_code(static_cast<ProcedureNode *>(node));
            case NodeKind::procedure_call:      return this->generate_code(static_cast<ProcedureCallNode *>(node));
            case NodeKind::return_statement:    return this->generate_code(static_cast<ReturnStatementNode *>(node));
            case NodeKind::goto_statement:      return this->generate_code(static_cast<GotoStatementNode *>(node));
            case NodeKind::label:               return this->generate_code(static_cast<LabelNode *>(node));
            case NodeKind::type_cast:           return this->generate_code(static_cast<TypeCastNode *>(node));

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
    auto module       = std::make_unique<Module>("inmemory_temp_module", *llvm_context);

    IrCompiler ir_compiler{*llvm_context, *module};
    ir_compiler.generate_code(node);

    return IrCompilationResult{std::move(llvm_context), std::move(module)};
}
