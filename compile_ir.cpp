#include "compile_ir.h"
#include "typecheck.h"

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/NoFolder.h>
#include <llvm/IR/Value.h>

// https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl03.html
// https://llvm.org/docs/ProgrammersManual.html
// https://github.com/mukul-rathi/bolt/blob/master/src/llvm-backend/deserialise_ir/expr_ir.cc

using namespace llvm;

static LLVMContext llvm_context;

struct IrCompiler
{
    explicit IrCompiler()
        : module{"JanguageTestModule", llvm_context}
        , ir{llvm_context}
    {
    }

    Module module;
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
                    UNREACHED;
                }

                if (std::holds_alternative<uint64_t>(length_literal->value) == false)
                {
                    UNREACHED;
                }

                auto length = std::get<uint64_t>(length_literal->value);

                return ArrayType::get(element_type, length);
            }

            case NodeKind::struct_type:
            {
                auto strukt = static_cast<const StructTypeNode *>(node);

                UNREACHED;
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
            UNREACHED;  // TODO
            return nullptr;
        }

        auto rhs = this->generate_code(bin_op->rhs);
        if (rhs == nullptr)
        {
            UNREACHED;  // TODO
            return nullptr;
        }

        assert(bin_op->type->kind == NodeKind::simple_type);
        auto type = node_cast<SimpleTypeNode, true>(bin_op->type);

        Value *result{};

        switch (type->type_kind)
        {
            case SimpleTypeNode::Kind::signed_integer:
            {
                switch (bin_op->operator_type)
                {
                    case Tt::asterisk: result = this->ir.CreateMul(lhs, rhs); break;
                    case Tt::slash:    result = this->ir.CreateSDiv(lhs, rhs); break;
                    case Tt::mod:      result = this->ir.CreateSRem(lhs, rhs); break;
                    case Tt::plus:     result = this->ir.CreateAdd(lhs, rhs); break;
                    case Tt::minus:    result = this->ir.CreateSub(lhs, rhs); break;
                    case Tt::assign:
                        result = this->ir.CreateStore(rhs, lhs);
                        break;  // TODO: Pointer to left hand side?
                    case Tt::bit_and:               result = this->ir.CreateAnd(lhs, rhs); break;
                    case Tt::bit_or:                result = this->ir.CreateOr(lhs, rhs); break;
                    case Tt::bit_xor:               result = this->ir.CreateXor(lhs, rhs); break;
                    case Tt::left_shift:            result = this->ir.CreateShl(lhs, rhs); break;
                    case Tt::right_shift:           result = this->ir.CreateLShr(lhs, rhs); break;  // TODO: LShr or AShr?
                    case Tt::equal:                 result = this->ir.CreateICmp(CmpInst::ICMP_EQ, lhs, rhs); break;
                    case Tt::inequal:               result = this->ir.CreateICmp(CmpInst::ICMP_NE, lhs, rhs); break;
                    case Tt::greater_than_or_equal: result = this->ir.CreateICmp(CmpInst::ICMP_SGE, lhs, rhs); break;
                    case Tt::greater_than:          result = this->ir.CreateICmp(CmpInst::ICMP_SGT, lhs, rhs); break;
                    case Tt::less_than_or_equal:    result = this->ir.CreateICmp(CmpInst::ICMP_SLE, lhs, rhs); break;
                    case Tt::less_than:             result = this->ir.CreateICmp(CmpInst::ICMP_SLT, lhs, rhs); break;
                    case Tt::logical_and:           UNREACHED;  // TODO
                    case Tt::logical_or:            UNREACHED;  // TODO
                    default:                        UNREACHED;
                }
            }

            case SimpleTypeNode::Kind::unsigned_integer:
            case SimpleTypeNode::Kind::floatingpoint:
            case SimpleTypeNode::Kind::type:             break;

            default: UNREACHED;
        }

        return result;
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

                if (decl->init_expression == nullptr)
                {
                    UNREACHED;
                }

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
                            GlobalValue::LinkageTypes::InternalLinkage,
                            decl->identifier,
                            &this->module);

                        auto i = 0;
                        for (auto &arg : this->function->args())
                        {
                            arg.setName(procedure->signature->arguments[i]->identifier);
                            ++i;
                        }

                        auto block =
                            BasicBlock::Create(llvm_context, std::format("{}_entry", decl->identifier), function);
                        this->ir.SetInsertPoint(block);  // TODO: Restore insert point when done?
                        this->generate_code(decl->init_expression);
                        this->function->print(outs());
                        this->function = nullptr;

                        break;
                    }

                    default:
                    {
                        if (decl->is_global())
                        {
                            UNREACHED;  // TODO: There are not global variables yet and they are not on the roadmap
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

                auto type  = convert_type(node->type);
                auto local = ident->containing_block->find_local(ident->identifier);
                if (local == std::nullopt)
                {
                    UNREACHED;  // TODO: Error message
                }

                assert(local->location != nullptr);

                return this->ir.CreateLoad(type, local->location);
            }

            case NodeKind::if_statement: UNREACHED;
            case NodeKind::literal:
            {
                auto literal = static_cast<LiteralNode *>(node);

                auto simple_type = node_cast<SimpleTypeNode, true>(literal->type);
                assert(simple_type != nullptr);

                switch (simple_type->type_kind)
                {
                    case SimpleTypeNode::Kind::invalid: UNREACHED;
                    case SimpleTypeNode::Kind::voyd:    UNREACHED;
                    case SimpleTypeNode::Kind::boolean: UNREACHED;

                    case SimpleTypeNode::Kind::signed_integer:
                    case SimpleTypeNode::Kind::unsigned_integer:
                    {
                        auto value = std::get<uint64_t>(literal->value);
                        return ConstantInt::get(IntegerType::get(llvm_context, simple_type->size * 8), value);
                    }

                    case SimpleTypeNode::Kind::floatingpoint: UNREACHED;
                    case SimpleTypeNode::Kind::type:          UNREACHED;
                }
            }

            case NodeKind::procedure:
            {
                auto procedure = static_cast<ProcedureNode *>(node);

                this->allocate_locals(procedure->body);
                this->generate_code(procedure->body);

                return nullptr;
            }

            case NodeKind::procedure_call: UNREACHED;

            case NodeKind::procedure_signature: UNREACHED;

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

std::string compile_to_ir(struct Node *node)
{
    IrCompiler ir_compiler{};
    auto value = ir_compiler.generate_code(node);
    // value->print(outs());

    return "TODO";
}
