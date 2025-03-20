#include "compile_ir.h"
#include "typecheck.h"

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

Type *make_type(const Node *node)
{
    assert(node != nullptr);

    switch (node->kind)
    {
        case NodeKind::simple_type:
        {
            auto simple = static_cast<const SimpleTypeNode *>(node);

            switch (simple->type_kind)
            {
                case SimpleTypeNode::Kind::voyd:             return ir_builder.getVoidTy();
                case SimpleTypeNode::Kind::boolean:          return ir_builder.getInt1Ty();
                case SimpleTypeNode::Kind::signed_integer:   return ir_builder.getIntNTy(simple->size * 8);
                case SimpleTypeNode::Kind::unsigned_integer: return ir_builder.getIntNTy(simple->size * 8);

                case SimpleTypeNode::Kind::floatingpoint:
                {
                    switch (simple->size)
                    {
                        case 4:  return ir_builder.getFloatTy();
                        case 8:  return ir_builder.getDoubleTy();
                        default: UNREACHED;
                    }
                }

                default: UNREACHED;
            }
        }

        case NodeKind::pointer_type:
        {
            auto pointer = static_cast<const PointerTypeNode *>(node);
            return ir_builder.getPtrTy();
        }

        case NodeKind::array_type:
        {
            auto array = static_cast<const ArrayTypeNode *>(node);

            auto element_type = make_type(array->element_type);

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

            auto return_type = make_type(signature->return_type);
            std::vector<Type *> argument_types;

            for (auto argument : signature->arguments)
            {
                auto argument_type = make_type(argument->init_expression->type);
                argument_types.push_back(argument_type);
            }

            auto function_type = FunctionType::get(return_type, argument_types, false);

            return function_type;
        }

        default: UNREACHED;
    }
}

Value *generate_code(Node *node);

Value *generate_binary_operator_code(BinaryOperatorNode *bin_op)
{
    auto lhs = generate_code(bin_op->lhs);
    if (lhs == nullptr)
    {
        UNREACHED;  // TODO
        return nullptr;
    }

    auto rhs = generate_code(bin_op->rhs);
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
                case Tt::asterisk:              result = ir_builder.CreateMul(lhs, rhs); break;
                case Tt::slash:                 result = ir_builder.CreateSDiv(lhs, rhs); break;
                case Tt::mod:                   result = ir_builder.CreateSRem(lhs, rhs); break;
                case Tt::plus:                  result = ir_builder.CreateAdd(lhs, rhs); break;
                case Tt::minus:                 result = ir_builder.CreateSub(lhs, rhs); break;
                case Tt::assign:                result = ir_builder.CreateStore(rhs, lhs); break;  // TODO: Pointer to left hand side?
                case Tt::bit_and:               result = ir_builder.CreateAnd(lhs, rhs); break;
                case Tt::bit_or:                result = ir_builder.CreateOr(lhs, rhs); break;
                case Tt::bit_xor:               result = ir_builder.CreateXor(lhs, rhs); break;
                case Tt::left_shift:            result = ir_builder.CreateShl(lhs, rhs); break;
                case Tt::right_shift:           result = ir_builder.CreateLShr(lhs, rhs); break;  // TODO: LShr or AShr?
                case Tt::equal:                 result = ir_builder.CreateICmp(CmpInst::ICMP_EQ, lhs, rhs); break;
                case Tt::inequal:               result = ir_builder.CreateICmp(CmpInst::ICMP_NE, lhs, rhs); break;
                case Tt::greater_than_or_equal: result = ir_builder.CreateICmp(CmpInst::ICMP_SGE, lhs, rhs); break;
                case Tt::greater_than:          result = ir_builder.CreateICmp(CmpInst::ICMP_SGT, lhs, rhs); break;
                case Tt::less_than_or_equal:    result = ir_builder.CreateICmp(CmpInst::ICMP_SLE, lhs, rhs); break;
                case Tt::less_than:             result = ir_builder.CreateICmp(CmpInst::ICMP_SLT, lhs, rhs); break;
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

Function *function{};  // TODO

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

            auto result_block = BasicBlock::Create(llvm_context, "block", function);
            ir_builder.SetInsertPoint(result_block);

            for (auto statement : block->statements)
            {
                generate_code(statement);
            }

            return result_block;
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
                    auto procedure = node_cast<ProcedureNode, true>(decl->init_expression);

                    auto type          = make_type(decl->init_expression->type);
                    auto function_type = cast<FunctionType>(type);
                    auto function =
                        Function::Create(function_type, GlobalValue::LinkageTypes::InternalLinkage, decl->identifier);

                    auto i = 0;
                    for (auto &arg : function->args())
                    {
                        arg.setName(procedure->signature->arguments[i]->identifier);
                        ++i;
                    }

                    // TODO: Do we need to do anything with the function?

                    ::function = function;  // TODO!!!!

                    generate_code(decl->init_expression);

                    function->print(outs());

                    break;
                }

                default:
                {
                    // TODO
                    break;
                }
            }

            return nullptr;
        }

        case NodeKind::identifier:
        {
            // TODO!
            return ConstantInt::get(IntegerType::get(llvm_context, 32), 1234);
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

            generate_code(procedure->body);

            return nullptr;
        }

        case NodeKind::procedure_call: UNREACHED;

        case NodeKind::procedure_signature: UNREACHED;

        case NodeKind::return_statement:
        {
            auto retyrn = static_cast<ReturnNode *>(node);

            if (retyrn->expression->kind == NodeKind::nop)
            {
                return ir_builder.CreateRet(nullptr);
            }

            auto value = generate_code(retyrn->expression);
            return ir_builder.CreateRet(value);
        }

        case NodeKind::program:
        {
            auto program = static_cast<ProgramNode *>(node);
            // generate_code(program->block);

            for (auto declaration : program->block->statements)
            {
                assert(declaration->kind == NodeKind::declaration);
                generate_code(declaration);
            }

            return nullptr;
        }

        case NodeKind::simple_type:
        case NodeKind::pointer_type:
        case NodeKind::array_type:
        case NodeKind::struct_type:
        case NodeKind::nop:          break;
    }

    UNREACHED;
}

std::string compile_to_ir(struct Node *node)
{
    auto value = generate_code(node);
    // value->print(outs());

    return "TODO";
}
