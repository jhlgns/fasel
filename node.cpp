#include "node.h"

void Node::set_inferred_type(Node *inferred_type)
{
    assert(
        this->inferred_type_ == nullptr || Node::types_equal(this->inferred_type_, &BuiltinTypes::poison) ||
        Node::types_equal(this->inferred_type_, &BuiltinTypes::nop));
    this->inferred_type_ = inferred_type;
}

Node *Node::inferred_type() const
{
    return this->inferred_type_;
}

bool Node::is_type() const
{
    return this->kind == NodeKind::basic_type || this->kind == NodeKind::pointer_type ||
           this->kind == NodeKind::array_type || this->kind == NodeKind::procedure_signature ||
           this->kind == NodeKind::struct_type || this->kind == NodeKind::nop;
}

bool Node::is_poisoned() const
{
    return types_equal(this->inferred_type(), &BuiltinTypes::poison);
}

bool Node::types_equal(const Node *lhs, const Node *rhs)
{
    assert(lhs != nullptr && rhs != nullptr);
    assert(lhs->is_type() && rhs->is_type());

    if (lhs->kind != rhs->kind)
    {
        return false;
    }

    switch (lhs->kind)
    {
        case NodeKind::basic_type:
        {
            auto lhs_basic = static_cast<const BasicTypeNode *>(lhs);
            auto rhs_basic = static_cast<const BasicTypeNode *>(rhs);

            return lhs_basic->type_kind == rhs_basic->type_kind && lhs_basic->size == rhs_basic->size;
        }

        case NodeKind::pointer_type:
        {
            auto lhs_pointer = static_cast<const PointerTypeNode *>(lhs);
            auto rhs_pointer = static_cast<const PointerTypeNode *>(rhs);

            return types_equal(lhs_pointer->target_type, rhs_pointer->target_type);
        }

        case NodeKind::array_type:
        {
            auto lhs_array = static_cast<const ArrayTypeNode *>(lhs);
            auto rhs_array = static_cast<const ArrayTypeNode *>(rhs);

            if (types_equal(lhs_array->element_type, rhs_array->element_type) == false)
            {
                return false;
            }

            // TODO
            auto lhs_length_literal = node_cast<LiteralNode>(lhs_array->length);
            auto rhs_length_literal = node_cast<LiteralNode>(rhs_array->length);

            if (lhs_length_literal == nullptr || rhs_length_literal == nullptr)
            {
                FATAL("Only literal array length expressions are supported for now");
            }

            if (std::holds_alternative<uint64_t>(lhs_length_literal->value) == false ||
                std::holds_alternative<uint64_t>(rhs_length_literal->value) == false)
            {
                FATAL("The length expression is not an integer literal value");
            }

            auto lhs_length = std::get<uint64_t>(lhs_length_literal->value);
            auto rhs_length = std::get<uint64_t>(rhs_length_literal->value);

            return lhs_length == rhs_length;
        }

        case NodeKind::procedure_signature:
        {
            auto lhs_signature = static_cast<const ProcedureSignatureNode *>(lhs);
            auto rhs_signature = static_cast<const ProcedureSignatureNode *>(rhs);

            if (lhs_signature->is_vararg != rhs_signature->is_vararg)
            {
                return false;
            }

            if (types_equal(lhs_signature->return_type, rhs_signature->return_type) == false)
            {
                return false;
            }

            if (lhs_signature->arguments.size() != rhs_signature->arguments.size())
            {
                return false;
            }

            for (auto i = 0; i < lhs_signature->arguments.size(); ++i)
            {
                auto lhs_type = lhs_signature->arguments[i]->init_expression->inferred_type();
                auto rhs_type = rhs_signature->arguments[i]->init_expression->inferred_type();

                if (types_equal(lhs_type, rhs_type) == false)
                {
                    return false;
                }
            }

            return true;
        }

        case NodeKind::struct_type:
        {
            // TODO
            UNREACHED;
        }

        case NodeKind::nop:
        {
            return true;
        }

        default: UNREACHED;
    }
}

std::string Node::type_to_string(const Node *type)
{
    assert(type->is_type());

    switch (type->kind)
    {
        case NodeKind::procedure_signature:
        {
            auto signature     = static_cast<const ProcedureSignatureNode *>(type);
            std::string result = "proc(";

            for (auto i = 0; i < signature->arguments.size(); ++i)
            {
                result += type_to_string(signature->arguments[i]->init_expression->inferred_type());

                if (i + 1 < signature->arguments.size())
                {
                    result += ", ";
                }
            }

            result += ") ";
            result += type_to_string(signature->return_type);

            return result;
        }

        case NodeKind::basic_type:
        {
            auto simple_type = static_cast<const BasicTypeNode *>(type);

            for (auto [builtin_type, name] : BuiltinTypes::type_names)
            {
                if (types_equal(builtin_type, type))
                {
                    return std::string{name};
                }
            }

            // TODO: Custom type names
            UNREACHED;
        }

        case NodeKind::pointer_type:
        {
            auto pointer_type = static_cast<const PointerTypeNode *>(type);
            return "*" + type_to_string(pointer_type->target_type);
        }

        case NodeKind::array_type:
        {
            auto array_type = static_cast<const ArrayTypeNode *>(type);
            // TODO
            return "[...]" + type_to_string(array_type->element_type);
        }

        case NodeKind::struct_type:
        {
            TODO;
        }

        case NodeKind::nop:
        {
            return "(nothing)";
        }

        default: UNREACHED;
    }
}

DeclarationNode *BlockNode::find_declaration(std::string_view name, bool recurse) const
{
    auto it = this->declarations.find(std::string{name});
    if (it != this->declarations.end())
    {
        return it->second;
    }

    if (recurse && this->parent_block != nullptr)
    {
        return this->parent_block->find_declaration(name);
    }

    return nullptr;
}

BasicTypeNode BuiltinTypes::voyd    = BasicTypeNode{BasicTypeNode::Kind::voyd, -1};
BasicTypeNode BuiltinTypes::i64     = BasicTypeNode{BasicTypeNode::Kind::signed_integer, 8};
BasicTypeNode BuiltinTypes::i32     = BasicTypeNode{BasicTypeNode::Kind::signed_integer, 4};
BasicTypeNode BuiltinTypes::i16     = BasicTypeNode{BasicTypeNode::Kind::signed_integer, 2};
BasicTypeNode BuiltinTypes::i8      = BasicTypeNode{BasicTypeNode::Kind::signed_integer, 1};
BasicTypeNode BuiltinTypes::u64     = BasicTypeNode{BasicTypeNode::Kind::unsigned_integer, 8};
BasicTypeNode BuiltinTypes::u32     = BasicTypeNode{BasicTypeNode::Kind::unsigned_integer, 4};
BasicTypeNode BuiltinTypes::u16     = BasicTypeNode{BasicTypeNode::Kind::unsigned_integer, 2};
BasicTypeNode BuiltinTypes::u8      = BasicTypeNode{BasicTypeNode::Kind::unsigned_integer, 1};
BasicTypeNode BuiltinTypes::f32     = BasicTypeNode{BasicTypeNode::Kind::floatingpoint, 4};
BasicTypeNode BuiltinTypes::f64     = BasicTypeNode{BasicTypeNode::Kind::floatingpoint, 8};
BasicTypeNode BuiltinTypes::boolean = BasicTypeNode{BasicTypeNode::Kind::boolean, 1};
BasicTypeNode BuiltinTypes::type    = BasicTypeNode{BasicTypeNode::Kind::type, -1};
BasicTypeNode BuiltinTypes::label   = BasicTypeNode{BasicTypeNode::Kind::label, -1};
BasicTypeNode BuiltinTypes::poison  = BasicTypeNode{BasicTypeNode::Kind::poison, -1};
NopNode BuiltinTypes::nop           = NopNode{};

PointerTypeNode BuiltinTypes::string_literal = []
{
    PointerTypeNode result{};
    result.target_type = &BuiltinTypes::i8;
    return result;
}();

ProcedureSignatureNode BuiltinTypes::main_signature = []
{
    ProcedureSignatureNode result;
    result.return_type = &BuiltinTypes::voyd;
    return result;
}();

const std::vector<std::tuple<BasicTypeNode *, std::string_view>> BuiltinTypes::type_names = {
    std::make_tuple(&BuiltinTypes::voyd, "void"),
    std::make_tuple(&BuiltinTypes::i64, "i64"),
    std::make_tuple(&BuiltinTypes::i32, "i32"),
    std::make_tuple(&BuiltinTypes::i16, "i16"),
    std::make_tuple(&BuiltinTypes::i8, "i8"),
    std::make_tuple(&BuiltinTypes::u64, "u64"),
    std::make_tuple(&BuiltinTypes::u32, "u32"),
    std::make_tuple(&BuiltinTypes::u16, "u16"),
    std::make_tuple(&BuiltinTypes::u8, "u8"),
    std::make_tuple(&BuiltinTypes::f32, "f32"),
    std::make_tuple(&BuiltinTypes::f64, "f64"),
    std::make_tuple(&BuiltinTypes::boolean, "bool"),
    std::make_tuple(&BuiltinTypes::type, "type"),
    std::make_tuple(&BuiltinTypes::label, "label"),
    std::make_tuple(&BuiltinTypes::poison, "poison"),
};
