#include "catch2/catch_test_macros.hpp"
#include "test_utils.h"
#include "typecheck.h"
#include <span>

using Types = BuiltinTypes;

TEST_CASE("Integer literals", "[typecheck]")
{
    std::tuple<AstNode *, const Node *> test_cases[] = {
        {make_int_literal(1), &BuiltinTypes::i64},
        {make_int_literal(1, 'u'), &BuiltinTypes::u64},
    };

    BlockNode block;

    for (auto [ast, expected_type] : test_cases)
    {
        auto node = make_node(&block, ast);
        REQUIRE(typecheck(node));
        REQUIRE(node != nullptr);
        REQUIRE(types_equal(node->type, expected_type));
    }
}

void test_types(std::span<std::tuple<AstNode *, const Node *>> test_cases)
{
    BlockNode block;

    for (auto [ast, expected_type] : test_cases)
    {
        auto node = make_node(&block, ast);
        REQUIRE(node != nullptr);

        if (expected_type == nullptr)
        {
            REQUIRE(typecheck(node) == false);
        }
        else
        {
            REQUIRE(typecheck(node));
            REQUIRE(types_equal(node->type, expected_type));
        }
    }
}

TEST_CASE("Literals", "[typecheck]")
{
    std::tuple<AstNode *, const Node *> test_cases[] = {
        {make_int_literal(1), &BuiltinTypes::i64},
        {make_int_literal(1, 'u'), &BuiltinTypes::u64},
        {make_float_literal(1.0f), &BuiltinTypes::f32},
        {make_double_literal(1.0), &BuiltinTypes::f64},
        {make_bool_literal(false), &BuiltinTypes::boolean},
    };

    test_types(test_cases);
}

TEST_CASE("Binary operators", "[typecheck]")
{
    SECTION("Arithmetic")
    {
        std::tuple<AstNode *, const Node *> test_cases[] = {
            {make_binary_operator(Tt::plus, make_int_literal(1), make_int_literal(2)), &Types::i64},
            {make_binary_operator(Tt::minus, make_int_literal(1), make_int_literal(2)), &Types::i64},
            {make_binary_operator(Tt::asterisk, make_int_literal(1), make_int_literal(2)), &Types::i64},
            {make_binary_operator(Tt::slash, make_int_literal(1), make_int_literal(2)), &Types::i64},
            {make_binary_operator(Tt::mod, make_int_literal(1), make_int_literal(2)), &Types::i64},

            {make_binary_operator(Tt::plus, make_float_literal(1), make_float_literal(2)), &Types::f32},
            {make_binary_operator(Tt::minus, make_float_literal(1), make_float_literal(2)), &Types::f32},
            {make_binary_operator(Tt::asterisk, make_float_literal(1), make_float_literal(2)), &Types::f32},
            {make_binary_operator(Tt::slash, make_float_literal(1), make_float_literal(2)), &Types::f32},
            {make_binary_operator(Tt::mod, make_float_literal(1), make_float_literal(2)), &Types::f32},

            {make_binary_operator(Tt::plus, make_double_literal(1), make_double_literal(2)), &Types::f64},
            {make_binary_operator(Tt::minus, make_double_literal(1), make_double_literal(2)), &Types::f64},
            {make_binary_operator(Tt::asterisk, make_double_literal(1), make_double_literal(2)), &Types::f64},
            {make_binary_operator(Tt::slash, make_double_literal(1), make_double_literal(2)), &Types::f64},
            {make_binary_operator(Tt::mod, make_double_literal(1), make_double_literal(2)), &Types::f64},
        };

        test_types(test_cases);
    };

    SECTION("Type coercion")
    {
        // TODO: Integer literals are defaulted to i64, so the floating point result is always going to
        // be f64 - think of some way to test coercion of differnt type sizes (i32 + f32 -> f32...)
        std::tuple<AstNode *, const Node *> test_cases[] = {
            {make_binary_operator(Tt::plus, make_float_literal(1), make_int_literal(2)), &Types::f64},
            {make_binary_operator(Tt::plus, make_int_literal(1), make_float_literal(2)), &Types::f64},
            {make_binary_operator(Tt::plus, make_double_literal(1), make_int_literal(2)), &Types::f64},
            {make_binary_operator(Tt::plus, make_int_literal(1), make_double_literal(2)), &Types::f64},

            {make_binary_operator(Tt::minus, make_float_literal(1), make_int_literal(2)), &Types::f64},
            {make_binary_operator(Tt::minus, make_int_literal(1), make_float_literal(2)), &Types::f64},
            {make_binary_operator(Tt::minus, make_double_literal(1), make_int_literal(2)), &Types::f64},
            {make_binary_operator(Tt::minus, make_int_literal(1), make_double_literal(2)), &Types::f64},

            {make_binary_operator(Tt::asterisk, make_float_literal(1), make_int_literal(2)), &Types::f64},
            {make_binary_operator(Tt::asterisk, make_int_literal(1), make_float_literal(2)), &Types::f64},
            {make_binary_operator(Tt::asterisk, make_double_literal(1), make_int_literal(2)), &Types::f64},
            {make_binary_operator(Tt::asterisk, make_int_literal(1), make_double_literal(2)), &Types::f64},

            {make_binary_operator(Tt::slash, make_float_literal(1), make_int_literal(2)), &Types::f64},
            {make_binary_operator(Tt::slash, make_int_literal(1), make_float_literal(2)), &Types::f64},
            {make_binary_operator(Tt::slash, make_double_literal(1), make_int_literal(2)), &Types::f64},
            {make_binary_operator(Tt::slash, make_int_literal(1), make_double_literal(2)), &Types::f64},

            {make_binary_operator(Tt::mod, make_float_literal(1), make_int_literal(2)), &Types::f64},
            {make_binary_operator(Tt::mod, make_int_literal(1), make_float_literal(2)), &Types::f64},
            {make_binary_operator(Tt::mod, make_double_literal(1), make_int_literal(2)), &Types::f64},
            {make_binary_operator(Tt::mod, make_int_literal(1), make_double_literal(2)), &Types::f64},
        };

        test_types(test_cases);
    };

    SECTION("Different signedness")
    {
        std::tuple<AstNode *, const Node *> test_cases[] = {
            {make_binary_operator(Tt::plus, make_int_literal(1, 'u'), make_int_literal(2)), nullptr},
            {make_binary_operator(Tt::minus, make_int_literal(1), make_int_literal(2, 'u')), nullptr},
            {make_binary_operator(Tt::asterisk, make_int_literal(1), make_int_literal(2, 'u')), nullptr},
            {make_binary_operator(Tt::slash, make_int_literal(1), make_int_literal(2, 'u')), nullptr},
            {make_binary_operator(Tt::mod, make_int_literal(1, 'u'), make_int_literal(2)), nullptr},
        };
        test_types(test_cases);
    };

    SECTION("Bitwise")
    {
        std::tuple<AstNode *, const Node *> test_cases[] = {
            {make_binary_operator(Tt::left_shift, make_int_literal(1, 'u'), make_int_literal(2, 'u')), &Types::u64},
            {make_binary_operator(Tt::right_shift, make_int_literal(1, 'u'), make_int_literal(2, 'u')), &Types::u64},
            {make_binary_operator(Tt::bit_and, make_int_literal(1, 'u'), make_int_literal(2, 'u')), &Types::u64},
            {make_binary_operator(Tt::bit_xor, make_int_literal(1, 'u'), make_int_literal(2, 'u')), &Types::u64},
            {make_binary_operator(Tt::bit_or, make_int_literal(1, 'u'), make_int_literal(2, 'u')), &Types::u64},

            {make_binary_operator(Tt::left_shift, make_int_literal(1, 'u'), make_int_literal(2)), nullptr},
            {make_binary_operator(Tt::right_shift, make_int_literal(1), make_int_literal(2, 'u')), nullptr},
            {make_binary_operator(Tt::bit_and, make_int_literal(1, 'u'), make_int_literal(2)), nullptr},
            {make_binary_operator(Tt::bit_xor, make_int_literal(1), make_int_literal(2)), nullptr},
            {make_binary_operator(Tt::bit_or, make_int_literal(1), make_int_literal(2)), nullptr},
        };

        test_types(test_cases);
    };

    SECTION("Boolean short circuit")
    {
        std::tuple<AstNode *, const Node *> test_cases[] = {
            {make_binary_operator(Tt::logical_or, make_bool_literal(true), make_int_literal(false)), &Types::boolean},
            {make_binary_operator(Tt::logical_and, make_bool_literal(false), make_int_literal(true)), &Types::boolean},
            {make_binary_operator(Tt::logical_or, make_int_literal(1), make_int_literal(2)), nullptr},
            {make_binary_operator(Tt::logical_and, make_int_literal(1), make_int_literal(2)), nullptr},
            {make_binary_operator(Tt::logical_and, make_int_literal(1), make_int_literal(2)), nullptr},
            {make_binary_operator(Tt::logical_or, make_int_literal(1), make_int_literal(2)), nullptr},
        };
        test_types(test_cases);
    };

    SECTION("Comparison")
    {
        std::tuple<AstNode *, const Node *> test_cases[] = {
            {make_binary_operator(Tt::inequal, make_int_literal(1), make_int_literal(2)), &Types::boolean},
            {make_binary_operator(Tt::equal, make_int_literal(1), make_int_literal(2)), &Types::boolean},
            {make_binary_operator(Tt::greater_than, make_int_literal(1), make_int_literal(2)), &Types::boolean},
            {make_binary_operator(Tt::greater_than_or_equal, make_int_literal(1), make_int_literal(2)),
             &Types::boolean},
            {make_binary_operator(Tt::less_than, make_int_literal(1), make_int_literal(2)), &Types::boolean},
            {make_binary_operator(Tt::less_than_or_equal, make_int_literal(1), make_int_literal(2)), &Types::boolean},
        };
        test_types(test_cases);
    };
}

TEST_CASE("Identifiers - declared in parent block", "[typecheck]")
{
    BlockNode parent_block{};

    auto decl              = new DeclarationNode{};
    decl->containing_block = &parent_block;
    decl->identifier       = "x";
    decl->specified_type   = const_cast<SimpleTypeNode *>(&Types::u8);  // TODO
    decl->init_expression  = new NopNode{};
    parent_block.statements.push_back(decl);

    REQUIRE(typecheck(decl));

    BlockNode child_block{};
    child_block.containing_block = &parent_block;

    {
        IdentifierNode identifier{};
        identifier.containing_block = &child_block;
        identifier.identifier       = "x";

        REQUIRE(typecheck(&identifier));
        REQUIRE(types_equal(identifier.type, &Types::u8));
    }

    {
        IdentifierNode identifier{};
        identifier.containing_block = &child_block;
        identifier.identifier       = "y";

        REQUIRE(typecheck(&identifier) == false);
    }
}

TEST_CASE("Identifiers - uninitialized, with explicit type", "[typecheck]")
{
    BlockNode block{};

    auto decl              = new DeclarationNode{};
    decl->containing_block = &block;
    decl->identifier       = "x";
    decl->specified_type   = const_cast<SimpleTypeNode *>(&Types::u8);  // TODO
    decl->init_expression  = new NopNode{};
    block.statements.push_back(decl);

    REQUIRE(typecheck(decl));

    {
        IdentifierNode identifier{};
        identifier.containing_block = &block;
        identifier.identifier       = "x";

        REQUIRE(typecheck(&identifier));
        REQUIRE(types_equal(identifier.type, &Types::u8));
    }

    {
        IdentifierNode identifier{};
        identifier.containing_block = &block;
        identifier.identifier       = "y";

        REQUIRE(typecheck(&identifier) == false);
    }
}

TEST_CASE("Identifiers - initialized, without explicit type", "[typecheck]")
{
    BlockNode block{};

    LiteralNode init_expression{};
    init_expression.value.emplace<bool>(true);

    auto decl              = new DeclarationNode{};
    decl->containing_block = &block;
    decl->identifier       = "x";
    decl->specified_type   = new NopNode{};
    decl->init_expression  = &init_expression;
    block.statements.push_back(decl);

    REQUIRE(typecheck(decl));

    {
        IdentifierNode identifier{};
        identifier.containing_block = &block;
        identifier.identifier       = "x";

        REQUIRE(typecheck(&identifier));
        REQUIRE(types_equal(identifier.type, &Types::boolean));
    }

    {
        IdentifierNode identifier{};
        identifier.containing_block = &block;
        identifier.identifier       = "y";

        REQUIRE(typecheck(&identifier) == false);
    }
}

TEST_CASE("Declarations - initialized, with different explicit type", "[typecheck]")
{
    BlockNode block{};

    LiteralNode init_expression{};
    init_expression.value.emplace<bool>(true);

    auto decl              = new DeclarationNode{};
    decl->containing_block = &block;
    decl->identifier       = "x";
    decl->specified_type   = const_cast<SimpleTypeNode *>(&Types::i8);  // TODO
    decl->init_expression  = &init_expression;
    block.statements.push_back(decl);

    REQUIRE(typecheck(decl) == false);
}
