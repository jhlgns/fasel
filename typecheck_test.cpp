#include "context.h"
#include "test_utils.h"
#include "typecheck.h"

#include <catch2/catch_test_macros.hpp>

using Types = BuiltinTypes;

// NOTE: For every call to TypeChecker::error() there must be a test case in here
// TODO: Convert all new XXXNode to ctx.make_xxx

static Context ctx{};

ProcedureNode *make_dummy_proc()
{
    auto fake_module_root_block = ctx.make_block(nullptr, {});
    return ctx.make_procedure(
        ctx.make_procedure_signature({}, &Types::voyd),
        ctx.make_block(fake_module_root_block, {}),
        false);
}

void test_type(AstNode *ast, const Node *expected_type)
{
    TypeChecker tc{ctx};

    BlockNode block{};

    auto node = tc.make_node(ast);
    REQUIRE(node != nullptr);

    auto proc = make_dummy_proc();
    proc->body->statements.push_back(node);

    if (expected_type == nullptr)
    {
        CHECK(tc.typecheck(proc) == false);
    }
    else
    {
        CHECK(tc.typecheck(proc));
        CHECK(types_equal(node->type, expected_type));
    }
}

TEST_CASE("Literals", "[typecheck]")
{
    test_type(make_int_literal(1), &Types::i64);
    test_type(make_int_literal(1, 'u'), &Types::u64);
    test_type(make_float_literal(1.0f), &Types::f32);
    test_type(make_double_literal(1.0), &Types::f64);
    test_type(make_bool_literal(false), &Types::boolean);
}

TEST_CASE("Binary operators", "[typecheck]")
{
    SECTION("Arithmetic")
    {
        test_type(make_binary_operator(Tt::plus, make_int_literal(1), make_int_literal(2)), &Types::i64);
        test_type(make_binary_operator(Tt::minus, make_int_literal(1), make_int_literal(2)), &Types::i64);
        test_type(make_binary_operator(Tt::asterisk, make_int_literal(1), make_int_literal(2)), &Types::i64);
        test_type(make_binary_operator(Tt::slash, make_int_literal(1), make_int_literal(2)), &Types::i64);
        test_type(make_binary_operator(Tt::mod, make_int_literal(1), make_int_literal(2)), &Types::i64);

        test_type(make_binary_operator(Tt::plus, make_float_literal(1), make_float_literal(2)), &Types::f32);
        test_type(make_binary_operator(Tt::minus, make_float_literal(1), make_float_literal(2)), &Types::f32);
        test_type(make_binary_operator(Tt::asterisk, make_float_literal(1), make_float_literal(2)), &Types::f32);
        test_type(make_binary_operator(Tt::slash, make_float_literal(1), make_float_literal(2)), &Types::f32);
        test_type(make_binary_operator(Tt::mod, make_float_literal(1), make_float_literal(2)), &Types::f32);

        test_type(make_binary_operator(Tt::plus, make_double_literal(1), make_double_literal(2)), &Types::f64);
        test_type(make_binary_operator(Tt::minus, make_double_literal(1), make_double_literal(2)), &Types::f64);
        test_type(make_binary_operator(Tt::asterisk, make_double_literal(1), make_double_literal(2)), &Types::f64);
        test_type(make_binary_operator(Tt::slash, make_double_literal(1), make_double_literal(2)), &Types::f64);
        test_type(make_binary_operator(Tt::mod, make_double_literal(1), make_double_literal(2)), &Types::f64);
    };

    SECTION("Type coercion")
    {
        // TODO: Integer literals are defaulted to i64, so the floating point result is always going to
        // be f64 - think of some way to test coercion of differnt type sizes (i32 + f32 -> f32...)

        test_type(make_binary_operator(Tt::plus, make_float_literal(1), make_int_literal(2)), &Types::f64);
        test_type(make_binary_operator(Tt::plus, make_int_literal(1), make_float_literal(2)), &Types::f64);
        test_type(make_binary_operator(Tt::plus, make_double_literal(1), make_int_literal(2)), &Types::f64);
        test_type(make_binary_operator(Tt::plus, make_int_literal(1), make_double_literal(2)), &Types::f64);

        test_type(make_binary_operator(Tt::minus, make_float_literal(1), make_int_literal(2)), &Types::f64);
        test_type(make_binary_operator(Tt::minus, make_int_literal(1), make_float_literal(2)), &Types::f64);
        test_type(make_binary_operator(Tt::minus, make_double_literal(1), make_int_literal(2)), &Types::f64);
        test_type(make_binary_operator(Tt::minus, make_int_literal(1), make_double_literal(2)), &Types::f64);

        test_type(make_binary_operator(Tt::asterisk, make_float_literal(1), make_int_literal(2)), &Types::f64);
        test_type(make_binary_operator(Tt::asterisk, make_int_literal(1), make_float_literal(2)), &Types::f64);
        test_type(make_binary_operator(Tt::asterisk, make_double_literal(1), make_int_literal(2)), &Types::f64);
        test_type(make_binary_operator(Tt::asterisk, make_int_literal(1), make_double_literal(2)), &Types::f64);

        test_type(make_binary_operator(Tt::slash, make_float_literal(1), make_int_literal(2)), &Types::f64);
        test_type(make_binary_operator(Tt::slash, make_int_literal(1), make_float_literal(2)), &Types::f64);
        test_type(make_binary_operator(Tt::slash, make_double_literal(1), make_int_literal(2)), &Types::f64);
        test_type(make_binary_operator(Tt::slash, make_int_literal(1), make_double_literal(2)), &Types::f64);

        test_type(make_binary_operator(Tt::mod, make_float_literal(1), make_int_literal(2)), &Types::f64);
        test_type(make_binary_operator(Tt::mod, make_int_literal(1), make_float_literal(2)), &Types::f64);
        test_type(make_binary_operator(Tt::mod, make_double_literal(1), make_int_literal(2)), &Types::f64);
        test_type(make_binary_operator(Tt::mod, make_int_literal(1), make_double_literal(2)), &Types::f64);
    };

    SECTION("Different signedness")
    {
        test_type(make_binary_operator(Tt::plus, make_int_literal(1, 'u'), make_int_literal(2)), nullptr);
        test_type(make_binary_operator(Tt::minus, make_int_literal(1), make_int_literal(2, 'u')), nullptr);
        test_type(make_binary_operator(Tt::asterisk, make_int_literal(1), make_int_literal(2, 'u')), nullptr);
        test_type(make_binary_operator(Tt::slash, make_int_literal(1), make_int_literal(2, 'u')), nullptr);
        test_type(make_binary_operator(Tt::mod, make_int_literal(1, 'u'), make_int_literal(2)), nullptr);
    };

    SECTION("Bitwise")
    {
        test_type(
            make_binary_operator(Tt::left_shift, make_int_literal(1, 'u'), make_int_literal(2, 'u')),
            &Types::u64);
        test_type(
            make_binary_operator(Tt::right_shift, make_int_literal(1, 'u'), make_int_literal(2, 'u')),
            &Types::u64);
        test_type(make_binary_operator(Tt::bit_and, make_int_literal(1, 'u'), make_int_literal(2, 'u')), &Types::u64);
        test_type(make_binary_operator(Tt::bit_xor, make_int_literal(1, 'u'), make_int_literal(2, 'u')), &Types::u64);
        test_type(make_binary_operator(Tt::bit_or, make_int_literal(1, 'u'), make_int_literal(2, 'u')), &Types::u64);

        test_type(make_binary_operator(Tt::left_shift, make_int_literal(1, 'u'), make_int_literal(2)), nullptr);
        test_type(make_binary_operator(Tt::right_shift, make_int_literal(1), make_int_literal(2, 'u')), nullptr);
        test_type(make_binary_operator(Tt::bit_and, make_int_literal(1, 'u'), make_int_literal(2)), nullptr);
        test_type(make_binary_operator(Tt::bit_xor, make_int_literal(1), make_int_literal(2)), nullptr);
        test_type(make_binary_operator(Tt::bit_or, make_int_literal(1), make_int_literal(2)), nullptr);
    };

    SECTION("Boolean short circuit")
    {
        test_type(
            make_binary_operator(Tt::logical_or, make_bool_literal(true), make_int_literal(false)),
            &Types::boolean);
        test_type(
            make_binary_operator(Tt::logical_and, make_bool_literal(false), make_int_literal(true)),
            &Types::boolean);
        test_type(make_binary_operator(Tt::logical_or, make_int_literal(1), make_int_literal(2)), nullptr);
        test_type(make_binary_operator(Tt::logical_and, make_int_literal(1), make_int_literal(2)), nullptr);
        test_type(make_binary_operator(Tt::logical_and, make_int_literal(1), make_int_literal(2)), nullptr);
        test_type(make_binary_operator(Tt::logical_or, make_int_literal(1), make_int_literal(2)), nullptr);
    };

    SECTION("Comparison")
    {
        test_type(make_binary_operator(Tt::inequal, make_int_literal(1), make_int_literal(2)), &Types::boolean);
        test_type(make_binary_operator(Tt::equal, make_int_literal(1), make_int_literal(2)), &Types::boolean);
        test_type(make_binary_operator(Tt::greater_than, make_int_literal(1), make_int_literal(2)), &Types::boolean);
        test_type(
            make_binary_operator(Tt::greater_than_or_equal, make_int_literal(1), make_int_literal(2)),
            &Types::boolean);
        test_type(make_binary_operator(Tt::less_than, make_int_literal(1), make_int_literal(2)), &Types::boolean);
        test_type(
            make_binary_operator(Tt::less_than_or_equal, make_int_literal(1), make_int_literal(2)),
            &Types::boolean);
    };
}

TEST_CASE("Binary operator coercion implicit type cast", "[typecheck]")
{
    // TODO: Test that TypeCastNodes are inserted upon implicit type coercion
}

TEST_CASE("Identifiers - declared in parent block", "[typecheck]")
{
    SECTION("Found")
    {
        TypeChecker tc{ctx};

        auto proc = make_dummy_proc();

        proc->body->statements.push_back(ctx.make_declaration("x", &Types::i8, nullptr));

        auto identifier = ctx.make_identifier("x");
        proc->body->statements.push_back(ctx.make_block(proc->body, {identifier}));

        REQUIRE(tc.typecheck(proc));
        REQUIRE(types_equal(identifier->type, &Types::i8));
    }

    SECTION("Not found")
    {
        TypeChecker tc{ctx};

        auto proc = make_dummy_proc();

        proc->body->statements.push_back(ctx.make_declaration("x", &Types::i8, nullptr));

        auto identifier = ctx.make_identifier("y");
        proc->body->statements.push_back(ctx.make_block(proc->body, {identifier}));

        REQUIRE_FALSE(tc.typecheck(proc));
    }
}

TEST_CASE("Identifiers - uninitialized, with explicit type", "[typecheck]")
{
    TypeChecker tc{ctx};

    auto proc = make_dummy_proc();

    auto decl = ctx.make_declaration("x", &Types::i8, nullptr);
    proc->body->statements.push_back(decl);

    REQUIRE(tc.typecheck(proc));
    REQUIRE(types_equal(decl->init_expression->type, &Types::i8));
}

TEST_CASE("Identifiers - initialized, without explicit type", "[typecheck]")
{
    TypeChecker tc{ctx};

    auto proc = make_dummy_proc();

    auto decl = ctx.make_declaration("x", nullptr, ctx.make_bool_literal(false));
    proc->body->statements.push_back(decl);

    REQUIRE(tc.typecheck(proc));
    REQUIRE(types_equal(decl->init_expression->type, &Types::boolean));
}

TEST_CASE("Declarations - initialized, with different explicit type", "[typecheck]")
{
    TypeChecker tc{ctx};

    auto proc = make_dummy_proc();

    auto decl = ctx.make_declaration("x", &Types::i8, ctx.make_bool_literal(false));
    proc->body->statements.push_back(decl);

    REQUIRE_FALSE(tc.typecheck(proc));
}

TEST_CASE("Procedures", "[typecheck]")
{
    // TODO: Rewrite this to not use AST nodes

    // auto signature_ast = new AstProcedureSignature{};
    // signature_ast->arguments.push_back(*make_declaration("a", make_simple_type("i64"), make_int_literal(1)));
    // signature_ast->arguments.push_back(*make_declaration("b", nullptr, make_float_literal(1.0f)));
    // signature_ast->arguments.push_back(*make_declaration("c", make_simple_type("f64"), nullptr));
    // signature_ast->return_type = make_simple_type("void");

    // auto proc_ast = make_procedure(signature_ast, make_block({}));
    // auto proc =

    // auto a                   = ctx.make_declaration(proc_ast->body, "a", ctx.make_nop(proc_ast->body), &Types::i64);
    // a->init_expression       = new NopNode{};
    // a->init_expression->type = &Types::i64;
    // auto b                   = new DeclarationNode{};
    // b->init_expression       = new NopNode{};
    // b->init_expression->type = &Types::f32;
    // auto c                   = new DeclarationNode{};
    // c->init_expression       = new NopNode{};
    // c->init_expression->type = &Types::f64;

    // auto expected_type = new ProcedureSignatureNode{};
    // expected_type->arguments.push_back(a);
    // expected_type->arguments.push_back(b);
    // expected_type->arguments.push_back(c);
    // expected_type->return_type = &Types::voyd;

    // test_type(proc_ast, expected_type);
}

TEST_CASE("Return expression", "[typecheck]")
{
    // TODO: Test that the return expression must match the procedure return type
    // or that the return expression is a NopNode if the procedure return type is void
}

TEST_CASE("Procedure calls", "[typecheck]")
{
    auto signature = new AstProcedureSignature{};
    signature->arguments.push_back(*make_declaration("a", make_simple_type("i64"), make_int_literal(1)));
    signature->arguments.push_back(*make_declaration("b", nullptr, make_float_literal(1.0f)));
    signature->arguments.push_back(*make_declaration("c", make_simple_type("f64"), nullptr));
    signature->return_type = make_simple_type("void");

    auto proc = make_procedure(signature, make_block({}));

    SECTION("Not callable")
    {
        auto call = make_procedure_call(make_int_literal(1), {make_int_literal(123)});

        test_type(call, nullptr);
    }

    SECTION("Wrong number of arguments")
    {
        auto call = make_procedure_call(proc, {make_int_literal(123)});

        test_type(call, nullptr);
    }

    SECTION("Wrong argument types")
    {
        auto call = make_procedure_call(proc, {make_int_literal(123), make_int_literal(456), make_int_literal(789)});

        test_type(call, nullptr);
    }

    SECTION("Correct call")
    {
        auto call =
            make_procedure_call(proc, {make_int_literal(123), make_float_literal(456), make_double_literal(789)});

        test_type(call, &Types::voyd);
    }
}
