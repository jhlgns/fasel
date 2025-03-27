#include "basics.h"
#include "parse.h"

#include <catch2/catch_test_macros.hpp>

// TODO: proc external test

namespace assertions
{
    template<typename T>
    using Assertion = std::function<void(T)>;

    struct Nop
    {
        void operator()(auto arg) { }
    };

    struct IsNull
    {
        void operator()(AstNode *node) { REQUIRE(node == nullptr); }
    };

    struct Token
    {
        explicit Token(std::string_view text)
            : text{text}
        {
        }

        explicit Token(TokenType type)
            : type{type}
        {
        }

        std::optional<std::string_view> text;
        std::optional<TokenType> type;

        void operator()(const ::Token &token)
        {
            if (this->type.has_value())
            {
                REQUIRE(token.type == this->type.value());
            }

            if (this->text.has_value())
            {
                REQUIRE(token.text() == this->text.value());
            }
        }

        void operator()(TokenType type) { REQUIRE(type == this->type.value()); }
    };

    struct BinaryOperator
    {
        TokenType type;
        Assertion<AstNode *> lhs;
        Assertion<AstNode *> rhs;

        void operator()(AstNode *node)
        {
            auto bin_op = ast_cast<AstBinaryOperator>(node);
            REQUIRE(bin_op != nullptr);

            REQUIRE(this->type != Tt::none);
            REQUIRE(this->type == bin_op->type);
            this->lhs(bin_op->lhs);
            this->rhs(bin_op->rhs);
        }
    };

    struct Block
    {
        std::vector<Assertion<AstNode *>> statements;

        void operator()(AstNode *node)
        {
            auto block = ast_cast<AstBlock>(node);
            REQUIRE(block != nullptr);

            REQUIRE(block->statements.size() == this->statements.size());

            for (auto i = 0; i < block->statements.size(); ++i)
            {
                this->statements[i](block->statements[i]);
            }
        }
    };

    struct Declaration
    {
        std::string_view identifier;
        Assertion<AstNode *> type;
        Assertion<AstNode *> init_expression;

        void operator()(AstNode *node)
        {
            auto decl = ast_cast<AstDeclaration>(node);
            REQUIRE(decl != nullptr);

            REQUIRE(this->identifier == decl->identifier.text());
            this->type(decl->type);
            this->init_expression(decl->init_expression);
        }
    };

    struct Identifier
    {
        explicit Identifier(std::string_view identifier)
            : identifier{identifier}
        {
        }

        std::string_view identifier;

        void operator()(AstNode *node)
        {
            auto ident = ast_cast<AstIdentifier>(node);
            REQUIRE(ident != nullptr);

            REQUIRE(ident->identifier.text() == this->identifier);
        }
    };

    struct If
    {
        Assertion<AstNode *> condition;
        Assertion<AstBlock *> then_block;
        Assertion<AstBlock *> else_block;

        void operator()(AstNode *node)
        {
            auto yf = ast_cast<AstIf>(node);
            REQUIRE(yf != nullptr);

            this->condition(yf->condition);
            this->then_block(&yf->then_block);
            this->else_block(yf->else_block);
        }
    };

    struct Literal
    {
        std::optional<Token> token;
        std::optional<bool> bool_value;
        std::optional<int64_t> int_value;
        std::optional<float> float_value;
        std::optional<double> double_value;
        std::optional<std::string> string_value;

        void operator()(AstNode *node)
        {
            auto n = this->int_value.has_value() + this->float_value.has_value() + this->double_value.has_value() +
                     this->bool_value.has_value() + this->string_value.has_value();
            REQUIRE(n == 1);

            auto literal = ast_cast<AstLiteral>(node);
            REQUIRE(literal != nullptr);

            if (this->token.has_value())
            {
                this->token.value()(literal->token);
            }

            if (this->bool_value.has_value())
            {
                REQUIRE(std::holds_alternative<bool>(literal->value));
                auto literal_bool_value = std::get<bool>(literal->value);
                REQUIRE(this->bool_value.value() == literal_bool_value);
            }

            if (this->int_value.has_value())
            {
                REQUIRE(std::holds_alternative<uint64_t>(literal->value));
                auto literal_int_value = std::get<uint64_t>(literal->value);
                REQUIRE(this->int_value.value() == literal_int_value);
            }

            if (this->float_value.has_value())
            {
                REQUIRE(std::holds_alternative<float>(literal->value));
                auto literal_float_value = std::get<float>(literal->value);
                REQUIRE(this->float_value.value() == literal_float_value);
            }

            if (this->double_value.has_value())
            {
                REQUIRE(std::holds_alternative<double>(literal->value));
                auto literal_double_value = std::get<double>(literal->value);
                REQUIRE(this->double_value.value() == literal_double_value);
            }

            if (this->string_value.has_value())
            {
                REQUIRE(std::holds_alternative<std::string>(literal->value));
                auto literal_string_value = std::get<std::string>(literal->value);
                REQUIRE(this->string_value.value() == literal_string_value);
            }
        }
    };

    struct ProcedureSignature
    {
        std::vector<Assertion<AstDeclaration *>> arguments;
        Assertion<AstNode *> return_type;

        void operator()(AstNode *node)
        {
            auto signature = ast_cast<AstProcedureSignature>(node);
            REQUIRE(signature != nullptr);

            REQUIRE(signature->arguments.size() == this->arguments.size());

            for (auto i = 0; i < signature->arguments.size(); ++i)
            {
                this->arguments[i](&signature->arguments[i]);
            }

            this->return_type(signature->return_type);
        }
    };

    struct Procedure
    {
        Assertion<AstProcedureSignature *> signature;
        Assertion<AstBlock *> body;

        void operator()(AstNode *node)
        {
            auto proc = ast_cast<AstProcedure>(node);
            REQUIRE(proc != nullptr);

            this->signature(&proc->signature);
            this->body(&proc->body);
        }
    };

    struct ProcedureCall
    {
        Assertion<AstNode *> procedure;
        std::vector<Assertion<AstNode *>> arguments;

        void operator()(AstNode *node)
        {
            auto call = ast_cast<AstProcedureCall>(node);
            REQUIRE(call != nullptr);

            this->procedure(call->procedure);

            REQUIRE(call->arguments.size() == this->arguments.size());

            for (auto i = 0; i < call->arguments.size(); ++i)
            {
                this->arguments[i](call->arguments[i]);
            }
        }
    };

    struct Return
    {
        Assertion<AstNode *> expression;

        void operator()(AstNode *node)
        {
            auto retyrn = ast_cast<AstReturn>(node);
            REQUIRE(retyrn != nullptr);

            this->expression(retyrn->expression);
        }
    };

    struct SimpleType
    {
        explicit SimpleType(std::string_view identifier)
            : identifier{identifier}
        {
        }

        std::string_view identifier;

        void operator()(AstNode *node)
        {
            auto type = ast_cast<AstTypeIdentifier>(node);
            REQUIRE(type != nullptr);

            REQUIRE(this->identifier == type->identifier.text());
        }
    };

    struct PointerType
    {
        Assertion<AstNode *> target_type;

        void operator()(AstNode *node)
        {
            auto type = ast_cast<AstPointerType>(node);
            REQUIRE(type != nullptr);

            this->target_type(type->target_type);
        }
    };

    struct ArrayType
    {
        Assertion<AstNode *> length_expression;
        Assertion<AstNode *> element_type;

        void operator()(AstNode *node)
        {
            auto type = ast_cast<AstArrayType>(node);
            REQUIRE(type != nullptr);

            this->length_expression(type->length_expression);
            this->element_type(type->element_type);
        }
    };
}  // namespace assertions

namespace as = assertions;

AstProcedure *parse_module_and_get_main(std::string_view source)
{
    AstModule module{};
    REQUIRE(parse_module(source, module));

    AstProcedure *main{};
    for (auto statement : module.block.statements)
    {
        auto decl = ast_cast<AstDeclaration>(statement);
        REQUIRE(decl != nullptr);

        if (decl->identifier.text() == "main")
        {
            main = ast_cast<AstProcedure>(decl->init_expression);
            break;
        }
    }

    REQUIRE(main != nullptr);

    return main;
}

template<typename T>
void test_literal(std::string_view literal_text, T value)
{
    auto source = std::format("main := proc() void {{ a := {} }}", literal_text);

    auto literal = as::Literal{};
    if constexpr (std::is_same_v<T, uint64_t>)
    {
        literal.int_value = value;
    }
    else if constexpr (std::is_same_v<T, float>)
    {
        literal.float_value = value;
    }
    else if constexpr (std::is_same_v<T, double>)
    {
        literal.double_value = value;
    }
    else if constexpr (std::is_same_v<T, bool>)
    {
        literal.bool_value = value;
    }
    else if constexpr (std::is_same_v<T, std::string>)
    {
        literal.string_value = value;
    }
    else
    {
        static_assert(false, "Invalid type");
    }

    as::Procedure{
        .signature = as::Nop{},

        .body = as::Block{
            .statements = {
                as::Declaration{
                    .identifier      = "a",
                    .type            = as::IsNull{},
                    .init_expression = literal,
                },
            }}}(parse_module_and_get_main(source));
}

TEST_CASE("Literals", "[parse]")
{
    test_literal<bool>("true", true);
    test_literal<bool>("false", false);

#define INTEGER_CASE(literal) test_literal<uint64_t>(#literal, literal);
    INTEGER_CASE(0);
    INTEGER_CASE(1);
    INTEGER_CASE(7);
    INTEGER_CASE(788);
    INTEGER_CASE(789237489234);
    INTEGER_CASE(9223372036854775807);  // int64_t max value

    INTEGER_CASE(0x0);
    INTEGER_CASE(0x00003);
    INTEGER_CASE(0xaaaaaaaaaa9900);
    INTEGER_CASE(0x0000000000000000);
    INTEGER_CASE(0x0000000000000001);
    INTEGER_CASE(0x7fffffffffffffff);
    INTEGER_CASE(0x78910fabc78eed81);
    INTEGER_CASE(0xAFFED00F);
    INTEGER_CASE(0xDec0dedFece5);
#undef INTEGER_CASE

#define FLOAT_CASE(literal) test_literal<float>(#literal, static_cast<float>(literal));
#define FLOAT_CASE_F(literal) test_literal<float>(#literal "f", static_cast<float>(literal));
    FLOAT_CASE(0.0f);
    FLOAT_CASE(0.f);
    FLOAT_CASE_F(0);
    FLOAT_CASE(1.2378492342738907891023475666147584f);
    FLOAT_CASE(2378492342738907891023475666147584.1f);
    FLOAT_CASE_F(2378492342738907891);
    FLOAT_CASE(0.f);
    FLOAT_CASE_F(0);
    FLOAT_CASE(127.0f);
    FLOAT_CASE_F(127);
    FLOAT_CASE(
        0.000000000000000000000000000000000000011754943508222875079687365372222456778186655567720875215087517062784172594547271728515625f);  // float min value
    FLOAT_CASE(340282346638528859811704183484516925440.0000f);  // float max value
    // TODO: Scientific float notation is not implemented yet
    // FLOAT_CASE(1.17549435E-38f);  // float min value
    // FLOAT_CASE(3.40282347E+38f);  // float max value
#undef FLOAT_CASE
#undef FLOAT_CASE_F

#define DOUBLE_CASE(literal) test_literal<double>(#literal, literal);
    DOUBLE_CASE(.0);
    DOUBLE_CASE(0.0);
    DOUBLE_CASE(.782937401900959);
    DOUBLE_CASE(123412784957798235.78293740190095923489);
    DOUBLE_CASE(
        0.0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000222507385850720138309023271733240406421921598046233183055332741688720443481391819585428315901251102056406733973103581100515243416155346010885601238537771882113077799353200233047961014744258363607192156504694250373420837525080665061665815894872049117996859163964850063590877011830487479978088775374994945158045160505091539985658247081864511353793580499211598108576605199243335211435239014879569960959128889160299264151106346631339366347758651302937176204732563178148566435087212282863764204484681140761391147706280168985324411002416144742161856716615054015428508471675290190316132277889672970737312333408698898317506783884692609277397797285865965494109136909540613646756870239867831529068098462);  // double min value (not really, I think it goes on)
    DOUBLE_CASE(
        179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168738177180919299881250404026184124858368.0000);  // double max value
    // TODO: Scientific float notation is not implemented yet
    // DOUBLE_CASE(2.2250738585072014E-308);  // double min value
    // DOUBLE_CASE(1.7976931348623157E+308);  // double max value
#undef DOUBLE_CASE

#define STRING_CASE(literal) test_literal<std::string>(#literal, literal);
    STRING_CASE("");
    STRING_CASE(" ");
    STRING_CASE("\0\0\0");
    STRING_CASE("*(){}:-=[]|&^<>1234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ");
    STRING_CASE("'\"\0TEXT\n");
    STRING_CASE("\\\"\"\"\\\"");
    STRING_CASE("This is a very normal string literal");
    STRING_CASE("abc\ndef");
    STRING_CASE(" \a\b\e\f\n\r\t\v\0\\\"");
#undef STRING_CASE
}

TEST_CASE("Basic binary operators", "[parse]")
{
    auto source = R"(
main := proc() void {
    a + b
    a - b
    a * b
    a / b
    a % b
    a >= b
    a = b
}
)"sv;

    as::Procedure{
        .signature = as::Nop{},

        .body = as::Block{
            .statements =
                {
                    as::BinaryOperator{
                        .type = Tt::plus,
                        .lhs  = as::Identifier{"a"},
                        .rhs  = as::Identifier{"b"},
                    },
                    as::BinaryOperator{
                        .type = Tt::minus,
                        .lhs  = as::Identifier{"a"},
                        .rhs  = as::Identifier{"b"},
                    },
                    as::BinaryOperator{
                        .type = Tt::asterisk,
                        .lhs  = as::Identifier{"a"},
                        .rhs  = as::Identifier{"b"},
                    },
                    as::BinaryOperator{
                        .type = Tt::slash,
                        .lhs  = as::Identifier{"a"},
                        .rhs  = as::Identifier{"b"},
                    },
                    as::BinaryOperator{
                        .type = Tt::mod,
                        .lhs  = as::Identifier{"a"},
                        .rhs  = as::Identifier{"b"},
                    },
                    as::BinaryOperator{
                        .type = Tt::greater_than_or_equal,
                        .lhs  = as::Identifier{"a"},
                        .rhs  = as::Identifier{"b"},
                    },
                    as::BinaryOperator{
                        .type = Tt::assign,
                        .lhs  = as::Identifier{"a"},
                        .rhs  = as::Identifier{"b"},
                    },
                },
        }}(parse_module_and_get_main(source));
}

TEST_CASE("Binary operator precedence", "[parse]")
{
    auto source = R"(
main := proc() void {
    a + b * c
    a * b + c
    1 * 2 < 3 & 4 | 5
}
)"sv;

    as::Procedure{
        .signature = as::Nop{},

        .body =
            as::Block{
                .statements =
                    {
                        // a + b * c
                        as::BinaryOperator{
                            .type = Tt::plus,
                            .lhs  = as::Identifier{"a"},
                            .rhs =
                                as::BinaryOperator{
                                    .type = Tt::asterisk,
                                    .lhs  = as::Identifier{"b"},
                                    .rhs  = as::Identifier{"c"},
                                },
                        },

                        // a * b + c
                        as::BinaryOperator{
                            .type = Tt::plus,
                            .lhs =
                                as::BinaryOperator{
                                    .type = Tt::asterisk,
                                    .lhs  = as::Identifier{"a"},
                                    .rhs  = as::Identifier{"b"},
                                },
                            .rhs = as::Identifier{"c"},
                        },

                        // 1 * 2 < 3 & 4 | 5
                        as::BinaryOperator{
                            .type = Tt::bit_or,
                            .lhs =
                                as::BinaryOperator{
                                    .type = Tt::bit_and,
                                    .lhs =
                                        as::BinaryOperator{
                                            .type = Tt::less_than,
                                            .lhs =
                                                as::BinaryOperator{
                                                    .type = Tt::asterisk,
                                                    .lhs  = as::Literal{.int_value = 1},
                                                    .rhs  = as::Literal{.int_value = 2},
                                                },
                                            .rhs = as::Literal{.int_value = 3},
                                        },
                                    .rhs = as::Literal{.int_value = 4},
                                },
                            .rhs = as::Literal{.int_value = 5},
                        },
                    },
            }}(parse_module_and_get_main(source));
}

TEST_CASE("Declaration", "[parse]")
{
    auto source = R"(
main := proc() void {
    a: i64
    b: i64 = 2
    c := 3
    d := proc() string {}
}
)"sv;

    as::Procedure{
        .signature = as::Nop{},

        .body =
            as::Block{
                .statements =
                    {
                        as::Declaration{
                            .identifier      = "a",
                            .type            = as::SimpleType{"i64"},
                            .init_expression = as::IsNull{},
                        },
                        as::Declaration{
                            .identifier      = "b",
                            .type            = as::SimpleType{"i64"},
                            .init_expression = as::Literal{.int_value = 2},
                        },
                        as::Declaration{
                            .identifier      = "c",
                            .type            = as::IsNull{},
                            .init_expression = as::Literal{.int_value = 3},
                        },
                        as::Declaration{
                            .identifier      = "d",
                            .type            = as::IsNull{},
                            .init_expression = as::Procedure{.signature = as::Nop{}, .body = as::Nop{}},
                        },
                    }},
    }(parse_module_and_get_main(source));
}

TEST_CASE("Parenthesis expression", "[parse]")
{
    auto source = R"(
main := proc() void {
    a := (1 + 2) * 3
}
)"sv;

    as::Procedure{
        .signature = as::Nop{},

        .body =
            as::Block{
                .statements = {as::Declaration{
                    .identifier = "a",
                    .type       = as::IsNull{},
                    .init_expression =
                        as::BinaryOperator{
                            .type = Tt::asterisk,
                            .lhs =
                                as::BinaryOperator{
                                    .type = Tt::plus,
                                    .lhs  = as::Literal{.int_value = 1},
                                    .rhs  = as::Literal{.int_value = 2},
                                },
                            .rhs = as::Literal{.int_value = 3},
                        },
                }}},
    }(parse_module_and_get_main(source));

    // std::cout << dump_node(0, parse_program_and_get_main(source));
}

TEST_CASE("If statement", "[parse]")
{
    // TODO: Once implemented, test simple statements without braces

    auto source = R"(
main := proc() void {
    a := 1

    if 2 < 3 {
        a = 2
    }

    if a == 2 {
        a = 3
    } else {
        a = 0
    }
}
)"sv;

    as::Procedure{
        .signature = as::Nop{},

        .body =
            as::Block{
                .statements =
                    {
                        as::Declaration{
                            .identifier      = "a",
                            .type            = as::IsNull{},
                            .init_expression = as::Literal{.int_value = 1},
                        },
                        as::If{
                            .condition =
                                as::BinaryOperator{
                                    .type = Tt::less_than,
                                    .lhs  = as::Literal{.int_value = 2},
                                    .rhs  = as::Literal{.int_value = 3},
                                },
                            .then_block =
                                as::Block{
                                    .statements =
                                        {
                                            as::BinaryOperator{
                                                .type = Tt::assign,
                                                .lhs  = as::Identifier{"a"},
                                                .rhs  = as::Literal{.int_value = 2},
                                            },
                                        }},
                            .else_block = as::IsNull{},
                        },
                        as::If{
                            .condition =
                                as::BinaryOperator{
                                    .type = Tt::equal,
                                    .lhs  = as::Identifier{"a"},
                                    .rhs  = as::Literal{.int_value = 2},
                                },
                            .then_block =
                                as::Block{
                                    .statements =
                                        {
                                            as::BinaryOperator{
                                                .type = Tt::assign,
                                                .lhs  = as::Identifier{"a"},
                                                .rhs  = as::Literal{.int_value = 3},
                                            },
                                        }},
                            .else_block =
                                as::Block{
                                    .statements =
                                        {
                                            as::BinaryOperator{
                                                .type = Tt::assign,
                                                .lhs  = as::Identifier{"a"},
                                                .rhs  = as::Literal{.int_value = 0},
                                            },
                                        }},
                        },
                    }}}(parse_module_and_get_main(source));
}

TEST_CASE("Procedure signature", "[parse]")
{
    auto source = R"(
main := proc() void {
    without_arguments := proc() void {}
    with_arguments := proc(a: i64, b: i64 = 2, c := 3 * 4) i64 {}
}
)"sv;

    as::Procedure{
        .signature = as::Nop{},

        .body =
            as::Block{
                .statements =
                    {
                        as::Declaration{
                            .identifier = "without_arguments",
                            .type       = as::IsNull{},
                            .init_expression =
                                as::Procedure{
                                    .signature =
                                        as::ProcedureSignature{
                                            .arguments   = {},
                                            .return_type = as::SimpleType{"void"},
                                        },
                                    .body = as::Block{.statements = {}},
                                }},
                        as::Declaration{
                            .identifier = "with_arguments",
                            .type       = as::IsNull{},
                            .init_expression =
                                as::Procedure{
                                    .signature =
                                        as::ProcedureSignature{
                                            .arguments =
                                                {
                                                    as::Declaration{
                                                        .identifier      = "a",
                                                        .type            = as::SimpleType{"i64"},
                                                        .init_expression = as::IsNull{},
                                                    },
                                                    as::Declaration{
                                                        .identifier      = "b",
                                                        .type            = as::SimpleType{"i64"},
                                                        .init_expression = as::Literal{.int_value = 2},
                                                    },
                                                    as::Declaration{
                                                        .identifier = "c",
                                                        .type       = as::IsNull{},
                                                        .init_expression =
                                                            as::BinaryOperator{
                                                                .type = Tt::asterisk,
                                                                .lhs  = as::Literal{.int_value = 3},
                                                                .rhs  = as::Literal{.int_value = 4},
                                                            },
                                                    },
                                                },
                                            .return_type = as::SimpleType{"i64"}},
                                    .body = as::Block{.statements = {}},
                                }},
                    }}}(parse_module_and_get_main(source));
}

TEST_CASE("Types", "[parse]")
{
    auto source = R"(
main := proc() void {
    simple: i64
    pointer: *i64
    array_of_simple: [27]i64

    pointer_to_pointer: **i64
    pointer_to_array: *[10]i64
    array_of_pointers: [10]*i64

    // procedure_pointer: proc(a i64, b []*i64, c: [2]proc(a: i64) *i64) void
    procedure_pointer: proc() void
    array_of_procedure_pointers: [10]proc() void
}
)"sv;

    as::Procedure{
        .signature = as::Nop{},

        .body =
            as::Block{
                .statements =
                    {
                        as::Declaration{
                            .identifier      = "simple",
                            .type            = as::SimpleType{"i64"},
                            .init_expression = as::IsNull{},
                        },
                        as::Declaration{
                            .identifier      = "pointer",
                            .type            = as::PointerType{.target_type = as::SimpleType{"i64"}},
                            .init_expression = as::IsNull{},
                        },
                        as::Declaration{
                            .identifier = "array_of_simple",
                            .type =
                                as::ArrayType{
                                    .length_expression = as::Literal{.int_value = 27},
                                    .element_type      = as::SimpleType{"i64"},
                                },
                            .init_expression = as::IsNull{},
                        },
                        as::Declaration{
                            .identifier = "pointer_to_pointer",
                            .type =
                                as::PointerType{
                                    .target_type =
                                        as::PointerType{
                                            .target_type = as::SimpleType{"i64"},
                                        },
                                },
                            .init_expression = as::IsNull{},
                        },
                        as::Declaration{
                            .identifier = "pointer_to_array",
                            .type =
                                as::PointerType{
                                    .target_type =
                                        as::ArrayType{
                                            .length_expression = as::Literal{.int_value = 10},
                                            .element_type      = as::SimpleType{"i64"}},
                                },
                            .init_expression = as::IsNull{},
                        },
                        as::Declaration{
                            .identifier = "array_of_pointers",
                            .type =
                                as::ArrayType{
                                    .length_expression = as::Literal{.int_value = 10},
                                    .element_type =
                                        as::PointerType{
                                            .target_type = as::SimpleType{"i64"},
                                        },
                                },
                            .init_expression = as::IsNull{},
                        },
                        as::Declaration{
                            .identifier = "procedure_pointer",
                            .type =
                                as::ProcedureSignature{
                                    .arguments = {
                                    },
                                    .return_type = as::SimpleType{"void"},
                                },
                            .init_expression = as::IsNull{},
                        },
                        as::Declaration{
                            .identifier = "array_of_procedure_pointers",
                            .type =
                                as::ArrayType{
                                    .length_expression = as::Literal{.int_value = 10},
                                    .element_type = as::ProcedureSignature{
                                        .arguments = {
                                        },
                                        .return_type = as::SimpleType{"void"},
                                    },
                                },
                            .init_expression = as::IsNull{},
                        },
                    }},
    }(parse_module_and_get_main(source));
}

TEST_CASE("Module", "[parse]")
{
    auto source = R"(
/*
Multiline
Comment

/* nested /* comments */
*/

*/

// Single line comment

f := proc(a: i64, b: i64) void {
    if a == 1 {
        return a * (b + 2) >> 2
    } else {
        return a / (400 + f(b | 2, a & 0xff))
    }
}

main := proc() void {
    x := f(3, 4)
}

)"sv;

    AstModule module;
    REQUIRE(parse_module(source, module));

    // TODO
    // std::cout << dump_node(0, &program) << std::endl;
}
