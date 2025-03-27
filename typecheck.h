#pragma once

#include "context.h"
#include "node.h"

struct AstNode;

bool types_equal(const Node *lhs, const Node *rhs);
std::string type_to_string(const Node *type);

struct TypeChecker
{
    explicit TypeChecker(Context &context)
        : ctx(context)
    {
    }

    Context &ctx;
    ProcedureNode *current_procedure{};
    BlockNode *current_block{};
    std::vector<std::string> errors{};

    Node *make_node(AstNode *ast);
    [[nodiscard]] bool typecheck(Node *node);

    void error(const Node *node, std::string_view message);
};

struct BuiltinTypes
{
    static BasicTypeNode voyd;
    static BasicTypeNode i64;
    static BasicTypeNode i32;
    static BasicTypeNode i16;
    static BasicTypeNode i8;
    static BasicTypeNode u64;
    static BasicTypeNode u32;
    static BasicTypeNode u16;
    static BasicTypeNode u8;
    static BasicTypeNode f32;
    static BasicTypeNode f64;
    static BasicTypeNode boolean;
    static BasicTypeNode type;
    static PointerTypeNode string_literal;
    static ProcedureSignatureNode main_signature;

    static const std::vector<std::tuple<BasicTypeNode *, std::string_view>> type_names;
};
