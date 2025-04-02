#pragma once

#include "context.h"
#include "node.h"

struct AstNode;

struct NodeConverter
{
    Context &ctx;
    BlockNode *current_block{};

    explicit NodeConverter(Context &ctx)
        : ctx{ctx}
    {
    }

    Node *make_node(AstNode *ast);
};

struct DeclarationRegistrar : NodeVisitorBase
{
    Context &ctx;

    explicit DeclarationRegistrar(Context &ctx)
        : ctx{ctx}
    {
    }

    void register_declarations(ModuleNode *node);
    // void visit_done(BlockNode *block) override;
    void visit(DeclarationNode *declaration) override;
    void visit(LabelNode *label) override;
    void visit(ProcedureNode *procedure) override;

    // void error(const Node *node, std::string_view message);
};

struct TypeChecker
{
    Context &ctx;
    ProcedureNode *current_procedure{};  // TODO: Implement a node stack and do a upward search
    BlockNode *current_block{};
    std::vector<std::string> errors{};

    explicit TypeChecker(Context &context)
        : ctx{context}
    {
    }

    bool do_implicit_cast_if_necessary(Node *&node, Node *type);
    Node *coerce_types(BinaryOperatorNode *bin_op);
    void typecheck(Node *node);
    void typecheck_internal(Node *node);
    bool typecheck_and_spread_poison(Node *node, Node *parent);

    void error(Node *node, bool poison, std::string_view message);
};
