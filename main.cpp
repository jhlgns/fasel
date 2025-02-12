#include <assert.h>
#include <charconv>
#include <span>
#include <format>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <vector>

template <typename F>
struct ScopeExit {
    F f;
    ScopeExit(F f) : f(f) {}
    ~ScopeExit() { f(); }
};

struct DEFER_TAG {};

template<class F>
ScopeExit<F> operator+(DEFER_TAG, F &&f) {
    return ScopeExit<F>{std::forward<F>(f)};
}

#define DEFER_1(x, y) x##y
#define DEFER_2(x, y) DEFER_1(x, y)
#define defer auto DEFER_2(ScopeExit, __LINE__) = DEFER_TAG{} + [&]()

// The arena is reset after each parsed top level declaration.
#define ARENA_SIZE 10 * 1024 * 1024
struct {
    char *memory_start;
    char *cursor;
} arena;

void *talloc(size_t bytes) {
    if ((arena.cursor - arena.memory_start) + bytes >= ARENA_SIZE) {
        printf("Out of arena memory\n");
        abort();
    }

    char *result = arena.cursor;
    arena.cursor += bytes;

    // TODO: Alignment?
    /* memset(result, 0x0, bytes); */

    return result;
}


//
// LEXING
//

enum TokenType {
    TOK_ASTERISK = '*',
    TOK_SLASH = '/',
    TOK_MOD = '%',
    TOK_PLUS = '+',
    TOK_MINUS = '-',
    TOK_EQUALS = '=',
    TOK_COMMA = ',',
    TOK_OPENPAREN = '(',
    TOK_CLOSEPAREN = ')',
    TOK_OPENBRACE = '{',
    TOK_CLOSEBRACE = '}',
    TOK_IDENT = 255,
    TOK_KEYWORD,
    TOK_NUM_LIT,
    TOK_DECLASSIGN,  // :=
    TOK_EOF,
    /* TOK_OPENBRACKET = '[', */
    /* TOK_CLOSERACKET = ']', */
};

const char *to_string(TokenType t) {
    switch (t) {
        case TOK_ASTERISK:   return "TOK_ASTERISK";
        case TOK_SLASH:      return "TOK_SLASH";
        case TOK_PLUS:       return "TOK_PLUS";
        case TOK_MINUS:      return "TOK_MINUS";
        case TOK_EQUALS:     return "TOK_EQUALS";
        case TOK_COMMA:      return "TOK_COMMA";
        case TOK_OPENPAREN:  return "TOK_OPENPAREN";
        case TOK_CLOSEPAREN: return "TOK_CLOSEPAREN";
        case TOK_OPENBRACE:  return "TOK_OPENBRACE";
        case TOK_CLOSEBRACE: return "TOK_CLOSEBRACE";
        case TOK_IDENT:      return "TOK_IDENT";
        case TOK_KEYWORD:    return "TOK_KEYWORD";
        case TOK_NUM_LIT:    return "TOK_NUM_LIT";
        case TOK_DECLASSIGN: return "TOK_DECLASSIGN";
        case TOK_EOF:        return "TOK_EOF";
        default:             return "(unknown!)";
    }
}

// The higher the precedence, the stronger the operator binds its arguments.
// '*' has higher precedence than '+'.
int binop_prec(TokenType binop) {
    switch (binop) {
        case '*':
        case '/':
        case '%': return 100;
        case '+':
        case '-': return 90;
        case '=': return 80;
        /* case ',': return 15; */
        default: return 0;
    }
}

struct LexerPos {
    char *c;
    int line;
    int line_offset;
};

struct Lexer {
    std::string_view source;
    LexerPos start;
    LexerPos at;
};

struct Token {
    TokenType type;
    LexerPos pos;
    size_t len;
};

std::string_view text_of(Token *token) {
    return std::string_view{token->pos.c, token->len};
}

void next(LexerPos *p, int n = 1) {
    for (int i = 0; i < n; ++i, ++p->c) {
        if (*p->c == '\0') {
            break;
        }

        if (*p->c == '\n') {
            ++p->line;
            p->line_offset = 0;
        } else {
            ++p->line_offset;
        }
    }
}

Token emit(Lexer *l, TokenType t) {
    assert(t == TOK_EOF || l->at.c > l->start.c);

    return Token{
        .type = t,
        .pos = l->start,
        .len = static_cast<size_t>(l->at.c - l->start.c),
    };
}

bool eat_seq(Lexer *l, std::string_view txt) {
    if (l->at.c - l->source.data() + txt.size() > l->source.size()) {
        return 0;
    }

    for (int i = 0; i < txt.size(); ++i) {
        if (l->at.c[i] != txt[i]) {
            return 0;
        }
    }

    next(&l->at, txt.size());
    return 1;
}

bool is_ident(char c, int i) {
    return
        c >= 'a' && c <= 'z' || 
        c >= 'A' && c <= 'Z' || 
        c == '_' ||
        (i > 0 && (c >= '0' && c <= '9')) ;
}

bool is_white(char c) {
    return c == ' ' || c == '\t' || c == '\n';
}

bool is_digit(char c) {
    return c >= '0' && c <= '9';
}

Token next_token(Lexer *l) {
    // TODO: Comments
    // TODO: Floating point literals

    for (; is_white(*l->at.c); next(&l->at)) {
    }

    l->start = l->at;

    if (eat_seq(l, "return")) { return emit(l, TOK_KEYWORD); }
    if (eat_seq(l, "proc"))   { return emit(l, TOK_KEYWORD); }
    if (eat_seq(l, "if"))     { return emit(l, TOK_KEYWORD); }
    if (eat_seq(l, "else"))   { return emit(l, TOK_KEYWORD); }
    if (eat_seq(l, "while"))  { return emit(l, TOK_KEYWORD); }

    for (; is_ident(*l->at.c, l->at.c - l->start.c); next(&l->at)) { }
    if (l->at.c > l->start.c) {
        return emit(l, TOK_IDENT);
    }

    for (; is_digit(*l->at.c); next(&l->at)) { }
    if (l->at.c > l->start.c) {
        return emit(l, TOK_NUM_LIT);
    }

    if (eat_seq(l, ":=")) { return emit(l, TOK_DECLASSIGN); }

    char single_char_tokens[] = {'*', '/', '%', '+', '-', ',', '(', ')', '{', '}', '='};
    for (int i = 0; i < sizeof(single_char_tokens); ++i) {
        if (*l->at.c == single_char_tokens[i]) {
            next(&l->at);
            return emit(l, static_cast<TokenType>(single_char_tokens[i]));
        }
    }

    if (*l->at.c == '\0') {
        return emit(l, TOK_EOF);
    }

    printf("Lexer error at %d:%d: unable to parse token.\n", l->at.line, l->at.line_offset);
    for (char *c = l->at.c; *c && (c - l->at.c) < 5; ++c) {
        printf("%c", *c);
    }
    printf("...\n");
    next(&l->at);

    return emit(l, TOK_EOF);
}

void reset(Lexer *l, Token token) {
    l->start = token.pos;
    l->at = token.pos;
}

//
// PARSING
//

struct Parser {
    Lexer lexer;
    std::span<char *> errors;
    struct AstBlock *current_block;
};

Token next_token(Parser *parser) { return next_token(&parser->lexer); }

// Attaches the errors of new to the errors of bak, returns bak.
Parser with_error(Parser bak, Parser *errored, const char *format, ...) {
    assert(bak.errors.size() == 0);

    char fmt[4096] = {0};

    auto next = next_token(&bak);
    char *line_start = next.pos.c;
    char *line_end = next.pos.c;
    while (line_start + 1 > bak.lexer.source && *(line_start - 1) != '\n') --line_start;
    while (*line_start != '\n' && (*line_start == ' ' || *line_start == '\t')) ++line_start;
    while (*line_end != '\n' && (line_end - bak.lexer.source.data()) <= bak.lexer.source.size()) ++line_end;
    char line[1024] = {0};
    strncpy(line, line_start, line_end - line_start);  // TODO: Max 1024
    
    snprintf(
        fmt, 
        sizeof(fmt), 
        "Parser error at %d:%d (line: '%s'): %s",
        (int)bak.lexer.at.line,
        (int)bak.lexer.at.line_offset,
        line,
        format);

    va_list va;
    va_start(va, format);
    char msg[4096] = {0};
    vsnprintf(msg, sizeof(msg), fmt, va);
    va_end(va);

    if (errored != nullptr && errored->errors.size() > 0) {
        size_t errors_len = 1 + errored->errors.size();
        auto es = static_cast<char **>(talloc(sizeof(char *) * errors_len));
        bak.errors = std::span{es, errors_len};
        bak.errors[0] = static_cast<char *>(talloc(strlen(msg) + 1));
        strcpy(bak.errors[0], msg);

        for (int i = 0; i < errored->errors.size(); ++i) {
            bak.errors[1 + i] = (char *)talloc(strlen(errored->errors[i]) + 1);
            strcpy(bak.errors[1 + i], errored->errors[i]);
        }
    } else {
        size_t errors_len = 1;
        auto es = static_cast<char **>(talloc(sizeof(char *) * errors_len));
        bak.errors = std::span{es, errors_len};
        bak.errors[0] = static_cast<char *>(talloc(strlen(msg) + 1));
        strcpy(bak.errors[0], msg);
    }

    return bak;
}

Parser eat_token(Parser p, TokenType type, Token *token) {
    auto bak = p;
    auto t = next_token(&p);
    if (t.type != type) {
        char tkn[1024];
        size_t len = sizeof(tkn);
        if (t.len < len) len = t.len;
        strncpy(tkn, t.pos.c, len);
        return with_error(bak, nullptr, "Expected %s, got %s (%s)", to_string(type), to_string(t.type), tkn);
    }

    if (token != nullptr) {
        *token = t;
    }

    return p;
}

Parser eat_token(Parser p, char type, Token *token) { return eat_token(p, static_cast<TokenType>(type), token); }
Parser eat_token(Parser p, char type) { return eat_token(p, static_cast<TokenType>(type), nullptr); }
Parser eat_token(Parser p, TokenType type) { return eat_token(p, type, nullptr); }

Parser eat_keyword(Parser p, const char *kw) {
    auto bak = p;
    auto t = next_token(&p);
    if (t.type != TOK_KEYWORD) {
        return with_error(bak, nullptr, "Expected keyword '%s', got %s", kw, to_string(t.type));
    }

    auto len = strlen(kw);
    if (t.len < len) { len = t.len; }
    if (strncmp(t.pos.c, kw, len) != 0) {
        return with_error(bak, nullptr, "Expected keyword '%s', got %s", kw, to_string(t.type));
    }

    return p;
}

bool advance(Parser *p, Parser parsed, bool require) {
    assert(p->errors.size() == 0);

    if (parsed.errors.size() != 0 || parsed.lexer.at.c <= p->lexer.at.c) {
        if (require) {
            p->errors = parsed.errors;
        }

        return false;
    }

    *p = parsed;
    return true;
}

bool must(Parser *p, Parser parsed) { return advance(p, parsed, true); }
bool maybe(Parser *p, Parser parsed) { return advance(p, parsed, false); }

enum AstKind {
    AST_BIN_OP,
    AST_BLOCK,
    AST_DECL,
    AST_IDENT,
    AST_LITERAL,
    AST_ARG,
    AST_PROC,
    AST_PROC_CALL,
    AST_PROC_SIGNATURE,
    AST_PROGRAM,
    AST_RETURN,
};

const char *to_string(AstKind kind) {
    switch (kind) {
    case AST_BIN_OP:         return "AST_BIN_OP";
    case AST_BLOCK:          return "AST_BLOCK";
    case AST_DECL:           return "AST_DECL";
    case AST_IDENT:          return "AST_IDENT";
    case AST_LITERAL:        return "AST_LITERAL";
    case AST_ARG:            return "AST_PARAM";
    case AST_PROC:           return "AST_PROC";
    case AST_PROC_CALL:      return "AST_PROC_CALL";
    case AST_PROC_SIGNATURE: return "AST_PROC_SIGNATURE";
    case AST_PROGRAM:        return "AST_PROGRAM";
    case AST_RETURN:         return "AST_RETURN";
    default: assert(false);
    }
}

struct AstNode {
    AstNode() = delete;
    explicit AstNode(AstKind kind) 
        : kind{kind} {}

    AstKind kind;
};

struct AstDecl : AstNode {
    AstDecl() : AstNode(AST_DECL) {}

    Token ident{};
    AstNode *init_expr{};

    // Compiler information
    struct AstBlock *block{};
    bool is_global{};
    int64_t address{};  // In case of a procedure-local declaration this is the offset from RSP after the the locals have been allocated (so it is always going to be a negative number). In case of a global declaration (is_global == false), this is the address of the symbol relative to the main memory start.
    /* size_t global_address; */
};

struct AstBinOp : AstNode {
    AstBinOp() : AstNode(AST_BIN_OP) {}

    AstNode *lhs;
    AstNode *rhs;
    TokenType binop;
};

struct AstArg : AstNode {
    AstArg() : AstNode(AST_ARG) {}

    Token ident;
    Token type;
};

enum AstLiteralType {
    LIT_NONE,
    LIT_INT,
    LIT_FLOAT,
    LIT_STRING,
};

struct AstLiteral : AstNode {
    AstLiteral() : AstNode(AST_LITERAL) {}

    Token token;
    AstLiteralType type;
    int64_t int_value;
};

struct AstProcSignature : AstNode {
    AstProcSignature() : AstNode(AST_PROC_SIGNATURE) {}

    std::vector<AstArg> arguments;
};

struct AstBlock : AstNode {
    AstBlock() : AstNode(AST_BLOCK) {}

    std::vector<AstNode *> statements;

    // Compiler information
    AstBlock *parent_block;
    /* size_t base_address; */
};

struct AstReturn : AstNode {
    AstReturn() : AstNode(AST_RETURN) {}

    AstNode *expr;
};

struct AstIdent : AstNode {
    AstIdent() : AstNode(AST_IDENT) {}

    Token ident;
};

struct AstProc : AstNode {
    AstProc() : AstNode(AST_PROC) {}

    AstProcSignature signature;
    AstBlock body;
};

struct AstProcCall : AstNode {
    AstProcCall() : AstNode(AST_PROC_CALL) {}

    AstNode *proc;
    std::vector<AstNode *> arguments;
};

struct AstProgram : AstNode {
    AstProgram() : AstNode(AST_PROGRAM) {}

    AstBlock block;
};

Parser parse_expr(Parser p, AstNode **out_expr);
Parser parse_decl(Parser p, AstDecl *out_decl);

Parser parse_statement(Parser p, AstNode **out_statement) {
    auto bak = p;
    const char *err = "Failed to parse statement";

    AstDecl decl{};
    if (maybe(&p, parse_decl(p, &decl))) {
        auto result = new AstDecl{};
        *result = decl;
        *out_statement = result;
        return p;
    }

    if (maybe(&p, eat_keyword(p, "return"))) {
        AstReturn *ret = new AstReturn;
        if (must(&p, parse_expr(p, &ret->expr)) == false) {
            delete ret;
            return with_error(bak, &p, err);
        }

        *out_statement = ret;
        return p;
    }

    AstNode *expr;
    if (maybe(&p, parse_expr(p, &expr))) {
        *out_statement = expr;
        return p;
    }

    return with_error(bak, nullptr, err);
}

Parser parse_block(Parser p, AstBlock *out_block) {
    auto bak = p;
    const char *err = "Failed to parse block";

    out_block->parent_block = p.current_block;
    p.current_block = out_block;
    defer { p.current_block = out_block->parent_block; };

    if (must(&p, eat_token(p, '{')) == false) {
        return with_error(bak, &p, err);
    }

    while (true) {
        if (maybe(&p, eat_token(p, '}'))) {
            return p;
        }

        AstNode *stmt = nullptr;
        if (must(&p, parse_statement(p, &stmt)) == false) {
            return with_error(bak, &p, err);
        }

        out_block->statements.push_back(stmt);
    }

    return p;
}

Parser parse_primary_expr(Parser p, AstNode **out_primary_expr) {
    auto bak = p;
    const char *err = "Failed to parse primary expression";

    Token t;
    if (maybe(&p, eat_token(p, TOK_NUM_LIT, &t))) {
        AstLiteral *literal = new AstLiteral;
        literal->token = t;
        literal->type = LIT_INT;

        auto result = std::from_chars(t.pos.c, t.pos.c + t.len, literal->int_value);
        if (result.ec != std::errc{}) {
            return with_error(bak, nullptr, "Failed to parse integer literal");
        }

        if (result.ptr != t.pos.c + t.len) {
            return with_error(bak, nullptr, "Failed to parse integer literal");
        }

        *out_primary_expr = literal;
        return p;
    }

    if (maybe(&p, eat_token(p, TOK_IDENT, &t))) {
        AstIdent *ident = new AstIdent;
        ident->ident = t;
        *out_primary_expr = ident;
        return p;
    }

    if (maybe(&p, eat_keyword(p, "proc"))) {
        if (must(&p, eat_token(p, '(')) == false) {
            return with_error(bak, &p, err);
        }

        auto proc = new AstProc;
        while (true) {
            bool done = false;
            auto bak = p;
            switch (next_token(&p).type) {
                case ',': break;  // TODO: Allows for ',' at beginning of parameter list
                case ')': done = 1; break;
                default: p = bak; break;
            }

            if (done) break;

            AstArg arg = {};
            if (must(&p, eat_token(p, TOK_IDENT, &arg.ident)) == false) {
                return with_error(bak, &p, err);
            }

            if (must(&p, eat_token(p, TOK_IDENT, &arg.type)) == false) {
                return with_error(bak, &p, err);
            }
           
            proc->signature.arguments.push_back(arg);
        }

        if (must(&p, parse_block(p, &proc->body)) == false) {
            return with_error(bak, &p, err);
        }

        *out_primary_expr = proc;
        return p;
    }

    return with_error(bak, nullptr, err);
}

Parser parse_suffix_expr(Parser p, AstNode *lhs, AstNode **node) {
    const char *err = "Failed to parse suffix expression";
    auto bak = p;

    if (maybe(&p, eat_token(p, '('))) {
        auto call = new AstProcCall;
        call->proc = lhs;
        
        auto is_end = false;
        while (true) {
            if (advance(&p, eat_token(p, ')'), is_end)) {
                break;
            } else if (is_end) {
                return with_error(bak, &p, err);
            }

            AstNode *argument;
            if (must(&p, parse_expr(p, &argument)) == false) {
                return with_error(bak, &p, err);
            }

            call->arguments.push_back(argument);

            if (maybe(&p, eat_token(p, ',')) == false) {
                is_end = true;
            }
        }

        *node = call;
        return p;
    }

    return with_error(bak, nullptr, err);
}

Parser parse_binary_expr(Parser p, AstNode **node, int prev_prec) {
    auto bak = p;
    const char *err = "Failed to parse expression";

    AstNode *lhs;
    if (must(&p, parse_primary_expr(p, &lhs)) == false) {
        return with_error(bak, &p, err);
    }
    maybe(&p, parse_suffix_expr(p, lhs, &lhs));

    while (true) {
        auto before_op = p;
        auto op = next_token(&p);
        auto prec = binop_prec(op.type);

        if (prec == 0 || prec <= prev_prec) {
            *node = lhs;
            p = before_op;
            return p;
        }

        AstNode *rhs;
        if (must(&p, parse_binary_expr(p, &rhs, prec)) == false) {
            return with_error(bak, &p, "Failed to parse binary operator right hand side");
        }

        auto bin_op = new AstBinOp;
        bin_op->binop = op.type;
        bin_op->lhs = lhs;
        bin_op->rhs = rhs;

        lhs = bin_op;
    }

    *node = lhs;

    return p;
}

Parser parse_expr(Parser p, AstNode **node) {
    return parse_binary_expr(p, node, -1);
}

Parser parse_decl(Parser p, AstDecl *decl) {
    auto bak = p;
    const char *err = "Failed to parse declaration";

    Token ident;
    if (must(&p, eat_token(p, TOK_IDENT, &ident)) == false) {
        return with_error(bak, &p, err);
    }

    if (must(&p, eat_token(p, TOK_DECLASSIGN)) == false) {
        return with_error(bak, &p, err);
    }

    AstNode *init_expr;
    if (must(&p, parse_expr(p, &init_expr)) == false) {
        return with_error(bak, &p, err);
    }

    decl->ident = ident;
    decl->init_expr = init_expr;

    return p;
}

Parser parse_program(Parser p, AstProgram *prog) {
    const char *err = "Failed to parse program";
    auto bak = p;

    while (true) {
        auto decl = new AstDecl{};
        if (must(&p, parse_decl(p, decl)) == false) {
            return with_error(bak, &p, err);
        }

        decl->is_global = true;
        prog->block.statements.push_back(decl);

        // Reset the arena
        arena.cursor = arena.memory_start;

        Token next = next_token(&p);
        if (next.type == TOK_EOF) {
            return p;
        }

        reset(&p.lexer, next);
    }

    return p;
}

//
// STRINGIFY
//

template<typename ...Args>
std::string indent(int indent_level, std::string_view text) {
    std::string indent = std::string(indent_level * 4, ' ');
    return std::format("{}{}", indent, text);
}

std::string dump_node(int indent_level, AstNode *node);

std::string to_string(int indent_level, AstBinOp *node) {
    std::string result;

    result += indent(indent_level, "BinOp(\n");
    result += std::format("{}\n", indent(indent_level + 1, std::string{(char)node->binop}));
    result += std::format("{}\n", dump_node(indent_level + 1, node->lhs));
    result += std::format("{}\n", dump_node(indent_level + 1, node->rhs));
    result += indent(indent_level, ")");

    return result;
}

std::string to_string(int indent_level, AstBlock *node) {
    std::string result;

    result += indent(indent_level, "Block(\n");

    for (AstNode *statement : node->statements) {
        result += dump_node(indent_level + 1, statement);
        result += "\n";
    }

    result += indent(indent_level, ")");

    return result;
}

std::string to_string(int indent_level, AstDecl *node) {
    std::string result;

    result += indent(indent_level, "Decl(\n");
    result += std::format("{}\n", indent(indent_level + 1, text_of(&node->ident)));
    result += std::format("{}\n", dump_node(indent_level + 1, node->init_expr));
    result += indent(indent_level, ")");

    return result;
}

std::string to_string(int indent_level, AstIdent *node) {
    std::string result;

    result += indent(indent_level, std::format("Ident({})", text_of(&node->ident)));

    return result;
}

std::string to_string(int indent_level, AstLiteral *node) {
    std::string result;

    result += indent(indent_level, std::format("Literal({})", text_of(&node->token)));

    return result;
}

std::string to_string(int indent_level, AstArg *node) {
    std::string result;

    result += indent(indent_level, "Arg(\n");
    result += std::format("{}\n", indent(indent_level + 1, text_of(&node->ident)));
    result += std::format("{}\n", indent(indent_level + 1, text_of(&node->type)));
    result += indent(indent_level, ")");

    return result;
}

std::string to_string(int indent_level, AstProc *node) {
    std::string result;

    result += indent(indent_level, "Proc(\n");
    result += std::format("{}\n", dump_node(indent_level + 1, &node->signature));
    result += std::format("{}\n", dump_node(indent_level + 1, &node->body));
    result += indent(indent_level, ")");

    return result;
}

std::string to_string(int indent_level, AstProcCall *node) {
    std::string result;

    result += indent(indent_level, "ProcCall(\n");
    result += std::format("{}\n", indent(indent_level + 1, "Proc:"));
    result += std::format("{}\n", dump_node(indent_level + 2, node->proc));
    result += std::format("{}\n", indent(indent_level + 1, "Args:"));

    for (AstNode *arg : node->arguments) {
        result += std::format("{}\n", dump_node(indent_level + 2, arg));
    }

    result += indent(indent_level, ")");

    return result;
}

std::string to_string(int indent_level, AstProcSignature *node) {
    std::string result;

    result += indent(indent_level, "ProcSignature(\n");

    for (AstArg &arg : node->arguments) {
        result += std::format("{}\n", dump_node(indent_level + 1, &arg));
    }

    result += indent(indent_level, ")");

    return result;
}

std::string to_string(int indent_level, AstProgram *node) {
    std::string result;

    result += indent(indent_level, "Program(\n");

    for (AstNode *decl : node->block.statements) {
        result += dump_node(indent_level + 1, decl);
        result += "\n";
    }

    result += indent(indent_level, ")");

    return result;
}

std::string to_string(int indent_level, AstReturn *node) {
    std::string result;

    result += indent(indent_level, "Return(\n");
    result += std::format("{}\n", dump_node(indent_level + 1, node->expr));
    result += indent(indent_level, ")");

    return result;
}

std::string dump_node(int indent_level, AstNode *node) {
    switch (node->kind) {
    case AST_BIN_OP: return to_string(indent_level, static_cast<AstBinOp *>(node));
    case AST_BLOCK: return to_string(indent_level, static_cast<AstBlock *>(node));
    case AST_DECL: return to_string(indent_level, static_cast<AstDecl *>(node));
    case AST_IDENT: return to_string(indent_level, static_cast<AstIdent *>(node));
    case AST_LITERAL: return to_string(indent_level, static_cast<AstLiteral *>(node));
    case AST_ARG: return to_string(indent_level, static_cast<AstArg *>(node));
    case AST_PROC: return to_string(indent_level, static_cast<AstProc *>(node));
    case AST_PROC_CALL: return to_string(indent_level, static_cast<AstProcCall *>(node));
    case AST_PROC_SIGNATURE: return to_string(indent_level, static_cast<AstProcSignature *>(node));
    case AST_PROGRAM: return to_string(indent_level, static_cast<AstProgram *>(node));
    case AST_RETURN: return to_string(indent_level, static_cast<AstReturn *>(node));
    default: assert(false);
    }
}

//
// COMPILE
//

enum OpCode : uint8_t {
    // Memory operations
    PUSHC,  // value = read i64; push i64 value;
    LOADR,  // offset = read i64; push i64 *(RSP + offset);
    LOAD,   // addr = read i64; push i64 *addr;
    /* STORE,  // addr = read i64; value = pop i64; *addr = value; */
    STORER,  // offset = read i64; value = pop i64; *(RSP + offset) = value;

    // Integer arithmetic operations
    ADD,  // a = pop i64; b = pop i64; push i64 a + b
    SUB,  // a = pop i64; b = pop i64; push i64 a - b
    MUL,  // a = pop i64; b = pop i64; push i64 a * b
    DIV,  // a = pop i64; b = pop i64; push i64 a / b
    MOD,  // a = pop i64; b = pop i64; push i64 a % b

    // Jumping and branching
    CMP,   // a = pop i64; b = pop i64; ...compare a and b and set status registers
    JMP,   // dst = read i64; jmp dst;
    JEQ,   // dst = read i64; if previous CMP yielded a == b: jmp dst;
    JNE,   // dst = read i64; if previous CMP yielded a != b: jmp dst;
    JLT,   // dst = read i64; if previous CMP yielded a < b: jmp dst;
    JLE,   // dst = read i64; if previous CMP yielded a <= b: jmp dst;
    JGT,   // dst = read i64; if previous CMP yielded a > b: jmp dst;
    JGE,   // dst = read i64; if previous CMP yielded a >= b: jmp dst;
    CALL,  // push i64 rip; dst = read i64; jmp dst;
    RET,   // value = pop i64; ret = pop i64; push i64 value; jmp ret;
};

const char *to_string(OpCode op) {
    switch (op) {
        case PUSHC: return "PUSHC";
        case LOADR: return "LOADR";
        case LOAD: return "LOAD";
        case STORER: return "STORER";

        case ADD: return "ADD";
        case SUB: return "SUB";
        case MUL: return "MUL";
        case DIV: return "DIV";
        case MOD: return "MOD";

        case CMP: return "CMP";
        case JMP: return "JMP";
        case JEQ: return "JEQ";
        case JNE: return "JNE";
        case JLT: return "JLT";
        case JLE: return "JLE";
        case JGT: return "JGT";
        case JGE: return "JGE";
        case CALL: return "CALL";
        case RET: return "RET";
    }

    assert(false);
};

AstDecl *find_decl(AstBlock *block, std::string_view name) {
    for (auto statement : block->statements) {
        if (statement->kind != AST_DECL) continue;

        auto decl = static_cast<AstDecl *>(statement);
        if (text_of(&decl->ident) == name) {
            return decl;
        }
    }

    if (block->parent_block != nullptr) {
        return find_decl(block->parent_block, name);
    }

    return nullptr;
}

struct SymbolAddressPlaceholder {
    std::string symbol_name;
    size_t bytecode_offset;
};

struct BytecodeWriter {
    std::vector<uint8_t> bytecode;
    AstBlock *current_block;
    std::vector<SymbolAddressPlaceholder> saps;

    bool generate_asm;
    std::string asm_source;
};

/* size_t absolute_address(AstBlock *block) { */
/*     auto result = block->base_address; */

/*     if (block->parent_block != nullptr) { */
/*         result += absolute_address(block->parent_block); */
/*     } */

/*     return result; */
/* } */

/* size_t absolute_address(AstDecl *decl) { */
/*     assert(decl->block != nullptr); */

/*     auto result = absolute_address(decl->block); */
/*     result += decl->block_offset; */

/*     return result; */
/* } */

void write(uint8_t *data, size_t len, BytecodeWriter *w) {
    w->bytecode.insert(w->bytecode.end(), data, data + len);
}

void _write8(uint8_t value, BytecodeWriter *w) {
    write(&value, sizeof(value), w);
}

void _write64(int64_t value, BytecodeWriter *w) {
    write((uint8_t *)&value, sizeof(value), w);
}

void write_op(OpCode op, BytecodeWriter *w) {
    if (w->generate_asm) {
        w->asm_source += std::format("{}\n", to_string(op));
    } else {
        _write8(op, w);
    }
}

void write_op_8(OpCode op, uint8_t value, BytecodeWriter *w) {
    if (w->generate_asm) {
        w->asm_source += std::format("{} {}\n", to_string(op), value);
    } else {
        _write8(op, w);
        _write8(value, w);
    }
}

void write_op_64(OpCode op, int64_t value, BytecodeWriter *w) {
    if (w->generate_asm) {
        w->asm_source += std::format("{} {}\n", to_string(op), value);
    } else {
        _write8(op, w);
        _write64(value, w);
    }
}

void compile_error(BytecodeWriter *w, std::string_view message) {
    printf("Compiler error: %s\n", message.data());
}

bool is_expression(AstKind kind) {
    return 
        kind == AST_BIN_OP ||
        kind == AST_IDENT ||
        kind == AST_LITERAL ||
        kind == AST_PROC_CALL;
}

[[nodiscard]] bool generate_code(AstNode *node, BytecodeWriter *w);

[[nodiscard]] bool generate_expr(AstNode *node, BytecodeWriter *w) {
    if (is_expression(node->kind) == false) {
        return false;
    }

    return generate_code(node, w);
}

[[nodiscard]] bool generate_code(AstNode *node, BytecodeWriter *w) {
    switch (node->kind) {
    case AST_BIN_OP: {
        auto binop = static_cast<AstBinOp *>(node);

        if (generate_expr(binop->lhs, w) == false ||
            generate_expr(binop->rhs, w) == false) {
            return false;
        }

        switch (binop->binop) {
            case TOK_ASTERISK: write_op(MUL, w); break;
            case TOK_SLASH:    write_op(DIV, w); break;
            case TOK_MOD:      write_op(MOD, w); break;
            case TOK_PLUS:     write_op(ADD, w); break;
            case TOK_MINUS:    write_op(SUB, w); break;
            default: assert(false); break;
        }
     } return true;

    case AST_IDENT: {
        /*
        main := proc() {
            // a has ofset -16 from RSP while calling main
            a := 0

            // b has offset -8 from RSP
            b := 123

            if true {
                c := 532

                // a found in parent block
                a = 8
            } else {
                a := 8
            }
        }
        */

        auto ident = static_cast<AstIdent *>(node);

        auto decl = find_decl(w->current_block, text_of(&ident->ident));
        if (decl == nullptr) {
            compile_error(w, std::format("Declaration of {} not found", text_of(&decl->ident)));
            return false;
        }

        assert(decl->address <= 0);  // TODO: This is not right for proc calls and for global variables

        write_op_64(LOADR, decl->address, w);
        /* write64(decl->, w); */  // TODO
    } return true;

    case AST_BLOCK: {
        auto block = static_cast<AstBlock *>(node);

        // TODO: Nested blocks for if-statements and raw blocks etc.

        /*

        a: int32 = 1  // -14 | 0
        b: int64 = 2  // -10 | 4
        c: int16 = 3  // -2  | 12
        RSP = 14

        */

        // Allocate locals
        int64_t cursor = 0;
        for (auto i = 0; i < block->statements.size(); ++i) {
            auto statement = block->statements[block->statements.size() - 1 - i];
            if (statement->kind != AST_DECL) continue;

            auto decl = static_cast<AstDecl *>(statement);

            if (decl->is_global == false) {  // Only if we are not compiling the top level program pseudo-block
                auto size_of_decl = 8;  // TODO
                cursor -= size_of_decl;

                decl->address = cursor;
            }
        }

        auto prev_block = w->current_block;
        w->current_block = block;
        defer { w->current_block = prev_block; };

        for (auto statement : block->statements) {
            if (generate_code(statement, w) == false) {
                return false;
            }
        }
    } return true;

    case AST_DECL: {
        auto decl = static_cast<AstDecl *>(node);

        assert((decl->is_global == false) == (decl->address < 0));

        /* assert(decl->is_global == (w->current_block == nullptr)); */

        if (decl->is_global) {
            auto address = w->bytecode.size();
            if (generate_code(decl->init_expr, w) == false) {
                return false;
            }

            decl->address = address;
        } else {
            if (generate_expr(decl->init_expr, w) == false) {
                return false;
            }

            write_op_64(STORER, decl->address, w);
        }

        return true;
    } break;

    case AST_LITERAL: {
        auto literal = static_cast<AstLiteral *>(node);

        assert(literal->type == LIT_INT);  // TODO
        write_op_64(PUSHC, literal->int_value, w);

        return true;
    } break;

    case AST_ARG: {
        assert(false);
    } break;

    case AST_PROC: {
        // TODO!
        auto proc = static_cast<AstProc *>(node);

        return generate_code(&proc->body, w);
    } return true;

    case AST_PROC_CALL: {
        assert(false);
    } return true;

    case AST_PROC_SIGNATURE: {
        assert(false);
    } return true;

    case AST_PROGRAM: {
        auto program = static_cast<AstProgram *>(node);

        for (auto node : program->block.statements) {
            if (node->kind != AST_DECL) {
                compile_error(w, "Expected declaration");
                return false;
            }

            /* auto decl = static_cast<AstDecl *>(node); */

            /* if (decl->init_expr->kind != AST_PROC) { */
            /*     compile_error(w, "Expected procedure top level declaration"); */
            /*     return false; */
            /* } */

            /* auto proc = static_cast<AstProc *>(decl->init_expr); */
            /* assert(w->current_block == nullptr); */

            return generate_code(&program->block, w);
        }
    } return true;

    case AST_RETURN: {
        auto ret = static_cast<AstReturn *>(node);

        if (is_expression(ret->expr->kind) == false) { assert(false); }

        if (generate_code(ret->expr, w) == false) {
            return false;
        }

        write_op(RET, w);
    } return true;

    default: assert(false);
    }

    return false;
}

//
// MAIN
//

void test() {
    auto source = std::string_view{
R"(
main := proc() {
    a := 1
    b := 2
    c := a + b
    return c
}
)"};

    BytecodeWriter mock;

    write_op_64(PUSHC, 1, &mock);  // Push a's initial value onto the stack
    write_op_64(STORER, -24, &mock);  // Store a at RSP - 8
    write_op_64(PUSHC, 2, &mock);  // Push b's initial value onto the stack
    write_op_64(STORER, -16, &mock);  // Store b at RSP - 16

    write_op_64(LOADR, -24, &mock);  // Push b
    write_op_64(LOADR, -16, &mock);  // Push a
    write_op(ADD, &mock);  // Pop a, pop b, push a + b

    write_op_64(STORER, -8, &mock);  // Store the top of the stack into c

    write_op(RET, &mock);  // Return

    auto lexer = Lexer{.source = source, .at = {.c = const_cast<char *>(source.data())}};

    AstProgram program;
    Parser p{.lexer = lexer};
    if (must(&p, parse_program(p, &program)) == false) {
        assert(false);
    }

    BytecodeWriter w{.generate_asm = true};
    if (generate_code(&program, &w) == false) {
    printf("ASM:\n%s\n", w.asm_source.data());
        assert(false);
    }

    printf("ASM:\n%s\n", w.asm_source.data());

    if (w.bytecode.size() != mock.bytecode.size()) {
        assert(false);
    }

    for (auto i = 0; i < w.bytecode.size(); ++i) {
        if (w.bytecode[i] != mock.bytecode[i]) {
            printf("The program does not match at bytecode index %d\n", (int)i);
            assert(false);
        }
    }

    int x = 0;
}

int main(int argc, char **argv) {
    printf("This is the Janguage compiler.\n");

    arena.memory_start = (char *)malloc(ARENA_SIZE);
    arena.cursor = arena.memory_start;

    test();

    if (argc != 2) {
        fprintf(stderr, "Usage: jang <main source file>\n");
        return 1;
    }

    char *path = argv[1];
    printf("Compiling file: %s\n", path);

    FILE *f = fopen(path, "r");
    if (f == nullptr) {
        fprintf(stderr, "Could not open file %s", path);
        return 1;
    }

    fseek(f, 0, SEEK_END);
    size_t file_size = ftell(f);
    rewind(f);
    char *source = (char *)malloc(file_size + 1);
    source[file_size] = 0;

    size_t pos = 0;
    size_t read = 0;
    for (
        size_t read = 0;
        (read = fread(&source[pos], 1, file_size - pos, f));
        pos += read) {
    }
    fclose(f);

    Lexer l{
        .source = source,
        .start = {.c = source},
        .at = {.c = source},
    };

#if 0
    printf("Dumping tokens\n");
    Token token;
    while (true) {
        token = next_token(&l);
        printf("Line %d char %d: %s ", (int)token.line, (int)token.line_offset, to_string(token.type));
        if (token.len > 0) fwrite(l.start, 1, l.at - l.start, stdout);
        printf("\n");

        if (token.type == TOK_EOF) {
            break;
        }
    }
#else
    AstProgram program = {};
    Parser p{.lexer = l};
    if (must(&p, parse_program(p, &program)) == false) {
        printf("Failed to parse the program\n");
    }

    printf("Program:\n%s\n", dump_node(0, &program).data());

    if (p.errors.size() > 0) {
        for (int i = 0; i < p.errors.size(); ++i) {
            for (int j = 0; j < i * 4; ++j) {
                printf(" ");
            }

            printf("%s\n", p.errors[i]);
        }
    }

    BytecodeWriter w{
        .current_block = &program.block,
    };
    generate_code(&program.block, &w);

    // TODO: Check that we are not at EOF

    /* printf("Got %d declarations\n", (int)program.decls.size()); */
    /* for (int i = 0; i < program.decls.size(); ++i) { */
    /*     printf("Decl: '"); */
    /*     fwrite(program.decls[i].ident.pos.c, 1, program.decls[i].ident.len, stdout); */
    /*     printf("'\n"); */
    /* } */
#endif

    free(source);

    return 0;
}

