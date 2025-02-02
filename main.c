#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
    TOK_ASTERISK = '*',
    TOK_SLASH = '/',
    TOK_PLUS = '+',
    TOK_MINUS = '-',
    TOK_EQUALS = '=',
    TOK_COMMA = ',',
    TOK_OPENPAREN = '(',
    TOK_CLOSEPAREN = ')',
    TOK_OPENBRACE = '{',
    TOK_CLOSEBRACE = '}',
    TOK_IDENT = 255,
    TOK_NUM_LIT,
    TOK_DECLASSIGN,  // :=
    TOK_EOF,
    /* TOK_OPENBRACKET = '[', */
    /* TOK_CLOSERACKET = ']', */
} TokenType;

char *token_type_to_string(TokenType t) {
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
        case TOK_NUM_LIT:    return "TOK_NUM_LIT";
        case TOK_DECLASSIGN: return "TOK_DECLASSIGN";
        case TOK_EOF:        return "TOK_EOF";
        default:             return "(unknown!)";
    }
}

typedef struct {
    TokenType type;
    char *start;
    size_t len;
} Token;

typedef struct {
    char *source;
    size_t source_len;
    char *start;
    char *at;
} Lexer;

Token emit(Lexer *l, TokenType t) {
    assert(t == TOK_EOF || l->at > l->start);

    return (Token){
        .type = t,
        .start = l->start,
        .len = l->at - l->start,
    };
}

// TODO: Inline
int eat_seq(Lexer *l, char *txt) {
    size_t len = strlen(txt);
    if (l->at - l->source + len > l->source_len) {
        return 0;
    }

    for (int i = 0; i < len; ++i) {
        if (l->at[i] != txt[i]) {
            return 0;
        }
    }

    l->at += len;

    return 1;

    /* char *c = l->at; */
    /* for (; *c == *txt && c - l->at <= len; ++c, ++txt) { */
    /* } */

    /* printf("txt - c = %d\n", (int)(c - txt)); */
    /* return c == txt; */
}

int is_ident(char c) {
    return c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z';
}

int is_digit(char c) {
    return c >= '0' && c <= '9';
}

Token next_token(Lexer *l) {
    for (;*l->at == ' ' || *l->at == '\t' || *l->at == '\n'; ++l->at) {
    }

    l->start = l->at;

    for (; is_ident(*l->at); ++l->at) { }
    if (l->at > l->start) {
        return emit(l, TOK_IDENT);
    }

    for (; is_digit(*l->at); ++l->at) { }
    if (l->at > l->start) {
        return emit(l, TOK_NUM_LIT);
    }

    if (eat_seq(l, ":=")) { return emit(l, TOK_DECLASSIGN); }

    char single_char_tokens[] = {'*', '/', '+', '-', ',', '(', ')', '{', '}', '='};
    for (int i = 0; i < sizeof(single_char_tokens); ++i) {
        if (*l->at == single_char_tokens[i]) {
            ++l->at;
            return emit(l, single_char_tokens[i]);
        }
    }

    if (*l->at == 0) {
        return emit(l, TOK_EOF);
    }

    printf("Lexer error at %d.", (int)(l->at - l->source));
    return emit(l, TOK_EOF);
}

int main(int argc, char **argv) {
    printf("This is the Janguage compiler.\n");

    if (argc != 2) {
        fprintf(stderr, "Usage: jang <main source file>\n");
        return 1;
    }

    char *path = argv[1];
    FILE *f = fopen(path, "r");
    if (f == NULL) {
        fprintf(stderr, "Could not open file %s", path);
        return 1;
    }

    fseek(f, 0, SEEK_END);
    size_t file_size = ftell(f);
    rewind(f);
    char *source = malloc(file_size + 1);
    source[file_size] = 0;

    size_t pos = 0;
    size_t read = 0;
    for (size_t read = 0; (read = fread(&source[pos], 1, file_size - pos, f)); pos += read) {
    }
    fclose(f);

    size_t num_tokens;
    Lexer l = {
        .source = source,
        .source_len = file_size,
        .start = source,
        .at = source,
    };

    Token token;
    while (token = next_token(&l), token.type != TOK_EOF) {
        printf("Token: %s (", token_type_to_string(token.type));
        fwrite(l.start, 1, l.at - l.start, stdout);
        printf(")\n");
    }

    free(source);

    return 0;
}

