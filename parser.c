#include "parser.h"
#include "lexer.h"
#include "logger.h"
#include "utils.h"
#include "parse_number.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

typedef enum {
    NodeCount, // counut
} NodeKind;

struct Symbol {

};
struct Node {
    Symbol symbl;
    NodeKind kind;
};


Node* new_node(Arena* a) {
    return (Node*)arena_alloc(a, sizeof(Node));
}
struct Parser {
    Node** nodes;
    size_t nodes_count;
    size_t nodes_cap;
    Lexer* l;
    size_t tokens_count, tokens_index;
};
Token current(Parser* p) {
    Token t;
    if (p->tokens_index >= p->tokens_count) {
        t = (Token){ .type=TokenEOF };
    } else {
        t = p->l->tokens[p->tokens_index];
        // p->tokens_index += 1;
    }
    // printf("Token %s\n", get_token_data(t));
    return t;
}
Token peek(Parser* p) {
    Token t;
    if (p->tokens_index + 1 >= p->tokens_count) {
        t = (Token){ .type=TokenEOF };
    } else {
        t = p->l->tokens[p->tokens_index + 1];
    }

    return t;

}
Token consume(Parser* p) {
    Token t;
    if (p->tokens_index >= p->tokens_count) {
        t = (Token){ .type=TokenEOF };
    } else {
        t = p->l->tokens[p->tokens_index];
        // info("\t\tconsume %s", get_token_data(t));
        p->tokens_index += 1;
    }
    return t;
}
Node* parse_let(Parser *p) {
    return NULL;
}
Node* parse(Parser *p) {
    Node** nodes = malloc(128*sizeof(Node*));
    if (!nodes) {
        err("FAiled to allocate nodes.");
        return NULL;
    }
    while (current(p).type != TokenEOF) {
        if (current(p).type != TokenKeyword) {
            err("expected keyword.");
            return 0;
        }
        Node* n;
        switch (current(p).kw) {
            case KwLet: n = parse_let(p); break;
            default: panic("handle");
        }
        if (!n) {
            err("Failed to parse statement.");
            return NULL;
        }
    }

    return NULL;
}
int parser_destry(Parser *p) {
    free(p->nodes);
    p->nodes = 0;
    return lexer_free(p->l);
}
