#include "parser.h"
#include "lexer.h"
#include "logger.h"
#include "utils.h"
#include "parse_number.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

char *random_string(size_t n) {
    const char charset[] =
        "abcdefghijklmnopqrstuvwxyz"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

    char *str = malloc(n + 1);
    if (!str) return NULL;

    for (size_t i = 0; i < n; i++) {
        int key = rand() % (sizeof(charset) - 1);
        str[i] = charset[key];
    }

    str[n] = '\0';
    return str;
}

Node* new_node(Parser* p) {
    Node* n = (Node*)arena_alloc(&p->arena, sizeof(Node));
    if (!n) {
        panic("Failed to allocate memory for node.");
        return NULL;
    }
    memset(n,0,sizeof(Node));
    return n;
}
Type* new_type(Parser* p) {
    Type* t = (Type*)arena_alloc(&p->arena, sizeof(Type));
    if (!t) {
        panic("Failed to allocate memory for type.");
        return NULL;
    }
    memset(t,0,sizeof(Type));
    return t;
}
Token current(Parser* p) {
    Token t;
    if (p->tokens_index >= p->tokens_count) {
        t = (Token){ .type=TokenEOF };
    } else {
        t = p->l->tokens[p->tokens_index];
        // p->tokens_index += 1;
    }
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
Node* parse_symbol(Parser *p) {
    if (current(p).type != TokenIdent) {
        err("expected ident/module acces, got %d.", current(p).type);
        return NULL;
    }
    Node* n = new_node(p);
    if (!n) {
        panic("Failed to allocate memory.");
        return NULL;
    }
    n->kind = NodeSymbol;
    n->token = consume(p);
    n->symbol = n->token.ident;
    return n;
}
Node* parse_path(Parser *p) {
    Node* n = parse_symbol(p);
    if (!n) {
        panic("Failed to parse symbol.");
        return NULL;
    }
    while (current(p).type == TokenDoubleColon) {
        Node* mod_access_n = new_node(p);
        if (!mod_access_n) {
            panic("Failed to allocate memory.");
            return NULL;
        }
        mod_access_n->token = consume(p);
        if (current(p).type != TokenIdent) {
            err("Expected ident, got %d.", current(p).type);
            return NULL;
        }
        mod_access_n->module_access.module = n;
        mod_access_n->module_access.target = consume(p).ident;
        n = mod_access_n;
    }
    return n;
}
// ptrs: "*u32"
// types: "i32"
// module types: "mod::u32"
Node* parse_type(Parser *p) {
    Node* n = new_node(p);
    if (!n) {
        panic("Failed to allocate memory.");
        return NULL;
    }
    n->kind = NodeTypeData;
    if (current(p).type == TokenStar) { // it's a pointer
        Type* t = new_type(p);
        n->token = consume(p);
        if (!t) {
            panic("Failed to allocate memory.");
            return NULL;
        }
        Node* target = parse_type(p);
        if (!target) {
            panic("Failed to parse ptr target.");
            return NULL;
        }
        t->kind = tt_ptr;
        t->ptr = target->type_data;
        n->type_data = t;
    } else if (current(p).type == TokenIdent) {
        Node* type_name = parse_path(p);
        if (!type_name) {
            panic("Failed to parse type atom(?).");
            return NULL;
        }
        n->token = type_name->token; // same token
        Type* t = new_type(p);
        if (!t) {
            panic("Failed to allocate memory.");
            return NULL;
        }
        t->kind = tt_to_determinate;
        t->symbol = type_name;
        n->type_data = t;
    }
    return NULL;
}
Node* parse_var_dec(Parser *p) {
    if (current(p).type != TokenIdent) {
        err("Expected ident, got %d.", current(p).type);
        return NULL;
    }
    Node* n = new_node(p);
    if (!n) {
        panic("Failed to allocate memory.");
        return NULL;
    }
    n->kind = NodeSymbol;
    n->token = consume(p);
    n->symbol = n->token.ident;
    if (current(p).type == TokenColon) {
        // parse type
    } else if (current(p).type == TokenDoubleColon) {
        // parse constant?
    } else {
        panic("Expected \":\" or \"::\" (for constants) for variable "
                "declaration.");
    }
    return NULL;
}
Node* parse_tls(Parser *p) {
    if (current(p).type == TokenKeyword) {
        Node* n;
        switch (current(p).kw) {
            default: panic("handle");
        }
        if (!n) {
            err("Failed to parse statement.");
            return NULL;
        }
    } else if (current(p).type == TokenIdent) {
        // vardec
        if (peek(p).type == TokenColon || peek(p).type == TokenDoubleColon){
            return parse_var_dec(p);
        } else {
            panic("Invalid token %d", current(p).type);
            return NULL;
        }
    } else {
        err("unexpected token.");
    }
    return NULL;
}
Node* parse(Parser *p) {
    size_t cap = 128;
    Node** nodes = malloc(cap*sizeof(Node*));
    if (!nodes) {
        err("FAiled to allocate nodes.");
        return NULL;
    }
    size_t count = 0;
    while (current(p).type != TokenEOF) {
        nodes[count] = 0;
        nodes[count] = parse_tls(p);
        if (!nodes[count]) {
            panic("Failed to parse tls.");
            return 0;
        }
        count++;
        // expand if necessary
        if (count >= cap) {
            Node** tmp = realloc(nodes, (cap*=2) * sizeof(Node*));
            if (!tmp) {
                panic("Failed to reallocate nodes.");
                return 0;
            }
        }
    }

    p->nodes = nodes;
    p->nodes_count = count;
    p->nodes_cap = cap;
    return NULL;
}
#define CODE_LEN 32
Parser* pctx_new(Lexer* l, char* path) {
    char* p_code = random_string(CODE_LEN);
    Span name = get_name_from_path(path);
    if (name.name == 0 || name.length == 0) {
        err("failed to get name from path \"%s\".");
        return NULL;
    }

    Parser* p = malloc(sizeof(Parser));
    if (!p) {
        panic("Failed to allocate parser.");
        return NULL;
    }
    memset(p, 0, sizeof(Parser));
    p->arena = arena_new(1024, sizeof(Node));
    p->path = path;
    p->l = l;
    p->module_name = name;
    // module coode/id ig
    p->module_code.name = p_code;
    p->module_code.length = CODE_LEN;
    return p;
};
int parser_destry(Parser *p) {
    dbg("Freeing pctx.");
    if (!p) return 0;
    if (p->nodes) {
        free(p->nodes);
        p->nodes = 0;
    }
    if (p->l) {
        lexer_free(p->l);
        p->l = 0;
    }

    // arena
    if (p->arena.pages) {
        for (size_t i = 0; i < p->arena.pages_count; i++) {
            free(p->arena.pages[i]);
        }
        free(p->arena.pages);
    }

    if (p->module_code.name) {
        free(p->module_code.name); // heap allocated
        p->module_code.name = 0; // heap allocated
    }

    free(p);
    return 1;
}

#define TYPE(t, tsize)  (Type){.kind=tt_##t, .size=tsize\
    , .name=(Span){(char*)#t, sizeof(#t) - 1}},
static Type  base_types[] = {
    TYPE(fn,    ptr_size) // it's a pointer
    TYPE(u8,    1)
    TYPE(u16,   2)
    TYPE(u32,   4)
    TYPE(u64,   8)
    TYPE(u128,  16)
    TYPE(i8,    1)
    TYPE(i16,   2)
    TYPE(i32,   4)
    TYPE(i64,   8)
    TYPE(i128,  16)
    TYPE(f32,   4)
    TYPE(f64,   8)
    // TYPE(ptr,   ptr_size)
    TYPE(usize, ptr_size)
    TYPE(char, 1)
    TYPE(void,  0)
    // TYPE(none,  0)
};
#undef TYPE

