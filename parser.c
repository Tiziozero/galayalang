#include "parser.h"
#include "lexer.h"
#include "logger.h"
#include "utils.h"
#include <assert.h>
#include <complex.h>
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
Node* parse_scope(Parser *p);
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
    n->ident = n->token.ident;
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
        mod_access_n->kind = NodeModuleAccess;
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
        if (target->kind != NodeTypeData) {
            panic("expected type data for ptr target.");
            return NULL;
        }
        t->kind = tt_ptr;
        t->ptr = target->type_data;
        n->type_data = t;
    } else if (current(p).type == TokenKeyword) {
        if (current(p).kw == KwFn) {
            // parse fn
            TODO("Parse Fn type.");
        }
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
        t->ident = type_name;
        n->type_data = t;
    } else {
        panic("unexpected token in parse type %s.", get_token_data(current(p)));
        return NULL;
    }
    return n;
}
Node* parse_var_dec(Parser *p) {
    Node* symbol = parse_symbol(p);
    if (!symbol) {
        err("Failed to parse symbol.");
        return NULL;
    }
    Node* n = new_node(p);
    if (!n) {
        panic("Failed to allocate memory.");
        return NULL;
    }
    n->kind = NodeVarDec;
    n->token = symbol->token;
    n->var_dec.ident = symbol; // set symbol
    if (current(p).type == TokenColon) {
        Token colon = consume(p);
        // parse type
        Node* type = parse_type(p);
        if (!type) {
            panic("Failed to parse type.");
            return NULL;
        }
        n->var_dec.type = type;
        switch (current(p).type) {
            case TokenAssign: /* variable */
                n->kind = NodeVarDec;
                n->var_dec.is_const = 0;
                break;
            case TokenColon: /* const */
                n->kind = NodeConstDec;
                n->var_dec.is_const = 1;
                break;
            case TokenSemicolon: /* no value, just vardec */
                n->kind = NodeVarDec;
                n->var_dec.is_const = 0;
                n->var_dec.value = NULL;
                break;
            default:
                err("Expected \"=\" (or \":\" for constants), "
                        "got %s.", get_token_data(current(p)));
                return NULL;
        }
        consume(p); // "="/":"
        Node* expr_n = parse_expression(p);
        if (!expr_n) {
            err("Failed to parse expression.");
            return NULL;
        }
        n->var_dec.value = expr_n;
    } else if (current(p).type == TokenDoubleColon) {
        // parse constant?
        TODO("handle ::");
    } else if (current(p).type == TokenColonEqual) {
        // inference
        TODO("handle :=");
    } else {
        panic("Expected \":\" or \"::\" (for constants) for variable "
                "declaration.");
    }
    if (current(p).type != TokenSemicolon) {
        err("Expected \";\" , got %s.", get_token_data(current(p)));
        return NULL;
    }
    consume(p); // ";"
    return n;
}
Node* parse_tls(Parser *p) {
    dbg("tls got token %s.", get_token_data(current(p)));
    return parse_statement(p);
}
Node* parse_fn_dec(Parser *p) {
    if (current(p).type != TokenKeyword) {
        err("expected keywork \"fn\" for function declaration, got %s.",
                get_token_data(current(p)));
        return NULL;
    }
    if (current(p).kw != KwFn) {
        err("expected keywork \"fn\" for function declaration, got %s.",
                get_token_data(current(p)));
        return NULL;
    }
    Token fn = consume(p);
    Node* fn_symbol = parse_symbol(p);
    if (!fn_symbol) {
        err("Failed to parse fn symbol.");
        return NULL;
    }

    if (current(p).type != TokenOpenParen) {
        err("Expected \"(\" after \"fn\", got %s.", get_token_data(current(p)));
        return NULL;
    }
    consume(p); // "("
    // parse args ig
    if (current(p).type != TokenCloseParen) {
        err("Expected \")\" after args, got %s.", get_token_data(current(p)));
        return NULL;
    }
    consume(p); // ")"

    Node* ret_type = NULL;
    if (current(p).type == TokenColon) {
        consume(p); // ":"
        ret_type = parse_type(p);
        if (!ret_type) {
            err("Failed to function return type.");
            return NULL;
        }
    }
    Node* n = new_node(p);
    if (!n) {
        err("Failed to allocate memory for node.");
        return NULL;
    }
    n->kind = NodeFnDec; // fn declaratio
    n->fn_dec.ident = fn_symbol;
    n->fn_dec.body = 0;
    n->fn_dec.count = 0;
    n->fn_dec.args = 0;
    n->fn_dec.return_type = NULL; // void
    if (current(p).type == TokenOpenBrace) {
        Node* body = parse_scope(p);
        if (!body) {
            err("Failed to .");
            return NULL;
        }
        // set bodys body to scope
        n->fn_dec.body = body;
    } else if (current(p).type == TokenSemicolon) {
        consume(p); // ";"
    }
    return n;

}
Node* parse_statement(Parser *p) {
    Node* n;
    n = new_node(p);
    if (!n) {
        err("Failed to allocate new node.");
        return NULL;
    }
    if (current(p).type == TokenKeyword) {
        if (current(p).kw == KwFn) {
            return parse_fn_dec(p);
        } else
        if (current(p).kw == KwReturn) {
            Token ret = consume(p); // "return"
            Node* expr = parse_expression(p);
            if (!expr) {
                err("Failed to parse expression.");
                return NULL;
            }
            n->kind = NodeRet;
            n->ret.expr = expr;
        } else
        if (current(p).kw == KwIf) {
            // return parse_fn_dec(p);
        } else {
            TODO("Parse unhandled/unknown kw");
        }
    } else {
        n = parse_expression(p);
        if (!n) {
            err("Failed to parse expression.");
            return NULL;
        }
    }
    // make it optional ig?
    if (current(p).type == TokenSemicolon) {
        consume(p); // ";"
    }
    return n;
    panic("wtf.");
}
Node* parse_scope(Parser *p) {
    if (current(p).type != TokenOpenBrace) {
        err("Expected \"{\", got %s.", get_token_data(current(p)));
        return NULL;
    }
    Token open_brace = consume(p);
    size_t count = 0, cap = 5;
    Node** stmts = malloc(cap*sizeof(Node*));
    if (!stmts) {
        err("Failed to allocate nodes for scope.");
        return NULL;
    }
    // also eof
    while (current(p).type != TokenCloseBrace
            && current(p).type != TokenEOF) {
        Node* stmt = parse_statement(p);
        if (!stmt) {
            err("Failed to parse statement.");
            consume(p); // cus invalid and could've not been consumed
            continue;
        }
        stmts[count++] = stmt;
        if (count >= cap) {
            Node** tmp = realloc(stmts, (cap*=2)*sizeof(Node*));
            if (!tmp) {
                err("Failed to realloc nodes for scope.");
                free(stmts);
                return NULL;
            }
            stmts = tmp;
        }
    }
    if (current(p).type != TokenCloseBrace) {
        err("Expected \"}\", got %s.", get_token_data(current(p)));
        return NULL;
    }
    consume(p); // "}"
    Node* n = new_node(p);
    if (!n) {
        err("Failed to allocate memory for node.");
        return NULL;
    }
    n->kind = NodeScope;
    Node** arena_stmts = arena_alloc(&p->arena, count*sizeof(Node*));
    if (!arena_stmts) {
        panic("Failed to allocate node* arreay in arena.");
        return NULL;
    }
    memcpy(arena_stmts, stmts, count*sizeof(Node*));
    free(stmts);
    n->scope.stmts = arena_stmts;
    n->scope.count = count;
    return n;
}
int parse(Parser *p) {
    info("Parsing...");
    size_t cap = 128;
    Node** nodes = malloc(cap*sizeof(Node*));
    if (!nodes) {
        err("FAiled to allocate nodes.");
        return 0;
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
    info("Parsing ok. %zu nodes.", count);
    if (!resolve_symbols(p)) {
        err("Failed to resolve symbols for parser.");
        return 0;
    }
    if (!type_check(p)) {
        err("Failed to type check for parser.");
        return 0;
    }
    info(" === TYPES === %d", p->syms->types_count);
        SymbolTable* s = p->syms;
    while (s) {
    
        for (int i = 0; i < s->types_count; i++) {
            print_type(s->types[i]);
            fflush(stdout);
        }
        s = s->parent;
    }
    print_parser_to_file(stdout, p);
    // or
    FILE* f = fopen("ast.txt", "w");
    print_parser_to_file(f, p);
    fclose(f);
    if (!all_good(p)) {
        panic("Failed to parse file \"%s\".", p->path);
        return 0;
    }
    return 1;
}
#define CODE_LEN 32
Parser* pctx_new(Lexer* l, char* path, SymbolTable* st) {
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
    p->syms = st_new(p, st);
    if (!p->syms) {
        panic("Failed to allocate symbol table.");
        return NULL;
    }
    p->path = path;
    p->l = l;
    p->tokens_index = 0;
    p->tokens_count = p->l->tokens_count;
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
    if (!st_destroy(p->syms)) {
        err("Failed to free parser symbols.");
    }
    p->syms = 0;

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


int add_base_types(SymbolTable* st) {
    for (int i = 0; i < sizeof(base_types) / sizeof(base_types[0]); i++) {
        Type t = base_types[i];
        dbg("Base Type %.*s, (size %zu, type %d)...",
                (int)t.name.length, t.name.name, t.size, t.kind);

        if (!st_add_type(st, t)) {
            panic("Failed to add base type.");
            return 0;
        }
        dbg("added type %.*s.",(int) base_types[i].name.length,
                base_types[i].name.name);
    }
    return 1;
}

