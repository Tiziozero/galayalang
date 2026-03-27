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

char *random_string(int n) {
    const char charset[] =
        "abcdefghijklmnopqrstuvwxyz"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

    char *str = malloc(n + 1);
    if (!str) return NULL;

    for (int i = 0; i < n; i++) {
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
    // dbg("Token %s.", get_token_data(t));
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
        consume(p); // ":="
        Node* expr_n = parse_expression(p);
        if (!expr_n) {
            err("Failed to parse expression.");
            return NULL;
        }
        n->var_dec.value = expr_n;
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
    int cap = 10;
    int count = 0;
    Node** args = calloc(1, cap*sizeof(Node*));
    while (current(p).type == TokenIdent) {
        Node* symbol = parse_symbol(p);
        if (!symbol) {
            panic("failed to parse symbol.");
            return NULL;
        }
        if (current(p).type != TokenColon) {
            panic("Expected \":\", got %s", get_token_data(current(p)));
            return NULL;
        }
        consume(p); // ":"
        Node* type = parse_type(p);
        if (!type) {
            panic("failed to parse symbol.");
            return NULL;
        }
        Node* arg = new_node(p);
        if (!arg) {
            panic("Failed to allocate new node.");
            return NULL;
        }
        arg->kind = NodeArg;
        arg->arg.ident = symbol;
        arg->arg.type = type;
        if (count >= cap) {
            args = realloc(args, (cap*=2)*sizeof(Node*));
            if (!args) {
                panic("Faield to realloc args.");
                return 0;
            }
        }
        args[count++] = arg;
        if (current(p).type == TokenComma) {
            consume(p);
        }
    }
    Node* arena_args = new_node(p);
    if (!arena_args) {
        panic("Failed to alloca arena args.");
        return NULL;
    }
    arena_args->kind = NodeArgs;
    arena_args->args.args = arena_alloc(&p->arena, count*sizeof(Node*));
    if (!arena_args->args.args) {
        panic("Failed to allocate args memory in arena.");
        return 0;
    }
    memcpy(arena_args->args.args, args, count*sizeof(Node*));
    arena_args->args.count = count;
    free(args); // free
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
    n->kind = NodeFnDec; // fn declaration
    n->fn_dec.ident = fn_symbol;
    n->fn_dec.body = 0;
    n->fn_dec.args = arena_args;
    n->fn_dec.return_type = ret_type;
    if (current(p).type == TokenOpenBrace) {
        Node* body = parse_scope(p);
        if (!body) {
            err("Failed to .");
            return NULL;
        }
        // set bodys body to block
        n->fn_dec.body = body;
    } else if (current(p).type == TokenSemicolon) {
        consume(p); // ";"
    }
    return n;

}
Node* parse_fields_dec(Parser* p) {
    panic("no");
    return NULL;
}
Node* parse_struct_dec(Parser* p) {
    if (current(p).kw != KwStruct) {
        panic("Expected keyworkd struct.");
        return 0;
    }
    Token dec = consume(p);
    Node* s = parse_symbol(p);
    if (!s) {
        panic("Faield to parse symbol.");
        return 0;
    }
    expect(p, TokenOpenBrace);
    consume(p);
    int cap = 10, count = 0;
    Node** fields = calloc(1, cap*sizeof(Node*));
    Node* unassigned[10] = {0}; // use this
    int unassigned_count = 0;
    while (current(p).type == TokenIdent) {
        Node* ident = parse_symbol(p);
        unassigned[unassigned_count++] = ident;
        // e.g. a, b : u32;
        if (current(p).type == TokenComma) { // set this fields type to next one:
            continue;
        }
        expect(p, TokenColon);
        consume(p); // ":"
        Node* type = parse_type(p);
        if (!type) {
            panic("Failed to parse struct dec type.");
            return 0;
        }
        expect(p, TokenSemicolon);
        consume(p); // ";"
        for (int i = 0; i < unassigned_count; i++) {
            Node* n = new_node(p);
            n->kind= NodeFieldDec;
            n->field_dec.type = type;
            n->field_dec.ident = unassigned[i];
            if (count >= cap) {
                fields = realloc(fields, sizeof(Node*) * (cap*=2));
            }
            fields[count++] = n;
        }
        unassigned_count = 0;
    }
    expect(p, TokenCloseBrace);
    consume(p); // "}"
    Node* n = new_node(p);
    n->kind = NodeStructDec;
    n->struct_dec.ident = s;
    n->struct_dec.count = count;
    n->struct_dec.fields = arena_alloc(&p->arena, count*sizeof(Node*));
    memcpy(n->struct_dec.fields, fields, count*sizeof(Node*));
    dbg("Finished struct. %s", get_token_data(current(p)));
    return n;
}
Node* parse_statement(Parser *p) {
    Node* n;
    // n = new_node(p);
    char* type = 0;
    if (current(p).type == TokenKeyword) {
        if (current(p).kw == KwFn) {
            type = "fn";
            return parse_fn_dec(p);
        } else
        if (current(p).kw == KwReturn) {
            Token ret = consume(p); // "return"
            Node* expr = parse_expression(p);
            if (!expr) {
                err("Failed to parse expression.");
                return NULL;
            }
            n = new_node(p);
            n->kind = NodeRet;
            n->ret.expr = expr;
            type = "return";
        } else
        if (current(p).kw == KwStruct) {
            n = parse_struct_dec(p);
            type = "struct dec";
        } else
        if (current(p).kw == KwIf) {
            type = "if/else";
            return parse_if_else(p);
        } else {
            TODO("Parse unhandled/unknown kw");
        }
    } else {
        n = parse_expression(p);
        if (!n) {
            err("Failed to parse expression.");
            return NULL;
        }
        type = "expression";
    }
    if (!n) {
        err("Failed to pare stmt.");
        return NULL;
    }
    // make it optional ig?
    if (current(p).type != TokenSemicolon) {
        panic("Semicolons are not optional after %s statement.", type);
        return 0;
    }
    consume(p); // ";"
    return n;
}
Node* parse_scope(Parser *p) {
    if (current(p).type != TokenOpenBrace) {
        err("Expected \"{\", got %s.", get_token_data(current(p)));
        return NULL;
    }
    Token open_brace = consume(p);
    int count = 0, cap = 5;
    Node** stmts = malloc(cap*sizeof(Node*));
    if (!stmts) {
        err("Failed to allocate nodes for block.");
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
                err("Failed to realloc nodes for block.");
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
    n->kind = NodeBlock;
    Node** arena_stmts = arena_alloc(&p->arena, count*sizeof(Node*));
    if (!arena_stmts) {
        panic("Failed to allocate node* arreay in arena.");
        return NULL;
    }
    memcpy(arena_stmts, stmts, count*sizeof(Node*));
    free(stmts);
    n->block.stmts = arena_stmts;
    n->block.count = count;
    return n;
}
Node* parse_if_else(Parser* p) {
    expect(p, TokenKeyword);
    expect_kw(p, KwIf);
    consume(p); // "if"
    Node* cond = parse_condition(p);
    if (!cond) {
        panic("Failed to parse if condition.");
        return 0;
    }
    Node* block = parse_scope(p);
    if (!block) {
        panic("Failed to parse if block.");
    }
    Node* else_block = 0;
    int cap = 10;
    Node** alt_cond = malloc(cap*sizeof(Node*));
    Node** alt_block = malloc(cap*sizeof(Node*));
    int count = 0;
    while (current(p).type == TokenKeyword &&
            current(p).kw == KwElse) {
        Token else_token = consume(p); // "else"
        // else if
        if (current(p).type == TokenKeyword &&
                current(p).kw == KwIf) {
            consume(p); // "if"
            Node* cond = parse_condition(p);
            if (!cond) {
                panic("Failed to parse else condition.");
                return NULL;
            }
            Node* block = parse_scope(p);
            if (!block) {
                panic("Failed to parse else cond block.");
                return NULL;
            }
            if (count >= cap) {
                cap*=2;
                alt_cond = realloc(alt_cond, cap*sizeof(Node*));
                alt_block = realloc(alt_block, cap*sizeof(Node*));
            }
            alt_cond[count] = cond;
            alt_block[count] = block;
            count++;
        } else {
            Node* block = parse_scope(p);
            if (!block) {
                panic("Failed to parse else block.");
                return NULL;
            }
            if (else_block) {
                panic("more than one else block.");
                return 0;
            }
            else_block = block;
        }
    }
    Node* n = new_node(p);
    n->kind = NodeIfStmt;
    n->if_stmt.cond = cond;
    n->if_stmt.block = block;
    n->if_stmt.alt_conds    = arena_alloc(&p->arena, count*sizeof(Node*));
    memcpy(n->if_stmt.alt_conds, alt_cond, count*sizeof(Node*));
    n->if_stmt.alt_blocks   = arena_alloc(&p->arena, count*sizeof(Node*));
    memcpy(n->if_stmt.alt_blocks, alt_block, count*sizeof(Node*));
    n->if_stmt.alt_count = count;
    n->if_stmt.else_block = else_block;
    free(alt_cond);
    free(alt_block);
    return n;
}
int parse(Parser *p) {
    info("Parsing...");
    int cap = 128;
    Node** nodes = malloc(cap*sizeof(Node*));
    if (!nodes) {
        err("Failed to allocate nodes.");
        return 0;
    }
    int count = 0;
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
    info("Parsing ok. %d nodes.", count);
    if (!resolve_symbols(p)) {
        err("Failed to resolve symbols for parser.");
        return 0;
    }
    if (!type_check(p)) {
        err("Failed to type check for parser.");
        return 0;
    }
    SymbolTable* s = p->syms;
    int i = 0;
    while (s) {
        info(" === TYPES === %d", i);
        for (int i = 0; i < s->types_count; i++) {
            print_type(s->types[i]);
            printf("\n");
            fflush(stdout);
        }
        s = s->parent;
        i++;
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
    // int flags
    p->parse_struct_lit = 1;
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
        for (int i = 0; i < p->arena.pages_count; i++) {
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
        dbg("Base Type %.*s, (size %d, type %d)...",
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

