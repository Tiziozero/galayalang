#include "parser.h"
#include "utils.h"

SymbolTable*    st_new(Parser* p) {
    SymbolTable* st = arena_alloc(&p->arena, sizeof(SymbolTable));
    if (!st) {
        panic("Failed to allocate memory in arena for sybol table.");
        return NULL;
    }
    memset(st, 0, sizeof(SymbolTable));
    st->cap = 10;
    st->symbols = malloc(st->cap*sizeof(Symbol));
    if (!st->symbols) {
        panic("Failed to allocate memory for sybols in symbol table.");
        return NULL;
    }
    return st;
}
int             st_destroy(SymbolTable* st) {
    if (!st)  return 0;
    if (!st->symbols)  return 0;
    free(st->symbols);
    return 1;
}
Symbol* st_add_symbol(SymbolTable* st, Symbol s) {
    st->symbols[st->count++] = s;
    if (st->count >= st->cap) {
        Symbol* tmp = realloc(st->symbols, st->cap*2*sizeof(Symbol));
        if (!tmp) {
            panic("failed to realloc memory for symbols.");
            return NULL;
        }
        st->cap *= 2;
        st->symbols = tmp; 
    }
    return &st->symbols[st->count-1];
}
Symbol*         st_sym_exists(SymbolTable* st, Span name) {
    for (size_t i = 0; i < st->count; i++) {
        Symbol* s = &st->symbols[i];
        if (s->kind == SymVar) {
            if (name_cmp(name, s->var.name))
                return s;
        } else if (s->kind == SymType) {
            if (name_cmp(name, s->type.name))
                return s;
        } else if (s->kind == SymField) {
            if (name_cmp(name, s->field.name))
                return s;
        } else {
            panic("Unknown symbol kind %d.", s->kind);
        }
    }
    return NULL;
}
Symbol* st_add_var(SymbolTable* st, Variable v) {
    if (!is_valid_type(v.type)) {
        err("invalid type in st_add_var");
        return NULL;
    }
    if (!is_valid_name(v.name)) {
        err("invalid name in st_add_var");
        return NULL;
    }
    if (st_sym_exists(st, v.name)) {
        err("Variable already exists.");
        return 0;
    }
    Symbol s;
    s.kind = SymVar;
    s.var = v;
    return st_add_symbol(st, s);
}
Symbol* st_add_type(SymbolTable* st, Type t) {
    if (!is_valid_type(&t)) {
        err("Invalid type in st_add_type.");
        return NULL;
    }
    if (st_sym_exists(st, t.name)) {
        err("Sym already exists.");
        return NULL;
    }
    Symbol s;
    s.kind = SymType;
    s.type = t;
    return st_add_symbol(st, s);
}
Symbol* st_get_var(SymbolTable* st, Span name);
Symbol* st_get_type(SymbolTable* st, Span name);

// sets type to known type in st;
Type* st_resolve_type(SymbolTable* st, Type* t);
