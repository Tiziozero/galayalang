#include "logger.h"
#include "parser.h"
#include "utils.h"


long new_uutid() {
    static long counter = 1; // start at 1, 0 is unhandled
    return counter++;
}
int is_valid_path(Node* path);
int is_valid_type(Type* t) {
    if (!t) {
        panic("no t in is valid type");
        return 0;
    }
    if (t->kind == tt_ptr) return is_valid_type(t->ptr);
    if (t->kind == tt_fn) {
        warn("implement");
        return is_valid_type(t->fn.return_type);
    }
    // else must have name if it's resolved
    else if (!is_valid_name(t->name)) { 
        err("invalid name in is_valid_type");
        return 0;
    }
    if (t->size == 0 && t->kind != tt_void) {
        panic("size is 0 for %.*s %zu.",
                (int)t->name.length, t->name.name, t->kind);
        return 0;
    }
    return 1;
}
SymbolTable*    st_new(Parser* p, SymbolTable* parent) {
    SymbolTable* st = arena_alloc(&p->arena, sizeof(SymbolTable));
    if (!st) {
        panic("Failed to allocate memory in arena for sybol table.");
        return NULL;
    }
    memset(st, 0, sizeof(SymbolTable));
    st->cap = 10;
    st->symbols = malloc(st->cap*sizeof(Symbol*));
    st->arena = &p->arena;
    if (!st->symbols) {
        panic("Failed to allocate memory for sybols in symbol table.");
        return NULL;
    }
    st->types_cap = 10;
    st->types = (Type**)malloc(st->types_cap * sizeof(Type*));
    st->types_count = 0;
    if (!st->types) {
        panic("Failed to allocate memory for types in symbol table.");
        return NULL;
    }
    st->parent = parent;

    return st;
}
int             st_destroy(SymbolTable* st) {
    if (!st)  return 0;
    if (!st->symbols)  return 0;
    free(st->symbols);
    return 1;
}
Symbol* st_add_symbol(SymbolTable* st, Symbol symbol) {
    Symbol* s = arena_alloc(st->arena, sizeof(Symbol));
    if (!s) {
        panic("Failed to allocate memory in arena for symbol.");
        return NULL;
    }
    *s = symbol;
    if (st->count >= st->cap) {
        if (st->cap == 0) st->cap = 10;
        Symbol** tmp = realloc(st->symbols, st->cap*2*sizeof(Symbol*));
        if (!tmp) {
            panic("failed to realloc memory for symbols.");
            return NULL;
        }
        st->cap *= 2;
        st->symbols = tmp;
    }
    st->symbols[st->count++] = s;

    // if it's a type symbol, also register it in the types array
    if (symbol.kind == SymType) {
        info("New Type");
        print_type(&symbol.type);
        if (st->types_count >= st->types_cap) {
            size_t new_cap = st->types_cap ? st->types_cap * 2 : 10;
            Type** tmp = realloc(st->types, new_cap * sizeof(Type*));
            if (!tmp) {
                panic("Failed to realloc types in symbol table.");
                return NULL;
            }
            st->types = tmp;
            st->types_cap = new_cap;
        }
        st->types[st->types_count++] = &st->symbols[st->count-1]->type;
    }

    return st->symbols[st->count-1];
}
Symbol* st_sym_exists(SymbolTable* st, Span name) {
    if (!st) panic("No st.");
    for (size_t i = 0; i < st->count; i++) {
        Symbol* s = st->symbols[i];
        if (!is_valid_name(s->name)){ 
            panic("wat.");
        }
        if (name_cmp(name, s->name)) // copy of name
            return s;
    }
    if (st->parent) {
        return st_sym_exists(st->parent, name);
    }
    return NULL;
}
Symbol* st_add_var(SymbolTable* st, Variable v) {
    dbg("new var %.*s %zu", 
            (int)v.name.length, v.name.name, v.type);
    if (!v.type) {
        panic("No type in vardec.");
        return 0;
    }
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
    s.is_public = 1;
    s.name = s.var.name;
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
    t.uutid= new_uutid(); // only called here
    Symbol s;
    s.kind = SymType;
    s.type = t;
    s.is_public = 1;
    s.name = s.type.name;
    return st_add_symbol(st, s);
}
Symbol* st_get_var(SymbolTable* st, Span name) {
    for (size_t i = 0; i < st->count; i++) {
        Symbol* s = st->symbols[i];
        if (s->kind == SymVar) {
            if (name_cmp(name, s->var.name)) {
                return s;
            }
        }
    }
    if (st->parent) {
        return st_get_var(st->parent, name);
    }
    return NULL;
}
Symbol* st_get_type(SymbolTable* st, Span name) {
    for (size_t i = 0; i < st->count; i++) {
        Symbol* s = st->symbols[i];
        if (s->kind == SymType) {
            if (name_cmp(name, s->type.name)) {
                return s;
            }
        }
    }
    if (st->parent) {
        return st_get_type(st->parent, name);
    }
    return NULL;
}
int is_valid_path(Node* path) {
    if (!path) {
        err("No path");
        return 0;
    }
    if (path->kind == NodeModuleAccess) {
        return is_valid_name(path->module_access.target)
            && is_valid_path(path->module_access.module);
    } else if (path->kind == NodeSymbol) {
        return is_valid_name(path->ident);
    } else {
        panic("invalid node kind in path %zu.", path->kind);
        return 0;
    }
}
/* sets type to known type in st.
 * takes in pointer to pointer, to
 * modify pointer to point to type in
 * symbol table.
 */
Type* st_resolve_type(SymbolTable* st, Type* t) {
    if (!t) {
        panic("No type in resolve type");
        return NULL;
    }
    if (t->kind == tt_ptr) {
        t->ptr = st_resolve_type(st, t->ptr);
        if (t->ptr) {
            return t;
        }
        t->size = ptr_size;
        err("Failed to resolve ptr type.");
        return NULL;
    }
    info("path %zu.", t->ident);
    if (!is_valid_name(t->name)) {
        warn("No name, trying path.");
        if (!is_valid_path(t->ident)) {
            err("Invalid name in resolve type.  (kind %zu)", t->kind);
            return NULL;
        }
    }
    // resolve symbol/module access symbol
    if (t->ident->kind == NodeSymbol) {
        char b[1000];
        print_name_to_buf(b, 1000, t->ident->ident);
        t->name = t->ident->ident;
        info("Resolving type %s.", b);
        // check if it exists
        if (!st_sym_exists(st, t->name)) {
            err("Type symbol doesn't exist. %s", b);
            return NULL;
        }
        Symbol* s = st_get_type(st, t->name);
        if (!s) {
            err("Symol is not a type in resolve type.");
            return NULL;
        }
        info("Returning %zu %zu (size kind).", s->type.size, s->type.kind);
        return &s->type; // ptr to type of symbol in symbol table 
    } else  if (t->ident->kind == NodeModuleAccess) {
        TODO("Implement");
    }
    /* if (!st_sym_exists(st, t->name)) {
        err("Type symbol doesn't exist.");
        return NULL;
    }
    Symbol* s = st_get_type(st, t->name);
    if (!s) {
        err("Symol is not a type in resolve type.");
        return NULL;
    }
    return &s->type; // ptr to type in symbol table 
    */
    TODO("Implement resolve type/Failed.");
    return 0;
}
