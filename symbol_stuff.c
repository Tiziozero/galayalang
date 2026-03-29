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
    else if (t->kind == tt_to_determinate) {
        warn("one symbol still to determinate.");
    }
    // else must have name if it's resolved
    else if (!is_valid_name(t->name)) { 
        err("invalid name in is_valid_type");
        return 0;
    }
    else if (t->size == 0 && t->kind != tt_void) {
        err("size is 0 for %.*s %d.",
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
    st->p = p;
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
    free(st->types);
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
            int new_cap = st->types_cap ? st->types_cap * 2 : 10;
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
Symbol* st_sym_exists_scope(SymbolTable* st, Span name) {
    if (!st) panic("No st.");
    for (int i = 0; i < st->count; i++) {
        Symbol* s = st->symbols[i];
        if (!is_valid_name(s->name)){ 
            panic("wat.");
        }
        char a[100], b[100];
        dbg("comparing (target) \"%s\" to \"%s\"...",
                print_name_to_buf(a, 100, name), print_name_to_buf(b,100,s->name));
        if (name_cmp(name, s->name)) // copy of name
            return s;
    }
    return NULL;
}
Symbol* st_sym_exists(SymbolTable* st, Span name) {
    Symbol* s = st_sym_exists_scope(st, name);
    if (s) return s;
    char b[100];
    dbg("symbol %s not found in current st.", print_name_to_buf(b,100, name));
    if (st->parent) {
        dbg("checking parent.");
        return st_sym_exists(st->parent, name);
    }
    return NULL;
}
Symbol*         st_add_arg(SymbolTable* st, Argument a) {
    info("new var %.*s %d", 
            (int)a.name.length, a.name.name, a.type);
    if (!a.type) {
        panic("No type in vardec.");
        return 0;
    }
    if (!is_valid_type(a.type)) {
        err("invalid type in st_add_arg");
        return NULL;
    }
    if (!is_valid_name(a.name)) {
        err("invalid name in st_add_arg");
        return NULL;
    }
    /*if (st_sym_exists_scope(st, a.name)) {
        print_st(st, 10);
        char b[100];
        err("Variable %s already exists (in current scope).",
                print_name_to_buf(b, 100, a.name));
        return 0;
    }*/ // name can exist. overshadow
    Symbol s;
    s.kind = SymArg;
    s.arg = a;
    s.is_public = 1;
    s.name = s.arg.name;
    return st_add_symbol(st, s);
}
Symbol* st_add_var(SymbolTable* st, Variable v) {
    info("new var %.*s %d", 
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
    if (st_sym_exists_scope(st, v.name)) {
        print_st(st, 10);
        char b[100];
        err("Variable %s already exists (in current scope).",
                print_name_to_buf(b, 100, v.name));
        return 0;
    }
    Symbol s;
    s.kind = SymVar;
    s.var = v;
    s.is_public = 1;
    s.name = s.var.name;
    return st_add_symbol(st, s);
}
// for recursion in structs and what not
Symbol* st_add_unfinished_type(SymbolTable* st, Type t) {
    // dont check
    /* if (!is_valid_type(&t)) {
        err("Invalid type in st_add_type.");
        return NULL;
    }*/
    if (st_sym_exists(st, t.name)) {
        err("Sym already exists.");
        return NULL;
    }
    // t.uutid= new_uutid(); // no uuid. unfinished
    Symbol s;
    s.kind = SymType;
    t.size = 0;
    s.type = t;
    s.is_public = 1;
    s.name = t.name;
    Symbol* type = st_add_symbol(st, s);
    if (!type) {
        panic("Failed to add symbol type.");
        return NULL;
    }
    // don't add
    // type_registry_add(&type->type);
    return  type;
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
    s.name = t.name;
    if (t.kind == tt_struct) {
        for (int i = 0; i < t.struct_t.count; i++) {
            char buf[100];
            print_name_to_buf(buf,100, t.struct_t.fields[i]->field.name);
            dbg("\t arg %s (%d)", buf, t.struct_t.fields[i]->field.name.length);
        }
    }
    Symbol* type = st_add_symbol(st, s);
    if (!type) {
        panic("Failed to add symbol type.");
        return NULL;
    }
    type_registry_add(&type->type);
    return  type;
}
Symbol* complete_type(Symbol* unfinished, Type t) {
    t.uutid = new_uutid();
    unfinished->type = t;
    type_registry_add(&unfinished->type);
    return unfinished;
}
Symbol* st_get_var(SymbolTable* st, Span name) {
    Symbol* s = st_sym_exists(st, name);
    if (!s) {
        err("Not found.");
        return 0;
    }
    if (s->kind != SymVar) {
        err("Symbol not var.");
        return 0;
    }
    return s;
}
Symbol* st_get_type(SymbolTable* st, Span name)  {
    Symbol* s = st_sym_exists(st, name);
    if (!s) {
        err("Not found.");
        return 0;
    }
    if (s->kind != SymType) {
        err("Symbol not type.");
        return 0;
    }
    return s;
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
        panic("invalid node kind in path %d.", path->kind);
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
    switch (t->kind) {
        case tt_ptr:
            dbg("Resolving ptr type.");
            t->ptr = st_resolve_type(st, t->ptr);
            if (t->ptr) {
                t->size = ptr_size;
                return t;
            }
            err("Failed to resolve ptr type.");
            return NULL;
            break;
        case  tt_to_determinate: break; // handle type
        case tt_fn:
            {
            dbg("Resolving fn type.");
                Type* ret_t = st_resolve_type(st, t->fn.return_type);
                if (!ret_t) {
                    panic("Failed to resolve return type.");
                }
                t->fn.return_type = ret_t;
                if (!symbols(st->p, st, t->fn.args)) {
                    panic("Faield to resolve fn type args.");
                    return NULL;
                }
                return t;
            } break;
        case tt_untyped_unsigned_int:
        case tt_untyped_int:
        case tt_untyped_float:
        case tt_untyped_struct:
            panic("Handle %d", t->kind);
        default: dbg("already resolved."); return t;
    }
    info("path %d.", t->ident);
    if (!is_valid_name(t->name)) {
        warn("No name, trying path.");
        if (!is_valid_path(t->ident)) {
            err("Invalid name in resolve type.  (kind %d)", t->kind);
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
        info("Returning %d %d (size kind).", s->type.size, s->type.kind);
        return &s->type; // ptr to type of symbol in symbol table 
    } else  if (t->ident->kind == NodeModuleAccess) {
        TODO("Implement");
    }
    TODO("Implement resolve type/Failed.");
    return 0;
}
