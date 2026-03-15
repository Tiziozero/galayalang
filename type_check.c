#include "parser.h"

Type* get_base_type_for_untyped(Type* t) {
    return NULL;
}
Type* type_cmp(Type* t1, Type* t2) {
    if (!t1) {
        panic("No t1.");
        return 0;
    }
    if (!t2) {
        panic("No t2.");
        return 0;
    }
    if (t1 == t2) return t1; // same type
    if (t1->kind != t2->kind) { // not compatible
        err("type kinds don't match %d %d", t1->kind, t2->kind);
        return 0;
    }
    if (t1->kind == tt_ptr) {
        return type_cmp(t1->ptr, t2->ptr);
    }
    if (t1->uutid == 0 || t2->uutid == 0) {
        panic("invalid uuidt %l %l", t1->uutid, t2->uutid);
        return 0;
    }
    if (t1->uutid == t2->uutid) return t1; // same type
    err("unhandled/failed %d", t1->kind);
    return 0;
}
typedef struct TypeChecker TypeChecker;
struct TypeChecker {
    TypeChecker* parent; // for scopes
    SymbolTable* st;
    Type* return_type; // for when return is allowed
};
int type_check_node(Parser* p, TypeChecker* tc, Node* n);
int type_check(Parser* p) {
    if (!p) {
        return 0;
    }

    size_t errs= 0;
    TypeChecker tc;
    tc.return_type = 0; // no return
    tc.parent = 0;
    tc.st = p->syms;
    for (size_t i = 0; i < p->nodes_count; i++) {
        if (!type_check_node(p, &tc, p->nodes[i])) {
            err("Failed to type check node %i.", i);
        }
    }
    return errs == 0;
}
// handles the case where one is untyped and the other typed
int handle_untyped_typed(Type* t, Type* unt) {

}
// return type to converge to (can be untyped.);
Type* handle_untyped(Type* t1, Type* t2) {
    if (!t1  || !t2) return  panic("No type 1/2") ,NULL;
    if (!is_untyped(t1)  && !is_untyped(t2)) return type_cmp(t1, t2);
    // handle both untyped
    if (is_untyped(t1) && is_untyped(t2)) {
        if (is_numeric(t1) && is_numeric(t2)) {
            // prioritise float
            if (t1->kind == tt_untyped_float) {
                return t1;
            }
            if (t2->kind == tt_untyped_float) {
                return t2;
            }
            // then signed integers
            if (t1->kind == tt_untyped_int) {
                return t1;
            }
            if (t2->kind == tt_untyped_int) {
                return t2;
            }
            // then unsigned integers
            if (t1->kind == tt_untyped_unsigned_int) {
                return t1;
            }
            if (t2->kind == tt_untyped_unsigned_int) {
                return t2;
            }
        } else if (is_struct(t1) && is_struct(t2)) {
            TODO("Handle untyped structs");
        } else {
            panic("Cannot handle.");
        }
    }
    TODO("Implement handle_untyped.");
}


Type* resolve_common_type(Type* t1, Type* t2) {
    // try handling untyped
    Type* to = handle_untyped(t1, t2);
    // if no type to convert to then check types
    if (!to) {
        dbg("no untyped handled.");
        to = type_cmp(t1, t2);
        if (!to){
            err("types are not compatable.");
            return NULL;
        }
        return to; // compatable so cast both to this
    }
    dbg("to %zu",to);
    return to;
}
//check symbol since it resolved
int type_check_node(Parser* p, TypeChecker* tc, Node* n) {
    dbg("Node %s (%d)", NodeKindToString(n->kind), n->kind);
    int errs = 0;
    if (!n->type) n->type = new_type(p);
    if (n->kind == NodeVarDec) {
        info("resolvinf vardec type.");;
        if (!is_valid_type(n->symbol->var.type)) {
            err("Invalid type in vardec.");
            errs++;
        }
        info("done");;
        if (n->var_dec.value) {
            if (!type_check_node(p, tc, n->var_dec.value)) {
                err("Failed to type check vardec value.");
                return 0;
            }
            Type* to = resolve_common_type(n->symbol->var.type,
                    n->var_dec.value->type);
            if (!to) {
                err("Types don't match.");
                errs++;
            } else if (is_untyped(to)) {
                to = get_base_type_for_untyped(to);
            }
            n->var_dec.value->type = to;
            n->symbol->var.type = to;
        }
        if (errs != 0) {
            n->type = NULL;
            return 0;
        }
        n->type = n->symbol->var.type;
    } else if (n->kind == NodeBinOp) {
        if (!type_check_node(p, tc, n->binop.left)) {
            err("Faield binop left typecheck.");
            errs++;
        }
        if (!type_check_node(p, tc, n->binop.right)) {
            err("Faield binop right typecheck.");
            errs++;
        }
        if (errs > 0) return 0;
        n->type = n->binop.left->type;
    } else if (n->kind == NodeNumLit) {
        n->type->kind = tt_untyped_unsigned_int;
        for (size_t i = 0; i < n->number.str_repr.length; i++) {
            if (n->number.str_repr.name[i] == '.') // check for dot
                n->type->kind = tt_untyped_float;
        }
        return 1;
    } else if (n->kind == NodeSymbol) { // variables/fns and what not
        Symbol* s = st_sym_exists(tc->st, n->var->name);
        if (!s) {
            err("Symbol %.*s doesn't exist.", (int)n->var->name.length, n->var->name.name);
            return 0;
        } else {
            info("Symbol %.*s exist.", (int)n->var->name.length, n->var->name.name);
            if (s->kind == SymVar) {
                n->symbol = s;
                n->type = s->var.type;
            } else {
                TODO("hadle");
            }
        }
    } else {
        panic("(tc) Unhandled node %s (%d).", NodeKindToString(n->kind), n->kind);
        return 0;

    }
    if (errs != 0) {
        n->type = NULL; // fails
    }
    info("returning %d.", errs==0);
    return errs == 0;
}
