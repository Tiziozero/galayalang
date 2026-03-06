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
        err("type kinds don't match");
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
    for (size_t i = 0; i < p->nodes_count; i++) {
        if (!type_check_node(p, &tc, p->nodes[i])) {
            err("Failed to type check node %i.", i);
        }
    }
    return errs == 0;
}
// return type to converge to (can be untyped.);
Type* handle_untyped(Type* t1, Type* t2) {
    TODO("Implement.");
}


Type* resolve_common_type(Type* t1, Type* t2) {
    // try handling untyped
    Type* to = handle_untyped(t1, t2);
    // if no type to convert to then check types
    if (!to) {
        to = type_cmp(t1, t2);
        if (!to){
            err("types are not compatable.");
            return NULL;
        }
        return to; // compatable so cast both to this
    }
    return to;
}
//check symbol since it resolved
int type_check_node(Parser* p, TypeChecker* tc, Node* n) {
    int errs = 0;
    switch (n->kind) {
        case NodeVarDec:
            {
                if (!is_valid_type(n->symbol->var.type)) {
                    err("Invalid type in vardec.");
                    errs++;
                }
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
                    }
                    if (is_untyped(to)) {
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
            } break;
        case NodeBinOp:
            {
            };
        default:
            panic("Unhandled node %d.", n->kind);
            return 0;
    }
    if (errs != 0) {
        n->type = NULL; // fails
    }
    return errs == 0;
}
