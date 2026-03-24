#include "logger.h"
#include "parser.h"
#include "utils.h"
#include <stdio.h>

int propagate_type(Type* t, Node* n) {
    if (!t || !n) return 0;

    // if node's type is untyped, set it
    if (!n->type || is_untyped(n->type)) {
        n->type = t;
    }

    switch (n->kind) {
        case NodeBinOp:
            propagate_type(t, n->binop.left);
            propagate_type(t, n->binop.right);
            n->type = t;
            break;
        case NodeUnary:
            propagate_type(t, n->unary.target);
            n->type = t;
            break;
        case NodeRet:
            if (n->ret.expr)
                propagate_type(t, n->ret.expr);
            n->type = t;
            break;
        case NodeVarDec:
        case NodeConstDec:
            if (n->var_dec.value)
                propagate_type(t, n->var_dec.value);
            break;
        case NodeNumLit:
            n->type = t;
            break;
        case NodeSymbol:
            // don't override — symbol type comes from the ST
            break;
        case NodeCast:
            // cast has an explicit target type, don't touch it
            break;
        default:
            break;
    }
    return 1;
}
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
    if (t1->kind != t2->kind) { // not compatible
        err("type kinds don't match %d %d", t1->kind, t2->kind);
        return 0;
    }
    if (t1->kind == tt_ptr) {
        return type_cmp(t1->ptr, t2->ptr);
    }
    if (t1->kind == tt_fn) { // panic
        panic("TODO");
        return type_cmp(t1->fn.return_type, t2->fn.return_type);
    }
    return t1 == t2 ? t1 : NULL;
    if (t1 == t2) return t1; // same type
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
/*
 * handles the case where one is untyped and the other typed
 * t is type, unt is untyped.
 */
Type* handle_untyped_typed(Type* t, Type* unt) {
    if (is_numeric(t) && is_numeric(unt)) {
        if (is_unsigned(t) || is_pointer(t)) {
            if (!is_unsigned(unt)) { // if it's not unsigned
                                     // than they're incompatible
                return NULL;
            }
            return t;
        } else if (is_signed(t)) {
            if (!is_integer(unt)) { // if it's not int (signed or unsigned)
                                     // than they're incompatible
                return NULL;
            }
            return t;
        } else if (is_float(t)) { // all can cast to float
            return t;
        } else {
            panic("what");
            return NULL;
        }
    } else if (is_struct(t)) {
        TODO("Handle structs in untyped_typed");
    } else {
        TODO("handle else in untyped_typed");
    }
    return NULL;
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
            return NULL;
        } else {
            panic("Cannot handle.");
            return NULL;
        }
    } else if (is_untyped(t1) && !is_untyped(t2)) { // t2 is typed
        return handle_untyped_typed(t2, t1);
    } else if (is_untyped(t2) && !is_untyped(t1)) { // t1 is typed
        return handle_untyped_typed(t1, t2);
    } else {
        TODO("Implement handle_untyped.");
        return NULL;
    }
    panic("huh");
    return NULL;
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
    if (!n->resolved) {
        panic("Node %s not resolved.", NodeKindToString(n->kind));
        return 0;
    }
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
                if (!to) {
                    panic("ye");
                }
            }
            propagate_type(to, n->var_dec.value);
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
        Symbol* s = n->symbol;
        if (!s) {
            err("Symbol %.*s doesn't exist.",
                    (int)n->ident.length, n->ident.name);
            return 0;
        } else {
            info("Symbol %.*s exist.",
                    (int)n->ident.length, n->ident.name);
            if (s->kind == SymVar) {
                n->symbol = s;
                n->type = s->var.type;
            } else {
                TODO("hadle");
            }
        }
    } else if (n->kind == NodeFnDec) { // fn dec
        Symbol* s = n->symbol;
        if (!s) {
            panic("no symbol in fn_dec node in typecheck.");
            return 0;
        }
        TypeChecker fn_tc = {0};
        fn_tc.parent = tc;
        // return type for return
        // var is of type ptr to fn, so access ptr first,
        // then fn and it's ret type
        fn_tc.return_type = s->var.type->ptr->fn.return_type;
        info("%zu ret type.", fn_tc.return_type);
        if (!type_check_node(p, &fn_tc, n->fn_dec.body)) {
            panic("Failed to type check fn body.");
            return 0;
        }
        n->type = s->var.type; // ptr to fn

        return 1;
    } else if (n->kind == NodeScope) { // fn dec
        dbg("%zu stmts in scope.", n->scope.count);
        for (size_t i = 0; i < n->scope.count; i++) {
            errs += !type_check_node(p, tc, n->scope.stmts[i]);
        }
        if (errs==0)
            n->type = &st_get_type(p->syms, cstr_to_name("void"))->type;
    } else if (n->kind == NodeRet) { // fn dec
        if (!tc->return_type) {
            panic("no return type/not in fucntion for ret node.");
            return 0;
        }
        if (!n->ret.expr) { // should already be void
            if (!n->type) {
                panic("Type should've been set to void, but is empty.");
                return 0;
            }
        } else {
            if (!type_check_node(p, tc, n->ret.expr)) {
                panic("failed typecheck return expr.");
                return 0;
            }
            n->type = n->ret.expr->type;
        }

        // get common
        Type* r = resolve_common_type(tc->return_type, n->type);
        if (!r) {
            panic("Failed to handle untyped/incompatible types.");
            return 0;
        }
        n->type = r;

        double check;
        if (!type_cmp(tc->return_type, n->type)) {
            err("incompatible types.");
            return 0;
        }
        propagate_type(r, n->ret.expr); // propagate
    } else {
        panic("(tc) Unhandled node %s (%d).",
                NodeKindToString(n->kind), n->kind);
        return 0;

    }
    if (errs != 0) {
        n->type = NULL; // fails
    }
    info("returning %d.", errs==0);
    return errs == 0;
}
