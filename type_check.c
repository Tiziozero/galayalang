#include "constants.h"
#include "lexer.h"
#include "logger.h"
#include "parse_number.h"
#include "parser.h"
#include "utils.h"
#include <stdio.h>

int propagate_type(Type* t, Node* n) {
    if (!t || !n) return 0;

    // if node's type is untyped, set it
    if (!n->type || is_untyped(n->type)) {
        n->type = t;
    }
    printf("Propagating ");
    print_type(t);
    printf("...\n");

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
            // the cast's type IS its target type — propagate that inward, not t
            if (is_untyped(n->cast.target->type)) {
                propagate_type(n->cast.to, n->cast.target);
            }
            // cast has an explicit target type, don't touch it
            break;
        default:
            break;
    }
    return 1;
}
Type* get_base_type_for_untyped(Parser* p, Type* t) {
    if (t->kind == tt_untyped_float) {
        return &st_get_type(p->syms, cstr_to_name("f32"))->type;
    }
    if (t->kind == tt_untyped_unsigned_int) {
        return &st_get_type(p->syms, cstr_to_name("u32"))->type;
    }
    if (t->kind == tt_untyped_int) {
        return &st_get_type(p->syms, cstr_to_name("i32"))->type;
    }
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

    int errs= 0;
    TypeChecker tc;
    tc.return_type = 0; // no return
    tc.parent = 0;
    for (int i = 0; i < p->nodes_count; i++) {
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
    if (!t1  || !t2) return  panic("No type 1/2 %d/%d", t1, t2) ,NULL;
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
    dbg("to %d",to);
    return to;
}
//check symbol since it resolved
int type_check_node(Parser* p, TypeChecker* tc, Node* n) {
    if (!n->resolved) {
        panic("Node %s (%s) not resolved.", NodeKindToString(n->kind), get_token_data(n->token));
        return 0;
    }
    dbg("Node %s (%d)", NodeKindToString(n->kind), n->kind);
    int errs = 0;
    if (!n->type) n->type = new_type(p);
    if (n->kind == NodeVarDec) {
        if (n->var_dec.value) {
            if (!type_check_node(p, tc, n->var_dec.value)) {
                err("Failed to type check vardec value.");
                return 0;
            }
            Type* to = NULL;
            if (n->symbol->var.type->kind == tt_to_determinate) {
                if (is_untyped(n->var_dec.value->type)) {
                    dbg("Is untyped inference.");
                    to = get_base_type_for_untyped(p, n->var_dec.value->type);
                    if (!to) {
                        panic("ye");
                    }
                } else {
                    to = n->var_dec.value->type;
                }
                // set symbol
                n->symbol->var.type = to;
            } else {
                to = resolve_common_type(n->symbol->var.type,
                        n->var_dec.value->type);
                if (!to) {
                    err("Types don't match.");
                    errs++;
                } else if (is_untyped(to)) {
                    to = get_base_type_for_untyped(p, to);
                    if (!to) {
                        panic("ye");
                    }
                }
            }
            propagate_type(to, n);
            n->var_dec.value->type = to;
            n->type = to;
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
        print_type(n->binop.left->type);
        printf(" binop ");
        print_type(n->binop.right->type);
        printf("\n");
        Type* t = resolve_common_type(n->binop.left->type, n->binop.left->type);
        if (!t) {
            panic("Failed to resolve common types in binop.");
            return 0;
        }
        if (errs > 0) return 0;
        n->binop.left->type = t;
        n->binop.right->type = t;
        n->type = n->binop.left->type;
    } else if (n->kind == NodeNumLit) {
        if (n->number.kind == NumKindFloat) {
            n->type->kind = tt_untyped_float;
        } else {
            n->type->kind = tt_untyped_unsigned_int;
        }
        return 1;
    } else if (n->kind == NodeSymbol) { // variables/fns and what not
        Symbol* s = n->symbol;
        if (!s) {
            err("Symbol %.*s doesn't exist.",
                    (int)n->ident.length, n->ident.name);
            return 0;
        } else {
            info("Symbol %.*s exists.",
                    (int)n->ident.length, n->ident.name);
            if (s->kind == SymVar) {
                n->symbol = s;
                n->type = s->var.type;
            } else {
                TODO("hadle");
            }
        }
    } else if (n->kind == NodeArg) { // already set in st
    } else if (n->kind == NodeFnDec) { // fn dec
        Symbol* s = n->symbol;
        if (!s) {
            panic("no symbol in fn_dec node in typecheck.");
            return 0;
        }
        if (!type_check_node(p, tc, n->fn_dec.args)) {
            panic("Failed to type check args.");
            return 0;
        }
        TypeChecker fn_tc = {0};
        fn_tc.parent = tc;
        // return type for return
        // var is of type ptr to fn, so access ptr first,
        // then fn and it's ret type
        fn_tc.return_type = s->var.type->ptr->fn.return_type;
        info("%d ret type.", fn_tc.return_type);
        if (!type_check_node(p, &fn_tc, n->fn_dec.body)) {
            panic("Failed to type check fn body.");
            return 0;
        }
        n->type = s->var.type; // ptr to fn
        return 1;
    } else if (n->kind == NodeBlock) { // fn dec
        dbg("%d stmts in block.", n->block.count);
        for (int i = 0; i < n->block.count; i++) {
            errs += !type_check_node(p, tc, n->block.stmts[i]);
        }
        if (errs==0) // set to last
            n->type = n->block.last->type;
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
    } else if (n->kind == NodeNodeList) {
        for (int i = 0; i < n->node_list.count; i++) {
            if (!type_check_node(p, tc, n->node_list.nodes[i])) {
                panic("Failed to type check node list node %d.", i);
                return 0;
            }
        }
    } else if (n->kind == NodeFnCall) {
        if (!type_check_node(p, tc, n->fn_call.target)) {
            panic("Failed to resolve dn call target.");
            return 0;
        }
        Type* t = n->fn_call.target->type;
        if (!t) {
            panic("Failed to get target type tc.");
            return 0;
        }
        print_type(t);
        if (t->kind != tt_ptr) {
            panic("can only perform a call on a function ptr.");
            return 0;
        }
        if (t->ptr->kind != tt_fn) {
            panic("fn call target not a pointer to fn.");
            return 0;
        }
        FunctionType fn =  t->ptr->fn;
        if (n->fn_call.args) {
            if (!type_check_node(p, tc, n->fn_call.args)) {
                panic("Failed to resolve type_check_node for fn call args.");
                return 0;
            }
            if (n->fn_call.args->node_list.count != fn.args->node_list.count) {
                panic("call args (%d) not same as target type args (%d).",
                        n->fn_call.args->node_list.count, fn.args->node_list.count);
            }
            for (int i = 0; i < n->fn_call.args->node_list.count; i++) {
                // already resolved
                Node* target = n->fn_call.args->node_list.nodes[i];
                Type* r = resolve_common_type(
                        fn.args->node_list.nodes[i]->type, target->type);
                if (!r) {
                    panic("Failed to resolve common type in fn call arg cmp.");
                }
                fn.args->node_list.nodes[i]->type = r;
                target->type = r;
            }
        }
        n->type = fn.return_type;
    } else if (n->kind == NodeCast) {
        if (!type_check_node(p,tc,n->cast.target)) {
            panic("Failed to type check cast target.");
        }
        n->type = n->cast.to;
    } else if (n->kind == NodeUnary) {
        if (!type_check_node(p,tc,n->unary.target)) {
            panic("Failed to type check unary target.");
        }
        dbg("Un Kind %d", n->unary.type);
        // check if can unary
        if (n->unary.type == UnNot){
            if (!is_numeric(n->unary.target->type)) {
                panic("Type must be numeric for \"not\" (\"!\").");
                return 0;
            }
            n->type=new_type(p); // can be anything numeric whose value
            n->type->kind = tt_untyped_unsigned_int;
        } else if (n->unary.type == UnCompliment) {
            if (!is_integer(n->unary.target->type)) {
                panic("Type must be integer for \"compliment\" (\"~\").");
                return 0;
            }
            n->type = n->unary.target->type;
        } else if (n->unary.type == UnNegative) {
            if (!is_numeric(n->unary.target->type)) {
                panic("can not have negative of non-numeric types.");
                return 0;
            }
            if (is_unsigned(n->unary.target->type)) {
                if (n->unary.target->type->kind == tt_untyped_unsigned_int) {
                    n->unary.target->type->kind = tt_untyped_int;
                } else {
                    dbg("%s", get_token_data(n->token));
                    dbg("%s", get_token_data(n->unary.target->token));
                    panic("can't have ngative of unsigned (cast pls).");
                }
            }
            n->type = n->unary.target->type;
        } else if (n->unary.type == UnRef) {
            if (!is_lvalue(n->unary.target)) {
                panic("can only reference lvalues.");
                return 0;
            }
            Type* t = new_type(p);
            t->kind = tt_ptr;
            t->ptr = n->unary.target->type;
            n->type = t;
        } else if (n->unary.type == UnDeref) {
            if (!is_pointer(n->unary.target->type)) { 
                panic("can only dereference pointers.");
                return 0;
            }
            if (n->unary.target->type->ptr->size == 0) {
                panic("can not dereference types whose size is 0/unknown.");
                return 0;
            }
            n->type = n->unary.target->type->ptr;
        } else {
            panic("idk bruh.");
        }
    } else if (n->kind == NodeIfStmt) {
        if (!type_check_node(p, tc, n->if_stmt.cond)) {
            panic("failed to symbol check if condition.");
            return 0;
        }
        if (!type_check_node(p, tc, n->if_stmt.block)) {
            panic("failed to symbol check if block.");
            return 0;
        }
        for (int i = 0; i < n->if_stmt.alt_count; i++) {
            if (!type_check_node(p, tc, n->if_stmt.alt_conds[i])) {
                panic("failed to symbol check "
                        "if else condition %d.", i);
                return 0;
            }
            if (!type_check_node(p, tc, n->if_stmt.alt_blocks[i])) {
                panic("failed to symbol check if else block %d.", i);
                return 0;
            }
        }
        if (!type_check_node(p, tc, n->if_stmt.else_block)) {
            panic("failed to symbol check if block.");
            return 0;
        }
    } else if (n->kind == NodeStructDec) { // nothing to do.
        return 1;
    } else if (n->kind == NodeStructLit) {
        StructType t = n->symbol->type.struct_t;
        // check fields
        Node* fields =  n->struct_literal.fields;
        for (int i = 0; i < fields->node_list.count; i++) {
            // check named field expr
            Node* expr = fields->node_list.nodes[i]->named_field.expr;
            if (!type_check_node(p, tc, expr)) {
                panic("failed to type check struct lit node %d.", i);
                return 0;
            }

            // get name too
            Span name = fields->node_list.nodes
                [i]->named_field.ident->ident;
            Type* expected = NULL;
            for (int j = 0; j < t.count; j++) {
                if (name_cmp(name, t.fields[j].name)) {
                    expected = t.fields[j].type;
                }
            }
            if (!expected) {
                panic("Failed to get type form struct st tc.");
                return 0;
            }
            Type* to = resolve_common_type(expected, expr->type);
            if (!to) {
                panic("Failed to resolve common type i nstruct lit tc.");
                return 0;
            }
            // make sure it's the same as to.
            if (to != expected) {
                panic("resolved type is not equla to struct type.");
                return 0;
            }
        }
        n->type = &n->symbol->type;
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
