#include "type_checker.h"
#include "parser.h"
#include "print.h"
#include "utils.h"
#include <stddef.h>
#include <stdio.h>

void print_two_types(Type* t1, Type* t2) {

    print_type(t1, 10);
    printf(" | ");
    print_type(t2, 10);
    printf("\n");
    fflush(stdout);
}

int cast(ParserCtx* pctx, Node** node, Type* type) {
    if (!pctx || !node || !type) return 0;
    Node* cast_node = new_node(pctx, NodeCast, (Token){0});
    if (!cast_node) return 0;
    cast_node->cast.to = type;
    cast_node->cast.expr = *node;
    *node = cast_node;
    return 1;
}

int type_cmp(Type* t1, Type* t2) {
    if (!t1) {
        err("type 1 is null");
        // assert(0&&"type 1 is null");
        return 0;
    }
    if (!t2) {
        err("type 2 is null");
        // assert(0&&"type 2 is null");
        return 0;
    }
    if (t1 == t2) return 1;
    if (t1->type != t2->type) return 0;
    // same type

    if (t1->type == tt_ptr) { // pointer types must be the same
        return type_cmp(t1->ptr, t1->ptr);
    }

    return 1;
    TODO("implement rest");
}

// returns type to which to cast to (t1, t2 or null if none)
Type* can_implicit_cast(Type* t1, Type* t2) {
    if (type_cmp(t1, t2)) return t1;

    if (is_numeric(t1) && is_numeric(t2)) {
        if (is_float(t1) && is_float(t2)) {
            return t1->size > t2->size ? t1 : t2; // return larger one
        }
        if (is_float(t1) && !is_float(t2)) {
            return t1;
        }
        if (!is_float(t1) && is_float(t2)) {
            return t2;
        }
        if (is_signed(t1) && is_signed(t2)) {
            return t1->size > t2->size ? t1 : t2; // return larger one
        }
        if (is_unsigned(t1) && is_unsigned(t2)) {
            return t1->size > t2->size ? t1 : t2; // return larger one
        }
        TODO("handle all cases");
    }
    return NULL;
}

struct TypeChecker* new_tc(
        struct TypeChecker* tc, ParserCtx* pctx, SymbolStore* ss) {
    memset(tc, 0, sizeof(struct TypeChecker));
    if (!ss) {
        ss = ss_new(NULL);
    }
    tc->pctx = pctx;
    tc->ss = ss;
    tc->parent = tc ? tc : NULL;
    tc->ok = 1;
    return tc;
}

struct TypeChecker* new_scope(struct TypeChecker* parent, SymbolStore* ss) {
    struct TypeChecker* tc = malloc(sizeof(struct TypeChecker));
    memset(tc, 0, sizeof(struct TypeChecker));
    tc->parent = parent;
    tc->pctx = parent->pctx;
    tc->ss = ss;
    tc->ok = 1;
    return tc;
}

struct TypeChecker* tc_fn(struct TypeChecker* parent,
        SymbolStore* ss, Function* fn) {
    struct TypeChecker* tc = new_scope(parent, ss);
    tc->fn = fn;
    return tc;
}

int type_check_expression(TypeChecker* tc, Node *node) {
    info("%s", node_type_to_string(node->kind));
    switch (node->kind) {
        case NodeNumLit:
            {
                int is_float = 0;
                node->type.type = NULL;
                node->type.state = TsOk;
                for (size_t i = 0; i < node->number.str_repr.length; i++) {
                    // if it has a dot it's a float
                    // TODO: add a "get untyped type" fucntion or smth
                    if (node->number.str_repr.name[i] == '.')
                        is_float = 1;
                }
                if (is_float)
                    node->type.state |= TsUntypedFloat;
                else
                    node->type.state |= TsUntypedInt;
            } break;
        case NodeCast:
            {
                node->type.type = node->cast.to;
                // could be a struct for all I care
                if (!type_check_node(tc, node->cast.expr))
                    node->type.state = TsFailed;
                else
                    node->type.state = TsOk;
            } break;
        case NodeBinOp:
            {
                int errs = 0;
                // type check left/right branch
                if (!type_check_expression(tc, node->binop.left))
                    errs++;
                if (!type_check_expression(tc, node->binop.right))
                    errs++;
                if (!can_binop(node->binop.left->type.type)
                        || !can_binop(node->binop.right->type.type)) {
                    print_two_types(node->binop.left->type.type,
                            node->binop.right->type.type);
                    assert(0&&"Cannot binop types");
                }
                if (!type_cmp(node->binop.left->type.type,
                            node->binop.right->type.type)) {
                    err("fuckass buttfuck gay nigger binop "
                            "types don't maths.");
                    print_two_types(node->binop.left->type.type,
                            node->binop.right->type.type);
                    errs++;
                }
                return errs == 0;
            } break;
        case NodeVar:
        case NodeFnCall:
            return type_check_node(tc, node);
        default:assert(0&&"Gay expression");
    }
    return 1;
}

int type_check_node(TypeChecker* tc, Node *node) {
    dbg("Node check %s", node_type_to_string(node->kind));
    switch (node->kind) {
        case NodeVarDec:
            {
                Type* var_type = node->var_dec.type;
                if (var_type->type == tt_void) { // type can not be void
                    err("Var type can not be void.");
                    return 0;
                }
                if (!type_check_expression(tc, node->var_dec.value)) {
                    err("Failed to type check expression.");
                    return 0;
                }
                if (node->var_dec.value) {
                    if (is_untyped(node->var_dec.value)) {
                        node->var_dec.value->type.type =
                            node->var_dec.type;
                        node->var_dec.value->type.state = TsOk;
                    }
                    if (!type_cmp(var_type,
                                node->var_dec.value->type.type)) {
                        err("type cmp failed in var dec");
                        return 0;
                    }
                }
                node->type.type = var_type;
                node->type.state = TsOk;
            } break;
        case NodeFnDec:
            {
                // create function type checker for return
                // etc e blah blah blah
                struct TypeChecker* fn_tc = tc_fn(tc,
                        node->fn_dec.body->block.ss, &node->symbol.fn);
                int errs = 0;
                errs += type_check_node(fn_tc, node->fn_dec.body);
                node->type.state = TsOk;
                return errs == 0;
            } break;
        case NodeBlock:
            {
                size_t errs = 0;
                for (size_t i = 0; i < node->block.nodes_count; i++) {
                    errs += type_check_node(tc, node->block.nodes[i]);
                }
                node->type.type = NULL;
                node->type.state = TsOk;
                return errs == 0;
            } break;
        case NodeRet:
            {
                if (!tc->fn) {
                    err("no function detected, not a function or what not.");
                    return 0;
                }
                if (!type_check_node(tc, node->ret)) {
                    err("failed to type check return expression.");
                    return 0;
                }
                if (is_untyped(node->ret)) { // handle untyped
                    if (!is_numeric(tc->fn->return_type)) {
                        err("can not return untyped values in fucntions "
                                " that return non-numeric values.");
                        return 0;
                    }
                    // set to fucntion return values as it's numeric
                    node->ret->type.type = tc->fn->return_type;
                    node->ret->type.state = TsOk;
                }
                if (!type_cmp(node->ret->type.type, tc->fn->return_type)) {
                    err("incompatible/invalid type between return statement "
                            "and return type.");
                    print_type(node->ret->type.type, 10);
                    printf(" | ");
                    print_type(tc->fn->return_type, 10);
                    printf("\n");
                    fflush(stdout);
                    assert(0&&"incompatible/invalid type between return "
                            "statement and return type.");
                    return 0;
                }
                info("Ret. Node");
                node->type.type = tc->fn->return_type;
                node->type.state = TsOk;
            } break;
        case NodeVar:
            {
                Variable const* v = ss_get_variable(tc->ss,node->var.name);
                if (!v) {
                    err("Failed to retrieve variable.");
                    return 0;
                }
                node->type.type = v->type;
                node->type.state = TsOk;
            } break;
        case NodeFnCall:
            {
            } break;
        case NodeUnary:
        case NodeBinOp:
        case NodeCast:
        case NodeNumLit:
            return type_check_expression(tc, node);
        default: err("unhandled/invalid node %d", node->kind);
                 assert(0); return 0;
    }
    return 1;
}
