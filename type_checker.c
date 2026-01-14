#include "type_checker.h"
#include "parser.h"
#include "print.h"
#include "utils.h"
#include <stddef.h>

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
    if (t1 == t2) return 1;
    if (!t1) {
        err("type 1 is null");
        assert(0);
        return 0;
    }
    if (!t2) {
        err("type 2 is null");
        return 0;
    }
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

struct TypeChecker* new_tc(struct TypeChecker* tc, ParserCtx* pctx, SymbolStore* ss) {
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


struct TypeChecker* tc_fn(struct TypeChecker* parent, SymbolStore* ss, Function* fn) {
    struct TypeChecker* tc = new_scope(parent, ss);
    tc->fn = fn;
    return tc;
}
int type_check_expression(Node* node) {

    return 1;
}
int type_check_node(TypeChecker* tc, Node *node) {
    dbg("Node check %s", node_type_to_string(node->type));
    switch (node->type) {
    case NodeVarDec:
        {
            Type* var_type = node->var_dec.type;
            if (var_type->type == tt_void) { // type can not be void
                err("Var type can not be void.");
                return 0;
            }
            if (!type_check_node(tc, node->var_dec.value)) {
                err("Failed to type check expression.");
                return 0;
            }
            if (is_untyped(node->var_dec.value)) {
                assert(0 && "untyped var??");
            }
            if (!type_cmp(var_type, node->var_dec.value->resulting_type.type)) {
                err("type cmp failed in var dec");
                return 0;
            }
            node->resulting_type.type = var_type;
            node->resulting_type.state = TsOk;
            break;
        }
    case NodeFnDec:
        {
            // create function type checker for return etc
            struct TypeChecker* fn_tc = tc_fn(tc, node->fn_dec.ss, &node->symbol.fn);
            int errs = 0;
            errs += type_check_node(fn_tc, node->fn_dec.body);
            node->resulting_type.state = TsOk;
            return errs == 0;
        } break;
    case NodeBlock:
        {
            size_t errs = 0;
            for (size_t i = 0; i < node->block.nodes_count; i++) {
                errs += type_check_node(tc, node->block.nodes[i]);
            }
            node->resulting_type.state = TsOk;
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
        if (!type_cmp(node->ret->resulting_type.type, tc->fn->return_type)) {
            err("incompatible type between return statement and return type.");
            return 0;
        }
        node->resulting_type.type = tc->fn->return_type;
        node->resulting_type.state = TsOk;
    } break;
    case NodeVar:
        node->resulting_type.type   = node->var.type;
        node->resulting_type.state  = TsOk;
        return 1;
    case NodeUnary:
    case NodeBinOp:
    case NodeCast:
    case NodeNumLit: return type_check_expression(node);
    default: err("unhandled/invalid node %d", node->type);
             assert(0); return 0;
    }
    return 1;
}
