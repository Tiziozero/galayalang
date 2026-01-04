#include "type_checker.h"
#include "parser.h"
#include "utils.h"



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
    if (t1->type == tt_struct && t1->size == t2->size) {
        return 0;
    } // TODO: handle aggregate data
    // TODO: implement arrays
    return 1;
}

// returns type to which to cast to (t1, t2 or null if none)
Type* can_implicit_cast(Type* t1, Type* t2) {
    if (type_cmp(t1, t2)) return t1;

    if (is_numeric(t1) && is_numeric(t2)) {
        if (is_signed(t1) && is_signed(t2)) {
            return t1->size > t2->size ? t1 : t2; // return larger one
        }
        if (is_unsigned(t1) && is_unsigned(t2)) {
            return t1->size > t2->size ? t1 : t2; // return larger one
        }
        if (is_float(t1) && is_float(t2)) {
            return t1->size > t2->size ? t1 : t2; // return larger one
        }
    }
    return NULL;
}

int type_check_fn_dec(ParserCtx *pctx, SymbolStore *ss, Node *node) {
    
    return 1;
}
int type_check_binop(ParserCtx *pctx, SymbolStore *ss, Node *node) {
    // if both are number literals OR neither is a num literal
    // then use default typecheck (to i32 or f32);
    if ((node->binop.left->type == NodeNumLit &&
            node->binop.right->type == NodeNumLit) || 
        (node->binop.left->type != NodeNumLit &&
             node->binop.right->type != NodeNumLit) ) {

        if (!type_check_node(pctx, ss, node->binop.left)) {
            err("Failed to type check left expression.");
            return 0;
        }
        if (!type_check_node(pctx, ss, node->binop.right)) {
            err("Failed to type check right expression.");
            return 0;
        }
        Type* left = node->binop.left->resulting_type;
        Type* right = node->binop.right->resulting_type;
        if (!type_cmp(left, right)) {
            Type* cast_to = 0;
            if ((cast_to = can_implicit_cast(left, right)) != 0) {
                return cast_to == left ?
                    cast(pctx, &node->binop.right, left) : // cast to left
                    cast(pctx, &node->binop.left, right); // cast to right
            } else {
                err("expression type is not the same as var dec type.");
                return 0;
            }
        }
        node->resulting_type = left; // same
        return 1;
    }
    // if left is number then cast it to right
    if (node->binop.left->type == NodeNumLit) {
        if (!type_check_node(pctx, ss, node->binop.right)) {
            err("Failed to type check right expression.");
            return 0;
        }
        if (!is_numeric(node->binop.right->resulting_type)) {
            err("Number literals can only binop with numeric types.");
            return 0;
        }
        // set to same
        node->binop.left->resulting_type = node->binop.right->resulting_type;
        return 1;
    }
    // if right is number then cast it to left
    if (node->binop.right->type == NodeNumLit) {
        if (!type_check_node(pctx, ss, node->binop.left)) {
            err("Failed to type check left expression.");
            return 0;
        }
        if (!is_numeric(node->binop.left->resulting_type)) {
            err("Number literals can only binop with numeric types.");
            return 0;
        }
        // set to same
        node->binop.right->resulting_type = node->binop.left->resulting_type;
        return 1;
    }
    return 0; // shoudn't happen?
}
int type_check_node(ParserCtx *pctx, SymbolStore *ss, Node *node) {
    switch (node->type) {
    
    case NodeVarDec:
        {
            Type* var_type = node->var_dec.type;
            if (var_type->type == tt_void) { // type can not be void
                err("Var type can not be void."); return 0;
            }
            if (!type_check_node(pctx, ss, node->var_dec.value)) {
                err("Failed to type check expression.");
                return 0;
            }
            if (!type_cmp(var_type, node->var_dec.value->resulting_type)) {
                err("expression type is not the same as var dec type.");
                return 0;
            }
            node->resulting_type = var_type;
            break;
        }
    case NodeFnDec:
            return type_check_fn_dec(pctx, ss, node);
    case NodeBinOp:
            return type_check_binop(pctx, ss, node);
    case NodeNumLit:
        {
            int has_dot = 0;
            for (size_t i = 0; i < node->number.str_repr.length; i++) {
                if (node->number.str_repr.name[i] == '.') {
                    has_dot = 1;
                }
            }
            if (has_dot) // it's a float
                node->resulting_type = ss_get_type(ss,
                        cstr_to_name("f32"));
            else // integer
                node->resulting_type = ss_get_type(ss,
                        cstr_to_name("i32"));
            return 1;
        }
    default: err("unhandled/invalid node %d", node->type);
             assert(0); return 0;
    }
    return 1;
}
