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

int check_block_for_expected_return(ParserCtx *pctx, SymbolStore *ss,
        Node *node, Type* expected_type) {
    if (!expected_type) {
        err("No type provided");
        return 0;
    }
    int errs = 0;
    // check if/else and returns for expected return type in block
    for (size_t i = 0; i < node->block.nodes_count; i++) {
        Node* c = node->block.nodes[i];
        if (c->type == NodeRet) {
            if (!type_check_node(pctx, ss, c->ret)) {
                err("type check error in returnexpression. %d %d",
                        c->ret->type, c->ret->resulting_type.state);
                errs++; continue;
            }
            if (!type_cmp(expected_type, c->ret->resulting_type.type)) {
                err("type cmp failed in block ret");
                err("Return type is not expected.");
                errs++; continue;
            }
        } else if (c->type == NodeIfElse) {
            // check base condition
            if (!type_check_node(pctx, ss, c->if_else_con.base_condition)) {
                err("Failed typecheck in if statement base condition.");
                errs++;
            }
            if (!check_block_for_expected_return(pctx, ss,
                        c->if_else_con.base_block, expected_type)) {
                err("Failed typecheck in if statement base block.");
                errs++;
            }
            // check alternate conditions
            for (size_t i = 0; i < c->if_else_con.count; i++) {
                if (!type_check_node(pctx, ss, c->if_else_con.
                            alternate_conditions[i])) {
                    err("Failed typecheck in if statement alt"
                            " condition %d.", i); errs++;
                }
                if (!check_block_for_expected_return(pctx, ss, c->if_else_con.
                            alternate_blocks[i], expected_type)) {
                    err("Failed typecheck in if statement alt"
                            " block %d.", i); errs++;
                }
            }
            // check else block
            if (c->if_else_con.else_block) {
                if (!check_block_for_expected_return(pctx, ss,
                            c->if_else_con.else_block, expected_type)) {
                    err("Failed typecheck in if statement else block.");
                    errs++;
                }
            }
        } else {
            if (!type_check_node(pctx, ss, c)) errs++;
        }
    }
    return errs == 0;;
}

int type_check_fn_dec(ParserCtx *pctx, SymbolStore *ss, Node *node) {
    if (!node->fn_dec.body) return 0;
    Node* block = node->fn_dec.body;
    return check_block_for_expected_return(pctx, ss, block, 
            node->fn_dec.return_type);
}

int propagate_type(ParserCtx *pctx, Type* t, Node* node) {
    dbg("propagating");
    if (!is_untyped(node)) return 1;
    switch (node->type) {
        case NodeNumLit:
            node->resulting_type.type = t;
            node->resulting_type.state = TsOk;
            return 1;
        case NodeBinOp:
            return propagate_type(pctx, t,node->binop.left)
                && propagate_type(pctx, t,node->binop.right);
        case NodeUnary:
            { //a whole bunch of type bs.
                info("Unary, ye");
                Node* target = node->unary.target;
                switch (node->unary.type) {
                    case UnNegative:
                        TODO("Handle");
                    case UnRef:
                    {
                        // Type* new_type = arena_alloc(&pctx->gpa,sizeof(Type));
                        assert(0&& "can't reference untyped data");
                        // fail? shouldn't have that: "&123"
                        // referencing a constant
                        // reference if it's a valid variable though
                    }
                    case UnDeref:
                        if (target->resulting_type.state == TsUntypedFloat) {
                            assert(0&&"cant dereference floats");
                        }
                        if (t->type == tt_ptr) {
                            target->resulting_type.type = t->ptr;
                            target->resulting_type.state = TsOk;
                            return 0;
                        }
                        assert(0&&"propagate type is not a ptr");
                        TODO("do rest");
                    case UnNot:
                        {
                            return propagate_type(pctx, t, target);
                        }
                    case UnCompliment:
                        {
                        }// keep
                    default: assert(0&&"invalid unary op.");
                }
                if (!propagate_type(pctx, t,node->unary.target)) return 0;
            } return 1;
        default:
            assert(0&&"propagate type");
            return 0;
    }
}

int type_check_binop(ParserCtx *pctx, SymbolStore *ss, Node *node) {
    Node* left = node->binop.left;
    Node* right = node->binop.right;
    if (!type_check_expression(pctx, ss, left) ||
        !type_check_expression(pctx, ss, right)) {
        err("Failed both binop expression");
        node->resulting_type.state = TsFailed;
        return 0;
    }

    if (left->resulting_type.state == right->resulting_type.state) {
        if (left->resulting_type.state == TsOk) { // if it got the type
            if (type_cmp(left->resulting_type.type,
                            right->resulting_type.type)) {
                node->resulting_type.state = TsOk;
                goto ok;
            } else {
                node->resulting_type.state = TsIncompatible;
                err("type cmpt fialed in binop check");
                err("incompatible");
                return 0;
            }
        } else if (left->resulting_type.state == TsUntypedInt) {
            node->resulting_type.state = TsUntypedInt;
            return 1;
        } else if (left->resulting_type.state == TsUntypedFloat) {
            node->resulting_type.state = TsUntypedFloat;
            return 1;
        } else if (left->resulting_type.state == TsFailed) {
            node->resulting_type.state = TsFailed;
            return 0;
        }
        assert(0 && "wtf?? shouldn't happend");
    }
    TODO("handle case where states are not the same (Ok/untyped, failed/ok etc");


    assert(0 && "Shouldn't happend, really."
            " all cases should be handeled");
    return 0;
ok:
    info("Binop res %zu", node->resulting_type.type);
    return 1;
}



int can_cast(Node* node, Type* to, SymbolStore* ss) {
    if (type_cmp(node->resulting_type.type, to)) return 1; // same type
    
    if (is_numeric(node->resulting_type.type)
            && is_numeric(to)) return 1;

    // both are ptrs (already covered but ok)
    if (is_ptr(node->resulting_type.type) && is_ptr(to)) return 1;

    // both are structs with same size
    if (is_struct(node->resulting_type.type) && is_struct(to)
            && node->resulting_type.type->size == to->size) return 1;
    info("None?"); assert(0);
    return 0;
}
int type_check_expression(ParserCtx *pctx, SymbolStore *ss, Node *node){
    switch (node->type) {
        case NodeNumLit:
            {
                int has_dot = 0;
                for (size_t i = 0; i < node->number.str_repr.length; i++)
                    if (node->number.str_repr.name[i] == '.') has_dot = 1;
                node->resulting_type.state = has_dot ?
                    TsUntypedFloat : TsUntypedInt;
                return 1;
            }
        case NodeBinOp: // ...
            return type_check_binop(pctx, ss, node);
        case NodeUnary:
            {
                info("Unary");
                if (!type_check_expression(pctx,ss,node->unary.target)) {
                    node->resulting_type.state=TsFailed;
                    err("failed unary inside");
                    return 0;
                }
                if (!is_numeric(node->unary.target->resulting_type.type)) {
                    node->resulting_type.state=TsFailed;
                    return 0;
                }
                info("Unary Ok");
                return 1;
                break;
            }
        case NodeVar:
            {
                info("var check");
                if (!type_check_node(pctx, ss, node)) { // check it exists
                    node->resulting_type.state = TsFailed;
                    err("failed var tyoecheck");
                    return 0;
                }
                if (!is_numeric(node->resulting_type.type)) {
                    err("var is not numeric");
                    return 0; // for expresson needs to be numeric
                }
                return 1;
            }; break;
        case NodeCast:
            {
                info("Cast");
                // first check expr
                if (!type_check_node(pctx, ss, node->cast.expr)) {
                    err("Failed tupecheck of expr cast");
                    return 0;
                }
                if (!node->cast.expr->resulting_type.type) assert(0
                        && "no type after cast expression type check");

                // if it's untyped expression and cast is a number then true
                if (is_untyped(node->cast.expr)) {
                    if(is_numeric(node->cast.to)) {
                        node->resulting_type.state = TsOk;
                        node->resulting_type.type = node->cast.to;
                        info("Cast Oke");
                        return 1;
                    } else {
                        err("Failed at untyped but not numeric");
                        node->resulting_type.state = TsFailed;
                        return 0;
                    }
                }
                // check if can cast
                info("Can cast?");
                if (!can_cast(node->cast.expr, node->cast.to, ss)) {
                    info("no");
                        err("Failed at can cast");
                    return 0;
                }
                info("can indeed cast");
                node->resulting_type.state = TsOk;
                node->resulting_type.type = node->cast.to;
                info("Cast Oke");
            } break;assert(0&&"fuck you");
        default: assert(0);
    }
    return 1;
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
            if (is_untyped(node->var_dec.value)) {
                propagate_type(var_type,node->var_dec.value);
            }
            if (!type_cmp(var_type, node->var_dec.value->resulting_type.type)) {
                err("type cmp failed in var dec");
                return 0;
            }
            node->resulting_type.type = var_type;
            break;
        }
    case NodeFnDec:
            return type_check_fn_dec(pctx, ss, node);
    case NodeVar:
            node->resulting_type.type   = node->var.type;
            node->resulting_type.state  = TsOk;
            return 1;
    case NodeUnary:
    case NodeBinOp:
    case NodeCast:
    case NodeNumLit: return type_check_expression(pctx, ss, node);
    default: err("unhandled/invalid node %d", node->type);
             assert(0); return 0;
    }
    return 1;
}
