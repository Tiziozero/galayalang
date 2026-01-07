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

int check_block_and_ret_type(ParserCtx *pctx, SymbolStore *ss, Node *node, Type* t) {
    Node** nodes = node->block.nodes;
    size_t count = node->block.nodes_count;
    size_t errs = 0;
    for (size_t i = 0; i < count; i++) {
        Node* n = nodes[i];
        switch (n->type) {
            case NodeIfElse:
            {   // check base condition
                if (!type_check_node(pctx, ss, n->if_else_con.base_condition)){
                    errs++;
                    err("failed to type check if base condition");
                }
                // check base block
                if (!check_block_and_ret_type(pctx, ss,
                            n->if_else_con.base_block,t)){
                    errs++;
                    err("failed to type check if base block");
                }
                // check alternate conditions "else if"s
                for (size_t j = 0; j < n->if_else_con.count; j++) {
                    // check base condition
                    Node* con = n->if_else_con.alternate_conditions[i];
                    if (!type_check_node(pctx, ss, con)){
                        errs++;
                        err("failed to type check if base condition");
                    }
                    // check base block
                    Node* block = n->if_else_con.alternate_blocks[i];
                    if (!check_block_and_ret_type(pctx, ss,
                                n->if_else_con.base_block,t)){
                        errs++;
                        err("failed to type check if base block");
                    }
                }
                if (!check_block_and_ret_type(pctx, ss,n->if_else_con.else_block, t)) {
                    errs++;
                    err("Failed to type check else block.");
                }
            } break; // if_else_con
            case NodeRet: {
                if (!type_check_node(pctx,ss,nodes[i]->ret)) return 0;
                // sure why not. shorter to reference
                n->resulting_type = n->ret->resulting_type;
                if (!type_cmp(t, nodes[i]->resulting_type.type)) {
                    errs++;
                    err("return type is not expected type");
                    print_type(t, 10);
                    print_type(n->resulting_type.type, 10);
                    printf("\n");
                }
            } break;
            default: errs += !type_check_node(pctx, ss, nodes[i]); break;
        }
    }
    return errs == 0;
}
int type_check_fn_dec(ParserCtx *pctx, SymbolStore *ss, Node *node) {
    if (!node->fn_dec.body) return 0;
    Type* ret_type = node->fn_dec.return_type;
    Node* block = node->fn_dec.body;
    Node** nodes = block->block.nodes;
    size_t errs = 0;
    for (size_t i = 0; i < block->block.nodes_count; i++) {
        Node* n = nodes[i]; 
        dbg("\tNode %s", node_type_to_string(n->type));
        switch (n->type) {
            case NodeRet:
            {
                if (!type_check_node(pctx,ss,nodes[i]->ret)) return 0;
                n->resulting_type = n->ret->resulting_type;
                if (!type_cmp(ret_type, nodes[i]->resulting_type.type)) {
                    err("return type is not expected type");
                    print_type(ret_type, 10);
                    print_type(n->resulting_type.type, 10);
                    printf("\n");
                }
            }; break;
            case NodeIfElse:
                errs += !check_block_and_ret_type(pctx,ss,n,ret_type); break;
            default: errs += !type_check_node(pctx, ss, nodes[i]); break;
        }
    }
    dbg("%zu errors in function.", errs);
    return errs == 0;
}

int propagate_type(ParserCtx *pctx, Type* t, Node* node) {
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
        case NodeVar: return 1;
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
    warn("None?"); assert(0);
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
                if (!type_check_expression(pctx,ss,node->unary.target)) {
                    node->resulting_type.state=TsFailed;
                    err("failed unary inside");
                    return 0;
                }
                if (!is_numeric(node->unary.target->resulting_type.type)) {
                    node->resulting_type.state=TsFailed;
                    return 0;
                }
                return 1;
                break;
            }
        case NodeVar:
            {
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
                        return 1;
                    } else {
                        err("Failed at untyped but not numeric");
                        node->resulting_type.state = TsFailed;
                        return 0;
                    }
                }
                // check if can cast
                if (!can_cast(node->cast.expr, node->cast.to, ss)) {
                    err("Failed at can cast");
                    return 0;
                }
                node->resulting_type.state = TsOk;
                node->resulting_type.type = node->cast.to;
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
                propagate_type(pctx,var_type,node->var_dec.value);
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
