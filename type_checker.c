#include "type_checker.h"
#include "parser.h"
#include "utils.h"
#include <stdio.h>

Type* to_signed(SymbolStore* ss, Type* t) {
    switch (t->type) {
        case tt_u8:
        case tt_i8: return ss_get_type(ss, cstr_to_name("i8"));
        case tt_u16:
        case tt_i16: return ss_get_type(ss, cstr_to_name("i16"));
        case tt_u32:
        case tt_i32: return ss_get_type(ss, cstr_to_name("i32"));
        case tt_u64:
        case tt_ptr:
        case tt_usize:
        case tt_i64: return ss_get_type(ss, cstr_to_name("i64"));
        default: return NULL;
    }
}
int is_numeric(Type* type) {
    TypeType t = type->type;
    return  t == tt_u8
        |   t == tt_u16
        |   t == tt_u32
        |   t == tt_u64
        |   t == tt_i8
        |   t == tt_i16
        |   t == tt_i32
        |   t == tt_i64
        |   t == tt_ptr
        |   t == tt_usize
        |   t == tt_f32
        |   t == tt_f64;
}
int is_ptr(Type* type) {
    TypeType t = type->type;
    return  t == tt_ptr;
}
int is_float(Type* type) {
    TypeType t = type->type;
    return  t == tt_f32
        |   t == tt_f64;
}
int is_signed(Type* type) {
    TypeType t = type->type;
    return  t == tt_i8
        |   t == tt_i16
        |   t == tt_i32
        |   t == tt_i64;
}
int is_int(Type* type) {
    TypeType t = type->type;
    return  t == tt_u8
        |   t == tt_u16
        |   t == tt_u32
        |   t == tt_u64
        |   t == tt_i8
        |   t == tt_i16
        |   t == tt_i32
        |   t == tt_i64
        |   t == tt_usize
        |   t == tt_ptr;
}
// cast t1 to t2?
// check if can cast t1 to t2
int can_cast_to(Type* t1, Type* t2) {
    // printf("\t\tcan cast? "); print_type(t1,5);printf(":"); 
    // print_type(t2,5); 

    if (t1->type == t2->type) {
        if (t1->type == tt_ptr) return 1; // im sure user knows what to do
        if (t1->type == tt_array) return 1; // im sure user knows what to do
        // both are struct and same size and size > 0 then sure, why not
        if (t1->type == tt_struct && t1->size == t2->size && t2->size > 0) {
            return 1;
        }
        // again, sure why not
        if (t1->type == tt_enum && t1->size == t2->size && t2->size > 0) {
            return 1;
        }
        if (t1->type == tt_enum && is_numeric(t2) || // enum to number
                t2->type == tt_enum && is_numeric(t1)) { //number to enum
            return 1;
        }
    }
    if (is_numeric(t1) && is_numeric(t2)) {
        return 1; // sure.
    }
                                                  // can cast f64 -> u8
                                                  // u8 -> i8 etc
    return 0;
}
// cast first to second. no checks. just cast
int cast(ParserCtx* pctx, Type* t1, Type* t2, Node** target) {
    if (!can_cast_to(t1, t2)) return 0;
    Node* n = new_node(pctx, NodeCast, (Token){0});
    if (!n) return 0;
    Node* to = new_node(pctx, NodeTypeData, (Token){0});
    if (!to) return 0;
    to->type_data = *t2;
    n->cast.to = to;
    n->cast.expr = *target;
    // set node type too
    n->expr_type = t2;
    *target = n;
    return 1;
}
int cast_to_prioritised_number(ParserCtx* pctx, Node** n1, Node** n2) {
    Type* t1 = (*n1)->expr_type;
    Type* t2 = (*n2)->expr_type;
    if (!is_numeric(t1) || !is_numeric(t2)) return 0;

    if (is_ptr(t1)) {
        if (is_float(t2)) return 0; // no float and ptr
        // cast t2 to ptr
        return cast(pctx,t2, t1, n2); // n2 to ptr
    } else if (is_ptr(t2)) {
        if (is_float(t1)) return 0; // no float and ptr
        return cast(pctx,t1, t2, n1); // n1 to ptr
    } else {
        if (is_float(t1) && !is_float(t2))
            return cast(pctx,t2, t1, n2); // cast n2 to float
        else if (is_float(t2) && !is_float(t1)) {
            return cast(pctx,t1, t2, n1); // cast n1 to float
        }

        // cast to unsigned of larger size
        if (is_signed(t1) || is_signed(t2)) {
            // get larger one
            Type* t = to_signed(&pctx->symbols, t1->size > t2->size?t1: t2);
            return cast(pctx, t1, t, n1) && !cast(pctx, t2, t, n2);
        } else { // both unsigned -> to larger unsigned
            Type* t = t1->size > t2->size ? t1 : t2;
            return cast(pctx, t1, t, n1) && !cast(pctx, t2, t, n2);
        }
    }
    return 0;
}
int type_cmp(Type* t1, Type* t2) {
    /* info("typecmp %zu : %zu", t1, t2);
        print_type(t1,5);
        fflush(stdout);
        printf(":");
        fflush(stdout);
        print_type(t2,5); 
        fflush(stdout);
        printf("\n");
        fflush(stdout); */
    if (t1->type != t2->type) {
        return 0;
    }
    if (t1->type == tt_array) { // array size must be the same
        // and same array type
        if (t1->static_array.size != t2->static_array.size) return 0; 
        return type_cmp(t1->static_array.type,t2->static_array.type);
    } else if (t1->type == tt_ptr) {
        // check same type
        return type_cmp(t1->ptr,t2->ptr); // compare pointer types
    } else if (is_numeric(t1)) { // numbers sure // all numbers
        return 1;
    } else {
        print_type(t1,5);printf(":"); print_type(t2,5); 
        printf("\n");
        fflush(stdout);
        TODO("implement");
    }
    return 1;
}


// check if can perform op or unary. only base types and ptr
int is_op_valid(Type* type) {
    TypeType t = type->type;

    return  is_numeric(type)
        ||  t == tt_char
        ||  t == tt_ptr; // for now
}

/* 
 * at this point name and type of everything should be known.
 * just check types and cast numbers and what not
 * this expects types and names to be resolved and everything exists.
 * this uses Node->expr_type to determinate if types are valid and/or
 * castable and casts them accordingly
 */

int check_let(ParserCtx* pctx, SymbolStore* ss, Node* node) {
    node->expr_type = node->var_dec.type;
    if (node->var_dec.value) {
        if (!type_check_node(pctx, ss, node->var_dec.value)) { // node ptr
            err("type check error in ver_dec value.");
            return 0;
        }
        // if they're not the same
        if (!type_cmp(node->expr_type, node->var_dec.value->expr_type)) {
            // check if can cast t1 to t2
            if (can_cast_to(node->var_dec.value->expr_type,node->expr_type)) {
                int res = cast(pctx, node->var_dec.value->expr_type,
                        node->expr_type, &node->var_dec.value);
                if (!res) {
                    err("Failed to cast node.");
                    return 0;
                }
            } else { // if can't cast, fail
                err("can't cast types.");
                print_type(node->expr_type, 10);
                print_type(node->var_dec.value->expr_type, 2);
                fflush(stdout);
                return 0;
            }
        }
        // if they are the same then ok. do nothing
    }
    return 1;
}
int check_unary(ParserCtx* pctx, SymbolStore* ss, Node* node) {
    if (!type_check_node(pctx, ss, node->unary.target) ) return 0;
    if (!is_numeric(node->unary.target->expr_type)) return 0;
    switch (node->unary.type) {
        case UnRef:
            {
                Type* t = arena_alloc(&pctx->gpa, sizeof(Type));
                memset(t, 0, sizeof(Type));
                t->type = tt_ptr;
                t->ptr = node->unary.target->expr_type;
                node->expr_type = t;
                break;
            }
        case UnDeref:
            {
                node->expr_type = node->unary.target->expr_type->type
                    == tt_ptr ? node->unary.target->expr_type->ptr :
                    node->unary.target->expr_type->static_array.type;
                break;
            }
        case UnNegative:
            node->expr_type = to_signed(ss, node->unary.target->expr_type);
            break;
   
        default: assert(0 && "unhandled");
    }

    return 1;
}
int check_binop(ParserCtx* pctx, SymbolStore* ss, Node* node) {
    int errs = 0;
    if (!type_check_node(pctx, ss, node->binop.left)) {
        err("type error in binop left node.");
        errs++;
    }
    if (!type_check_node(pctx, ss, node->binop.right)) {
        err("type error in binop right node.");
        errs++;
    }
    if (errs > 0) return 0; // skip binop check
    if (!is_numeric(node->binop.left->expr_type)) {
        err("binop can only be performed with numbers.");
        errs++;
    }
    if (!is_numeric(node->binop.right->expr_type)) {
        err("binop can only be performed with numbers.");
        errs++;
    }
    // if they're not the same, cast
    if (!type_cmp(node->binop.left->expr_type,node->binop.right->expr_type)){
        if (!cast_to_prioritised_number(pctx,
                &node->binop.left, &node->binop.right)) {
            err("Failed to cast binop values.");
            return 0;
        }
        // should be the same after
        node->expr_type = node->binop.left->expr_type;
    }
    return errs == 0;
}


int check_block(ParserCtx* pctx, SymbolStore* ss, Node* node, Type* return_type) {
    Node* body = node;
    
    SymbolStore* block_ss = body->block.ss;
    size_t errs = 0;
    for (size_t i = 0; i < body->block.nodes_count; i++) {
        Node* n = body->block.nodes[i];

        if (n->type == NodeIfElse) { // check if's return
            if (!type_check_node(pctx, block_ss,
                        n->if_else_con.base_condition)) errs++;

            if (!check_block(pctx, block_ss, n->if_else_con.base_condition,
                        return_type)) errs++;

            // check alternate conditions
            for (size_t j = 0; j < n->if_else_con.count; j++) {
                if (!type_check_node(pctx, block_ss,
                            n->if_else_con.alternate_conditions[i])) errs++;
                if (!check_block(pctx, block_ss,
                    n->if_else_con.alternate_blocks[i], return_type)) errs++;
            }

            // check else con
            if (n->if_else_con.else_block)
                if (!check_block(pctx, block_ss,
                            n->if_else_con.else_block, return_type)) errs++;

            // if it's a return
        } else if (n->type == NodeRet) {
            // check return expr type
            // print_symbol_store(block_ss, 10);
            if (!type_check_node(pctx, block_ss, n->ret))
                errs++;
            else if (return_type ) {
                // if it's not the same as return type
                if (!type_cmp(n->ret->expr_type, return_type)) {
                    // check if can cast
                    if (can_cast_to(n->ret->expr_type, return_type)) {
                        // cast
                        return cast(pctx, n->ret->expr_type, return_type, &n->ret);
                    }
                    // else error
                    err("return type is not expected return type.");
                    errs++;
                }
            } else errs++; // if no return type then increase error
        } else {
            if (!type_check_node(pctx, block_ss, n))
                errs++;
        }
    }
    return 1;
}
int check_fn_call(ParserCtx* pctx, SymbolStore* ss, Node* node) {
    Function* fn = ss_get_fn(ss, node->fn_call.fn_name);
    if (!fn) {
        err("no fn");
        return 0;
    }
    if (fn->args_count != node->fn_call.args_count) {
        char buf[100];
        print_name_to_buf(buf, 100, node->fn_call.fn_name);
        err("Not same arg count for function %s. expected  %d got %d.",
                buf, fn->args_count, node->fn_call.args_count);
        assert(0);
        return 0;
    }
    int errs = 0;
    for (size_t  i = 0; i < fn->args_count; i++) {
        if (!type_check_node(pctx, ss, node->fn_call.args[i])) {
            err("Failed typecheck arg %zu", i); errs++;
            continue;
        }
        if (!type_cmp(node->fn_call.args[i]->expr_type, fn->args[i].type)) {
            // try to cast arg type
            if (!cast(pctx, node->fn_call.args[i]->expr_type,
                        fn->args[i].type, &node->fn_call.args[i])) {
                err("Expected");
                print_type(fn->args[i].type, 10);
                printf(" got ");
                print_type(node->fn_call.args[i]->expr_type, 10);
                fflush(stdout);
                assert(0);
            }
        }
    }
    return 1;
}
int check_fn(ParserCtx* pctx, SymbolStore* ss, Node* node) {
    // should be valid: smth or void
    Type* ret_type = node->fn_dec.return_type;
    size_t errs = 0;
    if (!check_block(pctx, ss, node->fn_dec.body, ret_type)) {
        err("fn body returns something different.");
        errs++;
    }
    return errs == 0;
}
int type_check_node(ParserCtx* pctx, SymbolStore* ss, Node* node) {
    switch (node->type) {
        case NodeVarDec:
            return check_let(pctx, ss, node);
        case NodeVar:
            {
                Variable* v = ss_get_variable(ss, node->var.name);
                if (!v) {
                    warn("var doesn;t exists");
                    return 0;
                }
                node->expr_type = v->type;
                return 1;
            }
        case NodeBinOp:
            {
                int res = check_binop(pctx, ss, node);
                return res;
            }
        case NodeUnary:
            {
                int res = check_unary(pctx, ss, node);
                return res;
            }
        case NodeFnDec:
            return check_fn(pctx, ss, node);
        case NodeFnCall:
            return check_fn_call(pctx, ss, node);
        case NodeNumLit:
            node->expr_type = node->number.type;
            return node->expr_type != 0;
        default:
            err("Invalid not type in type checker./Node %s not handled.",
                    node_type_to_string(node->type));
            return 0;
    }
    return 0;
}

