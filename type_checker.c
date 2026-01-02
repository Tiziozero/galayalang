#include "type_checker.h"
#include "parser.h"
#include "utils.h"

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
        |   t == tt_i64;
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
    if (is_numeric(t1) && is_numeric(t2)) return 1; // sure.
                                                  // can cast f64 -> u8
                                                  // u8 -> i8 etc
    return 0;
}
// cast first to second. no checks. just cast
Node* cast(ParserCtx* pctx, Type* t1, Type* t2, Node* target) {
    if (!can_cast_to(t1, t2)) return NULL;
    Node* n = new_node(pctx, NodeCast, (Token){0});
    if (!n) return NULL;
    Node* to = new_node(pctx, NodeTypeData, (Token){0});
    if (!to) return NULL;
    to->type_data = *t2;
    n->cast.to = to;
    n->cast.expr = target;
    return n;
}
int cast_to_prioritised_number(ParserCtx* pctx, Node** n1, Node** n2) {
    Type* t1 = (*n1)->expr_type;
    Type* t2 = (*n2)->expr_type;
    if (!is_numeric(t1) || !is_numeric(t2)) return 0;

    if (is_ptr(t1) && is_float(t2)) return 0;
    if (is_ptr(t2) && is_float(t1)) return 0;

    if (is_float(t1) && is_float(t2)) {
        if (t2->size > t1->size) {
            Node* cast_node = cast(pctx,t1, t2, *n1);
            *n1 = cast_node;
        } else {
            Node* cast_node = cast(pctx,t2, t1, *n2);
            *n2 = cast_node;
        }
    } else if (is_float(t1) && !is_float(t2)) {
        Node* cast_node = cast(pctx,t2, t1, *n2);
        *n2 = cast_node;
    } else if (!is_float(t1) && is_float(t2)) {
        Node* cast_node = cast(pctx,t1, t2, *n1);
        *n1 = cast_node;
    } else { // no floats
        if (t2->size > t1->size) { // larger bitwidth takes precedence
            Node* cast_node = cast(pctx,t1, t2, *n1);
            *n1 = cast_node;
        } else {
            Node* cast_node = cast(pctx,t2, t1, *n2);
            *n2 = cast_node;
        }
    }
    return 1;
}
int type_cmp(Type* t1, Type* t2) {
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
    // } else if (is_numeric(t1)) { // numbers sure // all numbers
    //     return 1;
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
                Node* cast_node = cast(pctx, node->var_dec.value->expr_type,
                        node->expr_type, node->var_dec.value);

                if (!cast_node) {
                    err("Failed to cast node.");
                    return 0;
                }
                node->var_dec.value = cast_node;
            } else { // if can't cast, fail
                err("can't cast types.");
                return 0;
            }
        }
        // if they are the same then ok. do nothing
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
    }
    return errs == 0;
}
int type_check_node(ParserCtx* pctx, SymbolStore* ss, Node* node) {
    switch (node->type) {
        case NodeVarDec:
            return check_let(pctx, ss, node);
        case NodeBinOp:
            return check_binop(pctx, ss, node);
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

