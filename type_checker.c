#include "type_checker.h"
#include "parser.h"
#include "utils.h"

// cast first to second. no checks. just cast
Node* cast(Type t1, Type t2) {
    return NULL;
}
int is_number(Type type) {
    TypeType t = type.type;
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

int can_cast(Type t1, Type t2) {
    printf("\t\tcan cast? "); print_type(&t1,5);printf(":"); 
    print_type(&t2,5); 
    if (t1.type == t2.type) {
        if (t1.type == tt_ptr) return 1; // im sure user knows what to do
        if (t1.type == tt_array) return 1; // im sure user knows what to do
        // both are struct and same size and size > 0 then sure, why not
        if (t1.type == tt_struct && t1.size == t2.size && t2.size > 0) {
            return 1;
        }
        // again, sure why not
        if (t1.type == tt_enum && t1.size == t2.size && t2.size > 0) {
            return 1;
        }
        if (t1.type == tt_enum && is_number(t2) || // enum to number
                t2.type == tt_enum && is_number(t1)) { //number to enum
            return 1;
        }
    }
    if (is_number(t1) && is_number(t2)) return 1; // sure
    return 1;
}

int type_cmp(Type t1, Type t2) {
    if (t1.type != t2.type) {
        return 0;
    }
    if (t1.type == tt_array) { // array size must be the same
        if (t1.static_array.size != t2.static_array.size) return 0; 
        return type_cmp(*t1.static_array.type,*t2.static_array.type);
    } else if (t1.type == tt_ptr) {
        return type_cmp(*t1.ptr,*t2.ptr); // compare pointer types
    } else if (is_number(t1)) { // numbers sure
        return 1;
    } else {
        print_type(&t1,5);printf(":"); print_type(&t2,5); 
        printf("\n");
        fflush(stdout);
        TODO("implement");
    }
    return 1;
}


// check if can perform op or unary. only base types and ptr
int is_op_valid(Type type) {
    TypeType t = type.type;

    return  is_number(type)
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
int type_check_node(SymbolStore* ss, Node* node) {
    switch (node->type) {
        case NodeVarDec:
            node->expr_type = node->var_dec.type;
            if (node->var_dec.value) {
                if (!type_check_node(ss, node->var_dec.value)) { // nodeptr
                    err("type check error in ver_dec value.");
                    return 0;
                }
                // if they're not the same
                if (!type_cmp(node->expr_type,
                            node->var_dec.value->expr_type)) {
                    // check if can cast
                    if (can_cast(node->expr_type, // will check compatability
                            node->var_dec.value->expr_type)) {
                        Node* cast_node = cast(node->var_dec.value->expr_type,
                                node->expr_type);
                        if (!cast_node) {
                            err("Failed to cast node.");
                            return 0;
                        }
                        cast_node->cast.expr = node->var_dec.value;
                        node->var_dec.value = cast_node; // set value to cast
                    } else { // if can't cast, fail
                        err("can't cast types.");
                        return 0;
                    }
                } else { // if they are the same then ok. do nothing
                }
            }
            break;
        case NodeNumLit:
            node->expr_type = node->number.type;
            break;
        default:
            err("Invalid not type in type checker./Node %s not handled.",
                    node_type_to_string(node->type));
            return 0;
    }
    return 1;
}

