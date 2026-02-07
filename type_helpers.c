#include "parser.h"
#include "logger.h"


int type_is_untyped(Type* t) {
    if (!t) panic("type_is_unsigned: NULL type");
    return 0
     || t->type == tt_untyped_unsigned_int
     || t->type == tt_untyped_int
     || t->type == tt_untyped_float
     || t->type == tt_untyped_struct;
}
/* GPTMAXXING */
// type stuff
int type_is_unsigned(Type* t) {
    if (!t) panic("type_is_unsigned: NULL type");
    return 0
     || t->type == tt_u8
     || t->type == tt_u16
     || t->type == tt_u32
     || t->type == tt_u64
     || t->type == tt_u128
     || t->type == tt_untyped_unsigned_int
     || t->type == tt_ptr; // pointer arithmetic
}

int type_is_signed(Type* t) {
    if (!t) panic("type_is_signed: NULL type");
    return 0
     || t->type == tt_i8
     || t->type == tt_i16
     || t->type == tt_i32
     || t->type == tt_i64
     || t->type == tt_untyped_int
     || t->type == tt_i128;
}

int type_is_float(Type* t) {
    if (!t) panic("type_is_float: NULL type");
    return 0
     || t->type == tt_untyped_float
     || t->type == tt_f32
     || t->type == tt_f64;
}

int type_is_ptr(Type* t) {
    if (!t) panic("type_is_ptr: NULL type");
    return t->type == tt_ptr;
}

int type_is_struct(Type* t) {
    if (!t) panic("type_is_struct: NULL type");
    return t->type == tt_struct;
}

int type_is_numeric(Type* t) {
    if (!t) panic("type_is_numeric: NULL type");
    return 0
     || type_is_unsigned(t)
     || type_is_signed(t)
     || type_is_float(t);
}


// state bs
int state_is_untyped_signed(TypeState s) {
    return s & TsUntypedInt;
}

int state_is_untyped_unsigned(TypeState s) {
    return s & TsUntypedUnsignedInt;
}

int state_is_untyped_float(TypeState s) {
    return s & TsUntypedFloat;
}

int state_is_untyped_numeric(TypeState s) {
    return 0
     || state_is_untyped_signed(s)
     || state_is_untyped_unsigned(s)
     || state_is_untyped_float(s);
}

int state_is_untyped_struct(TypeState s) {
    return s & TsUntypedStruct;
}

int state_is_untyped_array(TypeState s) {
    return s & TsUntypedArray;
}

int state_is_untyped(TypeState s) {
    return 0
     || state_is_untyped_numeric(s)
     || state_is_untyped_struct(s)
     || state_is_untyped_array(s);
}

// type info


//  nodes
int node_is_untyped(Node* n) {
    return type_info_is_untyped(n->type);
}

int node_is_numeric(Node* n) {
    return type_info_is_numeric(n->type);
}

int node_can_binop(Node* n) {
    return node_is_numeric(n);
}

