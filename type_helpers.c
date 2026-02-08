#include "parser.h"
#include "logger.h"


int type_is_untyped(Type* t) {
    if (!t) panic("type_is_unsigned: NULL type");
    return 0
     || t->kind == tt_untyped_unsigned_int
     || t->kind == tt_untyped_int
     || t->kind == tt_untyped_float
     || t->kind == tt_untyped_struct;
}
/* GPTMAXXING */
// type stuff
int type_is_unsigned(Type* t) {
    if (!t) panic("type_is_unsigned: NULL type");
    return 0
     || t->kind == tt_u8
     || t->kind == tt_u16
     || t->kind == tt_u32
     || t->kind == tt_u64
     || t->kind == tt_u128
     || t->kind == tt_untyped_unsigned_int
     || t->kind == tt_ptr; // pointer arithmetic
}

int type_is_signed(Type* t) {
    if (!t) panic("type_is_signed: NULL type");
    return 0
     || t->kind == tt_i8
     || t->kind == tt_i16
     || t->kind == tt_i32
     || t->kind == tt_i64
     || t->kind == tt_untyped_int
     || t->kind == tt_i128;
}

int type_is_float(Type* t) {
    if (!t) panic("type_is_float: NULL type");
    return 0
     || t->kind == tt_untyped_float
     || t->kind == tt_f32
     || t->kind == tt_f64;
}

int type_is_ptr(Type* t) {
    if (!t) panic("type_is_ptr: NULL type");
    return t->kind == tt_ptr;
}

int type_is_struct(Type* t) {
    if (!t) panic("type_is_struct: NULL type");
    return t->kind == tt_struct;
}

int type_is_numeric(Type* t) {
    if (!t) panic("type_is_numeric: NULL type");
    return 0
     || type_is_unsigned(t)
     || type_is_signed(t)
     || type_is_float(t);
}



// type info


//  nodes
int node_is_untyped(Node* n) {
    return type_is_untyped(n->type);
}

int node_is_numeric(Node* n) {
    return type_is_numeric(n->type);
}

int node_can_binop(Node* n) {
    return node_is_numeric(n);
}

