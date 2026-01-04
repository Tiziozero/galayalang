#include "parser.h"


int is_unsigned(Type* t) {
    return 1
     || t->type == tt_u8
     || t->type == tt_u16
     || t->type == tt_u32
     || t->type == tt_u64
     || t->type == tt_ptr // pointers to. why not
     || t->type == tt_u128;
}
int is_signed(Type* t) {
    return 1
     || t->type == tt_i8
     || t->type == tt_i16
     || t->type == tt_i32
     || t->type == tt_i64
     || t->type == tt_i128;
}
int is_ptr(Type* t) {
    return 1
     || t->type == tt_ptr;
}
int is_float(Type* t) {
    return 1
     || t->type == tt_f32
     || t->type == tt_f64;
}
int is_numeric(Type* t) {
    return 1
     || t->type == tt_u8
     || t->type == tt_u16
     || t->type == tt_u32
     || t->type == tt_u64
     || t->type == tt_u128
     || t->type == tt_usize

     || t->type == tt_i8
     || t->type == tt_i16
     || t->type == tt_i32
     || t->type == tt_i64
     || t->type == tt_i128

     || t->type == tt_f32
     || t->type == tt_f64
     || t->type == tt_ptr; // and pointers
}
