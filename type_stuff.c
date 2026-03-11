#include "parser.h"
int is_untyped(Type* t) {
    return 0
        || t->kind == tt_untyped_float
        || t->kind == tt_untyped_int
        || t->kind == tt_untyped_unsigned_int
        || t->kind == tt_untyped_struct;
}

int is_float(Type* t) {
    return 0
        || t->kind == tt_f64
        || t->kind == tt_f32
        || t->kind == tt_untyped_float;
}

int is_unsigned(Type* t) {
    return 0
        || t->kind == tt_u8
        || t->kind == tt_u16
        || t->kind == tt_u32
        || t->kind == tt_u64
        || t->kind == tt_u128
        || t->kind == tt_usize
        || t->kind == tt_untyped_unsigned_int;
}

int is_signed(Type* t) {
    return 0
        || t->kind == tt_i8
        || t->kind == tt_i16
        || t->kind == tt_i32
        || t->kind == tt_i64
        || t->kind == tt_i128
        || t->kind == tt_untyped_int;
}

int is_integer(Type* t) {
    return is_signed(t) || is_unsigned(t);
}

int is_numeric(Type* t) {
    return is_integer(t) || is_float(t);
}

int is_pointer(Type* t) {
    return t->kind == tt_ptr;
}

int is_struct(Type* t) {
    return 0
        || t->kind == tt_struct
        || t->kind == tt_untyped_struct;
}

int is_void(Type* t) {
    return t->kind == tt_void;
}

int is_fn(Type* t) {
    return t->kind == tt_fn;
}

/* Returns 1 if the type can be used in arithmetic expressions */
int is_arithmetic(Type* t) {
    return is_numeric(t);
}

/* Returns 1 if the type can be compared with < > <= >= */
int is_ordered(Type* t) {
    return is_numeric(t) || t->kind == tt_char || t->kind == tt_ptr;
}

/* Returns 1 if the type can be compared with == != */
int is_comparable(Type* t) {
    return is_numeric(t) || t->kind == tt_char || t->kind == tt_ptr;
}

/* Returns 1 if the type has a known size at compile time */
int is_sized(Type* t) {
    return !is_untyped(t) && t->kind != tt_void && t->kind != tt_fn;
}

/* Returns 1 if the type needs to be resolved/inferred still */
int is_undetermined(Type* t) {
    return t->kind == tt_to_determinate;
}
int can_binop(Type* t) {
    return is_comparable(t) && is_numeric(t);
}
