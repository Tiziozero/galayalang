#include "parser.h"


int is_unsigned(Type* t) {
    return 0
     || t->type == tt_u8
     || t->type == tt_u16
     || t->type == tt_u32
     || t->type == tt_u64
     || t->type == tt_ptr // pointers too. why not
     || t->type == tt_u128;
}
int is_signed(Type* t) {
    return 0
     || t->type == tt_i8
     || t->type == tt_i16
     || t->type == tt_i32
     || t->type == tt_i64
     || t->type == tt_i128;
}
int is_ptr(Type* t) {
    return 0
     || t->type == tt_ptr;
}
int is_struct(Type* t) {
    return 0
     || t->type == tt_struct;
}
int is_float(Type* t) {
    return 0
     || t->type == tt_f32
     || t->type == tt_f64;
}
int is_numeric(Type* t) {
    return 0
     || is_unsigned(t)
     || is_signed(t)
     || is_float(t)
     || is_ptr(t);
}
int is_untyped(Node *n) {
    return 0
     || (n->type.state & TsUntypedInt)
     || (n->type.state & TsUntypedUnsignedInt)
     || (n->type.state & TsUntypedFloat)
     || (n->type.state & TsUntypedStruct)
     || (n->type.state & TsUntypedArray);
}
int can_binop(Type* t) { // update ?
    if (is_numeric(t)) return 1;
    return 0;
}
