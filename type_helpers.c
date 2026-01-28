#include "parser.h"
#include "logger.h"


int is_unsigned(Type* t) {
	if (!t) panic("no type in unsigned");
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
int state_is_untyped_number(TypeState state) {
    return 0
     || (state & TsUntypedInt)
     || (state & TsUntypedFloat);
}
int state_is_untyped(TypeState state) {
    return 0
     || (state & TsUntypedInt)
     || (state & TsUntypedFloat)
     || (state & TsUntypedStruct)
     || (state & TsUntypedArray);
}
int type_info_is_numeric(NodeTypeInfo ti) {
    if (state_is_untyped_number(ti.state)) return 1; // numeric untyped
	if (state_is_untyped(ti.state)) return 0; // untyped but not numeric
	if (!ti.type) {
		return 0;
		panic("no type in type info/type is NULL");
	}
	return is_numeric(ti.type);
}
int can_binop(NodeTypeInfo ti) { // update ?
    if (type_info_is_numeric(ti)) return 1;
    return 0;
}
