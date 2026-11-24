package main

TypeKind :: enum {
    Invalid,
    Generic,
    // basics
    // unsigned int
    UInt64,
    UInt32,
    UInt16,
    UInt_8,
    // signed int
    Int64,
    Int32,
    Int16,
    Int_8,
    // floating point
    Flt64,
    Flt32,
    Flt16,
    Flt_8,
    // rune (just a byte/i8)
    Rune,
    // byte (i8)
    Byte,
    // bool (i1)
    Bool,
    // aggregate
    Struct,
    Union, // untagged, all fields at offset 0
    // pointer stuff
    Slice, // { ptr, i64 }
    Pointer,
    Function, // ptr
    // literals
    UntypedInteger,
    UntypedFloat,
    UntypedStruct,
    String,
    FixedSizeArray,
    Any,
    Void,
}

Field :: struct {
    name: string,
    type: TypeId,
}
Type :: struct {
    name: string,
    kind: TypeKind,
    ptr: TypeId,
    fn: FunctionType,
    structure: struct {
        is_anonymous:       bool,
        is_generic:         bool,
        generic_parameters: []TypeId,
        fields: []Field,
        // set on instances only (0 = not an instance)
        template: TypeId,
        args:     []TypeId,
    },
    // always anonymous and interned, never generic
    union_fields: []Field, // always anonymous and interned, never generic
    fixed_size_array: struct { type: TypeId, size: int },
    slice: struct{type: TypeId},
}
FnParam :: struct {
    name: string,
    ty: TypeId,
}
FnGenericParam :: struct {
    name: string,
    ty: TypeId,
}
FnDefaultParam :: struct {
    name: string,
    ty: TypeId,
    inferred: bool,
    expr: ExprId,
    // types aren't interned, each function has it's own type
    obj:  ObjId, // for typechecking defaults and updating objects
}
FunctionType :: struct {
    args: []FnParam,
    type_args: []FnParam, // name (if needed) and generic type id
    ret_ty: TypeId,
    // wether it's a generic template - used for compile time
    is_generic: bool,
    // if it's generic - can't be generic and extern at the same time
    is_extern: bool,
    is_variadic: bool,
    variadic_name: string,
    variadic_ty, effective_variadic_ty: TypeId,
    generic: struct {
        template_id: int,
        generic_args: []FnGenericParam,
    },
    default_args: []FnDefaultParam,
}

// ---- classification helpers ----

is_integer_signed :: proc(id: TypeId) -> bool {
    k := get_type(id).kind
    #partial switch k {
    case .Int64, .Int32, .Int16, .Int_8: return true
    }
    return false
}

is_integer_unsigned :: proc(id: TypeId) -> bool {
    k := get_type(id).kind
    #partial switch k {
    case .UInt64, .UInt32, .UInt16, .UInt_8: return true
    }
    return false
}

is_integer :: proc(id: TypeId) -> bool {
    return is_integer_signed(id) || is_integer_unsigned(id)
}

is_byte_like :: proc(id: TypeId) -> bool {
    k := get_type(id).kind
    return k == .Rune || k == .Byte
}

is_float :: proc(id: TypeId) -> bool {
    k := get_type(id).kind
    #partial switch k {
    case .Flt64, .Flt32, .Flt16, .Flt_8: return true
    }
    return false
}

is_numeric_typed :: proc(id: TypeId) -> bool {
    return is_integer(id) || is_float(id) || is_byte_like(id)
}

is_numeric_untyped :: proc(id: TypeId) -> bool {
    k := get_type(id).kind
    return k == .UntypedInteger || k == .UntypedFloat
}

is_numeric :: proc(id: TypeId) -> bool {
    return is_numeric_typed(id) || is_numeric_untyped(id)
}

// Sizes for fixed-width kinds only. Struct/FixedSizeArray need the full Type
// (to walk fields/elem type via TypeId), so those take Type not TypeKind.
type_kind_size :: proc(k: TypeKind) -> int {
    #partial switch k {
    case .UInt64, .Int64, .Flt64:      return 8
    case .UInt32, .Int32, .Flt32:      return 4
    case .UInt16, .Int16, .Flt16:      return 2
    case .UInt_8, .Int_8, .Flt_8:      return 1
    case .Rune, .Byte, .Bool:          return 1
    case .Pointer, .Function:          return 8
    case .String, .Slice:              return 16 // { ptr, i64 }
    }
    return 0 // caller should fall back to type_size for aggregates
}

align_up :: proc(n, align: int) -> int {
    return (n + align - 1) / align * align
}

// Alignment of a type in bytes, matching LLVM's layout for non-packed types:
// scalars align to their size, ptr+len pairs (slice, string, any) align to 8,
// arrays align like their element, structs like their most-aligned field.
// Empty structs have alignment 1.
type_align :: proc(type_id: TypeId, loc := #caller_location) -> int {
    ty := get_type(type_id)
    #partial switch ty.kind {
    case .UInt64, .Int64, .Flt64, .Pointer, .Function:
        return 8
    case .UInt32, .Int32, .Flt32:
        return 4
    case .UInt16, .Int16, .Flt16:
        return 2
    case .UInt_8, .Int_8, .Flt_8, .Byte, .Rune, .Bool:
        return 1
    case .Slice, .String, .Any:
        return 8
    case .FixedSizeArray:
        return type_align(ty.fixed_size_array.type, loc)
    case .Struct:
        if is_generic(type_id) {
            gala_panicf("type_align called on generic struct \"%s\" from %v", ty.name, loc)
        }
        max_align := 1
        for f in ty.structure.fields {
            max_align = max(max_align, type_align(f.type, loc))
        }
        return max_align
    case .Union:
        max_align := 1
        for f in ty.union_fields do max_align = max(max_align, type_align(f.type, loc))
        return max_align
    case .Generic:
        gala_panicf("type_align called on generic type \"%s\" from %v", ty.name, loc)
    case:
        gala_panic("type_align: unhandled kind", ty.kind)
    }
    gala_panic("type_align: unhandled kind", ty.kind)
}

// Size of a type in bytes, matching LLVM's layout for non-packed types:
// fields sit at their natural alignment, and the struct's total size is
// rounded up to its own alignment (trailing padding), so arrays of
// structs stay aligned.
type_size :: proc(type_id: TypeId, loc := #caller_location) -> int {
    ty := get_type(type_id)
    #partial switch ty.kind {
    case .UInt64, .Int64, .Flt64, .Pointer, .Function:
        return 8
    case .UInt32, .Int32, .Flt32:
        return 4
    case .UInt16, .Int16, .Flt16:
        return 2
    case .UInt_8, .Int_8, .Flt_8, .Byte, .Rune, .Bool:
        return 1
    case .Slice, .String, .Any:
        return 16
    case .FixedSizeArray:
        // an element's size is already a multiple of its alignment
        return ty.fixed_size_array.size * type_size(ty.fixed_size_array.type, loc)
    case .Struct:
        if is_generic(type_id) {
            gala_panicf("type_size called on generic struct \"%s\" from %v", ty.name, loc)
        }
        offset := 0
        max_align := 1
        for f in ty.structure.fields {
            a := type_align(f.type, loc)
            offset = align_up(offset, a)
            offset += type_size(f.type, loc)
            max_align = max(max_align, a)
        }
        // trailing padding
        return align_up(offset, max_align)
    // type_size
    case .Union:
        size, max_align := 0, 1
        for f in ty.union_fields {
            size = max(size, type_size(f.type, loc))
            max_align = max(max_align, type_align(f.type, loc))
        }
        return align_up(size, max_align)
    case .Generic:
        gala_panicf("type_size called on generic type \"%s\" from %v", ty.name, loc)
    case:
        gala_panic("type_size: unhandled kind", ty.kind)
    }
    gala_panic("type_size: unhandled kind", ty.kind)
}
type_size_no_abi :: proc(id: TypeId) -> int {
    t := get_type(id)

    #partial switch t.kind {
    case .Struct, .Union:
        return type_size(id)
    case .Slice:
        return 16 // { ptr, i64 }

    case .FixedSizeArray:
        return t.fixed_size_array.size * type_size(t.fixed_size_array.type)

    case .Pointer, .Function:
        return 8

    case .Any: return 16; // { ptr + type_id }
    case:
        return type_kind_size(t.kind)
    }
    panic("impl")
}

// ---- type identity ----
// "Are these the same type?" and nothing else: no `any` wildcard, no untyped
// literal reduction (those live in assignable / unify_binop in the type checker).
//   structs   nominal: same id, or different types
//   unions    always anonymous, so structural (same fields, names and order)
//   functions structural (never interned, see new_fn_type); arg names ignored
//   pointer / slice / array: structural over their element types

type_equal :: proc(l, r: Type) -> bool {
    if l.kind != r.kind do return false

    switch l.kind {
    case .Invalid:
        return false

    case .Generic:
        // Generic placeholders are not concrete types.
        return false

    case .Struct:
        // Named structs are nominal: identity is the TypeId, which types_equal
        // already checked. Anonymous structs are structural: same fields,
        // same names, same order.
        if !l.structure.is_anonymous || !r.structure.is_anonymous do return false
        if len(l.structure.fields) != len(r.structure.fields) do return false
        for i in 0..<len(l.structure.fields) {
            a, b := l.structure.fields[i], r.structure.fields[i]
            if a.name != b.name || !types_equal(a.type, b.type) do return false
        }
        return true
    case .UntypedStruct:
        return false // each literal is its own type, ids were compared already

    case .Function:
        if l.fn.is_generic != r.fn.is_generic do return false
        if l.fn.is_extern != r.fn.is_extern do return false
        if l.fn.is_variadic != r.fn.is_variadic do return false

        if l.fn.is_generic do return false

        if len(l.fn.args) != len(r.fn.args) do return false

        if !types_equal(l.fn.ret_ty, r.fn.ret_ty) do return false

        for i in 0..<len(l.fn.args) {
            if !types_equal(l.fn.args[i].ty, r.fn.args[i].ty) do return false
        }

        if l.fn.is_variadic {
            if !types_equal(l.fn.variadic_ty, r.fn.variadic_ty) do return false
        }

        return true

    case .Pointer:
        return types_equal(l.ptr, r.ptr)

    case .Slice:
        return types_equal(l.slice.type, r.slice.type)

    case .FixedSizeArray:
        return l.fixed_size_array.size == r.fixed_size_array.size &&
               types_equal(l.fixed_size_array.type, r.fixed_size_array.type)

    case .Union:
        if len(l.union_fields) != len(r.union_fields) do return false
        for i in 0..<len(l.union_fields) {
            a, b := l.union_fields[i], r.union_fields[i]
            if a.name != b.name || !types_equal(a.type, b.type) do return false
        }
        return true
    case .String,
         .UInt64, .UInt32, .UInt16, .UInt_8,
         .Int64, .Int32, .Int16, .Int_8,
         .Flt64, .Flt32, .Flt16, .Flt_8,
         .Rune, .Byte, .Bool, .Void, .Any,
         .UntypedInteger, .UntypedFloat:
        return true
    }

    return false
}

types_equal :: proc(a, b: TypeId) -> bool {
    if a == b do return true

    ta := get_type(a)
    tb := get_type(b)

    if ta.kind != tb.kind do return false

    return type_equal(ta^, tb^)
}


struct_field_offset :: proc(
    type_id: TypeId,
    field_index: int,
    loc := #caller_location,
) -> int {
    ty := get_type(type_id)

    if ty.kind != .Struct {
        gala_panicf("struct_field_offset called on non-struct type \"%s\" from %v", ty.name, loc)
    }

    if is_generic(type_id) {
        gala_panicf("struct_field_offset called on generic struct \"%s\" from %v", ty.name, loc)
    }

    if field_index < 0 || field_index >= len(ty.structure.fields) {
        gala_panicf("invalid field index %d for struct \"%s\"", field_index, ty.name)
    }

    offset := 0

    for i in 0..<field_index {
        f := ty.structure.fields[i]
        a := type_align(f.type, loc)

        offset = align_up(offset, a)
        offset += type_size(f.type, loc)
    }

    // Align the requested field itself.
    field := ty.structure.fields[field_index]
    return align_up(offset, type_align(field.type, loc))
}


struct_field_index :: proc(type_id: TypeId, name: string) -> (int, bool) {
    ty := get_type(type_id)

    if ty.kind != .Struct do return 0, false
    if is_generic(type_id) do return 0, false

    for f, i in ty.structure.fields {
        if f.name == name {
            return i, true
        }
    }

    return 0, false
}

struct_field_offset_by_name :: proc(
    type_id: TypeId,
    name: string,
    loc := #caller_location,
) -> (int, bool) {
    index, ok := struct_field_index(type_id, name)
    if !ok do return 0, false

    return struct_field_offset(type_id, index, loc), true
}

is_untyped :: proc(ty: TypeId) -> bool {
    return is_numeric_untyped(ty) || get_type(ty).kind == .UntypedStruct
}
is_void :: proc(ty: TypeId) -> bool {
    return get_type(ty).kind == .Void
}

// ---- cast / transmute ----

// Whether the type has a concrete, known size right now. Generic things,
// void, invalid and untyped literals have none. Unions are *not* excluded
// here on purpose: type_size panics on them until they're implemented.
is_sized :: proc(id: TypeId) -> bool {
    if is_generic(id) do return false

    t := get_type(id)
    #partial switch t.kind {
    case .Invalid, .Generic, .Void, .UntypedInteger, .UntypedFloat, .UntypedStruct:
        return false
    case .FixedSizeArray:
        return is_sized(t.fixed_size_array.type)
    case .Struct:
        for f in t.structure.fields {
            if !is_sized(f.type) do return false
        }
    }
    return true
}

// transmute reinterprets the bits, so all it needs is two sized types of
// exactly the same size. type_size matches the LLVM layout, padding included.
can_transmute_to :: proc(target_id, to_id: TypeId) -> bool {
    if !is_sized(target_id) || !is_sized(to_id) do return false
    return type_size(target_id) == type_size(to_id)
}

// `target_id` is the type of the value being cast, `to_id` is the type it's
// being cast to. Casts are for scalar conversions, anything that needs
// reinterpreting aggregates (or slices, strings, ...) goes through transmute.
//
//   integer / untyped int  ->  integer, float, byte/rune, bool, pointer
//   float / untyped float  ->  integer, float, byte/rune
//   byte / rune            ->  any typed numeric, bool
//   bool                   ->  integer, byte/rune
//   pointer                ->  pointer, integer
//   function               ->  pointer
//   anything               ->  itself (same TypeId)
can_cast_to :: proc(target_id, to_id: TypeId) -> bool {
    // identity cast. TypeId compare only: type_equal can't handle structs yet
    if target_id == to_id do return true

    target := get_type(target_id)
    to := get_type(to_id)

    // Integer (incl. untyped int literal) -> integer/float/byte/rune/bool/pointer
    if is_integer(target_id) || target.kind == .UntypedInteger {
        if is_numeric_typed(to_id) {
            return true
        }

        #partial switch to.kind {
        case .Bool, .Pointer:
            return true
        }

        return false
    }

    // Float (incl. untyped float literal) -> integer/float/byte/rune
    if is_float(target_id) || target.kind == .UntypedFloat {
        return is_numeric_typed(to_id)
    }

    // Pointer -> pointer/integer
    if target.kind == .Pointer {
        return to.kind == .Pointer || is_integer(to_id)
    }

    // Function -> pointer
    if target.kind == .Function {
        return to.kind == .Pointer
    }

    // Byte/Rune -> numeric/bool
    if is_byte_like(target_id) {
        return is_numeric_typed(to_id) || to.kind == .Bool
    }

    // Bool -> integer/byte/rune
    if target.kind == .Bool {
        return is_integer(to_id) || is_byte_like(to_id)
    }

    // Struct, Union, Slice, String, FixedSizeArray, Any, Void, UntypedStruct, Invalid
    return false
}
is_array_type :: proc(t: TypeId) -> bool {
    #partial switch get_type(t).kind {
    case .FixedSizeArray, .Slice, .String:
        return true
    }
    return false
}
is_pointer :: proc(id: TypeId) -> bool {
    return get_type(id).kind == .Pointer
}

// A `rawptr` is Pointer{ptr=void_type()} — treat it like C's `void*`: it's
// compatible with any other pointer type in either direction.
is_void_pointer :: proc(id: TypeId) -> bool {
    t := get_type(id)
    return t.kind == .Pointer && get_type(t.ptr).kind == .Void
}
// A type is concrete when it is fully known at compile time: nothing in it
// is (or depends on) a generic placeholder like `$T`. Used to reject things
// such as `any_as(v, $T)` / `type_id($T)` before they reach lowering, where
// there is no real type to compare against or emit an id for.
//
// Not concrete:
//   - .Generic itself
//   - a generic struct template (`Box` with $T fields)
//   - a generic function type
//   - any pointer / slice / array / function / struct instance that
//     contains one of the above
//   - .Invalid, and the untyped literal kinds (.UntypedInteger,
//     .UntypedFloat): they have no runtime type, so there is
//     no type id to ask for. A literal has to be given a type first.
is_concrete_type :: proc(id: TypeId) -> bool {
    ty := get_type(id)

    #partial switch ty.kind {
    case .Generic:
        return false

    case .Pointer:
        // rawptr is Pointer{ptr = void}, and void is concrete
        return is_concrete_type(ty.ptr)

    case .Slice:
        return is_concrete_type(ty.slice.type)

    case .FixedSizeArray:
        return is_concrete_type(ty.fixed_size_array.type)


    case .Struct:
        if is_generic(id) do return false
            return true
    case .Function:
        return !is_generic(id)

    case .Union:
        for f in ty.union_fields {
            if !is_concrete_type(f.type) do return false
        }
        return true

    case .UInt64, .UInt32, .UInt16, .UInt_8,
         .Int64, .Int32, .Int16, .Int_8,
         .Flt64, .Flt32, .Flt16, .Flt_8,
         .Rune, .Byte, .Bool, .String, .Any, .Void:
         
        return true

    case:
        // .Invalid, .UntypedInteger, .UntypedFloat
        return false
    }

    panic("impl")
}
is_generic :: proc(id: TypeId) -> bool {
    t := get_type(id);
    #partial switch t.kind {
    case .Generic: return true
    case .Function: {
        for a in t.fn.args { if is_generic(a.ty) do return true }
        if is_generic(t.fn.ret_ty) do return true
        if t.fn.is_variadic do if is_generic(t.fn.variadic_ty) do return true
    }
    case .Slice:          return is_generic(t.slice.type)
    case .FixedSizeArray: return is_generic(t.fixed_size_array.type)
    case .Struct: {
        for a in t.structure.fields {
            if is_generic(a.type) do return true
        }
    }
    case .Pointer: return is_generic(t.ptr)
    case .Union: {
        for f in t.union_fields {
            if is_generic(f.type) do return true
        }
    }
    }
    return false
}
is_bool :: proc(id: TypeId) -> bool {
    return get_type(id).kind == .Bool
}

// ints, bytes/runes, and untyped int literals (what % & | ^ << >> accept)
is_integral :: proc(id: TypeId) -> bool {
    return is_integer(id) || is_byte_like(id) || get_type(id).kind == .UntypedInteger
}

can_equal :: proc(id: TypeId) -> bool {
    if is_numeric(id) do return true
    #partial switch get_type(id).kind {
    case .Bool, .Pointer, .Function, .String: return true
    }
    return false
}

// strings left out on purpose, add them if you want lexicographic ordering
can_order :: proc(id: TypeId) -> bool {
    return is_numeric(id) || is_pointer(id)
}

// single-operand check; unify_binop still has to match the two sides
// no pointer arithmetic: pointers only get == != < <= > >= (via can_equal / can_order)
can_binop :: proc(id: TypeId, op: BinopKind) -> bool {
    switch op {
    case .Addition, .Subtraction, .Multiply, .Divide:
        return is_numeric(id)
    case .Modulo, .BitAnd, .BitOr, .BitXor, .ShiftLeft, .ShiftRight:
        return is_integral(id)
    case .Equal, .NotEqual:
        return can_equal(id)
    case .Less, .LessEqual, .Greater, .GreaterEqual:
        return can_order(id)
    case .LogicalAnd, .LogicalOr:
        return is_bool(id)
    }
    return false
}

can_negate :: proc(id: TypeId) -> bool {
    return is_integer_signed(id) || is_float(id) || is_numeric_untyped(id)
}

can_logical_not :: proc(id: TypeId) -> bool {
    return is_bool(id)
}

// ---- indexing ----

// fixed arrays and slices only, for now
can_be_indexed :: proc(id: TypeId) -> bool {
    #partial switch get_type(id).kind {
    case .FixedSizeArray, .Slice, .String: return true
    }
    return false
}

// includes untyped ints so `arr[0]` works
is_valid_index_type :: proc(id: TypeId) -> bool {
    return is_integral(id)
}

can_index :: proc(base, index: TypeId) -> bool {
    return can_be_indexed(base) && is_valid_index_type(index)
}

index_elem_type :: proc(id: TypeId) -> (TypeId, bool) {
    t := get_type(id)
    #partial switch t.kind {
    case .FixedSizeArray: return t.fixed_size_array.type, true
    case .Slice:          return t.slice.type, true
    }
    return {}, false
}

// ---- pointers ----

// can't deref rawptr, there's no pointee type
can_dereference :: proc(id: TypeId) -> bool {
    return is_pointer(id) && !is_void_pointer(id)
}

deref_type :: proc(id: TypeId) -> (TypeId, bool) {
    if !can_dereference(id) do return {}, false
    return get_type(id).ptr, true
}

// type-level only: void, invalid and untyped literals have no address.
// whether the *expression* is an lvalue is a separate check in the checker
can_reference :: proc(id: TypeId) -> bool {
    #partial switch get_type(id).kind {
    case .Void, .Invalid, .UntypedInteger, .UntypedFloat, .UntypedStruct:
        return false
    }
    return true
}
slice_from :: proc(t: TypeId) -> TypeId {
    return intern_type({kind=.Slice, slice = { t } })
}
