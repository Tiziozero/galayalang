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
        fields: []Field,
        // generic_parameters: []string,
    },
    // always anonymous and interned, never generic
    union_fields: []string,
    fixed_size_array: struct { type: TypeId, size: int },
    slice: struct{type: TypeId},
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
    case .Union:
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
    case .Struct:
    case .Union:
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
    case .Generic: {
        return false;
    }
    case .String:
        return true

    case .Function:
    case .Pointer:
        return types_equal(l.ptr, r.ptr)

    case .Struct:
    case .Union:
    case .Slice:
        return types_equal(l.slice.type, r.slice.type)

    case .FixedSizeArray:
        return l.fixed_size_array.size == r.fixed_size_array.size &&
               types_equal(l.fixed_size_array.type, r.fixed_size_array.type)

    case .UInt64, .UInt32, .UInt16, .UInt_8,
        .Int64, .Int32, .Int16, .Int_8,
        .Flt64, .Flt32, .Flt16, .Flt_8,
        .Rune, .Byte, .Bool, .Void, .Any,
        .UntypedInteger, .UntypedFloat, .UntypedStruct:
        return true

    case .Invalid:
        return false
    }
    panic("impl")
    // return false
}
types_equal :: proc(a, b: TypeId) -> bool {
    if a == b { return true }
    return type_equal(get_type(a)^, get_type(b)^)
}
is_untyped :: proc(ty: TypeId) -> bool {
    return is_numeric_untyped(ty) || get_type(ty).kind == .UntypedStruct
}
can_transmute_to :: proc(target_id, to_id: TypeId) -> bool {
    return type_size(target_id) == type_size(to_id)
}
can_cast_to :: proc(target_id, to_id: TypeId) -> bool {
    target := get_type(target_id)
    to := get_type(to_id)

    // Integer -> integer/float/byte/bool/rune/pointer
    if is_integer(target_id) {
        if is_integer(to_id) || is_float(to_id) || is_byte_like(to_id) {
            return true
        }

        #partial switch to.kind {
        case .Bool, .Pointer:
            return true
        }

        return false
    }

    // Float -> integer/float/byte/rune
    if is_float(target_id) {
        if is_integer(to_id) || is_float(to_id) || is_byte_like(to_id) {
            return true
        }
        return false
    }

    // Pointer -> pointer/integer
    if target.kind == .Pointer {
        if to.kind == .Pointer || is_integer(to_id) {
            return true
        }
        return false
    }

    // Byte/Rune -> numeric
    if is_byte_like(target_id) {
        return is_numeric_typed(to_id) || to.kind == .Bool
    }

    // Bool -> integer/byte/rune
    if target.kind == .Bool {
        return is_integer(to_id) || is_byte_like(to_id)
    }

    // Struct, Union, Slice, FixedSizeArray, Function, Void, Untyped*, Invalid
    return false
}
is_array_type :: proc(t: TypeId) -> bool {
    #partial switch get_type(t).kind {
    case .FixedSizeArray, .Slice, .String:
        return true
    }
    return false
}
can_be_index :: proc(t: TypeId) -> bool {
    #partial switch get_type(t).kind {
    case .FixedSizeArray, .Slice, .String, .Pointer:
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

    case .Function:

    case .Struct:
      
    case .Union:

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
    }
    case .Struct: {
        for a in t.structure.fields {
            if is_generic(a.type) do return true
        }
    }
    case .Pointer: return is_generic(t.ptr)
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
can_binop :: proc(id: TypeId, op: BinopKind) -> bool {
    switch op {
    case .Addition, .Subtraction:
        return is_numeric(id) || (is_pointer(id) && !is_void_pointer(id))
    case .Multiply, .Divide:
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
    case .FixedSizeArray, .Slice: return true
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
