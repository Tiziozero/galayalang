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
    ZeroInit,
    FixedSizeArray,
    Any,
    Void,
}

// one instantiation of a generic struct, looked up by args
StructInstance :: struct {
    args: []TypeId,
    type: TypeId,
}

Type :: struct {
    name: string,
    kind: TypeKind,
    ptr: TypeId,
    fn: struct {
        args: []Arg,
        ret_ty: TypeId,
        generic_names: []string,

        is_external: bool,
        is_export: bool,

        is_variadic: bool,
        variadic_ty: TypeId,
        variadic_name: string,
        // if it's a gala function then variadic type is going to be a slice
        gala_abi_ty: TypeId,

        // for generics
        is_generic: bool,
        versions: [dynamic]FnVersion,
    },
    structure: struct {
        fields: []Field,

        // for generics
        // template: has $T fields, never lowered, can't be used as a bare type
        is_generic: bool,
        // is anonymous
        is_anonymous: bool,
        // $ names in order of first appearance, this is the order of Name(a, b)
        generic_names: []string,
        // on a template: one entry per distinct args
        instances: [dynamic]StructInstance,

        // on an instance (named like `Box.i64`)
        is_instance: bool,
        origin: TypeId,  // its template
        args: []TypeId,  // one per generic_names entry
    },
    // always anonymous and interned, never generic
    union_fields: []Field,
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
type_size :: proc(type_id: TypeId) -> int {
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
        return ty.fixed_size_array.size * type_size(ty.fixed_size_array.type)
    case .Struct:
        n := len(ty.structure.fields)
        if n == 0 do return 0

        offsets := struct_field_offsets(type_id)
        end := offsets[n - 1] + type_size(ty.structure.fields[n - 1].type)

        a := type_align_of(type_id)
        if end % a != 0 {
            end += a - (end % a)
        }
        return end
    case .Union:
        // biggest member, rounded up to the union's alignment
        biggest := 0
        for f in ty.union_fields do biggest = max(biggest, type_size(f.type))
        a := type_align_of(type_id)
        if biggest % a != 0 {
            biggest += a - (biggest % a)
        }
        return biggest
    case:
        gala_panic("type_size: unhandled kind", ty.kind)
    }
    return 0
}
type_size_no_abi :: proc(id: TypeId) -> int {
    t := get_type(id)

    #partial switch t.kind {
    case .Struct:
        size := 0
        for f in t.structure.fields {
            size += type_size(f.type)
        }
        return size

    case .Union:
        biggest := 0
        for f in t.union_fields do biggest = max(biggest, type_size(f.type))
        return biggest

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
        if l.fn.is_variadic != r.fn.is_variadic { return false }
        if l.fn.is_variadic && !types_equal(l.fn.variadic_ty, r.fn.variadic_ty) {
            return false
        }
        if !types_equal(l.fn.ret_ty, r.fn.ret_ty) { return false }
        if len(l.fn.args) != len(r.fn.args) { return false }

        for i in 0..<len(l.fn.args) {
            if !types_equal(l.fn.args[i].type, r.fn.args[i].type) {
                return false
            }
        }
        return true

    case .Pointer:
        return types_equal(l.ptr, r.ptr)

    case .Struct:
        // named structs stay nominal; anonymous ones are structural
        if !l.structure.is_anonymous || !r.structure.is_anonymous do return false
        if len(l.structure.fields) != len(r.structure.fields) do return false
        for lf, i in l.structure.fields {
            rf := r.structure.fields[i]
            if lf.name != rf.name || !types_equal(lf.type, rf.type) do return false
        }
        return true

    case .Union:
        if len(l.union_fields) != len(r.union_fields) do return false
        for lf, i in l.union_fields {
            rf := r.union_fields[i]
            if lf.name != rf.name || !types_equal(lf.type, rf.type) do return false
        }
        return true

    case .Slice:
        return types_equal(l.slice.type, r.slice.type)

    case .FixedSizeArray:
        return l.fixed_size_array.size == r.fixed_size_array.size &&
               types_equal(l.fixed_size_array.type, r.fixed_size_array.type)

    case .UInt64, .UInt32, .UInt16, .UInt_8,
        .Int64, .Int32, .Int16, .Int_8,
        .Flt64, .Flt32, .Flt16, .Flt_8,
        .Rune, .Byte, .Bool, .Void, .Any,
        .UntypedInteger, .UntypedFloat, .UntypedStruct, .ZeroInit:
        return true

    case .Invalid:
        return false
    }

    return false
}
types_equal :: proc(a, b: TypeId) -> bool {
    if a == b { return true }
    return type_equal(get_type(a)^, get_type(b)^)
}
is_untyped :: proc(ty: TypeId) -> bool {
    return is_numeric_untyped(ty) || get_type(ty).kind == .ZeroInit
}
can_binop :: proc(ty: TypeId, op: BinopKind) -> bool {
    if is_numeric(ty) do return true;
    if is_pointer(ty) && (op == .Addition ||
        op==.Subtraction || op == .NotEqual || op == .Equal) {
        return true;
    }
    return false;
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

    // Struct, Union, Slice, FixedSizeArray, Function, Void, ZeroInit, Untyped*, Invalid
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
is_valid_index_type :: proc(t: TypeId) -> bool {
    return is_integer(t) || is_byte_like(t)
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
//     .UntypedFloat, .ZeroInit): they have no runtime type, so there is
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
        // a generic function is a template, not a type you can name
        if ty.fn.is_generic do return false

        for arg in ty.fn.args {
            if !is_concrete_type(arg.type) do return false
        }
        if !is_concrete_type(ty.fn.ret_ty) do return false

        if ty.fn.is_variadic && !is_concrete_type(ty.fn.variadic_ty) {
            return false
        }
        return true

    case .Struct:
        // A bare template (`Box`) has $T fields and can't be used as a type.
        if ty.structure.is_generic do return false

        // An instance (`Box.i64`) is concrete exactly when all of its
        // arguments are. Inside a generic function body an instance can
        // still be waiting on an unsubstituted argument, e.g. `Box(T)`.
        if ty.structure.is_instance {
            for arg in ty.structure.args {
                if !is_concrete_type(arg) do return false
            }
        }

        // Deliberately not recursing into the fields: a non-generic struct
        // can't contain a generic field (it would be a template), and an
        // instance's fields are its template's fields with the arguments
        // substituted. Skipping them also avoids infinite recursion on
        // self-referential structs like `Node { next: ^Node }`.
        return true

    case .UInt64, .UInt32, .UInt16, .UInt_8,
         .Int64, .Int32, .Int16, .Int_8,
         .Flt64, .Flt32, .Flt16, .Flt_8,
         .Rune, .Byte, .Bool, .String, .Any, .Void,
         .Union: // fields can't be generic (the resolver rejects `$T` in them)
        return true

    case:
        // .Invalid, .UntypedInteger, .UntypedFloat, .ZeroInit
        return false
    }

    return false
}
