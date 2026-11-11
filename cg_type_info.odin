// cg_typeinfo.odin
package main

TypeLL :: struct {
    info, field, str, slice: string,
}

// kind None = no runtime meaning (untyped, generic templates, void)
// a type is a runtime type if it, and everything it points at, is concrete
is_runtime_type :: proc(tid: TypeId) -> bool {
    t := get_type(tid)
    #partial switch t.kind {
    case .Invalid, .Generic, .UntypedInteger, .UntypedFloat,
         .UntypedStruct, .Void:
        return false
    case .Struct:
        // don't recurse into fields: structs can point at themselves.
        // templates and placeholder instances (`Box(T)`) are not runtime
        return struct_is_concrete(tid)
    case .Union:
        // anonymous, can't be self-referential, so recursing is safe
        for f in t.union_fields {
            if !is_runtime_type(f.type) do return false
        }
        return true
    case .Pointer:
        // rawptr points at void, that's fine
        if get_type(t.ptr).kind == .Void do return true
        return is_runtime_type(t.ptr)
    case .Slice:
        return is_runtime_type(t.slice.type)
    case .FixedSizeArray:
        return is_runtime_type(t.fixed_size_array.type)
    case .Function:
        if t.fn.is_generic do return false
        for a in t.fn.args {
            if !is_runtime_type(a.type) do return false
        }
        return t.fn.ret_ty == void_type() || is_runtime_type(t.fn.ret_ty)
    }
    return true
}

// `{ptr, i64}` constant for a string, emitting its global if needed
cg_str_const :: proc(c: ^CGCtx, ll: TypeLL, s: string) -> string {
    r, ok := c.cg_strings[s]
    if !ok {
        t := new_tmp(c, p="string", symbol=true)
        r = emit_string_global(t, s)
        cwritefln(c, "%s", r.ir)
        c.cg_strings[s] = r
    }
    return aprintf(c, "%s {{ ptr %s, i64 %d }", ll.str, r.s, r.len)
}

cg_empty_slice :: proc(c: ^CGCtx, ll: TypeLL) -> string {
    return aprintf(c, "%s {{ ptr null, i64 0 }", ll.slice)
}

// one private [N x TypeField] global per struct/union, returns the slice constant
cg_type_fields :: proc(c: ^CGCtx, ll: TypeLL, tid: TypeId) -> string {
    t := get_type(tid)
    fields: []Field
    is_union := false
    #partial switch t.kind {
    case .Struct: fields = t.structure.fields
    case .Union:  fields = t.union_fields; is_union = true
    }
    if len(fields) == 0 do return cg_empty_slice(c, ll)

    owner_ll := ty_to_llvm_str(c, tid)
    items := make([dynamic]string, allocator=get_ctx().allocator)
    for f, k in fields {
        off := "0"
        if !is_union {
            // let LLVM compute the offset, so it always matches the real layout
            off = aprintf(c, "ptrtoint (ptr getelementptr (%s, ptr null, i32 0, i32 %d) to i64)",
                owner_ll, k)
        }
        append(&items, aprintf(c, "%s {{ %s, i64 %d, i64 %s }",
            ll.field, cg_str_const(c, ll, f.name), i64(f.type), off))
    }

    g := new_tmp(c, p="tfields", symbol=true)
    cwritefln(c, "%s = private constant [%d x %s] [", g, len(fields), ll.field)
    for it, i in items {
        sep := "," if i + 1 < len(items) else ""
        cwritefln(c, "%s", aprintf(c, "\t%s%s", it, sep))
    }
    cwritefln(c, "]")
    return aprintf(c, "%s {{ ptr %s, i64 %d }", ll.slice, g, len(fields))
}

cg_type_entry :: proc(c: ^CGCtx, ll: TypeLL, tid: TypeId) -> string {
    if !is_runtime_type(tid) {
        return aprintf(c, "%s {{ %s {{ ptr null, i64 0 }, i64 0, i64 0, i64 0, i64 0, %s }",
            ll.info, ll.str, cg_empty_slice(c, ll))
    }
    t := get_type(tid)

    elem: i64
    count: int
    #partial switch t.kind {
    case .Pointer: elem = i64(t.ptr)
    case .Slice:   elem = i64(t.slice.type)
    case .FixedSizeArray:
        elem  = i64(t.fixed_size_array.type)
        count = t.fixed_size_array.size
    }

    return aprintf(c, "%s {{ %s, i64 %d, i64 %d, i64 %d, i64 %d, %s }",
        ll.info,
        cg_str_const(c, ll, tts(tid)),
        i64(rt_kind_of(t.kind)),
        type_size(tid),
        elem, count,
        cg_type_fields(c, ll, tid))
}

// @__gala_types: one entry per TypeId, so type_info(id) is a plain GEP
cg_type_table :: proc(c: ^CGCtx) {
    info_tid  := ty_from_name("TypeInfo")
    field_tid := ty_from_name("TypeField")
    ll := TypeLL{
        info  = ty_to_llvm_str(c, info_tid),
        field = ty_to_llvm_str(c, field_tid),
        str   = ty_to_llvm_str(c, ty_from_name("string")),
        slice = ty_to_llvm_str(c, intern_type(Type{kind=.Slice, slice={type=field_tid}})),
    }

    // length is re-read each step: lowering a type may intern new ones
    entries := make([dynamic]string, allocator=get_ctx().allocator)
    for i := 0; i < len(get_ctx().types); i += 1 {
        append(&entries, cg_type_entry(c, ll, TypeId(i)))
    }

    cwritefln(c, "@__gala_types = constant [%d x %s] [", len(entries), ll.info)
    for e, i in entries {
        sep := "," if i + 1 < len(entries) else ""
        cwritefln(c, "%s", aprintf(c, "\t%s%s", e, sep))
    }
    cwritefln(c, "]")
}
