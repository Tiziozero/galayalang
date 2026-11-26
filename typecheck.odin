package main

import "core:slice"
import "core:fmt"
// try global context
TcContext :: struct {
    in_function: bool,
    fn_ret_ty: Maybe(TypeId),
    in_loop: Maybe(StmtId),
}
tctx := TcContext{}


// ai
can_coerce_to :: proc(to, target: TypeId) -> bool {
    src := get_type(target)
    dst := get_type(to)

    #partial switch src.kind {
    case .UntypedInteger:
        // value range is a separate check on the literal itself
        return is_integer(to) || is_float(to) || is_byte_like(to) || is_pointer(to)

    case .UntypedFloat:
        return is_float(to)

    case .UntypedStruct:
        if is_generic(to) do return false
        return untyped_struct_fits(dst, src)
    }

    // already typed, so it's just identity
    return types_equal(to, target)
}

// 100% claude
// does the `{...}` literal (src) fit the struct/union type dst?
// like typed literals: every member must be given
untyped_struct_fits :: proc(dst, src: ^Type) -> bool {
    given := src.structure.fields
    positional := len(given) > 0 && given[0].name == ""

    #partial switch dst.kind {
    case .Struct:
        want := dst.structure.fields
        if len(given) != len(want) do return false
        if positional {
            for g, i in given {
                if !can_coerce_to(want[i].type, g.type) do return false
            }
            return true
        }
        for g in given {
            idx, found := find_field(want, g.name)
            if !found || !can_coerce_to(want[idx].type, g.type) do return false
        }
        return true

    case .Union:
        // exactly one named member
        if positional || len(given) != 1 do return false
        idx, found := find_field(dst.union_fields, given[0].name)
        return found && can_coerce_to(dst.union_fields[idx].type, given[0].type)
    }
    return false
}
// fucking mess of a function
assignable :: proc(to, target: TypeId, loc:=#caller_location) -> (TypeId, bool, string) {
    if is_untyped(to) {
        gala_panicf("assignable \"to\" can't be untyped, but is. ({})\n\tcalled here: {}", tts(to), loc)
    }
    if get(to).kind == .Any {
        t, ok, msg := get_assignable_default_type(target)
        if ok {
            if !is_concrete_type(target) {
                return 0, false, fmt.aprintf("rhs doesn't have a concrete/is generic type ({}).", tts(target))
            }
        }
        return t, ok, msg
    }
    if to == target {
        if !is_concrete_type(target) {
            return 0, false, fmt.aprintf("rhs doesn't have a concrete/is generic type ({}).", tts(target))
        }
        return  to, true, "ok1"
    }
    if types_equal(to, target) {
        if !is_concrete_type(target) {
            return 0, false, fmt.aprintf("rhs doesn't have a concrete/is generic type ({}).", tts(target))
        }
        return to, true, "ok1"
    }
    if is_untyped(target) {
        if can_coerce_to(to, target) {
            if !is_concrete_type(to) { // would be propagated to target. assert it's a concrete type
                return 0, false, fmt.aprintf("rhs doesn't have a concrete/is generic type ({}).", tts(target))
            }
            return to, true, "ok2"
        } else do return 0, false, fmt.aprintf("Can't coerce type {} to {}", tts(target), tts(to))
    }
    return 0, false, fmt.aprintf("types {} and {} don't match.", tts(target), tts(to)) 
}

//. 100% claude
// one fresh type per `{...}` literal. named fields keep their name,
// positional ones get "" (the resolver already rejects mixing the two)
untyped_aggregate_type :: proc(lit: AggregateLit) -> TypeId {
    fields := make([dynamic]Field)
    for n, f in lit.fields {
        append(&fields, Field{n, expr_ty(f.expr)})
    }
    for e in lit.positional {
        append(&fields, Field{"", expr_ty(e.expr)})
    }
    t := Type{kind = .UntypedStruct, name = "untyped struct"}
    t.structure.fields = fields[:]
    append(&get_ctx().types, t)
    return TypeId(len(get_ctx().types) - 1)
}

propagate_aggregate :: proc(lit: AggregateLit, t: TypeId) {
    ty := get_type(t)
    #partial switch ty.kind {
    case .Struct:
        fields := ty.structure.fields
        for n, f in lit.fields {
            idx, _ := find_field(fields, n)
            coerce_lit_value(f.expr, fields[idx].type)
        }
        for e, i in lit.positional {
            coerce_lit_value(e.expr, fields[i].type)
        }
    case .Union:
        for n, f in lit.fields {
            idx, _ := find_field(ty.union_fields, n)
            coerce_lit_value(f.expr, ty.union_fields[idx].type)
        }
    case: panic("propagate_aggregate: not a struct or union")
    }
}

// already typechecked value, just settle its final type
coerce_lit_value :: proc(e: ExprId, want: TypeId) {
    r, ok, msg := assignable(want, expr_ty(e))
    if !ok do gala_panicf("compiler bug: literal value no longer fits: {}", msg)
    propagate_type(r, e)
}
propagate_type :: proc(t: TypeId, id: ExprId) {
    #partial switch i in get_expr(id) {
    case UnNot, BoolLitFalse, BoolLitTrue, String, Reference, Deref, TakeSlice,
         Index, FixedSizeArray, Cast, Transmute,
         Symbol, FnCall, FnLit, FieldAccess, TypeInfoOf, TypeIdOf: // "type_id" returns i64 or "int_type"
        return
    case AggregateLit: {
        if is_untyped(t) {
            panic(fmt.aprintf("Can't propagate type {} to untyped aggregate lit.", tts(t)))
        }
        if is_untyped(expr_ty(id)) {
            // assignable() already said it fits, now the values need their types
            propagate_aggregate(i, t)
        }
    }
    case UnNegative: {
        propagate_type(t, i.expr)
    }
    case Binop: {
        own := expr_ty(id)
        if is_bool(own) do return
            if !is_untyped(own) do return // skip if it's been assigned a type
        propagate_type(t, i.left)
        propagate_type(t, i.right)
    }
    case Len, SizeOf, Number:
        get_ctx().tc_types[id] = t

    case: debugln(i); panic("impl")
    }
    get_ctx().tc_types[id] = t
}

bool_ty :: proc() -> TypeId { return ty_from_name("bool") }
// return ty and conversion ty
binop_res_ty :: proc(kind: BinopKind, r: TypeId) -> (TypeId, TypeId, bool, string) {
    switch kind {
    case .Addition, .Subtraction:
        return r, r, true, ""
    case .Multiply, .Divide:
        return r, r, true, ""
    case .Modulo, .BitAnd, .BitOr, .BitXor, .ShiftLeft, .ShiftRight:
        return r, r, true, ""
    case .Equal, .NotEqual:
        if is_numeric_untyped(r) {
            v, ok , msg := get_assignable_default_type(r)
            assert(ok)
            return bool_ty(), v, true, ""
        }
        return bool_ty(), r, true, ""
    case .Less, .LessEqual, .Greater, .GreaterEqual:
        if is_numeric_untyped(r) {
            v, ok , msg := get_assignable_default_type(r)
            assert(ok)
            return bool_ty(), v, true, ""
        }
        return bool_ty(), r, true, ""
    case .LogicalAnd, .LogicalOr:
        return bool_ty(), r, true, ""
    }
    panic("what")
}
binop :: proc(kind: BinopKind, e1, e2: ExprId, loc:=#caller_location) -> (TypeId, bool, string) {
    t1 := expr_ty(e1)
    t2 := expr_ty(e2)
    if t1 == t2 {
        if can_binop(t1, kind) {
            t, to, ok, msg := binop_res_ty(kind, t1)
            propagate_type(to, e1)
            propagate_type(to, e2)
            return t, ok, msg

        }
        else do return 0, false, fmt.aprintf("Can't binop type {} with op {}", tts(t1), kind)
    }
        // pointer vs untyped integer (`p != 0`, `0 == p`): comparisons only.
    // can_binop already rejects arithmetic on pointers (is_numeric is false for them)
    t1_ptr_t2_int := is_pointer(t1) && get_type(t2).kind == .UntypedInteger
    t1_int_t2_ptr := get_type(t1).kind == .UntypedInteger && is_pointer(t2)
    if t1_ptr_t2_int || t1_int_t2_ptr {
        ptr := t1 if t1_ptr_t2_int else t2
        if !can_binop(ptr, kind) {
            return 0, false, fmt.aprintf("Can't binop type {} with op {}", tts(ptr), kind)
        }
        t, to, ok, msg := binop_res_ty(kind, ptr)
        // `to` is the pointer type, so the literal becomes a pointer-typed constant
        propagate_type(to, e1)
        propagate_type(to, e2)
        return t, ok, msg
    }
    if !is_numeric(t1) {
        highlight_lines(get_span(e1))
        gala_panicf("Expression is not numeric (got {})", tts(t1));
    }
    if !is_numeric(t2) {
        highlight_lines(get_span(e2))
        gala_panicf("Expression is not numeric (got {})", tts(t2));
    }
    
    r: TypeId = 0
    if is_untyped(t1) && !is_untyped(t2) {
        if is_integer(t2) && get_type(t1).kind == .UntypedFloat {
            return 0, false, fmt.aprintf("Can't binop Untyped Float (lhs) to {} (rhs) ", tts(r))
        }
        r = t2
    } else if is_untyped(t2) && !is_untyped(t1) {
        if is_integer(t1) && get_type(t2).kind == .UntypedFloat {
            return 0, false, fmt.aprintf("Can't binop Untyped Float (rhs) to {} (lhs) ", tts(r))
        }
        r = t1
    } else if is_untyped(t1) && is_untyped(t2) {
        if get_type(t1).kind == .UntypedFloat || get_type(t2).kind == .UntypedFloat {
            r = intern_type({kind=.UntypedFloat})
        } else {
            r = intern_type({kind=.UntypedInteger})
        }
    } else {
        return 0, false, fmt.aprintf("can't compare {} and {}", tts(t1), tts(t2))
    }
    if !can_binop(r, kind) {
        return 0, false, fmt.aprintf("Can't binop type {} with op {}", tts(r), kind)
    }
    t, to, ok, msg := binop_res_ty(kind, t1)
    if !ok do return 0, false, msg
    propagate_type(to, e1)
    propagate_type(to, e2)
    return t, ok, msg
    // panic("impl")
}

get_bindings_for_generic_fn :: proc(ty_args, args: []TypeId, template: TypeId, id: ExprId) -> (temp_id:TypeId, key:int) {
    s := get_expr(id).(FnCall)
    c := get_ctx()
    if temp, ok := &c.fn_templates[template]; ok {
        instances := temp.instances;
        search: for ins, key in instances {
            assert(len(ins.ty_args) == len(ty_args))
            assert(len(ins.args) == len(args))
            for a, i in ins.ty_args {
                if a != ty_args[i] do continue search; // next one
            }
            for a, i in ins.args {
                if a != args[i] do continue search; // next one
            }
            // if they do match, send that
            return template, key
        }
        // if it doesn't exist, generate
        bindings: map[TypeId]TypeId
        t, ok, msg := bind_generics(ty_args, args, template, s, &bindings);
        if !ok {
            highlight_lines(get_span(id))
            gala_panicf("Generic Binings error: {}.", msg)
        }
        // pass in bindings for sub functions to use, so:
        /* fn t::<$T>(s: string) {
                test_any::<T>(cast(T)1, s)
            } */
        // can know what "T" is
        append(&temp.instances, {ty_args, args, t, bindings, 0, false})
        return template, len(temp.instances) - 1
    } else {
        debugln("template doesn't exist.", template, s)
        panic("template doesn't exist")
    }
    panic("impl")
}


ty_has_generic :: proc(id: TypeId) -> (TypeId, bool) {
    t := get_type(id)
    switch t.kind {
    case .Generic: return id, true
    case .Pointer: return ty_has_generic(t.ptr)
    case .Function: {
        if t.fn.is_generic do return id, true
        // shouldn't have generics otherwise
    }
    case .UInt64, .UInt32, .UInt16, .UInt_8,
         .Int64,.Int32,.Int16,.Int_8,
         .Flt64,.Flt32,.Flt16,.Flt_8,
         .Rune, .Byte, .Bool, .Any, .Void: 
        return 0, false
    case .Slice: return ty_has_generic(t.slice.type)
    case .String: return 0, false
    case .Struct: return id, is_generic(id)
    case .Union:
        for f in t.union_fields {
            if tid, ok := ty_has_generic(f.type); ok do return tid, true
        }
        return 0, false
    case .FixedSizeArray: return ty_has_generic(t.fixed_size_array.type)

    case .Invalid, .UntypedInteger, .UntypedFloat, .UntypedStruct: panic("what")
    }
    return 0, false
}
// copies type, but with new "n" type instead of generic type
ty_fill_from_generics :: proc(g: TypeId, bindings: map[TypeId]TypeId) -> TypeId {
    t := get_type(g)
    switch t.kind {
    case .Generic: // meat and sauce of the thing
        nt, ok := bindings[g]
        assert(ok)
        return nt
    // pointer to new type
    case .Pointer:
        t := get_type(g)^ // clone
        if is_generic(t.ptr) {
            nt := ty_fill_from_generics(t.ptr, bindings)
            t.ptr= nt
            return intern_type(t)
        } else {
            return g
        }
    // 100% claude
    case .Function: {
        t := get_type(g)^ // clone
        new_args := make([]FnParam, len(t.fn.args))
        for a, i in t.fn.args {
            nt := a.ty
            if is_generic(nt) do nt = ty_fill_from_generics(nt, bindings)
                new_args[i] = {a.name, nt}
        }
        t.fn.args = new_args
        if is_generic(t.fn.ret_ty) {
            t.fn.ret_ty = ty_fill_from_generics(t.fn.ret_ty, bindings)
        }
        if t.fn.is_variadic && is_generic(t.fn.variadic_ty) {
            t.fn.variadic_ty = ty_fill_from_generics(t.fn.variadic_ty, bindings)
        }
        t.fn.effective_variadic_ty = slice_from(t.fn.variadic_ty)
        t.fn.is_generic = false
        t.fn.type_args = nil
        t.fn.generic = {}
        return new_fn_type(t)
    }
    case .UInt64, .UInt32, .UInt16, .UInt_8,
         .Int64,.Int32,.Int16,.Int_8,
         .Flt64,.Flt32,.Flt16,.Flt_8,
         .Rune,.Byte,.Bool, .Any, .Void: 
        return g
    case .Slice:
        t := get_type(g)^ // clone
        if is_generic(t.slice.type) {
            nt := ty_fill_from_generics(t.slice.type, bindings)
            t.slice.type= nt
            return intern_type(t)
        } else {
            return g
        }
    case .FixedSizeArray:
        t := get_type(g)^ // clone
        if is_generic(t.fixed_size_array.type) {
            nt := ty_fill_from_generics(t.fixed_size_array.type, bindings)
            t.fixed_size_array.type= nt
            return intern_type(t)
        } else {
            return g
        }
    case .String: return g
    case .Struct:
        if !is_generic(g) do return g
        if get_type(g).structure.is_anonymous {
            t := get_type(g)^ // clone
            old := t.structure.fields
            new_fields := make([]Field, len(old))
            for f, i in old {
                ft := f.type
                if is_generic(ft) do ft = ty_fill_from_generics(ft, bindings)
                new_fields[i] = {f.name, ft}
            }
            t.structure.fields = new_fields
            return intern_type(t)
        }
        tmpl, targs, ok := struct_origin(g)
        if !ok do return g
        new_args := make([]TypeId, len(targs))
        for a, i in targs {
            new_args[i] = ty_fill_from_generics(a, bindings)
        }
        return instantiate_struct(tmpl, new_args, get_ctx().spans.ty_decs[tmpl])
    case .Union:
        t := get_type(g)^ // clone
        new_fields := make([]Field, len(t.union_fields))
        for f, i in t.union_fields {
            ft := f.type
            if is_generic(ft) do ft = ty_fill_from_generics(ft, bindings)
            new_fields[i] = {f.name, ft}
        }
        t.union_fields = new_fields
        return intern_type(t)
    case .Invalid, .UntypedInteger, .UntypedFloat, .UntypedStruct: panic("what")
    }
    panic("impl")
}

// walk down types and match. handles "^$T" and "[]i32" errors

unify :: proc(template_type_id, arg_type_id: TypeId, bindings: ^map[TypeId]TypeId) -> (TypeId, bool, string) {
    p := get_type(template_type_id)
    aty := get_type(arg_type_id)

    #partial switch p.kind {
    case .Generic: {
        b, has := bindings[template_type_id]
        if !has {
            bindings^[template_type_id] = arg_type_id
            return arg_type_id, true, ""
        } else {
            // If previous binding is untyped and arg isn't, set to arg.
            // Generics bind first typed arg, and then use defaults if it's untyped.
            if is_untyped(b) && !is_untyped(arg_type_id) {
                bindings^[template_type_id] = arg_type_id
            } else if is_untyped(b) && is_untyped(arg_type_id) {
                if get_type(b).kind == .UntypedFloat || get_type(arg_type_id).kind == .UntypedFloat {
                    bindings^[template_type_id] = intern_type({kind=.UntypedFloat})
                }
            } else {
                _, ok, msg := assignable(b, arg_type_id)
                if !ok {
                    return 0, false, fmt.aprintf(
                        "Generic type {} was previously bound to {}, but received {}: {}",
                        tts(template_type_id), tts(b), tts(arg_type_id), msg,
                    )
                }
            }
            return bindings^[template_type_id], true, ""
        }
    }

    case .Slice: {
        if aty.kind != .Slice {
            return 0, false, fmt.aprintf(
                "Types mismatch: expected slice, got {}",
                tts(arg_type_id),
            )
        }

        if is_generic(p.slice.type) {
            np := p^
            ok: bool
            msg: string
            np.slice.type, ok, msg = unify(p.slice.type, aty.slice.type, bindings)
            if !ok {
                return 0, false, fmt.aprintf(
                    "Slice element type mismatch: {} vs {}: {}",
                    tts(p.slice.type), tts(aty.slice.type), msg,
                )
            }
            return intern_type(np), true, ""
        }

        return assignable(template_type_id, arg_type_id)
    }

    case .Pointer: {
        if aty.kind != .Pointer {
            return 0, false, fmt.aprintf(
                "Types mismatch: expected pointer to {}, got {}",
                tts(p.ptr), tts(arg_type_id),
            )
        }

        return unify(p.ptr, aty.ptr, bindings)
    }
    case .Union: {
        if !is_generic(template_type_id) {
            _, ok, msg := assignable(template_type_id, arg_type_id)
            if !ok {
                return 0, false, fmt.aprintf(
                    "Union type mismatch: expected {}, got {}: {}",
                    tts(template_type_id), tts(arg_type_id), msg,
                )
            }
            return template_type_id, true, ""
        }
        if aty.kind != .Union {
            return 0, false, fmt.aprintf(
                "Types mismatch: expected union, got {}", tts(arg_type_id),
            )
        }
        // copy slice headers: unify/intern_type can append to ctx.types
        pf := p.union_fields
        af := aty.union_fields
        if len(pf) != len(af) {
            return 0, false, fmt.aprintf(
                "Union type mismatch: expected {} fields, got {}", len(pf), len(af),
            )
        }
        for f, i in pf {
            if f.name != af[i].name {
                return 0, false, fmt.aprintf(
                    "Union field {} mismatch: expected \"{}\", got \"{}\"",
                    i, f.name, af[i].name,
                )
            }
            _, ok, msg := unify(f.type, af[i].type, bindings)
            if !ok {
                return 0, false, fmt.aprintf(
                    "Union field \"{}\": {}", f.name, msg,
                )
            }
        }
        return arg_type_id, true, ""
    }

    // 100% chatgpt
    case .Struct: {
        if !is_generic(template_type_id) {
            _, ok, msg := assignable(template_type_id, arg_type_id)
            if !ok {
                return 0, false, fmt.aprintf(
                    "Struct type mismatch: expected {}, got {}: {}",
                    tts(template_type_id), tts(arg_type_id), msg,
                )
            }
            return template_type_id, true, ""
        }

        // An untyped struct literal `{...}`: infer the generics by unifying each
        // given value's type against the matching template field. Nothing is
        // instantiated here; bind_generics defaults untyped bindings afterwards
        // and ty_fill_from_generics builds the real instance from the final bindings.
        if aty.kind == .UntypedStruct {
            // copy the slice headers now: unify/intern_type can append to ctx.types
            want  := p.structure.fields
            given := aty.structure.fields

            if len(given) != len(want) {
                return 0, false, fmt.aprintf(
                    "Struct literal has {} fields, but {} has {}",
                    len(given), tts(template_type_id), len(want),
                )
            }

            positional := len(given) > 0 && given[0].name == ""
            for g, i in given {
                fi := i
                if !positional {
                    idx, found := find_field(want, g.name)
                    if !found {
                        return 0, false, fmt.aprintf(
                            "Struct {} has no field \"{}\"",
                            tts(template_type_id), g.name,
                        )
                    }
                    fi = idx
                }
                _, ok, msg := unify(want[fi].type, g.type, bindings)
                if !ok {
                    return 0, false, fmt.aprintf(
                        "Struct literal field {} mismatch: expected {}, got {}: {}",
                        i, tts(want[fi].type), tts(g.type), msg,
                    )
                }
            }
            return template_type_id, true, ""
        }

        tp, targs, tok := struct_origin(template_type_id)
        ap, aargs, aok := struct_origin(arg_type_id)

        if !tok || !aok || tp != ap {
            return 0, false, fmt.aprintf(
                "Struct type mismatch: expected {}, got {}",
                tts(template_type_id), tts(arg_type_id),
            )
        }

        for ta, i in targs {
            _, ok, msg := unify(ta, aargs[i], bindings)
            if !ok {
                return 0, false, fmt.aprintf(
                    "Struct generic argument {} mismatch: expected {}, got {}: {}",
                    i, tts(ta), tts(aargs[i]), msg,
                )
            }
        }

        return arg_type_id, true, ""
    }

    // 100% claude
    case .Function: {
        // copy by value: unify/intern_type can append to ctx.types and invalidate p/aty
        pfn := p.fn

        if !is_generic(template_type_id) {
            _, ok, msg := assignable(template_type_id, arg_type_id)
            if !ok {
                return 0, false, fmt.aprintf(
                    "Function type mismatch: expected {}, got {}: {}",
                    tts(template_type_id), tts(arg_type_id), msg,
                )
            }
            return template_type_id, true, ""
        }

        if aty.kind != .Function {
            return 0, false, fmt.aprintf(
                "Types mismatch: expected function, got {}", tts(arg_type_id),
            )
        }
        afn := aty.fn
        if afn.is_generic {
            return 0, false, "Can't pass a generic function as an argument"
        }
        if len(pfn.args) != len(afn.args) {
            return 0, false, fmt.aprintf(
                "Function type mismatch: expected {} params, got {}",
                len(pfn.args), len(afn.args),
            )
        }
        if pfn.is_variadic != afn.is_variadic {
            return 0, false, "Function type mismatch: variadic-ness differs"
        }

        for pa, i in pfn.args {
            _, ok, msg := unify(pa.ty, afn.args[i].ty, bindings)
            if !ok {
                return 0, false, fmt.aprintf(
                    "Function param {} mismatch: expected {}, got {}: {}",
                    i, tts(pa.ty), tts(afn.args[i].ty), msg,
                )
            }
        }
        _, ok, msg := unify(pfn.ret_ty, afn.ret_ty, bindings)
        if !ok {
            return 0, false, fmt.aprintf(
                "Function return type mismatch: expected {}, got {}: {}",
                tts(pfn.ret_ty), tts(afn.ret_ty), msg,
            )
        }
        // the argument is already concrete, so it is the result
        return arg_type_id, true, ""
    }

    case: {
        _, ok, msg := assignable(template_type_id, arg_type_id)
        if !ok {
            return 0, false, fmt.aprintf(
                "Types mismatch: expected {} ({}), got {} ({}): {}",
                tts(template_type_id), p.kind,
                tts(arg_type_id), aty.kind,
                msg,
            )
        }
        return template_type_id, true, ""
    }
    }

    panic("unreachable")
}

// get type from args
// if type is in turbofish, use those
bind_generics :: proc(ty_args, args: []TypeId, template: TypeId, s: FnCall, bindings: ^map[TypeId]TypeId) -> (TypeId, bool, string) {
    fn := get_type(template).fn;
    // populate with type args first
    for a, i in ty_args {
        bindings[fn.type_args[i].ty] = a;
    }
    new_args := make([]FnParam, len(fn.args));
    // "unify" all args
    for a, i in fn.args {
        t, ok, msg := unify(a.ty, args[i], bindings)
        if !ok {
            return 0, false, fmt.aprintf("{} in arg {}", msg, i)
        }
    }
    if fn.is_variadic {
        // basically, do same as above, but keep "template_type_id" the same (fn.variadic_ty) and change "arg_type_id"
        for a, index in args[len(fn.args):] {
            i := index + len(fn.args)
            t, ok, msg := unify(fn.variadic_ty, a, bindings)
            if !ok {
                return 0, false, fmt.aprintf("{} in arg {}", msg, i)
            }
        }
    }
    // untyped valeues
    for i, k in bindings {
        t, ok, msg := get_assignable_default_type(k);
        if ! ok { return 0, false, msg }
        bindings[i] = t
    }
    // once we have all bindings, populate new args
    for a, i in fn.args {
        t : TypeId = a.ty
        if is_generic(t) {
            t = ty_fill_from_generics(t, bindings^)
        }
        new_args[i] = {a.name, t};
    }

    rt:= fn.ret_ty;
    if is_generic(rt) {
        rt = ty_fill_from_generics(rt, bindings^)
    }
    vt:= fn.variadic_ty;
    if fn.is_variadic && is_generic(vt) {
        vt = ty_fill_from_generics(vt, bindings^)
    }
    
    new_ty := get_type(template)^
    new_ty.fn.is_generic = false
    new_ty.fn.args = new_args;
    new_ty.fn.ret_ty = rt
    new_ty.fn.variadic_ty = vt
    // effective variadic type
    new_ty.fn.effective_variadic_ty = slice_from(vt)
    new_ty.fn.type_args = nil
    new_ty.fn.generic = {}
    return new_fn_type(new_ty), true, ""
}

// 100% claude
tc_arg_against :: proc(f: FnCall, call_idx: int, want: TypeId) {
    a := f.args[call_idx]
    t, ok, msg := assignable(want, expr_ty(a.expr))
    if !ok {
        highlight_lines(expr_span(a.expr))
        gala_panicf("Failed to tc fn call (arg {}): {}", call_idx, msg)
    }
    propagate_type(t, a.expr)
}

// ~50% claude
tc_call_args :: proc(f: FnCall, fn: FunctionType, m: CallMatch) {
    for ci, i in m.fixed {
        tc_arg_against(f, ci, fn.args[i].ty)
    }
    for ci, i in m.defaults {
        if ci == -1 do continue
        tc_arg_against(f, ci, fn.default_args[i].ty)
    }
    for ci in m.variadic {
        tc_arg_against(f, ci, fn.variadic_ty)
    }
}

// ~25-50% claude - default args
tc_generic_fn_call :: proc(id: ExprId) {
    f := get(id).(FnCall)
    tc_expr(f.target)
    ct := get_type(expr_ty(f.target))
    assert(ct.kind == .Function)
    assert(ct.fn.is_generic)
    assert(len(f.type_args) == len(ct.fn.type_args))

    m := match_call_args(id, f, ct.fn) // also does the arg count check

    ty_args_ty := make([]TypeId, len(f.type_args))
    for a, i in f.type_arg_ids do ty_args_ty[i] = a

    for a in f.args {
        tc_expr(a.expr)
    }

    // bind_generics wants fixed args then variadic args, and no defaults
    args_ty := make([dynamic]TypeId, 0, len(m.fixed) + len(m.variadic))
    for ci in m.fixed    do append(&args_ty, expr_ty(f.args[ci].expr))
    for ci in m.variadic do append(&args_ty, expr_ty(f.args[ci].expr))

    template_ty, key := get_bindings_for_generic_fn(ty_args_ty, args_ty[:], expr_ty(f.target), id)
    new_ty := get_ctx().fn_templates[template_ty].instances[key].ty_id
    get_ctx().call_instances[id] = { expr_ty(f.target), new_ty, key }

    fn := get_type(new_ty).fn
    tc_call_args(f, fn, m)
    get_ctx().tc_types[id] = fn.ret_ty
}

// ~25-50% claude - default args
tc_fn_call :: proc(id: ExprId) {
    f := get(id).(FnCall)
    tc_expr(f.target)
    ct := get_type(expr_ty(f.target))
    assert(ct.kind == .Function)
    fn := ct.fn
    if fn.is_generic {
        tc_generic_fn_call(id)
        return
    }
    assert(len(f.type_args) == 0)
    assert(len(f.type_arg_ids) == 0)

    m := match_call_args(id, f, fn)
    for a in f.args {
        tc_expr(a.expr)
    }
    tc_call_args(f, fn, m)
    get_ctx().tc_types[id] = fn.ret_ty
}
// any behaves like struct { data: rawptr, type: i64 }.
// Field order matches the IR layout { ptr, i64 }.
any_fields :: proc() -> []Field {
    f := make([]Field, 2)
    f[0] = {"data", ty_from_name("rawptr")}
    f[1] = {"id", ty_from_name("i64")}
    return f
}

//. 100% claude from previous version
// the fields you can access with `.name`, for struct-like types
struct_fields :: proc(gt: TypeId) -> ([]Field, bool) {
    t := get_type(gt)
    #partial switch t.kind {
    case .Struct: return t.structure.fields, true
    case .Union: return t.union_fields, true
    case .Any:    return any_fields(), true
    }
    return nil, false
}
//. 100% claude from previous version
find_field :: proc(fields: []Field, name: string) -> (int, bool) {
    for f, i in fields {
        if f.name == name do return i, true
    }
    return 0, false
}

//. 100% claude from previous version
// types one literal value and checks it against what the field wants
tc_lit_value :: proc(e: ExprId, want: TypeId, span: Span, what: string) {
    tc_expr(e)
    t, ok, msg := assignable(want, expr_ty(e))
    if !ok {
        highlight_lines(span)
        gala_panicf("{} error: {}.", what, msg)
    }
    propagate_type(t, e)
}

//. 100% claude from previous version
tc_struct_lit :: proc(lit: AggregateLit, t: TypeId, span: Span) {
    fields := get_type(t).structure.fields

    if len(lit.fields) != 0 {
        // named: every name must exist, and (no duplicates, the resolver checks) all must be given
        for n, f in lit.fields {
            idx, found := find_field(fields, n)
            if !found {
                highlight_lines(f.span)
                gala_panicf("Struct \"{}\" has no field \"{}\".", tts(t), n)
            }
            tc_lit_value(f.expr, fields[idx].type, f.span, "Struct")
        }
        if len(lit.fields) != len(fields) {
            for sf in fields {
                if sf.name not_in lit.fields {
                    highlight_lines(span)
                    gala_panicf("Missing field \"{}\" in literal of struct \"{}\".", sf.name, tts(t))
                }
            }
        }
        return
    }

    // positional (also covers `Foo{}`)
    if len(lit.positional) != len(fields) {
        highlight_lines(span)
        gala_panicf("Struct \"{}\" has {} fields, got {}.", tts(t), len(fields), len(lit.positional))
    }
    for e, index in lit.positional {
        tc_lit_value(e.expr, fields[index].type, e.span, "Struct")
    }
}

//. 100% claude from previous version
// a union literal sets exactly one field, by name
tc_union_lit :: proc(lit: AggregateLit, t: TypeId, span: Span) {
    fields := get_type(t).union_fields
    if len(lit.fields) != 1 || len(lit.positional) != 0 {
        highlight_lines(span)
        gala_panicf("A union literal must set exactly one field by name.")
    }
    for n, f in lit.fields {
        idx, found := find_field(fields, n)
        if !found {
            highlight_lines(f.span)
            gala_panicf("Union \"{}\" has no field \"{}\".", tts(t), n)
        }
        tc_lit_value(f.expr, fields[idx].type, f.span, "Union")
    }
}

//. 100% claude from previous version
tc_aggregate_lit :: proc(id: ExprId) {
    lit := get_expr(id).(AggregateLit)
    span := expr_span(id)
    ctx := get_ctx()

    t, has_ty := ctx.expr_types[id]
    if !has_ty {
        for _, f in lit.fields  do tc_expr(f.expr)
        for f in lit.positional do tc_expr(f.expr)
        ctx.tc_types[id] = untyped_aggregate_type(lit)
        return
    }

    if is_generic(t) {
        highlight_lines(span)
        gala_panicf("Generic struct \"{}\" needs type arguments in a literal.", tts(t))
    }

    #partial switch get_type(t).kind {
    case .Struct: tc_struct_lit(lit, t, span)
    case .Union:  tc_union_lit(lit, t, span)
    case:
        highlight_lines(span)
        gala_panicf("Aggregate literal type can't be {}.", tts(t))
    }
    ctx.tc_types[id] = t
}

//. 100% claude from previous version
// the template (and its args) a struct type comes from.
// the template itself counts as "template with its own params".
struct_origin :: proc(id: TypeId) -> (template: TypeId, args: []TypeId, ok: bool) {
    c := get_ctx()
    if tmpl, has := c.struct_templates[id]; has {
        return id, tmpl.params, true
    }
    t := get_type(id)
    if t.kind == .Struct && t.structure.template != 0 {
        return t.structure.template, t.structure.args, true
    }
    return 0, nil, false
}

// 100% claude from previous stuff
// Box(i64) -> the TypeId of that instance, creating it the first time.
// Only StructTemplate.instances remembers anything.
instantiate_struct :: proc(template: TypeId, args: []TypeId, span: Span) -> TypeId {
    c := get_ctx()
    tmpl, has := &c.struct_templates[template]
    if !has {
        highlight_lines(span)
        gala_panicf("\"{}\" is not a generic struct.", tts(template))
    }
    if len(args) != len(tmpl.params) {
        highlight_lines(span)
        gala_panicf("Struct \"{}\" takes {} type argument(s), got {}.",
            get_type(template).name, len(tmpl.params), len(args))
    }
    for a in args {
        if is_untyped(a) || is_void(a) {
            highlight_lines(span)
            gala_panicf("Can't use {} as a type argument.", tts(a))
        }
    }

    // Box(T) written inside Box's own definition is just the template
    if slice.equal(args, tmpl.params) do return template

    // args are interned/nominal TypeIds, so id equality is enough
    for ins in tmpl.instances {
        if slice.equal(ins.args, args) do return ins.ty_id
    }

    bindings: map[TypeId]TypeId
    for p, i in tmpl.params {
        _, ok, msg := unify(p, args[i], &bindings)
        if !ok {
            highlight_lines(span)
            gala_panicf("Struct type argument {}: {}", i, msg)
        }
    }

    // create the type and register it BEFORE filling the fields, so a
    // self reference (`next: ^Node($T)`) finds this instance instead of
    // recursing forever
    base := get_type(template)^ // clone
    tmpl_fields := base.structure.fields

    inst := base
    // inst.name = struct_instance_name(base.name, args)
    inst.name = base.name
    inst.structure.is_generic = false
    inst.structure.generic_parameters = nil
    inst.structure.template = template
    inst.structure.args = slice.clone(args)
    inst.structure.fields = nil

    append(&c.types, inst) // invalidates old get_type pointers
    id := TypeId(len(c.types) - 1)
    append(&tmpl.instances, StructInstance{inst.structure.args, id})
    c.type_modules[id] = tmpl.mid
    if sp, ok := c.spans.ty_decs[template]; ok do c.spans.ty_decs[id] = sp

    fields := make([]Field, len(tmpl_fields))
    for f, i in tmpl_fields {
        t := f.type
        if is_generic(t) do t = ty_fill_from_generics(t, bindings)
        fields[i] = {f.name, t}
    }
    get_type(id).structure.fields = fields
    return id
}
can_use_object :: proc(id: ObjId) -> (bool, string) {
    o := get_obj(id)
    if o.kind == .Constant do return true, ""
    if o.type == nil {
        return false, fmt.aprintf("Object {} doesn't have a type.", o.name)
    }
    if !is_concrete_type(o.type.(TypeId)) {
        return false, fmt.aprintf("Object {} does not have a concrete type.", o.name)
    }
    return true, ""
}
tc_expr :: proc(id: ExprId) {
    ctx := get_ctx()
    span := get_span(id)
    #partial switch i in get_expr(id) {
    case Binop: {
        tc_expr(i.left);
        tc_expr(i.right);
        ty, ok, msg := binop(i.kind, i.left, i.right)
        if !ok {
            highlight_lines(span)
            gala_panic("Failed binop:", msg)
        }
        // if it's a bool, it can be something like "1 == len(s)", where both are untyped
        // propagating down a "bool" wouldn't work, so we check if they're untyped numbers,
        // and if they are, we assign them an appropriate default type
        if is_bool(ty) {
            l := expr_ty(i.left)
            r := expr_ty(i.right)
            if is_untyped(l) || is_untyped(r) {
                t, ok, msg := get_assignable_default_type(expr_ty(i.left))
                assert(ok)
                propagate_type(t, i.left)
                propagate_type(t, i.right)
            }
        }
        // if it's not bool, then it's likely a normal binop ("6 + 7"), so propagate down
        // if one of the sides is typed
        opty := is_untyped(expr_ty(i.left)) ? expr_ty(i.right) : expr_ty(i.left)
        if !is_untyped(opty) {
            propagate_type(opty, i.left)
            propagate_type(opty, i.right)
        }
        ctx.tc_types[id] = ty
    }
    case Number: {
        // untyped float if it has a '.', otherwise untyped int (decimal, 0x.., 0b..)
        if is_float_literal(i.text) {
            ctx.tc_types[id] = intern_type(Type{kind=.UntypedFloat})
        } else {
            ctx.tc_types[id] = intern_type(Type{kind=.UntypedInteger})
        }
    }
    case Len: {
        tc_expr(i.target)
        t := expr_ty(i.target)
        k := get_type(t).kind
        if k != .Slice && k != .String && k != .FixedSizeArray {
            highlight_lines(get_span_expr(id))
            gala_panicf("Can't take len of an expression of type %s.", tts(t))
        }
        get_ctx().tc_types[id] = intern_type({kind=.UntypedInteger})
    }
    case TypeIdOf: {
        _, syntactic := i.t.(TypeSpecifier)
        if !syntactic && !i.names_type {
            ex := i.expr.(ExprId)
            tc_expr(ex)
            require_typed_value(ex, "take the type_id")   // was "take the size_of"
            // type_id(1) is i64, like `x := 1`. Lowering reads the type from
            // expr_types, same as for the type form.
            t, ok, msg := get_assignable_default_type(expr_ty(ex))
            assert(ok);
            ctx.expr_types[id] = t                         // was ctx.tc_types[id]
        }
        // type_id(int) / type_id(Foo): the resolver already stored the type
        t, has := ctx.expr_types[id]
        if !has {
            highlight_lines(span)
            gala_panic("type_id: no type was resolved")
        }
        if !is_concrete_type(t) {
            highlight_lines(span)
            gala_panicf("Can't take the type_id of non-concrete type {}.", tts(t))
        }
        ctx.tc_types[id] = intern_type(Type{kind=.UntypedInteger})
    }
    case SizeOf: {
        _, syntactic := i.t.(TypeSpecifier)
        if !syntactic && !i.names_type {
            ex := i.expr.(ExprId)
            tc_expr(ex)
            require_typed_value(ex, "take the size_of")
            // size_of(1) is i64, like `x := 1`. Codegen reads the type from
            // here, same as for the type form.
            t, ok, msg := get_assignable_default_type(expr_ty(ex))
            assert(ok);
            ctx.expr_types[id] = t
        }
        get_ctx().tc_types[id] = intern_type(Type{kind=.UntypedInteger})
    }
    case TypeInfoOf: {
        tc_expr(i.expr)

        t := expr_ty(i.expr)
        r, ok, err := assignable(ty_from_name("i64"), t)
        if !ok {
            highlight_lines(get_span_expr(id))
            gala_panicf("`type_info` takes an i64 type id, got %s (%s).", tts(t), err)
        }
        propagate_type(r, i.expr)
        get_ctx().tc_types[id] = intern_type({kind=.Pointer, ptr=ty_from_name("TypeInfo")})
    }
    case FieldAccess: {
        tc_expr(i.target)
        tid := expr_ty(i.target)
        fields, ok := struct_fields(tid)
        if !ok {
            highlight_lines(span)
            gala_panicf("Can't access field \"{}\" of type {}.", i.field, tts(tid))
        }
        found := false
        for f in fields {
            if f.name == i.field {
                ctx.tc_types[id] = f.type
                found = true
                break
            }
        }
        if !found {
            highlight_lines(span)
            gala_panicf("Can't find field \"{}\" in type {}.", i.field, tts(tid))
        }
    }
    case AggregateLit: tc_aggregate_lit(id)
    case BoolLitFalse, BoolLitTrue: { ctx.tc_types[id] = ty_from_name("bool") }
    case String: {
            ctx.tc_types[id] = ty_from_name("string")
    }
    case Transmute: {
        to, ok := ctx.expr_types[id]; assert(ok)
        tc_expr(i.target)
        t, aok, msg := get_assignable_default_type(expr_ty(i.target))
        if !aok {
            highlight_lines(get_span(i.target))
            gala_panicf("Default type error (since target is untyped): {}.", msg)
        }
        propagate_type(t, i.target)
        if! can_transmute_to(expr_ty(i.target), to) {
            highlight_lines(span)
            gala_panicf("Can't transmute expression of type {} to {}.", tts(expr_ty(i.target)), tts(to))
        }
        ctx.tc_types[id] = to
    }
    case Cast: {
        to, ok := ctx.expr_types[id]; assert(ok)
        tc_expr(i.target)
        t, aok, msg := get_assignable_default_type(expr_ty(i.target))
        if !aok {
            highlight_lines(get_span(i.target))
            gala_panicf("Default type error (since target is untyped): {}.", msg)
        }
        propagate_type(t, i.target)
        assert(can_cast_to(expr_ty(i.target), to))
        ctx.tc_types[id] = to
    }
    case Symbol: {
        oid := expr_obj(id)
        o := get_obj(oid);
        if o.type == nil { // ??? shouldn't happen at all
            highlight_lines(span)
            debugln(i)
            panic("yeah.")
        }
        if ok, msg := can_use_object(oid); !ok && false {
            highlight_lines(span)
            gala_panicf("Can't use object: {}.", msg)
        }
        ctx.tc_types[id] = o.type.(TypeId)
    }
    case FnCall: tc_fn_call(id)
    case Reference: {
        tc_expr(i.expr);
        t := expr_ty(i.expr)
        if !can_reference(t) {
            highlight_lines(span);
            gala_panicf("can't take reference of expr off type type {}.", tts(t))
        }
        if ok, r := expr_has_address(i.expr); !ok {
            highlight_lines(span);
            gala_panicf("can't take reference of expr: {}.", r)
        }
        ctx.tc_types[id] = intern_type({kind=.Pointer, ptr=t})
    }
    case Deref: {
        tc_expr(i.expr);
        t := expr_ty(i.expr)
        if !can_dereference(t) {
            highlight_lines(span);
            gala_panicf("can't dereference of expr off type type {}.", tts(t))
        }
        assert(get_type(t).kind == .Pointer)
        ctx.tc_types[id] = get_type(t).ptr
    }
    case Index: {
        tc_expr(i.target)
        tc_expr(i.index)
        target_ty := expr_ty(i.target)
        index_ty := expr_ty(i.index)
        ok: bool
        msg: string
        index_ty, ok, msg = get_assignable_default_type(index_ty)
        if !ok {
            highlight_lines(get_span(i.index))
            gala_panicf("Index error:", msg)
        }
        propagate_type(index_ty, i.index)
        if !can_be_indexed(target_ty) {
            highlight_lines(get_span(i.target));
            gala_panicf("expression of type {} can't be indexed.", tts(target_ty))
        }
        if !is_valid_index_type(index_ty) {
            highlight_lines(get_span(i.index));
            gala_panicf("expression of type {} can't be used as an index.", tts(index_ty))
        }
        t := get_indexable_type(target_ty)
        ctx.tc_types[id] = t
    }
    // copied from old galayalang
    case FixedSizeArray: {
        // every initialiser element is checked against the element type
        elem_ty := get_ctx().expr_types[id]

        for el in i.initialiser {
            tc_expr(el)
            r, ok, err := assignable(elem_ty, expr_ty(el))
            if !ok {
                highlight_lines(get_span(el))
                gala_panicf("Array element type mismatch: %s (expected %s, got %s).",
                    err, tts(elem_ty), tts(expr_ty(el)))
            }
            propagate_type(r, el)
        }

        t := Type{}
        t.kind = .FixedSizeArray
        t.fixed_size_array.type = elem_ty

        t.fixed_size_array.size = i.size
        get_ctx().tc_types[id] = intern_type(t)

    }
    // copied from old galayalang
    case TakeSlice: {
        tc_expr(i.target)
        target_ty := expr_ty(i.target)

        if !is_array_type(target_ty) {
            highlight_lines(get_span(i.target))
            gala_panicf("Can't slice a value of type %s.", tts(target_ty))
        }

        if !i.empty_start do tc_index_expr(i.start)
        if !i.empty_end   do tc_index_expr(i.end)

        if get_type(target_ty).kind == .String {
            get_ctx().tc_types[id] = target_ty;

            return
        }
        ty := get_indexable_type(target_ty)

        get_ctx().tc_types[id] = intern_type(Type{kind=.Slice, slice={type=ty}})
    }
    case UnNegative: {
        tc_expr(i.expr)
        t := expr_ty(i.expr)
        if !is_numeric(t) {
            highlight_lines(get_span_expr(id))
            gala_panicf("Can't negate an expression of type %s.", tts(t))
        }
        get_ctx().tc_types[id] = t
    }
    case UnNot: {
        tc_expr(i.expr)
        t := expr_ty(i.expr)
        if get_type(t).kind != .Bool {
            highlight_lines(get_span_expr(id))
            gala_panicf("`!` needs a bool, got %s.", tts(t))
        }
        get_ctx().tc_types[id] = ty_from_name("bool")
    }
    case FnLit: {
        tyid, has := ctx.expr_types[id]; assert(has)
        ret_ty := get_type(tyid).fn.ret_ty

        // default args (re-fetch the type each time, tc_expr can append to ctx.types)
        for j in 0..<len(get_type(tyid).fn.default_args) {
            a := get_type(tyid).fn.default_args[j]
            tc_expr(a.expr)
            t: TypeId
            ok: bool
            msg: string
            if a.inferred {
                t, ok, msg = get_assignable_default_type(expr_ty(a.expr))
            } else {
                t, ok, msg = assignable(a.ty, expr_ty(a.expr))
            }
            if !ok {
                highlight_lines(get_span(a.obj))
                gala_panicf("Default argument error: \"{}\": {}.", a.name, msg)
            }
            propagate_type(t, a.expr)
            if a.inferred {
                get_obj(a.obj).type = t
                get_type(tyid).fn.default_args[j].ty = t
            }
        }

        // body, with a fresh context so `return`/`break` don't leak in or out
        ptcxt := tctx
        tctx = TcContext{in_function = true}
        if !is_void(ret_ty) do tctx.fn_ret_ty = ret_ty
        tc_stmts(i.block.stmts)
        tctx = ptcxt

        if !is_void(ret_ty) && !tc_stmts_return(i.block.stmts) {
            highlight_lines(span)
            gala_panicf("function literal returns {} but not every path returns a value.", tts(ret_ty))
        }
        ctx.tc_types[id] = tyid
    }
    case: debugln(i); panic("impl")
    }
}
// copied from old galayalang
// Types an index or slice bound: untyped integers become i64, everything
// else must be a valid index type.
tc_index_expr :: proc(idx: ExprId) {
    tc_expr(idx)
    t := expr_ty(idx)

    if is_untyped(t) {
        if get_type(t).kind != .UntypedInteger {
            highlight_lines(get_span(idx))
            gala_panic("Array index must be an integer.")
        }
        t = ty_from_name("i64")
        propagate_type(t, idx)
    }
    if !is_valid_index_type(t) {

        highlight_lines(get_span(idx))
        gala_panicf("Can't use type %s to index an array.", tts(t))
    }
}
// copied from old galayalang
// Errors on values there is no type to take from (zero initialiser, void).
// `action` completes "Can't <action> a zero initialiser".
require_typed_value :: proc(id: ExprId, action: string) {
    #partial switch get_type(expr_ty(id)).kind {

    case .UntypedStruct: {
        highlight_lines(get_span(id))
        gala_panicf("Can't %s an untyped struct: it has no type.", action)
    }
    case .Void: {
        highlight_lines(get_span(id))
        gala_panicf("Can't %s an expression with no value.", action)

    }
    }
}
expr_has_address :: proc(id: ExprId) -> (bool, string) {
    #partial switch e in get_expr(id) {
    case Symbol: {
        o := get_obj(expr_obj(id))
        if o.read_only == .No do return true, ""
        return false, "object is read only"
    }
    // tecnically yeah, fixes a whole bunch of stuff
    case Index: return true, "" // return expr_has_address(e.target)
    case FieldAccess: return expr_has_address(e.target)
    case Deref: return true, "" // yeah, it comes from an address
    case: return false, fmt.aprintf("can't reference a {}", e)
    }
}
get_assignable_default_type :: proc(t: TypeId) -> (TypeId, bool, string) {
    if get_type(t).kind == .Void do return 0, false, "can't assign value of type void"
    if !is_untyped(t) do return t,  true, ""
    if is_untyped(t) {
        #partial switch get_type(t).kind {
        case .UntypedInteger: return ty_from_name("i64"), true, ""
        case .UntypedFloat: return ty_from_name("f64"), true, ""
        case: return 0, false, "can't determine valid type from expression (only untyped numbers are inferable)"
        }
    }
    return 0, false, "impl"
}
get_indexable_type :: proc(id: TypeId) -> TypeId {
    t := get_type(id)
    #partial switch t.kind {
    case .Slice: return t.slice.type
    case .FixedSizeArray: return t.fixed_size_array.type
    case .String: return ty_from_name("byte")
    case: panic("impl")
    }
}
loop_parent: map[StmtId]StmtId

tc_loop_label :: proc(loop: StmtId) -> string {
    #partial switch l in get_stmt(loop) {
    // case WhileLoop: return l.label
    // case ForLoop:   return l.label
    }
    return ""
}

// typechecks a loop body with `id` as the innermost loop
tc_loop_body :: proc(id: StmtId, stmts: []StmtId) {
    prev := tctx.in_loop
    if p, ok := prev.(StmtId); ok {
        loop_parent[id] = p
        // labels share the no-shadowing rule
        if label := tc_loop_label(id); label != "" {
            cur, has := p, true
            for has {
                if tc_loop_label(cur) == label {
                    highlight_lines(get_span(id))
                    gala_panicf("Loop label \"{}\" is already used by an enclosing loop.", label)
                }
                cur, has = loop_parent[cur]
            }
        }
    }
    tctx.in_loop = id
    tc_stmts(stmts)
    tctx.in_loop = prev
}

// the loop a `break`/`continue` refers to
tc_jump_target :: proc(span: Span, label: string, what: string) -> StmtId {
    cur, ok := tctx.in_loop.(StmtId)
    if !ok {
        highlight_lines(span)
        gala_panicf("`{}` outside of a loop.", what)
    }
    if label == "" do return cur
    for ok {
        if tc_loop_label(cur) == label do return cur
        cur, ok = loop_parent[cur]
    }
    highlight_lines(span)
    gala_panicf("No enclosing loop is labeled \"{}\".", label)
}

// does any `break` target this loop? (needs the body to be typechecked already)
loop_has_break :: proc(loop: StmtId) -> bool {
    for sid, target in get_ctx().jump_targets {
        if target != loop do continue
        if _, is_break := get_stmt(sid).(BreakStmt); is_break do return true
    }
    return false
}
// ----------------------------------------------------------------------------

tc_stmt :: proc(id: StmtId) {
    span := get_span(id)
    #partial switch i in get_stmt(id) {
    case VarDec: {
        o := get(stmt_obj(id))
        tc_expr(i.value)
        t := expr_ty(i.value)
        ok: bool
        msg: string
        if vt, tok := o.type.(TypeId); tok {
            t, ok, msg = assignable(vt, t);
        } else {
            t, ok, msg = get_assignable_default_type(t)
            t, ok, msg = assignable(t, t) // why not
            get(stmt_obj(id)).type = t // update
        }
        if !ok {
            highlight_lines(span)
            gala_panic("Failed vardec:", msg);
        }
        propagate_type(t, i.value)
    }
    case Assignment: {
        tc_expr(i.target)
        tc_expr(i.value)
        assert(expr_has_address(i.target))
        assert(!is_untyped(expr_ty(i.target)))
        t, ok, msg := assignable(expr_ty(i.target), expr_ty(i.value))
        if !ok {
            highlight_lines(span)
            gala_panicf("Assign error: {}.", msg)
        }
        propagate_type(t, i.value)
    }
    case IfElse: {
        tc_expr(i.base_cond)
        if !is_bool(expr_ty(i.base_cond)) {
            highlight_lines(expr_span(i.base_cond))
            gala_panicf("Condition must have boolean types.");
        }
        for s in i.base_block.stmts do tc_stmt(s)

        for a in i.alt {
            tc_expr(a.cond);
            if !is_bool(expr_ty(a.cond)) {
                highlight_lines(expr_span(i.base_cond))
                gala_panicf("Condition must have boolean types.");
            }
            for s in a.block.stmts do tc_stmt(s);
        }

        if i.has_else_block {
            for s in i.else_block.stmts do tc_stmt(s);
        }
    }
    case ForLoop: {
        tc_expr(i.expr);
        t := expr_ty(i.expr)
        if !((get(t).kind == .Slice || get(t).kind == .FixedSizeArray) || can_be_indexed(t)) {
            highlight_lines(get_span(i.expr))
            gala_panicf("Can't iterate over type {}.", tts(t))
        }
        ty := get_type(t)
        names := i.names;
        #partial switch ty.kind {
        case .Slice, .FixedSizeArray: {
            // can't really be less than one
            if len(names) > 2 {
                highlight_lines(span)
                gala_panicf("Can't have more than 2 names when iterating an array or slice.")
            }
            if len(names) >= 1 { // only name
                // first item will be the type
                oid := names[0].oid
                object := get_obj(oid) // ptr to obj
                otyid := get_indexable_type(t);
                assert(object.type == nil)
                object.type = otyid
            }
            if len(names) >= 2 { // index
                oid := names[1].oid
                object := get_obj(oid) // ptr to obj
                otyid := ty_from_name("i64")
                assert(object.type == nil)
                object.type = otyid
            }
        }
        case: panic("impl")
        }
        tc_loop_body(id, i.block.stmts) // NEW (was: save tctx, set in_loop, tc_stmts, restore)
    }
    case WhileLoop: {
        tc_expr(i.cond)
        if !is_bool(expr_ty(i.cond)) {
            highlight_lines(expr_span(i.cond))
            gala_panicf("Condition must have boolean types.");
        }
        tc_loop_body(id, i.block.stmts) // NEW (was: for s in ... do tc_stmt(s))
    }
    // NEW
    case BreakStmt: {
        get_ctx().jump_targets[id] = tc_jump_target(span, i.label, "break")
    }
    // NEW
    case ContinueStmt: {
        get_ctx().jump_targets[id] = tc_jump_target(span, i.label, "continue")
    }
    case Return: {
        if !tctx.in_function {
            highlight_lines(span)
            gala_panic("Can only return in a function.");
        }
        if e, ok := i.expr.(ExprId); ok {
            tc_expr(e);
            rt: TypeId;
            if rt, ok = tctx.fn_ret_ty.(TypeId); !ok {
                highlight_lines(span)
                gala_panic("Function does not expect a return value.");
            }
            if is_void(rt) {
                panic("what")
            }
            t, ok, msg := assignable(rt, expr_ty(e))
            if !ok {
                highlight_lines(get_span(e));
                gala_panicf("Can't assign type {} to function return type {}.", tts(expr_ty(e)), tts(rt))
            }
            propagate_type(t, e)
        }

    }
    case ExprId: tc_expr(i)
    case: debugln(i); panic("impl")
    }
}
tc_stmts :: proc(stmts: []StmtId) {
    ctx := get_ctx()
    for id in stmts {
        tc_stmt(id)
    }
}
// does this statement list return on every path?
tc_stmts_return :: proc(stmts: []StmtId) -> bool {
    for id in stmts {
        #partial switch s in get_stmt(id) {
        case Return:
            return true
        case IfElse:
            // every branch must return, and there must be an else
            all := s.has_else_block &&
                   tc_stmts_return(s.base_block.stmts) &&
                   tc_stmts_return(s.else_block.stmts)
            for a in s.alt do all &&= tc_stmts_return(a.block.stmts)
            if all do return true
        case WhileLoop:
            // `while true` only exits through a return, unless a `break` targets it
            // NEW: breaks are tracked now, so `while true { break }` doesn't count
            if _, is_true := get_expr(s.cond).(BoolLitTrue); is_true && !loop_has_break(id) do return true
        }
    }
    return false
}

// call at the end of a fn declaration: a non-void fn must return on every path
tc_check_fn_returns :: proc(item: ItemId, name: string, ret: TypeId, stmts: []StmtId) {
    if is_void(ret) do return
    if tc_stmts_return(stmts) do return
    highlight_lines(get_span(item))
    gala_panicf("fn \"{}\" returns {} but not every path returns a value.", name, tts(ret))
}


// every number indexes FnCall.args; -1 = not passed
CallMatch :: struct {
    fixed:    []int,
    defaults: []int,
    variadic: []int,
}
// 100% claude
match_call_args :: proc(id: ExprId, f: FnCall, fn: FunctionType) -> CallMatch {
    n_fixed := len(fn.args)
    n_def := len(fn.default_args)

    m := CallMatch{
        fixed    = make([]int, n_fixed),
        defaults = make([]int, n_def),
    }
    for i in 0..<n_def do m.defaults[i] = -1
    variadic := make([dynamic]int)

    seen_named := false
    n_positional := 0
    for a, i in f.args {
        if a.name != "" {
            seen_named = true

            slot := -1
            for d, k in fn.default_args {
                if d.name == a.name {
                    slot = k
                    break
                }
            }
            if slot < 0 {
                highlight_lines(expr_span(a.expr))
                is_required := false
                for p in fn.args {
                    if p.name == a.name do is_required = true
                }
                if is_required {
                    gala_panicf("Parameter \"{}\" has no default value, so it can't be passed by name.", a.name)
                }
                gala_panicf("The function has no parameter named \"{}\".", a.name)
            }
            if m.defaults[slot] != -1 {
                highlight_lines(expr_span(a.expr))
                gala_panicf("Parameter \"{}\" is passed more than once.", a.name)
            }
            m.defaults[slot] = i
            continue
        }

        if seen_named {
            highlight_lines(expr_span(a.expr))
            gala_panicf("Positional arguments can't come after named ones.")
        }

        pos := n_positional
        n_positional += 1
        if pos < n_fixed {
            m.fixed[pos] = i
        } else if fn.is_variadic {
            append(&variadic, i)
        } else if pos - n_fixed < n_def {
            m.defaults[pos - n_fixed] = i
        } else {
            highlight_lines(expr_span(a.expr))
            gala_panicf("Too many arguments: expected at most {}, got {}.", n_fixed + n_def, len(f.args))
        }
    }

    if n_positional < n_fixed {
        highlight_lines(get_span(id))
        gala_panicf("Wrong number of arguments: expected {}{} required, got {}.",
            (n_def > 0 || fn.is_variadic) ? "at least " : "", n_fixed, n_positional)
    }

    m.variadic = variadic[:]
    return m
}

tc_items :: proc(items: []ItemId) {
    ctx := get_ctx()
    // tc default fn args first
    for id in items {
        if i, ok := get_item(id).(FnDec); ok {
            oid := ctx.item_objects[id];
            tyid := get_obj(oid).type.(TypeId); // must be known atp
            ty := get_type(tyid)
            assert(ty.kind == .Function)
            // tc_fn_defaults(ty.fn)
            if ty.fn.is_generic {
                continue
            } else {
                // resolve any eventual infered default parameters
                index := 0
                for a in ty.fn.default_args {
                    // types aren't interned, each function has it's own type
                    tc_expr(a.expr)
                    if a.inferred { // tc and set in type
                        //the type has to be known before anything calls this fn
                        t, ok, msg:=get_assignable_default_type(expr_ty(a.expr))
                        if !ok {
                            highlight_lines(get_span(a.obj))
                            gala_panicf("Can't infer the type of parameter \"{}\": {}.", a.name, msg)
                        }
                        propagate_type(t, a.expr)
                        get_obj(a.obj).type = t // propagate to object
                        ty.fn.default_args[index].ty = t
                    } else {
                        // get assignable type
                        t, ok, msg := assignable(a.ty, expr_ty(a.expr))
                        if !ok {
                            highlight_lines(get_span(a.obj))
                            gala_panicf("Fn default param error: \"{}\": {}.", a.name, msg)
                        }
                        propagate_type(t, a.expr)
                    }
                    index += 1
                }
                get_type(tyid)^ = ty^ // update type cus it's a copy?
                // body in next pass
            }
        }
    }
    for id in items {
        span := get_span(id)
        #partial switch i in get_item(id) {
        case StructDec: {}
        case FnDec: {
            oid := ctx.item_objects[id];
            tyid := get_obj(oid).type.(TypeId); // must be known atp
            ty := get_type(tyid)
            assert(ty.kind == .Function)
            // tc_fn_defaults(ty.fn)
            if ty.fn.is_generic {
                continue
            } else {
                // tc body
                ptcxt := tctx
                tctx.in_function = true
                if is_void(ty.fn.ret_ty) { // means no return type
                    tctx.fn_ret_ty = nil // no return ty
                } else {
                    tctx.fn_ret_ty = ty.fn.ret_ty
                }
                t := tctx.fn_ret_ty

                tc_stmts(i.block.stmts)
                tc_check_fn_returns(id, i.name, ty.fn.ret_ty, i.block.stmts)

                tctx = ptcxt
            }
        }
        case ExternFnDec: {}
        case GlobalVarDec: {
            o := get(item_obj(id))
            tc_expr(i.value)
            if !is_known_at_comptime(i.value) {
                highlight_lines(get_span(i.value))
                gala_panic("Expression can not be known at compile time")
            }
            t := expr_ty(i.value)
            ok: bool
            msg: string
            if vt, tok := o.type.(TypeId); tok {
                t, ok, msg = assignable(vt, t);
            } else {
                t, ok, msg = get_assignable_default_type(t)
                get(item_obj(id)).type = t // update
            }
            if !ok {
                highlight_lines(span)
                gala_panic("Failed vardec:", msg);
            }
            propagate_type(t, i.value)
        }
        case TypeAlias, Import, Forward: {}
        case ConstDec: {} // ignore, will be replaced
        case: debugln(i); panic("impl")
        }
    }
}



is_known_at_comptime :: proc(id: ExprId) -> bool {
    // Every expression must have a concrete, non-generic type.
    if is_generic(expr_ty(id)) {
        debugln("comptime: generic expression type -> false")
        return false
    }

    #partial switch k in get_expr(id) {
    case Symbol:
        return true

    case Number, String, BoolLitTrue, BoolLitFalse:
        return true

    case Binop:
        return is_known_at_comptime(k.left) &&
               is_known_at_comptime(k.right)

    case UnNegative:
        return is_known_at_comptime(k.expr)
    case UnNot:
        return is_known_at_comptime(k.expr)

    case Cast:
        return is_known_at_comptime(k.target)
    case Transmute:
        return is_known_at_comptime(k.target)

    case AggregateLit:
        for _, f in k.fields {
            if !is_known_at_comptime(f.expr) do return false
        }

        for f in k.positional {
            if !is_known_at_comptime(f.expr) do return false
        }

        return true

    case FixedSizeArray:
        // The resolver normally folds size_expr into size.
        if size_expr, has := k.size_expr.(ExprId); has {
            if !is_known_at_comptime(size_expr) do return false
        }

        for e in k.initialiser {
            if !is_known_at_comptime(e) do return false
        }

        return true

    case FieldAccess:
        return is_known_at_comptime(k.target)

    case Index:
        return is_known_at_comptime(k.target) &&
               is_known_at_comptime(k.index)

    case TakeSlice:
        if !is_known_at_comptime(k.target) do return false

        if !k.empty_start {
            if !is_known_at_comptime(k.start) do return false
        }

        if !k.empty_end {
            if !is_known_at_comptime(k.end) do return false
        }

        return true

    case Reference:
        return is_known_at_comptime(k.expr)
    case Deref:
        return is_known_at_comptime(k.expr)

    case Len:
        return is_known_at_comptime(k.target)

    case SizeOf:
        // expr_types[id] stores the referenced/queried type
        // for these expressions when applicable.
        if t, has := get_ctx().expr_types[id]; has {
            if is_generic(t) do return false
        }

        // Type-only forms and bare type names need no expression walk.
        if _, has := k.t.(TypeSpecifier); has {
            return true
        }

        if k.names_type do return true

        if e, has := k.expr.(ExprId); has {
            return is_known_at_comptime(e)
        }

        return true
    case TypeIdOf:
        // expr_types[id] stores the referenced/queried type
        // for these expressions when applicable.
        if t, has := get_ctx().expr_types[id]; has {
            if is_generic(t) do return false
        }

        // Type-only forms and bare type names need no expression walk.
        if _, has := k.t.(TypeSpecifier); has {
            return true
        }

        if k.names_type do return true

        if e, has := k.expr.(ExprId); has {
            return is_known_at_comptime(e)
        }

        return true

    case TypeInfoOf:
        return is_known_at_comptime(k.expr)

    case FnCall:
        // A function call may execute runtime code.
        return false

    case FnLit:
        // Keep function literals out until their constant representation
        // and capture behavior are explicitly supported.
        return false

    case:
        debugln("comptime: unhandled expression -> false")
        return false
    }
}



typecheck_module :: proc(ast: ^AST, mid: ModId, resolved_path: string) {
    tctx = TcContext{false, nil, nil}

    // typecheck all first
    tc_items(ast.items[:]);

    tctx = TcContext{false, nil, nil}
}

clone_map :: proc(m: map[$K]$V, allocator := context.allocator) -> map[K]V {
    out := make(map[K]V, len(m), allocator)
    for k, v in m {
        out[k] = v
    }
    return out
}

