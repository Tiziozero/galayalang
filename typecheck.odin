package main

import "core:slice"
import "vendor:glfw/bindings"
import "core:fmt"
// try global context
TcContext :: struct {
    in_function: bool,
    fn_ret_ty: Maybe(TypeId),
    in_loop: Maybe(StmtId),
}
tctx := TcContext{}


can_coerce_to :: proc(to, target: TypeId) -> bool {
    src := get_type(target)
    dst := get_type(to)

    #partial switch src.kind {
    case .UntypedInteger:
        // value range is a separate check on the literal itself
        return is_integer(to) || is_float(to) || is_byte_like(to)

    case .UntypedFloat:
        return is_float(to)

    case .UntypedStruct:
        if dst.kind != .Struct do return false
        return untyped_struct_fits(dst, src)
    }

    // already typed, so it's just identity
    return types_equal(to, target)
}

// every field of the literal must exist in the destination and coerce into it
untyped_struct_fits :: proc(dst, src: ^Type) -> bool {
    for sf in src.structure.fields {
        found := false
        for df in dst.structure.fields {
            if df.name != sf.name do continue
            found = true
            if !types_equal(df.type, sf.type) && !can_coerce_to(df.type, sf.type) {
                return false
            }
            break
        }
        if !found do return false
    }
    return true
}
assignable :: proc(to, target: TypeId, loc:=#caller_location) -> (TypeId, bool, string) {
    if is_untyped(to) {
        gala_panicf("assignable \"to\" can't be untyped, but is. ({})\n\tcalled here: {}", tts(to), loc)
    }
    debugln("ASSIGNABLE:", tts(to), tts(target))
    debugln(tts(to), tts(target))
    if to == target do return  to, true, "ok1"
    if is_untyped(target) {
        if can_coerce_to(to, target) do return to, true, "ok2"
        else do return 0, false, fmt.aprintf("Can't coerce type {} to {}", tts(target), tts(to))
    }
    return 0, false, fmt.aprintf("types {} and {} don't match.", tts(target), tts(to)) 
    // panic("impl")
}


propagate_type :: proc(t: TypeId, id: ExprId) {
    #partial switch i in get_expr(id) {
    case Binop: {
        propagate_type(t, i.left)
        propagate_type(t, i.right)
    }
    case Number:
    // skip setting these
    case Symbol, FnCall, Reference, Deref, Cast, Transmute: return
    case: debugln(i); panic("impl")
    }
    get_ctx().tc_types[id] = t
}

bool_ty :: proc() -> TypeId { return ty_from_name("bool") }
binop_res_ty :: proc(kind: BinopKind, r: TypeId) -> (TypeId, bool, string) {
    switch kind {
    case .Addition, .Subtraction:
        return r, true, ""
    case .Multiply, .Divide:
        return r, true, ""
    case .Modulo, .BitAnd, .BitOr, .BitXor, .ShiftLeft, .ShiftRight:
        return r, true, ""
    case .Equal, .NotEqual:
        return bool_ty(), true, ""
    case .Less, .LessEqual, .Greater, .GreaterEqual:
        return bool_ty(), true, ""
    case .LogicalAnd, .LogicalOr:
        return bool_ty(), true, ""
    }
    panic("what")
}
binop :: proc(kind: BinopKind, e1, e2: ExprId) -> (TypeId, bool, string) {
    t1 := expr_ty(e1)
    t2 := expr_ty(e2)
    if t1 == t2 {
        if can_binop(t1, kind) do return binop_res_ty(kind, t1)
        else do return 0, false, fmt.aprintf("Can't binop type {} with op {}", tts(t1), kind)
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
    return binop_res_ty(kind, r)
    // panic("impl")
}

// old gen_bindings ig?
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
        t, ok, msg := bind_generics(ty_args, args, template, s);
        if !ok {
            highlight_lines(get_span(id))
            gala_panicf("Generic Binings error: {}.", msg)
        }
        append(&temp.instances, {ty_args, args, t, 0, false})
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
         .Rune,.Byte,.Bool,.Union, .Any, .Void: 
        return 0, false
    case .Slice: return ty_has_generic(t.slice.type)
    case .String: return 0, false
    case .Struct: panic("impl")
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
    case .Function: {
        panic("impl") // cba rn
    }
    case .UInt64, .UInt32, .UInt16, .UInt_8,
         .Int64,.Int32,.Int16,.Int_8,
         .Flt64,.Flt32,.Flt16,.Flt_8,
         .Rune,.Byte,.Bool,.Union, .Any, .Void: 
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
    case .Struct: panic("impl")
    case .Invalid, .UntypedInteger, .UntypedFloat, .UntypedStruct: panic("what")
    }
    panic("impl")
}

// walk down types and match. handles "^$T" and "[]i32" errors
unify :: proc(template_type_id, arg_type_id:  TypeId, bindings: ^map[TypeId]TypeId) -> (TypeId, bool, string) {
    p := get_type(template_type_id)
    #partial switch p.kind {
    case .Generic: {
        b, has := bindings[template_type_id];
        if !has {
            bindings^[template_type_id] = arg_type_id
            return arg_type_id, true, ""
        } else {
            // if previous binding is untyped and arg isn't, set to arg
            // generics bind first typed arg, and then use defaults if it's untyped
            if is_untyped(b) && !is_untyped(arg_type_id) {
                bindings^[template_type_id] = arg_type_id
            } else if is_untyped(b) && is_untyped(arg_type_id) {
                if get_type(b).kind == .UntypedFloat || get_type(arg_type_id).kind == .UntypedFloat  {
                    bindings^[template_type_id] = intern_type({kind=.UntypedFloat})
                }
            } else {
                t, ok, msg := assignable(b, arg_type_id);
                if !ok {
                    return 0, false, msg
                }
            }
            return b, true, ""
        }
    }
    case .Pointer: {
        if get_type(arg_type_id).kind != .Pointer {
            return 0, false, "Types don't match"
        }
        return unify(p.ptr, get_type(arg_type_id).ptr, bindings)
    }
    case .Union: {
        assert(!is_generic(template_type_id))
        return assignable(template_type_id, arg_type_id)
    }
    case .Struct: {
        assert(!is_generic(template_type_id))
        return assignable(template_type_id, arg_type_id)
    }
    case .Function: {
        assert(!is_generic(template_type_id))
        return assignable(template_type_id, arg_type_id)
    }

    case: {
        return assignable(template_type_id, arg_type_id);
    }
    }

    panic("impl")
}
// get type from args
// if type is in turbofish, use those
bind_generics :: proc(ty_args, args: []TypeId, template: TypeId, s: FnCall) -> (TypeId, bool, string) {
    for t in ty_args { debugln("arg ty:", tts(t), t) }
    bindings := make(map[TypeId]TypeId) // generic type id -> resolved type id
    fn := get_type(template).fn;
    // populate with type args first
    for a, i in ty_args {
        bindings[fn.type_args[i].ty] = a;
    }
    new_args := make([]FnArg, len(fn.args));
    debugln("GEN CALL BINDINGS")
    // "unify" all args
    for a, i in fn.args {
        t, ok, msg := unify(a.ty, args[i], &bindings)
        if !ok {
            return 0, false, fmt.aprintf("{} in arg {}", msg, i)
        }
    }
    if fn.is_variadic {
        // basically, do same as above, but keep "template_type_id" the same (fn.variadic_ty) and change "arg_type_id"
        for a, index in args[len(fn.args):] {
            i := index + len(fn.args)
            t, ok, msg := unify(fn.variadic_ty, a, &bindings)
            if !ok {
                return 0, false, fmt.aprintf("{} in arg {}", msg, i)
            }
        }
    }
    // untyped valeues
    for i, k in bindings {
        if is_untyped(k) {
            t, ok, msg := get_assignable_default_type(k);
            if ! ok { return 0, false, msg }
            bindings[i] = t
        }
    }
    // once we have all bindings, populate new args
    for a, i in fn.args {
        t : TypeId = a.ty
        if is_generic(t) {
            t = ty_fill_from_generics(t, bindings)
        }
        new_args[i] = {a.name, t};
    }

    rt:= fn.ret_ty;
    if is_generic(rt) {
        rt = ty_fill_from_generics(rt, bindings)
    }
    vt:= fn.variadic_ty;
    if fn.is_variadic && is_generic(vt) {
        vt = ty_fill_from_generics(vt, bindings)
    }
    debugln("bindings:")
    for a, i in fn.args {
        debugfln("\t{}: {} -> {}", a.name, tts(a.ty), tts(new_args[i].ty))
    }
    if fn.is_variadic {
        debugfln("  var ty: {} -> {}", tts(fn.variadic_ty), tts(vt))
    }
    debugfln("  ret ty: {} -> {}", tts(fn.ret_ty), tts(rt))
    
    new_ty := get_type(template)^
    new_ty.fn.is_generic = false
    new_ty.fn.args = new_args;
    new_ty.fn.ret_ty = rt
    new_ty.fn.variadic_ty = vt
    new_ty.fn.type_args = nil
    new_ty.fn.generic = {}
    return new_fn_type(new_ty), true, ""
}
// oof. this is gonna be ugly
tc_generic_fn_call :: proc(id: ExprId) {
    f := get(id).(FnCall)
    tc_expr(f.target);
    // call type
    ct := get_type(expr_ty(f.target));
    assert(ct.kind == .Function)
    fn := ct.fn;
    assert(fn.is_generic)

    // can be variadic
    // assert(len(f.args) == len(fn.args))
    assert(len(f.type_args) == len(fn.type_args))

    // generic interface? for instance, ig
    ty_args_ty := make([]TypeId, len(f.type_args))
    args_ty := make([]TypeId, len(f.args))

    for a, i in f.type_arg_ids {
        ty_args_ty[i] = a
        t := a
    }

    for a, i in f.args {
        assert(a.name == "")
        tc_expr(a.expr) // typecheck
        args_ty[i] = expr_ty(a.expr)
    }
    // get bindings for this signature
    template_ty, key := get_bindings_for_generic_fn(ty_args_ty, args_ty, expr_ty(f.target), id)
    // convoluted but it's ok. it's a one time thing
    new_ty := get_ctx().fn_templates[template_ty].instances[key].ty_id;
    // store in context
    get_ctx().call_instances[id] = { expr_ty(f.target), new_ty, key }
    // update and typecheck 
    fn = get_type(new_ty).fn;

    for a, i in fn.args {
        aty := a.ty
        ety := expr_ty(f.args[i].expr)
        expr := f.args[i].expr
        t, ok, msg := assignable(aty, ety)
        if !ok {
            highlight_lines(expr_span(expr))
            gala_panicf("Failed to tc fn call (arg {}): {}", i, msg)
        }
        propagate_type(t, expr)
    }


    ctx := get_ctx();
    ctx.tc_types[id] = fn.ret_ty
}

tc_fn_call :: proc(id: ExprId) {
    f := get(id).(FnCall)
    tc_expr(f.target);
    // call type
    ct := get_type(expr_ty(f.target));
    assert(ct.kind == .Function)
    fn := ct.fn;
    if fn.is_generic {
        tc_generic_fn_call(id)
        return
    }
    // not generic
    assert(len(f.type_args) == 0)
    assert(len(f.type_arg_ids) == 0)
    // make sure
    // can be variadic
    // assert(len(f.args) == len(fn.args))
    assert(len(f.type_args) == len(fn.type_args))

    for a in f.args {
        assert(a.name == "")
        tc_expr(a.expr) // typecheck
    }
    for a, i in fn.args {
        aty := a.ty
        ety := expr_ty(f.args[i].expr)
        expr := f.args[i].expr
        t, ok, msg := assignable(aty, ety)
        if !ok {
            highlight_lines(expr_span(expr))
            gala_panicf("Failed to tc fn call (arg {}): {}", i, msg)
        }
        propagate_type(t, expr)
    }
    if len(f.args) != len(fn.args) { assert(fn.is_variadic) }
    remaining_args := f.args[len(fn.args):]
    for a, ri in remaining_args {
        i := ri + len(fn.args);
        aty :=  fn.variadic_ty
        ety := expr_ty(f.args[i].expr)
        expr := f.args[i].expr
        t, ok, msg := assignable(aty, ety)
        if !ok {
            highlight_lines(expr_span(expr))
            gala_panicf("Failed to tc fn call (arg {}): {}", i, msg)
        }
        propagate_type(t, expr)
    }
    ctx := get_ctx();
    ctx.tc_types[id] = fn.ret_ty
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
        opty := is_untyped(expr_ty(i.left)) ? expr_ty(i.right) : expr_ty(i.left)  // for compare/logical ops
        propagate_type(opty, i.left)
        propagate_type(opty, i.right)
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
    case String: {
            ctx.tc_types[id] = ty_from_name("string")
    }
    case Transmute: {
        to, ok := ctx.expr_types[id]; assert(ok)
        tc_expr(i.target)
        if is_untyped(expr_ty(i.target)) {
            t, ok, msg := get_assignable_default_type(expr_ty(i.target))
            if !ok {
                highlight_lines(get_span(i.target))
                gala_panicf("Default type error (since target is untyped): {}.", msg)
            }
            propagate_type(t, i.target)
        }
        assert(can_cast_to(expr_ty(i.target), to))
        ctx.tc_types[id] = to
    }
    case Cast: {
        to, ok := ctx.expr_types[id]; assert(ok)
        tc_expr(i.target)
        if is_untyped(expr_ty(i.target)) {
            t, ok, msg := get_assignable_default_type(expr_ty(i.target))
            if !ok {
                highlight_lines(get_span(i.target))
                gala_panicf("Default type error (since target is untyped): {}.", msg)
            }
            propagate_type(t, i.target)
        }
        assert(can_cast_to(expr_ty(i.target), to))
        ctx.tc_types[id] = to
    }
    case Symbol: {
        o := get_obj(expr_obj(id));
        assert(o.type != nil)
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
        if is_untyped(index_ty) {
            ok: bool
            msg: string
            index_ty, ok, msg = get_assignable_default_type(index_ty)
            if !ok {
                highlight_lines(get_span(i.index))
                gala_panicf("Index error:", msg)
            }
            propagate_type(index_ty, i.index)
        }
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
    case: debugln(i); panic("impl")
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
    case: return false, fmt.aprintf("can't reference a {}", type_info_of(type_of(e))^)
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
    case .FixedSizeArray: return t.slice.type
    case .String: return ty_from_name("byte")
    case: panic("impl")
    }
}
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
            get(stmt_obj(id)).type = t // update
        }
        if !ok {
            highlight_lines(span)
            gala_panic("Failed vardec:", msg);
        }
        propagate_type(t, i.value)
        debugln("Vardec ok:", tts(get(stmt_obj(id)).type.(TypeId)))
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
        ptcxt := tctx
        tctx.in_loop = id // for loops
        tc_stmts(i.block.stmts)
        tctx = ptcxt
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
            // debugln("ret ty:",tts(rt), rt)
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
tc_items :: proc(items: []ItemId) {
    ctx := get_ctx()
    for id in items {
        span := get_span(id)
        #partial switch i in get_item(id) {
        case StructDec: {}
        case FnDec: {
            oid := ctx.item_objects[id];
            tyid := get_obj(oid).type.(TypeId); // must be known atp
            ty := get_type(tyid)
            assert(ty.kind == .Function)
            if ty.fn.is_generic {
                continue
            } else {
                ptcxt := tctx
                tctx.in_function = true
                if is_void(ty.fn.ret_ty) { // means no return type
                    tctx.fn_ret_ty = nil // no return ty
                } else {
                    tctx.fn_ret_ty = ty.fn.ret_ty
                }
                t := tctx.fn_ret_ty

                tc_stmts(i.block.stmts)

                tctx = ptcxt
            }
        }
        case ExternFnDec: {}
        case: debugln(i); panic("impl")
        }
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
resolve_and_typecheck_generics :: proc() {
    c := get_ctx()

    change := true
    iterations := 0
    for change && iterations < 20 {
        change = false

        // snapshot of the templates; the live map may grow while we work
        templates := clone_map(c.fn_templates)
        defer delete(templates)

        for tid, temp in templates {
            itemid := temp.item
            mod := c.mods[temp.mid]
            s := &mod.declarations

            // snapshot of the instances; new ones are picked up next pass
            instances := slice.clone(temp.instances[:])
            defer delete(instances)

            for inst, inst_index in instances {
                if inst.resolved do continue

                clone := clone_fn_item(c, itemid)
                item := get_item(clone).(FnDec)
                assert(!is_generic(inst.ty_id))
                ty := get_type(inst.ty_id)

                // resolve
                {
                    fn_scope := new_scope(s)
                    fn := ty.fn
                    for a, i in fn.args {
                        new_object(&fn_scope,
                            {kind=.Argument, type=a.ty, name=a.name, read_only=.Argument},
                            item.args[i].span)
                    }
                    if fn.is_variadic {
                        o := Object{kind=.Argument, read_only=.Argument, name=fn.variadic_name, type=fn.effective_variadic_ty};
                        oid := new_object(&fn_scope, o, item.variadic_arg_span); // auto panics on duplicate name
                    }
                    for stmt in item.block.stmts {
                        resolve_stmt(&fn_scope, stmt)
                    }
                }

                // typecheck
                {
                    ptcxt := tctx
                    tctx.in_function = true
                    if is_void(ty.fn.ret_ty) {
                        tctx.fn_ret_ty = nil
                    } else {
                        tctx.fn_ret_ty = ty.fn.ret_ty
                    }

                    tc_stmts(item.block.stmts)

                    tctx = ptcxt
                }

                change = true
                debugfln("updated {}/{} with key {}", tid, item.name, inst_index)

                // write to the LIVE template, looked up fresh each time
                c.fn_templates[tid].instances[inst_index].resolved = true
                c.fn_templates[tid].instances[inst_index].resolved_item = clone
                append(&mod.ast.items, clone)
            }
        }
        iterations += 1
    }
    if change {
        panic("too many iterations")
    }
}
