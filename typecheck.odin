package main

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
assignable :: proc(to, target: TypeId) -> (TypeId, bool, string) {
    if to == target do return  to, true, "ok1"
    if is_untyped(target) {
        if can_coerce_to(to, target) do return to, true, "ok2"
        else do return 0, false, fmt.aprintf("Can't coerce type {} to {}", tts(target), tts(to))
    }
    panic("impl")
}


propagate_type :: proc(t: TypeId, id: ExprId) {
    #partial switch i in get_expr(id) {
    case Binop: {
        propagate_type(t, i.left)
        propagate_type(t, i.right)
    }
    case Number:
    // skip setting these
    case Symbol, FnCall: return
    case: debugln(i); panic("impl")
    }
    get_ctx().tc_types[id] = t
}

binop :: proc(kind: BinopKind, e1, e2: ExprId) -> (TypeId, bool, string) {
    t1 := expr_ty(e1)
    t2 := expr_ty(e2)
    if t1 == t2 do return t1, true, ""
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
            return 0, false, fmt.aprint("Can't binop Untyped Float (lhs) to {} (rhs) ", tts(r))
        }
        r = t2
    } else if is_untyped(t2) && !is_untyped(t1) {
        if is_integer(t1) && get_type(t2).kind == .UntypedFloat {
            return 0, false, fmt.aprint("Can't binop Untyped Float (rhs) to {} (lhs) ", tts(r))
        }
        r = t1
    } else if is_untyped(t1) && is_untyped(t2) {
        if get_type(t1).kind == .UntypedFloat || get_type(t2).kind == .UntypedFloat {
            r = intern_type({kind=.UntypedFloat})
        } else {
            r = intern_type({kind=.UntypedInteger})
        }
    }

    if !can_binop(r, kind) {
        return 0, false, fmt.aprint("Can't binop type {} with op {}", tts(r), kind)
    }
    panic("impl")
}

// old gen_bindings ig?
get_bindings_for_generic_fn :: proc(ty_args, args: []TypeId, template: TypeId, s: FnCall) -> (temp_id:TypeId, key:int) {
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
        t := bind_generics(ty_args, args, template, s);
        append(&temp.instances, {ty_args, args, t})
        return template, len(temp.instances) - 1
    } else {
        debugln("template doesn't exist.", template, s)
        panic("template doesn't exist")
    }
    panic("impl")
}

// get type from args
// if type is in turbofish, use those
bind_generics :: proc(ty_args, args: []TypeId, template: TypeId, s: FnCall) -> TypeId {
    for t in ty_args { debugln("arg ty:", tts(t), t) }
    bindings := make(map[TypeId]TypeId) // generic type id -> resolved type id
    fn := get_type(template).fn;
    // populate with type args first
    for a, i in ty_args {
        bindings[fn.type_args[i].ty] = a;
    }
    new_args := make([]FnArg, len(fn.args));
    for a, i in fn.args {
        // if template arg is in bindings skip or change if it's untyped
        if t, ok := bindings[a.ty]; ok {
            // if it's untyped in bindings, but not in current arg, use typed from args
            if is_untyped(t) && !is_untyped(args[i]) {
                t := args[i]
                bindings[a.ty] = t;
            }
        } else { // otherwhise generate
            // we want to find what type it will be
            // infere from args given (args)
            t := args[i]
            bindings[a.ty] = t;
        }
    }
    // once we have all bindings, populate new args
    for a, i in fn.args {
        if t, ok := bindings[a.ty]; ok {
            if is_untyped(t) {
                nt, ok, msg := get_assignable_default_type(t)
                if !ok {
                    highlight_lines(expr_span(s.args[i].expr))
                    gala_panic("Failed to infer defalt type from arg for generic:", msg)
                }
                bindings[a.ty] = nt
                t = nt
            }
            new_args[i] = {a.name, t};
        } else {
            debugln(a, i)
            panic("Missed one")
        }
    }
    // TODO: binding might not be present if (eg) args give "$T" but return type is "^T", as "^T" is never declared
    rt, ok := bindings[fn.ret_ty];
    assert(ok);
    debugln("bindings:")
    for a, i in fn.args {
        debugfln("\t{}: {} -> {}", a.name, tts(a.ty), tts(new_args[i].ty))
    }
    debugfln("  ret ty: {} -> {}", tts(fn.ret_ty), tts(rt))
    
    new_ty := get_type(template)^
    new_ty.fn.is_generic = false
    new_ty.fn.args = new_args;
    new_ty.fn.ret_ty = rt
    new_ty.fn.type_args = nil
    new_ty.fn.generic = {}
    return new_fn_type(new_ty)
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

    assert(len(f.args) == len(fn.args))
    assert(len(f.type_args) == len(fn.type_args))

    // generic interface? for instance, ig
    ty_args_ty := make([]TypeId, len(f.type_args))
    args_ty := make([]TypeId, len(f.args))

    debugln("TC FN CALL ARG TY", f.type_arg_ids)
    for a, i in f.type_arg_ids {
        ty_args_ty[i] = a
        t := a
        debugln("FN CALL ARG TY", t, tts(t));

    }

    for a, i in f.args {
        assert(a.name == "")
        tc_expr(a.expr) // typecheck
        args_ty[i] = expr_ty(a.expr)
    }
    // get bindings for this signature
    template_ty, key := get_bindings_for_generic_fn(ty_args_ty, args_ty, expr_ty(f.target), f)
    // convoluted but it's ok. it's a one time thing
    new_ty := get_ctx().fn_templates[template_ty].instances[key].ty_id;
    // store in context
    get_ctx().call_instances[id] = { expr_ty(f.target), new_ty, key }
    // update and typecheck 
    fn = get_type(new_ty).fn;
    for a in fn.args {
        debugln("tc\t\t", a.name, tts(a.ty))
    }

    for a, i in fn.args {
        debugln("checking arg:", i, a.name)
        aty := a.ty
        ety := expr_ty(f.args[i].expr)
        expr := f.args[i].expr
        t, ok, msg := assignable(aty, ety)
        if !ok {
            highlight_lines(expr_span(expr))
            gala_panicf("Failed to tc fn call (arg {}): {}", i, msg)
        }
        debugln("propagating type:", tts(t), "to", get(expr))
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
    assert(len(f.args) == len(fn.args))
    assert(len(f.type_args) == len(fn.type_args))

    for a in f.args {
        assert(a.name == "")
        tc_expr(a.expr) // typecheck
    }
    for a, i in fn.args {
        debugln("checking arg:", i, a.name)
        aty := a.ty
        ety := expr_ty(f.args[i].expr)
        expr := f.args[i].expr
        t, ok, msg := assignable(aty, ety)
        if !ok {
            highlight_lines(expr_span(expr))
            gala_panicf("Failed to tc fn call (arg {}): {}", i, msg)
        }
        debugln("propagating type:", tts(t), "to", get(expr))
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
        propagate_type(ty, i.left)
        propagate_type(ty, i.right)
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
    case Symbol: {
        o := get_obj(expr_obj(id));
        assert(o.type != nil)
        ctx.tc_types[id] = o.type.(TypeId)
    }
    case FnCall: tc_fn_call(id)
    case: debugln(i); panic("impl")
    }
}
get_assignable_default_type :: proc(t: TypeId) -> (TypeId, bool, string) {
    if get_type(t).kind == .Pointer do return 0, false, "can't assign value of type void"
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
tc_stmts :: proc(stmts: []StmtId) {
    ctx := get_ctx()
    for id in stmts {
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
        case: debugln(i); panic("impl")
        }
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
                tctx.fn_ret_ty = ty.fn.ret_ty

                tc_stmts(i.block.stmts)

                tctx = ptcxt
            }
        }
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
