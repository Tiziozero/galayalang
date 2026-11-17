package main

import "core:fmt"
import "core:slice"

ObjectKind :: enum {
    Invalid,
    Item, // functions
    Variable,
    Argument,
}

ReadOnly :: enum {
    No,
    Argument,
    LoopVar,
    Function,
    Constant,
}

Object :: struct {
    kind: ObjectKind,
    name: string,
    type: Maybe(TypeId),
    read_only: ReadOnly,
}

// defined here because scopes need it
Scope :: struct {
    objects:        map[string]ObjId,
    types:          map[string]TypeId,
    obj_forward:   map[string]ObjId,
    ty_forward:    map[string]TypeId,
    ty_pending:     map[string]ItemId, // for type aliases resolution
    parent:         ^Scope,
}

ModuleScope :: struct {
    using scope: Scope,
    obj_exports:   map[string]ObjId,
    ty_exports:    map[string]TypeId,
}

report_first_decl :: proc(id: $T) {
    fmt.println("First declared here:")
    highlight_lines(get_span(id))
}

name_exists :: proc(scope: ^Scope, n: string, error := false) -> bool {
    for s := scope; s != nil; s = s.parent {
        if id, ok := s.types[n]; ok {
            if error do report_first_decl(id)
            return true
        }
        if id, ok := s.objects[n]; ok {
            if error do report_first_decl(id)
            return true
        }
        if id, ok := s.ty_forward[n]; ok {
            if error do report_first_decl(id)
            return true
        }
        if id, ok := s.obj_forward[n]; ok {
            if error do report_first_decl(id)
            return true
        }
        if id, ok := s.ty_pending[n]; ok {
            if error do report_first_decl(id)
            return true
        }
    }
    return false
}

@(private="file")
add_object :: proc(o: Object, span: Span) -> ObjId {
    ctx := get_ctx()
    assert(o.kind != .Invalid)
    assert(len(o.name) > 0)
    append(&ctx.objs, o)
    id := ObjId(len(ctx.objs) - 1)
    ctx.object_modules[id] = current_module_id
    ctx.spans.objs_decs[id] = span
    return id
}

@(private="file")
add_type :: proc(t: Type, span: Span) -> TypeId {
    ctx := get_ctx()
    assert(t.kind != .Invalid)
    assert(len(t.name) > 0)
    append(&ctx.types, t)
    id := TypeId(len(ctx.types) - 1)
    ctx.type_modules[id] = current_module_id
    ctx.spans.ty_decs[id] = span
    return id
}
// Names share one namespace and can't shadow, so parents are checked too.
check_undeclared :: proc(s: ^Scope, name: string, span: Span) {
    if !name_exists(s, name, true) do return
    highlight_lines(span)
    gala_panicf("\"%s\" is already declared (shadowing isn't allowed).", name)
}

new_object :: proc(s: ^Scope, o: Object, span: Span) -> ObjId {
    check_undeclared(s, o.name, span)
    id := add_object(o, span)
    s.objects[o.name] = id
    return id
}

new_type :: proc(s: ^Scope, t: Type, span: Span) -> TypeId {
    check_undeclared(s, t.name, span)
    id := add_type(t, span)
    s.types[t.name] = id
    return id
}

new_object_fd :: proc(s: ^ModuleScope, o: Object, span: Span) -> ObjId {
    check_undeclared(s, o.name, span)
    id := add_object(o, span)
    s.obj_forward[o.name] = id
    return id
}

new_type_fd :: proc(s: ^ModuleScope, t: Type, span: Span) -> TypeId {
    check_undeclared(s, t.name, span)
    id := add_type(t, span)
    s.ty_forward[t.name] = id
    return id
}

scope_get_object :: proc(s: ^Scope, n: string) -> (ObjId, bool) {
    scope := s
    for scope != nil {
        id, ok := scope.objects[n]
        if ok { return id, true }
        // check fds too
        id, ok = scope.obj_forward[n]
        if ok { return id, true }
        scope = scope.parent
    }
    return 0, false
}



// silent lookup: a miss is a normal answer here (see TypeIdOf)
scope_find_type :: proc(s: ^Scope, n: string) -> (TypeId, bool) {
    scope := s
    for scope != nil {
        e, _ := slice.map_entries(scope.types)
        // for k, v in e { debugfln("{} : {}", n, k.key) }
        id, ok := scope.types[n]
        if ok { return id, true }
        // check fds too
        e, _ = slice.map_entries(scope.ty_forward)
        // for k, v in e { debugfln("{} : {}", n, k.key) }
        id, ok = scope.ty_forward[n]
        if ok { return id, true }
        e2, _ := slice.map_entries(scope.ty_pending)
        // for k, v in e2 { debugfln("{} : {}", n, k.key) }
        if aid, ok := scope.ty_pending[n]; ok {
            return resolve_alias(scope, aid), true
        }
        scope = scope.parent
    }
    return 0, false
}

scope_get_type :: proc(s: ^Scope, n: string) -> (TypeId, bool) {
    id, ok := scope_find_type(s, n)
    if !ok do debugln(n, "Not found")
    return id, ok
}

// Like scope_find_type, but a miss is a compile error.
lookup_type :: proc(s: ^Scope, name: string, span: Span) -> TypeId {
    if tid, ok := scope_find_type(s, name); ok do return tid
    highlight_lines(span)
    gala_panicf("Type \"%s\" doesn't exist.", name)
}

intern_type :: proc(t: Type) -> TypeId {
    assert(t.kind == .Pointer || t.kind == .Void ||
        t.kind == .UntypedInteger || t.kind == .UntypedFloat || t.kind == .UntypedStruct ||
        t.kind == .FixedSizeArray || t.kind == .Slice || t.kind == .String || t.kind == .Any ||
        t.kind == .Union || (t.kind == .Struct && t.structure.is_anonymous))
    for ty, id in get_ctx().types {
        if type_equal(ty, t) { return TypeId(id) }
    }
    append(&get_ctx().types, t)
    return TypeId(len(get_ctx().types)-1)
}

panic_duplicate_arg :: proc(name: string, first, dup: Span) {
    fmt.println("First declared here:")
    highlight_lines(first)
    highlight_lines(dup)
    gala_panicf("Duplicate argument \"%s\".", name)
}


// ---------------------------------------------------------------------------
// Scopes and modules
// ---------------------------------------------------------------------------

// get_ctx().allocator may not be available on context init, so it's a param
new_scope :: proc(parent:^Scope=nil) -> Scope {
    s := Scope{}
    s.objects       = make(map[string]ObjId)
    s.types         = make(map[string]TypeId)
    s.ty_pending    = make(map[string]ItemId)
    s.parent = parent
    return s
}

new_module_scope :: proc(parent:^Scope=nil) -> ModuleScope {
    s := ModuleScope{}
    s.scope = new_scope(parent)
    s.parent = parent
    s.obj_forward  = make(map[string]ObjId)
    s.ty_forward   = make(map[string]TypeId)
    return s
}

Module :: struct {
    declarations: ModuleScope,
    ast: ^AST,
    path: string,
}

// resolution is gonna be simple for now
// 1. forward declare names
// 2. resolve names within each item

forward_items :: proc(s: ^ModuleScope, items: []ItemId) {
    ctx := get_ctx();
    for id in items {
        item := get_item(id)
        #partial switch i in item {
        case FnDec: {
            oid := new_object_fd(s, {kind=.Item, name=i.name, type=nil, read_only=.Function}, i.span);
            // set references
            ctx.object_modules[oid] = current_module_id;
            ctx.item_objects[id] = oid;
             // export symbol
            s.obj_exports[i.name] = oid;
        }
        case ExternFnDec: {
            oid := new_object_fd(s, {kind=.Item, name=i.name, type=nil, read_only=.Function}, i.span);
            // set references
            ctx.object_modules[oid] = current_module_id;
            ctx.item_objects[id] = oid;
             // export symbol
            s.obj_exports[i.name] = oid;
        }
        case GlobalVarDec: {
            oid := new_object_fd(s, {kind=.Item, name=i.name, type=nil, read_only=.Function}, get_span(id));
            // set references
            ctx.object_modules[oid] = current_module_id;
            ctx.item_objects[id] = oid;
             // export symbol
            s.obj_exports[i.name] = oid;
        }
        case StructDec: {
            tid := new_type_fd(s, {kind=.Struct, name=i.name}, get_span(id))
            ctx.type_modules[tid] = current_module_id;
            ctx.item_types[id] = tid;
            s.ty_exports[i.name] = tid;
        }
        case Import: {
            cm := ctx.mods[current_module_id]
            resolved := handle_file(cm.path, i.fname)
            imid, ok := ctx.modules[resolved]; assert(ok)
            m := ctx.mods[imid]
            decs := m.declarations
            for n, iid in decs.obj_exports {
                if i.imports_all {
                    s.objects[n] = iid
                } else {
                    if slice.contains(i.symbols, n) do s.objects[n] = iid
                }
            }
            for n, iid in decs.ty_exports {
                if i.imports_all {
                    s.types[n] = iid
                } else {
                    if slice.contains(i.symbols, n) do s.types[n] = iid
                }
            }

        }
        case TypeAlias: {
            check_undeclared(s, i.new_name, get_span(id))
            s.ty_pending[i.new_name] = id
        }
        case: debugln(i); panic("impl")
        }
    }
}
alias_active: map[ItemId]bool
// Resolved on first use, like ensure_const for constants.
resolve_alias :: proc(s: ^Scope, id: ItemId) -> TypeId {
    a := get(id).(TypeAlias)
    if t, done := s.types[a.new_name]; done do return t
    if alias_active[id] {
        highlight_lines(get_span(id))

        gala_panicf("Type alias \"%s\" is cyclic.", a.new_name)
    }
    if ts_is_generic(a.ty) {
        highlight_lines(get_span(id))
        gala_panic("Can't have generics in type aliases.")
    }
    alias_active[id] = true
    // no_declare_generic: an alias has nowhere to bind a `$T`
    t := resolve_type_specifier(s, a.ty, allow_generic_dec = false)
    delete_key(&alias_active, id)
    delete_key(&s.ty_pending, a.new_name)
    s.types[a.new_name] = t
    return t
}

resolve_type_specifier :: proc(s: ^Scope, t: TypeSpecifier, allow_generic_dec := true) -> TypeId {
    #partial switch k in t {
    case GenericSpecifier: {
        if ! allow_generic_dec {
            highlight_lines(k.span);
            gala_panic("can't specify generic item here")
        }
        check_undeclared(s, k.name, k.span)
        // return new generic type - do not intern
        return new_generic_param(s, {name=k.name, span=k.span});
    }
    case AnySpecifier: {
        return intern_type({kind=.Any})
    }
    case SliceSpecifier: {
        base := resolve_type_specifier(s, k.base^)
        return intern_type({kind=.Slice, slice={type=base}})
    }
    case BaseType: { // will get declared type id
        ty := lookup_type(s, string(k.ident), k.span)
        return ty
    }
     case PointerType: {
        id := resolve_type_specifier(s, k.ptr^)
        return intern_type({kind=.Pointer, ptr=id})
    }
    case FixedArreySpecifier: {
        id := resolve_type_specifier(s, k.base^)
        size := k.size
        if se, has_size_expr := k.size_expr.(ExprId); has_size_expr {
            i, _ := parse_integer_literal(get(se).(Number).text)
            size = 1
            // size = const_array_size(s, se)
        }
        return intern_type({kind=.FixedSizeArray,
            fixed_size_array={type=id, size=size}})
    }
    case FnSpecifier: {
        // assert(!allow_generic_dec) // no generics in fn specifiers
        if !allow_generic_dec {
            if ts_is_generic(t) {
                highlight_lines(k.span)
                gala_panicf("can't declare generics here.")
            }
        }
        ty := Type{};
        ty.kind = .Function
        declared: map[string]struct{t: TypeId, span: Span}
        fields := make([]FnParam, len(k.params))
        for a, i in k.params {
            if f, d := declared[a.name]; d {
                highlight_lines(f.span)
                gala_info("First declared here ^")
                highlight_lines(a.span)
                gala_panicf("Duplicate name \"{}\".", a.name)
            }
            assert(a.default == nil)
            t := resolve_type_specifier(s, a.t, false)
            arg := FnParam{a.name, t}
            fields[i] = arg
            declared[a.name] = {t, a.span}
        }
        ty.fn.args = fields
        if k.is_variadic {
            ty.fn.is_variadic = true
            ty.fn.variadic_ty = resolve_type_specifier(s, k.variadic_ty, false)
            ty.fn.effective_variadic_ty = 0
        }
        if rt, ok := k.ret_ty.(TypeSpecifier); ok {
            ty.fn.ret_ty = resolve_type_specifier(s, rt)
        } else {
            ty.fn.ret_ty = intern_type({kind=.Void})
        }
        return new_fn_type(ty)

    }
    case: debugln(t); panic("impl")
    }
    panic("impl");
}

new_generic_param :: proc(s: ^Scope, a: GenericParam) -> TypeId{
    check_undeclared(s, a.name, a.span)
    t := Type{kind=.Generic, name=a.name};
    return new_type(s, t, a.span)
}
// resolves signature
// 1. resolve generic argument types
//      create new generic type with TypeId
// 2. resolve arguments
// 3. resolve retrn type
// variadics later ig?
// 1. resolve type parameters first
// 2. resolve args in order
// 3. resolve return type
resolve_generic_function_signature :: proc(s: ^Scope, sig: FnDecSignature, id: ItemId) -> (TypeId, Scope, bool) {
    ty := Type{kind=.Function};

    fn_scope := new_scope(s);
    ty_args := make([]FnParam, len(sig.generics))
    for a, i in sig.generics {
        t := new_generic_param(&fn_scope, a); // defines new args. auto panics
        ty_args[i] = {a.name, t}
    }
    ty.fn.type_args = ty_args

    args := make([]FnParam, len(sig.params))
    for a, i in sig.params {
        assert(!a.inferred);
        assert(a.default == nil)
        t := resolve_type_specifier(&fn_scope, a.t);
        o := Object{kind=.Argument, read_only=.Argument, name=a.name, type=t};
        oid := new_object(&fn_scope, o, a.span); // auto panics on duplicate name
        args[i].name = a.name
        args[i].ty = t
    }
    ty.fn.args = args;

    if sig.is_variadic {
        t := resolve_type_specifier(&fn_scope, sig.variadic_ty);
        var_ty := t
        effective_var_ty := slice_from(t) // only internals
        o := Object{kind=.Argument, read_only=.Argument, name=sig.variadic_param_name, type=effective_var_ty};
        oid := new_object(&fn_scope, o, sig.variadic_param_span); // auto panics on duplicate name
        ty.fn.is_variadic = true
        ty.fn.variadic_ty = var_ty
        ty.fn.effective_variadic_ty = effective_var_ty
        ty.fn.variadic_name = sig.variadic_param_name
    }

    rt := ty_from_name("void");
    if sig.ret_ty != nil {
        rt = resolve_type_specifier(&fn_scope, sig.ret_ty.(TypeSpecifier), allow_generic_dec=false)
    }
    ty.fn.ret_ty = rt
    ty.fn.is_generic = true // hehe. yea
    tid := new_fn_type(ty)

    get_ctx().fn_templates[tid] = { id, current_module_id, make([dynamic]FnInstance) }

    return tid, fn_scope, true
}
ts_is_generic :: proc(t: TypeSpecifier) -> bool {
    switch k in t {
    case BaseType, AnySpecifier:
        return false

    case GenericSpecifier:
        return true

    case PointerType:
        return ts_is_generic(k.ptr^)

    case SliceSpecifier:
        return ts_is_generic(k.base^)

    case FixedArreySpecifier:
        return ts_is_generic(k.base^)

    case GenericInstSpecifier:
        // Node::<$T> is generic, Node::<i32> is not
        for arg in k.args {
            if ts_is_generic(arg) do return true
        }
        return false

    case FnSpecifier:
        // field names guessed, I haven't seen FnDecSignature
        for params in k.params {
            if ts_is_generic(params.t) do return true
        }
        if ret, ok := k.ret_ty.?; ok {
            return ts_is_generic(ret)
        }
        if k.is_variadic {
            return ts_is_generic(k.variadic_ty)
        }
        return false

    case StructSpecifier:
        for f in k.fields {
            if ts_is_generic(f.t) do return true
        }
        return false

    case UnionSpecifier:
        for f in k.fields {
            if ts_is_generic(f.t) do return true
        }
        return false
    }
    return false // nil union
}
signature_is_generic :: proc(sig: FnDecSignature) -> bool {
    if len(sig.generics) > 0 do return true
    for a in sig.params {
        if ts_is_generic(a.t) do return true
    }
    // can't do that
    if ts, ok := sig.ret_ty.(TypeSpecifier); ok {
        if ts_is_generic(ts) {
            highlight_lines(type_specifier_span(ts));
            gala_panicf("Can't specify Generic Type in return type.")
        }
    }
    if sig.is_variadic {
        if ts_is_generic(sig.variadic_ty) do return true
    }
    return false
}
resolve_function_signature :: proc(s: ^Scope, sig: FnDecSignature, id: ItemId, is_extern := false) -> (TypeId, Scope, bool) {
    ty := Type{kind=.Function};

    if signature_is_generic(sig) {
        if is_extern {
            highlight_lines(get_span(id))
            gala_panicf("Extern functions can't be generic.")
        }
        return resolve_generic_function_signature(s, sig, id)
    }

    fn_scope := new_scope(s);
    for a in sig.generics {
        new_generic_param(&fn_scope, a)
    }

    args := make([]FnParam, len(sig.params))
    for a, i in sig.params {
        assert(!a.inferred);
        assert(a.default == nil)
        t := resolve_type_specifier(&fn_scope, a.t);
        o := Object{kind=.Argument, read_only=.Argument, name=a.name, type=t};
        oid := new_object(&fn_scope, o, a.span); // auto panics on duplicate name
        args[i].name = a.name
        args[i].ty = t
    }
    ty.fn.args = args;

    if sig.is_variadic {
        t := resolve_type_specifier(&fn_scope, sig.variadic_ty);
        var_ty := t
        effective_var_ty := t
        if !is_extern {
            effective_var_ty = slice_from(t)
        }
        o := Object{kind=.Argument, read_only=.Argument, name=sig.variadic_param_name, type=effective_var_ty};
        oid := new_object(&fn_scope, o, sig.variadic_param_span); // auto panics on duplicate name
        ty.fn.is_variadic = true
        ty.fn.variadic_ty = var_ty
        ty.fn.effective_variadic_ty = effective_var_ty
        ty.fn.variadic_name = sig.variadic_param_name
    }

    rt := ty_from_name("void");
    if sig.ret_ty != nil {
        rt = resolve_type_specifier(&fn_scope, sig.ret_ty.(TypeSpecifier), allow_generic_dec=false)
    }
    ty.fn.ret_ty = rt
    return new_fn_type(ty), fn_scope, false
}
// always allocates a new TypeId, never dedupes: function types are nominal,
// not structural (distinct decls with identical signatures stay distinct)
new_fn_type :: proc(t: Type) -> TypeId {
    assert(t.kind == .Function)
    append(&get_ctx().types, t)
    return TypeId(len(get_ctx().types)-1)
}
resolve_fn_call :: proc(s: ^Scope, id: ExprId) {
    f := get_expr(id).(FnCall);

    f.type_arg_ids = make([]TypeId, len(f.type_args))
    for a, i in f.type_args {
        t := resolve_type_specifier(s, a, allow_generic_dec=false);
        f.type_arg_ids[i] = t
    }

    resolve_expr(s, f.target);

    for a in f.args {
        resolve_expr(s, a.expr);
    }
    get_ctx().exprs[id] = f; // update cus type arg ids
}
resolve_expr :: proc(s: ^Scope, id: ExprId) {
    ctx := get_ctx();
    #partial switch i in get_expr(id) {
    case UnNegative: {
        resolve_expr(s, i.expr)
    }
    case UnNot: {
        resolve_expr(s, i.expr)
    }
    case Symbol: {
        n, ok := scope_get_object(s, i.name)
        if !ok {
            highlight_lines(get_span(id))
            gala_panicf("Symbol \"{}\" doesn't exist.", i.name)
        }
        ctx.expr_objects[id] = n
    }
    case AggregateLit: {
        if len(i.name) == 0 {
            // struct lit - infer
        } else {
            t := lookup_type(s, i.name, get_span(id));
            ctx.expr_types[id] = t
        }
        if len(i.fields) != 0 && len(i.positional) != 0 {
            highlight_lines(get_span(id));
            gala_panicf("Can't have both positional and named fields in aggregate literal.");
        }
        if len(i.fields) != 0 {
            declared := make(map[string]struct{span: Span});
            for n, f  in i.fields {
                if s, ok := declared[n]; ok {
                    highlight_lines(f.span);
                    highlight_lines(s.span);
                    gala_panicf("Duplicate name \"{}\" in struct dec. First declared here.", n);
                }
                declared[n] = { f.span }
                resolve_expr(s, f.expr)
            }
        } else {
            for e in i.positional {
                resolve_expr(s, e.expr)
            }
        }
    }
    case Binop: {
        resolve_expr(s, i.left)
        resolve_expr(s, i.right)
    }
    case Cast: {
        resolve_expr(s, i.target);
        t := resolve_type_specifier(s, i.to, allow_generic_dec = false);
        ctx.expr_types[id] = t;
    }
    case Transmute: {
        resolve_expr(s, i.target);
        t := resolve_type_specifier(s, i.to, allow_generic_dec = false);
        ctx.expr_types[id] = t;
    }
    case FnCall: resolve_fn_call(s, id)
    case Number, String: {}
    case Reference: {
        resolve_expr(s, i.expr)
    }
    case Deref: {
        resolve_expr(s, i.expr)
    }
    case Index: {
        resolve_expr(s, i.target)
        resolve_expr(s, i.index)
    }
    case Len: {
        resolve_expr(s, i.target)
    }
    // copied from old galayalang
    case SizeOf: {
        // written with type-only syntax (`^T`, `[]T`, `fn(..)`)
        if spec, is_ty := i.t.(TypeSpecifier); is_ty {
            get_ctx().expr_types[id] = resolve_type_specifier(s, spec)
            return
        }

        inner := i.expr.(ExprId)
        node := get_expr(id).(SizeOf)
        node.names_type = false

        // `sizeof(Box(i32))` parses as a call; a generic callee means it's a type
        if call, is_call := get_expr(inner).(FnCall); is_call {
            if sym, is_sym := get_expr(call.target).(Symbol); is_sym {
                if tid, is_ty := scope_find_type(s, sym.name); is_ty &&
                   get_type(tid).kind == .Struct && get_type(tid).structure.is_generic {

                    if spec, ok := expr_as_type_spec(inner); ok {
                        node.t = spec
                        get_expr(id)^ = node
                        ctx.expr_types[id] = resolve_type_specifier(s, spec)
                        return
                    }
                }
            }
        }

        if tid, is_ty := bare_name_as_type(s, inner, get_span(id)); is_ty {
            node.names_type = true
            ctx.expr_types[id] = tid
        }
        get_expr(id)^ = node

        if !node.names_type do resolve_expr(s, inner)
    }
    case FieldAccess: {
        resolve_expr(s, i.target)
    }
    case TypeIdOf: {
        // written with type-only syntax (`^T`, `[]T`, `fn(..)`)
        if spec, is_ty := i.t.(TypeSpecifier); is_ty {
            get_ctx().expr_types[id] = resolve_type_specifier(s, spec)
            return
        }

        inner := i.expr.(ExprId)
        node := get_expr(id).(TypeIdOf)
        node.names_type = false

        // `sizeof(Box(i32))` parses as a call; a generic callee means it's a type
        if call, is_call := get_expr(inner).(FnCall); is_call {
            if sym, is_sym := get_expr(call.target).(Symbol); is_sym {
                if tid, is_ty := scope_find_type(s, sym.name); is_ty &&
                   get_type(tid).kind == .Struct && get_type(tid).structure.is_generic {

                    if spec, ok := expr_as_type_spec(inner); ok {
                        node.t = spec
                        get_expr(id)^ = node
                        ctx.expr_types[id] = resolve_type_specifier(s, spec)
                        return
                    }
                }
            }
        }

        if tid, is_ty := bare_name_as_type(s, inner, get_span(id)); is_ty {
            node.names_type = true
            ctx.expr_types[id] = tid
        }
        get_expr(id)^ = node

        if !node.names_type do resolve_expr(s, inner)
    }
    case TypeInfoOf: {
        resolve_expr(s, i.expr)

        tid, found := scope_find_type(s, "TypeInfo")
        if !found || get_type(tid).kind != .Struct {
            highlight_lines(get_span(id))
            gala_panic("`type_info` needs the struct `TypeInfo` in scope (import core).")
        }
        get_ctx().expr_types[id] = intern_type(Type{kind = .Pointer, ptr = tid})
    }
    case BoolLitFalse, BoolLitTrue: {}
    // copied from old galayalang
    case FixedSizeArray: {
        // `[MAX_COUNT]byte{}`: fold the constant size into `size`
        if se, has_size_expr := i.size_expr.(ExprId); has_size_expr {
            node := get_expr(id).(FixedSizeArray)
            // node.size = const_array_size(s, se)
            i, _ := parse_integer_literal(get(se).(Number).text)
            node.size = int(i)
            node.size_expr = nil
            if len(node.initialiser) > node.size {
                highlight_lines(get_span(id))
                gala_panicf("Too many elements in array initialiser: the array holds %d, got %d.",
                    node.size, len(node.initialiser))
            }
            get_expr(id)^ = node
        }
        get_ctx().expr_types[id] = resolve_type_specifier(s, i.ty, false)
        for el in i.initialiser do resolve_expr(s, el)
    }
    // copied from old galayalang
    case TakeSlice: {
        resolve_expr(s, i.target)
        if !i.empty_start do resolve_expr(s, i.start)
        if !i.empty_end do   resolve_expr(s, i.end)
    }
    case: debugln(i); panic("Impl")
    }
}
// copied from old galayalang
expr_as_type_spec :: proc(id: ExprId) -> (TypeSpecifier, bool) {
    #partial switch e in get_expr(id) {
    case Symbol:
        return TypeSpecifier(BaseType{e.name, get_span(id)}), true
    case FnCall:
        if len(e.type_args) > 0 do return {}, false // `f::<T>(..)` is never a type
        sym, is_sym := get_expr(e.target).(Symbol)
        if !is_sym do return {}, false
        args := make([dynamic]TypeSpecifier)
        for a in e.args {
            if len(a.name) > 0 do return {}, false
            spec, ok := expr_as_type_spec(a.expr)
            if !ok do return {}, false
            append(&args, spec)
        }
        return TypeSpecifier(GenericInstSpecifier{name=sym.name, args=args, span=get_span(id)}), true
    }
    return {}, false
}
// copied from old galayalang
// If `inner` is a bare name that resolves to a type, returns that type.
bare_name_as_type :: proc(s: ^Scope, inner: ExprId, span: Span) -> (TypeId, bool) {
    sym, is_sym := get_expr(inner).(Symbol)
    if !is_sym do return 0, false
    tid, is_ty := scope_find_type(s, sym.name)
    if !is_ty do return 0, false
    reject_bare_generic(tid, span)

    return tid, true
}
// copied from old galayalang
reject_bare_generic :: proc(tid: TypeId, span: Span) {
    ty := get_type(tid)
    if ty.kind == .Struct && ty.structure.is_generic {
        highlight_lines(span)
        gala_panicf("Generic struct \"%s\" needs type arguments, e.g. %s(i64).",
            ty.name, ty.name)
    }
}

resolve_stmt :: proc(s: ^Scope, id: StmtId) {
    ctx := get_ctx();
    #partial switch i in get_stmt(id) {
    case VarDec: {
        o := Object { kind=.Variable, name=i.name, read_only=.No };
        if ts, ok := i.type.(TypeSpecifier); ok {
            o.type = resolve_type_specifier(s, ts, allow_generic_dec=false);
        }
        resolve_expr(s, i.value);
        oid := new_object(s, o, get_span(id));
        ctx.stmt_objects[id] = oid
    }
    case Assignment: {
        resolve_expr(s, i.target);
        resolve_expr(s, i.value);
    }
    case IfElse: {
        // smth smth smth
        base_branch_scope := new_scope(s)
        resolve_expr(&base_branch_scope, i.base_cond)
        for s in i.base_block.stmts do resolve_stmt(&base_branch_scope, s)

        for a in i.alt {
            branch_scope := new_scope(s)
            resolve_expr(&branch_scope, a.cond);
            for s  in a.block.stmts do resolve_stmt(&branch_scope, s);
        }

        if i.has_else_block {
            branch_scope := new_scope(s)
            for s in i.else_block.stmts do resolve_stmt(&branch_scope, s);
        }
    }
    case ForLoop: {
        resolve_expr(s, i.expr)
        loop_scope := new_scope(s)
        for n, index in i.names {
            i.names[index].oid = new_object(&loop_scope, {kind=.Variable, read_only=.LoopVar, name=n.name}, n.span)
        }
        for s in i.block.stmts do resolve_stmt(&loop_scope, s);
    }
    case WhileLoop: {
        resolve_expr(s, i.cond)
        loop_scope := new_scope(s)
        for s in i.block.stmts do resolve_stmt(&loop_scope, s);

    }
    case Return: {
        if e, ok := i.expr.(ExprId); ok do resolve_expr(s, e)
    }
    case ExprId: resolve_expr(s, i)
    case: debugln(i); panic("Impl")
    }
}
resolve_items :: proc(s: ^ModuleScope, items: []ItemId) {
    ctx := get_ctx();
    for id in items {
        item := get_item(id)
        #partial switch i in item {
        case FnDec: {
            // resolve args, first, resolve return type, resolve body
            oid, ok := ctx.item_objects[id]; assert(ok);
            tyid, fn_scope, is_generic := resolve_function_signature(s, i.signature, id)
            get_obj(oid).type = tyid
            if is_generic { 
                // next one
                // resolve and typecheck again after everything
                continue;
            }
            for stmt in i.block.stmts {
                resolve_stmt(&fn_scope, stmt)
            }
            delete_key(&s.obj_forward, i.name)
            s.objects[i.name] = oid
        }
        case ExternFnDec: {
            // resolve args, first, resolve return type, resolve body
            oid, ok := ctx.item_objects[id]; assert(ok);
            tyid, fn_scope, is_generic := resolve_function_signature(s, i.signature, id, is_extern=true)
            get_obj(oid).type = tyid
            if is_generic { 
                // next one
                // resolve and typecheck again after everything
                panic("what")
                // continue;
            }
            // no body
            delete_key(&s.obj_forward, i.name)
            s.objects[i.name] = oid
        }
        case StructDec: {
            tid, ok := ctx.item_types[id]; assert(ok);
            assert(len(i.generics) == 0);
            assert(len(i.fields) > 0);
            declared := make(map[string]struct{span:Span});
            fields := make([]Field, len(i.fields));
            for a, i in i.fields {
                if loc, ok := declared[a.name]; ok {
                    highlight_lines(a.span);
                    highlight_lines(loc.span);
                    gala_panicf("Duplicate name \"{}\" in struct dec. First declared here.", a.name);
                }
                declared[a.name] = {a.span}
                ty := resolve_type_specifier(s, a.t);
                fields[i] = { a.name, ty }
            }
            st := Type{}
            st.kind = .Struct
            st.name = i.name
            st.structure.fields = fields
            st.structure.is_anonymous = false
            get_type(tid)^ = st
            delete_key(&s.ty_forward, i.name)
            s.types[i.name] = tid
        }
        case GlobalVarDec: {
            oid, ok := ctx.item_objects[id]; assert(ok);
            o := get_obj(oid)
            if ts, ok := i.type.(TypeSpecifier); ok {
                o.type = resolve_type_specifier(s, ts, allow_generic_dec=false);
            }
            resolve_expr(s, i.value);
            ctx.item_objects[id] = oid
        }
        case Import: {}
        case TypeAlias: {
            s.ty_exports[i.new_name] = resolve_alias(s, id)
        }
        case: panic("impl")
        }
    }
}
resolve_module_ast :: proc(ast: ^AST, path: string) -> ModId {
    prev_aliases := alias_active
    alias_active = make(map[ItemId]bool)
    module := Module{}
    decs := new_module_scope(&get_ctx().base_mod);
    append(&get_ctx().mods, Module{decs, ast, path});
    mid := ModId(len(get_ctx().mods) - 1)
    p := current_module_id
    current_module_id = mid;
    
    // 1. forward items
    forward_items(&decs, ast.items[:]);

    // 2. resolve items
    resolve_items(&decs, ast.items[:])

    get_ctx().mods[mid] = Module{decs, ast, path}

    current_module_id = p
    delete(alias_active)
    alias_active = prev_aliases
    return mid
}
