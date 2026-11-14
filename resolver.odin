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
        id, ok := scope.types[n]
        if ok { return id, true }
        // check fds too
        id, ok = scope.ty_forward[n]
        if ok { return id, true }
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
    assert(t.kind == .Pointer ||
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
        case StructDec: {
            tid := new_type_fd(s, {kind=.Struct, name=i.name}, get_span(id))
            ctx.type_modules[tid] = current_module_id;
            ctx.item_types[id] = tid;
            s.ty_exports[i.name] = tid;
        }
        case: debugln(id); panic("impl")
        }
    }
}

resolve_type_specifier :: proc(s: ^Scope, t: TypeSpecifier, allow_generics := true) -> TypeId {
    #partial switch k in t {
    case GenericSpecifier: {
        if ! allow_generics {
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
            panic("impl")
            // size = const_array_size(s, se)
        }
        return intern_type({kind=.FixedSizeArray,
            fixed_size_array={type=id, size=size}})
    }
    case: panic("impl")
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
FnArg :: struct {
    name: string,
    ty: TypeId,
}
FnGenericArg :: struct {
    name: string,
    ty: TypeId,
}
FunctionType :: struct {
    args: []FnArg,
    type_args: []FnArg, // name (if needed) and generic type id
    ret_ty: TypeId,
    // wether it's a generic template - used for compile time
    is_generic: bool,
    generic: struct {
        template_id: int,
        generic_args: []FnGenericArg,
        // bindings stuff. at resolution, get a vague idea of what its signature looks like
    }
}
// 1. resolve type parameters first
// 2. resolve args in order
// 3. resolve return type
resolve_generic_function_signature :: proc(s: ^Scope, sig: FnDecSignature, id: ItemId) -> (TypeId, Scope, bool) {
    assert(!sig.is_variadic)
    ty := Type{kind=.Function};

    fn_scope := new_scope(s);
    ty_args := make([]FnArg, len(sig.generics))
    for a, i in sig.generics {
        t := new_generic_param(&fn_scope, a); // defines new args. auto panics
        ty_args[i] = {a.name, t}
    }
    ty.fn.type_args = ty_args

    args := make([]FnArg, len(sig.args))
    for a, i in sig.args {
        assert(!a.inferred);
        assert(a.default == nil)
        assert(a.default_src == nil)
        t := resolve_type_specifier(&fn_scope, a.t);
        o := Object{kind=.Argument, read_only=.Argument, name=a.name, type=t};
        oid := new_object(&fn_scope, o, a.span); // auto panics on duplicate name
        args[i].name = a.name
        args[i].ty = t
    }
    ty.fn.args = args;
    rt := ty_from_name("void");
    if sig.ret_ty != nil {
        rt = resolve_type_specifier(&fn_scope, sig.ret_ty.(TypeSpecifier))
    }
    ty.fn.ret_ty = rt
    ty.fn.is_generic = true // hehe. yea
    tid := new_fn_type(ty)

    get_ctx().fn_templates[tid] = { id, make([dynamic]struct{ ty_args, args: []TypeId, ty_id: TypeId,}) }

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
        for arg in k.args {
            if ts_is_generic(arg.t) do return true
        }
        if ret, ok := k.ret_ty.?; ok {
            return ts_is_generic(ret)
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
    for a in sig.args {
        if ts_is_generic(a.t) do return true
    }
    // can't do that
    if ts, ok := sig.ret_ty.(TypeSpecifier); ok {
        if ts_is_generic(ts) {
            highlight_lines(type_specifier_span(ts));
            gala_panicf("Can't specify Generic Type in return type.")
        }
    }
    return false
}
resolve_function_signature :: proc(s: ^Scope, sig: FnDecSignature, id: ItemId) -> (TypeId, Scope, bool) {
    assert(!sig.is_variadic)
    ty := Type{kind=.Function};

    if signature_is_generic(sig) { return resolve_generic_function_signature(s, sig, id) }

    fn_scope := new_scope(s);
    for a in sig.generics {
        new_generic_param(&fn_scope, a)
    }

    args := make([]FnArg, len(sig.args))
    for a, i in sig.args {
        assert(!a.inferred);
        assert(a.default == nil)
        assert(a.default_src == nil)
        t := resolve_type_specifier(&fn_scope, a.t);
        o := Object{kind=.Argument, read_only=.Argument, name=a.name, type=t};
        oid := new_object(&fn_scope, o, a.span); // auto panics on duplicate name
        args[i].name = a.name
        args[i].ty = t
    }
    ty.fn.args = args;
    rt := ty_from_name("void");
    if sig.ret_ty != nil {
        rt = resolve_type_specifier(&fn_scope, sig.ret_ty.(TypeSpecifier))
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
        t := resolve_type_specifier(s, a, allow_generics=false);
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
        t := resolve_type_specifier(s, i.to, allow_generics = false);
        ctx.expr_types[id] = t;
    }
    case Transmute: {
        resolve_expr(s, i.target);
        t := resolve_type_specifier(s, i.to, allow_generics = false);
        ctx.expr_types[id] = t;
    }
    case FnCall: resolve_fn_call(s, id)
    case Number, String: {}
    case: debugln(i); panic("Impl")
    }
}
resolve_stmt :: proc(s: ^Scope, id: StmtId) {
    ctx := get_ctx();
    #partial switch i in get_stmt(id) {
    case VarDec: {
        o := Object { kind=.Variable, name=i.name, read_only=.No };
        if ts, ok := i.type.(TypeSpecifier); ok {
            o.type = resolve_type_specifier(s, ts, allow_generics=false);
        }
        resolve_expr(s, i.value);
        oid := new_object(s, o, get_span(id));
        ctx.stmt_objects[id] = oid
    }
    case Return: {
        if e, ok := i.expr.(ExprId); ok do resolve_expr(s, e)
    }
    case: debugln(i); panic("Impl")
    }
}
resolve_items :: proc(s: ^Scope, items: []ItemId) {
    ctx := get_ctx();
    for id in items {
        item := get_item(id)
        #partial switch i in item {
        case FnDec: {
            // resolve args, first, resolve return type, resolve body
            obj, ok := ctx.item_objects[id]; assert(ok);
            tyid, fn_scope, is_generic := resolve_function_signature(s, i.signature, id)
            get_obj(obj).type = tyid
            if is_generic { 
                // next one
                // resolve and typecheck again after everything
                debugln("skipping generic fn:", i.name);
                continue;
            }
            for stmt in i.block.stmts {
                resolve_stmt(&fn_scope, stmt)
            }
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
        }
        case: panic("impl")
        }
    }
}
resolve_module_ast :: proc(ast: ^AST, path: string) -> ModId {
    module := Module{}
    decs := new_module_scope(&get_ctx().base_mod);
    append(&get_ctx().mods, Module{decs, ast, path});
    p := current_module_id
    mid := ModId(len(get_ctx().mods) - 1)
    current_module_id = mid;
    
    // 1. forward items
    forward_items(&decs, ast.items[:]);

    // 2. resolve items
    resolve_items(&decs, ast.items[:])

    current_module_id = p
    return mid
}
