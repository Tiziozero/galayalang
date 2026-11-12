package main

import "core:fmt"
import "core:slice"
import "core:strings"

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
add_object :: proc(o: Object, span: SpanStruct) -> ObjId {
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
add_type :: proc(t: Type, span: SpanStruct) -> TypeId {
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
check_undeclared :: proc(s: ^Scope, name: string, span: SpanStruct) {
    if !name_exists(s, name, true) do return
    highlight_lines(span)
    gala_panicf("\"%s\" is already declared (shadowing isn't allowed).", name)
}

new_object :: proc(s: ^Scope, o: Object, span: SpanStruct) -> ObjId {
    check_undeclared(s, o.name, span)
    id := add_object(o, span)
    s.objects[o.name] = id
    return id
}

new_type :: proc(s: ^Scope, t: Type, span: SpanStruct) -> TypeId {
    check_undeclared(s, t.name, span)
    id := add_type(t, span)
    s.types[t.name] = id
    return id
}

new_object_fd :: proc(s: ^ModuleScope, o: Object, span: SpanStruct) -> ObjId {
    check_undeclared(s, o.name, span)
    id := add_object(o, span)
    s.obj_forward[o.name] = id
    return id
}

new_type_fd :: proc(s: ^ModuleScope, t: Type, span: SpanStruct) -> TypeId {
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
    highlight_lines({current_file, span})
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
    highlight_lines({current_file, first})
    highlight_lines({current_file, dup})
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
            oid := new_object_fd(s, {kind=.Item, name=i.name, type=nil, read_only=.Function}, {current_file, i.span});
            // set references
            ctx.object_modules[oid] = current_module_id;
            ctx.object_decs[oid] = {current_file, i.span};
            ctx.item_objects[id] = oid;
             // export symbol
            s.obj_exports[i.name] = oid;
        }
        case: panic("impl")
        }
    }
}

resolve_type_specifier :: proc(s: ^Scope, t: TypeSpecifier, allow_generics := true) -> TypeId {
    #partial switch k in t {
    case GenericSpecifier: {
        fmt.println(t);
        if ! allow_generics {
            highlight_lines({current_file, k.span});
            gala_panic("can't specify generic item here")
        }
        panic("impl")
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
}

new_generic_param :: proc(s: ^Scope, a: GenericParam) -> TypeId{
    check_undeclared(s, a.name, {current_file, a.span})
    t := Type{kind=.Generic, name=a.name};
    return new_type(s, t, {current_file, a.span})
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
FunctionType :: struct {
    args: []FnArg,
    ret_ty: TypeId
}
resolve_function_signature :: proc(s: ^Scope, sig: FnDecSignature) -> (TypeId, Scope) {
    assert(len(sig.generics) == 0)
    assert(!sig.is_variadic)
    fn_scope := new_scope(s);
    ty := Type{kind=.Function};
    for a in sig.generics {
        new_generic_param(&fn_scope, a)
    }
    args := make([]FnArg, len(sig.args))
    for a in sig.args {
        assert(!a.inferred);
        assert(a.default == nil)
        assert(a.default_src == nil)
        t := resolve_type_specifier(&fn_scope, a.t);
        o := Object{kind=.Argument, read_only=.Argument, name=a.name, type=t};
        oid := new_object(&fn_scope, o, {current_file, a.span});
    }
    ty.fn.args = args;
    rt := ty_from_name("void");
    if sig.ret_ty != nil {
        rt = resolve_type_specifier(&fn_scope, sig.ret_ty.(TypeSpecifier))
    }
    ty.fn.ret_ty = rt
    return new_fn_type(ty), fn_scope
}
// always allocates a new TypeId, never dedupes: function types are nominal,
// not structural (distinct decls with identical signatures stay distinct)
new_fn_type :: proc(t: Type) -> TypeId {
    assert(t.kind == .Function)
    append(&get_ctx().types, t)
    return TypeId(len(get_ctx().types)-1)
}
resolve_stmt :: proc(s: ^Scope, id: StmtId) {
    ctx := get_ctx();
    #partial switch i in get_stmt(id) {
    case VarDec: {
        o := Object { kind=.Variable, name=i.name };
        if ts, ok := i.type.(TypeSpecifier); ok {
            t := resolve_type_specifier(s, ts, allow_generics=false);
        }
        
    }
    case: panic("Impl")
    }
}
resolve_items :: proc(s: ^Scope, items: []ItemId) {
    ctx := get_ctx();
    for id in items {
        item := get_item(id)
        #partial switch i in item {
        case FnDec: {
            obj, ok := ctx.item_objects[id]; assert(ok);
            tyid, fn_scope := resolve_function_signature(s, i.signature)
            get_obj(obj).type = tyid
            for stmt in i.block.stmts {
                resolve_stmt(&fn_scope, stmt)
            }
            // resolve args, first, resolve return type, resolve body
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