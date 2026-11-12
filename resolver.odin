package main

import "core:fmt"
import "core:slice"
import "core:strings"

Field :: struct {
    name: string,
    type: TypeId,
    span: Span,
}

// `default` is the resolved call to the hidden default function used when a
// call omits this argument.
//
// For `name := expr` parameters `inferred` is set: `type` is a placeholder
// (void) until tc_default_args infers it from `default_src`, and `obj` lets
// the parameter's object be patched too.
Arg :: struct {
    name: string,
    type: TypeId,
    span: Span,
    is_variadic: bool,
    default: Maybe(ExprId),
    inferred: bool,
    default_src: Maybe(ExprId),
    obj: ObjId,
}

ObjectKind :: enum {
    Invalid,
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

// ---------------------------------------------------------------------------
// Declarations
// ---------------------------------------------------------------------------

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

// Names share one namespace and can't shadow, so parents are checked too.
check_undeclared :: proc(s: ^Scope, name: string, span: SpanStruct) {
    if !name_exists(s, name, true) do return
    highlight_lines(span)
    gala_panicf("\"%s\" is already declared (shadowing isn't allowed).", name)
}

@(private="file")
add_object :: proc(o: Object, span: SpanStruct) -> ObjId {
    ctx := get_ctx()
    assert(o.kind != .Invalid)
    assert(len(o.name) > 0)
    append(&ctx.objs, o)
    id := ObjId(len(ctx.objs) - 1)
    ctx.obj_modules[id] = ctx.current_module_id
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
    ctx.ty_modules[id] = ctx.current_module_id
    ctx.spans.ty_decs[id] = span
    return id
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

// Function literals don't capture: their body resolves against the module
// scope, so using a local of the surrounding function is a resolve error.
// Only module scopes have their forward maps allocated (see
// new_module_scope), which is how one is found from any nested scope.
enclosing_module_scope :: proc(s: ^Scope) -> ^Scope {
    sc := s
    for sc != nil && sc.obj_forward == nil {
        sc = sc.parent
    }
    if sc == nil do return s // shouldn't happen
    return sc
}

// `Box(i32)` in expression position parses as a call to `Box`. When the
// callee names a generic struct, turn it back into a type specifier. Only
// plain names (or the same shape again) work; `^`/`[]` args were already
// parsed as types.
expr_as_type_spec :: proc(id: ExprId) -> (TypeSpecifier, bool) {
    #partial switch e in get_expr(id) {
    case Symbol:
        return TypeSpecifier(BaseType{e.name, get_span(id).span}), true
    case FnCall:
        if len(e.type_args) > 0 do return {}, false // `f::<T>(..)` is never a type
        sym, is_sym := get_expr(e.target).(Symbol)
        if !is_sym do return {}, false
        args := make([dynamic]TypeSpecifier, allocator=get_ctx().allocator)
        for a in e.args {
            if len(a.name) > 0 do return {}, false
            spec, ok := expr_as_type_spec(a.expr)
            if !ok do return {}, false
            append(&args, spec)
        }
        return TypeSpecifier(GenericInstSpecifier{name=sym.name, args=args, span=get_span(id).span}), true
    }
    return {}, false
}

// ---------------------------------------------------------------------------
// Lookups and "did you mean" suggestions
// ---------------------------------------------------------------------------

edit_distance :: proc(a, b: string) -> int {
    if len(a) == 0 do return len(b)
    if len(b) == 0 do return len(a)
    prev := make([]int, len(b) + 1, context.temp_allocator)
    cur  := make([]int, len(b) + 1, context.temp_allocator)
    for j in 0..=len(b) do prev[j] = j
    for i in 1..=len(a) {
        cur[0] = i
        for j in 1..=len(b) {
            cost := 0 if a[i-1] == b[j-1] else 1
            cur[j] = min(prev[j] + 1, cur[j-1] + 1, prev[j-1] + cost)
        }
        prev, cur = cur, prev
    }
    return prev[len(b)]
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

alias_active: map[ItemId]bool

// The number of generic parameters written in `struct Name($G, $H)`.
// Remaining parameters are inferred from aggregate literals.
explicit_arg_count :: proc(tmpl_id: TypeId) -> int {
    return get_type(tmpl_id).structure.explicit_generic_count
}

// Resolves the written args of `Name(a, b)`. Accepts the explicit count or
// the full count; missing trailing generics become placeholders.
resolve_inst_args :: proc(s: ^Scope, k: GenericInstSpecifier, tmpl_id: TypeId,
        no_declare_generic := false, bindings: map[string]TypeId = nil) -> []TypeId {
    names := get_type(tmpl_id).structure.generic_names
    explicit := explicit_arg_count(tmpl_id)
    if len(k.args) != len(names) && len(k.args) != explicit {
        highlight_lines({get_ctx().current_file, k.span})
        gala_panicf("\"%s\" takes %d type argument(s), got %d.",
            k.name, explicit, len(k.args))
    }
    args := make([]TypeId, len(names), allocator=get_ctx().allocator)
    for a, i in k.args {
        args[i] = resolve_type_specifier(s, a, no_declare_generic, bindings)
        if args[i] == void_type() {
            highlight_lines({get_ctx().current_file, k.span})
            gala_panicf("\"void\" can't be a type argument of \"%s\".", k.name)
        }
    }
    params := get_type(tmpl_id).structure.generic_params
    assert(len(params) == len(names))
    for i in len(k.args)..<len(names) {
        // Reuse this template's own generic parameter, never a global
        // name-based placeholder shared with an unrelated struct.
        args[i] = params[i]
    }
    return args
}

// Resolved on first use, like ensure_const for constants.
resolve_alias :: proc(s: ^Scope, id: ItemId) -> TypeId {
    a := get(id).(TypeAlias)
    if t, done := s.types[a.new_name]; done do return t
    if alias_active[id] {
        highlight_lines(get_span(id))
        gala_panicf("Type alias \"%s\" is cyclic.", a.new_name)
    }
    if specifier_has_generic(a.ty) {
        highlight_lines(get_span(id))
        gala_panic("Can't have generics in type aliases.")
    }
    alias_active[id] = true
    // no_declare_generic: an alias has nowhere to bind a `$T`
    t := resolve_type_specifier(s, a.ty, no_declare_generic = true)
    delete_key(&alias_active, id)
    delete_key(&s.ty_pending, a.new_name)
    s.types[a.new_name] = t
    return t
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
    highlight_lines({get_ctx().current_file, span})
    gala_panicf("Type \"%s\" doesn't exist.", name)
}

reject_bare_generic :: proc(tid: TypeId, span: SpanStruct) {
    ty := get_type(tid)
    if ty.kind == .Struct && ty.structure.is_generic {
        highlight_lines(span)
        gala_panicf("Generic struct \"%s\" needs type arguments, e.g. %s(i64).",
            ty.name, ty.name)
    }
}

// If `inner` is a bare name that resolves to a type, returns that type.
bare_name_as_type :: proc(s: ^Scope, inner: ExprId, span: SpanStruct) -> (TypeId, bool) {
    sym, is_sym := get_expr(inner).(Symbol)
    if !is_sym do return 0, false
    tid, is_ty := scope_find_type(s, sym.name)
    if !is_ty do return 0, false
    reject_bare_generic(tid, span)
    return tid, true
}

// ---------------------------------------------------------------------------
// Expressions
// ---------------------------------------------------------------------------

resolve_expr :: proc(s: ^Scope, id: ExprId) {
    switch e in get(id) {
    case TypeInfoOf: {
        resolve_expr(s, e.expr)

        tid, found := scope_find_type(s, "TypeInfo")
        if !found || get_type(tid).kind != .Struct {
            highlight_lines(get_span(id))
            gala_panic("`type_info` needs the struct `TypeInfo` in scope (import core).")
        }
        get_ctx().expr_resolution_types[id] = intern_type(Type{kind = .Pointer, ptr = tid})
    }
    case TypeIdOf: {
        // written with type-only syntax (`^T`, `[]T`, `fn(..)`)
        if spec, is_ty := e.t.(TypeSpecifier); is_ty {
            get_ctx().expr_resolution_types[id] = resolve_type_specifier(s, spec)
            return
        }

        inner := e.expr.(ExprId)
        node := get_expr(id).(TypeIdOf)
        node.names_type = false
        // a bare name is a type if one is in scope, otherwise a variable
        // (no shadowing, so a name can't be both)
        if tid, is_ty := bare_name_as_type(s, inner, get_span(id)); is_ty {
            node.names_type = true
            get_ctx().expr_resolution_types[id] = tid
        }
        get_expr(id)^ = node

        // a type name has nothing to resolve; anything else is a real expression
        if !node.names_type do resolve_expr(s, inner)
    }
    case UnNegative: {
        resolve_expr(s, e.expr)
    }
    case UnNot: {
        resolve_expr(s, e.expr)
    }
    case BoolLitTrue: {
    }
    case BoolLitFalse: {
    }
    case Len: {
        resolve_expr(s, e.target)
    }
    case SizeOf: {
        // written with type-only syntax (`^T`, `[]T`, `fn(..)`)
        if spec, is_ty := e.t.(TypeSpecifier); is_ty {
            get_ctx().expr_resolution_types[id] = resolve_type_specifier(s, spec)
            return
        }

        inner := e.expr.(ExprId)
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
                        get_ctx().expr_resolution_types[id] = resolve_type_specifier(s, spec)
                        return
                    }
                }
            }
        }

        if tid, is_ty := bare_name_as_type(s, inner, get_span(id)); is_ty {
            node.names_type = true
            get_ctx().expr_resolution_types[id] = tid
        }
        get_expr(id)^ = node

        if !node.names_type do resolve_expr(s, inner)
    }
    case String: // ok
    case Deref: {
        resolve_expr(s, e.expr)
    }
    case Reference: {
        resolve_expr(s, e.expr)
    }
    case TakeSlice: {
        resolve_expr(s, e.target)
        if !e.empty_start do resolve_expr(s, e.start)
        if !e.empty_end do   resolve_expr(s, e.end)
    }
    case Index: {
        resolve_expr(s, e.target)
        resolve_expr(s, e.index)
    }
    case FixedSizeArray: {
        // `[MAX_COUNT]byte{}`: fold the constant size into `size`
        if se, has_size_expr := e.size_expr.(ExprId); has_size_expr {
            node := get_expr(id).(FixedSizeArray)
            node.size = const_array_size(s, se)
            node.size_expr = nil
            if len(node.initialiser) > node.size {
                highlight_lines(get_span(id))
                gala_panicf("Too many elements in array initialiser: the array holds %d, got %d.",
                    node.size, len(node.initialiser))
            }
            get_expr(id)^ = node
        }
        get_ctx().expr_resolution_types[id] = resolve_type_specifier(s, e.ty)
        for el in e.initialiser do resolve_expr(s, el)
    }
    case FieldAccess: {
        // field is checked in type checking
        resolve_expr(s, e.target)
    }
    case AggregateLit: {
        resolve_aggregate_lit(s, id)
    }
    case Transmute: {
        ty := resolve_type_specifier(s, e.to)
        resolve_expr(s, e.target)
        get_ctx().expr_resolution_types[id] = ty
    }
    case Cast: {
        ty := resolve_type_specifier(s, e.to)
        resolve_expr(s, e.target)
        get_ctx().expr_resolution_types[id] = ty
    }
    case Binop: {
        resolve_expr(s, e.left)
        resolve_expr(s, e.right)
    }
    case Number: {
    } // nothing
    case Symbol: {
        obj, ok := scope_get_object(s, e.name)
        if !ok {
            highlight_lines(get_span(id))
            gala_panicf("Couldn't find \"%s\" in scope.", e.name)
        }
        // `name :: value`: this use becomes the value itself
        if is_const_obj(obj) {
            inline_constant(id, obj)
            return
        }
        get_ctx().expr_objects[id] = obj
    }
    case FnCall: {
        resolve_expr(s, e.target)
        for a in e.args {
            resolve_expr(s, a.expr)
        }
        // `f::<A, B>(..)`: resolve the explicit type arguments
        if len(e.type_args) > 0 {
            ids := make([]TypeId, len(e.type_args), allocator=get_ctx().allocator)
            for spec, i in e.type_args {
                // no_declare_generic: `::<$T>` would declare one nobody can bind
                ids[i] = resolve_type_specifier(s, spec, no_declare_generic = true)
                if ids[i] == void_type() {
                    highlight_lines(get_ctx().current_file, e.type_args_span)
                    gala_panic("\"void\" can't be a type argument.")
                }
            }
            node := get_expr(id).(FnCall)
            node.type_arg_ids = ids
            get_expr(id)^ = node
        }
    }
    case FnLit: {
        // same as resolve_fn_dec_item minus the forward-declared object:
        // build the fn type, resolve the body in the signature's scope, and
        // stash the type for the type checker.
        fnty, fnscope := resolve_fn_dec_signature(enclosing_module_scope(s), e.signature)
        if fnty.fn.is_generic {
            free_scope(&fnscope)
            highlight_lines(get_span(id))
            gala_panic("Function literals can't be generic.")
        }

        body := e.block
        resolve_block(&fnscope, &body)
        free_scope(&fnscope)

        // not interned: function types are nominal (see new_fn_type)
        get_ctx().expr_resolution_types[id] = new_fn_type(fnty)
    }

    case: gala_panic("compiler bug: resolve_expr doesn't handle this expression kind")
    }
}

// Maps positional values onto fields, then reports unknown and missing
// fields. Shared by `Foo{..}` (resolver) and `{..}` (typechecker, once the
// target type is known). Doesn't resolve or check the values themselves.
bind_aggregate_lit_fields :: proc(id: ExprId, e: ^AggregateLit, tid: TypeId, type_name: string) {
    ty := get_type(tid)
    is_union := ty.kind == .Union
    sfields := ty.union_fields if is_union else ty.structure.fields

    // `Foo{1, 2}`: assign values to fields in declaration order, so
    // everything after this sees an ordinary named literal.
    if len(e.positional) > 0 {
        if is_union {
            highlight_lines(get_span(id))
            gala_panicf("A union literal sets one field by name, e.g. `{%s = ...}`.", sfields[0].name)
        }
        if len(e.positional) != len(sfields) {
            highlight_lines(get_span(id))
            gala_panicf("Type %s has %d fields, but the literal has %d values.",
                type_name, len(sfields), len(e.positional))
        }
        // the parser may not have allocated the map for a purely positional literal
        if e.fields == nil {
            e.fields = make(type_of(e.fields), allocator=get_ctx().allocator)
        }
        for sf, i in sfields {
            e.fields[sf.name] = e.positional[i]
        }
    }

    // unknown fields first, with a suggestion
    for name, f in e.fields {
        found := false
        best, best_d := "", max(2, len(name) / 3) + 1
        for sf in sfields {
            if sf.name == name do found = true
            if d := edit_distance(name, sf.name); d < best_d {
                best, best_d = sf.name, d
            }
        }
        if !found {
            highlight_lines(get_ctx().current_file, f.span)
            if len(best) > 0 {
                gala_panicf("Field \"%s\" doesn't exist in type %s. Did you mean \"%s\"?",
                    name, type_name, best)
            }
            gala_panicf("Field \"%s\" doesn't exist in type %s.", name, type_name)
        }
    }

    if is_union {
        // exactly one member may be set; the rest overlap it
        if len(e.fields) != 1 {
            highlight_lines(get_span(id))
            gala_panicf("A union literal sets exactly one field, got %d.", len(e.fields))
        }
        return
    }

    // then missing ones, by name
    missing := make([dynamic]string, allocator=context.temp_allocator)
    for sf in sfields {
        if sf.name not_in e.fields do append(&missing, sf.name)
    }
    if len(missing) > 0 {
        highlight_lines(get_span(id))
        gala_panicf("Literal of type %s is missing field(s): %s.",
            type_name, strings.join(missing[:], ", ", context.temp_allocator))
    }
}

resolve_aggregate_lit :: proc(s: ^Scope, id: ExprId) {
    e := get_expr(id).(AggregateLit)

    // `{...}`: nothing to look up yet, just resolve the values. Binding to
    // fields happens in the typechecker once the target type is known.
    if e.ty == nil {
        for _, f in e.fields do resolve_expr(s, f.expr)
        for f in e.positional do resolve_expr(s, f.expr)
        return
    }

    // `Foo`, `Foo(i64)`, or a generic struct named without arguments
    // (`Box{..}`): the latter keeps the template's id and the type checker
    // infers the instance.
    // `bind_tid` is the type whose fields are used to validate/map the
    // literal. `result_tid` is what the typechecker will infer from its values.
    tid: TypeId
    result_tid: TypeId
    inferred := false
    if bt, is_base := e.ty.(BaseType); is_base {
        if tmpl, found := scope_find_type(s, string(bt.ident)); found {
            tt := get_type(tmpl)
            if tt.kind == .Struct && tt.structure.is_generic {
                tid = tmpl
                result_tid = tmpl
                inferred = true
            }
        }
    }
    // `Name(A, B){..}` where trailing generic parameters are inferred.
    if gi, is_inst := e.ty.(GenericInstSpecifier); is_inst {
        if tmpl, found := scope_find_type(s, gi.name); found {
            tt := get_type(tmpl)
            if tt.kind == .Struct && tt.structure.is_generic &&
               len(gi.args) < len(tt.structure.generic_names) {
                args := resolve_inst_args(s, gi, tmpl)
                tid = tmpl
                result_tid = get_or_add_struct_instance(tmpl, args)
                inferred = true
            }
        }
    }
    if !inferred {
        tid = resolve_type_specifier(s, e.ty)
        result_tid = tid
    }

    ty := get_type(tid)
    if ty.kind != .Struct && ty.kind != .Union {
        highlight_lines(get_span(id))
        gala_panicf("\"%s\" is not a struct or union type.", e.name)
    }

    bind_aggregate_lit_fields(id, &e, tid, e.name)
    for _, f in e.fields do resolve_expr(s, f.expr)

    get_ctx().expr_resolution_types[id] = result_tid
    get_expr(id)^ = e
}

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

// always allocates a new TypeId, never dedupes: function types are nominal,
// not structural (distinct decls with identical signatures stay distinct)
new_fn_type :: proc(t: Type) -> TypeId {
    assert(t.kind == .Function)
    append(&get_ctx().types, t)
    return TypeId(len(get_ctx().types)-1)
}

// only interns pointers, slices, fixed arrays, strings, any and the untyped
// kinds; named structs and functions are nominal and never go through here
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

new_generic_ty :: proc(s: ^Scope, k: GenericSpecifier) -> TypeId {
    return new_type(s, Type{kind = .Generic, name = k.name},
        {get_ctx().current_file, k.span})
}

// `no_declare_generic` means "this position can't introduce a new `$T`":
// return types, explicit type arguments, aliases, anonymous struct fields and
// union fields. Generics already declared (by an earlier `$T`, or by the
// struct's/function's `($G, $H)` list) are plain names by then, so they
// resolve out of the scope like any other type.
resolve_type_specifier :: proc(s: ^Scope, t: TypeSpecifier, no_declare_generic := false,
        bindings: map[string]TypeId = nil) -> TypeId {
    switch k in t {
    case StructSpecifier: {
        fields := make([]Field, len(k.fields), allocator=get_ctx().allocator)
        for f, i in k.fields {
            for prev in fields[:i] {
                if prev.name == f.name {
                    highlight_lines({get_ctx().current_file, f.span})
                    gala_panicf("Duplicate field \"%s\".", f.name)
                }
            }
            // an anonymous struct has no name to hang a generic off
            ft := resolve_type_specifier(s, f.t, true, bindings)
            if ft == void_type() {
                highlight_lines({get_ctx().current_file, f.span})
                gala_panic("A struct field can't be void.")
            }
            fields[i] = {name=f.name, type=ft}
        }
        // interned: identical anonymous structs get the same TypeId
        return intern_type({kind=.Struct, structure={is_anonymous=true, fields=fields}})
    }
    case UnionSpecifier: {
        if len(k.fields) == 0 {
            highlight_lines({get_ctx().current_file, k.span})
            gala_panic("A union needs at least one field.")
        }
        fields := make([]Field, len(k.fields), allocator=get_ctx().allocator)
        for f, i in k.fields {
            for prev in fields[:i] {
                if prev.name == f.name {
                    highlight_lines({get_ctx().current_file, f.span})
                    gala_panicf("Duplicate field \"%s\".", f.name)
                }
            }
            // Only one member of a union is ever set, so a union can never be
            // where a `$T` gets inferred: no_declare_generic. Generics it uses
            // must already be in scope, e.g. `struct S($G) { f: $T, u: union { a: T, b: G } }`.
            ft := resolve_type_specifier(s, f.t, true, bindings)
            if ft == void_type() {
                highlight_lines({get_ctx().current_file, f.span})
                gala_panic("A union field can't be void.")
            }
            fields[i] = {name=f.name, type=ft}
        }
        return intern_type({kind=.Union, union_fields=fields})
    }
    case GenericSpecifier: {
        // instantiating a clone: declare the name as the concrete type
        if bindings != nil {
            if existing, ok := s.types[k.name]; ok do return existing
            tid, ok := bindings[k.name]
            if !ok {
                highlight_lines({get_ctx().current_file, k.span})
                gala_panicf("No binding for generic \"%s\".", k.name)
            }
            s.types[k.name] = tid
            return tid
        }
        if no_declare_generic {
            highlight_lines({get_ctx().current_file, k.span})
            gala_panicf("Can't declare a generic here. Declare `$%s` in a position that binds it " +
                "(a field or parameter) and write `%s` here.", k.name, k.name)
        }
        if name_exists(s, k.name) {
            highlight_lines({get_ctx().current_file, k.span})
            gala_panicf("Name \"%s\" already declared.", k.name)
        }
        return new_generic_ty(s, k)
    }
    case AnySpecifier: {
        return intern_type({kind=.Any})
    }
    case SliceSpecifier: {
        // no_declare_generic must reach the element type: `fn f(): []$T` is
        // as wrong as `fn f(): $T`
        base := resolve_type_specifier(s, k.base^, no_declare_generic, bindings)
        return intern_type({kind=.Slice, slice={type=base}})
    }
    case BaseType: { // will get declared type id
        ty := lookup_type(s, string(k.ident), k.span)
        reject_bare_generic(ty, {get_ctx().current_file, k.span})
        return ty
    }
    case GenericInstSpecifier: { // Name(args)
        tmpl_id := lookup_type(s, k.name, k.span)
        tmpl := get_type(tmpl_id)
        if tmpl.kind != .Struct || !tmpl.structure.is_generic {
            highlight_lines({get_ctx().current_file, k.span})
            gala_panicf("\"%s\" is not a generic struct.", k.name)
        }
        args := resolve_inst_args(s, k, tmpl_id, no_declare_generic, bindings)
        return get_or_add_struct_instance(tmpl_id, args)
    }
    case PointerType: {
        id := resolve_type_specifier(s, k.ptr^, no_declare_generic, bindings)
        return intern_type({kind=.Pointer, ptr=id})
    }
    case FixedArreySpecifier: {
        id := resolve_type_specifier(s, k.base^, no_declare_generic, bindings)
        size := k.size
        if se, has_size_expr := k.size_expr.(ExprId); has_size_expr {
            size = const_array_size(s, se)
        }
        return intern_type({kind=.FixedSizeArray,
            fixed_size_array={type=id, size=size}})
    }
    case FnSpecifier: {
        for a in k.signature.args {
            if a.default != nil {
                highlight_lines(get_ctx().current_file, a.span)
                gala_panic("Default values aren't allowed in function types.")
            }
        }

        fnty, fnscope := resolve_fn_dec_signature(s, k.signature^,
            no_declare_generic = no_declare_generic, bindings = bindings)

        // Generics declared inside the fn type (`$T`) belong to the enclosing
        // signature, so later params and the return type can use them.
        // (also covers the bindings != nil clone path, which binds into fnscope)
        for name, tid in fnscope.types {
            if _, exists := s.types[name]; !exists {
                s.types[name] = tid
            }
        }
        free_scope(&fnscope)

        // a function-typed *value* is never itself a generic function; its
        // generic-ness comes from the enclosing signature
        fnty.fn.is_generic = false
        return new_fn_type(fnty)
    }
    case: panic("impl")
    }
}

// How deep instances nest: `i32` is 0, `^i32` is 1, `Box(^i32)` is 2.
// Polymorphic recursion (`next: ^Bad(^T)`) makes this grow forever, so it
// needs a ceiling.
MAX_STRUCT_INSTANCE_DEPTH :: 32

type_nesting_depth :: proc(t: TypeId) -> int {
    ty := get_type(t)
    #partial switch ty.kind {
    case .Union: {
        deepest := 0
        for f in ty.union_fields {
            deepest = max(deepest, type_nesting_depth(f.type))
        }
        return 1 + deepest
    }
    case .Pointer:        return 1 + type_nesting_depth(ty.ptr)
    case .Slice:          return 1 + type_nesting_depth(ty.slice.type)
    case .FixedSizeArray: return 1 + type_nesting_depth(ty.fixed_size_array.type)
    case .Struct: {
        deepest := 0
        for a in ty.structure.args {
            deepest = max(deepest, type_nesting_depth(a))
        }
        return 1 + deepest
    }
    }
    return 0
}

// The one instance of generic struct `tmpl_id` for these args (made on first use).
get_or_add_struct_instance :: proc(tmpl_id: TypeId, args: []TypeId) -> TypeId {
    ctx := get_ctx()

    for inst in get_type(tmpl_id).structure.instances {
        same := len(inst.args) == len(args)
        for a, i in args {
            if !same do break
            if !types_equal(inst.args[i], a) do same = false
        }
        if same do return inst.type
    }

    // a new instance: refuse the ones only a runaway expansion asks for
    for a in args {
        if type_nesting_depth(a) > MAX_STRUCT_INSTANCE_DEPTH {
            highlight_lines(ctx.spans.ty_decs[tmpl_id])
            gala_panicf("Generic struct \"%s\" is instantiated with ever deeper types (polymorphic recursion?).",
                get_type(tmpl_id).name)
        }
    }

    tmpl := get_type(tmpl_id)^ // copy: the type table can move below
    // not intern_type: structs are nominal
    append(&ctx.types, Type{
        kind = .Struct,
        name = mangle_struct_instance(tmpl.name, args),
        structure = {is_instance = true, origin = tmpl_id, args = args},
    })
    new_id := TypeId(len(ctx.types) - 1)
    ctx.spans.ty_decs[new_id] = ctx.spans.ty_decs[tmpl_id]
    ctx.ty_modules[new_id] = ctx.ty_modules[tmpl_id]

    // register BEFORE filling the fields: a field may name this very
    // instance (`next: ^Node(T)`), which must find it and not recurse
    append(&get_type(tmpl_id).structure.instances, StructInstance{args = args, type = new_id})

    fill_struct_instance(tmpl_id, new_id)
    return new_id
}

// True for struct types that exist at runtime and so may be sized, laid out
// and emitted: plain structs and fully concrete instances. False for a
// template (`Box`, fields still `Generic`) and for a placeholder instance
// (`Box(T)` while a generic signature is resolved, no fields). Codegen,
// type_size and struct_field_offsets must skip those.
struct_is_concrete :: proc(tid: TypeId) -> bool {
    ty := get_type(tid)
    if ty.kind != .Struct && ty.kind != .Union do return false
    if gen_has(tid) do return false
    if ty.kind == .Struct && ty.structure.is_generic do return false
    return true
}

// Explicit `struct Name($G, $H)` names first (that is the `Name(a, b)` order),
// then any other `$T` declared in the fields, by first appearance.
struct_generic_names :: proc(sd: StructDec) -> []string {
    names := make([dynamic]string, allocator=get_ctx().allocator)
    for gp in sd.generics do append(&names, gp.name)
    for f in sd.fields do collect_generic_names(f.t, &names) // skips names already in `names`
    return names[:]
}

// Every generic name mentioned anywhere inside `t`.
gen_names_in :: proc(t: TypeId, out: ^[dynamic]string) {
    ty := get_type(t)^
    #partial switch ty.kind {
    case .Generic: {
        for n in out^ {
            if n == ty.name do return
        }
        append(out, ty.name)
    }
    case .Pointer:        gen_names_in(ty.ptr, out)
    case .Slice:          gen_names_in(ty.slice.type, out)
    case .FixedSizeArray: gen_names_in(ty.fixed_size_array.type, out)
    case .Struct: {
        if ty.structure.is_instance {
            for a in ty.structure.args do gen_names_in(a, out)
        }
    }
    case .Union: {
        for f in ty.union_fields do gen_names_in(f.type, out)
    }
    case .Function: {
        for a in ty.fn.args do gen_names_in(a.type, out)
        if v, ok := fn_variadic_elem(&ty); ok do gen_names_in(v, out)
        gen_names_in(ty.fn.ret_ty, out)
    }
    }
}

// Lowers the template's fields into the instance. Does nothing for a
// placeholder (an arg is still generic) or while the template's own fields
// aren't resolved yet; finish_struct_instances catches those later.
fill_struct_instance :: proc(tmpl_id, inst_id: TypeId) {
    ctx := get_ctx()
    inst := get_type(inst_id)^
    if inst.structure.fields != nil do return
    for a in inst.structure.args {
        if gen_has(a) do return
    }
    tmpl := get_type(tmpl_id)^ // copy: gen_subst can append types
    if len(tmpl.structure.fields) == 0 do return

    bound := make(map[string]TypeId, allocator = ctx.allocator)
    for n, i in tmpl.structure.generic_names {
        bound[n] = inst.structure.args[i]
    }
    fields := make([]Field, len(tmpl.structure.fields), allocator = ctx.allocator)
    for f, i in tmpl.structure.fields {
        fields[i] = f
        fields[i].type = gen_subst(f.type, bound, {})
    }
    get_type(inst_id).structure.fields = fields // fetched fresh: gen_subst appends
}

// Instances made before their template was resolved (`A { b: Box(i64) }`
// above `Box`). Instances made while filling go on the end of the list, so
// this loops by index.
finish_struct_instances :: proc(ast: ^AST) {
    for id in ast.items {
        if _, is_struct := get(id).(StructDec); !is_struct do continue
        tid := get_ctx().item_types[id]
        if !get_type(tid).structure.is_generic do continue
        for i := 0; i < len(get_type(tid).structure.instances); i += 1 {
            fill_struct_instance(tid, get_type(tid).structure.instances[i].type)
        }
    }
}

// Appends `t` to `b` using only [A-Za-z0-9_]: `^i32` -> `p_i32`, `[]f64` -> `a_f64`.
// Anything else (including the `.` in an instance name) becomes `_`.
mangle_ty :: proc(b: ^strings.Builder, t: TypeId) {
    for r in tts(t) {
        switch {
        case r >= 'a' && r <= 'z', r >= 'A' && r <= 'Z', r >= '0' && r <= '9', r == '_':
            strings.write_rune(b, r)
        case r == '^': strings.write_string(b, "p_")
        case r == '[': strings.write_string(b, "a_")
        case r == ']': // dropped
        case: strings.write_byte(b, '_')
        }
    }
}

// "f" + params [f64, ^i32] -> "f.f64.p_i32.v0"
mangle_version :: proc(base: string, params: []TypeId, idx: int) -> string {
    b: strings.Builder
    strings.builder_init(&b, get_ctx().allocator)
    strings.write_string(&b, base)
    for p in params {
        strings.write_byte(&b, '.')
        mangle_ty(&b, p)
    }
    // names are for readability only; the index is what makes them unique
    strings.write_string(&b, ".v")
    strings.write_int(&b, idx)
    return strings.to_string(b)
}

// `DynArray.i64`, `Pair.i64.p_f64`, `Box.Box_i64`
mangle_struct_instance :: proc(name: string, args: []TypeId) -> string {
    b := strings.builder_make(allocator = get_ctx().allocator)
    strings.write_string(&b, name)
    for a in args {
        strings.write_byte(&b, '.')
        mangle_ty(&b, a)
    }
    return strings.to_string(b)
}

// ---------------------------------------------------------------------------
// Builtin types
// ---------------------------------------------------------------------------

ty_from_name :: proc(name: string) -> TypeId {
    tid, ok := get_ctx().base_mod.types[name]
    if !ok do gala_panicf("compiler bug: builtin type \"%s\" missing from base module.", name)
    return tid
}
void_type    :: proc() -> TypeId { return ty_from_name("void") }
integer_type :: proc() -> TypeId { return ty_from_name("i64") }
byte_type    :: proc() -> TypeId { return ty_from_name("byte") }

get_untyped_default :: proc(t: TypeId) -> TypeId {
    #partial switch get_type(t).kind {
    case .UntypedInteger: return integer_type()
    case .UntypedFloat:   return ty_from_name("f64")
    }
    gala_panic("compiler bug: no default type for", tts(t))
}

// ---------------------------------------------------------------------------
// Statements
// ---------------------------------------------------------------------------

resolve_stmt :: proc(s: ^Scope, id: StmtId) {
    switch stmt in get(id) {
    case BreakStmt, ContinueStmt: {}
    case ForLoop: {
        resolve_expr(s, stmt.expr)
        new_s := new_scope(s)
        get_ctx().stmt_objects[id] = new_object(&new_s,
            Object{kind=.Argument, name=stmt.name, read_only=.LoopVar}, get_span(id))
        b := stmt.block
        resolve_block(&new_s, &b)
        free_scope(&new_s)
    }
    case WhileLoop: {
        resolve_expr(s, stmt.cond)
        b := stmt.block
        resolve_block(s, &b)
    }
    case VarDec: {
        resolve_expr(s, stmt.value)
        resolved_ty : Maybe(TypeId) = nil
        if stmt.type != nil {
            ty, ok := stmt.type.(TypeSpecifier); assert(ok)
            // no_declare_generic: a local has nowhere to bind a `$T`
            resolved_ty = resolve_type_specifier(s, ty, no_declare_generic = true)
        }
        oid := new_object(s, Object{kind=.Variable,
            name=stmt.name, type=resolved_ty}, get_span(id))
        get_ctx().stmt_objects[id] = oid
    }
    case Return: {
        if e, ok := stmt.expr.(ExprId); ok {
            resolve_expr(s, e)
        }
    }
    case Assignment: {
        resolve_expr(s, stmt.target)
        resolve_expr(s, stmt.value)
    }
    case IfElse: {
        resolve_expr(s, stmt.base_con)
        b := stmt.base_block
        resolve_block(s, &b)
        for a in stmt.alt {
            resolve_expr(s, a.cond)
            b := a.block
            resolve_block(s, &b)
        }
        if stmt.has_else_block {
            b = stmt.else_block
            resolve_block(s, &b)
        }
    }
    case ExprId: {
        resolve_expr(s, stmt)
    }
    case: gala_panic("compiler bug: resolve_stmt doesn't handle this statement kind")
    }
}

resolve_block :: proc(s: ^Scope, b: ^Block) {
    b_scope := new_scope(s)
    for id in b.stmts {
        resolve_stmt(&b_scope, id)
    }
    free_scope(&b_scope)
}

// ---------------------------------------------------------------------------
// Generics
// ---------------------------------------------------------------------------

// `$` names in order of first appearance (this is the order of `Name(a, b)`).
// Positions that can't declare a generic (union fields, anonymous struct
// fields, return types) are not walked: nothing there ever declares one.
collect_generic_names :: proc(t: TypeSpecifier, out: ^[dynamic]string) {
    #partial switch k in t {
    case GenericSpecifier: {
        for n in out^ {
            if n == k.name do return
        }
        append(out, k.name)
    }
    case SliceSpecifier:        collect_generic_names(k.base^, out)
    case PointerType:           collect_generic_names(k.ptr^, out)
    case FixedArreySpecifier:   collect_generic_names(k.base^, out)
    case GenericInstSpecifier: {
        for a in k.args do collect_generic_names(a, out)
    }
    case FnSpecifier: {
        for a in k.signature.args {
            if !a.inferred do collect_generic_names(a.t, out)
        }
        if k.signature.is_variadic do collect_generic_names(k.signature.variadic_ty, out)
    }
    }
}

specifier_has_generic :: proc(t: TypeSpecifier) -> bool {
    #partial switch k in t {
    case GenericSpecifier:
        return true
    case GenericInstSpecifier:
        for a in k.args {
            if specifier_has_generic(a) do return true
        }
        return false
    case SliceSpecifier:
        return specifier_has_generic(k.base^)
    case PointerType:
        return specifier_has_generic(k.ptr^)
    case FixedArreySpecifier:
        return specifier_has_generic(k.base^)
    case FnSpecifier:
        for a in k.signature.args {
            if !a.inferred && specifier_has_generic(a.t) do return true
        }
        if k.signature.is_variadic && specifier_has_generic(k.signature.variadic_ty) {
            return true
        }
        return false
    }
    return false
}

is_fn_signature_generic :: proc(sig: FnDecSignature) -> bool {
    if len(sig.generics) > 0 do return true // `fn f::<K, V>(..)`
    for a in sig.args {
        if !a.inferred && specifier_has_generic(a.t) do return true
    }
    if sig.is_variadic && specifier_has_generic(sig.variadic_ty) do return true
    // a generic only in the return type is rejected by no_declare_generic
    return false
}

// Walks a generic specifier against a concrete type to recover `$T`.
// Only positions that can declare a generic are walked (see collect_generic_names).
bind_generics :: proc(spec: TypeSpecifier, concrete: TypeId, out: ^map[string]TypeId) {
    ty := get_type(concrete)
    #partial switch k in spec {
    case GenericSpecifier:
        out^[k.name] = concrete
    case GenericInstSpecifier:
        if ty.kind == .Struct && ty.structure.is_instance {
            for a, i in k.args do bind_generics(a, ty.structure.args[i], out)
        }
    case PointerType:
        if ty.kind == .Pointer do bind_generics(k.ptr^, ty.ptr, out)
    case SliceSpecifier:
        if ty.kind == .Slice do bind_generics(k.base^, ty.slice.type, out)
    case FixedArreySpecifier:
        if ty.kind == .FixedSizeArray do bind_generics(k.base^, ty.fixed_size_array.type, out)
    case FnSpecifier:
        if ty.kind != .Function do return
        for a, i in k.signature.args {
            if a.inferred || i >= len(ty.fn.args) do continue
            bind_generics(a.t, ty.fn.args[i].type, out)
        }
        if k.signature.is_variadic && ty.fn.is_variadic {
            bind_generics(k.signature.variadic_ty, ty.fn.variadic_ty, out)
        }
    }
}

// Resolves clone `iid` of generic item `g` as the concrete function for `v`.
resolve_generic_clone :: proc(g, iid: ItemId, v: FnVersion) {
    ctx := get_ctx()

    // resolve in the module the generic was declared in
    mid := ctx.obj_modules[ctx.item_objects[g]]
    saved_mid := ctx.current_module_id
    ctx.current_module_id = mid
    defer ctx.current_module_id = saved_mid
    ms := &ctx.mods[mid].declarations

    orig, _ := get(g).(FnDec)
    fndec, ok := get(iid).(FnDec); assert(ok)

    if orig.export_name {
        highlight_lines(get_span(iid))
        gala_panic("Can't export generic functions.")
    }

    bindings := make(map[string]TypeId, allocator=ctx.allocator)
    for a, i in orig.signature.args {
        bind_generics(a.t, v.params[i], &bindings)
    }
    // version key layout: params, then the variadic element, then the
    // explicit `::<..>` generics
    tail := len(orig.signature.args)
    if orig.signature.is_variadic {
        bind_generics(orig.signature.variadic_ty, v.params[tail], &bindings)
        tail += 1
    }
    for gp, i in orig.signature.generics {
        bindings[gp.name] = v.params[tail + i]
    }

    fnty, fnscope := resolve_fn_dec_signature(ms, fndec.signature, bindings = bindings)
    resolve_block(&fnscope, &fndec.block)
    free_scope(&fnscope)

    tyid := new_fn_type(fnty)
    ctx.spans.ty_decs[tyid] = get_span(iid)

    append(&ctx.objs, Object{kind=.Variable, name=fndec.name, type=tyid, read_only=.Function})
    oid := ObjId(len(ctx.objs) - 1)
    ctx.obj_modules[oid] = mid
    ctx.spans.objs_decs[oid] = get_span(iid)
    ctx.item_objects[iid] = oid
}

// ---------------------------------------------------------------------------
// Items
// ---------------------------------------------------------------------------

// `struct Name($T, $G)` declares T and G up front as generic types in the
// struct's own scope. Otherwise the first `$T` in a field declares T there,
// and plain `T` refers to it in every field after. Positions that can't bind
// anything (union fields, anonymous struct fields) resolve with
// no_declare_generic, so they can only use what the scope already has.
resolve_struct_dec_item :: proc(s: ^ModuleScope, id: ItemId) {
    sd, ok := get_item(id).(StructDec); assert(ok)
    tid, iok := s.ty_forward[sd.name]; assert(iok) // make sure fd exists

    scope := new_scope(s)
    defer free_scope(&scope)

    generic_bindings := make(map[string]TypeId, allocator=get_ctx().allocator)
    template := get_type(tid)
    for name, i in template.structure.generic_names {
        generic_bindings[name] = template.structure.generic_params[i]
        // Preserve the old declaration check for both explicit and inferred
        // generic names. Only explicit names enter the scope immediately;
        // implicit `$T` enters it when its declaration is encountered.
        check_undeclared(&scope, name, get_span(id))
    }
    for gp, i in sd.generics {
        for prev in sd.generics[:i] {
            if prev.name == gp.name {
                highlight_lines({get_ctx().current_file, gp.span})
                gala_panicf("Duplicate generic \"%s\".", gp.name)
            }
        }
        scope.types[gp.name] = generic_bindings[gp.name]
    }

    fields := make([]Field, len(sd.fields), allocator=get_ctx().allocator)
    for f, i in sd.fields {
        for prev in fields[:i] {
            if prev.name == f.name {
                fmt.println("First declared here:")
                highlight_lines(get_ctx().current_file, prev.span)
                highlight_lines(get_ctx().current_file, f.span)
                gala_panicf("Field \"%s\" already exists in struct \"%s\".", f.name, sd.name)
            }
        }
        t := resolve_type_specifier(&scope, f.t, bindings=generic_bindings)
        fields[i] = Field{name=f.name, type=t, span=f.span}
    }

    // fill the forward-declared type in place: it already carries
    // is_generic / generic_names and any instances made so far
    get_type(tid).structure.fields = fields

    // link item to type
    delete_key(&s.ty_forward, sd.name) // delete fd and create type
    s.types[sd.name] = tid
    if !get_type(tid).structure.is_generic {
        if type_size(tid) == 0 {
            highlight_lines(get_span(id))
            gala_panicf("can't have empty structs (%s).", sd.name)
        }
    }
    get_ctx().item_types[id] = tid
}

panic_duplicate_arg :: proc(name: string, first, dup: Span) {
    fmt.println("First declared here:")
    highlight_lines({get_ctx().current_file, first})
    highlight_lines({get_ctx().current_file, dup})
    gala_panicf("Duplicate argument \"%s\".", name)
}

// For gala functions, an additional arg of type slice-of-variadic-type is
// added to the function body.
// Takes ^Scope (not ^ModuleScope) so fn type specifiers resolve from any scope.
//
// Explicit generics (`fn f::<K, V>(..)`) are declared in the arg scope first,
// as generic types (or as the bound concrete types when cloning), and their
// names are kept in `fn.generic_names`. Parameters may declare further ones
// with `$T`; the return type can only use what's already there.
resolve_fn_dec_signature :: proc(s: ^Scope, fndec: FnDecSignature, extern := false,
        no_declare_generic := false, bindings: map[string]TypeId = nil) -> (Type, Scope) {
    fnty := Type{}
    fnty.kind = .Function
    // a clone built from bindings is concrete
    fnty.fn.is_generic = is_fn_signature_generic(fndec) && bindings == nil
    arg_scope := new_scope(s)

    if len(fndec.generics) > 0 {
        names := make([]string, len(fndec.generics), allocator=get_ctx().allocator)
        for gp, i in fndec.generics {
            names[i] = gp.name
            if bindings != nil {
                tid, bok := bindings[gp.name]
                if !bok {
                    highlight_lines({get_ctx().current_file, gp.span})
                    gala_panicf("No binding for generic \"%s\".", gp.name)
                }
                arg_scope.types[gp.name] = tid
            } else {
                new_type(&arg_scope, Type{kind=.Generic, name=gp.name},
                    {get_ctx().current_file, gp.span})
            }
        }
        fnty.fn.generic_names = names[:]
    }

    if len(fndec.args) > 0 || fndec.is_variadic {
        args := make([]Arg, len(fndec.args), allocator=get_ctx().allocator)
        declared := make(map[string]Arg, allocator=get_ctx().allocator)
        defer delete(declared)
        for a, i in fndec.args {
            t: TypeId
            if a.inferred {
                t = void_type()
            } else {
                t = resolve_type_specifier(&arg_scope, a.t, no_declare_generic, bindings)
            }
            if da, ok := declared[a.name]; ok do panic_duplicate_arg(a.name, da.span, a.span)
            arg := Arg{name=a.name, type=t, span=a.span, inferred=a.inferred}

            if def, has_default := a.default.(ExprId); has_default {
                resolve_expr(s, def)
                arg.default = def
                arg.default_src = a.default_src
            }

            obj_ty: Maybe(TypeId) = t
            if a.inferred do obj_ty = nil
            arg.obj = new_object(&arg_scope,
                Object{kind=.Argument, name=a.name, type=obj_ty, read_only=.Argument},
                {file_name=get_ctx().current_file, span=a.span})

            args[i] = arg
            declared[a.name] = arg
        }
        fnty.fn.args = args
        if fndec.is_variadic {
            variadic_ty := resolve_type_specifier(&arg_scope, fndec.variadic_ty,
                no_declare_generic, bindings)
            name := fndec.variadic_arg_name
            if da, ok := declared[name]; ok do panic_duplicate_arg(name, da.span, fndec.variadic_arg_span)
            new_ty_id := intern_type(Type{kind=.Slice, slice={type=variadic_ty}})
            fnty.fn.gala_abi_ty = new_ty_id

            new_object(&arg_scope,
                Object{kind=.Argument, name=name, type=new_ty_id, read_only=.Argument},
                {file_name=get_ctx().current_file, span=fndec.variadic_arg_span})
            fnty.fn.is_variadic = true
            fnty.fn.variadic_ty = variadic_ty
            fnty.fn.variadic_name = name
        }
    }
    if fndec.ret_ty != nil {
        // a `$T` here would never be bound by a call
        fnty.fn.ret_ty = resolve_type_specifier(&arg_scope,
            fndec.ret_ty.(TypeSpecifier), no_declare_generic = true, bindings = bindings)
    } else {
        fnty.fn.ret_ty = void_type()
    }
    return fnty, arg_scope
}

// can't be generic, really. so fail
resolve_extern_fn_dec_item :: proc(s: ^ModuleScope, id: ItemId) {
    fndec, ok := get(id).(ExternFnDec); assert(ok)
    oid, ook := s.obj_forward[fndec.name]; assert(ook) // make sure fd exists
    obj := get(oid) // pointer, so modify that

    fnty, scope := resolve_fn_dec_signature(s, fndec, extern=true)
    if fnty.fn.is_generic {
        highlight_lines(get_span(id))
        gala_panicf("Extern function \"%s\" can't be generic.", fndec.name)
    }
    fnty.fn.is_external = true
    free_scope(&scope)

    tyid := new_fn_type(fnty)
    get_ctx().spans.ty_decs[tyid] = get_span(id)

    obj.type = tyid
    obj.name = fndec.name
    get_ctx().objs[oid] = obj^

    delete_key(&s.obj_forward, fndec.name) // delete fd and create object
    s.objects[fndec.name] = oid
    get_ctx().item_objects[id] = oid
}

resolve_fn_dec_item :: proc(s: ^ModuleScope, id: ItemId) {
    fndec, ok := get(id).(FnDec); assert(ok)
    oid, ook := s.obj_forward[fndec.name]; assert(ook) // make sure fd exists
    obj := get(oid) // pointer, so modify that

    fnty, fnscope := resolve_fn_dec_signature(s, fndec)

    // Generic bodies are resolved lazily, per clone, in
    // resolve_generic_clone. Only the signature is resolved here.
    if !fnty.fn.is_generic {
        resolve_block(&fnscope, &fndec.block)
    }
    if fndec.export_name { fnty.fn.is_export = true }

    free_scope(&fnscope)

    tyid := new_fn_type(fnty)
    get_ctx().spans.ty_decs[tyid] = get_span(id)

    obj.type = tyid
    obj.name = fndec.name
    get_ctx().objs[oid] = obj^

    delete_key(&s.obj_forward, fndec.name) // delete fd and create object
    s.objects[fndec.name] = oid
    get_ctx().item_objects[id] = oid
}

// Top-level variable. The object was forward-declared (see forward_item) so
// functions and other globals can use it regardless of order. The
// initialiser is resolved in the module scope (no locals/params visible) and
// the explicit type, if any, is resolved. For `:=` the type stays nil until
// the type checker infers it.
resolve_global_var_dec_item :: proc(s: ^ModuleScope, id: ItemId) {
    gd, ok := get(id).(GlobalVarDec); assert(ok)
    oid, ook := s.obj_forward[gd.name]; assert(ook) // make sure fd exists

    resolve_expr(s, gd.value)

    obj := get(oid) // pointer, so modify that
    if gd.type != nil {
        ts, tok := gd.type.(TypeSpecifier); assert(tok)
        // no_declare_generic: a global has nowhere to bind a `$T`
        obj.type = resolve_type_specifier(s, ts, no_declare_generic = true)
    }
    obj.name = gd.name
    get_ctx().objs[oid] = obj^

    delete_key(&s.obj_forward, gd.name) // delete fd and create object
    s.objects[gd.name] = oid
    get_ctx().item_objects[id] = oid
}

// does this import pull in the name n?
import_wants :: proc(imp: Import, n: string) -> bool {
    return imp.imports_all || slice.contains(imp.symbols, n)
}

// every listed symbol must be an exported type or object of the module
check_import_symbols :: proc(imp: Import, decls: $D, from: string) {
    if imp.imports_all do return
    for sym in imp.symbols {
        _, is_ty  := decls.ty_exports[sym]
        _, is_obj := decls.obj_exports[sym]
        if !is_ty && !is_obj {
            gala_panicf("Module \"%s\" has no exported symbol \"%s\" (imported from %s).",
                imp.fname, sym, from)
        }
    }
}

forward_item :: proc(s: ^ModuleScope, id: ItemId) {
    item := get(id)
    switch i in item {
    case Forward: {
        forward_item(s, i.i)
        imp := get_item(i.i).(Import)
        // get item id rather than using import path as it's not resolved.
        m, ok := get_ctx().item_module[i.i]
        if !ok {
            highlight_lines(get_span(i.i))
            gala_panicf("Can't find module \"{}\".", imp.fname)
        }
        mod := get_ctx().mods[m]
        check_import_symbols(imp, mod.declarations, get_ctx().current_file)

        for n, ty in mod.declarations.ty_exports {
            if !import_wants(imp, n) do continue
            // same type reached via two paths is fine, a different one is a clash
            if prev, exists := s.ty_exports[n]; exists && prev != ty {
                gala_panicf("Duplicate name \"%s\" (type) forwarded by %s from %s.",
                    n, get_ctx().current_file, imp.fname)
            }
            s.ty_exports[n] = ty
        }
        for n, o in mod.declarations.obj_exports {
            if !import_wants(imp, n) do continue
            if prev, exists := s.obj_exports[n]; exists && prev != o {
                gala_panicf("Duplicate name \"%s\" (object) forwarded by %s from %s.",
                    n, get_ctx().current_file, imp.fname)
            }
            s.obj_exports[n] = o
        }
    }
    case TypeAlias: {
        check_undeclared(s, i.new_name, get_span(id))
        s.ty_pending[i.new_name] = id
    }
    case Import: {
        resolved := resolve_import_path(get_ctx().current_file, i.fname)
        modid, ok := get_ctx().modules[resolved]
        debugln("IMPORTING {} FROM {}", resolved, cur_file())
        if !ok {
            gala_panicf("File import \"%s\" doesn't have a module id associated with it.\n", resolved)
        }
        decs := get_ctx().mods[modid]
        check_import_symbols(i, decs.declarations, get_ctx().current_file)

        // import exports only
        for obj, v in decs.declarations.obj_exports {
            if !import_wants(i, obj) do continue
            if prev, exists := s.objects[obj]; exists && prev != v {
                gala_panicf("Duplicate name \"%s\" in import (from %s importing %s)(object).",
                    obj, get_ctx().current_file, resolved)
            }
            s.objects[obj] = v
        }
        for ty, v in decs.declarations.ty_exports {
            if !import_wants(i, ty) do continue
            if prev, exists := s.types[ty]; exists && prev != v {
                gala_panicf("Duplicate name \"%s\" in import (from %s importing %s)(type).",
                    ty, get_ctx().current_file, resolved)
            }
            s.types[ty] = v
        }
        get_ctx().item_module[id] = modid
    }
    case StructDec:     new_type_fd(s, Type{kind=.Struct, name=i.name}, get_span(id))
    // kind=.Variable on purpose: a global is addressable like any local, so
    // the type checker and codegen treat its uses the same way.
    case GlobalVarDec:  new_object_fd(s, Object{kind=.Variable, name=i.name}, get_span(id))
    case FnDec:         new_object_fd(s, Object{kind=.Variable, name=i.name, read_only=.Function}, get_span(id))
    case ExternFnDec:   new_object_fd(s, Object{kind=.Variable, name=i.name, read_only=.Function}, get_span(id))
    // never looked up as an object: uses are rewritten, and const_table says
    // which objects are constants
    case ConstDec: {
        oid := new_object_fd(s, Object{kind=.Variable, name=i.name, read_only=.Constant}, get_span(id))
        const_table[oid] = ConstEntry{item = id, scope = s}
    }
    case: gala_panic("compiler bug: forward_item doesn't handle this item kind")
    }
}

resolve_item :: proc(s: ^ModuleScope, id: ItemId) {
    item := get(id)

    switch i in item {
    case Forward:       {}
    case Import:        {}
    case TypeAlias: {
        s.ty_exports[i.new_name] = resolve_alias(s, id)
    }
    case StructDec: {
        resolve_struct_dec_item(s, id)
        tid, ok := get_ctx().item_types[id]; assert(ok)
        s.ty_exports[i.name] = tid
    }
    case FnDec: {
        resolve_fn_dec_item(s, id)
        oid, ok := get_ctx().item_objects[id]; assert(ok)
        s.obj_exports[i.name] = oid
    }
    case ExternFnDec: {
        resolve_extern_fn_dec_item(s, id)
        oid, ok := get_ctx().item_objects[id]; assert(ok)
        s.obj_exports[i.name] = oid
    }
    case GlobalVarDec: {
        resolve_global_var_dec_item(s, id)
        oid, ok := get_ctx().item_objects[id]; assert(ok)
        s.obj_exports[i.name] = oid
    }
    case ConstDec: {
        // exports itself; may already be done if another constant needed it
        resolve_const_dec_item(s, id)
    }
    case: gala_panic("compiler bug: resolve_item doesn't handle this item kind")
    }
}

// ---------------------------------------------------------------------------
// Scopes and modules
// ---------------------------------------------------------------------------

// get_ctx().allocator may not be available on context init, so it's a param
new_scope :: proc(parent:^Scope=nil, allocator:=get_ctx().allocator) -> Scope {
    s := Scope{}
    s.objects       = make(map[string]ObjId,  allocator=allocator)
    s.types         = make(map[string]TypeId, allocator=allocator)
    s.ty_pending    = make(map[string]ItemId, allocator=allocator)
    s.parent = parent
    return s
}

new_module_scope :: proc(parent:^Scope=nil, allocator:=get_ctx().allocator) -> ModuleScope {
    s := ModuleScope{}
    s.scope = new_scope(parent, allocator)
    s.parent = parent
    s.obj_forward  = make(map[string]ObjId,  allocator=allocator)
    s.ty_forward   = make(map[string]TypeId, allocator=allocator)
    return s
}

free_scope :: proc(s: ^Scope) {
    if s == nil do return

    delete(s.objects)
    delete(s.types)

    s^ = {}
}

free_module_scope :: proc(s: ^ModuleScope) {
    if s == nil do return

    free_scope(&s.scope)

    delete(s.obj_forward)
    delete(s.ty_forward)

    s^ = {}
}

Module :: struct {
    declarations: ModuleScope,
    ast: ^AST,
    path: string,
}

// A struct can't contain itself by value, directly or through other structs
// and fixed arrays. Pointers, slices and functions break the cycle. Run once
// all of a module's structs are resolved, so declaration order doesn't
// matter. state: 0 = unvisited, 1 = in progress, 2 = done.

// The struct that a field type contains by value, if any (looking through
// fixed arrays). A placeholder instance (`Node(T)` inside Node's own
// declaration) has no fields yet; it stands for its template, which is what
// closes the cycle: Node -> Node(T) -> Node.
by_value_struct :: proc(t: TypeId) -> (TypeId, bool) {
    cur := t
    for {
        ty := get_type(cur)
        #partial switch ty.kind {
        case .FixedSizeArray: cur = ty.fixed_size_array.type
        case .Struct: {
            if ty.structure.is_instance {
                for a in ty.structure.args {
                    if gen_has(a) do return ty.structure.origin, true
                }
            }
            return cur, true
        }
        case .Union: return cur, true
        case:        return TypeId(0), false
        }
    }
    return TypeId(0), false
}

visit_struct :: proc(tid: TypeId, state: ^map[TypeId]int) {
    if state^[tid] == 2 do return
    state^[tid] = 1
    ty := get_type(tid)
    fields := ty.structure.fields
    if ty.kind == .Union do fields = ty.union_fields
    for f in fields {
        inner, ok := by_value_struct(f.type)
        if !ok do continue
        if state^[inner] == 1 {
            highlight_lines(get_ctx().current_file, f.span)
            gala_panicf("Struct \"%s\" would contain itself by value (infinite size): field \"%s\" (type \"%s\") closes a cycle. Use a pointer.",
                ty.name, f.name, get_type(inner).name)
        }
        visit_struct(inner, state)
    }
    state^[tid] = 2
}

check_struct_cycles :: proc(ast: ^AST) {
    state := make(map[TypeId]int, allocator=get_ctx().allocator)
    for id in ast.items {
        if _, is_struct := get(id).(StructDec); is_struct {
            tid, ok := get_ctx().item_types[id]; assert(ok)
            visit_struct(tid, &state)
        }
    }
    delete(state)
}

// Marks every generic struct before any field is resolved, so `Box(i64)`
// works above `Box`'s declaration. Runs against the forward declarations.
mark_generic_structs :: proc(s: ^ModuleScope, ast: ^AST) {
    for id in ast.items {
        sd, is_struct := get(id).(StructDec)
        if !is_struct do continue
        names := struct_generic_names(sd)
        if len(names) == 0 do continue
        tid := s.ty_forward[sd.name]
        get_type(tid).structure.is_generic = true
        get_type(tid).structure.generic_names = names
        get_type(tid).structure.explicit_generic_count = len(sd.generics)
        params := make([]TypeId, len(names), allocator=get_ctx().allocator)
        for name, i in names {
            // add_type may move the type table, so don't keep a Type pointer
            // alive across these appends.
            params[i] = add_type(Type{kind=.Generic, name=name}, get_span(id))
        }
        get_type(tid).structure.generic_params = params
    }
}

// Generic struct declarations are templates, not runtime structs, and
// constants are fully folded into their uses. Drops both from the AST now
// that resolution no longer needs them.
drop_template_items :: proc(ast: ^AST) {
    write := 0
    for id in ast.items {
        drop_item := false

        if _, ok := get(id).(StructDec); ok {
            if tid, exists := get_ctx().item_types[id]; exists {
                drop_item = get_type(tid).structure.is_generic
            }
        }
        if _, ok := get(id).(ConstDec); ok {
            drop_item = true
        }

        if drop_item do continue

        ast.items[write] = id
        write += 1
    }
    resize(&ast.items, write)
}

resolve_module_ast :: proc(ast: ^AST, path: string) -> ModId {
    alias_active = make(map[ItemId]bool, get_ctx().allocator)
    append(&get_ctx().mods, Module{})
    mid := ModId(len(get_ctx().mods) - 1)
    get_ctx().current_module_id = mid // for objects and what not

    global_scope := new_module_scope(&get_ctx().base_mod)

    // forward names
    for id in ast.items {
        forward_item(&global_scope, id)
    }

    mark_generic_structs(&global_scope, ast)

    // Structs first: a struct literal in a function body or a global
    // initialiser asserts that the struct's fields are filled in, so it
    // mustn't matter whether the struct is declared before or after its
    // first use.
    for id in ast.items {
        if _, is_struct := get(id).(StructDec); is_struct {
            resolve_item(&global_scope, id)
        }
    }
    finish_struct_instances(ast)
    check_struct_cycles(ast)

    // Constants next: they're folded here and every later use (function
    // bodies, globals, parameter defaults) is rewritten into the value, so
    // all must be known before anything else is resolved. Order between
    // constants doesn't matter (see ensure_const).
    for id in ast.items {
        if _, is_const := get(id).(ConstDec); is_const {
            resolve_item(&global_scope, id)
        }
    }

    // everything else
    for id in ast.items {
        _, is_struct := get(id).(StructDec)
        _, is_const := get(id).(ConstDec)
        if !is_struct && !is_const {
            resolve_item(&global_scope, id)
        }
    }

    drop_template_items(ast)

    get_ctx().mods[mid] = Module {
        declarations = global_scope,
        ast = ast,
        path = path,
    }
    delete(alias_active)
    return mid
}

