package main

import "core:slice"
import "core:strings"

TcContext :: struct {
    in_function: bool,
    fn_ret_ty: Maybe(TypeId),
    in_loop: Maybe(StmtId),
    generic_items: ^[dynamic]ItemId,
    generic_exprs: ^[dynamic]ExprId,
}

// one concrete instantiation of a generic function
FnVersion :: struct {
    llvm_fn_name: string,   // filled in by codegen
    params:       []TypeId, // concrete param types (the version's identity)
    ret_ty:       TypeId,
    item:         ItemId,   // the clone made for this version
}

// which version of which function a call expr resolved to
FnVersionRef :: struct {
    fn:  TypeId, // function types aren't interned, so this is unique per function
    idx: int,    // index into get_type(fn).fn.versions
}

// instantiation cap per generic function, catches infinite recursion
MAX_GENERIC_VERSIONS :: 64

// Finds the version with exactly these concrete params, or adds it.
// Type ids are interned, so comparing ids is enough.
get_or_add_fn_version :: proc(fn_tid: TypeId, params: []TypeId, ret_ty: TypeId) -> (idx: int, added: bool) {
    // fetch fresh each time, nothing here holds a pointer across calls
    f := &get_type(fn_tid).fn

    search: for v, i in f.versions {
        if len(v.params) != len(params) do continue
        for p, j in v.params {
            if p != params[j] do continue search
        }
        return i, false
    }

    owned := make([]TypeId, len(params), get_ctx().allocator)
    copy(owned, params)
    append(&f.versions, FnVersion{params = owned, ret_ty = ret_ty})
    return len(f.versions) - 1, true
}

expr_ty :: proc(id: ExprId) -> TypeId {
    t, ok := get_ctx().expr_types[id]
    if !ok {
        debugln(get(id))
        highlight_lines(get_span(id))
        gala_panic("compiler bug: expression has no type.")
    }
    return t
}

// ---------------------------------------------------------------------------
// Type compatibility
// ---------------------------------------------------------------------------
// Identity is types_equal (types.odin). On top of that the only implicit
// things are: untyped literals, and boxing into `any`.
//   assignable   a value of type `got` into a slot of type `expected`
//   unify_binop  the operand type of a binary operator
reduce_numerics :: proc(l, r: TypeId) -> (TypeId, bool, string) {
    if l == r do return l, true, ""

    lk := get_type(l).kind
    rk := get_type(r).kind

    l_untyped := is_numeric_untyped(l)
    r_untyped := is_numeric_untyped(r)

    // both untyped: UntypedFloat dominates
    if l_untyped && r_untyped {
        if lk == .UntypedFloat || rk == .UntypedFloat {
            return intern_type(Type{kind = .UntypedFloat}), true, ""
        }
        return intern_type(Type{kind = .UntypedInteger}), true, ""
    }

    // untyped on one side: it takes the typed side's type
    if l_untyped {
        if lk == .UntypedFloat && (is_integer(r) || is_byte_like(r)) {
            return TypeId(0), false, "type mismatch: float into integer"
        }
        return r, true, ""
    }
    if r_untyped {
        if rk == .UntypedFloat && (is_integer(l) || is_byte_like(l)) {
            return TypeId(0), false, "type mismatch: float into integer"
        }
        return l, true, ""
    }

    return TypeId(0), false, "type mismatch (different types)"
}

// pointer <-> pointer must be the same type; pointer <-> integer is offset
// math (result keeps the pointer's type, float offsets are rejected).
// Callers route here only when at least one side is a pointer.
reduce_pointers :: proc(l, r: TypeId) -> (TypeId, bool, string) {
    if l == r do return l, true, ""

    if is_pointer(l) && is_pointer(r) {
        if types_equal(l, r) do return l, true, ""
        return TypeId(0), false, "type mismatch: incompatible pointer types"
    }

    ptr, other := l, r
    if is_pointer(r) {
        ptr, other = r, l
    }
    if is_float(other) || get_type(other).kind == .UntypedFloat {
        return TypeId(0), false, "type mismatch: float offset into pointer"
    }
    if is_integer(other) || is_numeric_untyped(other) {
        return ptr, true, ""
    }
    return TypeId(0), false, "type mismatch: pointer with non-integer operand"
}

// Operand types of a binary operator.
unify_binop :: proc(l, r: TypeId) -> (TypeId, bool, string) {
    lk := get_type(l).kind
    rk := get_type(r).kind

    if lk == .UntypedStruct || rk == .UntypedStruct {
        return TypeId(0), false, "`{...}` can't be used in an expression: it has no type"
    }
    if l == r do return l, true, ""

    if is_pointer(l) || is_pointer(r) do return reduce_pointers(l, r)
    if is_numeric(l) && is_numeric(r) do return reduce_numerics(l, r)

    if types_equal(l, r) do return l, true, ""
    if lk == .Function && rk == .Function {
        return TypeId(0), false, "function types don't match"
    }
    return TypeId(0), false, "types don't match"
}

// A value of type `got` into a slot of type `expected` (initialiser,
// assignment, argument, return, literal element). Returns the type to
// propagate into the value; for everything except `any` that is `expected`.
//   untyped number -> a typed numeric (float into integer is an error);
//                     an untyped integer may also become a pointer (`p = 0`)
//   anything       -> `any` (boxing); untyped numbers box as their default type
//   otherwise      -> types_equal. `any` never converts back implicitly.
assignable :: proc(expected, got: TypeId, allow_box := false) -> (TypeId, bool, string) {
    ek := get_type(expected).kind
    gk := get_type(got).kind

    // `{...}` only becomes a struct. The fields are checked when the type
    // is propagated into it (complete_untyped_struct). It can't be boxed
    // into `any`, there's nothing to infer the struct from.
    if gk == .UntypedStruct {
        if ek == .Struct || ek == .Union do return expected, true, ""
        if ek == .Any {
            return TypeId(0), false, "can't infer the type of `{...}` for `any` (write the struct name)"
        }
        return TypeId(0), false, "`{...}` can only initialise a struct or a union"
    }
    if expected == got do return expected, true, ""

    if ek == .Any {
        // boxing is a call-site thing: only function arguments may do it
        if !allow_box {
            return TypeId(0), false, "values are only converted to `any` when passed as function arguments"
        }
        if gk == .Void {
            return TypeId(0), false, "can't use a value of type void"
        }
        if is_numeric_untyped(got) do return get_untyped_default(got), true, ""
        return got, true, ""
    }

    if is_numeric_untyped(got) {
        if is_numeric_typed(expected) {
            if gk == .UntypedFloat && !is_float(expected) {
                return TypeId(0), false, "type mismatch: float into integer"
            }
            return expected, true, ""
        }
        if gk == .UntypedInteger && is_pointer(expected) {
            return expected, true, ""
        }
    }

    if is_pointer(expected) != is_pointer(got) {
        return TypeId(0), false, "type mismatch: pointers and non-pointers don't convert implicitly (use cast)"
    }
    if types_equal(expected, got) do return expected, true, ""
    if ek == .Function && gk == .Function {
        return TypeId(0), false, "function types don't match"
    }
    return TypeId(0), false, "types don't match"
}

get_array_base_type :: proc(t: TypeId) -> (TypeId, bool) {
    ty := get_type(t)
    #partial switch ty.kind {
    case .FixedSizeArray: return ty.fixed_size_array.type, true
    case .Slice:          return ty.slice.type, true
    case .String:         return byte_type(), true
    case .Pointer: {
        // can't index through ^any or rawptr: there's no element type
        k := get_type(ty.ptr).kind
        if k == .Any || k == .Void {
            return TypeId(0), false
        }
        return ty.ptr, true
    }
    }
    return TypeId(0), false
}

can_reference :: proc(id: ExprId) -> bool {
    #partial switch _ in get_expr(id) {
    case Symbol, Index, FieldAccess:
        return true // unless proven otherwise ig
    }
    return false
}

// things that can sit on the left of `=`
can_assign_to :: proc(id: ExprId) -> bool {
    #partial switch _ in get_expr(id) {
    case Symbol, Index, FieldAccess, Deref:
        return true
    }
    return false
}

can_equal :: proc(id: TypeId, op: BinopKind) -> bool {
    t := get_type(id)

    if is_integer(id)   do return true
    if is_float(id)     do return true
    if is_byte_like(id) do return true

    #partial switch t.kind {
    case .Bool, .Pointer, .String:
        return true
    }
    return false
}

can_order :: proc(id: TypeId) -> bool {
    return is_integer(id) || is_float(id) || is_byte_like(id) || is_pointer(id)
}

// ---------------------------------------------------------------------------
// Shared checks
// ---------------------------------------------------------------------------

// Gives an untyped expression its default type (untyped int -> i64, float ->
// f64), pushes it down into the expression, and returns the final type.
settle_untyped :: proc(id: ExprId) -> TypeId {
    t := expr_ty(id)
    // checked before is_untyped, so it doesn't matter whether that
    // predicate covers UntypedStruct
    if get_type(t).kind == .UntypedStruct {
        highlight_lines(get_span(id))
        gala_panic("Can't infer the type of `{...}` here; write the struct name, e.g. `Vector2{...}`.")
    }
    if is_untyped(t) {
        t = get_untyped_default(t)
        propagate_type(t, id)
    }
    return t
}

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

require_bool :: proc(id: ExprId, what: string) {
    t := expr_ty(id)
    if get_type(t).kind != .Bool {
        highlight_lines(get_span(id))
        gala_panicf("%s must be a bool, got %s.", what, tts(t))
    }
}

// Types an index or slice bound: untyped integers become i64, everything
// else must be a valid index type.
tc_index_expr :: proc(tc: ^TcContext, idx: ExprId) {
    tc_expr(tc, idx)
    t := expr_ty(idx)
    if is_untyped(t) {
        if get_type(t).kind != .UntypedInteger {
            highlight_lines(get_span(idx))
            gala_panic("Array index must be an integer.")
        }
        t = integer_type()
        propagate_type(t, idx)
    }
    if !is_valid_index_type(t) {
        highlight_lines(get_span(idx))
        gala_panicf("Can't use type %s to index an array.", tts(t))
    }
}

// Re-checks every nested binop against the final operand type. `rule` is the
// operator rule of the outermost expression (arithmetic / equality / order).
BinopRule :: proc(ty: TypeId, op: BinopKind) -> bool

check_binop_tree :: proc(id: ExprId, ty: TypeId, rule: BinopRule) -> bool {
    if e, is_binop := get_expr(id).(Binop); is_binop {
        return rule(ty, e.kind) &&
            check_binop_tree(e.left, ty, rule) &&
            check_binop_tree(e.right, ty, rule)
    }
    return true
}

// don't rebox any
mark_arg :: proc(arg: ^FnCallArg, param_ty: TypeId, r: TypeId) {
    earg := arg.expr
    if get_type(param_ty).kind == .Any && get_type(expr_ty(earg)).kind != .Any {
        t := expr_ty(earg)
        if is_untyped(t) {
            propagate_type(get_untyped_default(t), earg)
        }
        arg.needs_boxing = true
        arg.box_type = expr_ty(earg)
    } else {
        assert(r == param_ty)
        propagate_type(r, earg)
    }
}

// Checks one call argument against its (concrete) parameter type.
// `name` is empty for variadic arguments.
tc_check_arg :: proc(arg: ^FnCallArg, pos: int, name: string, expected: TypeId) {
    earg := arg.expr
    r, ok, err := assignable(expected, expr_ty(earg), allow_box = true)
    if !ok {
        highlight_lines(get_span(earg))
        if len(name) > 0 {
            gala_panicf("Argument %d (\"%s\"): %s (expected %s, got %s).",
                pos, name, err, tts(expected), tts(expr_ty(earg)))
        }
        gala_panicf("Argument %d: %s (expected %s, got %s).",
            pos, err, tts(expected), tts(expr_ty(earg)))
    }
    mark_arg(arg, expected, r)
}

// Function types whose parameter defaults have already been checked.
// tc_default_args is reached from several places (the module pre-pass, a
// FnLit, every call), so it must only do the work once per function.
default_args_done: map[TypeId]bool

// Type-checks the parameter defaults of a function type, so a bad default is
// reported at the declaration rather than at some far-away call site.
//
// A default is stored as a call to a hidden function (see make_default_thunk),
// and typing a call only needs the callee's signature, so this doesn't depend
// on globals having been typed yet.
//
// For `name := expr` parameters the type is inferred here from the written
// expression (untyped literals take their default type, like `x := 1`), then
// patched into the parameter, its object (so the body sees it) and the hidden
// function's return type. So this must run before anything uses the function:
// typecheck_module runs it for module-level fns, FnLit before its body, and
// FnCall on its callee.
tc_default_args :: proc(tc: ^TcContext, fn_type_id: TypeId) {
    if default_args_done[fn_type_id] do return
    default_args_done[fn_type_id] = true

    fn_ty := get_type(fn_type_id)
    for a, i in fn_ty.fn.args {
        def, has_default := a.default.(ExprId)
        if !has_default do continue

        if a.inferred {
            src := a.default_src.(ExprId)
            tc_expr(tc, src)

            t := expr_ty(src)
            // no default type to give it
            kind := get_type(t).kind
            if kind == .Void {
                highlight_lines(get_span(src))
                gala_panicf("Can't infer the type of parameter \"%s\" from its default value.", a.name)
            }
            if is_untyped(t) {
                t = get_untyped_default(t)
                propagate_type(t, src)
            }

            call := get_expr(def).(FnCall)
            thunk_obj := get_obj(get_ctx().expr_objects[call.target])
            get_ctx().types[thunk_obj.type.(TypeId)].fn.ret_ty = t
            get_ctx().types[fn_type_id].fn.args[i].type = t
            get_obj(a.obj).type = t

            // type the call only now that the hidden function returns `t`
            tc_expr(tc, def)
            continue
        }

        tc_expr(tc, def)
        _, ok, err := assignable(a.type, expr_ty(def))
        if !ok {
            highlight_lines(get_span(def))
            gala_panicf("Default value doesn't match the type of parameter \"%s\": %s (expected %s, got %s).",
                a.name, err, tts(a.type), tts(expr_ty(def)))
        }
    }
}

// Puts a call's arguments into parameter order, for non-variadic callees.
// Positional arguments fill parameters from the left, `name = expr` ones fill
// the parameter with that name, and anything left takes its default. The
// result has one entry per parameter and no names, so everything downstream
// (including codegen) sees an ordinary full-arity positional call.
order_call_args :: proc(id: ExprId, args: [dynamic]FnCallArg, params: []Arg) -> [dynamic]FnCallArg {
    ordered := make([dynamic]FnCallArg, len(params), allocator=get_ctx().allocator)
    filled := make([]bool, len(params), allocator=get_ctx().allocator)

    for a, i in args {
        idx := i
        if len(a.name) > 0 {
            idx = -1
            for p, j in params {
                if p.name == a.name {
                    idx = j
                    break
                }
            }
            if idx < 0 {
                highlight_lines(get_ctx().current_file, a.name_span)
                gala_panicf("Function has no parameter named \"%s\".", a.name)
            }
        } else if i >= len(params) {
            highlight_lines(get_span(a.expr))
            gala_panicf("Too many arguments: the function takes at most %d, got %d.",
                len(params), len(args))
        }
        // covers named-twice and a positional landing on a named one (`f(b=1, 2)`)
        if filled[idx] {
            if len(a.name) > 0 {
                highlight_lines(get_ctx().current_file, a.name_span)
            } else {
                highlight_lines(get_span(a.expr))
            }
            gala_panicf("Parameter \"%s\" is given more than once.", params[idx].name)
        }
        ordered[idx] = a
        ordered[idx].name = ""
        filled[idx] = true
    }

    for p, i in params {
        if filled[i] do continue
        def, has_default := p.default.(ExprId)
        if !has_default {
            highlight_lines(get_span(id))
            gala_panicf("Missing argument \"%s\" (it has no default value).", p.name)
        }
        ordered[i] = FnCallArg{expr = def}
    }
    return ordered
}

// Why `id` is read-only (bad = true), or "", false when it can be written to
// (or, with writing = false, referenced). Walks to the root of the place:
//   x.f, arr[i] (fixed array)  part of x, so x decides
//   p^, slice[i], ptr[i]       other memory, always fine
//   str[i]                     read-only for writes
read_only_reason :: proc(id: ExprId, writing: bool) -> (why: string, bad: bool) {
    #partial switch e in get_expr(id) {
    case Symbol: {
        oid, ok := get_ctx().expr_objects[id]
        if !ok do return "", false
        what: string
        switch get_obj(oid).read_only {
        case .No:       return "", false
        case .Argument: what = "a function argument"
        case .LoopVar:  what = "a loop variable"
        case .Function: what = "a function"
        case .Constant: what = "a constant"
        }
        return strings.concatenate({"\"", e.name, "\" is ", what}, context.temp_allocator), true
    }
    case FieldAccess:
        return read_only_reason(e.target, writing)
    case Index: {
        #partial switch get_type(expr_ty(e.target)).kind {
        case .FixedSizeArray: return read_only_reason(e.target, writing)
        case .String:
            if writing do return "strings are read-only", true
        }
        return "", false
    }
    case Deref:
        return "", false
    }
    return "it's a temporary value", true
}

check_mutable :: proc(id: ExprId, action: string, writing: bool) {
    if why, bad := read_only_reason(id, writing); bad {
        highlight_lines(get_span(id))
        gala_panicf("Can't %s this expression: %s.", action, why)
    }
}

// ---------------------------------------------------------------------------
// Expressions
// ---------------------------------------------------------------------------

// `{...}` met a struct type it has to become: bind the values to the
// fields, check each one, and turn the node into an ordinary typed
// AggregateLit so codegen doesn't need to know `{...}` exists.
complete_untyped_struct :: proc(id: ExprId, target: TypeId) {
    e := get_expr(id).(AggregateLit)
    ty := get_type(target)

    if ty.kind == .Union { // your flag or kind check
        complete_untyped_union(id, e, target)
        return
    }

    if ty.kind != .Struct {
        highlight_lines(get_span(id))
        gala_panicf("Can't initialise a value of type %s with `{...}` (not a struct).", tts(target))
    }
    if ty.structure.is_generic {
        highlight_lines(get_span(id))
        gala_panicf("Can't infer the instance of generic struct %s from `{...}`; write the type, e.g. `%s(i64){...}`.",
            tts(target), ty.name)
    }

    bind_aggregate_lit_fields(id, &e, target, tts(target))

    for sf in ty.structure.fields {
        f := e.fields[sf.name]
        r, ok, err := assignable(sf.type, expr_ty(f.expr))
        if !ok {
            highlight_lines(get_ctx().current_file, f.span)
            gala_panicf("Field \"%s\": %s (expected %s, got %s).",
                sf.name, err, tts(sf.type), tts(expr_ty(f.expr)))
        }
        propagate_type(r, f.expr) // nested `{...}` completes itself here
    }

    get_ctx().expr_resolution_types[id] = target
    get_ctx().expr_types[id] = target
    get_expr(id)^ = e
}
// `{name = value}` becoming a union: exactly one field, set by name.
complete_untyped_union :: proc(id: ExprId, e: AggregateLit, target: TypeId) {
    ufields := get_type(target).union_fields
    if len(e.positional) > 0 || len(e.fields) != 1 {
        highlight_lines(get_span(id))
        gala_panicf("A union literal sets exactly one field by name, e.g. `{%s = ...}`.", ufields[0].name)
    }
    for name, f in e.fields {
        found := false
        for uf in ufields {
            if uf.name != name do continue
            found = true
            r, ok, err := assignable(uf.type, expr_ty(f.expr))
            if !ok {
                highlight_lines(get_ctx().current_file, f.span)
                gala_panicf("Field \"%s\": %s (expected %s, got %s).",
                    name, err, tts(uf.type), tts(expr_ty(f.expr)))
            }
            propagate_type(r, f.expr)
        }
        if !found {
            highlight_lines(get_ctx().current_file, f.span)
            gala_panicf("Field \"%s\" doesn't exist in union %s.", name, tts(target))
        }
    }
    get_ctx().expr_resolution_types[id] = target
    get_ctx().expr_types[id] = target
}
tc_expr :: proc(tc: ^TcContext, id: ExprId) {
    switch e in get_expr(id) {
    case TypeInfoOf: {
        tc_expr(tc, e.expr)
        t := expr_ty(e.expr)
        r, ok, err := assignable(integer_type(), t)
        if !ok {
            highlight_lines(get_span_expr(id))
            gala_panicf("`type_info` takes an i64 type id, got %s (%s).", tts(t), err)
        }
        propagate_type(r, e.expr)
        get_ctx().expr_types[id] = get_ctx().expr_resolution_types[id]
        get_ctx().uses_type_table = true
    }
    case TypeIdOf: {
        _, syntactic := e.t.(TypeSpecifier)
        if !syntactic && !e.names_type {
            ex := e.expr.(ExprId)
            tc_expr(tc, ex)
            require_typed_value(ex, "take the type_id of")
            // type_id(1) is i64, like `x := 1`. Codegen reads the id from
            // here, same as for the type form.
            get_ctx().expr_resolution_types[id] = settle_untyped(ex)
        }
        get_ctx().expr_types[id] = intern_type(Type{kind=.UntypedInteger})
    }
    case UnNegative: {
        tc_expr(tc, e.expr)
        t := expr_ty(e.expr)
        if !is_numeric(t) {
            highlight_lines(get_span_expr(id))
            gala_panicf("Can't negate an expression of type %s.", tts(t))
        }
        get_ctx().expr_types[id] = t
    }
    case UnNot: {
        tc_expr(tc, e.expr)
        t := expr_ty(e.expr)
        if get_type(t).kind != .Bool {
            highlight_lines(get_span_expr(id))
            gala_panicf("`!` needs a bool, got %s.", tts(t))
        }
        get_ctx().expr_types[id] = ty_from_name("bool")
    }
    case BoolLitFalse, BoolLitTrue: {
        get_ctx().expr_types[id] = ty_from_name("bool")
    }
    case String: {
        get_ctx().expr_types[id] = intern_type({kind=.String})
    }
    case Deref: {
        tc_expr(tc, e.expr)
        t := expr_ty(e.expr)
        if get_type(t).kind != .Pointer {
            highlight_lines(get_span_expr(id))
            gala_panicf("Can't dereference an expression of type %s (not a pointer).", tts(t))
        }
        if get_type(get_type(t).ptr).kind == .Void {
            highlight_lines(get_span_expr(id))
            gala_panicf("Can't dereference an expression of type %s (it points to void).", tts(t))
        }
        get_ctx().expr_types[id] = get_type(t).ptr
    }
    case Reference: {
        tc_expr(tc, e.expr)
        if !can_reference(e.expr) {
            highlight_lines(get_span_expr(id))
            gala_panic("Can't take the address of this expression (only variables, fields and elements).")
        }

        check_mutable(e.expr, "take the address of", false)

        t := Type{ kind = .Pointer, ptr = expr_ty(e.expr) }
        get_ctx().expr_types[id] = intern_type(t)
    }
    case TakeSlice: {
        tc_expr(tc, e.target)
        target_ty := expr_ty(e.target)

        if !is_array_type(target_ty) {
            highlight_lines(get_span(e.target))
            gala_panicf("Can't slice a value of type %s.", tts(target_ty))
        }

        if !e.empty_start do tc_index_expr(tc, e.start)
        if !e.empty_end   do tc_index_expr(tc, e.end)

        if get_type(target_ty).kind == .String {
            get_ctx().expr_types[id] = target_ty;
            return
        }
        ty, ok := get_array_base_type(target_ty)
        if !ok {
            highlight_lines(get_span(e.target))
            gala_panicf("Can't slice a value of type %s (it has no element type).", tts(target_ty))
        }
        get_ctx().expr_types[id] = intern_type(Type{kind=.Slice, slice={type=ty}})
    }
    case Index: {
        tc_expr(tc, e.target)
        target_ty := expr_ty(e.target)

        if !can_be_index(target_ty) {
            highlight_lines(get_span(e.target))
            gala_panicf("Can't index a value of type %s.", tts(target_ty))
        }

        tc_index_expr(tc, e.index)

        ty, ok := get_array_base_type(target_ty)
        if !ok {
            highlight_lines(get_span(e.target))
            gala_panicf("Can't index a value of type %s (it has no element type).", tts(target_ty))
        }
        if get_type(ty).kind == .Void /* || get_type(ty).kind == .Any */ {
            highlight_lines(get_span(e.target))
            gala_panicf("Can't index a value of type %s: its element type is %s.",
                tts(target_ty), tts(ty))
        }

        get_ctx().expr_types[id] = ty
    }
    case FixedSizeArray: {
        // every initialiser element is checked against the element type
        elem_ty := get_ctx().expr_resolution_types[id]

        for el in e.initialiser {
            tc_expr(tc, el)
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
        t.fixed_size_array.size = e.size
        get_ctx().expr_types[id] = intern_type(t)
    }
    case FieldAccess: {
        tc_expr(tc, e.target)
        target_tid := expr_ty(e.target)
        target_ty := get_type(target_tid)

        // `any` is { data: rawptr, id: i64 }
        if target_ty.kind == .Any {
            switch e.field {
            case "data":
                get_ctx().expr_types[id] = intern_type(Type{kind = .Pointer, ptr = void_type()})
            case "id":
                get_ctx().expr_types[id] = integer_type()
            case:
                highlight_lines(get_span(id))
                gala_panicf("Field \"%s\" doesn't exist in type any (it has \"data\" and \"id\").", e.field)
            }
            return
        }
        if target_ty.kind == .Union {
            for f in target_ty.union_fields {
                if f.name == e.field {
                    get_ctx().expr_types[id] = f.type
                    return
                }
            }
            highlight_lines(get_span(id))
            gala_panicf("Field \"%s\" doesn't exist in %s.", e.field, tts(target_tid))
        }

        if target_ty.kind != .Struct {
            highlight_lines(get_span(id))
            gala_panicf("Can't access field \"%s\" of a value of type %s (not a struct).",
                e.field, tts(target_tid))
        }
        for f in target_ty.structure.fields {
            if f.name == e.field {
                get_ctx().expr_types[id] = f.type
                return
            }
        }
        highlight_lines(get_span(id))
        gala_panicf("Field \"%s\" doesn't exist in type %s.", e.field, target_ty.name)
    }
    case AggregateLit: {
        if e.ty == nil {
            // `{...}`: type the values now, the struct type arrives later
            // through assignable/propagate_type
            for _, f in e.fields do tc_expr(tc, f.expr)
            for f in e.positional do tc_expr(tc, f.expr)
            get_ctx().expr_types[id] = intern_type(Type{kind=.UntypedStruct})
            return
        }

        tid := get_ctx().expr_resolution_types[id]
        if get_type(tid).kind == .Union {
            // `the_union{ f = v }`: type the value, then check it against
            // the named field (same path as the untyped `{ f = v }` form)
            for _, f in e.fields do tc_expr(tc, f.expr)
            complete_untyped_union(id, e, tid)
            return
        }
        typed_already := false
        initial_ty := get_type(tid)^
        needs_struct_inference := initial_ty.kind == .Struct &&
            (initial_ty.structure.is_generic ||
             (initial_ty.structure.is_instance && gen_has(tid)))
        if needs_struct_inference {
            // Bare `Box{..}` and partial `Box(i64){..}` both infer from the
            // template's fields. The partial instance carries explicit args.
            tmpl_id := tid
            if initial_ty.structure.is_instance do tmpl_id = initial_ty.structure.origin
            for sf in get_type(tmpl_id).structure.fields {
                tc_expr(tc, e.fields[sf.name].expr)
            }
            typed_already = true
            tid = infer_aggregate_lit_type(id, tid, e)
            get_ctx().expr_resolution_types[id] = tid
        }
        s := get_type(tid)
        for sf in s.structure.fields {
            f := e.fields[sf.name].expr
            if !typed_already do tc_expr(tc, f)
            r, ok, err := assignable(sf.type, expr_ty(f))
            if !ok {
                highlight_lines(get_ctx().current_file, e.fields[sf.name].span)
                gala_panicf("Field \"%s\": %s (expected %s, got %s).",
                    sf.name, err, tts(sf.type), tts(expr_ty(f)))
            }
            propagate_type(r, f)
        }
        get_ctx().expr_types[id] = tid
    }
    case Transmute: {
        tc_expr(tc, e.target)
        to := get_ctx().expr_resolution_types[id]
        from := settle_untyped(e.target)
        if !can_transmute_to(from, to) {
            highlight_lines(get_span(id))
            gala_panicf("Can't transmute %s to %s.", tts(from), tts(to))
        }
        get_ctx().expr_types[id] = to
    }
    case Cast: {
        tc_expr(tc, e.target)
        to := get_ctx().expr_resolution_types[id]
        from := settle_untyped(e.target)
        if !can_cast_to(from, to) {
            highlight_lines(get_span(id))
            gala_panicf("Can't cast %s to %s.", tts(from), tts(to))
        }
        get_ctx().expr_types[id] = to
    }
    case Symbol: {
        obj := get_ctx().expr_objects[id]
        // a global declared with `:=` has no type until its own initialiser
        // has been checked, so using one before that (a later global, or
        // itself) lands here with a nil type
        t, has_type := get_obj(obj).type.(TypeId)
        if !has_type {
            highlight_lines(get_span(id))
            gala_panicf("\"%s\" is used before its type is known (a global's initialiser can only use globals declared before it).", e.name)
        }
        get_ctx().expr_types[id] = t
    }
    case Number: {
        // untyped float if it has a '.', otherwise untyped int (decimal, 0x.., 0b..)
        if is_float_literal(e.text) {
            get_ctx().expr_types[id] = intern_type(Type{kind=.UntypedFloat})
            return
        }
        get_ctx().expr_types[id] = intern_type(Type{kind=.UntypedInteger})
    }
    case Len: {
        tc_expr(tc, e.target)
        t := expr_ty(e.target)
        k := get_type(t).kind
        if k != .Slice && k != .String && k != .FixedSizeArray {
            highlight_lines(get_span_expr(id))
            gala_panicf("Can't take len of an expression of type %s.", tts(t))
        }
        get_ctx().expr_types[id] = intern_type({kind=.UntypedInteger})
    }
    case SizeOf: {
        _, syntactic := e.t.(TypeSpecifier)
        if !syntactic && !e.names_type {
            ex := e.expr.(ExprId)
            tc_expr(tc, ex)
            require_typed_value(ex, "take the size_of")
            // size_of(1) is i64, like `x := 1`. Codegen reads the type from
            // here, same as for the type form.
            get_ctx().expr_resolution_types[id] = settle_untyped(ex)
        }
        get_ctx().expr_types[id] = intern_type(Type{kind=.UntypedInteger})
    }
    case Binop:   tc_binop(tc, id, e)
    case FnCall:  tc_fn_call(tc, id, e)
    case FnLit: {
        // the resolver already built the fn type from the signature
        tid := get_ctx().expr_resolution_types[id]
        assert(get_type(tid).kind == .Function)

        if get_type(tid).fn.is_generic {
            highlight_lines(get_span(id))
            gala_panic("Function literals can't be generic; declare a named generic function instead.")
        }

        // validate parameter defaults before anything can call this literal
        tc_default_args(tc, tid)

        // body is checked like a FnDec body: fresh function context, and the
        // enclosing loop must not be visible (no break/continue across it).
        // (type fetched after tc_default_args, which may grow the table)
        new_tc := tc^
        new_tc.in_function = true
        new_tc.fn_ret_ty = get_type(tid).fn.ret_ty
        new_tc.in_loop = nil
        tc_block(&new_tc, e.block)

        get_ctx().expr_types[id] = tid
    }
    case: gala_panic("compiler bug: tc_expr doesn't handle this expression kind")
    }
}

tc_binop :: proc(tc: ^TcContext, id: ExprId, e: Binop) {
    tc_expr(tc, e.left)
    tc_expr(tc, e.right)

    left_ty  := expr_ty(e.left)
    right_ty := expr_ty(e.right)

    ty, ok, why := unify_binop(left_ty, right_ty)
    if !ok {
        highlight_lines(get_span(id))
        gala_panicf("%s (%s vs %s)", why, tts(left_ty), tts(right_ty))
    }
    propagate_type(ty, e.left)
    propagate_type(ty, e.right)

    switch e.kind {
    case .Addition, .Subtraction, .Multiply, .Divide, .Modulo: {
        // pointer arithmetic: `ptr +- int`, `int + ptr`, `ptr - ptr`.
        // nothing else: not `ptr + ptr`, `int - ptr`, `*`, `/` or `%`.
        if is_pointer(left_ty) || is_pointer(right_ty) {
            if e.kind != .Addition && e.kind != .Subtraction {
                highlight_lines(get_span(id))
                gala_panicf("Operator %v can't be applied to pointers (only + and - are allowed).", e.kind)
            }
            if e.kind == .Addition && is_pointer(left_ty) && is_pointer(right_ty) {
                highlight_lines(get_span(id))
                gala_panic("Can't add two pointers.")
            }
            if e.kind == .Subtraction && is_pointer(right_ty) && !is_pointer(left_ty) {
                highlight_lines(get_span(id))
                gala_panic("Can't subtract a pointer from an integer.")
            }
            get_ctx().expr_types[id] = ty
            return
        }

        if !can_binop(ty, e.kind) {
            highlight_lines(get_span(id))
            gala_panicf("Operator %v can't be applied to type %s.", e.kind, tts(ty))
        }
        if e.kind == .Modulo {
            k := get_type(ty).kind
            // untyped integers (`7 % 3`) are fine; they get a concrete
            // integer type later when propagated into a slot
            if !is_int_kind(k) && k != .UntypedInteger {
                highlight_lines(get_span(id))
                gala_panicf("Modulo needs integer operands, got %s.", tts(ty))
            }
        }

        rule := proc(t: TypeId, op: BinopKind) -> bool { return can_binop(t, op) }
        if !check_binop_tree(id, ty, rule) {
            highlight_lines(get_span(id))
            gala_panicf("The operands of this expression can't be combined (type %s).", tts(ty))
        }

        get_ctx().expr_types[id] = ty
    }
    case .NotEqual, .Equal: {
        if is_untyped(ty) {
            ty = get_untyped_default(ty)
            // force operands to resolve first
            propagate_type(ty, e.left)
            propagate_type(ty, e.right)
        }

        if !can_equal(ty, e.kind) {
            highlight_lines(get_span(id))
            gala_panicf("Can't compare values of type %s.", tts(ty))
        }

        rule := proc(t: TypeId, op: BinopKind) -> bool { return can_equal(t, op) }
        if !check_binop_tree(id, ty, rule) {
            highlight_lines(get_span(id))
            gala_panicf("The operands of this expression can't be compared (type %s).", tts(ty))
        }

        get_ctx().expr_types[id] = ty_from_name("bool")
    }
    case .LessEqual, .GreaterEqual, .Less, .Greater: {
        if is_untyped(ty) {
            ty = get_untyped_default(ty)
        }
        propagate_type(ty, e.left)
        propagate_type(ty, e.right)

        if !can_order(ty) {
            highlight_lines(get_span(id))
            gala_panicf("Can't order values of type %s.", tts(ty))
        }

        rule := proc(t: TypeId, _: BinopKind) -> bool { return can_order(t) }
        if !check_binop_tree(id, ty, rule) {
            highlight_lines(get_span(id))
            gala_panicf("The operands of this expression can't be ordered (type %s).", tts(ty))
        }

        get_ctx().expr_types[id] = ty_from_name("bool")
    }
    case .BitAnd, .BitOr, .BitXor, .ShiftLeft, .ShiftRight: {
        if is_untyped(ty) {
            ty = get_untyped_default(ty)
        }
        propagate_type(ty, e.left)
        propagate_type(ty, e.right)

        if !is_integer(ty) && !is_byte_like(ty) {
            highlight_lines(get_span(id))
            gala_panicf("Bitwise operator %v needs integer or byte operands, got %s.", e.kind, tts(ty))
        }

        get_ctx().expr_types[id] = ty
    }
    case .LogicalAnd, .LogicalOr: {
        if get_type(ty).kind != .Bool {
            highlight_lines(get_span(id))
            gala_panicf("Logical operator %v needs bool operands, got %s.", e.kind, tts(ty))
        }
        propagate_type(ty, e.left)
        propagate_type(ty, e.right)
        get_ctx().expr_types[id] = ty
    }
    }
}

// `Generic`-kind type names inside a resolved type, in order of first appearance.
collect_generic_type_names :: proc(t: TypeId, out: ^[dynamic]string) {
    ty := get_type(t)
    #partial switch ty.kind {
    case .Union: {
        for f in ty.union_fields do collect_generic_type_names(f.type, out)
    }
    case .Generic: {
        for n in out^ {
            if n == ty.name do return
        }
        append(out, ty.name)
    }
    case .Pointer:        collect_generic_type_names(ty.ptr, out)
    case .Slice:          collect_generic_type_names(ty.slice.type, out)
    case .FixedSizeArray: collect_generic_type_names(ty.fixed_size_array.type, out)
    case .Struct: {
        for a in ty.structure.args do collect_generic_type_names(a, out)
    }
    case .Function: {
        for a in ty.fn.args do collect_generic_type_names(a.type, out)
        if ty.fn.is_variadic do collect_generic_type_names(ty.fn.variadic_ty, out)
        collect_generic_type_names(ty.fn.ret_ty, out)
    }
    }
}

// Does the signature still have generics (`$T`) that only the arguments can
// bind, i.e. ones that aren't declared in `::<..>`?
needs_inference :: proc(fargs: []Arg, variadic_ty: TypeId, is_variadic: bool,
        explicit: []string) -> bool {
    names := make([dynamic]string, allocator=context.temp_allocator)
    for a in fargs do collect_generic_type_names(a.type, &names)
    if is_variadic do collect_generic_type_names(variadic_ty, &names)
    for n in names {
        found := false
        for e in explicit {
            if e == n do found = true
        }
        if !found do return true
    }
    return false
}

tc_fn_call :: proc(tc: ^TcContext, id: ExprId, e: FnCall) {
    tc_expr(tc, e.target)
    fn_tid := expr_ty(e.target)
    if get_type(fn_tid).kind != .Function {
        highlight_lines(get_span(e.target))
        gala_panicf("Can't call a value of type %s.", tts(fn_tid))
    }

    // Read the signature only now: typing can intern types, which may move
    // the type table and leave an earlier pointer dangling.
    ty := get_type(fn_tid)
    fargs := ty.fn.args
    declared_var_ty := ty.fn.variadic_ty
    is_variadic := ty.fn.is_variadic
    is_generic := ty.fn.is_generic
    declared_ret := ty.fn.ret_ty // copied: substitution below can intern types
    gen_names := ty.fn.generic_names // explicit `fn f::<K, V>` names

    // explicit type arguments (`f::<A, B>(..)`) must match the declaration
    if len(e.type_args) > 0 {
        if len(gen_names) == 0 {
            highlight_lines(get_ctx().current_file, e.type_args_span)
            gala_panic("This function doesn't declare type parameters (declare them as `fn name::<T>(..)`).")
        }
        if len(e.type_arg_ids) != len(gen_names) {
            highlight_lines(get_ctx().current_file, e.type_args_span)
            gala_panicf("Expected %d type argument(s), got %d.", len(gen_names), len(e.type_arg_ids))
        }
    } else if len(gen_names) > 0 {
        highlight_lines(get_span(id))
        gala_panicf("This function needs explicit type arguments: `f::<%s>(..)`.",
            strings.join(gen_names, ", ", context.temp_allocator))
    }

    // make sure inferred parameter types are known before reading them
    tc_default_args(tc, fn_tid)

    has_named := false
    for a in e.args {
        if len(a.name) > 0 do has_named = true
    }

    for a in e.args {
        tc_expr(tc, a.expr)
    }

    call_args := e.args
    if is_variadic {
        // defaults and variadics can't be combined (see parse_args_dec), so
        // every fixed parameter has to be passed positionally
        if has_named {
            highlight_lines(get_span(id))
            gala_panic("Named arguments can't be used when calling a variadic function.")
        }
        if len(e.args) < len(fargs) {
            highlight_lines(get_span(id))
            gala_panicf("Too few arguments: the function takes at least %d, got %d.",
                len(fargs), len(e.args))
        }
    } else {
        // reorder named args and fill in defaults, then write the full-arity
        // call back (`e` is a copy of the union payload)
        call_args = order_call_args(id, e.args, fargs)
        new_call := e
        new_call.args = call_args
        get_ctx().exprs[id] = Expr(new_call)
    }

    // Concrete parameter types for this call, the variadic element type and
    // the return type. Generic: inferred from the arguments (and/or given
    // explicitly), then the matching version is found or created.
    param_tys := make([]TypeId, len(fargs), context.temp_allocator)
    ret_ty: TypeId
    var_ty: TypeId

    if !is_generic {
        for farg, i in fargs do param_tys[i] = farg.type
        var_ty = declared_var_ty
        ret_ty = declared_ret
    } else {
        // ASSUMPTION: infer_generic_bindings returns map[string]TypeId
        bound: map[string]TypeId
        if len(gen_names) == 0 || needs_inference(fargs, declared_var_ty, is_variadic, gen_names) {
            bound = infer_generic_bindings(id, call_args, fn_tid)
        } else {
            bound = make(map[string]TypeId, allocator=context.temp_allocator)
        }
        // explicit ones win
        for name, i in gen_names do bound[name] = e.type_arg_ids[i]

        for farg, i in fargs {
            param_tys[i] = gen_subst(farg.type, bound, id)
        }
        if is_variadic {
            var_ty = gen_subst(declared_var_ty, bound, id)
        }
        ret_ty = gen_subst(declared_ret, bound, id)

        // version key: params, the variadic element, then the explicit
        // generics (they can't be recovered from the params, e.g. a `V` that
        // only shows up in the return type)
        key_tys := param_tys
        if is_variadic || len(gen_names) > 0 {
            n := len(param_tys) + len(gen_names)
            if is_variadic do n += 1
            key_tys = make([]TypeId, n, context.temp_allocator)
            copy(key_tys, param_tys)
            k := len(param_tys)
            if is_variadic {
                key_tys[k] = var_ty
                k += 1
            }
            for name, i in gen_names do key_tys[k + i] = bound[name]
        }
        idx, added := get_or_add_fn_version(fn_tid, key_tys, ret_ty)
        if added && len(get_type(fn_tid).fn.versions) > MAX_GENERIC_VERSIONS {
            highlight_lines(get_span(id))
            gala_panicf("Generic instantiation limit reached (%d versions of one function); this looks like infinite recursion, e.g. a generic function calling itself with `^T`.",
                MAX_GENERIC_VERSIONS)
        }
        get_ctx().expr_fn_version[id] = FnVersionRef{fn = fn_tid, idx = idx}
    }

    for i in 0..<len(fargs) {
        tc_check_arg(&call_args[i], i + 1, fargs[i].name, param_tys[i])
    }

    // variadic tail, checked against the concrete element type
    if is_variadic {
        for i in len(fargs)..<len(call_args) {
            tc_check_arg(&call_args[i], i + 1, "", var_ty)
        }
    }

    get_ctx().expr_types[id] = ret_ty
}

// ---------------------------------------------------------------------------
// Statements
// ---------------------------------------------------------------------------

is_iterable :: proc(id: TypeId) -> bool {
    t := get_type(id)
    #partial switch t.kind {
    case .Slice, .String, .FixedSizeArray: return true
    }
    return false
}

get_iterable_base_type :: proc(id: TypeId) -> TypeId {
    t := get_type(id)
    #partial switch t.kind {
    case .String:         return ty_from_name("byte")
    case .FixedSizeArray: return t.fixed_size_array.type
    case .Slice:          return t.slice.type
    }
    gala_panicf("compiler bug: %s has no iteration base type.", tts(id))
}

tc_stmt :: proc(tc: ^TcContext, s: StmtId) {
    switch stmt in get_stmt(s) {
    case ForLoop: {
        last_loop := tc.in_loop // break/continue etc
        tc.in_loop = s
        oid := get_ctx().stmt_objects[s]
        tc_expr(tc, stmt.expr)
        if !is_iterable(expr_ty(stmt.expr)) {
            highlight_lines(get_span(stmt.expr))
            gala_panicf("Can't iterate over a value of type %s.", tts(expr_ty(stmt.expr)))
        }
        o := get_obj(oid)
        o.type = get_iterable_base_type(expr_ty(stmt.expr))
        tc_block(tc, stmt.block)
        tc.in_loop = last_loop
    }
    case BreakStmt, ContinueStmt: {
        loop_id := tc.in_loop
        if loop_id == nil {
            highlight_lines(get_span(s))
            gala_panic("`break` and `continue` can only be used inside a loop.")
        }
        get_ctx().break_lables[s] = loop_id.(StmtId)
    }
    case WhileLoop: {
        last_loop := tc.in_loop
        tc.in_loop = s
        tc_expr(tc, stmt.cond)
        require_bool(stmt.cond, "A loop condition")
        tc_block(tc, stmt.block)
        tc.in_loop = last_loop
    }
    case ExprId: tc_expr(tc, stmt)
    case IfElse: {
        // no in_loop change: break/continue inside an if/else target the
        // enclosing loop
        tc_expr(tc, stmt.base_con)
        require_bool(stmt.base_con, "An `if` condition")
        tc_block(tc, stmt.base_block)
        for a in stmt.alt {
            tc_expr(tc, a.cond)
            require_bool(a.cond, "An `else if` condition")
            tc_block(tc, a.block)
        }
        if stmt.has_else_block {
            tc_block(tc, stmt.else_block)
        }
    }
    case VarDec: {
        tc_expr(tc, stmt.value)
        oid := get_ctx().stmt_objects[s]
        declared := get_obj(oid).type
        if declared == nil {
            // no written type: infer it
            require_typed_value(stmt.value, "declare a variable from")
            t := expr_ty(stmt.value)
            if is_untyped(t) {
                propagate_type(get_untyped_default(t), stmt.value)
            }
            get_obj(oid).type = expr_ty(stmt.value)
        } else {
            expected := declared.(TypeId)
            r, ok, err := assignable(expected, expr_ty(stmt.value))
            if !ok {
                highlight_lines(get_span(s))
                gala_panicf("Can't initialise a variable of type %s with a value of type %s: %s.",
                    tts(expected), tts(expr_ty(stmt.value)), err)
            }
            propagate_type(r, stmt.value)
        }
    }
    case Return: {
        if !tc.in_function {
            highlight_lines(get_span(s))
            gala_panic("Can't return outside of a function.")
        }
        if e, ok := stmt.expr.(ExprId); ok {
            tc_expr(tc, e)
            if tc.fn_ret_ty == nil { // function expects no value
                highlight_lines(get_span(s))
                gala_panic("This function returns nothing, but a value was returned.")
            }
            expected := tc.fn_ret_ty.(TypeId)
            ty, cok, err := assignable(expected, expr_ty(e))
            if !cok {
                highlight_lines(get_span(e))
                gala_panicf("Return type mismatch: %s (expected %s, got %s).",
                    err, tts(expected), tts(expr_ty(e)))
            }
            propagate_type(ty, e)
        } else if ret, has_ret := tc.fn_ret_ty.(TypeId); has_ret && get_type(ret).kind != .Void {
            highlight_lines(get_span(s))
            gala_panicf("This function must return a value of type %s.", tts(ret))
        }
    }
    case Assignment: {
        tc_expr(tc, stmt.target)
        tc_expr(tc, stmt.value)

        if !can_assign_to(stmt.target) {
            highlight_lines(get_span(stmt.target))
            gala_panic("Can't assign to this expression.")
        }

        check_mutable(stmt.target, "assign to", true)

        // target must have a type
        _, tyok := get_ctx().expr_types[stmt.target]
        assert(tyok)

        t, ok, err := assignable(expr_ty(stmt.target), expr_ty(stmt.value))
        if !ok {
            highlight_lines(get_span(s))
            gala_panicf("Can't assign a value of type %s to a target of type %s: %s.",
                tts(expr_ty(stmt.value)), tts(expr_ty(stmt.target)), err)
        }
        propagate_type(t, stmt.value)
    }
    case: gala_panic("compiler bug: tc_stmt doesn't handle this statement kind")
    }
}

tc_block :: proc(tc: ^TcContext, b: Block) {
    for s in b.stmts {
        tc_stmt(tc, s)
    }
}

// ---------------------------------------------------------------------------
// Items and modules
// ---------------------------------------------------------------------------

// Same rules as a local VarDec: `:=` infers the type from the initialiser
// (untyped literals take their default type), `name: T = value` checks the
// initialiser against T. The result is written into the global's object so
// every later use (in functions or in later globals) sees a concrete type.
tc_global_var_dec :: proc(tc: ^TcContext, id: ItemId) {
    gd, gok := get_item(id).(GlobalVarDec); assert(gok)
    oid, ok := get_ctx().item_objects[id]; assert(ok)
    span := get_ctx().spans.items[id].span

    tc_expr(tc, gd.value)

    resolved_ty := get_obj(oid).type
    if resolved_ty == nil {
        require_typed_value(gd.value, "declare a global from")
        t := expr_ty(gd.value)
        if is_untyped(t) {
            propagate_type(get_untyped_default(t), gd.value)
        }
        get_obj(oid).type = expr_ty(gd.value)
    } else {
        expected := resolved_ty.(TypeId)
        t, cok, err := assignable(expected, expr_ty(gd.value))
        if !cok {
            highlight_lines(get_ctx().current_file, span)
            gala_panicf("Can't initialise a global of type %s with a value of type %s: %s.",
                tts(expected), tts(expr_ty(gd.value)), err)
        }
        propagate_type(t, gd.value)
    }
}

tc_item :: proc(tc: ^TcContext, id: ItemId) {
    item := get_item(id)
    switch i in item {
    case Forward: {}
    case TypeAlias: {}
    // nothing to check here: structs and constants are fully handled by the
    // resolver, externs have no body, globals are checked in typecheck_module
    case ConstDec, Import, StructDec, ExternFnDec, GlobalVarDec: {}
    case FnDec: {
        fn, ok := get_ctx().item_objects[id]; assert(ok)
        type := get_type(get_obj(fn).type.(TypeId))
        assert(type.kind == .Function)
        if type.fn.is_generic {
            append(tc.generic_items, id)
        } else {
            new_tc := tc^
            new_tc.in_function = true
            new_tc.fn_ret_ty = type.fn.ret_ty
            tc_block(&new_tc, i.block)
        }
    }
    case: gala_panic("compiler bug: tc_item doesn't handle this item kind")
    }
}

typecheck_module :: proc(ast: ^AST, mid: ModId, file: string) {
    generic_items := make([dynamic]ItemId, allocator=get_ctx().allocator)
    generic_exprs := make([dynamic]ExprId, allocator=get_ctx().allocator)
    tc := TcContext{
        fn_ret_ty=nil,
        in_function=false,
        generic_items=&generic_items,
        generic_exprs=&generic_exprs,
    }

    // First pass: globals, in declaration order, before any function body.
    // A `:=` global only gets its type here, and functions may use globals
    // declared further down the file, so all of them must be typed before the
    // first body is checked. A global initialiser that calls a function gets
    // that function's parameter defaults checked on demand (FnCall).
    for i in ast.items {
        if _, is_global := get_item(i).(GlobalVarDec); is_global {
            tc_global_var_dec(&tc, i)
        }
    }

    // Second pass: check parameter defaults of every module-level function
    // (extern ones included) before any body, so inferred parameter types and
    // hidden-function return types exist by the time a call needs them. Runs
    // after the globals so `fn f(x := SOME_GLOBAL)` can use them.
    for i in ast.items {
        #partial switch _ in get_item(i) {
        case FnDec, ExternFnDec: {
            fn_obj, ok := get_ctx().item_objects[i]; assert(ok)
            tc_default_args(&tc, get_obj(fn_obj).type.(TypeId))
        }
        }
    }

    // Third pass: bodies, including the hidden default-value functions, whose
    // `return <expr>;` is where the written default expression is checked.
    for i in ast.items {
        tc_item(&tc, i)
    }

    // Generic bodies aren't checked here: other modules may still add
    // versions of them. Hand them to typecheck_generics along with their
    // owner, and take the generic originals out of this module (only their
    // clones get emitted).
    for g in generic_items {
        append(&get_ctx().generic_items, g)
        get_ctx().generic_owner[g] = GenericOwner{ast = ast, mod = mid, file = file}
    }
    kept := make([dynamic]ItemId, allocator=get_ctx().allocator)
    for i in ast.items {
        if slice.contains(generic_items[:], i) {
            continue
        }
        append(&kept, i)
    }
    ast.items = kept
}

// Instantiates every requested version of every generic function, across all
// modules. Run once, after all modules are type-checked. A clone is resolved
// and checked in its owner's scope and added to the owner's items, so the
// owner's object file defines it and importers call it like any other
// function. Checking a clone can request more versions (of this or another
// generic), hence the loop.
typecheck_generics :: proc() {
    c := get_ctx()

    // clones are never generic, so these stay empty
    scratch_items := make([dynamic]ItemId, allocator=c.allocator)
    scratch_exprs := make([dynamic]ExprId, allocator=c.allocator)
    tc := TcContext{
        generic_items=&scratch_items,
        generic_exprs=&scratch_exprs,
    }

    prev_file := c.current_file
    prev_mod  := c.current_module_id

    progress := true
    for progress {
        progress = false
        for g in c.generic_items {
            owner := c.generic_owner[g]
            fn_tid := get_obj(c.item_objects[g]).type.(TypeId)

            for c.generic_done[g] < len(get_type(fn_tid).fn.versions) {
                idx := c.generic_done[g]
                c.generic_done[g] += 1

                // names and spans must resolve in the owner's module
                c.current_file = owner.file
                c.current_module_id = owner.mod

                iid := clone_item(g)
                // unique name per version, kept on the version too
                fd := c.items[iid].(FnDec)
                fd.name = mangle_version(fd.name, get_type(fn_tid).fn.versions[idx].params, idx)
                c.items[iid] = Item(fd)
                get_type(fn_tid).fn.versions[idx].llvm_fn_name = fd.name

                // fetched fresh each time: cloning/resolving appends to the tables
                get_type(fn_tid).fn.versions[idx].item = iid
                v := get_type(fn_tid).fn.versions[idx]

                append(&owner.ast.items, iid)
                resolve_generic_clone(g, iid, v)
                c.obj_modules[c.item_objects[iid]] = owner.mod

                tc_item(&tc, iid)
                progress = true
            }
        }
    }

    c.current_file = prev_file
    c.current_module_id = prev_mod

    retarget_generic_calls()
}

// Points every generic call at its clone, so codegen sees a plain call.
retarget_generic_calls :: proc() {
    for call_id, ref in get_ctx().expr_fn_version {
        v := get_type(ref.fn).fn.versions[ref.idx]
        if v.llvm_fn_name == "" do continue

        call := get_expr(call_id).(FnCall)
        oid := get_ctx().item_objects[v.item]
        get_ctx().expr_objects[call.target] = oid
        get_ctx().expr_types[call.target] = get_obj(oid).type.(TypeId)

        // codegen resolves callees by name, so rename the symbol too
        if sym, ok := get_expr(call.target).(Symbol); ok {
            sym.name = get_obj(oid).name
            get_ctx().exprs[call.target] = Expr(sym)
        }
    }
}

// ---------------------------------------------------------------------------
// Type propagation
// ---------------------------------------------------------------------------

// Pushes a final type down into an untyped expression tree.
propagate_type :: proc(ty: TypeId, expr: ExprId) {
    switch e in get_expr(expr) {
    // already typed (or fixed by their own syntax): nothing to propagate
    case UnNot, BoolLitFalse, BoolLitTrue, String, Reference, Deref, TakeSlice,
         Index, FixedSizeArray, Cast, Transmute,
         Symbol, FnCall, FnLit, FieldAccess, TypeInfoOf:
        return
    // `Foo{..}` is typed already; `{...}` takes its type (and checks its
    // fields) now that the target is known
    case AggregateLit: {
        if get_type(expr_ty(expr)).kind == .UntypedStruct {
            complete_untyped_struct(expr, ty)
        }
        return
    }
    // untyped by default: just set the type below
    case TypeIdOf, Len, SizeOf: {
    }
    case UnNegative: {
        propagate_type(ty, e.expr)
    }
    case Binop: {
        t, ok := get_ctx().expr_types[expr]
        if ok {
            if !is_untyped(t) do return // skip if it's been assigned a type
        }
        if e.kind == .Modulo && !is_int_kind(get_type(ty).kind) && !is_untyped(ty) {
            highlight_lines(get_span(expr))
            gala_panicf("Modulo needs integer operands, but the result is used as %s.", tts(ty))
        }
        propagate_type(ty, e.left)
        propagate_type(ty, e.right)
    }
    case Number: {
        t := expr_ty(expr)
        if !is_untyped(t) do return // skip if it's been assigned a type
    }
    case: gala_panic("compiler bug: propagate_type doesn't handle this expression kind")
    }
    get_ctx().expr_types[expr] = ty
}
