// 100% claude
package main

// ===========================================================================
// Allocation helpers: go through the parser's add_* so spans are registered,
// copying the span of the node being cloned.
// ===========================================================================

put_expr :: proc(c: ^Context, e: Expr, from: ExprId) -> ExprId {
    return add_expr(e, c.spans.exprs[from])
}
put_stmt :: proc(c: ^Context, s: Stmt, from: StmtId) -> StmtId {
    return add_stmt(s, c.spans.stmts[from])
}
put_item :: proc(c: ^Context, it: Item, from: ItemId) -> ItemId {
    return add_item(it, c.spans.items[from])
}

// ===========================================================================
// Entry point
// ===========================================================================

// Clones a generic fn item for instantiation. Output differences from the source:
//  - every `$T` specifier becomes a plain `T` (BaseType), so the instance scope
//    can alias T -> concrete type instead of the clone re-declaring a generic
//  - fresh Expr/Stmt ids, resolver outputs reset
// `generics` is copied as written; the caller clears it. Registering the
// object/module and binding T..., is instantiate_fn's job.
clone_fn_item :: proc(c: ^Context, src: ItemId) -> ItemId {
    fd := get_item(src).(FnDec)
    return put_item(c, clone_fn_dec(c, fd), src)
}

clone_fn_dec :: proc(c: ^Context, f: FnDec) -> FnDec {
    n := f
    n.signature = clone_signature(c, f.signature)
    n.block = clone_block(c, f.block)
    return n
}

// ===========================================================================
// Signatures
// ===========================================================================

clone_signature :: proc(c: ^Context, s: FnDecSignature) -> FnDecSignature {
    n := s
    n.ret_ty = clone_maybe_type(c, s.ret_ty)
    n.variadic_ty = clone_type_spec(c, s.variadic_ty)

    if s.params != nil {
        n.params = make([]FnDecParam, len(s.params))
        for a, i in s.params {
            na := a
            na.t = clone_type_spec(c, a.t)
            // NOTE: `default` is a call to a hidden thunk `__default_N`, which is
            // shared by name, not cloned. See the notes under this file.
            na.default = clone_maybe_expr(c, a.default)
            n.params[i] = na
        }
    }
    if s.generics != nil {
        n.generics = make([]GenericParam, len(s.generics))
        copy(n.generics, s.generics)
    }
    return n
}

// ===========================================================================
// Type specifiers
// ===========================================================================

clone_type_spec_ptr :: proc(c: ^Context, p: ^TypeSpecifier) -> ^TypeSpecifier {
    if p == nil do return nil
    n := new(TypeSpecifier)
    n^ = clone_type_spec(c, p^)
    return n
}

clone_maybe_type :: proc(c: ^Context, m: Maybe(TypeSpecifier)) -> Maybe(TypeSpecifier) {
    if t, ok := m.?; ok do return clone_type_spec(c, t)
    return nil
}

clone_struct_fields :: proc(c: ^Context, fs: []StructField) -> []StructField {
    if fs == nil do return nil
    out := make([]StructField, len(fs))
    for f, i in fs {
        out[i] = f
        out[i].t = clone_type_spec(c, f.t)
    }
    return out
}

clone_type_spec :: proc(c: ^Context, ts: TypeSpecifier) -> TypeSpecifier {
    switch t in ts {
    case nil:
        return nil
    case BaseType, AnySpecifier:
        return t
    case GenericSpecifier:
        // `$T` declares a generic; in an instance T is just a name bound in scope
        return BaseType{ident = t.name, span = t.span}
    case PointerType:
        n := t
        n.ptr = clone_type_spec_ptr(c, t.ptr)
        return n
    case FixedArreySpecifier:
        n := t
        n.size_expr = clone_maybe_expr(c, t.size_expr)
        n.base = clone_type_spec_ptr(c, t.base)
        return n
    case SliceSpecifier:
        n := t
        n.base = clone_type_spec_ptr(c, t.base)
        return n
    case GenericInstSpecifier:
        n := t
        n.args = make([dynamic]TypeSpecifier, 0, len(t.args))
        for a in t.args do append(&n.args, clone_type_spec(c, a))
        return n
    case FnSpecifier:
        n := t
        if t.signature != nil {
            sig := new(FnDecSignature)
            sig^ = clone_signature(c, t.signature^)
            n.signature = sig
        }
        return n
    case StructSpecifier:
        n := t
        n.fields = clone_struct_fields(c, t.fields)
        return n
    case UnionSpecifier:
        n := t
        n.fields = clone_struct_fields(c, t.fields)
        return n
    }
    panic("clone_type_spec: unhandled variant")
}

// ===========================================================================
// Expressions
// ===========================================================================

clone_maybe_expr :: proc(c: ^Context, m: Maybe(ExprId)) -> Maybe(ExprId) {
    if e, ok := m.?; ok do return clone_expr(c, e)
    return nil
}

clone_exprs :: proc(c: ^Context, es: []ExprId) -> []ExprId {
    if es == nil do return nil
    out := make([]ExprId, len(es))
    for e, i in es do out[i] = clone_expr(c, e)
    return out
}

clone_field_init :: proc(c: ^Context, f: FieldInit) -> FieldInit {
    n := f
    n.expr = clone_expr(c, f.expr)
    return n
}

clone_expr :: proc(c: ^Context, id: ExprId) -> ExprId {
    switch e in get_expr(id) {
    case Number, Symbol, String, BoolLitTrue, BoolLitFalse:
        return put_expr(c, e, id)

    case Binop:
        n := e
        n.left = clone_expr(c, e.left)
        n.right = clone_expr(c, e.right)
        return put_expr(c, n, id)

    case UnNegative:
        n := e; n.expr = clone_expr(c, e.expr)
        return put_expr(c, n, id)
    case UnNot:
        n := e; n.expr = clone_expr(c, e.expr)
        return put_expr(c, n, id)
    case Reference:
        n := e; n.expr = clone_expr(c, e.expr)
        return put_expr(c, n, id)
    case Deref:
        n := e; n.expr = clone_expr(c, e.expr)
        return put_expr(c, n, id)
    case TypeInfoOf:
        n := e; n.expr = clone_expr(c, e.expr)
        return put_expr(c, n, id)
    case Len:
        n := e; n.target = clone_expr(c, e.target)
        return put_expr(c, n, id)

    case Cast:
        n := e
        n.to = clone_type_spec(c, e.to)
        n.target = clone_expr(c, e.target)
        return put_expr(c, n, id)
    case Transmute:
        n := e
        n.to = clone_type_spec(c, e.to)
        n.target = clone_expr(c, e.target)
        return put_expr(c, n, id)

    case TypeIdOf:
        n := e
        n.t = clone_maybe_type(c, e.t)
        n.expr = clone_maybe_expr(c, e.expr)
        n.names_type = false // resolver output
        return put_expr(c, n, id)
    case SizeOf:
        n := e
        n.t = clone_maybe_type(c, e.t)
        n.expr = clone_maybe_expr(c, e.expr)
        n.names_type = false // resolver output
        return put_expr(c, n, id)

    case Index:
        n := e
        n.target = clone_expr(c, e.target)
        n.index = clone_expr(c, e.index)
        return put_expr(c, n, id)

    case TakeSlice:
        n := e
        n.target = clone_expr(c, e.target)
        // an omitted bound has no real expr behind it (the parser leaves it zero)
        if !e.empty_start do n.start = clone_expr(c, e.start)
        if !e.empty_end   do n.end   = clone_expr(c, e.end)
        return put_expr(c, n, id)

    case FieldAccess:
        n := e
        n.target = clone_expr(c, e.target)
        return put_expr(c, n, id)

    case FnCall:
        n := e
        n.target = clone_expr(c, e.target)
        n.args = make([dynamic]FnCallArg, 0, len(e.args))
        for a in e.args {
            na := a
            na.expr = clone_expr(c, a.expr)
            append(&n.args, na)
        }
        if e.type_args != nil {
            n.type_args = make([]TypeSpecifier, len(e.type_args))
            for t, i in e.type_args do n.type_args[i] = clone_type_spec(c, t)
        }
        n.type_arg_ids = nil // resolver output
        return put_expr(c, n, id)

    case AggregateLit:
        n := e
        n.ty = clone_maybe_type(c, e.ty) // maight be untyped
        n.fields = nil
        if e.fields != nil {
            n.fields = make(map[string]FieldInit, len(e.fields))
            for k, v in e.fields do n.fields[k] = clone_field_init(c, v)
        }
        n.positional = nil
        if e.positional != nil {
            n.positional = make([]FieldInit, len(e.positional))
            for f, i in e.positional do n.positional[i] = clone_field_init(c, f)
        }
        return put_expr(c, n, id)

    case FixedSizeArray:
        n := e
        n.size_expr = clone_maybe_expr(c, e.size_expr)
        n.ty = clone_type_spec(c, e.ty)
        n.initialiser = clone_exprs(c, e.initialiser)
        return put_expr(c, n, id)

    case FnLit:
        n := e
        n.signature = clone_signature(c, e.signature)
        n.block = clone_block(c, e.block)
        return put_expr(c, n, id)
    }
    panic("clone_expr: unhandled variant")
}

// ===========================================================================
// Statements
// ===========================================================================

clone_block :: proc(c: ^Context, b: Block) -> Block {
    if b.stmts == nil do return b
    n := Block{stmts = make([]StmtId, len(b.stmts))}
    for s, i in b.stmts do n.stmts[i] = clone_stmt(c, s)
    return n
}

clone_stmt :: proc(c: ^Context, id: StmtId) -> StmtId {
    switch s in get_stmt(id) {
    case VarDec:
        n := s
        n.type = clone_maybe_type(c, s.type)
        n.value = clone_expr(c, s.value)
        return put_stmt(c, n, id)

    case Assignment:
        // compound `a += b`: the parser stores target and Binop.left as the SAME
        // ExprId. Each gets its own clone here, which is fine.
        n := s
        n.target = clone_expr(c, s.target)
        n.value = clone_expr(c, s.value)
        return put_stmt(c, n, id)

    case Return:
        n := s
        n.expr = clone_maybe_expr(c, s.expr)
        return put_stmt(c, n, id)

    case ExprId:
        return put_stmt(c, clone_expr(c, s), id)

    case IfElse:
        n := s
        n.base_cond = clone_expr(c, s.base_cond)
        n.base_block = clone_block(c, s.base_block)
        if s.alt != nil {
            n.alt = make([]AltCon, len(s.alt))
            for a, i in s.alt {
                n.alt[i] = AltCon{cond = clone_expr(c, a.cond), block = clone_block(c, a.block)}
            }
        }
        if s.has_else_block do n.else_block = clone_block(c, s.else_block)
        return put_stmt(c, n, id)

    case WhileLoop:
        n := s
        n.cond = clone_expr(c, s.cond)
        n.block = clone_block(c, s.block)
        return put_stmt(c, n, id)

    case ForLoop:
        n := s
        n.names = make(type_of(s.names), len(s.names))
        copy(n.names, s.names)
        for &nm in n.names {
            zero: type_of(nm.oid)
            nm.oid = zero // resolver output
        }
        n.expr = clone_expr(c, s.expr)
        n.block = clone_block(c, s.block)
        return put_stmt(c, n, id)

    case BreakStmt, ContinueStmt:
        return put_stmt(c, s, id)
    }
    panic("clone_stmt: unhandled variant")
}
