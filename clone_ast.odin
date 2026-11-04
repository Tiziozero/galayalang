package main

// Deep clones of AST nodes. A clone gets fresh ids for itself and every
// child node it owns, and its span is copied so error highlighting still
// works. Resolution state (expr_types, expr_objects, item_types, ...) is
// NOT copied: clone the parsed AST, then resolve and typecheck the copy.
//
// TypeSpecifiers hold no node ids, so they're copied as-is (the ^TypeSpecifier
// pointers inside them stay shared). The one exception is the size expression
// of a fixed-size array type (`[MAX_COUNT]T`, FixedArreySpecifier.size_expr):
// that ExprId is shared between the original and its clones. That's fine
// because it is only ever read: const_eval folds it and never resolves or
// rewrites it.

clone_expr :: proc(id: ExprId) -> ExprId {
    c := get_ctx()
    alloc := c.allocator

    node := c.exprs[id]
    new_id := ExprId(len(c.exprs))
    append(&c.exprs, node) // reserve the slot first; children get appended after it
    if s, ok := c.spans.exprs[id]; ok do c.spans.exprs[new_id] = s

    switch &v in node {
    case StructLit:
        old := v.fields
        v.fields = make(type_of(v.fields), alloc)
        for k, f in old {
            nf := f
            nf.expr = clone_expr(f.expr)
            v.fields[k] = nf
        }
        pos := make(type_of(v.positional), len(v.positional), alloc)
        for p, i in v.positional {
            pos[i] = p
            pos[i].expr = clone_expr(p.expr)
        }
        v.positional = pos
    case Binop:
        v.left = clone_expr(v.left)
        v.right = clone_expr(v.right)
    case Number:
    case Symbol:
    case FnCall:
        v.target = clone_expr(v.target)
        args := make([dynamic]FnCallArg, 0, len(v.args), alloc)
        for a in v.args {
            na := a
            na.expr = clone_expr(a.expr)
            append(&args, na)
        }
        v.args = args
    case FieldAccess:
        v.target = clone_expr(v.target)
    case Index:
        v.target = clone_expr(v.target)
        v.index = clone_expr(v.index)
    case TakeSlice:
        // empty_start / empty_end say whether start / end are real
        // expressions, so only clone the ones that are
        v.target = clone_expr(v.target)
        if !v.empty_start do v.start = clone_expr(v.start)
        if !v.empty_end   do v.end   = clone_expr(v.end)
    case Cast:
        v.target = clone_expr(v.target)
    case Transmute:
        v.target = clone_expr(v.target)
    case TypeIdOf:
        v.expr = clone_maybe_expr(v.expr)
    case TypeInfoOf:
        v.expr = clone_expr(v.expr)
    case Reference:
        v.expr = clone_expr(v.expr)
    case Deref:
        v.expr = clone_expr(v.expr)
    case UnNot:
        v.expr = clone_expr(v.expr)
    case UnNegative:
        v.expr = clone_expr(v.expr)
    case FixedSizeArray:
        // `[MAX_COUNT]byte{}`: the size expression is folded by the resolver
        v.size_expr = clone_maybe_expr(v.size_expr)
        v.initialiser = clone_exprs(v.initialiser)
    case String:
    case Len:
        v.target = clone_expr(v.target)
    case SizeOf:
        v.expr = clone_maybe_expr(v.expr)
    case BoolLitTrue:
    case BoolLitFalse:
    case FnLit:
        v.signature = clone_sig(v.signature)
        v.block = clone_block(v.block)
    }

    c.exprs[new_id] = node
    return new_id
}

clone_stmt :: proc(id: StmtId) -> StmtId {
    c := get_ctx()
    alloc := c.allocator

    node := c.stmts[id]
    new_id := StmtId(len(c.stmts))
    append(&c.stmts, node)
    if s, ok := c.spans.stmts[id]; ok do c.spans.stmts[new_id] = s

    switch &v in node {
    case VarDec:
        v.value = clone_expr(v.value)
    case Assignment:
        v.target = clone_expr(v.target)
        v.value = clone_expr(v.value)
    case Return:
        v.expr = clone_maybe_expr(v.expr)
    case ExprId:
        node = ExprId(clone_expr(v))
    case IfElse:
        v.base_con = clone_expr(v.base_con)
        v.base_block = clone_block(v.base_block)
        alt := make([]AltCon, len(v.alt), alloc)
        for a, i in v.alt {
            alt[i] = AltCon{cond = clone_expr(a.cond), block = clone_block(a.block)}
        }
        v.alt = alt
        if v.has_else_block do v.else_block = clone_block(v.else_block)
    case WhileLoop:
        v.cond = clone_expr(v.cond)
        v.block = clone_block(v.block)
    case ForLoop:
        v.expr = clone_expr(v.expr)
        v.block = clone_block(v.block)
    case BreakStmt:
    case ContinueStmt:
    }

    c.stmts[new_id] = node
    return new_id
}

clone_item :: proc(id: ItemId) -> ItemId {
    c := get_ctx()

    node := c.items[id]
    new_id := ItemId(len(c.items))
    append(&c.items, node)
    if s, ok := c.spans.items[id]; ok do c.spans.items[new_id] = s

    switch &v in node {
    case TypeAlias: {}
    case Import:
    case StructDec:
    case FnDec:
        v.signature = clone_sig(v.signature)
        v.block = clone_block(v.block)
    case ExternFnDec:
        v.signature = clone_sig(v.signature)
    case GlobalVarDec:
        v.value = clone_expr(v.value)
    case ConstDec:
        // folded and removed from the item list before anything is cloned,
        // so this isn't reached today; here for the exhaustive switch
        v.value = clone_expr(v.value)
    }

    c.items[new_id] = node
    return new_id
}

// ---- helpers ----

clone_maybe_expr :: proc(m: Maybe(ExprId)) -> Maybe(ExprId) {
    if e, ok := m.?; ok do return clone_expr(e)
    return nil
}

clone_exprs :: proc(src: []ExprId) -> []ExprId {
    out := make([]ExprId, len(src), get_ctx().allocator)
    for e, i in src do out[i] = clone_expr(e)
    return out
}

clone_block :: proc(b: Block) -> Block {
    out := make([]StmtId, len(b.stmts), get_ctx().allocator)
    for s, i in b.stmts do out[i] = clone_stmt(s)
    return Block{stmts = out}
}

// Default values are expressions, so they need cloning with the signature.
clone_sig :: proc(sig: FnDecSignature) -> FnDecSignature {
    out := sig
    out.args = make([]FnDecArg, len(sig.args), get_ctx().allocator)
    for a, i in sig.args {
        out.args[i] = a
        out.args[i].default     = clone_maybe_expr(a.default)
        out.args[i].default_src = clone_maybe_expr(a.default_src)
    }
    return out
}
