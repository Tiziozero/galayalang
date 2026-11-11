package main

import "core:fmt"

// Lowering: runs after typecheck_module and before cg_module.
//
// Rewrites sugar into simpler, already-typed AST so codegen only has to know
// about the core forms. Every node built here gets its type / object / span
// filled in by hand, because the typechecker has already run.
//
// Currently lowers:
//
//   for name in expr { body }
// into
//   __for_iterN := expr;               (expr[:] if expr is a fixed array)
//   __for_idxN  := 0;
//   while __for_idxN < len(__for_iterN) {
//       name := __for_iterN[__for_idxN];
//       __for_idxN += 1;               // before the body, so `continue` is safe
//       body...
//   }
// The original ForLoop StmtId is reused for the WhileLoop, so the
// break_lables entries recorded by the typechecker stay valid.
//
//   target op= value        (when `target` contains a call)
// into
//   __assign_ptrN := &target;
//   __assign_ptrN^ = __assign_ptrN^ op value;
// so the target (and its side effects) is evaluated exactly once.
//
// lower_module finishes with check_lowered (lower_check.odin).

lower_counter: int

lower_module :: proc(ast: ^AST) {
    ctx := get_ctx()
    for id in ast.items {
        #partial switch item in ctx.items[id] {
        case FnDec: {
            f := item
            f.block = lower_block(f.block)
            ctx.items[id] = Item(f)
        }
        case GlobalVarDec: {
            lower_expr(item.value)
        }
        }
    }
    check_lowered(ast)
}

// Returns a new block; a lowered statement can expand into several, so the
// statement slice has to be rebuilt.
lower_block :: proc(b: Block) -> Block {
    out := make([dynamic]StmtId, allocator=get_ctx().allocator)
    for s in b.stmts {
        lower_stmt(s, &out)
    }
    return Block{stmts=out[:]}
}

// Appends `s` (and anything it expands into) to `out`.
lower_stmt :: proc(s: StmtId, out: ^[dynamic]StmtId) {
    ctx := get_ctx()
    // copy, never a pointer: new_stmt/new_expr below can reallocate the arrays
    stmt := ctx.stmts[s]

    #partial switch v in stmt {
    case ForLoop: {
        lower_for(s, v, out)
        return
    }
    case WhileLoop: {
        w := v
        lower_expr(w.cond)
        w.block = lower_block(w.block)
        ctx.stmts[s] = Stmt(w)
    }
    case IfElse: {
        ie := v
        lower_expr(ie.base_con)
        ie.base_block = lower_block(ie.base_block)
        for &a in ie.alt {
            lower_expr(a.cond)
            a.block = lower_block(a.block)
        }
        if ie.has_else_block {
            ie.else_block = lower_block(ie.else_block)
        }
        ctx.stmts[s] = Stmt(ie)
    }
    case VarDec: {
        lower_expr(v.value)
    }
    case Assignment: {
        lower_expr(v.target)
        if bin, compound := is_compound_assignment(v); compound {
            // the target is shared with bin.left and was just handled
            lower_expr(bin.right)
        } else {
            lower_expr(v.value)
        }
        lower_assignment(s, v, out)
        return
    }
    case Return: {
        if e, ok := v.expr.(ExprId); ok {
            lower_expr(e)
        }
    }
    case ExprId: {
        lower_expr(v)
    }
    }
    append(out, s)
}

// The direct sub-expressions of `id`, for generic walks. Doesn't descend into
// function literal bodies (those are blocks, not expressions). The returned
// array lives on the temp allocator.
expr_children :: proc(id: ExprId) -> [dynamic]ExprId {
    kids := make([dynamic]ExprId, context.temp_allocator)
    #partial switch e in get_ctx().exprs[id] {
    case Binop:       { append(&kids, e.left, e.right) }
    case UnNegative:  { append(&kids, e.expr) }
    case UnNot:       { append(&kids, e.expr) }
    case Deref:       { append(&kids, e.expr) }
    case Reference:   { append(&kids, e.expr) }
    case Len:         { append(&kids, e.target) }
    case Cast:        { append(&kids, e.target) }
    case Transmute:   { append(&kids, e.target) }
    case FieldAccess: { append(&kids, e.target) }
    case Index:       { append(&kids, e.target, e.index) }
    case TakeSlice: {
        append(&kids, e.target)
        if !e.empty_start do append(&kids, e.start)
        if !e.empty_end   do append(&kids, e.end)
    }
    case FnCall: {
        append(&kids, e.target)
        for a in e.args do append(&kids, a.expr)
    }
    case FixedSizeArray: {
        for el in e.initialiser do append(&kids, el)
    }
    case AggregateLit: {
        for _, f in e.fields do append(&kids, f.expr)
    }
    }
    return kids
}

// Does evaluating `id` call a function (so evaluating it twice is observable)?
expr_has_call :: proc(id: ExprId) -> bool {
    if _, is_call := get_ctx().exprs[id].(FnCall); is_call do return true
    for k in expr_children(id) {
        if expr_has_call(k) do return true
    }
    return false
}

// Walks an expression looking for function literals (their bodies are blocks
// that may contain `for` loops or compound assignments).
lower_expr :: proc(id: ExprId) {
    ctx := get_ctx()
    if lit, is_lit := ctx.exprs[id].(FnLit); is_lit {
        lit.block = lower_block(lit.block)
        ctx.exprs[id] = Expr(lit)
        return
    }
    for k in expr_children(id) {
        lower_expr(k)
    }
}

lower_for :: proc(s: StmtId, f: ForLoop, out: ^[dynamic]StmtId) {
    ctx  := get_ctx()
    span := ctx.spans.stmts[s]
    n    := lower_counter
    lower_counter += 1

    // inner stuff first, so nested for loops are already lowered
    lower_expr(f.expr)
    body := lower_block(f.block)

    // the iterable: arrays become slices so only slice/string reach codegen
    iter_src := f.expr
    iter_ty  := expr_ty(f.expr)
    elem_ty  := get_iterable_base_type(iter_ty)
    if get_type(iter_ty).kind == .FixedSizeArray {
        slice_ty := intern_type(Type{kind=.Slice, slice={type=elem_ty}})
        iter_src = new_expr(Expr(TakeSlice{target=f.expr, empty_start=true, empty_end=true}))
        lower_set(iter_src, slice_ty, span)
        iter_ty = slice_ty
    }

    // __for_iterN := <iterable>;
    iter_obj := lower_new_local(
        fmt.aprintf("__for_iter%d", n, allocator=ctx.allocator), iter_ty, span)
    append(out, lower_vardec(iter_obj, iter_src, span))

    // __for_idxN := 0;
    idx_obj := lower_new_local(
        fmt.aprintf("__for_idx%d", n, allocator=ctx.allocator), integer_type(), span)
    append(out, lower_vardec(idx_obj, lower_number("0", span), span))

    // the user's loop variable keeps its ObjId (body Symbols point at it);
    // it's just a normal local now
    var_obj := ctx.stmt_objects[s]
    ctx.objs[var_obj].kind = .Variable
    delete_key(&ctx.stmt_objects, s)

    // cond: __for_idxN < len(__for_iterN)
    len_e := new_expr(Expr(Len{target=lower_sym(iter_obj, iter_ty, span)}))
    lower_set(len_e, integer_type(), span)
    cond := new_expr(Expr(Binop{
        kind=.Less, left=lower_sym(idx_obj, integer_type(), span), right=len_e}))
    lower_set(cond, ty_from_name("bool"), span)

    stmts := make([dynamic]StmtId, allocator=ctx.allocator)

    // name := __for_iterN[__for_idxN];
    index := new_expr(Expr(Index{
        target=lower_sym(iter_obj, iter_ty, span),
        index=lower_sym(idx_obj, integer_type(), span)}))
    lower_set(index, elem_ty, span)
    append(&stmts, lower_vardec(var_obj, index, span))

    // __for_idxN += 1;   (same shape the parser produces for `+=`)
    sum := new_expr(Expr(Binop{
        kind=.Addition,
        left=lower_sym(idx_obj, integer_type(), span),
        right=lower_number("1", span)}))
    lower_set(sum, integer_type(), span)
    inc := new_stmt(Stmt(Assignment{
        kind=.Addition, target=lower_sym(idx_obj, integer_type(), span), value=sum}))
    ctx.spans.stmts[inc] = span
    append(&stmts, inc)

    for st in body.stmts do append(&stmts, st)

    // the ForLoop becomes the WhileLoop, in place
    ctx.stmts[s] = Stmt(WhileLoop{cond=cond, block=Block{stmts=stmts[:]}})
    append(out, s)
}

// The parser turns `T op= v` into Assignment{target=T, value=Binop{left=T, right=v}}
// with the *same* ExprId on both sides. A hand-written `x = x + 1` has two
// separately parsed `x`s, so id identity is what tells them apart.
// (Assignment.kind can't: a plain `=` also carries the zero value, .Addition.)
is_compound_assignment :: proc(a: Assignment) -> (Binop, bool) {
    bin, ok := get_ctx().exprs[a.value].(Binop)
    if !ok || bin.left != a.target do return {}, false
    return bin, true
}

// Only compound assignments whose target contains a call need lowering:
//
//     a[next()] += 10;
//
// becomes
//
//     __assign_ptrN := &a[next()];
//     __assign_ptrN^ = __assign_ptrN^ + 10;
//
// If the target is already `p^`, `p` itself is stored instead of `&p^`
// (the typechecker doesn't allow `&` on a Deref, so codegen may not either).
// Targets without calls are left alone: evaluating them twice is harmless and
// the plain load/store is what you want for `i += 1`.
lower_assignment :: proc(s: StmtId, a: Assignment, out: ^[dynamic]StmtId) {
    ctx := get_ctx()
    bin, compound := is_compound_assignment(a)
    if !compound || !expr_has_call(a.target) {
        append(out, s)
        return
    }

    span := ctx.spans.stmts[s]
    n    := lower_counter
    lower_counter += 1

    val_ty := expr_ty(a.target)
    ptr_src: ExprId
    ptr_ty:  TypeId
    if d, is_deref := ctx.exprs[a.target].(Deref); is_deref {
        ptr_src = d.expr
        ptr_ty  = expr_ty(d.expr)
    } else {
        ptr_src = new_expr(Expr(Reference{expr=a.target}))
        ptr_ty  = intern_type(Type{kind=.Pointer, ptr=val_ty})
        lower_set(ptr_src, ptr_ty, span)
    }

    // __assign_ptrN := <address of target>;
    ptr_obj := lower_new_local(
        fmt.aprintf("__assign_ptr%d", n, allocator=ctx.allocator), ptr_ty, span)
    append(out, lower_vardec(ptr_obj, ptr_src, span))

    // The Binop keeps its id (so its type and span survive); only its left
    // side changes.
    b := bin
    b.left = lower_deref(ptr_obj, ptr_ty, val_ty, span)
    ctx.exprs[a.value] = Expr(b)

    // __assign_ptrN^ = <that binop>;
    a2 := a
    a2.target = lower_deref(ptr_obj, ptr_ty, val_ty, span)
    ctx.stmts[s] = Stmt(a2)
    append(out, s)
}

// ---- helpers for building already-typed nodes ----

lower_set :: proc(id: ExprId, ty: TypeId, span: SpanStruct) {
    ctx := get_ctx()
    ctx.expr_types[id] = ty
    ctx.spans.exprs[id] = span
}

lower_new_local :: proc(name: string, ty: TypeId, span: SpanStruct) -> ObjId {
    ctx := get_ctx()
    append(&ctx.objs, Object{kind=.Variable, name=name, type=ty})
    oid := ObjId(len(ctx.objs) - 1)
    ctx.obj_modules[oid] = ctx.current_module_id
    ctx.spans.objs_decs[oid] = span
    return oid
}

lower_sym :: proc(oid: ObjId, ty: TypeId, span: SpanStruct) -> ExprId {
    ctx := get_ctx()
    id := new_expr(Expr(Symbol{ctx.objs[oid].name}))
    ctx.expr_objects[id] = oid
    lower_set(id, ty, span)
    return id
}

// `ptr_obj^`, typed as `val_ty`
lower_deref :: proc(ptr_obj: ObjId, ptr_ty, val_ty: TypeId, span: SpanStruct) -> ExprId {
    id := new_expr(Expr(Deref{expr=lower_sym(ptr_obj, ptr_ty, span)}))
    lower_set(id, val_ty, span)
    return id
}

lower_number :: proc(text: string, span: SpanStruct) -> ExprId {
    id := new_expr(Expr(Number{text}))
    lower_set(id, integer_type(), span)
    return id
}

// `name := value;` bound to an existing object
lower_vardec :: proc(oid: ObjId, value: ExprId, span: SpanStruct) -> StmtId {
    ctx := get_ctx()
    id := new_stmt(Stmt(VarDec{name=ctx.objs[oid].name, type=nil, value=value}))
    ctx.stmt_objects[id] = oid
    ctx.spans.stmts[id] = span
    return id
}

// Sanity checks run at the end of lower_module. They don't change anything;
// they panic, pointing at the node, if the AST codegen is about to see breaks
// an invariant:
//
//   - no ForLoop is left (codegen no longer handles them)
//   - every reachable expression has a type, and it isn't untyped
//   - every Symbol has an object, every VarDec has an object with a type
//   - every break/continue points at a loop (or if) statement that still exists
//   - no compound assignment still evaluates a call-containing target twice
//
// Most of these are really checks on lowering itself: a node built by hand
// without lower_set / lower_sym shows up here instead of as bad IR.
//
// TODO once FnLit hoisting exists: also assert that no FnLit remains.

check_lowered :: proc(ast: ^AST) {
    ctx := get_ctx()
    for id in ast.items {
        #partial switch item in ctx.items[id] {
        case FnDec:        { check_block(item.block) }
        case GlobalVarDec: { check_expr(item.value) }
        }
    }
}

check_block :: proc(b: Block) {
    for s in b.stmts {
        check_stmt(s)
    }
}

check_stmt :: proc(s: StmtId) {
    ctx := get_ctx()
    #partial switch v in ctx.stmts[s] {
    case ForLoop: {
        highlight_lines(get_span(s))
        gala_panic("lowering left a `for` loop behind.")
    }
    case WhileLoop: {
        check_expr(v.cond)
        check_block(v.block)
    }
    case IfElse: {
        check_expr(v.base_con)
        check_block(v.base_block)
        for a in v.alt {
            check_expr(a.cond)
            check_block(a.block)
        }
        if v.has_else_block {
            check_block(v.else_block)
        }
    }
    case VarDec: {
        oid, has_obj := ctx.stmt_objects[s]
        if !has_obj {
            highlight_lines(get_span(s))
            gala_panic("variable declaration has no object after lowering.")
        }
        if _, typed := ctx.objs[oid].type.(TypeId); !typed {
            highlight_lines(get_span(s))
            gala_panicf("variable \"%s\" has no type after lowering.", ctx.objs[oid].name)
        }
        check_expr(v.value)
    }
    case Assignment: {
        check_expr(v.target)
        check_expr(v.value)
        if _, compound := is_compound_assignment(v); compound && expr_has_call(v.target) {
            highlight_lines(get_span(s))
            gala_panic("compound assignment still evaluates its target twice.")
        }
    }
    case Return: {
        if e, ok := v.expr.(ExprId); ok {
            check_expr(e)
        }
    }
    case ExprId: {
        check_expr(v)
    }
    case BreakStmt, ContinueStmt: {
        label, has_label := ctx.break_lables[s]
        if !has_label {
            highlight_lines(get_span(s))
            gala_panic("break/continue has no target statement.")
        }
        #partial switch _ in ctx.stmts[label] {
        case WhileLoop, IfElse: // ok
        case:
            highlight_lines(get_span(s))
            gala_panic("break/continue targets a statement that isn't a loop or if.")
        }
    }
    }
}

check_expr :: proc(id: ExprId) {
    ctx := get_ctx()

    ty, typed := ctx.expr_types[id]
    if !typed {
        highlight_lines(get_span(id))
        gala_panic("expression has no type after lowering.")
    }
    if is_untyped(ty) {
        highlight_lines(get_span(id))
        gala_panicf("expression is still untyped (%s) after lowering.", tts(ty))
    }

    #partial switch e in ctx.exprs[id] {
    case Symbol: {
        if _, has_obj := ctx.expr_objects[id]; !has_obj {
            highlight_lines(get_span(id))
            gala_panicf("\"%s\" has no object after lowering.", e.name)
        }
    }
    case FnLit: {
        check_block(e.block)
    }
    }

    for k in expr_children(id) {
        check_expr(k)
    }
}
