package main

import "core:strings"
import "core:strconv"

// ============================================================================
// Small emit helpers
// ============================================================================

// Builds a `{ ptr, <len_ty> }` pair (slice / string / boxed `any` header)
// from already-evaluated operands and returns the SSA value.
cg_make_pair :: proc(c: ^CGCtx, ptr, len_ty, len: string) -> string {
    v1 := new_tmp(c)
    v2 := new_tmp(c)
    cwritefln(c, "\t%s = insertvalue {{ ptr, %s }} undef, ptr %s, 0", v1, len_ty, ptr)
    cwritefln(c, "\t%s = insertvalue {{ ptr, %s }} %s, %s %s, 1", v2, len_ty, v1, len_ty, len)
    return v2
}

// Index of the field called `name` in struct type `ty`.
struct_field_index :: proc(ty: ^Type, name: string) -> int {
    for f, k in ty.structure.fields {
        if f.name == name do return k
    }
    gala_panic("struct_field_index: no such field", name)
    // return -1
}

// Boxes `val` (of concrete type `concrete_ty`) into an `any` value:
// { ptr, i64 } = { address of a spilled copy of val, typeid_of(concrete_ty) }.
// `any` is opaque — the callee only ever sees a rawptr + tag, never the
// value directly — so we always spill to get an address, even for values
// that would otherwise happily live in a register.
//
// Returns the boxed SSA value and its LLVM type string directly: the ABI
// shape of a boxed `any` is a fixed, compile-time-known constant, so there
// is nothing to look up and no dependency on `any` being a named type.
cg_box_any :: proc(c: ^CGCtx, val: string, concrete_ty: TypeId) -> (string, string) {
    concrete_ty_str := ty_to_llvm_str(c, concrete_ty)

    // entry-block slot: a boxed `any` inside a loop must not grow the stack
    slot := new_entry_alloca(c, concrete_ty_str)
    cwritefln(c, "\tstore %s %s, ptr %s", concrete_ty_str, val, slot)

    boxed := cg_make_pair(c, slot, "i64", aprintf(c, "%d", typeid_of(concrete_ty)))
    return boxed, "{ ptr, i64 }"
}

// Function literals (lambdas) are lowered to ordinary, separate LLVM
// functions — a `define` can't be nested inside another function's body, so
// the lambda is generated into its own string builder and parked in
// c.lambdas; cg_module appends every parked definition after the regular
// items. The expression itself evaluates to the lambda's address
// (`@gala.lambda.N`), which is just a `ptr` — the same representation every
// other function value already has, so calling it through a variable, a
// struct field, an argument, or immediately all work unchanged.
//
// Lambdas don't capture (the resolver only lets the body see module-level
// names), so the body's scope is parented to the module's root scope rather
// than to whatever scope the lambda expression happens to sit in.
//
// They're emitted with `internal` linkage so the per-lambda names only have
// to be unique inside this module (each module is compiled to its own .o).
cg_fn_lit :: proc(c: ^CGCtx, id: ExprId, e: FnLit) -> CGExprRes {
    fn_type_id := expr_ty(id)
    name := aprintf(c, "gala.lambda.%d", next_tmp_index(c))

    lambda_b: strings.Builder
    strings.builder_init(&lambda_b, get_ctx().allocator)

    // everything the body generation mutates, saved so the enclosing
    // function carries on exactly where it left off
    // (c.allocas is saved/restored by cg_fn_definition itself)
    saved_b := c.b
    saved_scope := c.scope
    saved_break := c.cur_break_label
    saved_continue := c.cur_continue_label

    // find the root (module) scope and keep a stable heap copy to parent to.
    // (maps are reference types, so the copy sees the same declarations.)
    root := &c.scope
    for root.parent != nil {
        root = root.parent
    }
    root_copy := new(CGScope, get_ctx().allocator)
    root_copy^ = root^

    c.b = &lambda_b
    c.scope = new_gcscope(root_copy)
    // a break/continue can't target a loop outside the lambda
    c.cur_break_label = ""
    c.cur_continue_label = ""

    cg_fn_definition(c, fn_type_id, name, e.block, id, internal = true)

    c.b = saved_b
    c.scope = saved_scope
    c.cur_break_label = saved_break
    c.cur_continue_label = saved_continue

    append(&c.lambdas, strings.to_string(lambda_b))

    return {kind=.Value, v=aprintf(c, "@%s", name), id=id}
}

// ============================================================================
// Expression results
// ============================================================================

// Collapses a CGExprRes into a single SSA operand. Returns false for
// expressions that produce no value (void calls).
//
// NOTE: for a *large aggregate* (see is_memory_type) this is the slow path —
// it materialises the whole value as one SSA value (a full `load`). Code that
// may see big structs/arrays should use cg_expr_into / cg_value_addr instead
// and only come here for scalars and small values.
reduce_expr_to_single_value :: proc(c: ^CGCtx, e: CGExprRes) -> (string, bool) {
    switch e.kind {
    case .Address, .Value, .Number:
        return e.v, true
    case .Place: {
        // value lives in memory at e.v; load it (whole thing)
        t := new_tmp(c)
        cwritefln(c, "\t%s = load %s, ptr %s", t, ty_to_llvm_str(c, expr_ty(e.id)), e.v)
        return t, true
    }
    case .Struct: {
        // cg_expr(StructLit) already evaluated every field and stored the
        // reduced SSA values in struct_lit.fields (declaration order) —
        // reuse them instead of generating each field expression a second
        // time (which would also duplicate side effects).
        tid := expr_ty(e.id)
        ty_str := ty_to_llvm_str(c, tid)

        cur := "undef" // starting aggregate — a literal LLVM keyword, not a register
        for field, i in get_type(tid).structure.fields {
            next := new_tmp(c)
            cwritefln(c, "\t%s = insertvalue %s %s, %s %s, %d",
                next, ty_str, cur, ty_to_llvm_str(c, field.type), e.struct_lit.fields[i], i)
            cur = next
        }
        return cur, true
    }
    case .Binop: {
        t := new_tmp(c)
        cwritefln(c, "\t%s = %s", t, e.v)
        return t, true
    }
    case .None:
        return "", false
    case .Invalid:
        gala_panic("reduce_expr_to_single_value: invalid expression result")
    }
    unreachable()
}

// `{name = value}` for a union: zero the whole union if the member is
// smaller (so padding is deterministic), then write the one member at
// offset 0. The typechecker guarantees exactly one field.
cg_union_lit_into :: proc(c: ^CGCtx, lit: StructLit, tid: TypeId, dest: string) {
    for _, f in lit.fields {
        if type_size(expr_ty(f.expr)) < type_size(tid) {
            cg_memset(c, dest, 0, int(type_size(tid)))
        }
        cg_expr_into(c, f.expr, dest)
    }
}

// Evaluates expression `id` and writes its value into memory at `dest`
// (an LLVM pointer operand, e.g. "%x.3", "%.sret", "@global").
//
// This is the destination-passing path for aggregates: instead of building
// a value and copying it, literals and calls write straight into `dest`,
// and anything that already lives in memory is `memcpy`'d. Large aggregates
// never become SSA values on this path.
//
// Not safe if `dest` is also read by the expression (e.g. `t = {a = t.b}`):
// callers that assign into an existing object evaluate into a temporary
// first (see cg_stmt's Assignment).
cg_expr_into :: proc(c: ^CGCtx, id: ExprId, dest: string) {
    tid := expr_ty(id)
    ty_str := ty_to_llvm_str(c, tid)
    if get_type(tid).kind == .Union {
        if lit, ok := get_expr(id).(StructLit); ok {
            cg_union_lit_into(c, lit, tid, dest)
            return
        }
    }

    if is_memory_type(tid) {
        #partial switch e in get_expr(id) {
        case StructLit: {
            st := get_type(tid)
            for f, k in st.structure.fields {
                fptr := new_tmp(c)
                cwritefln(c, "\t%s = getelementptr inbounds %s, ptr %s, i32 0, i32 %d",
                    fptr, ty_str, dest, k)
                if fl, ok := e.fields[f.name]; ok {
                    cg_expr_into(c, fl.expr, fptr)
                } else {
                    // field not mentioned in the literal -> zero it
                    cg_memset(c, fptr, 0, int(type_size(f.type)))
                }
            }
            return
        }
        case FixedSizeArray: {
            at := get_type(tid).fixed_size_array
            // `{}` and short initialisers: zero everything first, then
            // overwrite the elements that were given.
            if len(e.initialiser) < int(at.size) {
                cg_memset(c, dest, 0, int(type_size(tid)))
            }
            elem_ty := ty_to_llvm_str(c, at.type)
            for el, i in e.initialiser {
                p := new_tmp(c)
                cwritefln(c, "\t%s = getelementptr inbounds %s, ptr %s, i64 %d",
                    p, elem_ty, dest, i)
                cg_expr_into(c, el, p)
            }
            return
        }
        case FnCall: {
            // large returns are sret: hand the callee `dest` directly
            r := cg_fn_call(c, id, e, dest)
            if r.kind == .Place {
                if r.v != dest {
                    cg_memcpy(c, dest, r.v, int(type_size(tid)))
                }
            } else {
                v, returns := reduce_expr_to_single_value(c, r)
                assert(returns)
                cwritefln(c, "\tstore %s %s, ptr %s", ty_str, v, dest)
            }
            return
        }
        case Symbol, FieldAccess, Index, Deref: {
            if is_addressable(c, id) {
                cg_memcpy(c, dest, cg_addr(c, id), int(type_size(tid)))
                return
            }
        }
        }
    }

    r := cg_expr(c, id)
    if r.kind == .Place {
        cg_memcpy(c, dest, r.v, int(type_size(tid)))
        return
    }
    v, returns := reduce_expr_to_single_value(c, r)
    assert(returns)
    cwritefln(c, "\tstore %s %s, ptr %s", ty_str, v, dest)
}

// Short-circuit && / ||. The result goes through an entry-block i1 slot
// rather than a phi, so nested logical ops (which add blocks) stay correct.
cg_logical :: proc(c: ^CGCtx, e: Binop) -> CGExprRes {
    is_and := e.kind == .LogicalAnd

    slot := new_entry_alloca(c, "i1")
    n := next_tmp_index(c)
    rhs_l := aprintf(c, "logic.rhs.%d", n)
    end_l := aprintf(c, "logic.end.%d", n)

    l, lok := reduce_expr_to_single_value(c, cg_expr(c, e.left))
    assert(lok)
    cwritefln(c, "\tstore i1 %s, ptr %s", l, slot)
    if is_and {
        // false -> result already stored, skip rhs
        cwritefln(c, "\tbr i1 %s, label %%%s, label %%%s", l, rhs_l, end_l)
    } else {
        // true -> result already stored, skip rhs
        cwritefln(c, "\tbr i1 %s, label %%%s, label %%%s", l, end_l, rhs_l)
    }

    cwritefln(c, "%s:", rhs_l)
    r, rok := reduce_expr_to_single_value(c, cg_expr(c, e.right))
    assert(rok)
    cwritefln(c, "\tstore i1 %s, ptr %s", r, slot)
    cwritefln(c, "\tbr label %%%s", end_l)

    cwritefln(c, "%s:", end_l)
    t := new_tmp(c)
    cwritefln(c, "\t%s = load i1, ptr %s", t, slot)
    return {kind=.Value, v=t}
}

// ============================================================================
// len / slicing
// ============================================================================

// Length of a string / slice / fixed-size array expression. Returns the
// operand text and whether it is a compile-time constant (arrays).
//
// Fixed arrays never have their contents loaded: an addressable array isn't
// evaluated at all, and a non-addressable one is evaluated (for side
// effects) but not reduced — reducing a large array would be a whole-value
// load.
cg_len_of :: proc(c: ^CGCtx, target: ExprId) -> (v: string, is_const: bool) {
    tid := expr_ty(target)
    ty := get_type(tid)
    #partial switch ty.kind {
    case .String, .Slice: {
        inner, returns := reduce_expr_to_single_value(c, cg_expr(c, target))
        assert(returns)
        t := new_tmp(c)
        cwritefln(c, "\t%s = extractvalue %s %s, 1", t, ty_to_llvm_str(c, tid), inner)
        return t, false
    }
    case .FixedSizeArray: {
        if !is_addressable(c, target) {
            _ = cg_expr(c, target)
        }
        return aprintf(c, "%d", ty.fixed_size_array.size), true
    }
    case:
        highlight_lines(get_span(target))
        gala_panic("can't take len of expression", ty.kind)
    }
    return "", false
}

// Evaluates a slice bound and converts it to the language's integer type
// (a no-op when it already is that type).
cg_slice_bound :: proc(c: ^CGCtx, id: ExprId) -> string {
    v, returns := reduce_expr_to_single_value(c, cg_expr(c, id))
    assert(returns)

    int_kind := get(ty_from_name("u64")).kind
    if get(expr_ty(id)).kind == int_kind {
        return v
    }
    op, ok := ty_to_llvm_cast_op(expr_ty(id), integer_type())
    if !ok {
        return v
    }
    t := new_tmp(c)
    cwritefln(c, "\t%s = %s %s %s to %s", t, op,
        ty_to_llvm_str(c, expr_ty(id)), v, ty_to_llvm_str(c, integer_type()))
    return t
}

// ============================================================================
// Binary operators
// ============================================================================

OperandClass :: enum {
    SignedInt,
    UnsignedInt,
    Float,
    Bool,
}

operand_class :: proc(t: TypeId) -> OperandClass {
    kind := get_type(t).kind
    switch {
    case is_integer_signed(t):
        return .SignedInt
    case is_integer_unsigned(t) || kind == .Byte || kind == .Rune:
        return .UnsignedInt
    case is_float(t):
        return .Float
    case kind == .Bool:
        return .Bool
    case kind == .Void:
        gala_panic("can't binop voids")
    }
    debugln(kind)
    gala_panic("operand_class: unsupported operand type")
    // return .SignedInt
}

// LLVM opcode (incl. the icmp/fcmp predicate) for a binop on `cls`.
binop_llvm_op :: proc(e: Binop, cls: OperandClass) -> string {
    switch cls {
    case .SignedInt, .UnsignedInt: {
        signed := cls == .SignedInt
        #partial switch e.kind {
        case .Addition:     return "add"
        case .Subtraction:  return "sub"
        case .Multiply:     return "mul"
        case .Divide:       return "sdiv" if signed else "udiv"
        case .Modulo:       return "srem" if signed else "urem"
        case .Equal:        return "icmp eq"
        case .NotEqual:     return "icmp ne"
        case .LessEqual:    return "icmp sle" if signed else "icmp ule"
        case .GreaterEqual: return "icmp sge" if signed else "icmp uge"
        case .Less:         return "icmp slt" if signed else "icmp ult"
        case .Greater:      return "icmp sgt" if signed else "icmp ugt"
        case .BitAnd:       return "and"
        case .BitXor:       return "xor"
        case .BitOr:        return "or"
        case .ShiftLeft:    return "shl"
        case .ShiftRight:   return "ashr" if signed else "lshr"
        }
    }
    case .Float: {
        #partial switch e.kind {
        case .Addition:     return "fadd"
        case .Subtraction:  return "fsub"
        case .Multiply:     return "fmul"
        case .Divide:       return "fdiv"
        case .Modulo:       return "frem"
        case .Equal:        return "fcmp oeq"
        case .NotEqual:     return "fcmp une"
        case .LessEqual:    return "fcmp ole"
        case .GreaterEqual: return "fcmp oge"
        case .Less:         return "fcmp olt"
        case .Greater:      return "fcmp ogt"
        }
    }
    case .Bool: {
        #partial switch e.kind {
        case .Equal:      return "icmp eq"
        case .NotEqual:   return "icmp ne"
        case .LogicalAnd: return "and"
        case .LogicalOr:  return "or"
        case:
            gala_panic("can't order booleans")
        }
    }
    }
    gala_panic("unsupported binary operation", e.kind, cls)
    // return ""
}

// Pointers are always compared / offset as plain integers.
ptr_cmp_op :: proc(e: Binop) -> (string, bool) {
    #partial switch e.kind {
    case .Equal:        return "icmp eq", true
    case .NotEqual:     return "icmp ne", true
    case .LessEqual:    return "icmp ule", true
    case .GreaterEqual: return "icmp uge", true
    case .Less:         return "icmp ult", true
    case .Greater:      return "icmp ugt", true
    }
    return "", false
}

cg_ptr_to_i64 :: proc(c: ^CGCtx, res: CGExprRes) -> string {
    v, returns := reduce_expr_to_single_value(c, res)
    assert(returns)
    t := new_tmp(c)
    cwritefln(c, "\t%s = ptrtoint ptr %s to i64", t, v)
    return t
}

// ---- pointer arithmetic / pointer comparison ----
// Operands of a Binop always share a type, so if either side is a pointer,
// both are. Number literals that were coerced onto a pointer type (`ptr - 1`,
// null checks) come out of cg_expr as a real `inttoptr` value, so
// ptrtoint on them is always valid.
cg_pointer_binop :: proc(c: ^CGCtx, e: Binop) -> CGExprRes {
    li := cg_ptr_to_i64(c, cg_expr(c, e.left))
    ri := cg_ptr_to_i64(c, cg_expr(c, e.right))

    if op, is_cmp := ptr_cmp_op(e); is_cmp {
        return {kind=.Binop, v=aprintf(c, "%s i64 %s, %s", op, li, ri)}
    }

    #partial switch e.kind {
    case .Addition, .Subtraction: {
        arith := "add" if e.kind == .Addition else "sub"
        res := new_tmp(c)
        cwritefln(c, "\t%s = %s i64 %s, %s", res, arith, li, ri)
        t := new_tmp(c)
        cwritefln(c, "\t%s = inttoptr i64 %s to ptr", t, res)
        return {kind=.Value, v=t}
    }
    }
    gala_panic("unsupported pointer binary operation")
    // return {}
}

// ============================================================================
// Expressions
// ============================================================================

cg_expr :: proc(c: ^CGCtx, id: ExprId) -> CGExprRes {
    switch e in get_expr(id) {
    case TypeInfoOf: {
        // the id is a runtime i64 (e.g. `v.id`); index the global table with it
        id_v, returns := reduce_expr_to_single_value(c, cg_expr(c, e.expr))
        assert(returns)

        info_ty := get_type(expr_ty(id)).ptr   // expr type is ^TypeInfo

        p := new_tmp(c)
        cwritefln(c, "\t%s = getelementptr inbounds %s, ptr @__gala_types, i64 %s",
            p, ty_to_llvm_str(c, info_ty), id_v)
        return {kind=.Value, v=p}
    }
    case UnNegative: {
        v, returns := reduce_expr_to_single_value(c, cg_expr(c, e.expr))
        assert(returns)

        t := expr_ty(e.expr)
        llvm_ty := ty_to_llvm_str(c, t)

        result := new_tmp(c)
        if is_float(t) {
            cwritefln(c, "\t%s = fneg %s %s", result, llvm_ty, v)
        } else {
            cwritefln(c, "\t%s = sub %s 0, %s", result, llvm_ty, v)
        }
        return {kind=.Value, v=result}
    }
    case UnNot: {
        v, returns := reduce_expr_to_single_value(c, cg_expr(c, e.expr))
        assert(returns)

        t := new_tmp(c)
        cwritefln(c, "\t%s = xor i1 %s, true", t, v)
        return {kind=.Value, v=t}
    }
    case BoolLitFalse:
        return {kind=.Value, v="0"}
    case BoolLitTrue:
        return {kind=.Value, v="1"}
    case Len: {
        v, is_const := cg_len_of(c, e.target)
        if is_const {
            return {kind=.Number, v=v}
        }
        // slice/string length is a runtime i64; convert to whatever type the
        // checker settled on (i32, u8, f32, ...)
        to_ty := expr_ty(id)
        op, ok := ty_to_llvm_cast_op(integer_type(), to_ty)
        if ok {
            t := new_tmp(c)
            cwritefln(c, "\t%s = %s %s %s to %s", t, op,
                ty_to_llvm_str(c, integer_type()), v, ty_to_llvm_str(c, to_ty))
            v = t
        }
        return {kind=.Value, v=v}
    }
    case SizeOf: {
        s := type_size(get_ctx().expr_resolution_types[id])
        return {kind=.Number, v=aprintf(c, "%d", s)}
    }
    case TypeIdOf: {
        // same resolution slot Sizeof uses for its type specifier
        tid := get_ctx().expr_resolution_types[id]
        return {kind=.Number, v=aprintf(c, "%d", tid)}
    }
    case String: {
        r := c.cg_strings[e.s] // global string thingy
        data := new_tmp(c)
        cwritefln(c, "\t%s = getelementptr inbounds %s, ptr %s, i64 0, i64 0",
            data, r.array_type, r.s)
        return {kind=.Value, v=cg_make_pair(c, data, "i64", aprintf(c, "%d", r.len))}
    }
    case Deref: {
        // NOTE: this must mirror cg_addr's Deref case in how it computes
        // the pointer's VALUE (cg_expr + reduce — NOT cg_addr, which would
        // ask for the address of e.expr, and e.expr is very often not an
        // addressable lvalue at all — e.g. `(ints + 0 * sizeof(i32))^`
        // has a Binop as e.expr, which has no address to take). Once we
        // have that pointer value, cg_expr additionally loads through it
        // (unlike cg_addr's Deref, which just returns the pointer value
        // itself as "the address").
        ptr_val, returns := reduce_expr_to_single_value(c, cg_expr(c, e.expr))
        assert(returns)
        ptr_ty := get_type(expr_ty(e.expr))

        if ptr_ty.kind != .Pointer {
            gala_panic("cannot dereference non-pointer type")
        }

        pointee_ty := ptr_ty.ptr

        // large pointee: hand back the address, don't load the whole thing
        if is_memory_type(pointee_ty) {
            return {kind=.Place, v=ptr_val, id=id}
        }

        loaded := new_tmp(c)
        cwritefln(c, "\t%s = load %s, ptr %s", loaded, ty_to_llvm_str(c, pointee_ty), ptr_val)
        return {kind=.Value, v=loaded}
    }
    case Reference:
        return {kind=.Value, v=cg_addr(c, e.expr)}
    case Transmute: {
        from_ty := expr_ty(e.target)
        to_ty := expr_ty(id)
        reduced, returns := reduce_expr_to_single_value(c, cg_expr(c, e.target))
        assert(returns)

        op, result := ty_to_llvm_transmute_op(from_ty, to_ty)
        switch result {
        case .NoOp:
            return {kind=.Value, v=reduced}

        case .Instr:
            t := new_tmp(c)
            cwritefln(c, "\t%s = %s %s %s to %s", t, op,
                ty_to_llvm_str(c, from_ty), reduced, ty_to_llvm_str(c, to_ty))
            return {kind=.Value, v=t}

        case .Memory:
            // the slot is sized for `from_ty`; reading a bigger `to_ty`
            // back out of it would run off the end of the alloca
            assert(type_size(from_ty) == type_size(to_ty), "transmute between types of different sizes")
            slot := new_entry_alloca(c, ty_to_llvm_str(c, from_ty))
            cwritefln(c, "\tstore %s %s, ptr %s", ty_to_llvm_str(c, from_ty), reduced, slot)
            t := new_tmp(c)
            cwritefln(c, "\t%s = load %s, ptr %s", t, ty_to_llvm_str(c, to_ty), slot)
            return {kind=.Value, v=t}
        }
        unreachable()
    }
    case TakeSlice: {
        start := "0"
        if !e.empty_start {
            start = cg_slice_bound(c, e.start)
        }
        end: string
        if !e.empty_end {
            end = cg_slice_bound(c, e.end)
        } else {
            end, _ = cg_len_of(c, e.target)
        }

        llvm_int := ty_to_llvm_str(c, integer_type())
        len_s := new_tmp(c)
        cwritefln(c, "\t%s = sub nsw nuw %s %s, %s", len_s, llvm_int, end, start)

        base_ptr, elem_ty := cg_data_ptr(c, e.target)
        elem_ptr := new_tmp(c)
        cwritefln(c, "\t%s = getelementptr inbounds %s, ptr %s, %s %s",
            elem_ptr, elem_ty, base_ptr, llvm_int, start)

        return {kind=.Value, v=cg_make_pair(c, elem_ptr, llvm_int, len_s)}
    }
    case Index: {
        ptr := cg_elem_ptr(c, e.target, e.index)
        // large element: return its address, load only if someone needs the value
        if is_memory_type(expr_ty(id)) {
            return {kind=.Place, v=ptr, id=id}
        }
        v := new_tmp(c)
        cwritefln(c, "\t%s = load %s, ptr %s", v, ty_to_llvm_str(c, expr_ty(id)), ptr)
        return {kind=.Value, v=v}
    }
    case FixedSizeArray: {
        // `{}` is all zeroes; otherwise start from zeroinitializer
        // and insertvalue each given element, so any elements past the end
        // of the initialiser stay zero.
        //
        // NOTE: this builds an SSA aggregate, so it's only for small arrays.
        // Large ones are written in place by cg_expr_into (memset + stores).
        if len(e.initialiser) == 0 {
            return {kind=.Value, v="zeroinitializer"}
        }

        arr_ty := ty_to_llvm_str(c, expr_ty(id))
        elem_ty := ty_to_llvm_str(c, get_type(expr_ty(id)).fixed_size_array.type)

        acc := "zeroinitializer"
        for el, i in e.initialiser {
            v, returns := reduce_expr_to_single_value(c, cg_expr(c, el))
            assert(returns)
            t := new_tmp(c)
            cwritefln(c, "\t%s = insertvalue %s %s, %s %s, %d",
                t, arr_ty, acc, elem_ty, v, i)
            acc = t
        }
        return {kind=.Value, v=acc}
    }
    case FieldAccess: {
        target_ty := expr_ty(e.target)
        idx, field_ty := field_of(target_ty, e.field, id)

        // unions
        if is_addressable(c, e.target) || is_memory_type(target_ty) ||
           get_type(target_ty).kind == .Union {
            base_ptr := cg_value_addr(c, e.target)
            field_ptr := cg_field_ptr(c, base_ptr, target_ty, idx)

            // large field (e.g. `t.pixels`): address only
            if is_memory_type(field_ty) {
                return {kind=.Place, v=field_ptr, id=id}
            }

            loaded := new_tmp(c)
            cwritefln(c, "\t%s = load %s, ptr %s",
                loaded, ty_to_llvm_str(c, field_ty), field_ptr)
            return {kind=.Value, v=loaded}
        }

        // Memory-backed target (a variable / deref / index, or any large
        // aggregate rvalue): GEP to the field and load just that field.
        // The whole struct is never loaded. An rvalue large aggregate
        // (e.g. `make_terrain().w`) is first written to a temporary.
        if is_addressable(c, e.target) || is_memory_type(target_ty) {
            base_ptr := cg_value_addr(c, e.target)

            field_ptr := new_tmp(c)
            cwritefln(c, "\t%s = getelementptr inbounds %s, ptr %s, i32 0, i32 %d",
                field_ptr, ty_to_llvm_str(c, target_ty), base_ptr, idx)

            // large field (e.g. `t.pixels`): address only
            if is_memory_type(field_ty) {
                return {kind=.Place, v=field_ptr, id=id}
            }

            loaded := new_tmp(c)
            cwritefln(c, "\t%s = load %s, ptr %s",
                loaded, ty_to_llvm_str(c, field_ty), field_ptr)
            return {kind=.Value, v=loaded}
        }

        // Small rvalue target (fn call result, small struct literal, an
        // argument passed in registers, ...): it's already an SSA value.
        value, returns := reduce_expr_to_single_value(c, cg_expr(c, e.target))
        assert(returns)

        t := new_tmp(c)
        cwritefln(c, "\t%s = extractvalue %s %s, %d",
            t, ty_to_llvm_str(c, target_ty), value, idx)
        return {kind=.Value, v=t}
    }
    case StructLit: {
        ty := get_ctx().expr_resolution_types[id]
        if get_type(ty).kind == .Union {
            // lives in memory; reduce_expr_to_single_value loads the whole
            // thing if someone needs it as a value
            slot := new_entry_alloca(c, ty_to_llvm_str(c, ty))
            cg_union_lit_into(c, e, ty, slot)
            return {kind=.Place, v=slot, id=id}
        }
        sfields := get_type(ty).structure.fields
        fields := make([]string, len(sfields), allocator=get_ctx().allocator)
        for f, k in sfields {
            r, ok := reduce_expr_to_single_value(c, cg_expr(c, e.fields[f.name].expr))
            assert(ok)
            fields[k] = r
        }
        r: CGExprRes
        r.struct_lit.fields = fields
        r.id = id
        r.kind = .Struct
        return r
    }
    case Cast: {
        target := cg_expr(c, e.target)
        target_ty := expr_ty(e.target)
        to_ty := expr_ty(id)
        op, ok := ty_to_llvm_cast_op(target_ty, to_ty)
        if !ok {
            return target
        }
        reduced_target, returns := reduce_expr_to_single_value(c, target)
        assert(returns)
        t := new_tmp(c)
        cwritefln(c, "\t%s = %s %s %s to %s", t, op,
            ty_to_llvm_str(c, target_ty), reduced_target, ty_to_llvm_str(c, to_ty))
        return {kind=.Value, v=t}
    }
    case Number: {
        ty_id := expr_ty(id)
        if is_float(ty_id) {
            // `1.5` is a real float literal; `5`, `0xff`, `0b101` are integer
            // literals that were converted to a float type.
            is_real := is_float_literal(e.text)
            int_val: i64
            if !is_real {
                ok: bool
                int_val, ok = parse_integer_literal(e.text)
                assert(ok)
            }

            #partial switch get_type(ty_id).kind {
            case .Flt64: {
                f := f64(int_val)
                if is_real {
                    ok: bool
                    f, ok = strconv.parse_f64(e.text)
                    assert(ok)
                }
                return {kind=.Number, v=llvm_double_const(f)}
            }
            case .Flt32: {
                f := f32(int_val)
                if is_real {
                    ok: bool
                    f, ok = strconv.parse_f32(e.text)
                    assert(ok)
                }
                return {kind=.Number, v=llvm_float_const(f)}
            }
            case:
                gala_panic("float literal of unsupported width", get_type(ty_id).kind)
            }
        }

        n, ok := parse_integer_literal(e.text)
        assert(ok)
        if get_type(ty_id).kind == .Pointer {
            t := new_tmp(c)
            cwritefln(c, "\t%s = inttoptr i64 %d to ptr", t, n)
            return {kind=.Number, v=t}
        }
        return {kind=.Number, v=aprintf(c, "%d", n)}
    }
    case Binop: {
        if e.kind == .LogicalAnd || e.kind == .LogicalOr {
            return cg_logical(c, e)
        }
        left_ty := expr_ty(e.left)
        assert(left_ty == expr_ty(e.right), "expressions in binop must have the same type")

        if is_pointer(left_ty) {
            return cg_pointer_binop(c, e)
        }

        l_v, returns_l := reduce_expr_to_single_value(c, cg_expr(c, e.left))
        assert(returns_l)
        r_v, returns_r := reduce_expr_to_single_value(c, cg_expr(c, e.right))
        assert(returns_r)

        op := binop_llvm_op(e, operand_class(left_ty))
        return {kind=.Binop,
            v=aprintf(c, "%s %s %s, %s", op, ty_to_llvm_str(c, left_ty), l_v, r_v)}
    }
    case Symbol: {
        v := cgscope_get(&c.scope, e.name)

        switch v.kind {
        case .Symbol:
            return {kind=.Address, v=v.name}
        case .Variable: {
            // large aggregate variable: give back its address, don't load it
            if is_memory_type(expr_ty(id)) {
                return {kind=.Place, v=v.name, id=id}
            }
            t := new_tmp(c)
            cwritefln(c, "\t%s = load %s, ptr %s", t, ty_to_llvm_str(c, expr_ty(id)), v.name)
            return {kind=.Value, v=t}
        }
        case .Argument:
            return {kind=.Value, v=v.name}
        case .Invalid: {
            debugln("Invalid object for:", e, id)
            gala_panic("invalid object")
        }
        }
        unreachable()
    }
    case FnCall:
        return cg_fn_call(c, id, e)
    case FnLit:
        return cg_fn_lit(c, id, e)
    }
    unreachable()
}

// ============================================================================
// Calls
// ============================================================================

// Internal (Gala) variadics are lowered to an ordinary function whose last
// parameter is the `[]T` slice (fn.gala_abi_ty, named fn.variadic_name).
// Both the definition and every call site lower against this type, so the
// ABI code never needs to know about `..T`.
// Anything that isn't an internal variadic is returned unchanged.
lower_internal_variadic :: proc(fn_type_id: TypeId) -> TypeId {
    fn_ty := get_type(fn_type_id)
    if fn_ty.kind != .Function || !fn_ty.fn.is_variadic || fn_ty.fn.is_external {
        return fn_type_id
    }

    n_fixed := len(fn_ty.fn.args)
    args := make([]Arg, n_fixed + 1, allocator=get_ctx().allocator)
    copy(args, fn_ty.fn.args)
    args[n_fixed] = Arg{name=fn_ty.fn.variadic_name, type=fn_ty.fn.gala_abi_ty}

    lowered := Type{}
    lowered.kind = .Function
    lowered.fn = fn_ty.fn
    lowered.fn.args = args
    lowered.fn.is_variadic = false
    return new_fn_type(lowered)
}

// `any` is passed around as { ptr, i64 }: a pointer to a spilled copy of the
// value plus the TypeId of its concrete type (see cg_box_any).
is_any_type :: proc(t: TypeId) -> bool {
    return get_type(t).kind == .Any
}

// Evaluates one call argument for a parameter of type `param_ty`.
// If the parameter is `any` and the argument isn't already an `any`, the
// value is boxed. If the typechecker marked the arg (needs_boxing), its
// box_type is taken as the concrete type; otherwise the concrete type is
// the argument expression's own type.
//
// Returns an SSA value, so it's for small values only — large aggregates
// passed `byval` go through cg_emit_call_arg, which never loads them.
cg_call_arg_value :: proc(c: ^CGCtx, a: FnCallArg, param_ty: TypeId) -> string {
    r, returns := reduce_expr_to_single_value(c, cg_expr(c, a.expr))
    assert(returns)

    concrete_ty := expr_ty(a.expr)
    should_box := is_any_type(param_ty) && !is_any_type(concrete_ty)
    if a.needs_boxing {
        concrete_ty = a.box_type
        should_box = true
    }

    if should_box {
        boxed, _ := cg_box_any(c, r, concrete_ty)
        return boxed
    }
    return r
}

// Turns an already-evaluated SSA value `r` for lowered parameter `k` of
// `sig` into the text of the matching call operand and appends it to
// `call_args` (byval slot / coerced direct value / plain direct value).
// Memory slot for round-tripping a value of `orig_type` through its coerced
// ABI type. The coerced type can be LARGER than the value (`{i64,i32}` is 12
// bytes of data but `{ i64, i32 }` allocates 16), so the slot is sized for
// whichever is bigger; otherwise the coerced load/store would run past it.
//
// Returns the slot and the `, align N` suffix to put on the coerced-type
// load/store ("" when the slot is already the coerced type). Use this in the
// callee prologue and for coerced return values too.
cg_abi_coerce_slot :: proc(c: ^CGCtx, orig_type: TypeId, coerced_type: string, coerced_size: int) -> (slot: string, coerced_align: string) {
    if coerced_size >= type_size(orig_type) {
        // Coerced types (iN, float/double/half vectors, ptr, pairs thereof)
        // are never less aligned than the aggregate they cover, so storing
        // the original type into this slot is fine.
        return new_entry_alloca(c, coerced_type), ""
    }
    // Coerced type smaller than the value (tail padding): slot is the
    // original type; access it with the original's alignment, since e.g.
    // an i64 load from a 4-aligned { i32, i32 } slot would claim align 8.
    return new_entry_alloca(c, ty_to_llvm_str(c, orig_type)), aprintf(c, ", align %d", type_align_of(orig_type))
}

cg_emit_abi_arg :: proc(c: ^CGCtx, sig: AbiSignature, k: int, r: string, call_args: ^[dynamic]string) {
    cg_emit_lowered_arg(c, sig.args[k], r, call_args)
}

// Same as cg_emit_abi_arg but takes the lowering directly, so C-variadic
// extras (from cg_abi_lower_c_variadic_arg, not in sig.args) can use it:
//
//     regs := sig.regs_after_fixed
//     for v, i in extra_vals {
//         al := cg_abi_lower_c_variadic_arg(sig.kind, &regs, extra_tys[i])
//         cg_emit_lowered_arg(c, al, v, &call_args)
//     }
cg_emit_lowered_arg :: proc(c: ^CGCtx, al: AbiArgLowering, r: string, call_args: ^[dynamic]string) {
    arg_ty_str := ty_to_llvm_str(c, al.orig_type)

    switch al.mode {
    case .Ignore:
        // Nothing is passed. `r` was already evaluated by the caller, so any
        // side effects of the argument expression still happen.

    case .ByVal:
        slot := new_entry_alloca(c, arg_ty_str)
        cwritefln(c, "\tstore %s %s, ptr %s", arg_ty_str, r, slot)
        append(call_args, aprintf(c, "ptr byval(%s) align %d %s", arg_ty_str, al.byval_align, slot))

    case .Indirect:
        // Win64 by-reference: the callee may modify the pointee, so it must
        // be a private copy, never the address of the source variable. A new
        // alloca per argument + a store right before the call is enough —
        // reusing the entry-block slot across loop iterations is fine since
        // it's refilled each time and the callee can't keep it past return.
        slot := new_entry_alloca(c, arg_ty_str)
        cwritefln(c, "\tstore %s %s, ptr %s, align %d", arg_ty_str, r, slot, type_align_of(al.orig_type))
        append(call_args, aprintf(c, "ptr noundef %s", slot))

    case .Direct:
        if al.needs_coercion {
            slot, coerced_align := cg_abi_coerce_slot(c, al.orig_type, al.coerced_type, al.coerced_size)
            cwritefln(c, "\tstore %s %s, ptr %s", arg_ty_str, r, slot)
            coerced := new_tmp(c)
            cwritefln(c, "\t%s = load %s, ptr %s%s", coerced, al.coerced_type, slot, coerced_align)
            append(call_args, aprintf(c, "%s %s", al.coerced_type, coerced))
        } else if al.promotion != .None {
            // C default argument promotion in a `...` slot; coerced_type is
            // already the promoted type (double / i32).
            op: string
            switch al.promotion {
            case .FloatToDouble:   op = "fpext"
            case .ZeroExtendToInt: op = "zext"
            case .SignExtendToInt: op = "sext"
            case .None:
            }
            promoted := new_tmp(c)
            cwritefln(c, "\t%s = %s %s %s to %s", promoted, op, scalar_llvm_str(al.orig_type), r, al.coerced_type)
            append(call_args, aprintf(c, "%s %s", al.coerced_type, promoted))
        } else if al.ext != "" {
            // sub-32-bit scalar: the caller must extend it (`i1 zeroext %v`)
            append(call_args, aprintf(c, "%s %s %s", al.coerced_type, al.ext, r))
        } else {
            append(call_args, aprintf(c, "%s %s", al.coerced_type, r))
        }
    }
}




// Evaluates call argument `a` for lowered parameter `k` and appends the call
// operand to `call_args`.
//
// A large aggregate passed `byval` is never turned into an SSA value:
// `byval(T)` means the callee gets its own copy made from the pointer at
// the call, so we just pass the address of the source (making a temporary
// first only if the argument isn't already in memory). LLVM does the one
// copy into the argument area.
cg_emit_call_arg :: proc(c: ^CGCtx, a: FnCallArg, sig: AbiSignature, k: int, call_args: ^[dynamic]string) {
    al := sig.args[k]
    if al.mode == .ByVal && is_memory_type(al.orig_type) && !a.needs_boxing {
        src := cg_value_addr(c, a.expr)
        append(call_args, aprintf(c, "ptr byval(%s) align %d %s",
            ty_to_llvm_str(c, al.orig_type), al.byval_align, src))
        return
    }
    r := cg_call_arg_value(c, a, al.orig_type)
    cg_emit_abi_arg(c, sig, k, r, call_args)
}

// Packs the variadic tail of a call into a `{ ptr, i64 }` slice value:
// the elements are stored into a stack array and the slice points at it.
// With no extra arguments the slice is { null, 0 }.
// For `..any` every element is boxed first, so the array is [n x { ptr, i64 }].
cg_pack_variadic_slice :: proc(c: ^CGCtx, tail: []FnCallArg, elem_ty: TypeId) -> string {
    llvm_int := ty_to_llvm_str(c, integer_type())
    elem_str := "{ ptr, i64 }" if is_any_type(elem_ty) else ty_to_llvm_str(c, elem_ty)
    n := len(tail)

    base := "null"
    if n > 0 {
        arr_ty := aprintf(c, "[%d x %s]", n, elem_str)
        base = new_entry_alloca(c, arr_ty)
        for a, i in tail {
            v := cg_call_arg_value(c, a, elem_ty)
            p := new_tmp(c)
            cwritefln(c, "\t%s = getelementptr inbounds %s, ptr %s, i64 0, i64 %d",
                p, arr_ty, base, i)
            cwritefln(c, "\tstore %s %s, ptr %s", elem_str, v, p)
        }
    }

    return cg_make_pair(c, base, llvm_int, aprintf(c, "%d", n))
}

// Writes "(a, b, c)\n" for an already-built call operand list.
cg_write_call_args :: proc(c: ^CGCtx, call_args: [dynamic]string) {
    cwrite(c, "(")
    for a, i in call_args {
        if i > 0 do cwrite(c, ", ")
        cwritef(c, "%s", a)
    }
    cwriteln(c, ")")
}

// Writes `<head><variadic_prefix><target>(args)\n`.
cg_write_call :: proc(c: ^CGCtx, head, variadic_prefix, target: string, call_args: [dynamic]string) {
    cwritef(c, "%s%s%s", head, variadic_prefix, target)
    cg_write_call_args(c, call_args)
}

// Emits the `call` instruction itself and produces the call's result.
// Shared by normal calls and internal-variadic calls.
//
// `sret_slot` is the memory the callee writes an indirect (sret) result
// into. For a large aggregate result the answer is handed back as a
// `.Place` pointing at that slot — it is NOT loaded.
// `variadic_prefix` is the "(T, U, ...)" fixed-parameter list LLVM wants
// before the callee of a C-variadic call ("" otherwise).
cg_emit_call :: proc(c: ^CGCtx, id: ExprId, sig: AbiSignature, target: string,
        call_args: [dynamic]string, variadic_prefix: string, sret_slot: string) -> CGExprRes {
    if sig.ret.mode == .Indirect {
        cg_write_call(c, "\tcall void ", variadic_prefix, target, call_args)

        if is_memory_type(sig.ret.orig_type) {
            return {kind=.Place, v=sret_slot, id=id}
        }
        loaded := new_tmp(c)
        cwritefln(c, "\t%s = load %s, ptr %s", loaded, ty_to_llvm_str(c, sig.ret.orig_type), sret_slot)
        return {kind=.Value, v=loaded, id=id}
    }

    if sig.ret.coerced_type == "void" {
        cg_write_call(c, "\tcall void ", variadic_prefix, target, call_args)
        return {kind=.None, id=id}
    }

    result := new_tmp(c)
    // `call zeroext i1 @f(...)` — the return attribute goes BEFORE the type
    ret_ext := ""
    if sig.ret.ext != "" {
        ret_ext = aprintf(c, "%s ", sig.ret.ext)
    }
    cg_write_call(c, aprintf(c, "\t%s = call %s%s ", result, ret_ext, sig.ret.coerced_type),
        variadic_prefix, target, call_args)

    if sig.ret.needs_coercion {
        real_ty_str := ty_to_llvm_str(c, sig.ret.orig_type)
        slot := new_entry_alloca(c, real_ty_str)
        cwritefln(c, "\tstore %s %s, ptr %s", sig.ret.coerced_type, result, slot)
        loaded := new_tmp(c)
        cwritefln(c, "\t%s = load %s, ptr %s", loaded, real_ty_str, slot)
        return {kind=.Value, v=loaded, id=id}
    }
    return {kind=.Value, v=result, id=id}
}

// The sret operand shared by both call paths. Uses `dest` directly when the
// caller supplied one (no temporary, no copy), else a fresh entry-block slot.
// Returns "" when the callee doesn't return through sret.
cg_prepare_sret :: proc(c: ^CGCtx, sig: AbiSignature, dest: string, call_args: ^[dynamic]string) -> string {
    if sig.ret.mode != .Indirect do return ""

    slot := dest
    if slot == "" {
        slot = new_entry_alloca(c, ty_to_llvm_str(c, sig.ret.orig_type))
    }
    append(call_args, aprintf(c, "ptr sret(%s) align %d %s",
        ty_to_llvm_str(c, sig.ret.orig_type), sig.ret.sret_align, slot))
    return slot
}

// Call to an internal variadic: fixed args are evaluated as usual, the tail
// is packed into a slice, and the result is a plain non-variadic call
// against the lowered type.
cg_fn_call_internal_variadic :: proc(c: ^CGCtx, id: ExprId, e: FnCall, target: string, dest := "") -> CGExprRes {
    fn_type_id := expr_ty(e.target)
    fn_ty := get_type(fn_type_id)
    sig := cg_abi_lower_signature(c, fn_type_id, get_ctx().abi)   // or however cg_fn_definition gets it

    n_fixed := len(fn_ty.fn.args)
    assert(len(e.args) >= n_fixed, "not enough arguments for variadic call")
    assert(len(sig.args) == n_fixed + 1)

    call_args := make([dynamic]string, allocator=get_ctx().allocator)
    sret_slot := cg_prepare_sret(c, sig, dest, &call_args)

    // fixed args, then the packed slice as the last lowered parameter
    for k in 0 ..< n_fixed {
        cg_emit_call_arg(c, e.args[k], sig, k, &call_args)
    }
    slice_v := cg_pack_variadic_slice(c, e.args[n_fixed:], fn_ty.fn.variadic_ty)
    cg_emit_abi_arg(c, sig, n_fixed, slice_v, &call_args)

    return cg_emit_call(c, id, sig, target, call_args, "", sret_slot)
}

// LLVM requires the full fixed-parameter TYPE list between the callee's
// return type and "..." at a variadic call site: "(T, U, ...)".
cg_c_variadic_prefix :: proc(c: ^CGCtx, sig: AbiSignature, fixed_arg_count: int) -> string {
    vb: strings.Builder
    strings.builder_init(&vb, get_ctx().allocator)
    strings.write_string(&vb, "(")
    if sig.ret.mode == .Indirect {
        strings.write_string(&vb, aprintf(c, "ptr sret(%s), ", ty_to_llvm_str(c, sig.ret.orig_type)))
    }
    for al in sig.args[:fixed_arg_count] {
        if al.mode == .ByVal {
            strings.write_string(&vb, aprintf(c, "ptr byval(%s) align %d",
                ty_to_llvm_str(c, al.orig_type), al.byval_align))
        } else {
            strings.write_string(&vb, al.coerced_type)
        }
        strings.write_string(&vb, ", ")
    }
    strings.write_string(&vb, "...)")
    return strings.to_string(vb)
}

// `dest`: if non-empty and the call returns through sret, the callee is
// handed this address directly (no temporary, no copy) and the result comes
// back as `.Place` with v == dest. See cg_expr_into.
cg_fn_call :: proc(c: ^CGCtx, id: ExprId, e: FnCall, dest := "") -> CGExprRes {
    t := cg_fn_call_target(c, e.target)
    fn_type_id := expr_ty(e.target)
    fn_ty := get_type(fn_type_id)

    // internal (Gala) variadics: pack the tail into a slice and call the
    // lowered, non-variadic form. Extern C variadics continue below.
    if fn_ty.fn.is_variadic && !fn_ty.fn.is_external {
        return cg_fn_call_internal_variadic(c, id, e, t, dest)
    }

    sig := cg_abi_lower_signature(c, fn_type_id, get_ctx().abi)

    call_args := make([dynamic]string, allocator=get_ctx().allocator)
    sret_slot := cg_prepare_sret(c, sig, dest, &call_args)

    // Only the true fixed prefix goes through sig.args — the trailing
    // C-variadic placeholder slot in fn_ty.fn.args (and its corresponding
    // sig.args entry) is never a real argument to lower here.
    fixed_arg_count := len(e.args)
    if fn_ty.fn.is_variadic { // get fixed args from fn signature
        fixed_arg_count = len(fn_ty.fn.args)
    }

    // ---- fixed params ----
    // (boxes into `any` when the parameter is `any`; large byval
    // aggregates are passed by address, never loaded)
    for k in 0 ..< fixed_arg_count {
        cg_emit_call_arg(c, e.args[k], sig, k, &call_args)
    }

    // ---- trailing C-variadic arguments (extern `...`) ----
    // Each gets C's default argument promotions applied (see
    // c_variadic_promote), never the boxing or slice-packing that a Gala
    // `..T` tail would need.
    variadic_prefix := ""
    if fn_ty.fn.is_variadic { // (always external here, see early return above)
        for k in fixed_arg_count ..< len(e.args) {
            a := e.args[k]
            v, returns := reduce_expr_to_single_value(c, cg_expr(c, a.expr))
            assert(returns)

            promoted_v, promoted_ty_str := cg_variadic_promote(c, v, expr_ty(a.expr))
            append(&call_args, aprintf(c, "%s %s", promoted_ty_str, promoted_v))
        }
        variadic_prefix = cg_c_variadic_prefix(c, sig, fixed_arg_count)
    }

    return cg_emit_call(c, id, sig, t, call_args, variadic_prefix, sret_slot)
}

// Emits the C-variadic default-argument-promotion conversion for an
// already-reduced value `v` of static type `type_id`, if one applies.
// Pure passthrough (same value, same type string) otherwise.
cg_variadic_promote :: proc(c: ^CGCtx, v: string, type_id: TypeId) -> (string, string) {
    promotion, promoted_ty_str := c_variadic_promote(type_id)
    orig_ty_str := ty_to_llvm_str(c, type_id)

    instr := ""
    switch promotion {
    case .None:            return v, orig_ty_str
    case .FloatToDouble:   instr = "fpext"
    case .ZeroExtendToInt: instr = "zext"
    case .SignExtendToInt: instr = "sext"
    }

    t := new_tmp(c)
    cwritefln(c, "\t%s = %s %s %s to %s", t, instr, orig_ty_str, v, promoted_ty_str)
    return t, promoted_ty_str
}
