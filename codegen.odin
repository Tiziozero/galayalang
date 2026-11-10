// codegen.odin
package main

import "core:os"
import "core:io"
import "core:fmt"
import "core:strings"
import "core:mem"
import "core:path/filepath"

// Name of the hidden sret pointer parameter of a function that returns
// through memory (see cg_fn_header / the Return statement).
SRET_PARAM :: "%.sret"

// `5`, `0xff`, `0b101`
is_float_literal :: proc(text: string) -> bool {
    for c in text {
        if c == '.' do return true
    }
    return false
}

// "123", "0xff", "0XFF", "0b101". Hex/binary use the full 64 bits
// (0xFFFFFFFFFFFFFFFF == -1); decimal must fit in i64.
parse_integer_literal :: proc(text: string) -> (n: i64, ok: bool) {
    base: u64 = 10
    digits := text
    if len(text) > 2 && text[0] == '0' {
        switch text[1] {
        case 'x', 'X': base = 16; digits = text[2:]
        case 'b', 'B': base = 2;  digits = text[2:]
        }
    }
    if len(digits) == 0 do return 0, false

    limit := max(u64) if base != 10 else u64(max(i64))
    v: u64
    for ch in digits {
        d := hex_digit_val(byte(ch))
        if d < 0 || u64(d) >= base do return 0, false
        if v > (limit - u64(d)) / base do return 0, false // overflow
        v = v * base + u64(d)
    }
    return i64(v), true
}

CGResKind :: enum {Invalid, Address, Value, Binop, Number, Struct, None, Place}

CGExprRes :: struct {
    id: ExprId,
    // .Place: the value lives in memory at `v` (a pointer operand) and has
    // the type of expression `id`. Nothing has been loaded yet; reducing it
    // loads the whole value, cg_expr_into memcpy's it.
    kind: CGResKind,
    v: string,
    struct_lit: struct {
        fields: []string, // string of results
    },
}

// A global variable whose LLVM definition line hasn't been written yet.
// cg_items_dec records these; cg_globals_dec writes them once every struct
// type has a name (see cg_globals_dec).
PendingGlobal :: struct {
    id:       ItemId,
    external: bool, // true -> `external global` (declared by an importer)
}

CGCtx :: struct {
    arena: ^mem.Dynamic_Arena,
    b: ^strings.Builder,
    tmp_id: int,
    scope: CGScope,
    cg_strings: map[string]StringGlobalResult,
    cur_fn_ret: AbiRetLowering, // ABI lowering of the return value of the function currently being emitted
    break_labels: map[StmtId]string,    // loop (or if/else passthrough) StmtId -> label to jump to on `break`
    continue_labels: map[StmtId]string, // loop (or if/else passthrough) StmtId -> label to jump to on `continue`
    // "currently active" loop targets, saved/restored around each loop body
    // (see cg_enter_loop / cg_leave_loop) so that any IfElse nested inside
    // (however deeply) can register itself as pointing at the same targets —
    // see cg_stmt's IfElse case.
    cur_break_label: string,
    cur_continue_label: string,
    // Finished `define` texts for function literals. A lambda is generated
    // into its own builder (you can't nest a `define` inside another
    // function's body) and parked here; cg_module writes them out after the
    // regular items. See cg_fn_lit.
    lambdas: [dynamic]string,

    // Entry-block allocas of the function currently being emitted. Local
    // variables and large temporaries are allocated here rather than where
    // they're declared, so an alloca inside a loop body doesn't grow the
    // stack every iteration (and mem2reg can promote it, since it only
    // looks at the entry block). cg_fn_definition splices this in right
    // after `entry:`. nil outside a function body (e.g. while emitting a
    // module's __init_globals), in which case allocas are written inline.
    allocas: ^strings.Builder,

    // ---- declaration dedupe (see cg_items_dec) ----
    // A module can be reached through several import paths (main -> rl ->
    // core and main -> core), and LLVM rejects redefining a type or
    // redeclaring a function. These record what this output file has
    // already seen so every declaration is written exactly once.
    declared_items: map[ItemId]bool, // items whose declaration was handled
    declared_mods: map[ModId]bool,   // modules whose items were already walked
    emitted_externs: map[string]bool, // C symbol names already `declare`d

    // ---- globals ----
    // Collected by cg_items_dec, written by cg_globals_dec once all types exist.
    pending_globals: [dynamic]PendingGlobal,
    // Imported modules that have globals, in post-order (a module's own
    // imports come before it). The entry `main` wrapper calls their
    // __init_globals functions in this order.
    init_mods: [dynamic]ModId,

    // for generics - types form importing modules to imported module
    declared_types: map[TypeId]bool,
    extra_types:    strings.Builder,
}

CGObjectKind :: enum {
    Invalid,
    Argument,
    Variable,
    Symbol, // like functions
}
CGObj :: struct {
    kind: CGObjectKind,
    name: string,
}
CGScope :: struct {
    vars : map[string]CGObj,
    parent: ^CGScope,
}
new_gcscope :: proc(parent: ^CGScope) -> CGScope {
    s := CGScope{}
    s.vars = make(map[string]CGObj, allocator=get_ctx().allocator)
    s.parent = parent
    return s
}
free_cgscope :: proc(s: ^CGScope) {
    // delete(s.vars)
}

cwritef :: proc(c: ^CGCtx, format: string, data: ..any) {
    fmt.sbprintf(c.b, format, ..data)
}
cwrite :: proc(c: ^CGCtx, format: string) {
    fmt.sbprint(c.b, format)
}
cwriteln :: proc(c: ^CGCtx, format: string) {
    fmt.sbprintln(c.b, format)
}
cwritefln :: proc(c: ^CGCtx, format: string, data: ..any) {
    fmt.sbprintfln(c.b, format, ..data)
}

// ---- large aggregates ----
//
// Big structs/arrays must never be first-class SSA values: LLVM's passes
// split an aggregate value into one scalar per element, so a
// `[16384 x Pixel]` becomes tens of thousands of values and llc crawls
// (or looks hung). Like Clang and rustc, such types live in memory and are
// only touched through addresses: GEP + scalar load/store for access,
// `llvm.memcpy` for whole-value copies, `llvm.memset` for zeroing, and
// destination-passing (cg_expr_into) for construction and calls.
//
// Structs/arrays up to this many bytes still travel as SSA values (cheap
// for LLVM, and the ABI coercion code relies on loading small structs).
LARGE_AGGREGATE_BYTES :: 64

is_memory_type :: proc(t: TypeId) -> bool {
    k := get_type(t).kind
    if k != .Struct && k != .FixedSizeArray && k != .Union {
        return false
    }
    return type_size(t) > LARGE_AGGREGATE_BYTES
}

type_llvm_name :: proc(tid: TypeId) -> string {
    name, ok := get_ctx().cg_ty_names[tid]
    if !ok {
        mid, mid_ok := get_ctx().ty_modules[tid]
        if !mid_ok {
            gala_panicf("struct type %s has no module associated with it.", get(tid).name)
        }
        name = fmt.aprintf("%s.%s", module_prefix(mid), get(tid).name, allocator=get_ctx().allocator)
        get_ctx().cg_ty_names[tid] = name
        get_ctx().llvm_ty[tid] = fmt.aprintf("%%%s", name, allocator=get_ctx().allocator)
    }
    return name
}

mod_item_type_name :: proc(c: ^CGCtx, id: ItemId) -> string {
    return type_llvm_name(get_ctx().item_types[id])
}
// Declares, in THIS module's output, every named struct reachable by value from `id`.
ensure_type_declared :: proc(c: ^CGCtx, id: TypeId) {
    ty := get_type(id)
    #partial switch ty.kind {
    case .Struct:
        if c.declared_types[id] do return
        if ty.structure.is_anonymous {
            // nothing to emit, but named structs nested inside must be declared
            c.declared_types[id] = true
            for f in ty.structure.fields do ensure_type_declared(c, f.type)
            return
        }
        if ty.name == "" do return
        c.declared_types[id] = true
        llvm_name := type_llvm_name(id)
        fields := ty.structure.fields
        for f in fields do ensure_type_declared(c, f.type)
        strings.write_string(&c.extra_types, "%")
        strings.write_string(&c.extra_types, llvm_name)
        strings.write_string(&c.extra_types, " = type {")
        for f, k in fields {
            if k > 0 do strings.write_string(&c.extra_types, ",")
            strings.write_string(&c.extra_types, ty_to_llvm_str(c, f.type))
        }
        strings.write_string(&c.extra_types, "}\n")
    case .FixedSizeArray:
        ensure_type_declared(c, ty.fixed_size_array.type)
    }
}
// True if cg_addr can produce a real address for this expression, i.e. it
// names an object in memory (variable, field of one, element, deref).
is_addressable :: proc(c: ^CGCtx, id: ExprId) -> bool {
    #partial switch e in get_expr(id) {
    case Symbol:
        return cgscope_get(&c.scope, e.name).kind == .Variable
    case FieldAccess:
        return is_addressable(c, e.target)
    case Index, Deref:
        return true
    }
    return false
}

// Address of the memory holding the value of `id`. Uses the real address if
// the expression is an lvalue, otherwise evaluates it into a fresh
// entry-block temporary and returns that.
cg_value_addr :: proc(c: ^CGCtx, id: ExprId) -> string {
    if is_addressable(c, id) {
        return cg_addr(c, id)
    }
    slot := new_entry_alloca(c, ty_to_llvm_str(c, expr_ty(id)))
    cg_expr_into(c, id, slot)
    return slot
}

// `declare`s for these three are written once at the top of every module
// (see cg_module); a declare can't be emitted from inside a function body.

// dst <- src, `size` bytes, regions must not overlap.
cg_memcpy :: proc(c: ^CGCtx, dst: string, src: string, size: int) {
    cwritefln(c, "\tcall void @llvm.memcpy.p0.p0.i64(ptr %s, ptr %s, i64 %d, i1 false)",
        dst, src, size)
}

// dst <- src, `size` bytes, overlap allowed (e.g. `a = a`).
cg_memmove :: proc(c: ^CGCtx, dst: string, src: string, size: int) {
    cwritefln(c, "\tcall void @llvm.memmove.p0.p0.i64(ptr %s, ptr %s, i64 %d, i1 false)",
        dst, src, size)
}

// fill `size` bytes at dst with `value` (0 for zero-init).
cg_memset :: proc(c: ^CGCtx, dst: string, value: int, size: int) {
    cwritefln(c, "\tcall void @llvm.memset.p0.i64(ptr %s, i8 %d, i64 %d, i1 false)",
        dst, value, size)
}

// `name = alloca ty` in the current function's entry block (or inline when
// there's no function-level allocas builder, which is only ever in an
// entry block anyway).
cg_alloca_named :: proc(c: ^CGCtx, name: string, ty_str: string) {
    if c.allocas != nil {
        fmt.sbprintfln(c.allocas, "\t%s = alloca %s", name, ty_str)
    } else {
        cwritefln(c, "\t%s = alloca %s", name, ty_str)
    }
}

// Fresh-named entry-block alloca; returns the pointer name.
//
// The name must NOT be a bare number (`%49`): LLVM requires unnamed values
// to appear in increasing numeric order in the text, and a hoisted alloca
// is written above body code that was numbered earlier. A prefixed name
// (`%slot49`) has no ordering rule. The shared counter still makes it unique.
new_entry_alloca :: proc(c: ^CGCtx, ty_str: string) -> string {
    name := new_tmp(c, "slot")
    cg_alloca_named(c, name, ty_str)
    return name
}

// ---- TypeKind-level helpers (is_integer/is_float take TypeId and exclude
// Byte/Rune/Bool — these work on raw TypeKind and include them, since all
// three are LLVM integer types) ----

is_int_kind :: proc(k: TypeKind) -> bool {
    #partial switch k {
    case .UInt64, .UInt32, .UInt16, .UInt_8,
         .Int64, .Int32, .Int16, .Int_8,
         .Byte, .Rune, .Bool:
        return true
    }
    return false
}

is_float_kind :: proc(k: TypeKind) -> bool {
    #partial switch k {
    case .Flt64, .Flt32, .Flt16, .Flt_8:
        return true
    }
    return false
}

bit_width_of :: proc(k: TypeKind) -> int {
    #partial switch k {
    case .UInt64, .Int64, .Flt64:               return 64
    case .UInt32, .Int32, .Flt32:               return 32
    case .UInt16, .Int16, .Flt16:               return 16
    case .UInt_8, .Int_8, .Flt_8, .Byte, .Rune: return 8
    case .Bool:                                  return 1 // LLVM i1, not i8
    case: gala_panic("bit_width_of: not a scalar numeric kind")
    }
}

is_signed :: proc(k: TypeKind) -> bool {
    #partial switch k {
    case .Int64, .Int32, .Int16, .Int_8:
        return true
    case .UInt64, .UInt32, .UInt16, .UInt_8, .Byte, .Rune, .Bool:
        return false
    case:
        gala_panic("is_signed: not an integer-ish kind")
    }
}

// LLVM type text for a gala type. Scalars come straight from
// scalar_llvm_str (cg_abi.odin) so there is exactly one scalar table in the
// compiler; only aggregates are worth caching.
ty_to_llvm_str :: proc(c: ^CGCtx, id: TypeId) -> string {
    ensure_type_declared(c, id)
    if t, ok := get_ctx().llvm_ty[id]; ok {
        return t
    }
    ty := get_type(id)
    #partial switch ty.kind {
    case .UntypedInteger, .UntypedFloat:
        gala_panic("ty_to_llvm_str: untyped literal type reached codegen")
    case .Void:
        return "void"
    case .Slice, .String, .Any:
        // "any" is boxed as { data ptr, typeid } — same two-word shape as
        // a slice/string header, just with the second field reinterpreted
        // as a runtime type tag instead of a length. See cg_box_any.
        return "{ ptr, i64 }"
    case .Struct: {
        if ty.structure.is_anonymous {
            // literal LLVM type, no %name
            sb := strings.builder_make(get_ctx().allocator)
            strings.write_string(&sb, "{")
            for f, k in ty.structure.fields {
                if k > 0 do strings.write_string(&sb, ", ")
                strings.write_string(&sb, ty_to_llvm_str(c, f.type))
            }
            strings.write_string(&sb, "}")
            n := strings.to_string(sb)
            get_ctx().llvm_ty[id] = n
            return n
        }
        if ty.name == "" {
            debugln(ty)
            gala_panic("ty_to_llvm_str: struct has no LLVM name")
        }
        n := aprintf(c, "%%%s", type_llvm_name(id))
        get_ctx().llvm_ty[id] = n
        return n
    }
    case .Union: {
        a := type_align_of(id)
        return aprintf(c, "[%d x i%d]", type_size(id) / a, a * 8)
    }
    case .FixedSizeArray: {
        n := aprintf(c, "[%d x %s]", ty.fixed_size_array.size,
            ty_to_llvm_str(c, ty.fixed_size_array.type))
        get_ctx().llvm_ty[id] = n
        return n
    }
    case:
        return scalar_llvm_str(id)
    }
    unreachable()
}


// eg "%t1"
new_tmp :: proc(c: ^CGCtx, p := "", symbol := false) -> string {
    if symbol {
        return fmt.aprintf("@%s%d", p, next_tmp_index(c), allocator=c.arena.block_allocator)
    }
    return fmt.aprintf("%%%s%d", p, next_tmp_index(c), allocator=c.arena.block_allocator)
}
aprintf :: proc(c: ^CGCtx, format: string, data: ..any) -> string {
    return fmt.aprintf(format, ..data, allocator=get_ctx().allocator)
}

// `extractvalue { ptr, i64 } v, idx` — field 0 (data) or 1 (len) of a
// slice / string header.
cg_pair_field :: proc(c: ^CGCtx, v: string, idx: int, prefix := "") -> string {
    t := new_tmp(c, prefix)
    cwritefln(c, "\t%s = extractvalue {{ ptr, i64 }} %s, %d", t, v, idx)
    return t
}

// returns value
cg_fn_call_target :: proc(c: ^CGCtx, id: ExprId) -> string {
    v, ok := reduce_expr_to_single_value(c, cg_expr(c, id))
    if !ok {
        highlight_lines(get_span(id))
        gala_panic("Expression can't be void/must return.")
    }
    return v
}

// Stable per-TypeId integer used as the runtime tag inside a boxed `any`
// value. TypeId is already a small dense index into the type table, so it
// doubles as its own typeid. IMPORTANT: this must be the exact same
// function the `TypeAssert` (downcast, e.g. `x.(int)`) codegen uses to
// compare tags against — box and unbox have to agree on what a type's id
// is, or every assertion will spuriously fail (or worse, spuriously pass).
typeid_of :: proc(t: TypeId) -> i64 {
    return i64(t)
}

// LLVM textual IR: a `float`-typed constant is written as the hex bits of
// the *double* representation of the value (not the float's raw bits) —
// this is LLVM's own quirk, not a bug in our lowering.
// (Allocated from the compiler allocator, not the temp one: the text lives
// in a CGExprRes until the surrounding expression is emitted.)
llvm_float_const :: proc(v: f32) -> string {
    bits := transmute(u64)f64(v)
    return fmt.aprintf("0x%016X", bits, allocator=get_ctx().allocator)
}

// Always printed as hex bits, so doubles never lose precision to decimal.
llvm_double_const :: proc(v: f64) -> string {
    bits := transmute(u64)v
    return fmt.aprintf("0x%016X", bits, allocator=get_ctx().allocator)
}

// ============================================================================
// Statements
// ============================================================================

// Does every branch of this if/else-if/else chain end in a terminator?
// (Requires an `else`; otherwise control can fall past the whole chain.)
if_returns_everywhere :: proc(s: IfElse) -> bool {
    if !s.has_else_block do return false
    if !check_rets(s.base_block) || !check_rets(s.else_block) do return false
    for a in s.alt {
        if !check_rets(a.block) do return false
    }
    return true
}

stmt_ends_block :: proc(stmt: StmtId) -> bool {
    switch s in get(stmt) {
    // both are unconditional jumps (`br label ...`) — an LLVM basic-block
    // terminator, exactly like Return, so nothing may follow either in
    // the same block.
    case BreakStmt, ContinueStmt, Return:
        return true
    // Loops can run zero times (and `break` can leave them early), so
    // control can always fall through to whatever follows — even if the
    // body itself ends in a terminator.
    case WhileLoop, ForLoop:
        return false
    case IfElse:
        return if_returns_everywhere(s)
    case VarDec, Assignment, ExprId:
        return false
    case:
        gala_panic("stmt_ends_block: unhandled statement kind")
    }
    unreachable()
}

next_tmp_index :: proc(c: ^CGCtx) -> int {
    c.tmp_id += 1
    return c.tmp_id
}

// Emits `block` in a fresh child scope, optionally pre-binding one name
// (the `for` loop variable). Rejects statements after a terminator.
cg_scoped_block :: proc(c: ^CGCtx, block: Block, bind_name := "", bind_obj := CGObj{}) {
    old := c.scope
    c.scope = new_gcscope(&old)
    if bind_name != "" {
        c.scope.vars[bind_name] = bind_obj
    }

    last := len(block.stmts) - 1
    for statement, i in block.stmts {
        cg_stmt(c, statement)
        if stmt_ends_block(statement) && i != last {
            gala_panic("nothing past will be executed")
        }
    }

    free_cgscope(&c.scope)
    c.scope = old
}

LoopTargets :: struct {
    brk, cont: string,
}

// Registers loop `id`'s break/continue labels (keyed by its own StmtId —
// this is what get_ctx().break_lables[break/continue id] resolves to) and
// makes them the "current" targets so any IfElse nested in the body, at any
// depth, can register the same targets under its own StmtId too.
// Returns the previous targets for cg_leave_loop.
cg_enter_loop :: proc(c: ^CGCtx, id: StmtId, brk, cont: string) -> LoopTargets {
    c.break_labels[id] = brk
    c.continue_labels[id] = cont

    prev := LoopTargets{c.cur_break_label, c.cur_continue_label}
    c.cur_break_label = brk
    c.cur_continue_label = cont
    return prev
}

cg_leave_loop :: proc(c: ^CGCtx, prev: LoopTargets) {
    c.cur_break_label = prev.brk
    c.cur_continue_label = prev.cont
}

// `target op= rhs` with the target's address computed exactly once.
// s.value is the Binop the parser built (left is the target, which we skip).
cg_compound_assign :: proc(c: ^CGCtx, s: Assignment) {
    bin := get_expr(s.value).(Binop)
    tid := expr_ty(s.target)
    ty_str := ty_to_llvm_str(c, tid)
    assert(tid == expr_ty(bin.right), "expressions in binop must have the same type")

    ptr := cg_addr(c, s.target)
    cur := new_tmp(c)
    cwritefln(c, "\t%s = load %s, ptr %s", cur, ty_str, ptr)

    rhs, returns := reduce_expr_to_single_value(c, cg_expr(c, bin.right))
    assert(returns)

    result := ""
    if is_pointer(tid) {
        if bin.kind != .Addition && bin.kind != .Subtraction {
            gala_panic("unsupported pointer compound assignment")
        }
        ci := new_tmp(c)
        cwritefln(c, "\t%s = ptrtoint ptr %s to i64", ci, cur)
        ri := new_tmp(c)
        cwritefln(c, "\t%s = ptrtoint ptr %s to i64", ri, rhs)
        arith := "add" if bin.kind == .Addition else "sub"
        sum := new_tmp(c)
        cwritefln(c, "\t%s = %s i64 %s, %s", sum, arith, ci, ri)
        result = new_tmp(c)
        cwritefln(c, "\t%s = inttoptr i64 %s to ptr", result, sum)
    } else {
        op := binop_llvm_op(bin, operand_class(tid))

        result = new_tmp(c)
        cwritefln(c, "\t%s = %s %s %s, %s", result, op, ty_str, cur, rhs)
    }
    cwritefln(c, "\tstore %s %s, ptr %s", ty_str, result, ptr)
}
cg_stmt :: proc(c: ^CGCtx, id: StmtId) {
    switch s in get_stmt(id) {
    case BreakStmt: {
        // get_ctx().break_lables maps this break's own StmtId to the
        // StmtId of the construct it targets (the enclosing loop, or —
        // if it was registered while inside nested if/else — the
        // innermost IfElse, which itself carries the same loop's label
        // through via the passthrough registration below).
        target_id := get_ctx().break_lables[id]
        label, ok := c.break_labels[target_id]
        if !ok {
            gala_panic("break: no enclosing loop label found")
        }
        cwritefln(c, "\tbr label %%%s", label)
    }
    case ContinueStmt: {
        target_id := get_ctx().break_lables[id]
        label, ok := c.continue_labels[target_id]
        if !ok {
            gala_panic("continue: no enclosing loop label found")
        }
        cwritefln(c, "\tbr label %%%s", label)
    }
    case WhileLoop: {
        id_suffix := next_tmp_index(c)

        cond_label := aprintf(c, "while_cond_label%d", id_suffix)
        body_label := aprintf(c, "while_body_label%d", id_suffix)
        end_label  := aprintf(c, "while_end_label%d", id_suffix)

        prev := cg_enter_loop(c, id, end_label, cond_label)

        // jump into condition check
        cwritefln(c, "\tbr label %%%s", cond_label)

        // condition block
        cwritefln(c, "%s:", cond_label)
        cond, returns := reduce_expr_to_single_value(c, cg_expr(c, s.cond))
        assert(returns)

        // type checker guarantees this, but keep this assertion in codegen
        assert(get_type(expr_ty(s.cond)).kind == .Bool)

        cwritefln(c, "\tbr i1 %s, label %%%s, label %%%s", cond, body_label, end_label)

        // body
        cwritefln(c, "%s:", body_label)
        cg_scoped_block(c, s.block)

        // only loop back if body doesn't terminate
        if !check_rets(s.block) {
            cwritefln(c, "\tbr label %%%s", cond_label)
        }

        // exit
        cwritefln(c, "%s:", end_label)
        cg_leave_loop(c, prev)
    }
    case ForLoop: {
        if true do unreachable()
        // `for name in expr { body }` lowers to:
        //
        //     <evaluate expr ONCE -> data ptr + length>
        //     %idx = alloca i64 ; store 0
        //     %name = alloca elem
        //     br cond
        //   cond:   %i = load idx ; br (i < len) ? body : end
        //   body:   name = data[i] ; ...user statements...  br step
        //   step:   idx += 1 ; br cond            <- `continue` jumps HERE
        //   end:                                  <- `break` jumps here
        //
        // `continue` must target `step`, not `cond` (unlike while), or the
        // index would never advance.
        id_suffix := next_tmp_index(c)

        cond_label := aprintf(c, "for_cond_label%d", id_suffix)
        body_label := aprintf(c, "for_body_label%d", id_suffix)
        step_label := aprintf(c, "for_step_label%d", id_suffix)
        end_label  := aprintf(c, "for_end_label%d", id_suffix)

        // Evaluate the iterable exactly once, before the loop, so side
        // effects (e.g. `for x in make_list()`) don't repeat per iteration.
        // These SSA values are defined in a block that dominates the whole
        // loop, so using them in cond/body is fine.
        data_ptr, len_v, elem_tid := cg_iter_parts(c, s.expr)
        elem_ty := ty_to_llvm_str(c, elem_tid)

        idx_slot := new_tmp(c, "for_idx")
        cg_alloca_named(c, idx_slot, "i64")
        cwritefln(c, "\tstore i64 0, ptr %s", idx_slot)

        // The loop variable: one slot, overwritten each iteration with a
        // by-value copy of the element.
        var_slot := aprintf(c, "%%%s.for%d", s.name, id_suffix)
        cg_alloca_named(c, var_slot, elem_ty)

        prev := cg_enter_loop(c, id, end_label, step_label)

        cwritefln(c, "\tbr label %%%s", cond_label)

        // --- cond ---
        cwritefln(c, "%s:", cond_label)
        cur_i := new_tmp(c, "for_i")
        cwritefln(c, "\t%s = load i64, ptr %s", cur_i, idx_slot)
        in_range := new_tmp(c, "for_lt")
        cwritefln(c, "\t%s = icmp slt i64 %s, %s", in_range, cur_i, len_v)
        cwritefln(c, "\tbr i1 %s, label %%%s, label %%%s", in_range, body_label, end_label)

        // --- body ---
        cwritefln(c, "%s:", body_label)
        elem_ptr := new_tmp(c, "for_elem_ptr")
        cwritefln(c, "\t%s = getelementptr inbounds %s, ptr %s, i64 %s",
            elem_ptr, elem_ty, data_ptr, cur_i)
        if is_memory_type(elem_tid) {
            // large element: copy it with memcpy, never load it as a value
            cg_memcpy(c, var_slot, elem_ptr, int(type_size(elem_tid)))
        } else {
            elem_val := new_tmp(c, "for_elem")
            cwritefln(c, "\t%s = load %s, ptr %s", elem_val, elem_ty, elem_ptr)
            cwritefln(c, "\tstore %s %s, ptr %s", elem_ty, elem_val, var_slot)
        }

        cg_scoped_block(c, s.block, s.name, CGObj{.Variable, var_slot})

        // fall into step unless the body already ended in a terminator
        if !check_rets(s.block) {
            cwritefln(c, "\tbr label %%%s", step_label)
        }

        // --- step --- (always emitted: `continue` targets it)
        cwritefln(c, "%s:", step_label)
        step_i := new_tmp(c, "for_i")
        cwritefln(c, "\t%s = load i64, ptr %s", step_i, idx_slot)
        next_i := new_tmp(c, "for_next")
        cwritefln(c, "\t%s = add i64 %s, 1", next_i, step_i)
        cwritefln(c, "\tstore i64 %s, ptr %s", next_i, idx_slot)
        cwritefln(c, "\tbr label %%%s", cond_label)

        // --- end ---
        cwritefln(c, "%s:", end_label)
        cg_leave_loop(c, prev)
    }
    case ExprId: {
        r := cg_expr(c, s)
        // a discarded large-aggregate result (e.g. `make_terrain();`) is
        // just an address — don't load the whole thing to throw it away
        if r.kind != .Place {
            _, _ = reduce_expr_to_single_value(c, r)
        }
    }
    case IfElse: {
        id_suffix := next_tmp_index(c)
        end_label := aprintf(c, "end_label%d", id_suffix)

        // Passthrough registration: if this IfElse sits inside an
        // enclosing loop (tracked via c.cur_break_label/cur_continue_label,
        // set by cg_enter_loop around its body), register the SAME
        // targets under this IfElse's own StmtId. This makes a
        // break/continue resolve correctly regardless of whether the
        // resolution phase pointed it directly at the loop or at this
        // intermediate IfElse.
        // Guarded so an IfElse outside any loop doesn't insert a bogus
        // empty-string entry (which would otherwise satisfy the `ok` check
        // in BreakStmt/ContinueStmt and mask a real "break outside loop"
        // error).
        if c.cur_break_label != "" {
            c.break_labels[id] = c.cur_break_label
        }
        if c.cur_continue_label != "" {
            c.continue_labels[id] = c.cur_continue_label
        }

        // Precompute all labels we'll need up front so branch targets
        // can reference "the next check" before that block is emitted.
        base_block := aprintf(c, "base_block_label%d", id_suffix)

        alt_cond_labels := make([dynamic]string, get_ctx().allocator)
        alt_body_labels := make([dynamic]string, get_ctx().allocator)
        for _, i in s.alt {
            append(&alt_cond_labels, aprintf(c, "alt_cond_label%d_%d", id_suffix, i))
            append(&alt_body_labels, aprintf(c, "alt_block_label%d_%d", id_suffix, i))
        }
        has_else := s.has_else_block
        else_label := aprintf(c, "else_block_label%d", id_suffix)

        // Where control goes when the condition at position `i` of the
        // chain (0 = base, 1.. = alts) is false.
        next_after :: proc(i: int, alts: int, has_else: bool, alt_conds: [dynamic]string,
                else_label, end_label: string) -> string {
            if i < alts do return alt_conds[i]
            if has_else do return else_label
            return end_label
        }

        // emits `br i1 cond, body, next` for a boolean condition expression
        branch_on :: proc(c: ^CGCtx, cond_id: ExprId, body, next: string) {
            cond, returns := reduce_expr_to_single_value(c, cg_expr(c, cond_id))
            assert(returns)
            assert(get_type(expr_ty(cond_id)).kind == .Bool)
            cwritefln(c, "\tbr i1 %s, label %%%s, label %%%s", cond, body, next)
        }

        // --- base condition ---
        branch_on(c, s.base_con, base_block,
            next_after(0, len(s.alt), has_else, alt_cond_labels, else_label, end_label))
        cwritefln(c, "%s:", base_block)
        cg_scoped_block(c, s.base_block)
        if !check_rets(s.base_block) {
            cwritefln(c, "\tbr label %%%s", end_label)
        }

        // --- else-if chain ---
        for a, i in s.alt {
            cwritefln(c, "%s:", alt_cond_labels[i])
            branch_on(c, a.cond, alt_body_labels[i],
                next_after(i + 1, len(s.alt), has_else, alt_cond_labels, else_label, end_label))
            cwritefln(c, "%s:", alt_body_labels[i])
            cg_scoped_block(c, a.block)
            if !check_rets(a.block) {
                cwritefln(c, "\tbr label %%%s", end_label)
            }
        }

        // --- else ---
        if has_else {
            cwritefln(c, "%s:", else_label)
            cg_scoped_block(c, s.else_block)
            if !check_rets(s.else_block) {
                cwritefln(c, "\tbr label %%%s", end_label)
            }
        }

        // returns in every branch, so never needs end label
        if !if_returns_everywhere(s) {
            cwritefln(c, "%s:", end_label)
        }
    }
    case VarDec: {
        obj_id := get_ctx().stmt_objects[id]
        obj := get_obj(obj_id)
        var_tid := obj.type.(TypeId)
        var_ty_str := ty_to_llvm_str(c, var_tid)
        name := aprintf(c, "%%%s.%d", s.name, obj_id)

        if is_memory_type(var_tid) {
            // Large aggregate: allocate the slot and build the value
            // straight into it (memcpy / in-place literal / sret call).
            // The value is never an SSA value.
            cg_alloca_named(c, name, var_ty_str)
            cg_expr_into(c, s.value, name)
        } else {
            value, returns := reduce_expr_to_single_value(c, cg_expr(c, s.value))
            assert(returns)
            // allocate (entry block, so loops don't grow the stack)
            cg_alloca_named(c, name, var_ty_str)
            cwritefln(c, "\tstore %s %s, ptr %s", var_ty_str, value, name)
        }

        // write name to scope (after the initialiser, so `x := x` still
        // sees the outer x)
        c.scope.vars[s.name] = {.Variable, name}
    }
    case Return: {
        e, has_value := s.expr.(ExprId)
        if !has_value {
            cwriteln(c, "\tret void")
            break
        }

        ret_tid := expr_ty(e)
        ret_ty_str := ty_to_llvm_str(c, ret_tid)

        if c.cur_fn_ret.mode == .Indirect {
            if is_memory_type(ret_tid) {
                // Large aggregate return: construct directly into the
                // caller's sret slot (or memcpy from a local) — no SSA copy.
                cg_expr_into(c, e, SRET_PARAM)
            } else {
                r, returns := reduce_expr_to_single_value(c, cg_expr(c, e))
                assert(returns)
                // caller-allocated slot, already passed in as %.sret
                cwritefln(c, "\tstore %s %s, ptr %s", ret_ty_str, r, SRET_PARAM)
            }
            cwriteln(c, "\tret void")
            break
        }

        r, returns := reduce_expr_to_single_value(c, cg_expr(c, e))
        assert(returns)
        if c.cur_fn_ret.needs_coercion {
            slot := new_entry_alloca(c, ret_ty_str)
            cwritefln(c, "\tstore %s %s, ptr %s", ret_ty_str, r, slot)
            coerced := new_tmp(c)
            cwritefln(c, "\t%s = load %s, ptr %s", coerced, c.cur_fn_ret.coerced_type, slot)
            cwritefln(c, "\tret %s %s", c.cur_fn_ret.coerced_type, coerced)
        } else {
            cwritefln(c, "\tret %s %s", ret_ty_str, r)
        }
    }
    case Assignment: {
        if s.compound {
            cg_compound_assign(c, s)
            break
        }
        target_tid := expr_ty(s.target)
        if is_memory_type(target_tid) {
            size := int(type_size(target_tid))
            if is_addressable(c, s.value) {
                // memory -> memory. memmove, since `a = a` (or overlapping
                // views of the same object) is legal and memcpy forbids it.
                src := cg_addr(c, s.value)
                dst := cg_addr(c, s.target)
                cg_memmove(c, dst, src, size)
            } else {
                // Evaluate into a temporary first: the expression may read
                // the target (`t = {a = t.b}`), so it can't be built in
                // place.
                tmp := new_entry_alloca(c, ty_to_llvm_str(c, target_tid))
                cg_expr_into(c, s.value, tmp)
                dst := cg_addr(c, s.target)
                cg_memcpy(c, dst, tmp, size)
            }
        } else {
            value, returns := reduce_expr_to_single_value(c, cg_expr(c, s.value))
            assert(returns)
            target_ptr := cg_addr(c, s.target)
            cwritefln(c, "\tstore %s %s, ptr %s",
                ty_to_llvm_str(c, expr_ty(s.target)), value, target_ptr)
        }
    }
    case:
        gala_panic("cg_stmt: unhandled statement kind")
    }
    cwriteln(c, "")
}

// Evaluates an iterable expression (for `for x in <expr>`) EXACTLY ONCE and
// returns everything the loop needs:
//   data_ptr  - pointer to element 0
//   len_v     - element count, an i64 operand (literal or SSA value)
//   elem_tid  - TypeId of one element
// Unlike cg_data_ptr this also yields the length, which for slices/strings
// lives in the same {ptr, i64} value — so the value is evaluated once and
// both fields extracted from it, instead of evaluating the expression twice.
cg_iter_parts :: proc(c: ^CGCtx, id: ExprId) -> (data_ptr: string, len_v: string, elem_tid: TypeId) {
    ty := get_type(expr_ty(id))
    #partial switch ty.kind {
    case .FixedSizeArray:
        // iterates the array in place (no copy): the address is the data
        // pointer, the length is a compile-time constant. (An rvalue array,
        // e.g. `for x in make_arr()`, is written to a temporary first.)
        return cg_value_addr(c, id),
               aprintf(c, "%d", ty.fixed_size_array.size),
               ty.fixed_size_array.type

    case .Slice, .String:
        v, ok := reduce_expr_to_single_value(c, cg_expr(c, id))
        assert(ok)
        elem := ty.slice.type if ty.kind == .Slice else byte_type()
        return cg_pair_field(c, v, 0, "for_data"), cg_pair_field(c, v, 1, "for_len"), elem

    case:
        highlight_lines(get_span(id))
        gala_panic("for: expression is not iterable (expected array, slice or string)")
    }
}

// Resolves ANY indexable expression down to a pointer that already points at
// element 0, plus that element's LLVM type string. This is the one place
// that needs to know how array/slice/pointer differ — everything downstream
// (Index, TakeSlice, `for x in ...`) is a uniform single-index GEP off the
// result.
cg_data_ptr :: proc(c: ^CGCtx, id: ExprId) -> (ptr: string, elem_ty_str: string) {
    ty := get_type(expr_ty(id))
    #partial switch ty.kind {
    case .FixedSizeArray:
        // arrays always live in memory, never SSA values — get its address,
        // which (with opaque pointers) already IS "pointer to element 0".
        // An rvalue array (e.g. `make_arr()[3]`) is spilled to a temporary.
        return cg_value_addr(c, id), ty_to_llvm_str(c, ty.fixed_size_array.type)

    case .Slice, .String: {
        // small by-value {ptr, i64} — get the value however it naturally
        // arises (load, extractvalue, straight from TakeSlice, a function
        // return, whatever cg_expr already knows how to do) and pull the
        // data pointer straight out of it
        v, ok := reduce_expr_to_single_value(c, cg_expr(c, id))
        assert(ok)
        elem := ty.slice.type if ty.kind == .Slice else byte_type()
        return cg_pair_field(c, v, 0), ty_to_llvm_str(c, elem)
    }
    case .Pointer: {
        // already IS a pointer to element 0
        v, ok := reduce_expr_to_single_value(c, cg_expr(c, id))
        assert(ok)
        return v, ty_to_llvm_str(c, ty.ptr)
    }
    case:
        gala_panic("cg_data_ptr: not indexable")
    }
    unreachable()
}

// Address of target[index]. Used by both cg_expr's Index (which loads
// afterward) and cg_addr's Index (which just returns this).
cg_elem_ptr :: proc(c: ^CGCtx, target: ExprId, index: ExprId) -> string {
    base_ptr, elem_ty := cg_data_ptr(c, target)
    idx_v, ok := reduce_expr_to_single_value(c, cg_expr(c, index))
    assert(ok)
    idx_ty := ty_to_llvm_str(c, expr_ty(index))

    t := new_tmp(c)
    cwritefln(c, "\t%s = getelementptr inbounds %s, ptr %s, %s %s",
        t, elem_ty, base_ptr, idx_ty, idx_v)
    return t
}

cg_field_ptr :: proc(c: ^CGCtx, base_ptr: string, base_ty: TypeId, idx: int) -> string {
    if get_type(base_ty).kind == .Union do return base_ptr
    t := new_tmp(c)
    cwritefln(c, "\t%s = getelementptr inbounds %s, ptr %s, i32 0, i32 %d",
        t, ty_to_llvm_str(c, base_ty), base_ptr, idx)
    return t
}
cg_addr :: proc(c: ^CGCtx, id: ExprId) -> string {
    #partial switch e in get_expr(id) {
    case Symbol: {
        v := cgscope_get(&c.scope, e.name)
        #partial switch v.kind {
        case .Variable:
            return v.name
        case .Argument:
            // can't assign to a by-value param (pointer/array args are
            // reached through Deref / Index, not through their own address)
            gala_panic("arguments can't have an address:", e.name)
        }
        gala_panic("cg_addr: symbol has no address:", e.name)
    }
    case FieldAccess: {
        // cg_value_addr: for a real lvalue this is just cg_addr; an rvalue
        // struct (`make_terrain().pixels[3]`) is spilled to a temporary.
        base_ptr := cg_value_addr(c, e.target)
        base_ty := expr_ty(e.target)
        idx, _ := field_of(base_ty, e.field, id)
        return cg_field_ptr(c, base_ptr, base_ty, idx)
    }
    case Index:
        return cg_elem_ptr(c, e.target, e.index)
    case Deref: {
        // generate expression, as that would already be a pointer, otherwise
        // dereferencing wouldn't make sense; the pointer VALUE is the address
        ptr_val, returns := reduce_expr_to_single_value(c, cg_expr(c, e.expr))
        assert(returns)
        if get_type(expr_ty(e.expr)).kind != .Pointer {
            gala_panic("cannot dereference non-pointer type")
        }
        return ptr_val
    }
    case Cast, Transmute: {
        v, returns := reduce_expr_to_single_value(c, cg_expr(c, id))
        assert(returns)
        return v
    }
    }
    debugln(get(id))
    highlight_lines(get_span(id))
    gala_panic("not an lvalue")
}

field_of :: proc(tid: TypeId, name: string, e: ExprId) -> (idx: int, field_ty: TypeId) {
    ty := get_type(tid)
    if ty.kind == .Any {
        switch name {
        case "data": return 0, intern_type(Type{kind = .Pointer, ptr = void_type()})
        case "id":   return 1, integer_type()
        }
        gala_panic("any has no field", name)
    }
    if ty.kind == .Union {
        for f, k in ty.union_fields {
            if f.name == name do return k, f.type
        }
        gala_panic("union has no field", name)
    }
    idx = struct_field_index(ty, name)
    return idx, ty.structure.fields[idx].type
}

// ============================================================================
// Functions
// ============================================================================

// Writes the signature line for a function — `declare` for a prototype
// (extern, or a forward-declared import), `define` for the header of an
// actual definition. Both extern and gala (non-extern) forms go through
// the same ABI lowering (cg_abi_lower_signature, shared with calls) and the
// same parameter-writing loop; the only per-kind divergence is:
//   - is_extern: only extern C functions get a literal `...` for a
//     trailing variadic — a gala function's trailing variadic parameter
//     has already been lowered to an ordinary slice arg by this point.
//   - define: whether we're opening a definition (needs real parameter
//     names to bind and reference in the body, and binds them into
//     c.scope) or just declaring a prototype (types only, no names,
//     nothing bound into scope).
//   - internal: `define internal` linkage, used for function literals so
//     their generated names only need to be unique within one module.
//
// Works from a function TypeId + a bare name (no `@`) rather than an item,
// so named functions and function literals share all of it.
//
// Returns the lowered signature and, when define==true, the raw
// (pre-coercion) SSA names of each parameter as they appear on the
// `define` line — the caller needs both to emit the prologue and set
// `c.cur_fn_ret` before generating the body.

// `kind` defaults to the target's native ABI (.None). Pass .SysV / .Win64
// explicitly to force a foreign convention; sig.llvm_cc then carries the
// matching `x86_64_sysvcc ` / `win64cc ` keyword, which the CALL site must
// emit too.
//
// param_raw_names (define only) stays index-aligned with sig.args:
//   .Direct (no coercion) -> "%name"      bound as .Argument
//   .Direct (coerced)     -> "%name.abi"  NOT bound; prologue reconstructs
//   .ByVal / .Indirect    -> "%name"      bound as .Variable (already an address)
//   .Ignore               -> ""           bound as .Argument zeroinitializer
cg_fn_header :: proc(c: ^CGCtx, fn_type_id: TypeId, name: string,
        is_extern := false, define := false, internal := false,
        kind: AbiKind = .None) -> (sig: AbiSignature, param_raw_names: [dynamic]string) {
    fn_ty := get_type(fn_type_id)
    sig = cg_abi_lower_signature(c, fn_type_id, kind, is_extern)

    // define [linkage] [cconv] [ret attrs] <ret ty> @name(...)
    cwrite(c, "define " if define else "declare ")
    if internal {
        cwrite(c, "internal ")
    }
    cwrite(c, sig.llvm_cc)
    switch sig.ret.mode {
    case .Indirect, .Ignore:
        cwrite(c, "void ")
    case .Direct:
        if sig.ret.ext != "" {
            cwritef(c, "%s ", sig.ret.ext)
        }
        cwritef(c, "%s ", sig.ret.coerced_type)
    }
    cwritef(c, "@%s ", name)
    cwrite(c, "(")

    wrote_any := false
    sep :: proc(c: ^CGCtx, wrote_any: ^bool) {
        if wrote_any^ do cwrite(c, ", ")
        wrote_any^ = true
    }

    if sig.ret.mode == .Indirect {
        sep(c, &wrote_any)
        cwritef(c, "ptr sret(%s) align %d", ty_to_llvm_str(c, sig.ret.orig_type), sig.ret.sret_align)
        if define {
            cwritef(c, " %s", SRET_PARAM)
        }
    }

    if define {
        param_raw_names = make([dynamic]string, allocator = get_ctx().allocator)
    }


    // sig.args is fixed args, plus (for gala variadics) one trailing slice

    // arg named fn.variadic_name — all lowered the same way.
    for al, k in sig.args {
        is_gala_variadic := k >= len(fn_ty.fn.args)
        arg_name := fn_ty.fn.variadic_name if is_gala_variadic else fn_ty.fn.args[k].name

        base := aprintf(c, "%s.gala_variadic", arg_name) if is_gala_variadic else arg_name


        switch al.mode {
        case .Ignore:
            // Empty aggregate (SysV): no LLVM parameter at all, so no
            // separator. The name still has to resolve inside the body; an
            // empty value carries no data, so a constant is enough.
            if define {
                append(&param_raw_names, "")
                c.scope.vars[arg_name] = {.Argument, "zeroinitializer"}
            }

        case .ByVal:
            sep(c, &wrote_any)
            cwritef(c, "ptr byval(%s) align %d", ty_to_llvm_str(c, al.orig_type), al.byval_align)
            if define {
                pname := aprintf(c, "%%%s", base)

                cwritef(c, " %s", pname)

                append(&param_raw_names, pname)
                c.scope.vars[arg_name] = {.Variable, pname}
            }

        case .Indirect:
            // Win64: plain pointer to a caller-made copy that this function
            // owns for the duration of the call — safe to read AND write in
            // place, so bind it directly like byval (no local copy needed).
            sep(c, &wrote_any)
            cwrite(c, "ptr noundef")
            if define {
                pname := aprintf(c, "%%%s", base)
                cwritef(c, " %s", pname)
                append(&param_raw_names, pname)
                c.scope.vars[arg_name] = {.Variable, pname}
            }

        case .Direct:
            sep(c, &wrote_any)

            cwrite(c, al.coerced_type)
            if al.ext != "" {
                cwritef(c, " %s", al.ext)
            }
            if define {
                if al.needs_coercion {
                    pname := aprintf(c, "%%%s.abi", base)
                    cwritef(c, " %s", pname)

                    append(&param_raw_names, pname)
                } else {

                    pname := aprintf(c, "%%%s", base)
                    cwritef(c, " %s", pname)
                    append(&param_raw_names, pname)
                    c.scope.vars[arg_name] = {.Argument, pname}
                }
            }
        }

    }

    // extern C functions keep a literal `...`
    if fn_ty.fn.is_variadic && is_extern {
        sep(c, &wrote_any)
        cwrite(c, "...")
    }

    if define {
        cwrite(c, ") ")
    } else {
        cwriteln(c, ")")

    }
    return
}


// Item-based wrapper: declarations for extern functions and for functions
// pulled in via imports. (Definitions of named functions go through
// cg_fn_definition, see cg_item.)
cg_fn_declaration :: proc(c: ^CGCtx, id: ItemId, is_extern := false) {
    obj := get_ctx().objs[get_ctx().item_objects[id]]
    fn_type_id := obj.type.(TypeId)

    // extern C functions keep their own symbol name; everything else uses
    // the compiler's per-item name
    name := obj.name if is_extern else get_ctx().cg_item_names[id]
    _, _ = cg_fn_header(c, fn_type_id, name, is_extern)
}

// Emits a complete function definition — header, entry block, argument
// prologue, body statements, implicit trailing `ret void` — into the
// current builder. Shared by named functions (cg_item's FnDec) and
// function literals (cg_fn_lit).
//
// The caller owns the scope: c.scope must already be a fresh scope with
// the right parent (parameters are bound into it by cg_fn_header).
//
// The body is generated into a separate builder so that every alloca made
// along the way (locals, big temporaries, sret slots — see c.allocas) can
// be written in one block right after the prologue, i.e. in the entry
// block, before the body text.
cg_fn_definition :: proc(c: ^CGCtx, fn_type_id: TypeId, name: string, block: Block, id: $ID, internal := false) {
    old_ret := c.cur_fn_ret

    sig, param_raw_names := cg_fn_header(c, fn_type_id, name, false, true, internal)
    c.cur_fn_ret = sig.ret

    cwriteln(c, "{")
    cwriteln(c, "entry:")

    fn_ty := get_type(fn_type_id)

    // prologue: unconvert any coerced-by-value struct args back into
    // a real aggregate SSA value via a memory roundtrip
    for a, k in fn_ty.fn.args {
        al := sig.args[k]
        if al.mode == .Direct && al.needs_coercion {
            real_ty_str := ty_to_llvm_str(c, al.orig_type)
            slot := new_tmp(c)
            cwritefln(c, "\t%s = alloca %s", slot, real_ty_str)
            cwritefln(c, "\tstore %s %s, ptr %s", al.coerced_type, param_raw_names[k], slot)
            loaded := new_tmp(c)
            cwritefln(c, "\t%s = load %s, ptr %s", loaded, real_ty_str, slot)
            c.scope.vars[a.name] = {.Argument, loaded}
        }
    }

    // from here on: body -> its own builder, allocas -> their own builder
    outer_b := c.b
    saved_allocas := c.allocas

    alloca_b: strings.Builder
    strings.builder_init(&alloca_b, get_ctx().allocator)
    body_b: strings.Builder
    strings.builder_init(&body_b, get_ctx().allocator)

    c.b = &body_b
    c.allocas = &alloca_b

    block_ends := false
    for statement, index in block.stmts {
        cg_stmt(c, statement)

        if stmt_ends_block(statement) {
            block_ends = true
            if index != len(block.stmts) - 1 {
                gala_panic("nothing past will be executed")
            }
            break
        }
    }

    if !block_ends {
        // sret functions have an ABI return of void too, so reaching the
        // end of either kind still needs `ret void`.
        if sig.ret.mode == .Indirect || sig.ret.coerced_type == "void" {
            cwriteln(c, "\tret void")
        } else {
            highlight_lines(get_span(id));
            gala_panic("Function does not return a value")
        }
    }

    // restore, then splice: entry allocas first, then the body
    c.b = outer_b
    c.allocas = saved_allocas

    cwrite(c, strings.to_string(alloca_b))
    cwrite(c, strings.to_string(body_b))
    cwriteln(c, "}")

    c.cur_fn_ret = old_ret
}

cg_item :: proc(c: ^CGCtx, id: ItemId) {
    switch i in get_item(id) {
    case Forward: {}
    case TypeAlias: {}
    case ConstDec: {} // ok
    case Import: {} // ok
    case StructDec: {
        if c.declared_items[id] {
        } else {
            c.declared_items[id] = true
            ensure_type_declared(c, get_ctx().item_types[id])
        }
    }
    case GlobalVarDec: {
        // declared by cg_globals_dec, initialised by cg_globals_init
    }
    case ExternFnDec: {
        // cg_items_dec may already have declared this C symbol while walking
        // an import; LLVM rejects a second `declare` of the same name.
        if c.emitted_externs[i.name] do return
        c.emitted_externs[i.name] = true
        cg_fn_declaration(c, id, is_extern = true)
    }
    case FnDec: {
        old_scope := c.scope
        c.scope = new_gcscope(&old_scope)

        fn_type_id := get_ctx().objs[get_ctx().item_objects[id]].type.(TypeId)
        cg_fn_definition(c, fn_type_id, get_ctx().cg_item_names[id], i.block, id)

        // reset scope
        c.scope = old_scope
    }
    case:
        gala_panic("cg_item: unhandled item kind")
    }
}

cg_ast :: proc(c: ^CGCtx, ast: ^AST) {
    for id in ast.items {
        cg_item(c, id)
    }
}

check_rets :: proc(b: Block) -> bool {
    if len(b.stmts) < 1 { return false }
    return stmt_ends_block(b.stmts[len(b.stmts)-1]) // does the last statement end the block?
}

check_fn :: proc(f: FnDec) -> bool {
    if !check_rets(f.block) {
        highlight_lines(get_ctx().current_file, f.span)
        gala_panic("function must return at all branches")
    }
    return true
}

cgscope_get :: proc(scope: ^CGScope, v: string) -> CGObj {
    s := scope
    for s != nil {
        if n, ok := s.vars[v]; ok do return n
        s = s.parent
    }
    debugln(v, "doesn't exist cg scope get")
    return CGObj{kind=.Invalid}
}

// ============================================================================
// String constants
// ============================================================================

// Escapes a byte for LLVM's c"..." string-constant syntax.
// LLVM requires every byte outside printable, non-special ASCII
// to be hex-escaped as \XX (two uppercase hex digits, no 0x prefix).
llvm_escape_byte :: proc(sb: ^strings.Builder, b: byte) {
    switch b {
    case '\\':
        strings.write_string(sb, "\\5C")
    case '"':
        strings.write_string(sb, "\\22")
    case:
        if b >= 0x20 && b < 0x7f {
            // printable ASCII, safe to emit directly
            strings.write_byte(sb, b)
        } else {
            fmt.sbprintf(sb, "\\%02X", b)
        }
    }
}

StringGlobalResult :: struct {
    ir:         string, // the full .ll global definition text
    array_type: string, // e.g. "[6 x i8]" -- the array type as declared (INCLUDES null term)
    len:        int,    // logical length, EXCLUDING the null terminator
    s:          string, // name
}

emit_string_global :: proc(name: string, content: string) -> StringGlobalResult {
    sb: strings.Builder
    strings.builder_init(&sb, get_ctx().allocator)

    logical_len := len(content) // length WITHOUT null term (this is what you store as `i64` len)
    array_type := fmt.aprintf("[%d x i8]", logical_len + 1, allocator=get_ctx().allocator)

    fmt.sbprintf(&sb, "%s = private unnamed_addr constant %s c\"", name, array_type)
    for i in 0 ..< len(content) {
        llvm_escape_byte(&sb, content[i])
    }
    strings.write_string(&sb, "\\00\", align 1\n") // trailing null terminator

    return StringGlobalResult{
        ir          = strings.to_string(sb),
        array_type  = array_type,
        len         = logical_len,
        s           = name,
    }
}

// ============================================================================
// Global variables
// ============================================================================
//
// A top-level `x := expr;` / `x: T = expr;` becomes an LLVM global
// `@<mod prefix>.x = global T zeroinitializer` in the module that defines it
// (`external global T` in every module that imports it). Its initialiser
// runs at startup in a per-module `__init_globals` function, in declaration
// order; the entry `main` wrapper calls the imported modules' init functions
// first (post-order over the import graph), then its own, then gala `main`.
//
// In scope a global is a `.Variable` whose name is the `@global` address, so
// the existing load-on-read / store-through-address paths work unchanged.

items_have_globals :: proc(items: []ItemId) -> bool {
    for id in items {
        if _, ok := get_item(id).(GlobalVarDec); ok do return true
    }
    return false
}

// Cached "gala.mod_src_foo" prefix for a module.
module_prefix :: proc(mid: ModId) -> string {
    if p, ok := get_ctx().cg_module_prefix[mid]; ok {
        return p
    }
    p := mod_prefix_from_path(get_ctx().mods[mid].path, prefix="gala_")
    get_ctx().cg_module_prefix[mid] = p
    return p
}

// "@gala.mod_src_foo.__init_globals"
mod_init_name :: proc(c: ^CGCtx, mid: ModId) -> string {
    return aprintf(c, "@%s.__init_globals", module_prefix(mid))
}

// Writes the `@global = [external] global T` lines. Runs after cg_items_dec
// so every struct type already has its LLVM name (a global may use a struct
// declared later in the file, or in a later import).
cg_globals_dec :: proc(c: ^CGCtx) {
    for g in c.pending_globals {
        objid := get_ctx().item_objects[g.id]
        ty_str := ty_to_llvm_str(c, get_ctx().objs[objid].type.(TypeId))
        name := get_ctx().cg_item_names[g.id]
        if g.external {
            cwritefln(c, "@%s = external global %s", name, ty_str)
        } else {
            cwritefln(c, "@%s = global %s zeroinitializer", name, ty_str)
        }
    }
}

// Emits this module's init function: evaluates each global's initialiser in
// declaration order and stores it into the global.
cg_globals_init :: proc(c: ^CGCtx, mid: ModId, items: []ItemId) {
    old_ret := c.cur_fn_ret

    cwritef(c, "define void %s() ", mod_init_name(c, mid))
    cwriteln(c, "{")
    cwriteln(c, "entry:")

    for id in items {
        g, ok := get_item(id).(GlobalVarDec)
        if !ok do continue

        objid := get_ctx().item_objects[id]
        gtid := get_ctx().objs[objid].type.(TypeId)
        ty_str := ty_to_llvm_str(c, gtid)
        name := get_ctx().cg_item_names[id]

        if is_memory_type(gtid) {
            // large aggregate global: build it straight into the global
            cg_expr_into(c, g.value, aprintf(c, "@%s", name))
            continue
        }

        value, returns := reduce_expr_to_single_value(c, cg_expr(c, g.value))
        if !returns {
            highlight_lines(get_span(g.value))
            gala_panic("Global initialiser must produce a value.")
        }
        cwritefln(c, "\tstore %s %s, ptr @%s", ty_str, value, name)
    }

    cwriteln(c, "\tret void")
    cwriteln(c, "}")
    cwriteln(c, "")

    c.cur_fn_ret = old_ret
}

// ============================================================================
// Declarations / modules
// ============================================================================

// Writes the type definitions and function prototypes this module needs,
// including everything reachable through its imports.
//
// A module can be reached along several import paths (main -> rl -> core
// and main -> core), and LLVM rejects redefining a type or redeclaring a
// function, so everything is emitted at most once per output file:
//   - declared_mods:    a module's items are walked only the first time we
//                       see it (this also makes import cycles terminate).
//   - declared_items:   per-item guard, keyed on ItemId. Imports share the
//                       same AST, so the same declaration always has the
//                       same ItemId however it was reached.
//   - emitted_externs:  extern C functions are keyed on their symbol name,
//                       since two different .gala files may each declare
//                       `extern fn printf` (different ItemIds, same symbol).
// Transitive imports need no `exports` flag: we follow every Import item,
// so `main -> rl -> core` still declares core's items even if `main` never
// imports core itself.
//
// Skipping an already-handled item is safe for name lookup too: the
// `ctx.scope.vars` binding is made the first time the item is seen, and
// there is one scope for the whole output file.
cg_items_dec :: proc(ctx: ^CGCtx, items: []ItemId, is_import := false) {
    for id in items {
        switch i in get_item(id) {
        case Forward: {
            cg_items_dec(ctx, []ItemId{i.i}, is_import)
        }
        case TypeAlias: {}
        case ConstDec: {} // ok
        case Import: {
            mid := get_ctx().item_module[id]
            cwritefln(ctx, "; decs from mod %s %d", i.fname, mid)
            if ctx.declared_mods[mid] {
                cwritefln(ctx, "; exst already")
                continue
            }
            ctx.declared_mods[mid] = true // mark before recursing so cycles terminate
            m := get_ctx().mods[mid]
            cg_items_dec(ctx, m.ast.items[:], true) // gen items into this

            // Post-order: this module's own imports were appended during the
            // recursion above, so they run before it.
            if items_have_globals(m.ast.items[:]) {
                append(&ctx.init_mods, mid)
                cwritefln(ctx, "declare void %s()", mod_init_name(ctx, mid))
            }
            cwritefln(ctx, "; end")
        }
        case StructDec: {
            if ctx.declared_items[id] do continue
            ctx.declared_items[id] = true
            t := get_ctx().item_types[id]
            ty := get_type(t).structure;

            ensure_type_declared(ctx, t)
        }
        case GlobalVarDec: {
            if ctx.declared_items[id] do continue
            ctx.declared_items[id] = true

            // Bind now (so later code can look the name up); the actual
            // `@x = global ...` line is written by cg_globals_dec once every
            // struct type has a name. A global is an address, so it's a
            // .Variable: loads/stores go through `ptr @name`.
            name := mod_item_obj_name(ctx, id)
            ctx.scope.vars[i.name] = {.Variable, aprintf(ctx, "@%s", name)}
            append(&ctx.pending_globals, PendingGlobal{id = id, external = is_import})
        }
        case FnDec: {
            if ctx.declared_items[id] do continue
            ctx.declared_items[id] = true

            // it's a function, so use "@main" instead of "%main"
            name := mod_item_obj_name(ctx, id)
            ctx.scope.vars[i.name] = {.Symbol, aprintf(ctx, "@%s", name)}
            if is_import {
                cg_fn_declaration(ctx, id)
            }
        }
        case ExternFnDec: {
            if ctx.declared_items[id] do continue
            ctx.declared_items[id] = true

            name := i.name // use normal name here since it's external
            get_ctx().cg_item_names[id] = name
            ctx.scope.vars[i.name] = {.Symbol, aprintf(ctx, "@%s", name)}
            if is_import && !ctx.emitted_externs[name] {
                ctx.emitted_externs[name] = true
                cg_fn_declaration(ctx, id, is_extern = true)
            }
        }
        }
    }
}

// Finds the top-level `main` function item in this module's AST (not one
// pulled in via an import — entry point must be declared directly in the
// entry file). Returns its ItemId so the caller can resolve its name
// through the normal item/obj lookup, since item naming is going to change
// soon and we don't want a separate hardcoded path for the entry symbol.
find_main_item :: proc(ast: ^AST) -> (ItemId, bool) {
    for id in ast.items {
        #partial switch i in get_item(id) {
        case FnDec:
            if i.name == "main" {
                return id, true
            }
        }
    }
    return {}, false
}

// Runs an external tool to completion, panicking (with the full command
// line) if it can't be started or exits non-zero.
run_tool :: proc(what: string, cmd: []string) {
    line := strings.join(cmd, " ", get_ctx().allocator)

    p, err := os.process_start({command = cmd})
    if err != nil {
        debugln(line)
        gala_panic("Failed to start", what, "process:", err)
    }

    state, werr := os.process_wait(p)
    if werr != nil {
        gala_panic("Failed to wait for", what, "process:", werr)
    }
    if state.exit_code != 0 {
        debugln(line)
        gala_panic("Failed to run", what, "- exit code:", state.exit_code)
    }
    debugln(what, "exit code:", state.exit_code)
}

// Writes the C `main` wrapper: runs global initialisers (imported modules
// first, post-order, then this module's own) and calls gala `main`.
// The wrapper has to match gala main's real return type — calling a `void`
// gala main as `i32` would leave the process exit status as whatever
// happened to be in eax. void -> exit 0; i32 -> pass it through.
cg_entry_wrapper :: proc(c: ^CGCtx, m: ModId, ast: ^AST, has_globals: bool) {
    main_id, found := find_main_item(ast)
    if !found {
        gala_panic("entry file has no `main` function (use -c to compile without an entry point)")
    }
    entry := aprintf(c, "@%s", get_ctx().cg_item_names[main_id])

    main_obj := get_ctx().objs[get_ctx().item_objects[main_id]]
    ret_kind := get_type(get_type(main_obj.type.(TypeId)).fn.ret_ty).kind

    call_line, ret_line: string
    #partial switch ret_kind {
    case .Void:
        call_line = aprintf(c, "call void %s()", entry)
        ret_line = "ret i32 0"
    case .Int32:
        call_line = aprintf(c, "%%result = call i32 %s()", entry)
        ret_line = "ret i32 %result"
    case:
        gala_panic("`main` must return void or i32")
    }

    cwritefln(c, "define i32 @main(i32 %%argc, ptr %%argv) {{")
    for mid in c.init_mods {
        cwritefln(c, "\tcall void %s()", mod_init_name(c, mid))
    }
    if has_globals {
        cwritefln(c, "\tcall void %s()", mod_init_name(c, m))
    }
    cwritefln(c, "\t%s", call_line)
    cwritefln(c, "\t%s", ret_line)
    cwritefln(c, "}")
}
// -c mode: no C `main`, so nothing would run the global initialisers.
// Export one function that does (unmangled, callable from a C shim).
cg_library_init :: proc(c: ^CGCtx, m: ModId, has_globals: bool) {
    cwritefln(c, "define void @gala_init_globals() {{")
    for mid in c.init_mods {
        cwritefln(c, "\tcall void %s()", mod_init_name(c, mid))
    }
    if has_globals {
        cwritefln(c, "\tcall void %s()", mod_init_name(c, m))
    }
    cwritefln(c, "\tret void")
    cwritefln(c, "}")
}
// mingw triple: we link with plain `ld`, not MSVC
cg_module :: proc(m: ModId) {
    module := get_ctx().mods[m]
    ast := module.ast
    is_entry := module.path == get_ctx().entry_file
    abi := abi_resolve_kind(.None) // the context's ABI

    cgctx := CGCtx{}
    arena: mem.Dynamic_Arena
    mem.dynamic_arena_init(&arena)
    defer mem.dynamic_arena_free_all(&arena)
    cgctx.arena = &arena
    sb: strings.Builder
    strings.builder_init(&sb)
    defer strings.builder_destroy(&sb)
    cgctx.b = &sb

    alloc := get_ctx().allocator
    cgctx.cg_strings      = make(map[string]StringGlobalResult, allocator=alloc)
    cgctx.break_labels    = make(map[StmtId]string, allocator=alloc)
    cgctx.continue_labels = make(map[StmtId]string, allocator=alloc)
    cgctx.lambdas         = make([dynamic]string, allocator=alloc)
    cgctx.declared_items  = make(map[ItemId]bool, allocator=alloc)
    cgctx.declared_mods   = make(map[ModId]bool, allocator=alloc)
    cgctx.emitted_externs = make(map[string]bool, allocator=alloc)
    cgctx.pending_globals = make([dynamic]PendingGlobal, allocator=alloc)
    cgctx.init_mods       = make([dynamic]ModId, allocator=alloc)
    cgctx.scope = new_gcscope(nil)

    cgctx.declared_types = make(map[TypeId]bool, allocator=alloc)
    strings.builder_init(&cgctx.extra_types)
    defer strings.builder_destroy(&cgctx.extra_types)


    // boilerplate + garbage
    cwritefln(&cgctx, "; file \"%s\"", module.path)
    cwritefln(&cgctx, "; target info")
    cwritefln(&cgctx, "target datalayout = \"%s\"", abi_datalayout(abi))
    cwritefln(&cgctx, "target triple = \"%s\"", abi_triple(abi))

    types_at := strings.builder_len(sb)

    // memory intrinsics used by cg_memcpy / cg_memmove / cg_memset.
    // Declared unconditionally: an unused `declare` costs nothing, and one
    // can't be emitted lazily from inside a function body.
    cwritefln(&cgctx, "declare void @llvm.memcpy.p0.p0.i64(ptr, ptr, i64, i1)")
    cwritefln(&cgctx, "declare void @llvm.memmove.p0.p0.i64(ptr, ptr, i64, i1)")
    cwritefln(&cgctx, "declare void @llvm.memset.p0.i64(ptr, i8, i64, i1)")

    // struct types (and prototypes) first: everything below refers to them
    cg_items_dec(&cgctx, ast.items[:])

    // globals go after all type definitions (see cg_globals_dec)
    cg_globals_dec(&cgctx)

    for s in get_ctx().data {
        t := new_tmp(&cgctx, p="string", symbol=true)
        v := emit_string_global(t, s)
        cwritefln(&cgctx, "%s", v.ir)
        cgctx.cg_strings[s] = v
    }

    // gen
    cg_ast(&cgctx, ast)

    // this module's global initialisers (needs the scope bindings made by
    // cg_items_dec, so it must come before anything resets cgctx.scope)
    has_globals := items_have_globals(ast.items[:])
    if has_globals {
        cg_globals_init(&cgctx, m, ast.items[:])
    }

    // function literals found while generating the items above were
    // generated into their own builders (see cg_fn_lit); emit them now.
    for lambda in cgctx.lambdas {
        cwritefln(&cgctx, "%s", lambda)
    }

    if is_entry {
        if get_ctx().object_only {
            cg_library_init(&cgctx, m, has_globals)
        } else {
            cg_entry_wrapper(&cgctx, m, ast, has_globals)
        }
    }
    // runtime type table: defined once (entry module), declared elsewhere
    if get_ctx().uses_type_table {
        if is_entry {
            cg_type_table(&cgctx)
        } else {
            cwritefln(&cgctx, "@__gala_types = external constant [0 x %s]",
                ty_to_llvm_str(&cgctx, ty_from_name("TypeInfo")))
        }
    }

    // ---- write + compile ----
    if dir_err := os.make_directory(".gala_build"); dir_err != io.Error.None && dir_err != .Exist {
        gala_panic("Failed make .gala_build directory:", dir_err)
    }


    name := mod_prefix_from_path(get_ctx().current_file, "gala_")

    ll_name  := aprintf(&cgctx, ".gala_build/%s.ll", name)
    opt_name := aprintf(&cgctx, ".gala_build/%s.opt.ll", name)
    obj_ext := ".obj" if abi == .Win64 else ".o"

    o_name := aprintf(&cgctx, ".gala_build/%s%s", name, obj_ext)

    
    // write relevant types from importing files first
    ir := strings.to_string(sb)
    full := strings.concatenate({ir[:types_at], strings.to_string(cgctx.extra_types), ir[types_at:]},
        get_ctx().allocator)
    if e := os.write_entire_file_from_string(ll_name, full); e != io.Error.None {
        gala_panic("Failed to write to file:", e)
    }


    // mem2reg: promotes the entry-block allocas to SSA registers
    // run_tool("opt", {"opt", "-passes=mem2reg", ll_name, "-S", "-o", opt_name})
    // append(&compile_jobs, CompileJob{ll_name, opt_name, o_name})

    // PIC is needed for shared objects (-c); harmless for executables too
    // run_tool("llc", {"llc", "-filetype=obj", "-O2", "-relocation-model=pic", opt_name, "-o", o_name})
    // append(&compile_jobs, CompileJob{ll_name, opt_name, o_name})

    // debugln("ADDING:", o_name)
    // append(&get_ctx().o_files, o_name)
    // queue for compile_all (one llvm-link + one llc for the whole program)
    append(&compile_jobs, CompileJob{ll_name, opt_name, o_name})

    append(&get_ctx().o_files, o_name)
}

CompileJob :: struct { ll, opt, obj: string }
compile_jobs: [dynamic]CompileJob


// Starts every command (at most `max_jobs` at a time), then waits for them.
run_parallel :: proc(what: string, cmds: [][]string, max_jobs := 8) {
    for start := 0; start < len(cmds); start += max_jobs {
        end := min(start + max_jobs, len(cmds))
        procs := make([dynamic]os.Process, allocator = get_ctx().allocator)

        for cmd in cmds[start:end] {
            p, err := os.process_start({command = cmd})
            if err != nil {
                gala_panic("Failed to start", what, "process:", err)
            }
            debugln("process:", cmd, p.pid);
            append(&procs, p)
        }
        for p, k in procs {
            state, werr := os.process_wait(p)
            if werr != nil {
                gala_panic("Failed to wait for", what, "process:", werr)
            }
            if state.exit_code != 0 {
                gala_panic("Failed to run", what, "- exit code:", state.exit_code,
                    "(", strings.join(cmds[start + k], " ", get_ctx().allocator), ")")
            }
            debugln("process", p.pid, "finished");
        }
    }
}

// With llvm_out set, stops after producing one merged IR file at that path
// (textual .ll if llvm_text, else bitcode); no llc, no linking.
compile_all :: proc(llvm_out := "", llvm_text := false) {
    release := get_ctx().mode == .RELEASE
    abi := abi_resolve_kind(.None)
    obj_ext := ".obj" if abi == .Win64 else ".o"
    emit_ir := llvm_out != ""

    ir_ext := ".ll" if llvm_text else ".bc"
    // dev + emit: llvm-link writes the final file directly
    linked := llvm_out if emit_ir && !release else aprintf_global(".gala_build/all%s", ir_ext)
    final_obj := aprintf_global(".gala_build/all%s", obj_ext)

    // 1. merge every module into one IR file
    cmd := make([dynamic]string, allocator = get_ctx().allocator)
    append(&cmd, "llvm-link")
    if emit_ir && llvm_text do append(&cmd, "-S")
    for j in compile_jobs do append(&cmd, j.ll)
    append(&cmd, "-o", linked)
    run_tool("llvm-link", cmd[:])
    if emit_ir && !release do return

    // 2. release: promote allocas (and whatever else you add later)
    src := linked
    if release {
        src = llvm_out if emit_ir else ".gala_build/all.opt.bc"
        ocmd := make([dynamic]string, allocator = get_ctx().allocator)
        append(&ocmd, "opt", "-passes=mem2reg")
        if emit_ir && llvm_text do append(&ocmd, "-S")
        append(&ocmd, linked, "-o", src)
        run_tool("opt", ocmd[:])
        if emit_ir do return
    }

    // 3. one llc for the whole program
    level := "-O2" if release else "-O0"
    run_tool("llc", {"llc", "-filetype=obj", level, "-relocation-model=pic", src, "-o", final_obj})

    // the linker now gets a single object
    clear(&get_ctx().o_files)
    append(&get_ctx().o_files, final_obj)
}

aprintf_global :: proc(format: string, args: ..any) -> string {
    return fmt.aprintf(format, ..args, allocator = get_ctx().allocator)
}

// ============================================================================
// Names
// ============================================================================

// eg src/some/folder_file -> src_some_folder_file
files_prefixes: map[string]string
path_to_file_prefix :: proc(c: ^CGCtx, path: string) -> string {
    if v, ok := files_prefixes[path]; ok {
        return v
    }

    assert(strings.has_suffix(path, ".gala"))

    base := path[:len(path)-len(".gala")]

    buf := make([]byte, len(base), allocator=c.arena.block_allocator)
    for ch, i in base {
        buf[i] = '_' if ch == '/' else byte(ch)
    }

    out := string(buf)
    files_prefixes[path] = out
    return out
}

_mod_item_type_name :: proc(c: ^CGCtx, id: ItemId) -> string {
    tid := get_ctx().item_types[id]
    name, ok := get_ctx().cg_ty_names[tid]
    if !ok {
        mid, mid_ok := get_ctx().ty_modules[tid]
        if !mid_ok {
            debugln("MODULE:", id)
            gala_panic("type has no module associated with it.")
        }
        name = aprintf(c, "%s.%s", module_prefix(mid), get(tid).name)
        get_ctx().cg_ty_names[tid] = name
        get_ctx().llvm_ty[tid] = aprintf(c, "%%%s", name) // set llvm ty as well
    }
    return name
}

mod_item_obj_name :: proc(c: ^CGCtx, id: ItemId) -> string {
    oid := get_ctx().item_objects[id]
    name, ok := get_ctx().cg_item_names[id]
    if !ok {
        mid, mid_ok := get_ctx().obj_modules[oid]
        if !mid_ok {
            debugln("OBJ MODULE:", oid, id)
            gala_panic("item has no module associated with it.")
        }
        if is_exported_fn(id, oid, mid) {
            // keep the plain symbol so C (the shim) can link against it
            name = get(oid).name
        } else {
            name = aprintf(c, "%s.%s", module_prefix(mid), get(oid).name)
        }
        get_ctx().cg_item_names[id] = name
    }
    return name
}
// -c mode only: `gala_*` functions of the entry module keep their plain
// name. Stopgap until there is a real `export fn`.
is_exported_fn :: proc(id: ItemId, oid: ObjId, mid: ModId) -> bool {
    // exports everywhere
    // if !get_ctx().object_only do return false
    // if get_ctx().mods[mid].path != get_ctx().entry_file do return false
    t := get_type(get_obj(oid).type.(TypeId))
    if t.kind == .Function && t.fn.is_export { return true }
    return false
}

hash_string :: proc(s: string) -> u64 {
    hash: u64 = 14695981039346656037
    for c in s {
        hash = (hash ~ u64(c)) * 1099511628211
    }
    return hash
}
mod_prefix_from_path :: proc(path: string, prefix: string = "") -> string {
    hash := fmt.aprintf("{}", hash_string(path));
    if len(hash) > 6 do hash = hash[:6];
    return fmt.aprintf("{}{}.{}", prefix, hash, mod_prefix_from_path_old(path));
}
// "abc/efg/abc.txt" -> "myprefix_abc_efg_abc"
mod_prefix_from_path_old :: proc(path: string, prefix: string = "") -> string {
    dir_all := filepath.dir(path) // "abc/efg"

    dir, err := filepath.clean(dir_all)
    assert(err == .None)

    normalized, was_allocated := strings.replace_all(dir, "\\", "/")
    defer if was_allocated {
        delete(normalized)
    }

    parts := strings.split(normalized, "/")
    defer delete(parts)

    sb := strings.builder_make()
    strings.write_string(&sb, prefix)

    for part in parts {
        if part == "" || part == "." {
            continue
        }
        strings.write_string(&sb, "_")
        strings.write_string(&sb, part)
    }

    // Append the filename (without extension).
    base := filepath.base(path)        // "abc.txt"
    ext := filepath.ext(base)          // ".txt"
    stem := base[:len(base)-len(ext)]  // "abc"

    if stem != "" {
        strings.write_string(&sb, "_")
        strings.write_string(&sb, stem)
    }

    return strings.to_string(sb)
}

