// constants.odin
package main

import "core:fmt"
import "core:math"
import "core:strconv"
import "core:strings"
import "core:reflect"

// A folded compile-time value. Integers are i128 so overflow can be
// detected; they must stay inside [i64 min, u64 max].
ConstValue :: union { i128, f64, bool, string }

ConstState :: enum { Unvisited, InProgress, Done }

ConstEntry :: struct {
    item:  ItemId,
    scope: ^ModuleScope, // the module the constant was declared in
    state: ConstState,
    value: ConstValue,
    type:  Maybe(TypeId), // nil = untyped (`a :: 5`), set for `a : T : 5`
}

// every `::` constant, keyed by its object
const_table: map[ObjId]ConstEntry

// names of the constants being evaluated right now, for cycle errors
const_stack: [dynamic]string

CONST_INT_MIN :: -9223372036854775808
CONST_INT_MAX :: 18446744073709551615

const_errorf :: proc(id: ExprId, format: string, args: ..any) -> ! {
    highlight_lines(get_span(id))
    gala_panicf(format, ..args)
}

is_const_obj :: proc(oid: ObjId) -> bool {
    _, ok := const_table[oid]
    return ok
}

const_kind_name :: proc(v: ConstValue) -> string {
    #partial switch _ in v {
    case i128:   return "integer"
    case f64:    return "float"
    case bool:   return "boolean"
    case string: return "string"
    }
    return "unknown"
}

// Evaluates the constant if that hasn't happened yet. This is what makes
// top-level constants order independent (`A :: B + 1` above `B :: 2`) and
// lets a struct field's `[MAX]byte` use a constant before the constants pass.
ensure_const :: proc(oid: ObjId) {
    entry := const_table[oid]
    if entry.state == .Done do return
    resolve_const_dec_item(entry.scope, entry.item)
}

// `name :: expr;` or `name : T : expr;`
// Folds the value, then moves the name from the forward map into the scope
// and exports it, like the other item kinds.
resolve_const_dec_item :: proc(s: ^ModuleScope, id: ItemId) {
    if _, done := get_ctx().item_objects[id]; done do return

    cd, ok := get(id).(ConstDec); assert(ok)
    oid, ook := s.obj_forward[cd.name]; assert(ook)

    e := &const_table[oid]
    if e.state == .InProgress {
        // show the loop: the stack from the first time this name appears
        start := 0
        for n, i in const_stack {
            if n == cd.name {
                start = i
                break
            }
        }
        chain := strings.join(const_stack[start:], " -> ", context.temp_allocator)
        const_errorf(cd.value, "Constant \"%s\" is defined in terms of itself: %s -> %s.",
            cd.name, chain, cd.name)
    }
    e.state = .InProgress
    append(&const_stack, cd.name)

    val := const_eval(s, cd.value)

    typ: Maybe(TypeId) = nil
    if ts, has := cd.type.(TypeSpecifier); has {
        if specifier_has_generic(ts) {
            const_errorf(cd.value, "Constant \"%s\" can't have a generic type.", cd.name)
        }
        tid := resolve_type_specifier(s, ts)
        val = const_convert(cd.value, val, tid)
        typ = tid
    }

    pop(&const_stack)

    e = &const_table[oid] // re-fetch, evaluation may have touched the table
    e.value = val
    e.type = typ
    e.state = .Done

    delete_key(&s.obj_forward, cd.name) // delete fd and create object
    s.objects[cd.name] = oid
    s.obj_exports[cd.name] = oid
    get_ctx().item_objects[id] = oid
}

// ---- evaluation ----

// `typed_ok`: allow typed constants (`N : i32 : 5`) as operands. Off inside
// other constants (their type would be lost), on for array sizes where only
// the number matters.
const_eval :: proc(s: ^Scope, id: ExprId, typed_ok := false) -> ConstValue {
    #partial switch e in get_expr(id) {
    case Number:       return const_number(id, e.text)
    case String:       return e.s
    case BoolLitTrue:  return true
    case BoolLitFalse: return false
    case UnNegative: {
        v := const_eval(s, e.expr, typed_ok)
        #partial switch x in v {
        case i128: return const_check_range(id, -x)
        case f64:  return -x
        }
        const_errorf(id, "Can't negate a %s constant.", const_kind_name(v))
    }
    case UnNot: {
        v := const_eval(s, e.expr, typed_ok)
        if b, ok := v.(bool); ok do return !b
        const_errorf(id, "`!` needs a boolean constant, got a %s.", const_kind_name(v))
    }
    case Binop: {
        l := const_eval(s, e.left, typed_ok)
        r := const_eval(s, e.right, typed_ok)
        return const_binop(id, e.kind, l, r)
    }
    case Symbol: {
        oid, ok := scope_get_object(s, e.name)
        if !ok {
            const_errorf(id, "Couldn't find \"%s\" in scope.", e.name)
        }
        if !is_const_obj(oid) {
            const_errorf(id, "\"%s\" is not a constant.", e.name)
        }
        ensure_const(oid)
        entry := const_table[oid]
        if entry.type != nil && !typed_ok {
            const_errorf(id, "Typed constant \"%s\" can't be used inside another constant expression yet (make it untyped).", e.name)
        }
        return entry.value
    }
    }
    const_errorf(id, "Not a constant expression (only literals, other constants and operators are allowed).")
}

// The size in `[N]T`: any integer constant expression that isn't negative.
const_array_size :: proc(s: ^Scope, id: ExprId) -> int {
    val := const_eval(s, id, typed_ok = true)
    v, is_int := val.(i128)
    if !is_int {
        const_errorf(id, "Array size must be an integer constant, got a %s.", const_kind_name(val))
    }
    if v < 0 {
        const_errorf(id, "Array size can't be negative (got %d).", v)
    }
    if v > i128(max(int)) {
        const_errorf(id, "Array size %d is too large.", v)
    }
    return int(v)
}

const_number :: proc(id: ExprId, text: string) -> ConstValue {
    if is_float_literal(text) {
        v, ok := strconv.parse_f64(text)
        if !ok do const_errorf(id, "Invalid float literal \"%s\".", text)
        return v
    }
    v, ok := parse_integer_literal(text)
    if !ok do const_errorf(id, "Invalid integer literal \"%s\".", text)
    return const_check_range(id, i128(v))
}

const_check_range :: proc(id: ExprId, v: i128) -> i128 {
    if v < CONST_INT_MIN || v > CONST_INT_MAX {
        const_errorf(id, "Integer constant is out of range (it must fit between i64 min and u64 max).")
    }
    return v
}

const_check_finite :: proc(id: ExprId, v: f64) -> f64 {
    if math.is_nan(v) || math.is_inf(v) {
        const_errorf(id, "Float constant overflowed.")
    }
    return v
}

const_to_f64 :: proc(v: ConstValue) -> (f64, bool) {
    #partial switch x in v {
    case i128: return f64(x), true
    case f64:  return x, true
    }
    return 0, false
}

const_binop :: proc(id: ExprId, op: BinopKind, l, r: ConstValue) -> ConstValue {
    #partial switch lv in l {
    case bool: {
        rv, ok := r.(bool)
        if !ok do const_kind_mismatch(id, op, l, r)
        #partial switch op {
        case .LogicalAnd: return lv && rv
        case .LogicalOr:  return lv || rv
        case .Equal:      return lv == rv
        case .NotEqual:   return lv != rv
        }
        const_errorf(id, "Operator %v can't be applied to boolean constants.", op)
    }
    case string: {
        rv, ok := r.(string)
        if !ok do const_kind_mismatch(id, op, l, r)
        #partial switch op {
        case .Equal:    return lv == rv
        case .NotEqual: return lv != rv
        }
        const_errorf(id, "Operator %v can't be applied to string constants.", op)
    }
    }

    li, l_int := l.(i128)
    ri, r_int := r.(i128)
    if l_int && r_int do return const_int_binop(id, op, li, ri)

    lf, lok := const_to_f64(l)
    rf, rok := const_to_f64(r)
    if !lok || !rok do const_kind_mismatch(id, op, l, r)
    return const_float_binop(id, op, lf, rf)
}

const_kind_mismatch :: proc(id: ExprId, op: BinopKind, l, r: ConstValue) -> ! {
    const_errorf(id, "Can't apply operator %v to a %s and a %s constant.",
        op, const_kind_name(l), const_kind_name(r))
}

const_int_binop :: proc(id: ExprId, op: BinopKind, a, b: i128) -> ConstValue {
    #partial switch op {
    case .Addition:     return const_check_range(id, a + b)
    case .Subtraction:  return const_check_range(id, a - b)
    case .Multiply:     return const_check_range(id, a * b)
    case .Divide: {
        if b == 0 do const_errorf(id, "Constant division by zero.")
        return const_check_range(id, a / b)
    }
    case .Modulo: {
        if b == 0 do const_errorf(id, "Constant modulo by zero.")
        return a % b
    }
    case .Equal:        return a == b
    case .NotEqual:     return a != b
    case .Less:         return a < b
    case .LessEqual:    return a <= b
    case .Greater:      return a > b
    case .GreaterEqual: return a >= b
    case .BitAnd:       return a & b
    case .BitOr:        return a | b
    case .BitXor:       return a ~ b
    case .ShiftLeft: {
        if b < 0 || b > 63 do const_errorf(id, "Shift amount must be between 0 and 63, got %d.", b)
        return const_check_range(id, a << uint(b))
    }
    case .ShiftRight: {
        if b < 0 || b > 63 do const_errorf(id, "Shift amount must be between 0 and 63, got %d.", b)
        return a >> uint(b)
    }
    }
    const_errorf(id, "Operator %v can't be applied to integer constants.", op)
}

const_float_binop :: proc(id: ExprId, op: BinopKind, a, b: f64) -> ConstValue {
    #partial switch op {
    case .Addition:     return const_check_finite(id, a + b)
    case .Subtraction:  return const_check_finite(id, a - b)
    case .Multiply:     return const_check_finite(id, a * b)
    case .Divide: {
        if b == 0 do const_errorf(id, "Constant division by zero.")
        return const_check_finite(id, a / b)
    }
    case .Equal:        return a == b
    case .NotEqual:     return a != b
    case .Less:         return a < b
    case .LessEqual:    return a <= b
    case .Greater:      return a > b
    case .GreaterEqual: return a >= b
    }
    const_errorf(id, "Operator %v can't be applied to float constants.", op)
}

// `a : T : value`: the value must be usable as T. (No range check for
// sized integers yet.)
const_convert :: proc(id: ExprId, val: ConstValue, tid: TypeId) -> ConstValue {
    #partial switch v in val {
    case i128: {
        if is_integer(tid) || is_byte_like(tid) do return v
        if is_float(tid) do return f64(v)
    }
    case f64: {
        if is_float(tid) do return v
    }
    case bool: {
        if get_type(tid).kind == .Bool do return v
    }
    case string: {
        if get_type(tid).kind == .String do return v
    }
    }
    const_errorf(id, "A %s constant doesn't fit type %s.", const_kind_name(val), tts(tid))
}

// ---- use sites ----

// A float as literal text the lexer-side code understands (always has a '.',
// never an exponent). Fixed notation with enough decimals to keep ~17
// significant digits, trailing zeros trimmed: 2.5 -> "2.5".
const_float_text :: proc(v: f64) -> string {
    if v == 0 do return "0.0"

    mag := int(math.floor(math.log10(abs(v))))
    decimals := 17
    if mag < 0 do decimals = 17 - mag
    decimals = min(decimals, 400)

    format := fmt.tprintf("%%.%df", decimals) // e.g. "%.17f"
    s := fmt.aprintf(format, v, allocator = get_ctx().allocator)

    end := len(s)
    for end > 2 && s[end-1] == '0' && s[end-2] != '.' do end -= 1
    return s[:end]
}

const_new_literal :: proc(val: ConstValue, sp: SpanStruct) -> ExprId {
    mk :: proc(e: Expr, sp: SpanStruct) -> ExprId {
        nid := new_expr(e)
        get_ctx().spans.exprs[nid] = sp
        return nid
    }
    alloc := get_ctx().allocator

    #partial switch v in val {
    case i128: {
        if v < 0 {
            inner := mk(Number{text = fmt.aprintf("%d", -v, allocator = alloc)}, sp)
            return mk(UnNegative{expr = inner}, sp)
        }
        return mk(Number{text = fmt.aprintf("%d", v, allocator = alloc)}, sp)
    }
    case f64: {
        if v < 0 {
            inner := mk(Number{text = const_float_text(-v)}, sp)
            return mk(UnNegative{expr = inner}, sp)
        }
        return mk(Number{text = const_float_text(v)}, sp)
    }
    case bool: {
        if v do return mk(BoolLitTrue{}, sp)
        return mk(BoolLitFalse{}, sp)
    }
    case string: return mk(String{s = v}, sp)
    }
    panic("unreachable")
}

const_is_number :: proc(v: ConstValue) -> bool {
    #partial switch _ in v {
    case i128, f64: return true
    }
    return false
}

// Rewrites the Symbol `id` in place into the constant's value. Untyped
// constants become plain literals (so they adapt to whatever they're used
// as); typed numeric ones become `cast(T) literal`.
inline_constant :: proc(id: ExprId, oid: ObjId) {
    ensure_const(oid)
    entry := const_table[oid]
    sp := get_span(id)
    lit := const_new_literal(entry.value, sp)

    if tid, typed := entry.type.(TypeId); typed && const_is_number(entry.value) {
        get_expr(id)^ = Cast{
            to     = TypeSpecifier(BaseType{ident = "", span = sp.span}), // placeholder
            target = lit,
        }
        get_ctx().expr_resolution_types[id] = tid
        return
    }
    get_expr(id)^ = get_expr(lit)^
}

// constants.odin (or wherever base types are set up)

inject_kind_constants :: proc(base: ^ModuleScope) {
    for k in RtKind {
        name := strings.concatenate(
            {"KIND_", strings.to_upper(reflect.enum_string(k), context.temp_allocator)},
            get_ctx().allocator)

        oid := new_object(base, Object{kind=.Variable, name=name, read_only=.Constant}, {})
        const_table[oid] = ConstEntry{
            state = .Done,
            value = i128(k),
            type  = nil,       // untyped, adapts to its use like `a :: 5`
        }
        base.objects[name]     = oid
        base.obj_exports[name] = oid
    }
}
