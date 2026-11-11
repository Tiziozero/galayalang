package main

import "core:fmt"
import "core:strings"

Parser :: struct {
    file:   string,
    tokens: []Token,
    i:      int,

    ignore_struct_lit: bool,

    // Hidden top-level items created while parsing the current top-level
    // item (see make_default_thunk). They go ahead of the item that caused them.
    extra_items: [dynamic]ItemId,
}

// ===========================================================================
// Helpers
// ===========================================================================

MIXED_FIELDS_MSG   :: "Can't mix named and positional fields in a struct literal."
GROUPED_DEFAULT_MSG :: "Default values can't be used with grouped parameters (`a, b: type`)."

maybe_semi :: proc(p: ^Parser) -> Maybe(Token) {
    if is_symbol(current_token(p), ";") {
        return consume_token(p)
    }
    return nil
}

cur_file :: proc() -> string {
    return get_ctx().current_file
}

span_of :: proc(e: ExprId) -> Span {
    return get_span(e).span
}

span_between :: proc(a, b: Span) -> Span {
    return Span{start=a.start, end=b.end}
}

// Highlight `span` and abort.
fail_at :: proc(span: Span, msg: string) -> ! {
    highlight_lines(cur_file(), span)
    gala_panic(msg)
}

new_list :: proc($T: typeid) -> [dynamic]T {
    return make([dynamic]T, allocator=get_ctx().allocator)
}

set_expr_span :: proc(id: ExprId, span: Span) {
    get_ctx().spans.exprs[id] = {file_name=cur_file(), span=span}
}

add_expr :: proc(e: Expr, span: Span) -> ExprId {
    id := new_expr(e)
    set_expr_span(id, span)
    return id
}

add_stmt :: proc(s: Stmt, span: Span) -> StmtId {
    id := new_stmt(s)
    get_ctx().spans.stmts[id] = {file_name=cur_file(), span=span}
    return id
}

add_item :: proc(i: Item, span: Span) -> ItemId {
    id := new_item(i)
    get_ctx().spans.items[id] = {file_name=cur_file(), span=span}
    return id
}

// ===========================================================================
// Token access
// ===========================================================================

// n tokens ahead of the current one (0 = current)
peek_token :: proc(p: ^Parser, n: int) -> Token {
    i := p.i + n
    if i >= 0 && i < len(p.tokens) {
        return p.tokens[i]
    }
    return Token{kind=.EOF}
}

current_token :: proc(p: ^Parser) -> Token {
    return peek_token(p, 0)
}

next_token :: proc(p: ^Parser) -> Token {
    return peek_token(p, 1)
}

consume_token :: proc(p: ^Parser) -> Token {
    t := current_token(p)
    if p.i < len(p.tokens) do p.i += 1
    return t
}

// span of the most recently consumed token
prev_token_span :: proc(p: ^Parser) -> Span {
    if p.i > 0 && p.i - 1 < len(p.tokens) {
        return p.tokens[p.i - 1].span
    }
    return Span{}
}

is_symbol :: proc(t: Token, s: string) -> bool {
    return t.kind == .Symbol && t.text == s
}

is_kw :: proc(t: Token, k: Keyword) -> bool {
    return t.kind == .Keyword && t.kw == k
}

is_current_kw :: proc(p: ^Parser, k: Keyword) -> bool {
    return is_kw(current_token(p), k)
}

// Consumes the symbol if it is next.
accept_symbol :: proc(p: ^Parser, s: string) -> bool {
    if is_symbol(current_token(p), s) {
        consume_token(p)
        return true
    }
    return false
}

expect_symbol :: proc(p: ^Parser, str: string) -> Token {
    c := current_token(p)
    if c.kind != .Symbol {
        highlight_lines(cur_file(), c.span)
        gala_panicf("Expected symbol \"{}\", got: {}", str, c)
    }
    if c.text != str {
        highlight_lines(cur_file(), c.span)
        gala_panic("Expected", str, "got:", c.text)
    }
    return consume_token(p)
}

expect_ident :: proc(p: ^Parser) -> Token {
    c := current_token(p)
    if c.kind != .Ident {
        highlight_lines(cur_file(), c.span)
        gala_panic("Expected ident got:", c)
    }
    return consume_token(p)
}

// "=>" may be lexed as one symbol, or as "=" followed by ">"; accept both.
is_fat_arrow :: proc(p: ^Parser) -> bool {
    if is_symbol(current_token(p), "=>") do return true
    return is_symbol(current_token(p), "=") && is_symbol(next_token(p), ">")
}

consume_fat_arrow :: proc(p: ^Parser) {
    if is_symbol(current_token(p), "=>") {
        consume_token(p)
    } else {
        consume_token(p) // "="
        consume_token(p) // ">"
    }
}

// ===========================================================================
// Operators
// ===========================================================================

op_kind :: proc(t: Token) -> (kind: BinopKind, ok: bool) {
    if t.kind != .Symbol do return {}, false
    switch t.text {
    case "+":  return .Addition,     true
    case "-":  return .Subtraction,  true
    case "*":  return .Multiply,     true
    case "/":  return .Divide,       true
    case "%":  return .Modulo,       true
    case "==": return .Equal,        true
    case "!=": return .NotEqual,     true
    case "<=": return .LessEqual,    true
    case ">=": return .GreaterEqual, true
    case "<":  return .Less,         true
    case ">":  return .Greater,      true
    case "&&": return .LogicalAnd,   true
    case "||": return .LogicalOr,    true
    case "&":  return .BitAnd,       true
    case "~":  return .BitXor,       true
    case "|":  return .BitOr,        true
    case "<<": return .ShiftLeft,    true
    case ">>": return .ShiftRight,   true
    }
    return {}, false
}

compound_op_kind :: proc(t: Token) -> (kind: BinopKind, ok: bool) {
    if t.kind != .Symbol do return {}, false
    switch t.text {
    case "+=":  return .Addition,    true
    case "-=":  return .Subtraction, true
    case "*=":  return .Multiply,    true
    case "/=":  return .Divide,      true
    case "%=":  return .Modulo,      true
    case "&=":  return .BitAnd,      true
    case "|=":  return .BitOr,       true
    case "~=":  return .BitXor,      true
    case "<<=": return .ShiftLeft,   true
    case ">>=": return .ShiftRight,  true
    }
    return {}, false
}

// Go/Odin-style: bitwise ops bind tighter than comparisons.
op_precedence :: proc(t: Token) -> int {
    if t.kind != .Symbol do return -1
    switch t.text {
    case "||": return 1
    case "&&": return 2
    case "==", "!=", "<=", ">=", "<", ">": return 3
    case "+", "-", "|", "~": return 4
    case "*", "/", "%", "&", "<<", ">>": return 5
    }
    return -1
}

op_is_right_assoc :: proc(t: Token) -> bool {
    return false // extend for ** etc.
}

// ===========================================================================
// Expressions
// ===========================================================================

parse_expr_with_aggregate_lits :: proc(p: ^Parser, ignore: bool) -> ExprId {
    prev := p.ignore_struct_lit
    p.ignore_struct_lit = ignore
    defer p.ignore_struct_lit = prev
    return parse_expr(p)
}

// Header of an `if`/`while`/`for`: `Name {` is the block, not a struct lit.
parse_condition :: proc(p: ^Parser) -> ExprId {
    return parse_expr_with_aggregate_lits(p, true)
}

// Inside (), [] and {} a struct literal is unambiguous, even in a header.
parse_nested_expr :: proc(p: ^Parser) -> ExprId {
    return parse_expr_with_aggregate_lits(p, false)
}

// Struct literals, array literals and strings are only allowed here, at the
// start of an expression (so `a == Foo{}` and `"abc"[0]` don't parse).
parse_expr :: proc(p: ^Parser) -> ExprId {
    cur := current_token(p)
    aggregate_lit_ok := !p.ignore_struct_lit

    switch {
    case aggregate_lit_ok && (
            is_symbol(cur, "{") || // type comes from context
            (cur.kind == .Ident && is_symbol(next_token(p), "{")) ||
            is_generic_lit_ahead(p)):
        return parse_aggregate_lit(p)
    case is_symbol(cur, "["):
        return parse_array_lit(p)
    case is_symbol(cur, "{"):
        fail_at(cur.span, "`{}` zero-initialiser isn't implemented yet.")
    case cur.kind == .String:
        s := consume_token(p)
        return add_expr(String{s=s.text}, s.span)
    }
    return parse_binop(p, 0)
}

// `Name{x=1, y=2}`, `Name{1, 2}` (not both in one literal), `Name(T){..}`, `{..}`
parse_aggregate_lit :: proc(p: ^Parser) -> ExprId {
    name:  string
    ty:    TypeSpecifier
    start: int

    if is_symbol(current_token(p), "{") {
        // untyped: ty stays nil, the type checker decides
        start = current_token(p).span.start
    } else if is_generic_lit_ahead(p) {
        ty = parse_type(p) // GenericInstSpecifier
        inst := ty.(GenericInstSpecifier)
        name = inst.name
        start = inst.span.start
    } else {
        tok := expect_ident(p)
        name = tok.text
        ty = BaseType{ident=tok.text, span=tok.span}
        start = tok.span.start
    }
    consume_token(p) // "{"

    fields := make(map[string]FieldInit, allocator=get_ctx().allocator)
    positional := new_list(FieldInit)

    for !is_symbol(current_token(p), "}") {
        cur := current_token(p)
        if cur.kind == .Ident && is_symbol(next_token(p), "=") {
            if len(positional) > 0 do fail_at(cur.span, MIXED_FIELDS_MSG)
            fname := expect_ident(p)
            consume_token(p) // "="
            expr := parse_nested_expr(p)
            if fname.text in fields do fail_at(fname.span, "duplicate fields.")
            fields[fname.text] = {expr, fname.span}
        } else {
            if len(fields) > 0 do fail_at(cur.span, MIXED_FIELDS_MSG)
            expr := parse_nested_expr(p)
            append(&positional, FieldInit{expr, span_of(expr)})
        }
        if !accept_symbol(p, ",") do break
    }
    end := expect_symbol(p, "}")

    return add_expr(
        AggregateLit{name=name, ty=ty, fields=fields, positional=positional[:]},
        Span{start=start, end=end.span.end},
    )
}

// `[N]T{}` (zeroed) or `[N]T{a, b, c}` (trailing comma ok). N is a number
// or a constant expression.
parse_array_lit :: proc(p: ^Parser) -> ExprId {
    open_b := consume_token(p) // "["
    size, size_expr := parse_array_size(p)
    expect_symbol(p, "]")
    t := parse_type(p)
    expect_symbol(p, "{")

    elems := new_list(ExprId)
    for !is_symbol(current_token(p), "}") {
        append(&elems, parse_nested_expr(p))
        if !accept_symbol(p, ",") do break
    }
    end := expect_symbol(p, "}")
    span := span_between(open_b.span, end.span)

    // with a constant-expression size the resolver does this check
    if size_expr == nil && len(elems) > size {
        fail_at(span, "too many elements in array initialiser")
    }

    init: []ExprId // nil for `{}`
    if len(elems) > 0 do init = elems[:]

    return add_expr(FixedSizeArray{size=size, size_expr=size_expr, ty=t, initialiser=init}, span)
}

// The size inside `[N]T` / `[N]T{...}`; the "[" is already consumed, the
// "]" is left for the caller. A plain number literal stays a plain int,
// anything else is parsed as an expression and folded by the resolver.
parse_array_size :: proc(p: ^Parser) -> (size: int, expr: Maybe(ExprId)) {
    n := current_token(p)
    if n.kind == .Number && is_symbol(next_token(p), "]") {
        consume_token(p)
        size64, ok := parse_integer_literal(n.text)
        if !ok do fail_at(n.span, "Invalid array size.")
        return int(size64), nil
    }
    return 0, parse_binop(p, 0)
}

parse_binop :: proc(p: ^Parser, min_prec: int) -> ExprId {
    lhs := parse_compiler_op(p)
    start := span_of(lhs).start

    for {
        op := current_token(p)
        prec := op_precedence(op)
        if prec < min_prec do break

        consume_token(p)
        next_min := prec if op_is_right_assoc(op) else prec + 1
        rhs := parse_binop(p, next_min)

        kind, _ := op_kind(op)
        lhs = add_expr(
            Binop{kind=kind, left=lhs, right=rhs},
            Span{start=start, end=span_of(rhs).end},
        )
    }
    return lhs
}

// `cast(T) x` / `transmute(T) x`
parse_compiler_op :: proc(p: ^Parser) -> ExprId {
    tok := current_token(p)
    if tok.kind != .Cast && tok.kind != .Transmute {
        return parse_unary(p)
    }
    consume_token(p)
    expect_symbol(p, "(")
    ty := parse_type(p)
    expect_symbol(p, ")")
    target := parse_compiler_op(p)

    e: Expr
    if tok.kind == .Cast {
        e = Cast{ty, target}
    } else {
        e = Transmute{ty, target}
    }
    return add_expr(e, Span{start=tok.span.start, end=span_of(target).end})
}

parse_unary :: proc(p: ^Parser) -> ExprId {
    t := current_token(p)
    /* if is_symbol(t, "+") { // no-op
        consume_token(p)
        return parse_compiler_op(p)
    } */ // what?
    if is_symbol(t, "&") || is_symbol(t, "!") || is_symbol(t, "-") {
        consume_token(p)
        operand := parse_compiler_op(p) // so `-cast(T) x` works
        e: Expr
        switch t.text {
        case "&": e = Reference{operand}
        case "!": e = UnNot{operand}
        case:     e = UnNegative{operand}
        }
        return add_expr(e, Span{start=t.span.start, end=span_of(operand).end})
    }
    return parse_postfix(p)
}

parse_postfix :: proc(p: ^Parser) -> ExprId {
    t := parse_primary(p)
    for {
        start := span_of(t).start
        if turbofish_ahead(p) > 0 {
            // `f::<A, B>(..)`: the type args must be followed by a call
            targs, tspan := parse_type_args(p)
            if !is_symbol(current_token(p), "(") {
                fail_at(current_token(p).span, "Expected \"(\" after type arguments.")
            }
            t = parse_call(p, t, targs, tspan)
        } else if is_symbol(current_token(p), "(") {
            t = parse_call(p, t)
        } else if is_symbol(current_token(p), ".") {
            consume_token(p)
            ident := expect_ident(p)
            t = add_expr(
                FieldAccess{target=t, field=ident.text},
                Span{start=start, end=ident.span.end},
            )
        } else if is_symbol(current_token(p), "[") {
            t = parse_index_or_slice(p, t)
        } else if is_symbol(current_token(p), "^") {
            caret := consume_token(p)
            t = add_expr(Deref{t}, Span{start=start, end=caret.span.end})
        } else do break
    }
    return t
}

parse_primary :: proc(p: ^Parser) -> ExprId {
    tok := current_token(p)

    #partial switch tok.kind {
    case .Keyword:
        if tok.kw == .Fn do return parse_fn_lit(p)
    case .Symbol:
        if tok.text == "(" do return parse_paren_expr(p)
    case .Ident:
        consume_token(p)
        return add_expr(Symbol{tok.text}, tok.span)
    case .Number:
        consume_token(p)
        return add_expr(Number{tok.text}, tok.span)
    case .True:
        consume_token(p)
        return add_expr(BoolLitTrue{}, tok.span)
    case .False:
        consume_token(p)
        return add_expr(BoolLitFalse{}, tok.span)
    case .Len:
        consume_token(p)
        expect_symbol(p, "(")
        e := parse_expr(p)
        close := expect_symbol(p, ")")
        return add_expr(Len{e}, span_between(tok.span, close.span))
    case .TypeInfoOf:
        consume_token(p)
        expect_symbol(p, "(")
        e := parse_expr(p)
        close := expect_symbol(p, ")")
        return add_expr(TypeInfoOf{e}, span_between(tok.span, close.span))
    case .TypeIdOf:
        consume_token(p)
        expect_symbol(p, "(")
        node := TypeIdOf{}
        node.t, node.expr = parse_type_or_expr(p)
        close := expect_symbol(p, ")")
        return add_expr(node, span_between(tok.span, close.span))
    case .SizeOf:
        consume_token(p)
        expect_symbol(p, "(")
        t, ex := parse_type_or_expr(p)
        close := expect_symbol(p, ")")
        return add_expr(SizeOf{t=t, expr=ex}, span_between(tok.span, close.span))
    }
    fail_at(tok.span, "invalid primary token")
}

parse_paren_expr :: proc(p: ^Parser) -> ExprId {
    open := consume_token(p)
    e := parse_nested_expr(p)
    close := expect_symbol(p, ")")
    // the parens' span replaces the inner span (handy for binop spans)
    set_expr_span(e, span_between(open.span, close.span))
    return e
}

parse_call :: proc(p: ^Parser, target: ExprId,
        type_args: []TypeSpecifier = nil, type_args_span: Span = {}) -> ExprId {
    consume_token(p) // "("
    args := new_list(FnCallArg)
    seen_named := false
    for !is_symbol(current_token(p), ")") {
        arg := FnCallArg{}
        // `name = expr`; positional args must all come before these
        if current_token(p).kind == .Ident && is_symbol(next_token(p), "=") {
            n := consume_token(p)
            consume_token(p) // "="
            arg.name = n.text
            arg.name_span = n.span
            seen_named = true
        } else if seen_named {
            fail_at(current_token(p).span, "Positional arguments can't come after named arguments.")
        }
        arg.expr = parse_nested_expr(p)
        append(&args, arg)
        if !accept_symbol(p, ",") do break
    }
    end := expect_symbol(p, ")")
    return add_expr(
        FnCall{target=target, args=args, type_args=type_args, type_args_span=type_args_span},
        Span{start=span_of(target).start, end=end.span.end},
    )
}

// `t[i]`, `t[a:b]`, `t[:b]`, `t[a:]`, `t[:]`
parse_index_or_slice :: proc(p: ^Parser, t: ExprId) -> ExprId {
    consume_token(p) // "["
    slice := TakeSlice{target=t, empty_start=true, empty_end=true}
    first: ExprId
    if !is_symbol(current_token(p), ":") {
        first = parse_nested_expr(p)
        slice.start = first
        slice.empty_start = false
    }
    is_slice := accept_symbol(p, ":")
    if is_slice && !is_symbol(current_token(p), "]") {
        slice.end = parse_nested_expr(p)
        slice.empty_end = false
    }
    end := expect_symbol(p, "]") // errors on `a[1 2`

    span := Span{start=span_of(t).start, end=end.span.end}
    if is_slice {
        return add_expr(slice, span)
    }
    return add_expr(Index{target=t, index=first}, span)
}

// fn(args): ret { ... }      block body
// fn(args): ret => expr      expression body
parse_fn_lit :: proc(p: ^Parser) -> ExprId {
    kw := consume_token(p) // "fn"

    lit := FnLit{}
    lit.signature = parse_args_dec(p)

    // the body is a fresh context: struct literals are fine in here even if
    // the lambda sits inside an `if`/`while` condition.
    prev := p.ignore_struct_lit
    p.ignore_struct_lit = false
    defer p.ignore_struct_lit = prev

    end_pos: int
    if is_fat_arrow(p) {
        consume_fat_arrow(p)
        e := parse_expr(p)
        end_pos = span_of(e).end

        ret_id := add_stmt(Return{expr=e}, span_of(e))
        stmts := new_list(StmtId)
        append(&stmts, ret_id)
        lit.block = Block{stmts=stmts[:]}
    } else if is_symbol(current_token(p), "{") {
        lit.block = parse_block(p)
        end_pos = prev_token_span(p).end // the closing "}"
    } else {
        fail_at(current_token(p).span, "Expected '{' or '=>' after function literal signature.")
    }

    lit.span = {start=kw.span.start, end=end_pos}
    return add_expr(lit, lit.span)
}

// Current token is an ident and the next is "(": is the matching ")" followed by "{"?
// Only reads tokens, so it can't panic like parse_type would on `f(1 + 2)`.
is_generic_lit_ahead :: proc(p: ^Parser) -> bool {
    if current_token(p).kind != .Ident || !is_symbol(peek_token(p, 1), "(") do return false
    depth := 0
    for i := 1; ; i += 1 {
        t := peek_token(p, i)
        if t.kind == .EOF || is_symbol(t, ";") do return false
        if is_symbol(t, "(") {
            depth += 1
        } else if is_symbol(t, ")") {
            depth -= 1
            if depth == 0 do return is_symbol(peek_token(p, i + 1), "{")
        }
    }
}

// Syntax that can only be a type, never an expression.
starts_type_only :: proc(p: ^Parser) -> bool {
    t := current_token(p)
    return is_symbol(t, "^") || is_symbol(t, "[") || is_symbol(t, "$") ||
           is_kw(t, .Fn) || t.kind == .Any
}

// `Box(^i32)`, `Pair([]i64, $T)`: a call-looking `Name(...)` with an argument
// that only makes sense as a type. Plain `Box(i32)` can't be told apart from a
// call here, the resolver handles that (expr_as_type_spec).
type_args_ahead :: proc(p: ^Parser) -> bool {
    if current_token(p).kind != .Ident || !is_symbol(peek_token(p, 1), "(") do return false
    depth := 0
    for i := 1; ; i += 1 {
        t := peek_token(p, i)
        if t.kind == .EOF || is_symbol(t, ";") do return false
        if is_symbol(t, "(") {
            depth += 1
        } else if is_symbol(t, ")") {
            depth -= 1
            if depth == 0 do return false
        } else if depth >= 1 {
            prev := peek_token(p, i - 1)
            if (is_symbol(prev, "(") || is_symbol(prev, ",")) &&
               (is_symbol(t, "^") || is_symbol(t, "[") || is_symbol(t, "$") || t.kind == .Any) {
                return true
            }
        }
    }
}

// Argument of `size_of(..)` / `type_id(..)`: type-only syntax, or an expression
// (a bare type name parses as a Symbol, the resolver sorts it out).
parse_type_or_expr :: proc(p: ^Parser) -> (t: Maybe(TypeSpecifier), e: Maybe(ExprId)) {
    if starts_type_only(p) || type_args_ahead(p) {
        return parse_type(p), nil
    }
    return nil, parse_expr(p)
}

// ===========================================================================
// Turbofish / generics
// ===========================================================================

// How many tokens the `::<` opener takes at the current position (`::` may
// be lexed as one symbol or two), 0 if there is none.
turbofish_ahead :: proc(p: ^Parser) -> int {
    if is_symbol(current_token(p), "::") && is_symbol(peek_token(p, 1), "<") do return 2
    if is_symbol(current_token(p), ":") && is_symbol(peek_token(p, 1), ":") &&
       is_symbol(peek_token(p, 2), "<") { return 3 }
    return 0
}

// `::<A, B>` at a call site.
parse_type_args :: proc(p: ^Parser) -> ([]TypeSpecifier, Span) {
    start := current_token(p).span.start
    tb := turbofish_ahead(p)
    for _ in 0..<tb { consume_token(p); }

    list := new_list(TypeSpecifier)
    for !is_symbol(current_token(p), ">") {
        append(&list, parse_type(p))
        if !accept_symbol(p, ",") do break
    }
    close := current_token(p)
    if len(list) == 0 do fail_at(close.span, "Expected at least one type argument.")
    expect_symbol(p, ">")
    return list[:], Span{start=start, end=close.span.end}
}

// `::<$K, $V>` in `fn name::<$K, $V>(..)`. Returns nil if there is none.
parse_generic_params :: proc(p: ^Parser) -> []GenericParam {
    skip := turbofish_ahead(p)
    if skip == 0 do return nil
    for _ in 0..<skip do consume_token(p)

    list := new_list(GenericParam)
    for !is_symbol(current_token(p), ">") {
        expect_symbol(p, "$")
        n := expect_ident(p)
        append(&list, GenericParam{name=n.text, span=n.span})
        if !accept_symbol(p, ",") do break
    }
    if len(list) == 0 do fail_at(current_token(p).span, "Expected at least one type parameter name.")
    expect_symbol(p, ">")
    return list[:]
}

// ===========================================================================
// Types
// ===========================================================================

type_span :: proc(t: ^TypeSpecifier) -> Span {
    switch v in t {
    case UnionSpecifier:        return v.span
    case StructSpecifier:       return v.span
    case BaseType:              return v.span
    case PointerType:           return v.span
    case SliceSpecifier:        return v.span
    case FixedArreySpecifier:   return v.span
    case AnySpecifier:          return v.span
    case FnSpecifier:           return v.span
    case GenericSpecifier:      return v.span
    case GenericInstSpecifier:  return v.span
    }
    panic("impl")
}

// All names in an `a, b: T` group share T; only the first may declare a `$T`.
share_type :: proc(ty: TypeSpecifier, index: int) -> TypeSpecifier {
    if g, ok := ty.(GenericSpecifier); ok && index > 0 {
        return BaseType{ident=g.name, span=g.span}
    }
    return ty
}

parse_boxed_type :: proc(p: ^Parser) -> ^TypeSpecifier {
    b := new(TypeSpecifier, allocator=get_ctx().allocator)
    b^ = parse_type(p)
    return b
}

parse_type :: proc(p: ^Parser) -> TypeSpecifier {
    tok := current_token(p)

    switch {
    case is_symbol(tok, "$"):
        consume_token(p)
        n := expect_ident(p)
        return GenericSpecifier{name=n.text, span=span_between(tok.span, n.span)}

    case tok.kind == .Ident:
        return parse_named_type(p)

    case is_kw(tok, .Fn):
        consume_token(p)
        sig := new(FnDecSignature, get_ctx().allocator)
        sig^ = parse_args_dec(p)
        return FnSpecifier{span=tok.span, signature=sig}

    case is_symbol(tok, "["):
        consume_token(p)
        if accept_symbol(p, "]") { // slice
            base := parse_boxed_type(p)
            return SliceSpecifier{base=base, span=Span{start=tok.span.start, end=type_span(base).end}}
        }
        size, size_expr := parse_array_size(p)
        expect_symbol(p, "]")
        base := parse_boxed_type(p)
        return FixedArreySpecifier{
            size=size, size_expr=size_expr, base=base,
            span=Span{start=tok.span.start, end=type_span(base).end},
        }

    case is_symbol(tok, "^"):
        consume_token(p)
        base := parse_boxed_type(p)
        return PointerType{ptr=base, span=Span{start=tok.span.start, end=type_span(base).end}}

    case is_kw(tok, .Struct):
        consume_token(p)
        fields, close := parse_struct_body(p)
        return StructSpecifier{fields=fields, span=span_between(tok.span, close.span)}

    case is_kw(tok, .Union):
        consume_token(p)
        fields, close := parse_struct_body(p)
        return UnionSpecifier{fields=fields, span=span_between(tok.span, close.span)}

    case tok.kind == .Any:
        consume_token(p)
        return AnySpecifier{span=tok.span}
    }
    fail_at(tok.span, "Invalid token in type specifier.")
}

// `Name` or `Name(args)` (generic struct instance)
parse_named_type :: proc(p: ^Parser) -> TypeSpecifier {
    name := consume_token(p)
    if !is_symbol(current_token(p), "(") {
        return BaseType{ident=name.text, span=name.span}
    }
    consume_token(p) // "("
    if is_symbol(current_token(p), ")") {
        fail_at(current_token(p).span, "Expected at least one generic argument.")
    }
    args := new_list(TypeSpecifier)
    for {
        append(&args, parse_type(p))
        if !accept_symbol(p, ",") do break
    }
    close := current_token(p)
    expect_symbol(p, ")")
    return GenericInstSpecifier{name=name.text, args=args, span=span_between(name.span, close.span)}
}

// `a` or `a, b, c` (the names of a `a, b: type` group). The caller expects
// the ":" afterwards.
parse_ident_group :: proc(p: ^Parser) -> [dynamic]Token {
    names := new_list(Token)
    append(&names, expect_ident(p))
    for accept_symbol(p, ",") {
        append(&names, expect_ident(p))
    }
    return names
}

// `{ a, b: T, c: U }`; returns the fields and the closing brace token
parse_struct_body :: proc(p: ^Parser) -> (fields: []StructField, close: Token) {
    expect_symbol(p, "{")
    out := new_list(StructField)
    for !is_symbol(current_token(p), "}") {
        names := parse_ident_group(p)
        expect_symbol(p, ":")
        ty := parse_type(p)
        for n, i in names {
            append(&out, StructField{name=n.text, t=share_type(ty, i), span=n.span})
        }
        if !accept_symbol(p, ",") do break
    }
    close = current_token(p)
    expect_symbol(p, "}")
    return out[:], close
}

// ===========================================================================
// Statements
// ===========================================================================

parse_block :: proc(p: ^Parser) -> Block {
    expect_symbol(p, "{")
    prev := p.ignore_struct_lit
    p.ignore_struct_lit = false
    stmts := new_list(StmtId)
    for !is_symbol(current_token(p), "}") && current_token(p).kind != .EOF {
        append(&stmts, parse_stmt(p))
    }
    expect_symbol(p, "}")
    p.ignore_struct_lit = prev
    return Block{stmts=stmts[:]}
}

parse_stmt :: proc(p: ^Parser) -> StmtId {
    tok := current_token(p)
    next := next_token(p)

    switch {
    case tok.kind == .Ident && (is_symbol(next, ":=") || is_symbol(next, ":")):
        return parse_var_dec(p)
    case is_kw(tok, .Return):   return parse_return(p)
    case is_kw(tok, .If):       return parse_if(p)
    case is_kw(tok, .While):    return parse_while(p)
    case is_kw(tok, .For):      return parse_for(p)
    case is_kw(tok, .Break), is_kw(tok, .Continue):
        return parse_loop_jump(p)
    }
    return parse_expr_stmt(p)
}

// `name := expr;` or `name: type = expr;`
parse_var_dec :: proc(p: ^Parser) -> StmtId {
    name := consume_token(p)
    ty: Maybe(TypeSpecifier)
    if !accept_symbol(p, ":=") {
        expect_symbol(p, ":")
        ty = parse_type(p)
        expect_symbol(p, "=")
    }
    value := parse_expr(p)
    end: Span
    if s, ok := maybe_semi(p).(Token); ok { end = s.span } else { end = get_span(value).span }
    return add_stmt(VarDec{name=name.text, type=ty, value=value}, span_between(name.span, end))
}

parse_return :: proc(p: ^Parser) -> StmtId {
    tok := consume_token(p)
    if is_symbol(current_token(p), ";") {
        semi := consume_token(p)
        return add_stmt(Return{expr=nil}, span_between(tok.span, semi.span))
    }
    e := parse_expr(p)
    end : Span
    if s, ok := maybe_semi(p).(Token); ok { end = s.span } else { end = get_span(e).span }
    return add_stmt(Return{expr=e}, span_between(tok.span, end))
}

parse_if :: proc(p: ^Parser) -> StmtId {
    tok := consume_token(p) // "if"
    s := IfElse{}
    s.base_con = parse_condition(p)
    s.base_block = parse_block(p)

    alts := new_list(AltCon)
    for is_current_kw(p, .Else) && is_kw(next_token(p), .If) {
        consume_token(p) // else
        consume_token(p) // if
        cond := parse_condition(p)
        append(&alts, AltCon{cond, parse_block(p)})
    }
    s.alt = alts[:]

    if is_current_kw(p, .Else) {
        consume_token(p)
        s.else_block = parse_block(p)
        s.has_else_block = true
    }
    return add_stmt(s, tok.span)
}

parse_while :: proc(p: ^Parser) -> StmtId {
    tok := consume_token(p) // "while"
    cond := parse_condition(p)
    block := parse_block(p)
    return add_stmt(WhileLoop{cond, block}, tok.span)
}

// `for name in expr { ... }`
parse_for :: proc(p: ^Parser) -> StmtId {
    tok := consume_token(p) // "for"
    ident := expect_ident(p)
    if !is_current_kw(p, .In) {
        fail_at(current_token(p).span, "Expected \"in\".")
    }
    consume_token(p) // "in"
    expr := parse_condition(p)
    block := parse_block(p)
    return add_stmt(ForLoop{name=ident.text, expr=expr, block=block}, tok.span)
}

parse_loop_jump :: proc(p: ^Parser) -> StmtId {
    tok := consume_token(p) // "break" / "continue"
    maybe_semi(p)
    if tok.kw == .Break {
        return add_stmt(BreakStmt{}, tok.span)
    }
    return add_stmt(ContinueStmt{}, tok.span)
}

// Expression statement, `a = b;` or `a += b;`
parse_expr_stmt :: proc(p: ^Parser) -> StmtId {
    expr := parse_expr(p)
    start := span_of(expr).start

    if accept_symbol(p, "=") {
        v := parse_expr(p)
        end : Span
        if s, ok := maybe_semi(p).(Token); ok { end = s.span } else { end = get_span(v).span }

        return add_stmt(Assignment{target=expr, value=v}, Span{start=start, end=end.end})
    }

    if kind, ok := compound_op_kind(current_token(p)); ok {
        consume_token(p)
        v := parse_expr(p)
        end : Span
        if s, ok := maybe_semi(p).(Token); ok { end = s.span } else { end = get_span(v).span }

        // `a += b` is stored as `a = a + b` with `compound` set
        value := add_expr(
            Binop{kind=kind, left=expr, right=v},
            Span{start=start, end=span_of(v).end},
        )
        return add_stmt(
            Assignment{target=expr, value=value, kind=kind, compound=true},
            Span{start=start, end=end.end},
        )
    }

    maybe_semi(p)
    return add_stmt(Stmt(expr), span_of(expr))
}

// ===========================================================================
// Functions
// ===========================================================================

// Unique across the whole compiler run, not per file: codegen binds items
// into its scope by *name*, so two modules must never produce the same name.
default_thunk_counter: int

// A parameter default can be any expression, so it can't be re-evaluated at
// the call site (a caller's local might shadow a global it names). Instead it
// moves into a hidden top-level function
//
//     fn __default_N(): <param type> { return <expr>; }
//
// and the default becomes the call `__default_N()`, re-evaluated on every
// call that omits the argument. `ty` is nil for `name := expr` parameters
// (the type checker fills in the return type). The thunk is queued in
// p.extra_items and inserted ahead of the item containing the signature.
make_default_thunk :: proc(p: ^Parser, ty: Maybe(TypeSpecifier), expr: ExprId) -> ExprId {
    span := span_of(expr)

    name := fmt.aprintf("__default_%d", default_thunk_counter, allocator=get_ctx().allocator)
    default_thunk_counter += 1

    stmts := new_list(StmtId)
    append(&stmts, add_stmt(Return{expr=expr}, span))

    f := FnDec{}
    f.name = name
    f.ret_ty = ty
    f.span = span
    f.block = Block{stmts=stmts[:]}
    append(&p.extra_items, add_item(f, span))

    sym := add_expr(Symbol{name}, span)
    return add_expr(FnCall{target=sym, args=new_list(FnCallArg)}, span)
}

// Parameters are `name: type`, `name: type = default`, `name := default`
// (type inferred from the default) or a trailing variadic `name: ..type`.
//
// Once a parameter has a default every later one must too, and defaults can't
// be combined with a variadic tail. `any` parameters can't have a default.
// `a, b: type` groups can't have defaults or be variadic.
//
// For the inferred form `inferred` is set, `t` is a placeholder and
// `default_src` is the written expression.
parse_args_dec :: proc(p: ^Parser) -> FnDecSignature {
    f := FnDecSignature{}
    args := new_list(FnDecArg)
    seen_default := false
    expect_symbol(p, "(")

    for !is_symbol(current_token(p), ")") {
        names := parse_ident_group(p)
        name := names[len(names) - 1]
        grouped := len(names) > 1

        ty: TypeSpecifier
        written: ExprId
        has_written := false
        inferred := false

        if is_symbol(current_token(p), ":=") {
            if grouped do fail_at(current_token(p).span, GROUPED_DEFAULT_MSG)
            consume_token(p)
            inferred = true
            ty = BaseType{ident="", span=name.span} // placeholder
            written = parse_nested_expr(p)
            has_written = true
        } else {
            expect_symbol(p, ":")
            if is_symbol(current_token(p), ".") && is_symbol(next_token(p), ".") {
                if grouped do fail_at(name.span, "A variadic parameter can't be part of a grouped declaration.")
                consume_token(p)
                consume_token(p)
                f.is_variadic = true
                f.variadic_ty = parse_type(p)
                f.variadic_arg_name = name.text
                f.variadic_arg_span = name.span
                break
            }
            ty = parse_type(p)

            if is_symbol(current_token(p), "=") {
                if grouped do fail_at(current_token(p).span, GROUPED_DEFAULT_MSG)
                consume_token(p)
                if _, is_any := ty.(AnySpecifier); is_any {
                    fail_at(name.span, "Default values aren't supported for `any` parameters.")
                }
                written = parse_nested_expr(p)
                has_written = true
            }
        }

        default: Maybe(ExprId)
        default_src: Maybe(ExprId)
        if has_written {
            thunk_ret: Maybe(TypeSpecifier) // stays nil when inferred
            if !inferred do thunk_ret = ty
            default = make_default_thunk(p, thunk_ret, written)
            if inferred do default_src = written
            seen_default = true
        } else if seen_default {
            fail_at(names[0].span, "A parameter without a default value can't come after one with a default.")
        }

        for n, i in names {
            append(&args, FnDecArg{
                name=n.text, t=share_type(ty, i), span=n.span,
                default=default, inferred=inferred, default_src=default_src,
            })
        }
        if !accept_symbol(p, ",") do break
    }
    end := expect_symbol(p, ")")
    if f.is_variadic && seen_default {
        fail_at(end.span, "Default parameter values can't be combined with a variadic parameter.")
    }
    f.args = args[:]
    if accept_symbol(p, ":") {
        f.ret_ty = parse_type(p)
    }
    return f
}

// `fn name(..)` or `fn name::<$K, $V>(..)`
parse_fn_signature :: proc(p: ^Parser) -> FnDec {
    kw := consume_token(p) // "fn"
    if !is_kw(kw, .Fn) do fail_at(kw.span, "Expected \"fn\".")
    name := expect_ident(p)
    generics := parse_generic_params(p)

    f := FnDec{}
    f.signature = parse_args_dec(p)
    f.name = name.text
    f.generics = generics // after the signature: it replaces the whole struct
    f.span = {start=kw.span.start, end=current_token(p).span.end}
    return f
}

// ===========================================================================
// Top-level items
// ===========================================================================

// Top-level `name := expr;`, `name: type = expr;`, `name :: expr;` or `name : type : expr;`
parse_global_var_dec :: proc(p: ^Parser) -> ItemId {
    name := expect_ident(p)

    ty: Maybe(TypeSpecifier)
    is_const := false
    if accept_symbol(p, ":=") {
        // inferred variable
    } else if accept_symbol(p, "::") {
        is_const = true
    } else if accept_symbol(p, ":") {
        if accept_symbol(p, ":") { // `::` lexed as two tokens
            is_const = true
        } else {
            ty = parse_type(p)
            if accept_symbol(p, ":") { // `name : T : value`
                is_const = true
            } else {
                expect_symbol(p, "=")
            }
        }
    } else {
        fail_at(current_token(p).span,
            "Expected \":=\", \":\" or \"::\" after a top-level name (only declarations are allowed at the top level).")
    }

    if is_const && is_current_kw(p, .Struct) {
        fail_at(current_token(p).span,
            "`::` only declares constant values for now (use `struct Name {...}` for structs).")
    }

    value := parse_expr(p)
    end : Span
    if s, ok := maybe_semi(p).(Token); ok { end = s.span } else { end = get_span(value).span }


    item: Item
    if is_const {
        item = ConstDec{name=name.text, type=ty, value=value}
    } else {
        item = GlobalVarDec{name=name.text, type=ty, value=value}
    }
    return add_item(item, span_between(name.span, end))
}

parse_module_kw :: proc(p: ^Parser) -> ItemId {
    #partial switch current_token(p).kw {
    case .Alias:  return parse_type_alias(p)
    case .Struct: return parse_struct_dec(p)
    case .Fn:     return parse_fn_item(p, false)
    case .Export:
        consume_token(p)
        return parse_fn_item(p, true)
    case .Forward:
        s := consume_token(p)
        i := parse_import(p);
        return add_item(Forward{i}, s.span);
    case .Extern: return parse_extern_fn(p)
    case .Import: return parse_import(p)
    }
    fail_at(current_token(p).span, "Unexpected keyword at top level.")
}

// `alias Name = type;`
parse_type_alias :: proc(p: ^Parser) -> ItemId {
    kw := consume_token(p)
    new_name := expect_ident(p)
    expect_symbol(p, "=")
    ty := parse_type(p)
    end : Span
    if s, ok := maybe_semi(p).(Token); ok { end = s.span } else { end = type_span(&ty) }

    return add_item(TypeAlias{new_name.text, ty}, span_between(kw.span, end))
}

// `($T, $G)` in `struct Name($T, $G) { .. }`. Returns nil if there is none.
parse_struct_generic_params :: proc(p: ^Parser) -> []GenericParam {
    if !accept_symbol(p, "(") do return nil

    list := new_list(GenericParam)
    for !is_symbol(current_token(p), ")") {
        expect_symbol(p, "$")
        n := expect_ident(p)
        append(&list, GenericParam{name=n.text, span=n.span})
        if !accept_symbol(p, ",") do break
    }
    if len(list) == 0 {
        fail_at(current_token(p).span, "Expected at least one type parameter name.")
    }
    expect_symbol(p, ")")
    return list[:]
}
// `struct Name { a, b: T }` or `struct Name($T, $G) { a: T, b: G }`
parse_struct_dec :: proc(p: ^Parser) -> ItemId {
    consume_token(p) // "struct"
    name := expect_ident(p)
    generics := parse_struct_generic_params(p)
    fields, _ := parse_struct_body(p)
    return add_item(StructDec{name=name.text, generics=generics, fields=fields}, name.span)
}

// `fn ... { }`; the optional `export` is already consumed
parse_fn_item :: proc(p: ^Parser, exported: bool) -> ItemId {
    f := parse_fn_signature(p)
    f.block = parse_block(p)
    f.export_name = exported
    return add_item(f, f.span)
}

// `extern fn name(..): T;`
parse_extern_fn :: proc(p: ^Parser) -> ItemId {
    consume_token(p) // "extern"
    f := parse_fn_signature(p)
    maybe_semi(p)
    return add_item(ExternFnDec{signature=f.signature, span=f.span}, f.span)
}

// `import "file.gala";`
parse_import :: proc(p: ^Parser) -> ItemId {
    importer := cur_file()
    kw := consume_token(p) // "import"
    if current_token(p).kind == .String {
        fname := consume_token(p)
        if fname.kind != .String do fail_at(fname.span, "Expected string.")
        end : Span
        if s, ok := maybe_semi(p).(Token); ok { end = s.span } else { end = fname.span }
        handle_file(fname.text)
        get_ctx().current_file = importer // handle_file switches to the imported file

        return add_item(Import{fname=fname.text, symbols=nil, imports_all=true}, span_between(kw.span, end))
    } else if is_symbol(current_token(p), "{") {
        open := consume_token(p);
        symbols := make([dynamic]string, get_ctx().allocator);
        for !is_symbol(current_token(p), "}") {
            n := expect_ident(p);
            append(&symbols, n.text);
            if is_symbol(current_token(p), ",") {
                consume_token(p);
                continue;
            } else do break;
        }
        expect_symbol(p, "}");
        from := consume_token(p);
        if from.kind != .Keyword || from.kw != .From {
            fail_at(from.span, "Expected keyword \"from\".");
        }
        fname := consume_token(p)
        if fname.kind != .String do fail_at(fname.span, "Expected string.")
        end : Span
        if s, ok := maybe_semi(p).(Token); ok { end = s.span } else { end = fname.span }
        handle_file(fname.text)
        get_ctx().current_file = importer // handle_file switches to the imported file
        return add_item(Import{fname=fname.text, symbols=symbols[:], imports_all=false}, span_between(kw.span, end))

    }
    fail_at(current_token(p).span, "Invalid token after \"import\".");
}

// ---- compile-time conditions ----------------------------------------------
// cond := or ; or := and ("||" and)* ; and := not ("&&" not)*
// not  := "!" not | "(" cond ")" | WINDOWS | POSIX

comptime_name :: proc(t: Token) -> bool {
    is_windows := abi_resolve_kind(get_ctx().abi) == .Win64
    switch t.text {
    case "WINDOWS": return is_windows
    case "POSIX":   return !is_windows
    }
    highlight_lines(cur_file(), t.span)
    gala_panic("Unknown compile-time condition:", t.text, "(expected WINDOWS or POSIX)")
}

parse_comptime_not :: proc(p: ^Parser) -> bool {
    if accept_symbol(p, "!") {
        return !parse_comptime_not(p)
    }
    if accept_symbol(p, "(") {
        v := parse_comptime_cond(p)
        expect_symbol(p, ")")
        return v
    }
    return comptime_name(expect_ident(p))
}

parse_comptime_and :: proc(p: ^Parser) -> bool {
    v := parse_comptime_not(p)
    for accept_symbol(p, "&&") {
        r := parse_comptime_not(p) // no short circuit: always consume the tokens
        v = v && r
    }
    return v
}

parse_comptime_cond :: proc(p: ^Parser) -> bool {
    v := parse_comptime_and(p)
    for accept_symbol(p, "||") {
        r := parse_comptime_and(p)
        v = v || r
    }
    return v
}

// `(cond)`
parse_comptime_paren_cond :: proc(p: ^Parser) -> bool {
    expect_symbol(p, "(")
    cond := parse_comptime_cond(p)
    expect_symbol(p, ")")
    return cond
}

// Skips a `{ ... }` token-wise. The skipped code is never parsed, so
// imports in it aren't loaded and it may not even be valid on this platform.
skip_braced_block :: proc(p: ^Parser) {
    open := expect_symbol(p, "{")
    depth := 1
    for depth > 0 {
        t := consume_token(p)
        if t.kind == .EOF do fail_at(open.span, "Unterminated `{` in compile-time if.")
        if is_symbol(t, "{") {
            depth += 1
        } else if is_symbol(t, "}") {
            depth -= 1
        }
    }
}

// Parses the block if `cond` holds and no earlier branch was taken, skips it otherwise.
parse_comptime_branch :: proc(p: ^Parser, items: ^[dynamic]ItemId, cond: bool, taken: ^bool) {
    if cond && !taken^ {
        taken^ = true
        expect_symbol(p, "{")
        parse_item_list(p, items, true)
        expect_symbol(p, "}")
    } else {
        skip_braced_block(p)
    }
}

// `@if(cond) { items } else if (cond) { items } else { items }`
// The "@" is already consumed; the current token is `if`.
parse_comptime_if :: proc(p: ^Parser, items: ^[dynamic]ItemId) {
    consume_token(p) // "if"
    taken := false

    first := parse_comptime_paren_cond(p)
    parse_comptime_branch(p, items, first, &taken)

    for is_current_kw(p, .Else) {
        consume_token(p)
        if is_current_kw(p, .If) {
            consume_token(p)
            c := parse_comptime_paren_cond(p)
            parse_comptime_branch(p, items, c, &taken)
        } else {
            parse_comptime_branch(p, items, true, &taken)
            break
        }
    }
}

// `@link("./x.a")` / `@link("../x.a")` are relative to the file containing
// the directive. Anything else (absolute paths, bare names) goes to ld as is.
resolve_link_path :: proc(importer: string, path: string) -> string {
    if strings.has_prefix(path, "./") || strings.has_prefix(path, "../") {
        return resolve_import_path(importer, path)
    }
    return path
}

// `@link("x.a")`, `@link(WINDOWS, "x.lib")`, `@link(POSIX, "a.a", "b.a")`.
// The "@" is already consumed; the current token is `link`.
parse_link_directive :: proc(p: ^Parser, file_name: string) {
    consume_token(p) // "link"
    expect_symbol(p, "(")
    active := true
    if current_token(p).kind != .String {
        active = parse_comptime_cond(p)
        expect_symbol(p, ",")
    }
    for {
        if current_token(p).kind != .String {
            fail_at(current_token(p).span, "Expected a string for the @link path.")
        }
        s := consume_token(p)
        if active {
            append(&get_ctx().links, resolve_link_path(file_name, s.text))
        }
        if !accept_symbol(p, ",") do break
    }
    expect_symbol(p, ")")
}

// `@if ...` or `@link ...`
parse_directive :: proc(p: ^Parser, items: ^[dynamic]ItemId) {
    consume_token(p) // "@"
    cur := current_token(p)
    if is_kw(cur, .If) {
        parse_comptime_if(p, items)
    } else if cur.kind == .Link {
        parse_link_directive(p, p.file)
    } else {
        fail_at(cur.span, "Unexpected token. want compiler directive")
    }
}

// Parses items until EOF, or (nested) until the `}` closing a compile-time block.
parse_item_list :: proc(p: ^Parser, items: ^[dynamic]ItemId, nested: bool) {
    for current_token(p).kind != .EOF {
        tok := current_token(p)
        if nested && is_symbol(tok, "}") do return

        id: ItemId
        switch {
        case tok.kind == .Keyword:
            id = parse_module_kw(p)
        case tok.kind == .Ident:
            id = parse_global_var_dec(p)
        case is_symbol(tok, "@"):
            parse_directive(p, items)
            continue
        case:
            fail_at(tok.span, "Expected a declaration, import or directive.")
        }

        // hidden default-value functions go in first, ahead of their item
        for extra in p.extra_items {
            append(items, extra)
        }
        clear(&p.extra_items)

        append(items, id)
    }
    if nested {
        fail_at(prev_token_span(p), "Expected \"}\" to close the compile-time block.")
    }
}

parse_tokens :: proc(file_name: string, tokens: []Token) -> AST {
    p := Parser{file=file_name, tokens=tokens}
    p.extra_items = new_list(ItemId)
    items := new_list(ItemId)
    parse_item_list(&p, &items, false)
    return AST{items=items}
}
