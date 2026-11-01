package main

import "core:fmt"
import "core:strings"

BinopKind :: enum {
    Addition,
    Subtraction,
    Multiply,
    Divide,
    Modulo,
    Equal,
    NotEqual,
    LessEqual,
    GreaterEqual,
    Less,
    Greater,
    LogicalAnd,
    LogicalOr,
    BitAnd,
    BitOr,
    BitXor,
    ShiftLeft,
    ShiftRight,
};
Binop :: struct {
    kind: BinopKind,
    left, right: ExprId,
}
Number :: struct {
    text: string,
}
Symbol :: struct {
    name: string,
}
Cast :: struct {
    to: TypeSpecifier,
    target: ExprId,
}
Transmute :: struct {
    to: TypeSpecifier,
    target: ExprId,
}
TypeIdOf :: struct {
    t:          Maybe(TypeSpecifier), // set when written with type-only syntax
    expr:       Maybe(ExprId),   // set otherwise
    names_type: bool,            // resolver: `expr` was a bare type name
}
TypeInfoOf :: struct {
    expr:       ExprId,
}
ZeroInit :: struct {}
BreakStmt :: struct {label: string};
ContinueStmt :: struct {label: string};
StructLit :: struct {
    name: string,
    // `Foo` or `Foo(i64)`; resolve this to get the struct type, `name` is for messages
    ty: TypeSpecifier,
    fields: map[string]struct{expr:ExprId,span:Span},
    // `Foo{1, 2}` form, in source order. The resolver moves these into
    // `fields`; unused after that.
    positional: []struct{expr:ExprId,span:Span},
}
Len :: struct { target: ExprId };
SizeOf :: struct {
    t:          Maybe(TypeSpecifier), // set when written with type-only syntax
    expr:       Maybe(ExprId),   // set otherwise
    names_type: bool,            // resolver: `expr` was a bare type name
}
BoolLitTrue :: distinct struct {}
BoolLitFalse :: distinct struct {}
UnNegative :: struct {
    expr: ExprId,
}
// Anonymous function expression. Both forms produce this node:
//   fn(i: i32, j: i32): i32 { return i + j }
//   fn(i: i32, j: i32): i32 => i + j
// The `=>` form is desugared into a block with a single `return <expr>;`.
FnLit :: struct {
    using signature: FnDecSignature,
    span: Span,
    block: Block,
}
// Top-level constant:
//   limit :: 100;
//   ratio : f32 : 0.5;
// The resolver folds the value and replaces every use with a literal; the
// item is dropped afterwards, so later passes never see it.
ConstDec :: struct {
    name: string,
    type: Maybe(TypeSpecifier),
    value: ExprId,
}
Expr :: union {
    StructLit,
    Binop,
    Number,
    Symbol,
    FnCall,
    FieldAccess,
    Index,
    TakeSlice,
    Cast,
    Transmute,
    TypeIdOf,
    TypeInfoOf,
    Reference,
    Deref,
    UnNot,
    UnNegative,
    FixedSizeArray,
    String,
    Len,
    SizeOf,
    ZeroInit,
    BoolLitTrue,
    BoolLitFalse,
    FnLit,
}
BoolLit :: struct { text: string }
String :: struct {
    s: string
}
Deref :: struct {
    expr: ExprId,
}
Reference :: struct {
    expr: ExprId,
}
UnNot :: struct {
    expr: ExprId,
}
Index :: struct {
    target, index: ExprId,
}
TakeSlice :: struct {
    target, start, end: ExprId,
    empty_start, empty_end: bool,
}
FixedSizeArray :: struct {
    size: int,
    // set when the size is written as a constant expression (`[MAX_COUNT]byte{}`);
    // the resolver folds it into `size` and clears this
    size_expr: Maybe(ExprId),
    ty: TypeSpecifier,
    initialiser: []ExprId,
}
FieldAccess :: struct {
    target: ExprId,
    field: string,
}
// `f::<A, B>(x)`: `type_args` are the explicit type arguments, as written;
// the resolver fills `type_arg_ids`.
FnCall :: struct {
    target: ExprId,
    args: [dynamic]FnCallArg,
    type_args: []TypeSpecifier,
    type_args_span: Span,
    type_arg_ids: []TypeId,
}
// `name` is set for `f(x = 1)` style arguments. The type checker reorders
// everything into parameter order and clears the names, so codegen only sees
// positional, full-arity calls.
FnCallArg :: struct{
    expr: ExprId,
    needs_boxing: bool,
    box_type: TypeId,
    name: string,
    name_span: Span,
};
BaseType :: struct { ident: string, span: Span };
// can't have ptr to itself
PointerType :: struct {ptr:^TypeSpecifier, span: Span };
FixedArreySpecifier :: struct { size: int, size_expr: Maybe(ExprId), base: ^TypeSpecifier, span: Span };
SliceSpecifier :: struct {base : ^TypeSpecifier, span: Span }
AnySpecifier :: struct { span: Span }
GenericSpecifier :: struct { name: string, span: Span }
GenericInstSpecifier :: struct {
    name: string,
    args: [dynamic]TypeSpecifier,
    span: Span,
}
FnSpecifier :: struct { span: Span, using signature: ^FnDecSignature }
StructSpecifier :: struct {
    fields: []StructField,
    span:   Span,
}
UnionSpecifier :: struct {
    fields: []StructField,
    span:   Span,
}
TypeSpecifier :: union {
    BaseType,
    PointerType,
    FixedArreySpecifier,
    SliceSpecifier,
    AnySpecifier,
    FnSpecifier,
    StructSpecifier,
    UnionSpecifier,
    GenericSpecifier,
    GenericInstSpecifier,
}
VarDec :: struct {
    name: string,
    type: Maybe(TypeSpecifier),
    value: ExprId,
}
Assignment :: struct {
    kind: BinopKind,
    compound: bool,
    target, value: ExprId,
}
Return :: struct {
    expr: Maybe(ExprId),
}
Stmt :: union {
    VarDec,
    Assignment,
    Return,
    ExprId,
    IfElse,
    WhileLoop,
    ForLoop,
    BreakStmt,
    ContinueStmt,
}
WhileLoop :: struct { cond: ExprId, block: Block }
ForLoop :: struct { name: string, expr: ExprId, block: Block }
AltCon :: struct{cond:ExprId, block:Block}
IfElse :: struct {
    base_con: ExprId,
    base_block: Block,
    alt: []AltCon,
    has_else_block: bool,
    else_block: Block,
}
Parser::struct{
    file: string,
    tokens: []Token,
    i: int,

    ignore_struct_lit: bool,

    // Hidden top-level items created while parsing the current top-level
    // item (see make_default_thunk). parse_tokens moves them into the item
    // list, ahead of the item that caused them.
    extra_items: [dynamic]ItemId,
}
Block :: struct {
    stmts: []StmtId,
}

FnDecArg :: struct{
    name: string,
    t: TypeSpecifier,
    span: Span,
    default: Maybe(ExprId),
    inferred: bool,
    default_src: Maybe(ExprId),
}
// A name declared in `fn f::<K, V>(..)`.
GenericParam :: struct {
    name: string,
    span: Span,
}
FnDecSignature :: struct {
    name: string,
    args: []FnDecArg,
    ret_ty: Maybe(TypeSpecifier),
    is_variadic: bool,
    variadic_ty: TypeSpecifier,
    variadic_arg_name: string,
    variadic_arg_span: Span,
    // explicit type params (`fn f::<K, V>(..)`), only on named fn items
    generics: []GenericParam,
}
FnDec :: struct {
    using signature: FnDecSignature,
    span: Span,
    block: Block,
    export_name: bool,
}
StructField :: struct{name: string, t: TypeSpecifier, span: Span}
StructDec :: struct {
    name: string,
    fields: []StructField,
}
ExternFnDec :: struct {
    using signature: FnDecSignature,
    span: Span,
}
// Top-level variable:
//   counter := 0;
//   limit: i32 = 100;
// `type` is nil for the `:=` form (inferred by the type checker). Codegen
// runs the value in a per-module init function before gala `main`, in
// declaration order.
GlobalVarDec :: struct {
    name: string,
    type: Maybe(TypeSpecifier),
    value: ExprId,
}

Import :: struct {
    fname, alias: string,
}
TypeAlias :: struct {
    new_name: string,
    ty: TypeSpecifier,
}
Item :: union {
    Import,
    TypeAlias,
    StructDec,
    FnDec,
    ExternFnDec,
    GlobalVarDec,
    ConstDec,
}
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

// Header of an `if`/`while`/`for`: `Name {` is the block, not a struct lit.
parse_condition :: proc(p: ^Parser) -> ExprId {
    prev_ignore_struct_lit := p.ignore_struct_lit
    p.ignore_struct_lit = true
    e := parse_expr(p)
    p.ignore_struct_lit = prev_ignore_struct_lit
    return e;
}

// Inside (), [] and {} a struct literal is unambiguous, even in an
// `if`/`while`/`for` header.
parse_nested_expr :: proc(p: ^Parser) -> ExprId {
    prev := p.ignore_struct_lit
    p.ignore_struct_lit = false
    e := parse_expr(p)
    p.ignore_struct_lit = prev
    return e
}

// `Name{x=1, y=2}` or `Name{1, 2}` (not both in one literal)
parse_struct_lit :: proc(p: ^Parser) -> ExprId {
    name: string
    ty: TypeSpecifier
    start: int // ASSUMPTION: your span positions are ints, use whatever type Span.start has

    if is_symbol(current_token(p), "{") {
        // untyped `{...}`: ty stays nil, the typechecker decides
        start = current_token(p).span.start
    } else if is_generic_lit_ahead(p) {
        ty = parse_type(p) // Name(args) -> GenericInstSpecifier
        inst := ty.(GenericInstSpecifier)
        name = inst.name
        start = inst.span.start
    } else {
        tok := expect_ident(p);
        name = tok.text
        ty = TypeSpecifier(BaseType({tok.text, tok.span}))
        start = tok.span.start
    }
    consume_token(p); // "{"
    fields := make(map[string]struct{expr: ExprId, span: Span}, allocator=get_ctx().allocator);
    positional := make([dynamic]struct{expr: ExprId, span: Span}, allocator=get_ctx().allocator);

    for !is_symbol(current_token(p), "}") {
        if current_token(p).kind == .Ident && is_symbol(next_token(p), "=") {
            // named: `x = expr`
            if len(positional) > 0 {
                highlight_lines(get_ctx().current_file, current_token(p).span)
                gala_panic("Can't mix named and positional fields in a struct literal.")
            }
            fname := expect_ident(p);
            consume_token(p); // "="
            expr := parse_nested_expr(p);
            if _, exists := fields[fname.text]; exists {
                highlight_lines(get_ctx().current_file, fname.span)
                gala_panic("duplicate fields.");
            }
            fields[fname.text] = {expr, fname.span}
        } else {
            // positional: `expr`
            if len(fields) > 0 {
                highlight_lines(get_ctx().current_file, current_token(p).span)
                gala_panic("Can't mix named and positional fields in a struct literal.")
            }
            expr := parse_nested_expr(p);
            append(&positional, struct{expr: ExprId, span: Span}{expr, get_span(expr).span})
        }

        if is_symbol(current_token(p), ",") {
            consume_token(p); // ","
        } else do break
    }
    end := expect_symbol(p, "}");

    id := new_expr(StructLit{name=name, ty=ty, fields=fields, positional=positional[:]})
    get_ctx().spans.exprs[id] = {
        file_name=get_ctx().current_file,
        span={start, end.span.end}
    }
    return id;
}

// `[N]T{}` (zeroed) or `[N]T{a, b, c}` (trailing comma ok). N is a number
// or a constant expression (`[MAX_COUNT]byte{}`).
parse_array_lit :: proc(p: ^Parser) -> ExprId {
    open_b := consume_token(p) // "["
    size, size_expr := parse_array_size(p)
    expect_symbol(p, "]")
    t := parse_type(p)
    expect_symbol(p, "{")

    elems := make([dynamic]ExprId, allocator=get_ctx().allocator)
    for !is_symbol(current_token(p), "}") {
        append(&elems, parse_nested_expr(p))
        if is_symbol(current_token(p), ",") {
            consume_token(p)
        } else {
            break
        }
    }
    end := expect_symbol(p, "}")
    span := Span{start=open_b.span.start, end=end.span.end}

    // with a constant-expression size the resolver does this check
    if size_expr == nil && len(elems) > size {
        highlight_lines(get_ctx().current_file, span)
        gala_panic("too many elements in array initialiser")
    }

    init: []ExprId // nil for `{}`
    if len(elems) > 0 do init = elems[:]

    id := new_expr(Expr(FixedSizeArray{size=size, size_expr=size_expr, ty=t, initialiser=init}))
    get_ctx().spans.exprs[id] = {file_name=get_ctx().current_file, span=span}
    return id
}

// Struct literals, array literals and strings are only allowed here, at the
// start of an expression (so `a == Foo{}` and `"abc"[0]` don't parse).
parse_expr :: proc(p: ^Parser) -> ExprId {
    cur := current_token(p)
    // Name(args){...}: generic struct literal
    if (current_token(p).kind == .Ident && is_symbol(next_token(p), "{") || is_generic_lit_ahead(p)) &&
        !p.ignore_struct_lit {
        return parse_struct_lit(p)
    /* } else if cur.kind == .Ident && is_symbol(next_token(p), "{") && !p.ignore_struct_lit {
        return parse_struct_lit(p) */ 
    } else if is_symbol(cur, "{") && !p.ignore_struct_lit { // `{...}`: struct literal whose type comes from context
        return parse_struct_lit(p)
    } else if is_symbol(cur, "[") {
        return parse_array_lit(p)
    } else if is_symbol(cur, "{") {
        highlight_lines(get_ctx().current_file, cur.span)
        gala_panic("`{}` zero-initialiser isn't implemented yet.")
    } else if cur.kind == .String {
        s := consume_token(p);
        id := new_expr(String{s=s.text})
        get_ctx().spans.exprs[id] = {
            file_name=get_ctx().current_file,
            span=s.span
        }
        return id;
    }
    return parse_binop(p, 0)
}

parse_compiler_op :: proc(p: ^Parser) -> ExprId {
    if current_token(p).kind == .Cast {
        token := consume_token(p); // "cast"
        expect_symbol(p, "(");
        ty := parse_type(p);
        expect_symbol(p, ")");
        expr := parse_compiler_op(p);
        id := new_expr(Expr(Cast{ty, expr}));
        get_ctx().spans.exprs[id] = {
            file_name=get_ctx().current_file,
            span={token.span.start, get_span(expr).span.end},
        }
        return id
    }else if current_token(p).kind == .Transmute {
        token := consume_token(p); // "transmute"
        expect_symbol(p, "(");
        ty := parse_type(p);
        expect_symbol(p, ")");
        expr := parse_compiler_op(p);
        id := new_expr(Expr(Transmute{ty, expr}));
        get_ctx().spans.exprs[id] = {
            file_name=get_ctx().current_file,
            span={token.span.start, get_span(expr).span.end},
        }
        return id
    }
    return parse_unary(p);
}

parse_unary :: proc(p: ^Parser) -> ExprId {
    t := current_token(p)
    if is_symbol(t, "&") || is_symbol(t, "!") || is_symbol(t, "-") {
        consume_token(p)
        expr := parse_compiler_op(p) // so `-cast(T) x` works
        id: ExprId
        switch t.text {
        case "&": id = new_expr(Expr(Reference{expr}))
        case "!": id = new_expr(Expr(UnNot{expr}))
        case:     id = new_expr(Expr(UnNegative{expr}))
        }
        get_ctx().spans.exprs[id] = {
            file_name=get_ctx().current_file,
            span={t.span.start, get_span(expr).span.end},
        }
        return id
    }
    if is_symbol(t, "+") { // no-op
        consume_token(p)
        return parse_compiler_op(p)
    }
    return parse_postfix(p);
}

parse_binop :: proc(p: ^Parser, min_prec: int) -> ExprId {
    lhs := parse_compiler_op(p)

    for {
        op := current_token(p)
        prec := op_precedence(op)
        if prec < min_prec { break }

        consume_token(p)

        next_min := prec + (0 if op_is_right_assoc(op) else 1)
        rhs := parse_binop(p, next_min)

        kind, _ := op_kind(op)
        prev := lhs
        lhs = new_expr(Expr(Binop{
            kind  = kind,
            left  = lhs,
            right = rhs,
        }))
        // start at lhs start and end at rhs end
        get_ctx().spans.exprs[lhs] = {
            file_name=get_ctx().current_file,
            span={
                start=get_span(prev).span.start,
                end=get_span(rhs).span.end,
            }
        }
    }

    return lhs
}
consume_token :: proc(p: ^Parser) -> Token {
    if p.i < len(p.tokens) {
        t := p.tokens[p.i];
        p.i += 1;
        return t;
    } else {
        return Token{kind=.EOF}
    }
}
current_token :: proc(p: ^Parser) -> Token {
    if p.i < len(p.tokens) {
        return p.tokens[p.i];
    } else {
        return Token{kind=.EOF}
    }
}
// n tokens ahead of the current one (0 = current)
peek_token :: proc(p: ^Parser, n: int) -> Token {
    i := p.i + n
    if i >= 0 && i < len(p.tokens) {
        return p.tokens[i]
    }
    return Token{kind=.EOF}
}

next_token :: proc(p: ^Parser) -> Token {
    return peek_token(p, 1)
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
// span of the most recently consumed token (used to find the end of a block)
prev_token_span :: proc(p: ^Parser) -> Span {
    if p.i > 0 && p.i - 1 < len(p.tokens) {
        return p.tokens[p.i - 1].span
    }
    return Span{}
}
is_symbol :: proc(t: Token, s: string) -> bool {
    if t.kind == .Symbol && t.text == s { return true }
    return false;
}
parse_stmt :: proc(p: ^Parser) -> StmtId {
    if current_token(p).kind == .Ident &&
        is_symbol(next_token(p), ":=") {
        name := consume_token(p)
        consume_token(p);
        expr := parse_expr(p);
        end := expect_symbol(p, ";");
        id := new_stmt(Stmt(VarDec{name=name.text, type=nil, value=expr}));
        get_ctx().spans.stmts[id] = {
            file_name=get_ctx().current_file,
            span={name.span.start, end.span.end},
        }
        return id
    } else if current_token(p).kind == .Ident &&
        is_symbol(next_token(p), ":") {
        name := consume_token(p);
        consume_token(p); // ":"
        ty := parse_type(p);
        expect_symbol(p, "=");
        expr := parse_expr(p);
        end := expect_symbol(p, ";");
        id := new_stmt(Stmt(VarDec{name=name.text, type=ty, value=expr}));
        get_ctx().spans.stmts[id] = {
            file_name=get_ctx().current_file,
            span={name.span.start, end.span.end},
        }
        return id
    } else if is_kw(current_token(p), .Return) {
        token := consume_token(p); // "return";

        if is_symbol(current_token(p), ";") {
            end_semi := consume_token(p); // ";"
            id := new_stmt(Stmt(Return{expr=nil}));
            get_ctx().spans.stmts[id] = {
                file_name=get_ctx().current_file,
                span={
                    start=token.span.start,
                    end=end_semi.span.end
                }
            }
            return id
        }
        e := parse_expr(p);
        end_semi := expect_symbol(p, ";");
        id := new_stmt(Stmt(Return{expr=e}));
        get_ctx().spans.stmts[id] = {
            file_name=get_ctx().current_file,
            span={
                start=token.span.start,
                end=end_semi.span.end
            }
        }
        return id
    } else if is_kw(current_token(p), .If) {
        token := consume_token(p); // "if"
        s := IfElse{}
        s.base_con = parse_condition(p);
        s.base_block = parse_block(p);
        alts := make([dynamic]AltCon, allocator=get_ctx().allocator);
        for is_kw(current_token(p), .Else) && is_kw(next_token(p), .If) {
            consume_token(p); // else
            consume_token(p); // if
            cond := parse_condition(p);
            block := parse_block(p);
            append(&alts, AltCon{cond, block})
        }
        s.alt = alts[:]
        if is_kw(current_token(p), .Else) {
            consume_token(p); // else
            block := parse_block(p);
            s.else_block = block
            s.has_else_block = true
        }
        id := new_stmt(s);
        get_ctx().spans.stmts[id] = {
            file_name=get_ctx().current_file,
            span=token.span
        }
        return id;
    } else if is_kw(current_token(p), .While) {
        token := consume_token(p); // "while"
        cond := parse_condition(p);
        block := parse_block(p);
        id := new_stmt(WhileLoop{cond, block});
        get_ctx().spans.stmts[id] = {
            file_name=get_ctx().current_file,
            span=token.span
        }
        return id;
    // "for name in obj {..."
    } else if is_kw(current_token(p), .For) {
        token := consume_token(p); // "for"
        ident := expect_ident(p);
        if !is_kw(current_token(p), .In) {
            highlight_lines(get_ctx().current_file, current_token(p).span);
            gala_panic("Expected \"in\".");
        }
        consume_token(p); // "in"

        expr := parse_condition(p)
        block := parse_block(p);

        id := new_stmt(ForLoop{name=ident.text, expr=expr, block=block});
        get_ctx().spans.stmts[id] = {
            file_name=get_ctx().current_file,
            span=token.span
        }
        return id;
    } else if is_kw(current_token(p), .Break) {
        token := consume_token(p); // "break"
        expect_symbol(p, ";");
        id := new_stmt(BreakStmt{});
        get_ctx().spans.stmts[id] = {
            file_name=get_ctx().current_file,
            span=token.span
        }
        return id;
    } else if is_kw(current_token(p), .Continue) {
        token := consume_token(p); // "continue"
        expect_symbol(p, ";");
        id := new_stmt(ContinueStmt{});
        get_ctx().spans.stmts[id] = {
            file_name=get_ctx().current_file,
            span=token.span
        }
        return id;
    } else {
        expr := parse_expr(p);
        if is_symbol(current_token(p), "=") {
            consume_token(p); // "="
            v := parse_expr(p);
            end := expect_symbol(p, ";");
            id := new_stmt(Stmt(Assignment{target=expr, value=v}));
            get_ctx().spans.stmts[id] = {
                file_name=get_ctx().current_file,
                span={
                    start=get_ctx().spans.exprs[expr].span.start,
                    end=end.span.end
                }
            }
            return id;
        } else if kind, is_compound := compound_op_kind(current_token(p)); is_compound {
            consume_token(p);

            v := parse_expr(p);
            end := expect_symbol(p, ";");

            value := new_expr(Binop{kind=kind, left=expr, right=v})
            get_ctx().spans.exprs[value] = {
                file_name=get_ctx().current_file,
                span={
                    start=get_span(expr).span.start,
                    end=get_span(v).span.end,
                }
            };
            id := new_stmt(Stmt(Assignment{
                target=expr,
                value=value,
                kind=kind,
                compound=true,
            }));

            get_ctx().spans.stmts[id] = {
                file_name=get_ctx().current_file,
                span={
                    start=get_ctx().spans.exprs[expr].span.start,
                    end=end.span.end,
                }
            }

            return id;
        }

        expect_symbol(p, ";");
        id := new_stmt(Stmt(ExprId(expr)));
        get_ctx().spans.stmts[id] = {
            file_name=get_ctx().current_file,
            span=get_ctx().spans.exprs[expr].span
        }
        return id;
    }
}
is_current_kw :: proc(p: ^Parser, k: Keyword) -> bool {
    if current_token(p).kind == .Keyword && current_token(p).kw == k do return true
    return false
}
is_kw :: proc(t: Token, k: Keyword) -> bool {
    if t.kind == .Keyword && t.kw == k do return true
    return false
}

// "=>" may be lexed as one symbol, or as "=" followed by ">"; accept both.
is_fat_arrow :: proc(p: ^Parser) -> bool {
    if is_symbol(current_token(p), "=>") do return true
    return is_symbol(current_token(p), "=") && is_symbol(next_token(p), ">")
}
consume_fat_arrow :: proc(p: ^Parser) {
    if is_symbol(current_token(p), "=>") {
        consume_token(p) // "=>"
    } else {
        consume_token(p) // "="
        consume_token(p) // ">"
    }
}

// `a` or `a, b, c` (the names of a `a, b: type` group). The caller expects
// the ":" afterwards.
parse_ident_group :: proc(p: ^Parser) -> [dynamic]Token {
    names := make([dynamic]Token, allocator=get_ctx().allocator)
    append(&names, expect_ident(p))
    for is_symbol(current_token(p), ",") {
        consume_token(p) // ","
        append(&names, expect_ident(p))
    }
    return names
}
// fn(args): ret { ... }      block body
// fn(args): ret => expr      expression body (desugared to `{ return expr; }`)
parse_fn_lit :: proc(p: ^Parser) -> ExprId {
    kw := consume_token(p) // "fn"

    lit := FnLit{}
    lit.signature = parse_args_dec(p)

    // the body is a fresh context: struct literals are fine in here even if
    // the lambda sits inside an `if`/`while` condition.
    prev_ignore_struct_lit := p.ignore_struct_lit
    p.ignore_struct_lit = false
    defer p.ignore_struct_lit = prev_ignore_struct_lit

    end_pos: int
    if is_fat_arrow(p) {
        consume_fat_arrow(p)
        e := parse_expr(p)
        end_pos = get_span(e).span.end

        ret_id := new_stmt(Stmt(Return{expr=e}))
        get_ctx().spans.stmts[ret_id] = {
            file_name=get_ctx().current_file,
            span=get_span(e).span,
        }
        stmts := make([dynamic]StmtId, allocator=get_ctx().allocator)
        append(&stmts, ret_id)
        lit.block = Block{stmts=stmts[:]}
    } else if is_symbol(current_token(p), "{") {
        lit.block = parse_block(p)
        end_pos = prev_token_span(p).end // the closing "}"
    } else {
        highlight_lines(get_ctx().current_file, current_token(p).span)
        gala_panic("Expected '{' or '=>' after function literal signature.")
    }

    lit.span = {start=kw.span.start, end=end_pos}

    id := new_expr(Expr(lit))
    get_ctx().spans.exprs[id] = {
        file_name=get_ctx().current_file,
        span=lit.span,
    }
    return id
}

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
    t := turbofish_ahead(p)
    // fmt.println("turbo fish:",  t);
    for i in 0..<t {
        _ = consume_token(p).text;
        // fmt.println(i, _text) // "::" "<"
    }

    list := make([dynamic]TypeSpecifier, allocator=get_ctx().allocator)
    for !is_symbol(current_token(p), ">") {
        append(&list, parse_type(p))
        if is_symbol(current_token(p), ",") {
            consume_token(p) // ","
        } else {
            break
        }
    }
    close := current_token(p)
    if len(list) == 0 {
        highlight_lines(get_ctx().current_file, close.span)
        gala_panic("Expected at least one type argument.")
    }
    expect_symbol(p, ">")
    return list[:], Span{start=start, end=close.span.end}
}

// `::<K, V>` in `fn name::<K, V>(..)`. Returns nil if there is none.
parse_generic_params :: proc(p: ^Parser) -> []GenericParam {
    skip := turbofish_ahead(p)
    if skip == 0 do return nil
    for _ in 0..<skip do consume_token(p) // "::" "<"

    list := make([dynamic]GenericParam, allocator=get_ctx().allocator)
    for !is_symbol(current_token(p), ">") {
        expect_symbol(p, "$"); // "define" type ig? make it clearer/more consistant
        n := expect_ident(p)
        append(&list, GenericParam{name=n.text, span=n.span})
        if is_symbol(current_token(p), ",") {
            consume_token(p) // ","
        } else {
            break
        }
    }
    if len(list) == 0 {
        highlight_lines(get_ctx().current_file, current_token(p).span)
        gala_panic("Expected at least one type parameter name.")
    }
    expect_symbol(p, ">")
    return list[:]
}

parse_call :: proc(p: ^Parser, target: ExprId,
        type_args: []TypeSpecifier = nil, type_args_span: Span = {}) -> ExprId {
    consume_token(p) // "("
    args := make([dynamic]FnCallArg, allocator=get_ctx().allocator)
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
            highlight_lines(get_ctx().current_file, current_token(p).span)
            gala_panic("Positional arguments can't come after named arguments.")
        }
        arg.expr = parse_nested_expr(p)
        append(&args, arg)
        if is_symbol(current_token(p), ",") {
            consume_token(p)
        } else do break
    }
    end := expect_symbol(p, ")")
    id := new_expr(FnCall{
        target=target, args=args,
        type_args=type_args, type_args_span=type_args_span,
    })
    get_ctx().spans.exprs[id] = {
        file_name=get_ctx().current_file,
        span={start=get_span(target).span.start, end=end.span.end},
    }
    return id
}

// `t[i]`, `t[a:b]`, `t[:b]`, `t[a:]`, `t[:]`
parse_index_or_slice :: proc(p: ^Parser, t: ExprId) -> ExprId {
    consume_token(p) // "["
    slice := TakeSlice{target=t, empty_start=true, empty_end=true}
    index: ExprId
    if !is_symbol(current_token(p), ":") {
        index = parse_nested_expr(p)
        slice.start = index
        slice.empty_start = false
    }
    is_slice := is_symbol(current_token(p), ":")
    if is_slice {
        consume_token(p) // ":"
        if !is_symbol(current_token(p), "]") {
            slice.end = parse_nested_expr(p)
            slice.empty_end = false
        }
    }
    end := expect_symbol(p, "]") // errors on `a[1 2`

    id: ExprId
    if is_slice {
        id = new_expr(slice)
    } else {
        id = new_expr(Index{target=t, index=index})
    }
    get_ctx().spans.exprs[id] = {
        file_name=get_ctx().current_file,
        span={start=get_span(t).span.start, end=end.span.end},
    }
    return id
}

parse_postfix :: proc(p: ^Parser) -> ExprId {
    t := parse_primary(p);
    for {
        if turbofish_ahead(p) > 0 {
            // `f::<A, B>(..)`: the type args must be followed by a call
            targs, tspan := parse_type_args(p)
            if !is_symbol(current_token(p), "(") {
                highlight_lines(get_ctx().current_file, current_token(p).span)
                gala_panic("Expected \"(\" after type arguments.")
            }
            t = parse_call(p, t, targs, tspan)
        } else if is_symbol(current_token(p), "(") {
            t = parse_call(p, t)
        } else if is_symbol(current_token(p), ".") {
            consume_token(p); // "."
            ident := expect_ident(p);
            id := new_expr(FieldAccess{target=t, field=ident.text});
            get_ctx().spans.exprs[id] = {
                file_name=get_ctx().current_file,
                span={start=get_span(t).span.start, end=ident.span.end}
            }
            t = id
        } else if is_symbol(current_token(p), "[") {
            t = parse_index_or_slice(p, t)
        } else if is_symbol(current_token(p), "^") {
            token := consume_token(p); // "^"
            id := new_expr(Deref{t});
            get_ctx().spans.exprs[id] = {
                file_name=get_ctx().current_file,
                span={get_span(t).span.start, token.span.end}
            }
            t = id
        } else do break
    }
    return t;
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
parse_primary :: proc(p: ^Parser) -> ExprId {
    if is_kw(current_token(p), .Fn) {
        return parse_fn_lit(p)
    } else if current_token(p).kind == .Ident {
        token := consume_token(p)
        e :=  Expr(Symbol{token.text});
        id := new_expr(e)
        get_ctx().spans.exprs[id] = {
            file_name=get_ctx().current_file,
            span=token.span
        }
        return id
    } else if current_token(p).kind == .Number {
        token := consume_token(p)
        id := new_expr(Expr(Number{token.text}));
        get_ctx().spans.exprs[id] = {
            file_name=get_ctx().current_file,
            span=token.span
        }
        return id
    } else if is_symbol(current_token(p), "(") {
        token := consume_token(p); // "("
        e := parse_nested_expr(p);
        end := expect_symbol(p, ")");
        // the parens' span replaces the inner span (handy for binop spans)
        get_ctx().spans.exprs[e] = {
            file_name=get_ctx().current_file,
            span={token.span.start, end.span.end}
        }
        return e
    } else if current_token(p).kind == .Len {
        token := consume_token(p);// "len"
        expect_symbol(p, "(");
        e := parse_expr(p);
        close := expect_symbol(p, ")");
        id := new_expr(Len{e});
        get_ctx().spans.exprs[id] = {
            file_name=get_ctx().current_file,
            span={token.span.start, close.span.end}
        }
        return id
    } else if current_token(p).kind == .TypeIdOf {
        token := consume_token(p); // "type_id"
        expect_symbol(p, "(");
        node := TypeIdOf{}
        node.t, node.expr = parse_type_or_expr(p)
        end := expect_symbol(p, ")");
        id := new_expr(Expr(node));
        get_ctx().spans.exprs[id] = {
            file_name=get_ctx().current_file,
            span={token.span.start, end.span.end},
        }
        return id
    } else if current_token(p).kind == .TypeInfoOf {
        token := consume_token(p); // "type_info"
        expect_symbol(p, "(");
        e := parse_expr(p);
        id := new_expr(Expr(TypeInfoOf{e}));
        end := expect_symbol(p, ")");
        get_ctx().spans.exprs[id] = {
            file_name=get_ctx().current_file,
            span={token.span.start, end.span.end},
        }
        return id
    } else if current_token(p).kind == .SizeOf {
        token := consume_token(p);// "size_of"
        expect_symbol(p, "(");
        t, ex := parse_type_or_expr(p)
        close := expect_symbol(p, ")");
        id := new_expr(SizeOf{t = t, expr = ex});
        get_ctx().spans.exprs[id] = {
            file_name=get_ctx().current_file,
            span={token.span.start, close.span.end}
        }
        return id
    } else if current_token(p).kind == .True {
        token := consume_token(p);
        id := new_expr(BoolLitTrue{});
        get_ctx().spans.exprs[id] = {
            file_name=get_ctx().current_file,
            span=token.span
        }
        return id
    } else if current_token(p).kind == .False {
        token := consume_token(p);
        id := new_expr(BoolLitFalse{});
        get_ctx().spans.exprs[id] = {
            file_name=get_ctx().current_file,
            span=token.span
        }
        return id
    }
    highlight_lines(get_ctx().current_file, current_token(p).span);
    gala_panic("invalid primary token")
}
parse_block :: proc(p: ^Parser) -> Block{
    expect_symbol(p, "{");
    prev := p.ignore_struct_lit;
    p.ignore_struct_lit = false;
    stmts := make([dynamic]StmtId, allocator=get_ctx().allocator);
    for !(current_token(p).kind == .Symbol && current_token(p).text == "}") &&
        (current_token(p).kind != .EOF) {
        stmt := parse_stmt(p);
        append(&stmts, stmt)
    }
    expect_symbol(p, "}");
    p.ignore_struct_lit = prev ;
    return Block{stmts=stmts[:]}
}

// A parameter is `name: type`, `name: type = default` or `name := default`
// (type inferred from the default, like a local `x := ...`).
//
// `default` is never the written expression itself: it is moved into a hidden
// function and `default` is a call to it (see make_default_thunk). For the
// inferred form `inferred` is set, `t` is a placeholder and `default_src` is
// the written expression, so the type checker can infer the type from it.

base_span :: proc(t: ^TypeSpecifier) -> Span {
    switch t in t {
    case UnionSpecifier: return t.span;
    case StructSpecifier: return t.span;
    case BaseType: return t.span;
    case PointerType: return t.span;
    case SliceSpecifier: return t.span;
    case FixedArreySpecifier: return t.span;
    case AnySpecifier: return t.span;
    case FnSpecifier: return t.span;
    case GenericSpecifier: return t.span;
    case GenericInstSpecifier: return t.span;
    }
    panic("impl")
}
// The size inside `[N]T` / `[N]T{...}`; the "[" is already consumed, the
// "]" is left for the caller. A plain number literal stays a plain int,
// anything else is parsed as an expression and folded by the resolver.
parse_array_size :: proc(p: ^Parser) -> (size: int, expr: Maybe(ExprId)) {
    n := current_token(p)
    if n.kind == .Number && is_symbol(next_token(p), "]") {
        consume_token(p)
        size64, ok := parse_integer_literal(n.text)
        if !ok {
            highlight_lines(get_ctx().current_file, n.span)
            gala_panic("Invalid array size.")
        }
        return int(size64), nil
    }
    return 0, parse_binop(p, 0)
}

// parses `{ a, b: T, c: U }`; returns the fields and the closing brace token
parse_struct_body :: proc(p: ^Parser) -> (fields: []StructField, close: Token) {
    expect_symbol(p, "{");
    out := make([dynamic]StructField, allocator=get_ctx().allocator);
    for !is_symbol(current_token(p), "}") {
        names := parse_ident_group(p) // `a, b: type`
        expect_symbol(p, ":");
        ty := parse_type(p);

        // all names share the type; only the first may declare a `$T`
        for n, i in names {
            field_ty := ty
            if g, is_generic := ty.(GenericSpecifier); is_generic && i > 0 {
                field_ty = TypeSpecifier(BaseType{ident=g.name, span=g.span})
            }
            append(&out, StructField{name=n.text, t=field_ty, span=n.span});
        }
        if is_symbol(current_token(p), ",") {
            consume_token(p); // ","
        } else do break
    }
    close = current_token(p);
    expect_symbol(p, "}");
    return out[:], close;
}

parse_type :: proc(p: ^Parser) -> TypeSpecifier {
    if is_symbol(current_token(p), "$") {
        g := consume_token(p);
        n := expect_ident(p);
        return TypeSpecifier(GenericSpecifier{n.text, {g.span.start, n.span.end}});
    }
    if current_token(p).kind == .Ident {
        token := consume_token(p)
        if is_symbol(current_token(p), "(") { // Name(args): generic struct instance
            consume_token(p) // "("
            if is_symbol(current_token(p), ")") {
                highlight_lines(get_ctx().current_file, current_token(p).span)
                gala_panic("Expected at least one generic argument.")
            }
            args := make([dynamic]TypeSpecifier, allocator=get_ctx().allocator)
            for {
                append(&args, parse_type(p))
                if !is_symbol(current_token(p), ",") do break
                consume_token(p) // ","
            }
            close := current_token(p)
            expect_symbol(p, ")")
            return TypeSpecifier(GenericInstSpecifier{
                name=token.text, args=args,
                span={start=token.span.start, end=close.span.end},
            })
        }
        return TypeSpecifier(BaseType({token.text, token.span}));
    }
    if is_kw(current_token(p), .Fn) {
        token:=consume_token(p); // "fn"

        spec := new(FnDecSignature, get_ctx().allocator);
        spec^ = parse_args_dec(p);
        return FnSpecifier{ span = token.span, signature = spec };

    }
    if is_symbol(current_token(p), "[") {
        token := consume_token(p); // "["
        if is_symbol(current_token(p), "]") { // slice
            consume_token(p); // "]"
            base_specifier := new(TypeSpecifier, allocator=get_ctx().allocator)
            base_specifier^ = parse_type(p);

            return TypeSpecifier(SliceSpecifier{
                base=base_specifier,
                span={
                    start=token.span.start,
                    end=base_span(base_specifier).end
                }
            })
        }
        size, size_expr := parse_array_size(p)
        expect_symbol(p, "]");

        base_specifier := new(TypeSpecifier, allocator=get_ctx().allocator);
        base_specifier^ = parse_type(p);

        return TypeSpecifier(FixedArreySpecifier{
            size=size, size_expr=size_expr, base=base_specifier,
            span={
                start=token.span.start,
                end=base_span(base_specifier).end
            }
        })
    }
    if is_kw(current_token(p), .Struct) {
        kw := consume_token(p); // "struct"
        fields, close := parse_struct_body(p);
        return TypeSpecifier(StructSpecifier{
            fields=fields,
            span={start=kw.span.start, end=close.span.end},
        });
    }
    if is_kw(current_token(p), .Union) {
        kw := consume_token(p) // "union"
        fields, close := parse_struct_body(p)
        return TypeSpecifier(UnionSpecifier{
            fields=fields,
            span={start=kw.span.start, end=close.span.end},
        })
    }
    if is_symbol(current_token(p), "^") {
        token := consume_token(p); // "^"
        base_specifier := new(TypeSpecifier, allocator=get_ctx().allocator);
        base_specifier^ = parse_type(p);
        return TypeSpecifier(PointerType{
            ptr=base_specifier,
            span={start=token.span.start, end=base_span(base_specifier).end},
        })
    } else if current_token(p).kind == .Any {
        t := consume_token(p); // "any"
        return AnySpecifier{span=t.span};
    }
    highlight_lines(get_ctx().current_file, current_token(p).span)
    gala_panic("Invalid token in type specifier.");
}

// Unique across the whole compiler run, not per file: codegen binds items
// into its scope by *name*, and an output file sees the items of every
// module it imports, so two modules must never produce the same thunk name.
default_thunk_counter: int

// A parameter default can be any expression, so it can't be re-evaluated at
// the call site (it might name a global a caller's local shadows, or contain
// a function literal). Instead it is moved into a hidden top-level function
//
//     fn __default_N(): <param type> { return <expr>; }
//
// and the parameter's default becomes the call `__default_N()`. The default
// is re-evaluated on every call that omits it. `ty` is nil for `name := expr`
// parameters: the type checker fills the return type in.
//
// The thunk item is queued in p.extra_items; parse_tokens inserts it ahead of
// the item that contains the signature.
make_default_thunk :: proc(p: ^Parser, ty: Maybe(TypeSpecifier), expr: ExprId) -> ExprId {
    span := get_span(expr).span
    file := get_ctx().current_file

    name := fmt.aprintf("__default_%d", default_thunk_counter, allocator=get_ctx().allocator)
    default_thunk_counter += 1

    // body: `return <expr>;`
    ret_id := new_stmt(Stmt(Return{expr=expr}))
    get_ctx().spans.stmts[ret_id] = {file_name=file, span=span}
    stmts := make([dynamic]StmtId, allocator=get_ctx().allocator)
    append(&stmts, ret_id)

    f := FnDec{}
    f.name = name
    f.ret_ty = ty
    f.span = span
    f.block = Block{stmts=stmts[:]}
    item_id := new_item(Item(f))
    get_ctx().spans.items[item_id] = {file_name=file, span=span}
    append(&p.extra_items, item_id)

    // the new default: `__default_N()`
    sym := new_expr(Expr(Symbol{name}))
    get_ctx().spans.exprs[sym] = {file_name=file, span=span}

    call := new_expr(FnCall{target=sym, args=make([dynamic]FnCallArg, allocator=get_ctx().allocator)})
    get_ctx().spans.exprs[call] = {file_name=file, span=span}
    return call
}

// Once a parameter has a default every parameter after it must too, and
// defaults can't be combined with a variadic tail. `any` parameters can't have
// a default (the hidden function would have to return `any`). `a, b: type`
// groups can't have defaults or be variadic.
parse_args_dec :: proc(p: ^Parser) -> FnDecSignature {
    f := FnDecSignature{};
    args := make([dynamic]FnDecArg, allocator=get_ctx().allocator)
    seen_default := false
    expect_symbol(p, "(");
    for !is_symbol(current_token(p), ")") {
        names := parse_ident_group(p)
        name := names[len(names) - 1]
        grouped := len(names) > 1

        ty: TypeSpecifier
        written: ExprId
        has_written := false
        inferred := false

        if is_symbol(current_token(p), ":=") {
            // `name := expr`
            if grouped {
                highlight_lines(get_ctx().current_file, current_token(p).span)
                gala_panic("Default values can't be used with grouped parameters (`a, b: type`).")
            }
            consume_token(p); // ":="
            inferred = true
            ty = TypeSpecifier(BaseType{ident="", span=name.span}) // placeholder
            written = parse_nested_expr(p)
            has_written = true
        } else {
            expect_symbol(p, ":")
            if is_symbol(current_token(p), ".") &&
                is_symbol(next_token(p), ".") {
                if grouped {
                    highlight_lines(get_ctx().current_file, name.span)
                    gala_panic("A variadic parameter can't be part of a grouped declaration.")
                }
                consume_token(p); // "."
                consume_token(p); // "."
                t := parse_type(p);
                f.is_variadic = true;
                f.variadic_ty = t;
                f.variadic_arg_name = name.text;
                f.variadic_arg_span = name.span;
                break;
            }
            ty = parse_type(p);

            // `name: type = expr`
            if is_symbol(current_token(p), "=") {
                if grouped {
                    highlight_lines(get_ctx().current_file, current_token(p).span)
                    gala_panic("Default values can't be used with grouped parameters (`a, b: type`).")
                }
                consume_token(p); // "="

                if _, is_any := ty.(AnySpecifier); is_any {
                    highlight_lines(get_ctx().current_file, name.span)
                    gala_panic("Default values aren't supported for `any` parameters.")
                }
                written = parse_nested_expr(p)
                has_written = true
            }
        }

        default: Maybe(ExprId) = nil
        default_src: Maybe(ExprId) = nil
        if has_written {
            thunk_ret: Maybe(TypeSpecifier) // stays nil when inferred
            if !inferred do thunk_ret = ty
            default = make_default_thunk(p, thunk_ret, written)
            if inferred do default_src = written
            seen_default = true
        } else if seen_default {
            highlight_lines(get_ctx().current_file, names[0].span)
            gala_panic("A parameter without a default value can't come after one with a default.")
        }

        // all names share the type; only the first may declare a `$T`
        for n, i in names {
            arg_ty := ty
            if g, is_generic := ty.(GenericSpecifier); is_generic && i > 0 {
                arg_ty = TypeSpecifier(BaseType{ident=g.name, span=g.span})
            }
            append(&args, FnDecArg{
                name=n.text, t=arg_ty, span=n.span,
                default=default, inferred=inferred, default_src=default_src,
            })
        }
        if is_symbol(current_token(p), ",") {
            consume_token(p);
        } else {
            break;
        }
    }
    end := expect_symbol(p, ")");
    if f.is_variadic && seen_default {
        highlight_lines(get_ctx().current_file, end.span)
        gala_panic("Default parameter values can't be combined with a variadic parameter.")
    }
    f.args = args[:]
    if is_symbol(current_token(p), ":") {
        consume_token(p); // ":"
        f.ret_ty = parse_type(p);
    }
    return f;
}

// `fn name(..)` or `fn name::<K, V>(..)`
parse_fn_signature :: proc(p: ^Parser) -> FnDec {
    kw := consume_token(p); // "fn"
    if !(kw.kind == .Keyword && kw.kw == .Fn) {
        highlight_lines(get_ctx().current_file, kw.span)
        gala_panic("Expected \"fn\".");
    }
    name := expect_ident(p);
    generics := parse_generic_params(p)
    f := FnDec{};
    f.signature = parse_args_dec(p);
    f.name = name.text;
    f.generics = generics // after the signature: it replaces the whole struct

    f.span.start = kw.span.start
    f.span.end = current_token(p).span.end;
    return f;
}
// Top-level `name := expr;`, `name: type = expr;`, `name :: expr;` or `name : type : expr;`
parse_global_var_dec :: proc(p: ^Parser) -> ItemId {
    name := expect_ident(p)

    ty: Maybe(TypeSpecifier) = nil
    is_const := false
    if is_symbol(current_token(p), ":=") {
        consume_token(p) // ":="
    } else if is_symbol(current_token(p), "::") {
        consume_token(p) // "::"
        is_const = true
    } else if is_symbol(current_token(p), ":") {
        consume_token(p) // ":"
        if is_symbol(current_token(p), ":") { // `::` lexed as two tokens
            consume_token(p)
            is_const = true
        } else {
            ty = parse_type(p)
            if is_symbol(current_token(p), ":") { // `name : T : value`
                consume_token(p)
                is_const = true
            } else {
                expect_symbol(p, "=")
            }
        }
    } else {
        highlight_lines(get_ctx().current_file, current_token(p).span)
        gala_panic("Expected \":=\", \":\" or \"::\" after a top-level name (only declarations are allowed at the top level).")
    }

    if is_const && is_kw(current_token(p), .Struct) {
        highlight_lines(get_ctx().current_file, current_token(p).span)
        gala_panic("`::` only declares constant values for now (use `struct Name {...}` for structs).")
    }

    value := parse_expr(p)
    end := expect_symbol(p, ";")

    item: Item
    if is_const {
        item = ConstDec{name=name.text, type=ty, value=value}
    } else {
        item = GlobalVarDec{name=name.text, type=ty, value=value}
    }
    id := new_item(item)
    get_ctx().spans.items[id] = {
        file_name=get_ctx().current_file,
        span={name.span.start, end.span.end},
    }
    return id
}
parse_module_kw :: proc(p: ^Parser) -> ItemId {
    #partial switch current_token(p).kw {
    case .Alias: {
        t := consume_token(p);
        new_name := expect_ident(p);
        expect_symbol(p, "=");
        ty := parse_type(p);
        e := expect_symbol(p, ";");
        id := new_item(TypeAlias{new_name.text, ty})
        get_ctx().spans.items[id] = {
            file_name=get_ctx().current_file,
            span={t.span.start, e.span.end},
        }

        return id
    }
    case .Struct: {
        consume_token(p); // "struct"
        name := expect_ident(p);
        fields, _ := parse_struct_body(p);
        sd := StructDec{
            name=name.text,
            fields=fields,
        }
        id := new_item(sd)
        get_ctx().spans.items[id] = {
            file_name=get_ctx().current_file,
            span=name.span
        }

        return id
    }
    case .Fn: {
        f := parse_fn_signature(p);
        b := parse_block(p);
        f.block = b;

        id := new_item(Item(f))
        get_ctx().spans.items[id] = {
            file_name=get_ctx().current_file,
            span=f.span
        }

        return id
    }
    case .Export: {
        consume_token(p);
        f := parse_fn_signature(p);
        b := parse_block(p);
        f.block = b;
        f.export_name = true;

        id := new_item(Item(f))
        get_ctx().spans.items[id] = {
            file_name=get_ctx().current_file,
            span=f.span
        }

        return id
    }
    case .Extern: {
        consume_token(p); // "extern"
        f := parse_fn_signature(p);
        ef := ExternFnDec{}
        ef.name = f.name;
        ef.args = f.args;
        ef.ret_ty = f.ret_ty
        ef.signature = f.signature
        ef.span = f.span

        expect_symbol(p, ";");

        id := new_item(Item(ef))
        get_ctx().spans.items[id] = {
            file_name=get_ctx().current_file,
            span=ef.span
        }
        return id;
    }
    case .Import: { // import "file.gala";
        current_name := get_ctx().current_file;
        token := consume_token(p); // "import"
        fname := consume_token(p); // file name?
        if fname.kind != .String {
            highlight_lines(get_ctx().current_file, fname.span)
            gala_panic("Expected string.");
        }
        debugln("IMPORTIN:", fname.text, "FROM", get_ctx().current_file);
        semi := expect_symbol(p, ";"); // ";"
        handle_file(fname.text);
        id := new_item(Item(Import{fname=fname.text}))
        get_ctx().spans.items[id] = {
            file_name=get_ctx().current_file,
            span={token.span.start, semi.span.end}
        }
        get_ctx().current_file = current_name
        return id;
    }
    case:
        highlight_lines(get_ctx().current_file, current_token(p).span)
        gala_panic("Unexpected keyword at top level.")
    }
}
expect_symbol :: proc(p: ^Parser, str: string) -> Token {
    c := current_token(p);
    if c.kind != .Symbol {
        highlight_lines(get_ctx().current_file, c.span);
        gala_panic("Expected symbol {}, got: {}", str, c);
    }
    if c.text != str {
        highlight_lines(get_ctx().current_file, c.span);
        gala_panic("Expected", str, "got:", c.text);
    }
    return consume_token(p)
}
expect_ident :: proc(p: ^Parser) -> Token {
    c := current_token(p);
    if c.kind != .Ident {
        highlight_lines(get_ctx().current_file, c.span)
        gala_panic("Expected ident got:", c);
    }
    return consume_token(p)
}

// `@link("./x.a")` / `@link("../x.a")` are relative to the file containing
// the directive. Anything else (absolute paths, bare names) goes to ld as is.
resolve_link_path :: proc(importer: string, path: string) -> string {
    if strings.has_prefix(path, "./") || strings.has_prefix(path, "../") {
        return resolve_import_path(importer, path)
    }
    return path
}

// ---- compile-time conditions ------------------------------------------
// cond := or ; or := and ("||" and)* ; and := not ("&&" not)*
// not  := "!" not | "(" cond ")" | WINDOWS | POSIX

comptime_name :: proc(t: Token) -> bool {
    is_windows := abi_resolve_kind(get_ctx().abi) == .Win64
    switch t.text {
    case "WINDOWS": return is_windows
    case "POSIX":   return !is_windows
    }
    highlight_lines(get_ctx().current_file, t.span)
    gala_panic("Unknown compile-time condition:", t.text, "(expected WINDOWS or POSIX)")
}

parse_comptime_not :: proc(p: ^Parser) -> bool {
    if is_symbol(current_token(p), "!") {
        consume_token(p)
        return !parse_comptime_not(p)
    }
    if is_symbol(current_token(p), "(") {
        consume_token(p)
        v := parse_comptime_cond(p)
        expect_symbol(p, ")")
        return v
    }
    return comptime_name(expect_ident(p))
}

parse_comptime_and :: proc(p: ^Parser) -> bool {
    v := parse_comptime_not(p)
    for is_symbol(current_token(p), "&&") {
        consume_token(p)
        r := parse_comptime_not(p) // no short circuit: always consume the tokens
        v = v && r
    }
    return v
}

parse_comptime_cond :: proc(p: ^Parser) -> bool {
    v := parse_comptime_and(p)
    for is_symbol(current_token(p), "||") {
        consume_token(p)
        r := parse_comptime_and(p)
        v = v || r
    }
    return v
}

// Skips a `{ ... }` token-wise. The skipped code is never parsed, so
// imports in it aren't loaded and it may not even be valid on this platform.
skip_braced_block :: proc(p: ^Parser) {
    open := expect_symbol(p, "{")
    depth := 1
    for depth > 0 {
        t := consume_token(p)
        if t.kind == .EOF {
            highlight_lines(get_ctx().current_file, open.span)
            gala_panic("Unterminated `{` in compile-time if.")
        }
        if is_symbol(t, "{") do depth += 1
        else if is_symbol(t, "}") do depth -= 1
    }
}

// `@if(cond) { items } else if (cond) { items } else { items }`
// The "@" is already consumed; the current token is `if`.
parse_comptime_if :: proc(p: ^Parser, items: ^[dynamic]ItemId) {
    consume_token(p) // "if"
    taken := false

    branch :: proc(p: ^Parser, items: ^[dynamic]ItemId, cond: bool, taken: ^bool) {
        if cond && !taken^ {
            taken^ = true
            expect_symbol(p, "{")
            parse_item_list(p, items, true)
            expect_symbol(p, "}")
        } else {
            skip_braced_block(p)
        }
    }

    expect_symbol(p, "(")
    cond := parse_comptime_cond(p)
    expect_symbol(p, ")")
    branch(p, items, cond, &taken)

    for is_kw(current_token(p), .Else) {
        consume_token(p) // "else"
        if is_kw(current_token(p), .If) {
            consume_token(p) // "if"
            expect_symbol(p, "(")
            c := parse_comptime_cond(p)
            expect_symbol(p, ")")
            branch(p, items, c, &taken)
        } else {
            branch(p, items, true, &taken)
            break
        }
    }
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
            highlight_lines(get_ctx().current_file, current_token(p).span)
            gala_panic("Expected a string for the @link path.")
        }
        s := consume_token(p)
        if active {
            append(&get_ctx().links, resolve_link_path(file_name, s.text))
        }
        if is_symbol(current_token(p), ",") {
            consume_token(p)
        } else do break
    }
    expect_symbol(p, ")")
}

// ---- top level ---------------------------------------------------------

AST :: struct {
    items: [dynamic]ItemId,
}

// Parses items until EOF, or (nested) until the `}` closing a compile-time block.
parse_item_list :: proc(p: ^Parser, items: ^[dynamic]ItemId, nested: bool) {
    for current_token(p).kind != .EOF {
        if nested && is_symbol(current_token(p), "}") do return

        id: ItemId
        #partial switch current_token(p).kind {
        case .Keyword: {
            id = parse_module_kw(p)
        }
        case .Ident: {
            id = parse_global_var_dec(p)
        }
        case:
            if is_symbol(current_token(p), "@") {
                consume_token(p) // "@"
                if is_kw(current_token(p), .If) {
                    parse_comptime_if(p, items)
                    continue
                } else if current_token(p).kind == .Link {
                    parse_link_directive(p, p.file)
                    continue
                } else {
                    highlight_lines(get_ctx().current_file, current_token(p).span)
                    gala_panic("Unexpected token. want compiler directive");
                }
            } else {
                highlight_lines(get_ctx().current_file, current_token(p).span)
                gala_panic("Expected a declaration, import or directive.")
            }
        }

        // hidden default-value functions go in first, ahead of their item
        for extra in p.extra_items {
            append(items, extra)
        }
        clear(&p.extra_items)

        append(items, id)
    }
    if nested {
        highlight_lines(get_ctx().current_file, prev_token_span(p))
        gala_panic("Expected \"}\" to close the compile-time block.")
    }
}

parse_tokens :: proc(file_name: string, tokens: []Token) -> AST {
    p := Parser{file=file_name, tokens=tokens}
    p.extra_items = make([dynamic]ItemId, allocator=get_ctx().allocator)
    items := make([dynamic]ItemId, allocator=get_ctx().allocator)
    parse_item_list(&p, &items, false)
    return AST{items=items}
}
