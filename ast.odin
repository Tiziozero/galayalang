package main

// ===========================================================================
// Expressions
// ===========================================================================

Expr :: union {
    AggregateLit,
    Binop,
    Number,
    Symbol,
    FnCall,
    GenericInst,
    FieldAccess,
    ImplicitSelector,
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
    BoolLitTrue,
    BoolLitFalse,
    FnLit,
}

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
}

Binop :: struct {
    kind:         BinopKind,
    left, right:  ExprId,
}

Number :: struct { text: string }
Symbol :: struct { name: string }
String :: struct { s: string }
BoolLit :: struct { text: string }
BoolLitTrue :: distinct struct {}
BoolLitFalse :: distinct struct {}

UnNegative :: struct { expr: ExprId }
UnNot :: struct { expr: ExprId }
Reference :: struct { expr: ExprId }
Deref :: struct { expr: ExprId }

Cast :: struct {
    to:     TypeSpecifier,
    target: ExprId,
}
Transmute :: struct {
    to:     TypeSpecifier,
    target: ExprId,
}

TypeIdOf :: struct {
    t:          Maybe(TypeSpecifier), // set when written with type-only syntax
    expr:       Maybe(ExprId),        // set otherwise
    names_type: bool,                 // resolver: `expr` was a bare type name
}
TypeInfoOf :: struct { expr: ExprId }
SizeOf :: struct {
    t:          Maybe(TypeSpecifier), // set when written with type-only syntax
    expr:       Maybe(ExprId),        // set otherwise
    names_type: bool,                 // resolver: `expr` was a bare type name
}
Len :: struct { target: ExprId }

Index :: struct {
    target, index: ExprId,
}
TakeSlice :: struct {
    target, start, end:    ExprId,
    empty_start, empty_end: bool,
}
// `Color.RED` parses as this too: the resolver turns it into an enum member
// access when `target` names an enum type.
FieldAccess :: struct {
    target: ExprId,
    field:  string,
}
// `.RED`: an enum member whose enum type comes from the context
// (`c: Color = .RED`, `c == .RED`, `f(.RED)`). The type checker resolves it
// against the expected type; with no expected type it's an error.
ImplicitSelector :: struct {
    field: string,
}

// `f::<A, B>(x)`: `type_args` are the explicit type arguments as written,
// the resolver fills `type_arg_ids`.
FnCall :: struct {
    target:         ExprId,
    args:           [dynamic]FnCallArg,
    // type_args:      []TypeSpecifier,
    // type_args_span: Span,
    // type_arg_ids:   []TypeId,
}
// `f::<A, B>`
GenericInst :: struct {
    target:         ExprId,
    args:           []TypeSpecifier,
    type_arg_ids:   []TypeId,
}
// `name` is set for `f(x = 1)` style arguments. The type checker reorders
// everything into parameter order and clears the names, so codegen only sees
// positional, full-arity calls.
FnCallArg :: struct {
    expr:         ExprId,
    needs_boxing: bool,
    box_type:     TypeId,
    name:         string,
    name_span:    Span,
}

FieldInit :: struct {
    expr: ExprId,
    span: Span,
}
AggregateLit :: struct {
    // `Foo` or `Foo(i64)`; resolve this to get the struct type, `name` is for messages
    ty:   Maybe(TypeSpecifier),
    fields: map[string]FieldInit,
    // `Foo{1, 2}` form, in source order. The resolver moves these into
    // `fields`; unused after that.
    positional: []FieldInit,
}

FixedSizeArray :: struct {
    size: int,
    // set when the size is a constant expression (`[MAX_COUNT]byte{}`);
    // the resolver folds it into `size` and clears this
    size_expr:   Maybe(ExprId),
    ty:          TypeSpecifier,
    initialiser: []ExprId,
}

// Anonymous function. Both forms produce this node:
//   fn(i: i32, j: i32): i32 { return i + j }
//   fn(i: i32, j: i32): i32 => i + j      (desugared to `{ return <expr>; }`)
FnLit :: struct {
    using signature: FnDecSignature,
    span:  Span,
    block: Block,
}

// ===========================================================================
// Type specifiers
// ===========================================================================

TypeSpecifier :: union {
    BaseType,
    PointerType,
    FixedArreySpecifier,
    SliceSpecifier,
    AnySpecifier,
    FnSpecifier,
    StructSpecifier,
    UnionSpecifier,
    EnumSpecifier,
    GenericSpecifier,
    GenericInstSpecifier,
}

BaseType :: struct { ident: string, span: Span }
PointerType :: struct { ptr: ^TypeSpecifier, span: Span }
FixedArreySpecifier :: struct {
    size:      int,
    size_expr: Maybe(ExprId),
    base:      ^TypeSpecifier,
    span:      Span,
}
SliceSpecifier :: struct { base: ^TypeSpecifier, span: Span }
AnySpecifier :: struct { span: Span }
GenericSpecifier :: struct { name: string, span: Span }
GenericInstSpecifier :: struct {
    name: string,
    args: [dynamic]TypeSpecifier,
    span: Span,
}
FnSpecifier :: struct {
    span: Span,
    using signature: ^FnDecSignature,
}
StructSpecifier :: struct {
    fields: []StructField,
    span:   Span,
}
UnionSpecifier :: struct {
    fields: []StructField,
    span:   Span,
}
// `enum { A, B = 5, C }` or `enum: u8 { A, B }`
EnumSpecifier :: struct {
    backing: ^TypeSpecifier, // nil unless written (`enum: u8 {..}`)
    members: []EnumMember,
    span:    Span,
}
// `value` is the expression as written; the resolver folds it (like a
// `::` constant). Members without one continue from the previous member.
EnumMember :: struct {
    name:  string,
    span:  Span,
    value: Maybe(ExprId),
}

// ===========================================================================
// Statements
// ===========================================================================

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

Block :: struct { stmts: []StmtId }

VarDec :: struct {
    name:  string,
    type:  Maybe(TypeSpecifier),
    value: ExprId,
}
Assignment :: struct {
    kind:         BinopKind,
    compound:     bool,
    target, value: ExprId,
}
Return :: struct { expr: Maybe(ExprId) }
BreakStmt :: struct { label: string }
ContinueStmt :: struct { label: string }

AltCon :: struct { cond: ExprId, block: Block }
IfElse :: struct {
    base_cond:       ExprId,
    base_block:     Block,
    alt:            []AltCon,
    has_else_block: bool,
    else_block:     Block,
}
WhileLoop :: struct { cond: ExprId, block: Block }
ForLoop :: struct { names: []struct{name: string, span: Span, oid: ObjId}, expr: ExprId, block: Block }

// ===========================================================================
// Functions
// ===========================================================================

FnDecParam :: struct {
    name: string,
    t:    TypeSpecifier,
    span: Span,
    default:  Maybe(ExprId), // the default value expression, as written
    inferred: bool,          // `name := expr`: `t` is a placeholder, the type comes from `default`
}
// A name declared in `fn f::<$K, $V>(..)`.
GenericParam :: struct {
    name: string,
    span: Span,
}
FnDecSignature :: struct {
    name:                string,
    params:              []FnDecParam,
    ret_ty:              Maybe(TypeSpecifier),
    is_variadic:         bool,
    variadic_ty:         TypeSpecifier,
    variadic_param_name: string,
    variadic_param_span: Span,
    // explicit type params, only on named fn items
    generics:            []GenericParam,
}

// ===========================================================================
// Top-level items
// ===========================================================================

Item :: union {
    Import,
    Forward,
    TypeAlias,
    StructDec,
    EnumDec,
    FnDec,
    ExternFnDec,
    GlobalVarDec,
    ConstDec,
}

StructField :: struct { name: string, t: TypeSpecifier, span: Span }
StructDec :: struct {
    name:     string,
    generics: []GenericParam, // explicit `struct Name($T, $G)`; nil when absent
    fields:   []StructField,
}
// `enum Name { A, B }` / `enum Name: u8 { A, B = 5 }`
EnumDec :: struct {
    name:    string,
    backing: ^TypeSpecifier, // nil unless written
    members: []EnumMember,
}
FnDec :: struct {
    using signature: FnDecSignature,
    span:        Span,
    block:       Block,
    export_name: bool,
}
ExternFnDec :: struct {
    using signature: FnDecSignature,
    span: Span,
}
GlobalVarDec :: struct {
    name:  string,
    type:  Maybe(TypeSpecifier),
    value: ExprId,
}
// `limit :: 100;` / `ratio : f32 : 0.5;`
// The resolver folds the value and replaces every use with a literal; the
// item is dropped afterwards, so later passes never see it.
ConstDec :: struct {
    name:  string,
    type:  Maybe(TypeSpecifier),
    value: ExprId,
}
Import :: struct { fname, alias: string, symbols: []string, imports_all: bool }
Forward :: struct { i: ItemId }
TypeAlias :: struct {
    new_name: string,
    ty:       TypeSpecifier,
}

AST :: struct { items: [dynamic]ItemId }
