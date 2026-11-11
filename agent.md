# Gala Language Guide for AI Agents

This document is a practical reference for AI coding agents working with
**Gala** source code. It describes the language using the syntax and
behaviour demonstrated by the supplied language tour, plus the `galac`
command-line interface.

The goal is to help an agent read, write, explain, and debug Gala code
without accidentally importing assumptions from C, Odin, Rust, or
another language.

## 1. Working rules for an AI agent

When generating or modifying Gala code:

1.  **Follow the syntax in this guide and in nearby project code.** Do
    not silently substitute syntax from another language.
2.  **Preserve exact types.** Gala is strictly typed; do not assume
    implicit numeric conversions or pointer conversions.
3.  **Use the compiler as the authority.** If behaviour is unclear, make
    a small test program and compile it with `galac`.
4.  **Do not invent unsupported features.** In particular, do not assume
    that a feature exists just because it is common in another language.
5.  **Keep changes local.** Prefer the smallest change that fits the
    existing codebase and its conventions.
6.  **When a compiler error occurs, diagnose the actual Gala type or
    syntax involved.** Do not immediately rewrite the code into a
    different language style.
7.  **Do not take the address of function arguments or assign to them.**
    Arguments are not addressable/assignable in the current language
    model. The same restriction applies to `for` loop variables.
8.  **Do not assume structs are reference types.** Struct values are
    value types; pointers must be explicit.
9.  **Use the project's existing imports and core library.** Core
    functionality is provided by Gala modules, not necessarily built
    into the language.
10. **Treat examples as executable syntax, not merely pseudocode.**

If a requested feature is not covered here, first inspect nearby source
files and tests. If still uncertain, isolate the question in a minimal
`.gala` file and compile it.

## 2. Compiler: `galac`

The compiler accepts exactly one Gala source file per invocation.

``` sh
galac <file.gala> [-c] [-S -emit-llvm | -emit-llvm] [-o <name>] [--dev|--release] [--abi sysv|win64] [-l <library>]...
```

Examples:

``` sh
# Compile a Gala file using the default output mode.
galac main.gala

# Generate an object file.
galac main.gala -c

# Emit LLVM IR.
galac main.gala -emit-llvm

# Emit one LLVM file using the assembly-style output option.
galac main.gala -S -emit-llvm

# Specify an output name.
galac main.gala -c -o main.o

# Select an optimisation mode and ABI.
galac main.gala --dev --abi sysv
```

### Options

  Option            Meaning
  ----------------- --------------------------------------------------
  `-c`              Generate an object file.
  `-emit-llvm`      Emit LLVM IR.
  `-S -emit-llvm`   Generate one LLVM file.
  `-o <name>`       Set the output name.
  `--dev`           Use development optimisation settings.
  `--release`       Use release optimisation settings.
  `--abi sysv`      Select the SysV ABI. This is supported on Linux.
  `--abi win64`     Select the 64-bit Windows ABI.
  `-l <library>`    Link/include a library; may be repeated.

`--dev` and `--release` select different optimisation settings. They are
not different language modes.

The ABI option selects the target calling convention. `sysv` is for
Linux; `win64` is for 64-bit Windows. Do not choose an ABI based only on
the host language or source syntax.

The compiler reports an error if the invocation does not specify exactly
one `.gala` source file.

## 3. Files, packages, and imports

A source file can begin with a package declaration:

``` gala
package main
```

Imports can select specific symbols:

``` gala
import { any_as, int, println } from "core/core.gala";
```

Or import all symbols from a module:

``` gala
import "core/mem.gala";
```

The selective form imports only the listed names. The unqualified form
imports all symbols exposed by that module.

Examples of core modules used in the language tour:

-   `core/core.gala` --- core definitions and common functions.
-   `core/mem.gala` --- memory utilities, size constants, and
    allocators.
-   `core/core_posix.gala` --- POSIX-oriented core functionality such as
    the demonstrated `println`.
-   `core/mem_posix.gala` --- POSIX memory implementation used by the
    demonstrated arena allocator.

Use the imports already used by the project. Do not assume a function is
globally available just because it appears in another example.

## 4. Declarations and basic syntax

### Functions

``` gala
fn add(a: int, b: int): int {
    return a + b;
}
```

A function with no explicit return type returns no value:

``` gala
fn announce() {
    println("hello");
}
```

Function declarations do not need to appear before their call sites.

### Variables

A variable can have an explicit type:

``` gala
count: int = 10;
```

Or infer its type from its initialiser:

``` gala
count := 10;
```

All variable declarations require an initialiser. A declaration such as
`count: int;` is not valid.

Use `:=` for an inferred variable declaration. Use `=` for assignment to
an existing variable.

### Constants

Constants use `::`:

``` gala
HUNDRED :: 100;
DOUBLE_HUNDRED :: HUNDRED * 2;
GREETING :: "hello";
```

Constants are compile-time expressions. Numeric constants remain untyped
literals in expressions, allowing the receiving context to determine
their type.

A constant can be an expression built from literals and other constants.
The demonstrated language tour does not support constant struct values:

``` gala
// Not supported in the demonstrated language:
/// CONST_VECTOR :: Vector2{ 1.0, 2.0 };
```

### Semicolons and commas

Semicolons are optional in many places. The examples generally include
them for clarity. Trailing commas are accepted in argument lists and
aggregate literals:

``` gala
values := [3]int{ 1, 2, 3, };
```

Use the style of the surrounding file.

## 5. Types and type inference

Gala is strictly typed.

A few important distinctions:

-   `i64` and `int` can be the same type when `int` is provided as an
    alias by the core library.
-   `f64` and `float` can be the same type when `float` is declared as
    an alias.
-   `byte` is used for byte-sized values and C strings are represented
    as `^byte`.
-   `bool` is the boolean type.
-   `rawptr` is effectively an alias for `^void`; it is not a universal
    pointer type that accepts every typed pointer.
-   `any` is a runtime type-erased value represented by data and type
    information.

### Untyped numeric literals

Untyped numeric literals are inferred from their context when possible:

``` gala
fn example(a: int, b: f64) {
    x := 10;   // defaults to i64
    y := 1.25; // defaults to f64

    a = 20;    // literal 20 is accepted as int
    b = 2.5;   // literal 2.5 is accepted as f64
}
```

Without a receiving type, integer literals default to `i64` and
floating-point literals default to `f64`.

A floating-point literal cannot be implicitly converted to an integer:

``` gala
// Invalid: an untyped float is not implicitly converted to int.
// x: int = 1.2;
```

Constants preserve the literal nature of their expression. For example,
a constant defined as `2.5` is still an untyped floating-point literal
when used later; it does not become an integer merely because it has a
name.

### Explicit conversions

Use `cast` for numeric/value conversion:

``` gala
wide := cast(i64)some_i32;
```

Use `transmute` for a bit-level reinterpretation:

``` gala
bits := transmute(int)some_float;
```

Do not use `transmute` as a substitute for ordinary numeric conversion.
A cast converts the value; a transmute reinterprets its representation.

### Type aliases

Aliases name an existing type and share its type identity:

``` gala
alias float = f64;
alias count_type = i64;
```

An alias is not a distinct nominal type.

## 6. Function parameters, defaults, and named arguments

Parameters can have explicit types, inferred generic types, or default
values.

``` gala
fn sample(a: int, b, c: f64, d: byte = 0, e := 1.2) {
}
```

The demonstrated parameter forms include:

-   `a: int` --- explicitly typed parameter.
-   `b` --- parameter whose type is inferred from the call.
-   `d: byte = 0` --- typed parameter with a default value.
-   `e := 1.2` --- parameter with an inferred default type.

Default values can be typed or untyped. Untyped numeric defaults use the
receiving type where possible; otherwise the usual literal defaults
apply.

Arguments can be passed by position or by name:

``` gala
sample(1, 2.5, 3.0, e = 0.5);
sample(1, 2.5, 3.0, 0);
sample(1, 2.5, 3.0, e = 2);
```

A named argument uses `name = value` at the call site. Any parameter can
be supplied by name.

Do not assume that every parameter can be omitted: omission is only
valid when a default exists.

## 7. Operators and expressions

### Arithmetic

The demonstrated arithmetic operators are:

  Operator   Meaning
  ---------- -----------------
  `+`        Addition
  `-`        Subtraction
  `*`        Multiplication
  `/`        Division
  `%`        Integer modulus

`%` is only valid for integer types.

### Bitwise and shift operators

The demonstrated integer-only operators are:

  Operator   Meaning
  ---------- -----------------------------------------------------------
  `<<`       Left shift
  `>>`       Right shift
  `&`        Bitwise AND
  `\|`       Bitwise OR
  `~`        Bitwise complement; currently broken in the supplied tour

Do not emit `~` unless the compiler implementation has since been
verified to support it.

### Boolean operators

Boolean operations use:

-   `&&` --- logical AND.
-   `||` --- logical OR.
-   `!` --- logical NOT.

These operate on booleans, not arbitrary integers.

### Comparisons

Comparisons such as `==`, `<`, and `>` produce booleans. Operands must
have compatible types. Do not assume implicit conversion will make
differently typed operands comparable.

Struct arithmetic and general struct equality are not supported in the
demonstrated language.

### Precedence

Operator precedence follows conventional arithmetic expectations. For
example:

``` gala
result := b / c * e; // (b / c) * e
```

Use parentheses when an expression could be unclear.

## 8. Pointers and addressability

Pointers are explicitly typed. The type `^T` means pointer to `T`.

``` gala
value := 42;
ptr := &value; // ^i64
copy := ptr^;  // dereference
```

The postfix `^` dereferences a pointer. The prefix `&` takes an address.

Pointers to different element types are not interchangeable:

``` gala
// A ^int is not a ^float.
// rawptr is not a general implicit escape hatch.
```

### Addressability restrictions

Function arguments cannot currently be addressed or assigned to:

``` gala
fn cannot_modify_argument(value: int) {
    // Do not take &value or assign value.
}
```

The same restriction applies to `for` loop variables. If mutation is
required, copy the value into a local variable:

``` gala
fn work(argument: int) {
    local := argument;
    local += 1;
}
```

Use an explicit pointer parameter when a function is intended to mutate
a caller-owned value:

``` gala
fn increment(value: ^int) {
    value^ += 1;
}
```

The pointer parameter itself is a value; dereferencing it accesses the
pointed-to storage.

### Pointer arithmetic

Pointers support addition, subtraction, and comparison.
Addition/subtraction are not scaled by the size of the pointed-to element
type:

``` gala
values := [4]int{ 10, 20, 30, 40 };
ptr := &values[0];

ptr += size_of(int);
second := ptr^; // 20
```

Pointer arithmetic returns a pointer of the same pointer type, not an
unsigned integer size. Pointer modulus is not valid.

## 9. Arrays and slices

### Arrays

Array types include their length:

``` gala
bytes := [1024]byte{};
numbers := [5]int{ 10, 20, 30 };
```

The syntax is:

``` gala
[SIZE]TYPE{ INITIALISERS }
```

An empty initialiser zero-initialises the array. If fewer elements are
supplied than the array length, the remaining elements are
zero-initialised.

Arrays must be initialised:

``` gala
// Invalid: no initialiser.
// values: [32]int;
```

An explicitly typed array declaration is also valid:

``` gala
values: [32]int = [32]int{};
```

### Slices

A slice is written `[]T`. In memory, the demonstrated representation is
a pointer and a 64-bit length:

``` text
{ ptr, i64 }
```

Slices can be created from arrays using range syntax:

``` gala
numbers := [10]int{ 1, 2, 3 };

all: []int = numbers[:];
first_three := numbers[:3];
from_second := numbers[1:];
middle := numbers[1:3];
```

The start is inclusive and the end is exclusive.

Either bound can be omitted:

-   `array[start:end]`
-   `array[start:]` --- from `start` to the end.
-   `array[:end]` --- from the beginning to `end`.
-   `array[:]` --- the whole array.

Indexing uses square brackets:

``` gala
first := numbers[0];
```

The demonstrated `for` loop can iterate over arrays and slices.

## 10. Structs

Structs are declared with named fields:

``` gala
struct Vector2 {
    x: f32,
    y: f32,
}

struct Vector3 {
    x, y, z: f32,
}
```

Multiple field names can share a type declaration.

### Struct initialisation

A named struct can be initialised positionally or by field name:

``` gala
a := Vector3{ 1, 2, 3 };
b := Vector3{ x = 1, y = 2, z = 3 };
```

If the expected type is known, the type name can be omitted:

``` gala
c: Vector3 = { 1, 2, 3 };
d: Vector3 = { x = 1, y = 2, z = 3 };
```

Do not mix named and positional fields in the same literal:

``` gala
// Invalid:
// value: Vector3 = { x = 1, 2, z = 3 };
```

### Field access and mutation

``` gala
value := Vector3{ 1, 2, 3 };
value.x = 10;
```

Structs are not automatically dereferenced through pointers. Use
explicit dereferencing:

``` gala
ptr := &value;
ptr^.x = 20;
```

### Struct value behaviour

Treat structs as value types, not implicit references. Do not assume
that passing a struct to a function gives the callee a mutable reference
to the caller's original value. Function arguments themselves are not
assignable/addressable in the demonstrated language model.

Struct addition and general struct equality are not supported.

### Anonymous structs

An anonymous struct type can be declared inline:

``` gala
point: struct { x: f32, y: f32 } = { 1, 2 };
```

Anonymous struct types are interned: anonymous declarations with the
same field structure can resolve to the same type identity. However,
anonymous structs are not structurally interchangeable with separately
named structs.

A `struct { ... }` type expression is not a valid struct-literal type
expression:

``` gala
// Not valid:
// point := struct { x: f32, y: f32 }{ 1, 2 };
```

Use a typed variable with an inferred aggregate literal instead.

## 11. Unions

Unions behave like C unions: their fields overlap in memory and the
union does not store a tag identifying the active field.

``` gala
alias Shape = union {
    vector2: Vector2,
    vector3: Vector3,
};
```

A union initialiser must specify the field to initialise:

``` gala
shape := Shape{ vector2 = Vector2{ 1, 2 } };
shape = { vector3 = Vector3{ 1, 2, 3 } };
```

The field name is required because the compiler cannot reliably infer
which union member was intended.

Reading a member other than the one most recently written is allowed
syntactically, but the result may be meaningless:

``` gala
// If vector3 was written, reading vector2 does not convert it.
// It reinterprets the same union storage.
v := shape.vector2;
```

Union members can be accessed and assigned through normal field access:

``` gala
shape.vector2 = { 4, 5 };
```

Do not treat a union as a tagged/sum type. Track the active member
yourself if the program needs that information.

## 12. Functions as values

Function values are function pointers. A function type includes its
parameter and return types:

``` gala
fn add(a, b: int): int {
    return a + b;
}

fn use_function(callback: fn(a, b: int): int) {
    result := callback(2, 3);
}
```

Function literals are supported:

``` gala
callback := fn(a, b: int): int {
    return a + b;
};
```

A single-expression function literal can use `=>`:

``` gala
callback = fn(a, b: int): int => a + b;
```

The literal's signature must match the destination function type.
Function literals do not support generic parameters or default parameter
values.

Function types are checked structurally rather than relying on interned
type IDs. Two function values with matching signatures can be assigned
to one another even if their reported type IDs differ.

A generic function is a compile-time template, not an ordinary function
value that can be stored or passed around as a first-class function.

## 13. Generics

Generic parameters are introduced with `$` and then referred to by their
declared name without `$`.

``` gala
fn identity(value: $T): T {
    return value;
}
```

`$T` declares generic type `T`. Once declared, use `T` as a type.

A generic type can only be declared once in a generic declaration. To
give multiple parameters the same generic type, declare it on one
parameter and reuse the name:

``` gala
fn same_type(a: $T, b: T) {
}
```

Multiple parameters can share a newly declared generic:

``` gala
fn two_types(a: $T, b, c: $U, d: U) {
}
```

A generic cannot first be declared in a function's return type. It must
be introduced by a parameter or explicit generic parameter list.

### Generic inference

For inferred generic function arguments, the compiler uses the first
typed argument that can bind the generic. Untyped literals do not
necessarily determine the generic type.

For example, if a generic function receives an untyped integer followed
by an `f32`, the generic may resolve to `f32` from the typed argument.
If arguments provide conflicting concrete types for the same generic, do
not expect an implicit conversion to reconcile them.

Generic bodies are checked once the concrete types are known, separately
from ordinary type checking. An operation inside a generic function is
valid only if it is valid for the concrete types used to instantiate
that function.

### Explicit generic parameters: turbofish

A function can declare explicit generic parameters with `::<...>`:

``` gala
fn apply::<$F>(callback: F) {
    result := callback();
}
```

The call supplies those explicit type parameters using the same syntax:

``` gala
apply::<fn(): int>(fn(): int => 123);
```

Explicitly listed generic parameters are not inferred. A function can
combine explicitly supplied and inferred generic parameters:

``` gala
fn combine::<$F>(callback: F, value: $T): T {
    return value;
}

result := combine::<fn(): int>(fn(): int => 1, 42);
```

Use the exact declaration and call syntax already present in the
project; generic syntax for functions and structs is different.

### Generic structs

Generic structs declare their type parameters in the struct declaration:

``` gala
struct Pair {
    first, second: $T,
}
```

A concrete type is written by supplying the generic type:

``` gala
pair := Pair(int){ 10, 20 };
```

The compiler can also infer generic struct arguments from an
initialiser:

``` gala
pair := Pair{ 10, 20 };              // Pair(i64)
float_pair := Pair{ 1.0, cast(f32)2 }; // inferred from typed values
```

A bare generic struct name is not a complete concrete type. Supply its
type arguments or use a context in which inference is supported.

A generic struct can be used in a function parameter pattern:

``` gala
fn sum_pair(value: Pair($T)): int {
    return cast(int)(value.first + value.second);
}
```

Or a function can explicitly declare the type parameter:

``` gala
fn sum_pair_explicit::<$T>(value: Pair(T)): int {
    return cast(int)(value.first + value.second);
}
```

A generic type alias can instantiate an already defined generic type:

``` gala
alias IntPair = Pair(int);
```

Do not define a new anonymous generic struct inside an alias and expect
its generic names to be in scope there. Generic names must already be
defined in the declaration context.

### Generic types in unions

Generic structs can contain unions whose member types use declared
generic parameters:

``` gala
struct Choice($A, $B) {
    data: union {
        first: A,
        second: B,
    },
}
```

All generic type names used inside the union must be declared by the
surrounding generic struct/function declaration.

## 14. Variadic arguments

Variadic parameters use `..`:

``` gala
fn sum(values: ..int): int {
    total := 0;
    for value in values {
        total += value;
    }
    return total;
}
```

Inside the function, the variadic parameter behaves as a slice of the
declared element type. It can be iterated, indexed, and queried for its
length.

A generic variadic parameter is also supported:

``` gala
fn sum_generic(values: ..$T): T {
    total: T = 0;
    for value in values {
        total += value;
    }
    return total;
}
```

The generic type is inferred using typed arguments. When a variadic call
contains untyped literals and at least one typed argument, the typed
argument can determine the generic type.

Call sites can include a trailing comma:

``` gala
result := sum(1, 2, 3, 4,);
```

## 15. `any` and C variadic calls

### Gala `any`

`any` is a runtime type-erased value. Its demonstrated representation
contains:

-   `data`: a `rawptr` pointing to the stored value.
-   `id`: the Gala type ID of the stored value.

Passing a value to an `any` parameter boxes it at runtime:

``` gala
fn inspect(value: any) {
    println("type id:", value.id);
    println("data pointer:", value.data);
}
```

A variadic `..any` parameter receives a slice of boxed `any` values:

``` gala
fn inspect_many(values: ..any) {
    for value in values {
        println(value.id, value.data);
    }
}
```

Do not confuse Gala's `any` with C's `...`. Gala `any` values are boxed
and carry type metadata.

### External C variadic functions

An external declaration can use `..any` for C variadic arguments:

``` gala
extern fn printf(fmt: ^byte, data: ..any);
```

For an `extern` function, `..any` is **not boxed**: values are passed to
C as they are. This is distinct from an ordinary Gala function receiving
`..any`.

C interop requires particular care with ABI, pointer types, format
strings, and argument representations. Match the C function's actual
signature and calling convention.

## 16. Control flow

### `if` / `else if` / `else`

Conditions must be booleans:

``` gala
if value > 10 {
    println("large");
} else if value > 0 {
    println("positive");
} else {
    println("zero or negative");
}
```

Only one branch of an `if` chain executes.

### `while`

``` gala
i := 0;
while i < 10 {
    i += 1;
}
```

### `for`

The demonstrated `for` loop iterates over arrays and slices:

``` gala
for item in [5]int{ 1, 2, 3, 4, 5 } {
    println(item);
}
```

Loop variables cannot be assigned to or addressed. Copy a loop variable
into a local if a mutable local is needed.

### `break` and `continue`

Both apply to loops:

-   `continue` skips the remainder of the current iteration.
-   `break` exits the current loop.

``` gala
i := 0;
while i < 100 {
    i += 1;

    if i % 2 == 0 {
        continue;
    }

    if i > 7 {
        break;
    }

    println(i);
}
```

## 17. External functions and exported symbols

### External declarations

Use `extern fn` to declare a function implemented outside Gala:

``` gala
extern fn printf(fmt: ^byte, data: ..any);
```

The Gala declaration must match the external function's ABI and
parameter representation. For C strings, `^byte` is used in the
demonstrated examples.

A null-terminated string can be created through the memory helpers, or a
pointer to a string literal's first byte can be used:

``` gala
printf(&("hello\n\0")[0]);
```

### Exported functions

`export` makes the external symbol use the function's declared name
rather than the compiler-generated Gala-qualified name:

``` gala
export fn this_fn() {
}
```

The demonstrated symbol is `@this_fn` rather than a Gala-mangled name.

## 18. Memory and core-library examples

The language tour demonstrates an arena allocator from the memory
modules:

``` gala
import "core/mem.gala";

fn main() {
    arena := arena_make(200 * KB);
    // use arena_allocator(&arena) for allocations
    arena_destroy(&arena);
}
```

A C string can be cloned into the arena:

``` gala
cstr := clone_cstring(arena_allocator(&arena), "hello\n");
```

The relevant names in the demonstrated environment include:

-   `arena_make`
-   `arena_allocator`
-   `arena_destroy`
-   `clone_cstring`
-   `KB`, `MB`, `GB`

These are library facilities, not syntax. Import the module that defines
them and check the project's implementation for exact allocator APIs.

## 19. Type reflection and size queries

The core library exposes type reflection helpers in the tour:

``` gala
type_id(value_or_type)
size_of(value_or_type)
type_info(type_id)
```

-   `type_id` returns a Gala type ID for a type or expression.
-   `size_of` returns the size of a type or expression's type.
-   `type_info` returns a pointer to generated type information.

Example:

``` gala
println("size:", size_of(Vector3));
info := type_info(type_id(Vector3));
println(info^.name);
```

Type IDs are useful for comparisons and generic/runtime type operations,
but should not be treated as stable identifiers across different
programs or compilations.

Aliases share the type ID of their underlying type. Function types are
structurally checked and should not be assumed to have a unique interned
ID.

## 20. Common mistakes to avoid

  -----------------------------------------------------------------------
  Mistake                             Correct approach
  ----------------------------------- -----------------------------------
  Assuming `int` is a separate        Check the core alias; in the tour,
  built-in type                       `int` aliases `i64`.

  Assuming `float` is a built-in type In the tour, `float` aliases `f64`.

  Assigning a float literal to an     Use an integer literal or an
  integer                             explicit conversion where
                                      appropriate.

  Mixing `f32` and `i8` in an         Convert explicitly so operand types
  expression                          match.

  Treating `rawptr` as compatible     Pointer types are strict; convert
  with every pointer                  explicitly only if a supported
                                      operation exists.

  Taking `&argument`                  Arguments are not addressable; use
                                      a pointer parameter or a local
                                      copy.

  Assigning to a `for` loop variable  Copy it into a local variable
                                      first.

  Expecting `ptr.field` to            Write `ptr^.field`.
  auto-dereference                    

  Using `%` with floats or pointers   `%` is integer-only.

  Treating a union as tagged          Track the active member separately.

  Reading a union member that was not It is only a reinterpretation of
  written                             the same storage and may be
                                      meaningless.

  Mixing named and positional struct  Use one initialisation style per
  fields                              literal.

  Using a generic struct without type Supply its type arguments or use
  arguments                           supported inference.

  Treating a generic function as a    Generic functions are compile-time
  normal function value               templates.

  Expecting function literals to      Use a declared generic function
  support generics/defaults           instead.

  Assuming a C `...` call boxes `any` External `..any` arguments are
  arguments                           passed unboxed.

  Assuming every common language      Verify against this guide, project
  feature exists                      code, and compiler tests.
  -----------------------------------------------------------------------

## 21. A compact complete example

``` gala
package main

import { println } from "core/core.gala";

struct Vector2 {
    x, y: f32,
}

fn dot(a: Vector2, b: Vector2): f32 {
    return a.x * b.x + a.y * b.y;
}

fn first(values: []$T): T {
    return values[0];
}

fn main() {
    a := Vector2{ 1, 2 };
    b := Vector2{ x = 3, y = 4 };

    println("dot: {}", dot(a, b));

    numbers := [3]int{ 10, 20, 30 };
    view := numbers[:];

    println("first: {}", first(view));

    total := 0;
    for value in view {
        total += value;
    }

    println("total: {}", total);
}
```

The generic function syntax above is illustrative of the concepts in
this guide. When in doubt, use the exact generic declaration form
accepted by the compiler and demonstrated in the project's source. The
tour's established generic function form is:

``` gala
fn first(values: []$T): T {
    return values[0];
}
```

## 22. Agent workflow checklist

Before presenting a Gala patch:

-   [ ] Confirm the file uses the correct imports.
-   [ ] Check every variable declaration has an initialiser.
-   [ ] Check that `:=` is used for declarations and `=` for assignment.
-   [ ] Check that operand types match exactly or are explicitly
    converted.
-   [ ] Check pointer types and explicit dereferences.
-   [ ] Check that no argument or loop variable is assigned to or
    addressed.
-   [ ] Check aggregate literals use valid positional or named
    initialisation.
-   [ ] Check generic parameters are declared with `$` and reused
    without `$`.
-   [ ] Check generic structs have concrete type arguments or a valid
    inference context.
-   [ ] Check C declarations against the actual ABI and C signature.
-   [ ] Compile the smallest relevant target with `galac`.
-   [ ] If possible, run the relevant tests or a minimal executable
    test.

When reporting a result, state the exact compiler command used and
whether compilation, object generation, or LLVM emission succeeded. Do
not claim that code was tested if it was only inspected.


> **Note:** to be provided along tour.gala
