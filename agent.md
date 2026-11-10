# Gala language reference (for coding agents)

Gala is a small, strictly typed, systems-style language. Its compiler is written in Odin and emits textual LLVM IR. This file describes what is **known to work**, derived from the language tour and the standard library (`core/mem.gala`). If a construct is not listed here, **assume it is unsupported** and use the simplest equivalent. The compiler is the source of truth: when unsure, write a tiny test program and compile it.

## 0. Ground rules

- Strictly typed. No implicit conversions between different typed values (`f32 + i8` is an error). Use `cast(T)x` (value conversion, truncates) or `transmute(T)x` (bitcast).
- **Untyped literals** (`1`, `2.5`, `0x10`) adapt to the type the receiver expects. With no receiver, integers become `i64` (`int`) and floats become `f64` (`float`). An untyped float never converts to an integer type (`i: int = 1.2;` is an error), and the same holds for float constants (`i: int = NON_INT_CONST;` fails).
- Every variable declaration needs a value. `m: int;` is an error.
- Function parameters and `for` loop variables have no address: you cannot assign to them or take `&` of them. Copy to a local first (`x := param;`).
- Pointers are **not** auto-dereferenced. Write `p^.field`, never `p.field`.
- Declaration order of top-level items does not matter.
- Always end statements with `;`. Some tour lines omit it (`v := f(1, 2)`, `return g`), and it is unclear whether that is legal. Don't rely on it.
- Trailing commas are allowed in argument lists, struct literals, and field lists.
- Comments are `//` only (no block comments observed).
- Style used in the codebase: 4-space indent, `snake_case` functions and variables, `PascalCase` structs, `UPPER_CASE` constants.

## 1. Types

| Type | Notes |
|---|---|
| `i8`, `i32`, `i64`, `u64`, `byte` | confirmed in the tour and `mem.gala`. `i16`, `u8`, `u16`, `u32` are likely but unverified. `byte` is the 8-bit type for raw data and C strings |
| `int` | alias for `i64` (same type id) |
| `f32`, `f64`, `float` | `float` is an alias for `f64` |
| `bool` | `true` / `false` |
| `string` | pointer + length; see section 9 |
| `^T` | pointer to `T` |
| `rawptr` | untyped pointer (like `^void`). **Not** implicitly convertible to or from `^T` |
| `[N]T` | fixed array, always initialised: `[N]T{}` |
| `[]T` | slice, memory layout `{ ptr, i64 }` |
| `fn(a, b: int): int` | function pointer type, structurally compared |
| `any` | `{ data: rawptr, id: type_id }`, values are boxed at runtime |
| `struct {...}` | named or anonymous (anonymous types are interned by shape) |

Aliases: `alias float = f64;` (same type id as the target).

`NULL` is defined in core as `NULL :: 0;`, an untyped constant `0`, not a `rawptr`. Being untyped, it adapts to the receiver like any untyped literal, so `return NULL;` and `p == NULL` work with a `rawptr`. To get a null typed pointer, the library writes `transmute(^T)NULL`. See section 2 for `::` constants.

## 2. Variables and constants

```gala
i: i64 = 1;          // explicit type
k := 1;              // inferred i64
l := 1.2;            // inferred f64
j: float = 1.2;

KB: u64 = 1024;      // top-level (global) variable
G := Allocator{proc, NULL};   // globals may use := and call functions in initialisers
```

### Constants (`::`)

```gala
ONE_HUNDRED :: 100;
TWO_HUNDRED :: ONE_HUNDRED * 2;            // expressions of literals and other constants
ONE_AND_A_HALF_HUNDRED :: 1.5 * ONE_HUNDRED;
STRING :: "strings are also allowed";
NON_INT_CONST :: 1.234;
NULL :: 0;                                 // defined in core
```

- Constants are evaluated at compile time and are always treated as literals, so they are **untyped** in any expression (`a := arena_make(TWO_HUNDRED * KB);` works because the constant adapts to `u64`).
- Allowed values: numbers, strings, and expressions built from literals and other constants.
- There are no typed constants. A float constant cannot be used where an integer is expected.
- Struct constants (`CONST_STRUCT :: Vector2{ 1.2, 3.4 };`) are **not allowed** for now.
- Use `::` for constants and `:` / `:=` for variables. Don't mix them up.

## 3. Operators

- Arithmetic `+ - * / %`. `%` is integers only.
- Bit ops `<<  >>  &  |` (integers only). **`~` (bitwise not) currently does not work.** There is no confirmed xor operator, so avoid needing one. For max values write `cast(u64)0 - cast(u64)1`.
- Logic `&&  ||  !` (booleans only). Conditions must be `bool`.
- Comparison `== != < > <= >=`.
- Compound assignment: `+=`, `-=` confirmed. Others: test first.
- Pointers allow `+`, `-` and comparison only. The result keeps the pointer's type. Whether `p + 1` scales by element size is not documented. For byte offsets on a `rawptr` use `ptr_add(p, bytes)` from core.
- Structs cannot be added or compared with `==`. Compare field by field.
- Precedence is the usual one (`a / b * c` is `(a / b) * c`).

## 4. Functions

```gala
fn add(a, b: int): int { return a + b; }          // shared type for `a, b`
fn scale(v: f64, k := 2.0): f64 { return v * k; } // default value, type inferred
fn nothing() { }                                  // no return type means no return value
```

- Defaults may be typed or untyped. They must be trailing parameters.
- Named arguments at call sites: `expressions(1, 2, 3.4, 123, e = 3.4);`. Positional and named can be mixed (positional first).
- Function values are pointers. Literals: `fn(a, b: int): int { return a + b; }` or `fn(a, b: int): int => a + b`.
- Function literals cannot be generic and cannot have default values.
- Function types are checked structurally and are not interned.
- To call a function pointer stored in a struct field, the library copies it to a local first. Follow that pattern:

```gala
cb := a.callback;
return cb(a.data, MODE_ALLOC, size, align, NULL, 0);
```

- `extern fn printf(fmt: ^byte, data: ..any);` declares a C function. `..any` forwards varargs. Pass C strings as `^byte`.
- `export fn this_fn() { }` gives the function the plain symbol `@this_fn` instead of a mangled `@gala_...` name, so C and other objects can link to it. Only the zero-parameter form appears in the tour.

### Variadics

```gala
fn sum(data: ..int): int {           // `data` is a []int inside the function
    total := 0;
    for i in data { total += i; }
    return total;
}
fn sum_g(data: ..$T): T { ... }      // generic variadic
```

## 5. Structs

```gala
struct Vector3 { x, y, z: f32, }          // grouped fields allowed

d := Vector3{ x = 1, y = 2, z = 3 };      // named init
g: Vector3 = { -1, -2, -3 };              // positional init, type from context
// Mixing named and positional in one literal is an error.

p := &d;            // ^Vector3
p^.x = 5;           // explicit deref
```

- Structs are padded/aligned to their largest member (`size_of` includes padding).
- Anonymous struct types: `v: struct { x: f32, y: f32 } = { 0, 0 };`. There is no expression form like `struct {...}{...}`. Two identical anonymous struct types are the same type. An anonymous struct is **not** compatible with a named struct of the same shape.
- Struct fields can hold function pointers (`Allocator.callback`).

## 6. Generics

Generics are template-like: instantiated at compile time and type-checked after the types are known. A generic function does not exist as a value (`f := generic_fn;` is an error).

Inferred type parameters (`$T` in the parameter list):

```gala
fn generic_fn(a: $T, b: T, c, d: $D): D { ... }  // `c, d: $D` shares one D
fn deref(a: ^$T): T { return a^; }               // pointer form: `^$T`, not `$^T`
fn first(a: []$T): T { return a[0]; }            // slice form: `[]$T`, not `$[]T`
```

- Declare a `$X` only once, then reuse the bare name (`T`). `(a: $T, b: $T)` is an error.
- You cannot declare a generic in the return type (`fn f(): $D` is an error), but you can return an already declared one.
- When several arguments could bind a type parameter, the **first typed** argument wins. Untyped literals only fall back to the default (`i64`/`f64`) if nothing typed binds it.

Explicit ("turbofish") type parameters (declared with `::<$T>`, never inferred):

```gala
fn new::<$T>(a: Allocator): ^T { ... }
p := new::<Vector3>(a);

fn call3::<$G>(f: G) { ... }
call3::<fn(a, b: f32): f32>(fn(a, b: f32): f32 => a * b);

fn type_name::<$G>() { println(type_info(type_id(G))^.name); }
```

Turbofish and inferred parameters can be mixed: `fn f::<$G>(f: G, g: $H): H` is called as `f::<Ty>(fn_value, 67)`. Don't use `::<>` on a function that declares no turbofish parameters.

Generic structs (different syntax, subject to change):

```gala
struct Pair { x, y: $G, }
a := Pair(int){ 1, 2 };            // explicit
b := Pair{ 1, cast(f32)2 };        // inferred (first typed field wins, so f32)
fn use_pair(p: Pair($T)): int { ... }              // inferred
fn use_pair2::<$T>(p: Pair(T)): int { ... }        // explicit
```

A bare `Pair` is never a valid type on its own.

## 7. Control flow

```gala
if cond { ... } else if other { ... } else { ... }   // braces required, cond must be bool

while !done { ... }

for item in array_or_slice { ... }   // only arrays and slices; `item` is read-only
// There is no known range form (`for i in 0..n`): use while with a counter.

break;      // loops only
continue;   // loops only
```

No ternary, `switch`, `defer`, enums or unions are documented. Do not assume they exist.

## 8. Arrays and slices

```gala
buf   := [1024]byte{};              // zeroed
nums  := [10]int{ 1, 2, 3 };        // rest zero
s: []int = nums[0:10];              // [lo:hi), hi exclusive
s = nums[0:];  s = nums[:10];  s = nums[:];
x := s[0];                          // indexing
```

- Arrays must always be initialised (`[N]T{}`).
- Slice length is an `i64` in practice (`len(s)` returns a signed value; the library casts to `u64` when needed).
- `raw_data(slice)` gives the element pointer. `len(x)` works on slices and strings.
- Build a slice from raw memory without allocating: `slice_from(ptr, n)` / `bytes_from(rawptr, n)` (from `mem.gala`).

## 9. Strings

- `string` has a length and a data pointer. Literals support `\n` and `\0`.
- `len(s)` is its length. `str_data(s)` is the data pointer.
- Pass a literal to C by taking the address of its first byte: `&("text\n\0")[0]`, or use `clone_cstring(alloc, s)` for a zero-terminated heap copy (returns `^byte`).
- `transmute(string)bytes` reinterprets a `[]byte` as a string (used by `clone_string`).

## 10. Printing

```gala
println("Hello");
println("count:", n, "ok:", flag);          // args printed space-separated
println("{} mod {} is {}", a, b, c);       // `{}` placeholders filled in order
eprintln("to stderr");
```

Printing a struct, `type_info(...)^`, or a `type_id` works. Use these in tests to inspect types.

## 11. Reflection helpers

- `type_id(type_or_expr)` returns a type id. Ids are not stable across compilations.
- `size_of(type_or_expr)` returns the size in bytes, including padding.
- `type_info(id)^` is the compile-time type info struct (`.name` is the readable name).

## 12. Modules

```gala
import "core/core.gala";       // path relative to the project root
import "./core_posix.gala";    // path relative to the importing file

import "core/mem.gala";                                  // imports all symbols
import { any_as, int, println } from "core/core.gala";   // imports only the listed symbols
```

Selective imports list every name you use, and that includes core-defined types and aliases (the tour's list contains `int`) as well as functions like `println`. `any_as` is exported by core. Its purpose and signature are not documented in the tour, so check core before using it.

Platform-specific files use a suffix (`core_posix.gala`, `cfunctions_posix.gala`). C functions such as `malloc`, `calloc`, `realloc`, `free`, `memmove`, `memset`, `memcmp`, `exit` come from `cfunctions_posix.gala`.

## 13. Memory: `core/mem.gala`

**There is no default allocator. Every allocating or freeing call takes an `Allocator` as its first argument.** Pass the allocator explicitly everywhere, and thread it through your own APIs.

```gala
struct Allocator { callback: fn(data: rawptr, mode: i32, size: u64, align: u64, old: rawptr, old_size: u64): rawptr, data: rawptr }
// modes: MODE_ALLOC, MODE_FREE, MODE_FREE_ALL, MODE_RESIZE
```

Rules for every allocator: alloc/resize memory is zeroed; failure returns `NULL` and leaves `old` valid; freeing `NULL` is a no-op; `align` must be a power of two; `old_size` only matters for `Tracking`.

| Function | Purpose |
|---|---|
| `alloc(a, size, align = 16)` | zeroed raw memory or `NULL` |
| `dealloc(a, p, size = 0)` | free (pass `size` when using Tracking) |
| `dealloc_all(a)` | free everything (arena, pool) |
| `resize(a, p, old_size, new_size, align = 16)` | grow/shrink, `NULL` on failure |
| `new::<T>(a)` / `del(a, p)` | one zeroed `T` / free it |
| `make_slice::<T>(a, n)` / `del_slice(a, s)` | zeroed `[]T` (empty on failure or overflow) / free |
| `clone_slice(a, s)` | shallow copy |
| `alloc_bytes(a, n)` / `free_bytes(a, b)` | `[]byte` |
| `clone_string(a, s)` / `free_string(a, s)` | string copy |
| `clone_cstring(a, s)` | zero-terminated copy as `^byte`. Free with `dealloc(a, p, len(s) + 1)` |

Allocators:

| Constructor | Notes |
|---|---|
| `heap_allocator()` | libc malloc. Alignment up to 16 |
| `arena_make(size)` → `Arena`, `arena_allocator(&arena)` | bump allocator. `arena_destroy(&arena)` frees all. Also `arena_init(&a, buf, size)`, `arena_reset`, `arena_mark` / `arena_restore` for scratch scopes, `arena_used`, `arena_remaining` |
| `temp_allocator()` | global arena (4 MB default). Call `temp_reset()` each frame. `temp_init(size)` / `temp_destroy()` |
| `pool_make(chunk_size, count)` → `Pool`, `pool_allocator(&pool)` | fixed-size chunks, O(1). `pool_destroy`, `pool_reset`, `pool_remaining`. No double-free detection |
| `tracking_init(&t, backing)`, `tracking_allocator(&t)` | counts allocs/frees/live/peak. `tracking_report(&t)` |
| `PANIC_ALLOCATOR` | aborts on any use. For buffers that must never allocate |

Raw helpers: `addr(p)`, `from_addr(u64)`, `ptr_add(p, bytes)`, `align_forward`, `is_power_of_two`, `mem_copy` (memmove), `mem_zero`, `mem_set`, `mem_equal`, `min_u64`, `max_u64`, `u64_max()`, constants `KB`, `MB`, `GB`.

Typical pattern:

```gala
arena := arena_make(1 * MB);
a := arena_allocator(&arena);
v  := new::<Vector3>(a);
xs := make_slice::<f64>(a, 100);
s  := clone_string(a, "hello");
arena_destroy(&arena);
```

Library code should take `a: Allocator` as a parameter rather than reaching for a global.

## 14. Common mistakes (checklist before you compile)

1. Mixed numeric types in one expression. Add `cast(...)`.
2. Assigning to a function parameter or loop variable. Copy to a local.
3. `p.x` on a pointer. Use `p^.x`.
4. A `rawptr` used where `^T` is expected (or the reverse). Use `transmute`.
5. `%` on floats or pointers.
6. Using `~`. It is broken right now.
7. Mixed named and positional values in a struct literal.
8. `$T` declared twice, `$` in a return type, or `$^T` / `$[]T`.
9. Calling a turbofish-declared generic without `::<...>`, or the reverse.
10. Missing `;`.
11. Allocating without an `Allocator` argument, or forgetting to pass `size` to `dealloc` when using Tracking.
12. Relying on features not listed here (enums, `defer`, ranges in `for`, multiple return values, `switch`).
13. Putting an untyped float (literal or constant) where an integer is expected.
14. Declaring a struct constant with `::`. Not allowed for now.
15. Using a core name (`int`, `println`, ...) that a selective `import { ... } from` list doesn't include.

## 15. Open questions (verify with the compiler)

- Are semicolons optional at line end in some positions? (no)
- Does `p + 1` on `^T` scale by `size_of(T)`? (yes)
- Is there a range-based `for`, `defer`, enums, unions, or multiple return values? (no defer/range based. for loops iterate on arrays and slices only for now)
- Can a struct field of function type be called directly (`a.callback(...)`) without a local copy? (yes. fn calls bind left)
- Which compound assignment operators exist beyond `+=` and `-=`? (+=, -=, *=, /=)
- Does `NULL` (untyped `0`) coerce directly to `^T`, or is `transmute(^T)NULL` always required? (yes. untyped values coerce to whatever they can. untyped integers can coerce to pointers)
