/*
 * C API / ABI stress-test surface
 *
 * Purpose:
 *   Expose a deliberately broad set of C declarations so a foreign language
 *   (e.g. Gala) can test C type mapping, argument passing, return values,
 *   struct layout, alignment, arrays, pointers, callbacks, enums, unions,
 *   variadic calls, and pointer indirection.
 *
 * Suggested build:
 *   cc -std=c11 -Wall -Wextra -Wpedantic -Wconversion -Wshadow \
 *      -fPIC -shared c_api_abi_stress.c -o libc_api_abi_stress.so
 *
 * For a standalone smoke test:
 *   cc -std=c11 -Wall -Wextra -Wpedantic c_api_abi_stress.c \
 *      -DABI_STRESS_MAIN -o abi_stress_test
 *
 * Notes:
 *   - C's long and size_t widths are platform-dependent; do not assume they
 *     match a fixed-width integer type.
 *   - Struct layout is ABI/compiler/platform-dependent.
 *   - Bit-field layout and packed structs are especially implementation-
 *     dependent. The packed type below uses a GCC/Clang extension when present.
 *   - Calling a function through an incompatible function-pointer type is
 *     undefined behavior. Match the declared signature exactly.
 */

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <limits.h>
#include <float.h>
#include <string.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

/* ------------------------------------------------------------------------- */
/* 1. Fundamental scalar aliases                                              */
/* ------------------------------------------------------------------------- */

typedef signed char        abi_i8;
typedef unsigned char      abi_u8;
typedef short              abi_short;
typedef unsigned short     abi_ushort;
typedef int                abi_int;
typedef unsigned int       abi_uint;
typedef long               abi_long;
typedef unsigned long      abi_ulong;
typedef long long          abi_i64_native;
typedef unsigned long long abi_u64_native;
typedef int8_t             abi_int8_t;
typedef uint8_t            abi_uint8_t;
typedef int16_t            abi_int16_t;
typedef uint16_t           abi_uint16_t;
typedef int32_t            abi_int32_t;
typedef uint32_t           abi_uint32_t;
typedef int64_t            abi_int64_t;
typedef uint64_t           abi_uint64_t;
typedef size_t             abi_size;
typedef ptrdiff_t          abi_ptrdiff;
typedef intptr_t           abi_intptr;
typedef uintptr_t          abi_uintptr;
typedef float              abi_f32;
typedef double             abi_f64;
typedef long double        abi_fld;
typedef bool               abi_bool;
typedef char               abi_char;
typedef const char        *abi_cstr;
typedef void              *abi_ptr;
typedef const void        *abi_const_ptr;

/* ------------------------------------------------------------------------- */
/* 2. Scalar identity, arithmetic, and argument order                         */
/* ------------------------------------------------------------------------- */

abi_i8   abi_echo_i8(abi_i8 x)                 { return x; }
abi_u8   abi_echo_u8(abi_u8 x)                 { return x; }
abi_short abi_echo_short(abi_short x)           { return x; }
abi_ushort abi_echo_ushort(abi_ushort x)        { return x; }
abi_int  abi_echo_int(abi_int x)               { return x; }
abi_uint abi_echo_uint(abi_uint x)              { return x; }
abi_long abi_echo_long(abi_long x)              { return x; }
abi_ulong abi_echo_ulong(abi_ulong x)           { return x; }
abi_i64_native abi_echo_ll(abi_i64_native x)    { return x; }
abi_u64_native abi_echo_ull(abi_u64_native x)   { return x; }
abi_int8_t abi_echo_int8(abi_int8_t x)          { return x; }
abi_uint8_t abi_echo_uint8(abi_uint8_t x)       { return x; }
abi_int16_t abi_echo_int16(abi_int16_t x)       { return x; }
abi_uint16_t abi_echo_uint16(abi_uint16_t x)    { return x; }
abi_int32_t abi_echo_int32(abi_int32_t x)       { return x; }
abi_uint32_t abi_echo_uint32(abi_uint32_t x)    { return x; }
abi_int64_t abi_echo_int64(abi_int64_t x)       { return x; }
abi_uint64_t abi_echo_uint64(abi_uint64_t x)    { return x; }
abi_size abi_echo_size(abi_size x)              { return x; }
abi_ptrdiff abi_echo_ptrdiff(abi_ptrdiff x)     { return x; }
abi_intptr abi_echo_intptr(abi_intptr x)        { return x; }
abi_uintptr abi_echo_uintptr(abi_uintptr x)     { return x; }
abi_f32 abi_echo_f32(abi_f32 x)                 { return x; }
abi_f64 abi_echo_f64(abi_f64 x)                 { return x; }
abi_fld abi_echo_long_double(abi_fld x)         { return x; }
abi_bool abi_echo_bool(abi_bool x)              { return x; }
abi_char abi_echo_char(abi_char x)              { return x; }
abi_ptr abi_echo_ptr(abi_ptr x)                 { return x; }
abi_const_ptr abi_echo_const_ptr(abi_const_ptr x){ return x; }
abi_cstr abi_echo_cstr(abi_cstr x)              { return x; }

int abi_mix_1(int a, double b, int c) {
    return a * 100 + (int)b * 10 + c;
}
double abi_mix_2(double a, int b, float c, long d, short e) {
    return a + (double)b * 2.0 + (double)c * 3.0 + (double)d * 4.0 + (double)e * 5.0;
}
uint64_t abi_mix_3(uint8_t a, uint64_t b, uint16_t c, double d, uint32_t e) {
    return (uint64_t)a + b * UINT64_C(3) + (uint64_t)c * UINT64_C(5)
         + (uint64_t)d + (uint64_t)e * UINT64_C(7);
}
double abi_many_args(
    int a0, double a1, uint8_t a2, float a3, int64_t a4,
    uint16_t a5, void *a6, int a7, double a8, uint32_t a9,
    int16_t a10, float a11, int64_t a12, uint8_t a13, double a14)
{
    return (double)a0 + a1 + (double)a2 + (double)a3 + (double)a4
         + (double)a5 + (a6 != NULL ? 17.0 : 0.0) + (double)a7 + a8
         + (double)a9 + (double)a10 + (double)a11 + (double)a12
         + (double)a13 + a14;
}

void abi_write_scalars(
    int8_t *i8_out, uint8_t *u8_out, int16_t *i16_out, uint16_t *u16_out,
    int32_t *i32_out, uint32_t *u32_out, int64_t *i64_out, uint64_t *u64_out,
    float *f32_out, double *f64_out, bool *bool_out)
{
    if (i8_out)   *i8_out = -101;
    if (u8_out)   *u8_out = 231;
    if (i16_out)  *i16_out = -30001;
    if (u16_out)  *u16_out = 60001;
    if (i32_out)  *i32_out = -2000000001;
    if (u32_out)  *u32_out = UINT32_C(4000000001);
    if (i64_out)  *i64_out = -INT64_C(8000000000000000000);
    if (u64_out)  *u64_out = UINT64_C(16000000000000000000);
    if (f32_out)  *f32_out = 123.25f;
    if (f64_out)  *f64_out = -98765.125;
    if (bool_out) *bool_out = true;
}

/* ------------------------------------------------------------------------- */
/* 3. Structs: scalar order, padding, alignment, nested values                 */
/* ------------------------------------------------------------------------- */

typedef struct AbiEmptyLike {
    uint8_t marker;
} AbiEmptyLike;

typedef struct AbiAllSmall {
    int8_t a;
    uint8_t b;
    int16_t c;
    uint16_t d;
    int32_t e;
    uint32_t f;
} AbiAllSmall;

typedef struct AbiMixedOrderA {
    uint8_t a;
    uint64_t b;
    uint16_t c;
    double d;
    uint8_t e;
} AbiMixedOrderA;

typedef struct AbiMixedOrderB {
    double d;
    uint64_t b;
    uint16_t c;
    uint8_t a;
    uint8_t e;
} AbiMixedOrderB;

typedef struct AbiFloatMix {
    float a;
    double b;
    long double c;
    float d;
} AbiFloatMix;

typedef struct AbiPointers {
    void *p0;
    const char *text;
    int *int_ptr;
    double *double_ptr;
    void **pointer_to_pointer;
} AbiPointers;

typedef struct AbiInner {
    uint8_t tag;
    int32_t value;
    double weight;
} AbiInner;

typedef struct AbiNested {
    int16_t before;
    AbiInner inner;
    uint64_t after;
    AbiInner second;
} AbiNested;

typedef struct AbiArrayFields {
    uint8_t bytes[7];
    int16_t shorts[3];
    int32_t ints[4];
    float floats[2];
} AbiArrayFields;

typedef struct AbiLarge {
    uint64_t id;
    char name[32];
    uint8_t flags;
    uint8_t padding_probe[7];
    double values[5];
    AbiInner nested[3];
    void *user_data;
    size_t length;
    int32_t status;
} AbiLarge;

typedef struct AbiLongNamesAndOddSizes {
    uint8_t a;
    uint32_t b;
    uint8_t c;
    uint64_t d;
    uint16_t e;
    uint8_t f;
    double g;
    uint8_t h;
    long double i;
    uint8_t j;
} AbiLongNamesAndOddSizes;

AbiAllSmall abi_make_all_small(void) {
    AbiAllSmall s = { -7, 250, -1234, 54321, -123456789, UINT32_C(3456789012) };
    return s;
}
AbiMixedOrderA abi_make_mixed_a(void) {
    AbiMixedOrderA s = { 11, UINT64_C(0x1122334455667788), 2222, 333.5, 44 };
    return s;
}
AbiMixedOrderB abi_make_mixed_b(void) {
    AbiMixedOrderB s = { 123.75, UINT64_C(0x8877665544332211), 4321, 99, 88 };
    return s;
}
AbiFloatMix abi_make_float_mix(void) {
    AbiFloatMix s = { 1.25f, -2.5, 3.75L, 4.5f };
    return s;
}
AbiNested abi_make_nested(void) {
    AbiNested n;
    n.before = -123;
    n.inner.tag = 9;
    n.inner.value = 123456;
    n.inner.weight = 0.125;
    n.after = UINT64_C(0xFEDCBA9876543210);
    n.second.tag = 17;
    n.second.value = -654321;
    n.second.weight = -999.875;
    return n;
}
AbiArrayFields abi_make_array_fields(void) {
    AbiArrayFields a = {
        { 1, 2, 3, 127, 128, 254, 255 },
        { -1, 2, -300 },
        { 100000, -200000, 300000, -400000 },
        { 0.5f, -123.25f }
    };
    return a;
}
AbiLarge abi_make_large(void) {
    AbiLarge x;
    memset(&x, 0, sizeof x);
    x.id = UINT64_C(0x0123456789ABCDEF);
    memcpy(x.name, "abi-large-struct", sizeof "abi-large-struct");
    x.flags = 0xA5;
    for (size_t i = 0; i < 7; ++i) x.padding_probe[i] = (uint8_t)(0xF0u + i);
    for (size_t i = 0; i < 5; ++i) x.values[i] = (double)i * -1.25 + 0.5;
    for (size_t i = 0; i < 3; ++i) {
        x.nested[i].tag = (uint8_t)(i + 10);
        x.nested[i].value = (int32_t)(i * 1000) - 100;
        x.nested[i].weight = (double)i + 0.0625;
    }
    x.user_data = (void *)(uintptr_t)0x1234;
    x.length = 0x12345u;
    x.status = -77;
    return x;
}

int abi_sum_all_small(AbiAllSmall s) {
    return (int)s.a + (int)s.b + s.c + (int)s.d + s.e + (int)s.f;
}
double abi_score_mixed_a(AbiMixedOrderA s) {
    return (double)s.a + (double)s.b * 0.001 + (double)s.c
         + s.d + (double)s.e;
}
double abi_score_mixed_b(AbiMixedOrderB s) {
    return s.d + (double)s.b * 0.001 + (double)s.c
         + (double)s.a + (double)s.e;
}
double abi_score_nested(AbiNested n) {
    return (double)n.before + n.inner.weight + (double)n.inner.value
         + (double)n.after * 0.000001 + n.second.weight
         + (double)n.second.value + (double)n.inner.tag + (double)n.second.tag;
}
uint64_t abi_hash_array_fields(AbiArrayFields a) {
    uint64_t h = 1469598103934665603ULL;
    const unsigned char *p = (const unsigned char *)&a;
    for (size_t i = 0; i < sizeof a; ++i) {
        h ^= p[i];
        h *= 1099511628211ULL;
    }
    return h;
}
int abi_large_check(AbiLarge x) {
    return (x.id == UINT64_C(0x0123456789ABCDEF)
         && strcmp(x.name, "abi-large-struct") == 0
         && x.flags == 0xA5
         && x.nested[2].value == 1900
         && x.status == -77) ? 1 : 0;
}
void abi_mutate_nested(AbiNested *n, int32_t delta, double scale) {
    if (!n) return;
    n->before = (int16_t)(n->before + delta);
    n->inner.value += delta;
    n->inner.weight *= scale;
    n->after += (uint64_t)(delta < 0 ? -delta : delta);
    n->second.value -= delta;
    n->second.weight *= scale;
}

/* ------------------------------------------------------------------------- */
/* 4. Layout introspection: query size, alignment, and offsets from C          */
/* ------------------------------------------------------------------------- */

size_t abi_sizeof_char(void) { return sizeof(char); }
size_t abi_sizeof_short(void) { return sizeof(short); }
size_t abi_sizeof_int(void) { return sizeof(int); }
size_t abi_sizeof_long(void) { return sizeof(long); }
size_t abi_sizeof_long_long(void) { return sizeof(long long); }
size_t abi_sizeof_size_t(void) { return sizeof(size_t); }
size_t abi_sizeof_pointer(void) { return sizeof(void *); }
size_t abi_sizeof_bool(void) { return sizeof(bool); }
size_t abi_sizeof_float(void) { return sizeof(float); }
size_t abi_sizeof_double(void) { return sizeof(double); }
size_t abi_sizeof_long_double(void) { return sizeof(long double); }

#define ABI_LAYOUT_FUNCTIONS(Type, Name) \
    size_t abi_sizeof_##Name(void) { return sizeof(Type); } \
    size_t abi_alignof_##Name(void) { return _Alignof(Type); }

ABI_LAYOUT_FUNCTIONS(AbiAllSmall, all_small)
ABI_LAYOUT_FUNCTIONS(AbiMixedOrderA, mixed_order_a)
ABI_LAYOUT_FUNCTIONS(AbiMixedOrderB, mixed_order_b)
ABI_LAYOUT_FUNCTIONS(AbiFloatMix, float_mix)
ABI_LAYOUT_FUNCTIONS(AbiPointers, pointers)
ABI_LAYOUT_FUNCTIONS(AbiInner, inner)
ABI_LAYOUT_FUNCTIONS(AbiNested, nested)
ABI_LAYOUT_FUNCTIONS(AbiArrayFields, array_fields)
ABI_LAYOUT_FUNCTIONS(AbiLarge, large)
ABI_LAYOUT_FUNCTIONS(AbiLongNamesAndOddSizes, odd_sizes)

size_t abi_offset_mixed_a_a(void) { return offsetof(AbiMixedOrderA, a); }
size_t abi_offset_mixed_a_b(void) { return offsetof(AbiMixedOrderA, b); }
size_t abi_offset_mixed_a_c(void) { return offsetof(AbiMixedOrderA, c); }
size_t abi_offset_mixed_a_d(void) { return offsetof(AbiMixedOrderA, d); }
size_t abi_offset_mixed_a_e(void) { return offsetof(AbiMixedOrderA, e); }
size_t abi_offset_nested_inner(void) { return offsetof(AbiNested, inner); }
size_t abi_offset_nested_after(void) { return offsetof(AbiNested, after); }
size_t abi_offset_nested_second(void) { return offsetof(AbiNested, second); }
size_t abi_offset_large_name(void) { return offsetof(AbiLarge, name); }
size_t abi_offset_large_values(void) { return offsetof(AbiLarge, values); }
size_t abi_offset_large_nested(void) { return offsetof(AbiLarge, nested); }
size_t abi_offset_large_user_data(void) { return offsetof(AbiLarge, user_data); }
size_t abi_offset_large_length(void) { return offsetof(AbiLarge, length); }
size_t abi_offset_large_status(void) { return offsetof(AbiLarge, status); }

/* ------------------------------------------------------------------------- */
/* 5. Enums and integer-like constants                                        */
/* ------------------------------------------------------------------------- */

typedef enum AbiSmallEnum {
    ABI_ENUM_ZERO = 0,
    ABI_ENUM_ONE = 1,
    ABI_ENUM_NEGATIVE = -7,
    ABI_ENUM_BIG = 100000
} AbiSmallEnum;

typedef enum AbiFlags {
    ABI_FLAG_NONE = 0,
    ABI_FLAG_READ = 1u << 0,
    ABI_FLAG_WRITE = 1u << 1,
    ABI_FLAG_EXECUTE = 1u << 2,
    ABI_FLAG_HIDDEN = 1u << 7
} AbiFlags;

int abi_enum_echo(AbiSmallEnum e) { return (int)e; }
unsigned abi_flags_or(unsigned a, unsigned b) { return a | b; }
int abi_enum_size(void) { return (int)sizeof(AbiSmallEnum); }
int abi_enum_value_negative(void) { return ABI_ENUM_NEGATIVE; }
int abi_enum_value_big(void) { return ABI_ENUM_BIG; }

/* ------------------------------------------------------------------------- */
/* 6. Unions and tagged unions                                                 */
/* ------------------------------------------------------------------------- */

typedef union AbiScalarUnion {
    uint64_t u64;
    int64_t i64;
    double f64;
    uint32_t u32[2];
    uint8_t bytes[8];
} AbiScalarUnion;

typedef struct AbiTaggedUnion {
    uint32_t kind;
    union {
        int64_t as_i64;
        double as_f64;
        void *as_ptr;
        uint8_t as_bytes[16];
    } value;
} AbiTaggedUnion;

AbiScalarUnion abi_union_from_u64(uint64_t x) {
    AbiScalarUnion u;
    u.u64 = x;
    return u;
}
AbiScalarUnion abi_union_from_double(double x) {
    AbiScalarUnion u;
    u.f64 = x;
    return u;
}
uint64_t abi_union_read_u64(AbiScalarUnion u) { return u.u64; }
double abi_union_read_double(AbiScalarUnion u) { return u.f64; }
size_t abi_sizeof_scalar_union(void) { return sizeof(AbiScalarUnion); }
size_t abi_alignof_scalar_union(void) { return _Alignof(AbiScalarUnion); }
size_t abi_sizeof_tagged_union(void) { return sizeof(AbiTaggedUnion); }
size_t abi_offset_tagged_union_value(void) { return offsetof(AbiTaggedUnion, value); }

/* ------------------------------------------------------------------------- */
/* 7. Pointer levels, buffers, strings, and arrays                             */
/* ------------------------------------------------------------------------- */

int abi_read_int_ptr(const int *p) { return p ? *p : INT_MIN; }
void abi_write_int_ptr(int *p, int value) { if (p) *p = value; }
double abi_read_double_ptr(const double *p) { return p ? *p : -DBL_MAX; }
void abi_write_double_ptr(double *p, double value) { if (p) *p = value; }
void abi_set_ptr(void **slot, void *value) { if (slot) *slot = value; }
void *abi_get_ptr(void *const *slot) { return slot ? *slot : NULL; }
size_t abi_strlen_or_zero(const char *s) { return s ? strlen(s) : 0; }
int abi_copy_string(char *dst, size_t dst_capacity, const char *src) {
    if (!dst || dst_capacity == 0 || !src) return -1;
    size_t n = strlen(src);
    if (n >= dst_capacity) {
        memcpy(dst, src, dst_capacity - 1);
        dst[dst_capacity - 1] = '\0';
        return 1; /* Truncated. */
    }
    memcpy(dst, src, n + 1);
    return 0;
}
uint64_t abi_sum_u8(const uint8_t *values, size_t count) {
    if (!values && count) return UINT64_MAX;
    uint64_t sum = 0;
    for (size_t i = 0; i < count; ++i) sum += values[i];
    return sum;
}
int64_t abi_sum_i32(const int32_t *values, size_t count) {
    if (!values && count) return INT64_MIN;
    int64_t sum = 0;
    for (size_t i = 0; i < count; ++i) sum += values[i];
    return sum;
}
double abi_sum_f64(const double *values, size_t count) {
    if (!values && count) return -DBL_MAX;
    double sum = 0.0;
    for (size_t i = 0; i < count; ++i) sum += values[i];
    return sum;
}
void abi_fill_u8(uint8_t *dst, size_t count, uint8_t start, uint8_t step) {
    if (!dst) return;
    for (size_t i = 0; i < count; ++i) dst[i] = (uint8_t)(start + i * step);
}
void abi_fill_i32(int32_t *dst, size_t count, int32_t start, int32_t step) {
    if (!dst) return;
    for (size_t i = 0; i < count; ++i) dst[i] = start + (int32_t)i * step;
}
void abi_reverse_bytes(uint8_t *dst, const uint8_t *src, size_t count) {
    if (!dst || !src) return;
    for (size_t i = 0; i < count; ++i) dst[i] = src[count - 1 - i];
}
int abi_pointer_difference(const int *a, const int *b) {
    if (!a || !b) return INT_MIN;
    return (int)(a - b); /* Both pointers must refer into the same array. */
}

/* Pointer-to-array and array-of-pointers cases. */
int abi_sum_fixed_2x3(const int (*matrix)[3]) {
    if (!matrix) return INT_MIN;
    return matrix[0][0] + matrix[0][1] + matrix[0][2]
         + matrix[1][0] + matrix[1][1] + matrix[1][2];
}
size_t abi_sum_cstrings(const char *const *strings, size_t count) {
    if (!strings && count) return SIZE_MAX;
    size_t total = 0;
    for (size_t i = 0; i < count; ++i)
        if (strings[i]) total += strlen(strings[i]);
    return total;
}

/* ------------------------------------------------------------------------- */
/* 8. Out parameters and multi-level pointer mutation                          */
/* ------------------------------------------------------------------------- */

int abi_out_two_ints(int seed, int *out_a, int *out_b) {
    if (!out_a || !out_b) return -1;
    *out_a = seed + 111;
    *out_b = seed * 3;
    return 0;
}
int abi_out_many(
    uint8_t *u8, int16_t *i16, uint32_t *u32, int64_t *i64,
    float *f32, double *f64, const char **text, void **ptr)
{
    if (!u8 || !i16 || !u32 || !i64 || !f32 || !f64 || !text || !ptr) return -1;
    *u8 = 0xE7;
    *i16 = -23456;
    *u32 = UINT32_C(0xDEADBEEF);
    *i64 = -INT64_C(0x123456789ABCDEF);
    *f32 = 6.125f;
    *f64 = -0.0009765625;
    *text = "C says hello";
    *ptr = (void *)(uintptr_t)0xBEEF;
    return 0;
}
void abi_increment_pointer_chain(int ***p, int amount) {
    if (p && *p && **p) ***p += amount;
}
int abi_set_and_return_pointer(int *target, int **out_pointer, int new_value) {
    if (!target || !out_pointer) return -1;
    *target = new_value;
    *out_pointer = target;
    return *target;
}

/* ------------------------------------------------------------------------- */
/* 9. Function pointers and callbacks                                          */
/* ------------------------------------------------------------------------- */

typedef int (*AbiIntBinaryFn)(int, int);
typedef double (*AbiDoubleUnaryFn)(double);
typedef void (*AbiVisitFn)(size_t index, int value, void *context);
typedef int (*AbiTransformFn)(int value, void *context);
typedef void (*AbiLogFn)(int level, const char *message, void *context);

int abi_add(int a, int b) { return a + b; }
int abi_subtract(int a, int b) { return a - b; }
int abi_multiply(int a, int b) { return a * b; }
double abi_square_double(double x) { return x * x; }
double abi_sin_approx_input(double x) { return x; } /* Avoids a libm dependency. */

int abi_call_binary(AbiIntBinaryFn fn, int a, int b) {
    return fn ? fn(a, b) : INT_MIN;
}
double abi_call_unary(AbiDoubleUnaryFn fn, double x) {
    return fn ? fn(x) : -DBL_MAX;
}
int abi_apply_transform(int *values, size_t count, AbiTransformFn fn, void *context) {
    if ((!values && count) || !fn) return -1;
    for (size_t i = 0; i < count; ++i) values[i] = fn(values[i], context);
    return (int)count;
}
int abi_visit_values(const int *values, size_t count, AbiVisitFn fn, void *context) {
    if ((!values && count) || !fn) return -1;
    for (size_t i = 0; i < count; ++i) fn(i, values[i], context);
    return (int)count;
}
void abi_log_twice(AbiLogFn fn, void *context) {
    if (!fn) return;
    fn(1, "first callback", context);
    fn(2, "second callback", context);
}

/* C stores a callback and context so callers can test callback lifetime. */
static AbiIntBinaryFn g_saved_binary_fn;
static void *g_saved_binary_context;

void abi_save_binary_callback(AbiIntBinaryFn fn, void *context) {
    g_saved_binary_fn = fn;
    g_saved_binary_context = context;
}
int abi_call_saved_binary(int a, int b) {
    if (!g_saved_binary_fn) return INT_MIN;
    /* Context is intentionally only retained for pointer-lifetime tests. */
    (void)g_saved_binary_context;
    return g_saved_binary_fn(a, b);
}
void abi_clear_saved_callback(void) {
    g_saved_binary_fn = NULL;
    g_saved_binary_context = NULL;
}

/* ------------------------------------------------------------------------- */
/* 10. Variadic functions                                                     */
/* ------------------------------------------------------------------------- */

double abi_variadic_sum_ints(int count, ...) {
    if (count < 0) return -DBL_MAX;
    va_list ap;
    va_start(ap, count);
    double sum = 0.0;
    for (int i = 0; i < count; ++i) sum += (double)va_arg(ap, int);
    va_end(ap);
    return sum;
}
double abi_variadic_sum_doubles(int count, ...) {
    if (count < 0) return -DBL_MAX;
    va_list ap;
    va_start(ap, count);
    double sum = 0.0;
    for (int i = 0; i < count; ++i) sum += va_arg(ap, double);
    va_end(ap);
    return sum;
}
double abi_variadic_mixed(int count, ...) {
    /*
     * Each pair is: int tag, then:
     *   tag 0 => int
     *   tag 1 => double
     *   tag 2 => const char *
     * This is a custom protocol, not type-safe C reflection.
     */
    if (count < 0) return -DBL_MAX;
    va_list ap;
    va_start(ap, count);
    double result = 0.0;
    for (int i = 0; i < count; ++i) {
        int tag = va_arg(ap, int);
        if (tag == 0) result += (double)va_arg(ap, int);
        else if (tag == 1) result += va_arg(ap, double);
        else if (tag == 2) {
            const char *s = va_arg(ap, const char *);
            if (s) result += (double)strlen(s);
        } else {
            va_end(ap);
            return -DBL_MAX;
        }
    }
    va_end(ap);
    return result;
}

/* ------------------------------------------------------------------------- */
/* 11. Return structs by value and pass structs by value                      */
/* ------------------------------------------------------------------------- */

typedef struct AbiTwoInts { int a; int b; } AbiTwoInts;
typedef struct AbiThreeDoubles { double a; double b; double c; } AbiThreeDoubles;
typedef struct AbiEightBytes { uint8_t bytes[8]; } AbiEightBytes;
typedef struct AbiIntDouble { int i; double d; } AbiIntDouble;
typedef struct AbiDoubleInt { double d; int i; } AbiDoubleInt;
typedef struct AbiFourFloats { float a, b, c, d; } AbiFourFloats;
typedef struct AbiFiveInts { int a, b, c, d, e; } AbiFiveInts;
typedef struct AbiTwoU64 { uint64_t a, b; } AbiTwoU64;
typedef struct AbiMixedReturn {
    uint8_t a;
    double b;
    int32_t c;
    uint64_t d;
    float e;
} AbiMixedReturn;

AbiTwoInts abi_make_two_ints(int a, int b) { AbiTwoInts x = { a, b }; return x; }
int abi_sum_two_ints(AbiTwoInts x) { return x.a + x.b; }
AbiThreeDoubles abi_make_three_doubles(double a, double b, double c) {
    AbiThreeDoubles x = { a, b, c }; return x;
}
double abi_sum_three_doubles(AbiThreeDoubles x) { return x.a + x.b + x.c; }
AbiEightBytes abi_make_eight_bytes(uint8_t start) {
    AbiEightBytes x;
    for (size_t i = 0; i < 8; ++i) x.bytes[i] = (uint8_t)(start + i);
    return x;
}
uint64_t abi_pack_eight_bytes(AbiEightBytes x) {
    uint64_t result = 0;
    for (size_t i = 0; i < 8; ++i) result |= (uint64_t)x.bytes[i] << (i * 8);
    return result;
}
AbiIntDouble abi_make_int_double(int i, double d) { AbiIntDouble x = { i, d }; return x; }
AbiDoubleInt abi_make_double_int(double d, int i) { AbiDoubleInt x = { d, i }; return x; }
double abi_score_int_double(AbiIntDouble x) { return (double)x.i + x.d; }
double abi_score_double_int(AbiDoubleInt x) { return x.d + (double)x.i; }
AbiFourFloats abi_make_four_floats(float a, float b, float c, float d) {
    AbiFourFloats x = { a, b, c, d }; return x;
}
float abi_sum_four_floats(AbiFourFloats x) { return x.a + x.b + x.c + x.d; }
AbiFiveInts abi_make_five_ints(int a, int b, int c, int d, int e) {
    AbiFiveInts x = { a, b, c, d, e }; return x;
}
int abi_sum_five_ints(AbiFiveInts x) { return x.a + x.b + x.c + x.d + x.e; }
AbiTwoU64 abi_make_two_u64(uint64_t a, uint64_t b) { AbiTwoU64 x = { a, b }; return x; }
uint64_t abi_xor_two_u64(AbiTwoU64 x) { return x.a ^ x.b; }
AbiMixedReturn abi_make_mixed_return(uint8_t a, double b, int32_t c, uint64_t d, float e) {
    AbiMixedReturn x = { a, b, c, d, e }; return x;
}
double abi_score_mixed_return(AbiMixedReturn x) {
    return (double)x.a + x.b + (double)x.c + (double)x.d * 0.0001 + (double)x.e;
}

/* ------------------------------------------------------------------------- */
/* 12. Bit-fields: use only for exploratory tests, not portable serialization */
/* ------------------------------------------------------------------------- */

typedef struct AbiBitFields {
    unsigned int low3 : 3;
    unsigned int next5 : 5;
    signed int signed7 : 7;
    unsigned int next9 : 9;
    unsigned int full_width : 8;
} AbiBitFields;

AbiBitFields abi_make_bitfields(void) {
    AbiBitFields x;
    x.low3 = 5;
    x.next5 = 19;
    x.signed7 = -23;
    x.next9 = 257;
    x.full_width = 0xA6;
    return x;
}
unsigned abi_bitfields_checksum(AbiBitFields x) {
    return x.low3 + x.next5 * 3u + (unsigned)(x.signed7 + 64) * 5u
         + x.next9 * 7u + x.full_width * 11u;
}
size_t abi_sizeof_bitfields(void) { return sizeof(AbiBitFields); }
size_t abi_alignof_bitfields(void) { return _Alignof(AbiBitFields); }

/* ------------------------------------------------------------------------- */
/* 13. Packed struct extension, when supported                                 */
/* ------------------------------------------------------------------------- */

#if defined(__GNUC__) || defined(__clang__)
typedef struct __attribute__((packed)) AbiPacked {
    uint8_t a;
    uint32_t b;
    uint16_t c;
    double d;
} AbiPacked;
#define ABI_HAS_PACKED 1
#else
typedef struct AbiPacked {
    uint8_t a;
    uint32_t b;
    uint16_t c;
    double d;
} AbiPacked;
#define ABI_HAS_PACKED 0
#endif

AbiPacked abi_make_packed(void) {
    AbiPacked x;
    x.a = 0xAB;
    x.b = UINT32_C(0x12345678);
    x.c = UINT16_C(0x9ABC);
    x.d = 123.5;
    return x;
}
double abi_score_packed(AbiPacked x) {
    return (double)x.a + (double)x.b * 0.000001 + (double)x.c + x.d;
}
int abi_has_packed_extension(void) { return ABI_HAS_PACKED; }
size_t abi_sizeof_packed(void) { return sizeof(AbiPacked); }
size_t abi_alignof_packed(void) { return _Alignof(AbiPacked); }
size_t abi_offset_packed_b(void) { return offsetof(AbiPacked, b); }
size_t abi_offset_packed_d(void) { return offsetof(AbiPacked, d); }

/* ------------------------------------------------------------------------- */
/* 14. Fixed-width arrays and matrix-by-value                                  */
/* ------------------------------------------------------------------------- */

typedef struct AbiMatrix2x2 {
    double m[2][2];
} AbiMatrix2x2;

AbiMatrix2x2 abi_make_matrix(double a, double b, double c, double d) {
    AbiMatrix2x2 m = { { { a, b }, { c, d } } };
    return m;
}
double abi_matrix_determinant(AbiMatrix2x2 m) {
    return m.m[0][0] * m.m[1][1] - m.m[0][1] * m.m[1][0];
}
void abi_matrix_transpose(AbiMatrix2x2 *m) {
    if (!m) return;
    double t = m->m[0][1];
    m->m[0][1] = m->m[1][0];
    m->m[1][0] = t;
}

/* ------------------------------------------------------------------------- */
/* 15. Opaque object pattern and ownership/lifetime checks                     */
/* ------------------------------------------------------------------------- */

typedef struct AbiOpaque AbiOpaque;
struct AbiOpaque {
    uint64_t magic;
    int value;
    double weight;
};

AbiOpaque *abi_opaque_create(int value, double weight) {
    AbiOpaque *p = (AbiOpaque *)malloc(sizeof *p);
    if (!p) return NULL;
    p->magic = UINT64_C(0xAB1AB1AB1AB1AB1A);
    p->value = value;
    p->weight = weight;
    return p;
}
void abi_opaque_destroy(AbiOpaque *p) {
    if (!p) return;
    p->magic = 0;
    free(p);
}
int abi_opaque_get_value(const AbiOpaque *p) {
    return p && p->magic == UINT64_C(0xAB1AB1AB1AB1AB1A) ? p->value : INT_MIN;
}
double abi_opaque_get_weight(const AbiOpaque *p) {
    return p && p->magic == UINT64_C(0xAB1AB1AB1AB1AB1A) ? p->weight : -DBL_MAX;
}
int abi_opaque_set(AbiOpaque *p, int value, double weight) {
    if (!p || p->magic != UINT64_C(0xAB1AB1AB1AB1AB1A)) return -1;
    p->value = value;
    p->weight = weight;
    return 0;
}

/* malloc/free declarations come from stdlib.h (included above). */

/* ------------------------------------------------------------------------- */
/* 16. Error/status conventions and C-string diagnostics                      */
/* ------------------------------------------------------------------------- */

typedef enum AbiStatus {
    ABI_STATUS_OK = 0,
    ABI_STATUS_NULL = -1,
    ABI_STATUS_RANGE = -2,
    ABI_STATUS_BAD_VALUE = -3
} AbiStatus;

AbiStatus abi_validate_range(int value, int minimum, int maximum) {
    if (minimum > maximum) return ABI_STATUS_BAD_VALUE;
    if (value < minimum || value > maximum) return ABI_STATUS_RANGE;
    return ABI_STATUS_OK;
}
const char *abi_status_string(AbiStatus status) {
    switch (status) {
        case ABI_STATUS_OK: return "ok";
        case ABI_STATUS_NULL: return "null";
        case ABI_STATUS_RANGE: return "range";
        case ABI_STATUS_BAD_VALUE: return "bad-value";
        default: return "unknown";
    }
}

/* ------------------------------------------------------------------------- */
/* 17. Volatile, const, and pointer qualifiers                                 */
/* ------------------------------------------------------------------------- */

int abi_read_volatile_int(const volatile int *p) { return p ? *p : INT_MIN; }
void abi_write_volatile_int(volatile int *p, int v) { if (p) *p = v; }
const int *abi_return_const_int_array(void) {
    static const int values[] = { -100, -1, 0, 1, 100, 123456789 };
    return values;
}
size_t abi_return_const_int_array_count(void) { return 6; }
int abi_read_const_int_array(size_t index) {
    const int *values = abi_return_const_int_array();
    return index < abi_return_const_int_array_count() ? values[index] : INT_MIN;
}

/* ------------------------------------------------------------------------- */
/* 18. ABI facts and diagnostic helpers                                        */
/* ------------------------------------------------------------------------- */

const char *abi_compiler_family(void) {
#if defined(__clang__)
    return "clang";
#elif defined(__GNUC__)
    return "gcc-compatible";
#elif defined(_MSC_VER)
    return "msvc";
#else
    return "unknown";
#endif
}
const char *abi_platform_hint(void) {
#if defined(_WIN64)
    return "windows-64";
#elif defined(_WIN32)
    return "windows-32";
#elif defined(__APPLE__) && defined(__MACH__)
    return "apple";
#elif defined(__linux__) && defined(__x86_64__)
    return "linux-x86_64";
#elif defined(__linux__) && defined(__aarch64__)
    return "linux-aarch64";
#elif defined(__linux__)
    return "linux-other";
#else
    return "other";
#endif
}
uint64_t abi_integer_width_mask(unsigned bits) {
    if (bits == 0) return 0;
    if (bits >= 64) return UINT64_MAX;
    return (UINT64_C(1) << bits) - 1;
}
int abi_compare_bytes(const void *a, const void *b, size_t n) {
    if ((!a || !b) && n != 0) return INT_MIN;
    return memcmp(a, b, n);
}
void abi_memset_pattern(void *p, size_t n, uint8_t byte) {
    if (p) memset(p, byte, n);
}

/* ------------------------------------------------------------------------- */
/* 19. Standalone smoke test                                                    */
/* ------------------------------------------------------------------------- */

#ifdef ABI_STRESS_MAIN
static int smoke_failures;

#define CHECK(expr) do { \
    if (!(expr)) { \
        fprintf(stderr, "FAIL: %s (%s:%d)\n", #expr, __FILE__, __LINE__); \
        ++smoke_failures; \
    } \
} while (0)

static int smoke_callback_add(int a, int b) { return a + b + 10; }

int main(void) {
    printf("compiler: %s\n", abi_compiler_family());
    printf("platform: %s\n", abi_platform_hint());
    printf("sizeof(void*)=%zu sizeof(long)=%zu sizeof(size_t)=%zu\n",
           sizeof(void *), sizeof(long), sizeof(size_t));

    CHECK(abi_echo_i8(-42) == -42);
    CHECK(abi_echo_uint64(UINT64_C(0xFEDCBA9876543210)) == UINT64_C(0xFEDCBA9876543210));
    CHECK(abi_echo_f64(12.5) == 12.5);

    AbiAllSmall s = abi_make_all_small();
    CHECK(s.a == -7 && s.b == 250 && s.c == -1234);
    CHECK(abi_sum_two_ints(abi_make_two_ints(10, 20)) == 30);
    CHECK(abi_sum_three_doubles(abi_make_three_doubles(1.0, 2.0, 3.0)) == 6.0);
    CHECK(abi_call_binary(smoke_callback_add, 2, 3) == 15);

    int values[] = { 1, 2, 3, 4 };
    CHECK(abi_sum_i32(values, 4) == 10);
    CHECK(abi_copy_string((char[8]){0}, 8, "hello") == 0);
    CHECK(abi_validate_range(5, 0, 10) == ABI_STATUS_OK);
    CHECK(abi_validate_range(50, 0, 10) == ABI_STATUS_RANGE);

    printf("AbiAllSmall size=%zu align=%zu\n",
           abi_sizeof_all_small(), abi_alignof_all_small());
    printf("AbiMixedOrderA size=%zu align=%zu\n",
           abi_sizeof_mixed_order_a(), abi_alignof_mixed_order_a());

    if (smoke_failures) {
        fprintf(stderr, "%d smoke test(s) failed\n", smoke_failures);
        return 1;
    }
    puts("smoke tests passed");
    return 0;
}
#endif
