// cg_abi.odin
package main

import "core:fmt"
import "core:strings"

// ============================================================================
// Unified ABI layer.
//
// Every function definition, declaration, and call goes through
// cg_abi_lower_signature instead of branching on `is_external`.
//
//   .SysV  — System V AMD64 psABI, matching clang's x86_64 lowering
//            (Linux, macOS, BSD).
//   .Win64 — Microsoft x64 calling convention as used by MSVC (and
//            clang-cl / MinGW): 4 positional register slots, aggregates
//            of 1/2/4/8 bytes go in an integer register, everything else
//            by reference to a caller-owned copy.
//   .None  — "the target's native ABI": resolves to `abi_native_kind`.
//            Internal Gala-to-Gala calls use this, so they get exactly the
//            same treatment as C calls on the same target.
//
// Call-site obligations per mode:
//
//   Arg  .Direct    pass `<coerced_type> <ext>`; if needs_coercion, store the
//                   value to a temp of max(type_size, coerced_size) bytes and
//                   load it back as coerced_type.
//   Arg  .ByVal     (SysV) pass `ptr byval(%T) align <byval_align>`.
//   Arg  .Indirect  (Win64) copy the value into a FRESH temp alloca aligned to
//                   indirect_align and pass a plain `ptr` (no byval). The
//                   callee owns and may modify that copy.
//   Arg  .Ignore    (SysV empty aggregate) pass nothing.
//   Ret  .Direct    return `<ext> <coerced_type>`; coerce through memory if
//                   needs_coercion.
//   Ret  .Indirect  hidden first param `ptr sret(%T) align <sret_align>`,
//                   function returns void.
//   Ret  .Ignore    function returns void; caller materialises a zero value.
//
// C-variadic calls: start from `sig.regs_after_fixed` and lower each extra
// argument with cg_abi_lower_c_variadic_arg so SysV register accounting
// continues correctly across the `...`.
// ============================================================================

AbiKind :: enum {
    None,
    SysV,
    Win64,
}

// What `.None` resolves to: the ABI kind stored in the compiler context.
// (Eventually this becomes a per-type/per-function decision; everything
// funnels through here so that's the only place that has to change.)
abi_resolve_kind :: proc(kind: AbiKind) -> AbiKind {
    if kind != .None do return kind
    native := get_ctx().abi
    if native == .None do return .SysV
    return native
}

abi_kind_from_triple :: proc(triple: string) -> AbiKind {
    if strings.contains(triple, "windows") ||
       strings.contains(triple, "win32") ||
       strings.contains(triple, "mingw") ||
       strings.contains(triple, "cygwin") {
        return .Win64
    }
    return .SysV
}
// MSVC x64 target.
abi_triple :: proc(kind: AbiKind) -> string {
    if abi_resolve_kind(kind) == .Win64 do return "x86_64-pc-windows-msvc"
    return "x86_64-pc-linux-gnu"
}

abi_datalayout :: proc(kind: AbiKind) -> string {
    if abi_resolve_kind(kind) == .Win64 {
        return "e-m:w-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
    }
    return "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
}

// LLVM calling-convention keyword (with trailing space) to put before the
// return type in define/declare/call when `kind` differs from the module's
// native ABI. Empty when they match (plain `ccc` already is the native ABI).
abi_llvm_callconv :: proc(kind: AbiKind) -> string {
    k := abi_resolve_kind(kind)
    if k == abi_resolve_kind(.None) do return ""
    switch k {
    case .SysV:  return "x86_64_sysvcc "
    case .Win64: return "win64cc "
    case .None:  return ""
    }
    return ""
}

AbiClass :: enum {
    NoClass,
    Integer,
    Sse,
    Memory,
}

AbiEightbyte :: struct {
    class:     AbiClass,
    llvm_type: string,
}

// Result of SysV-classifying an aggregate (struct / fixed array / union /
// slice / string / any).
AbiClassified :: struct {
    is_memory:      bool,
    is_empty:       bool, // size 0 -> ignored as an argument / void return
    eightbytes:     [2]AbiEightbyte,
    num_eightbytes: int,    // 0 if memory/empty, else 1 or 2
    llvm_type:      string, // full coerced type ("i64", "{ double, i32 }", ...)
    coerced_size:   int,    // LLVM alloc size of llvm_type
    needed_int:     int,    // GPRs needed to pass in registers
    needed_sse:     int,    // XMM registers needed to pass in registers
    size:           int,
    align:          int,
}

merge_abi_class :: proc(a, b: AbiClass) -> AbiClass {
    if a == b do return a
    if a == .NoClass do return b
    if b == .NoClass do return a
    if a == .Memory || b == .Memory do return .Memory
    if a == .Integer || b == .Integer do return .Integer
    return .Sse
}

scalar_abi_class :: proc(k: TypeKind) -> AbiClass {
    #partial switch k {
    case .Flt64, .Flt32, .Flt16:
        return .Sse
    case:
        // ints, Byte, Rune, Bool, Pointer, Function — and Flt_8, which is
        // lowered as a plain i8 (see scalar_llvm_str) and so travels in an
        // integer register.
        return .Integer
    }
}

abi_is_scalar_kind :: proc(k: TypeKind) -> bool {
    #partial switch k {
    case .UInt64, .UInt32, .UInt16, .UInt_8,
         .Int64, .Int32, .Int16, .Int_8,
         .Flt64, .Flt32, .Flt16, .Flt_8,
         .Bool, .Byte, .Rune, .Pointer, .Function:
        return true
    }
    return false
}

abi_is_slice_like_kind :: proc(k: TypeKind) -> bool {
    return k == .Slice || k == .String || k == .Any
}

abi_require_aggregate :: proc(type_id: TypeId) {
    ty := get_type(type_id)
    #partial switch ty.kind {
    case .Struct, .FixedSizeArray, .Union, .Slice, .String, .Any:
        return
    }
    debugln(type_id, ty, get_span(type_id))
    highlight_lines(get_span(type_id))
    gala_panic("abi: unhandled kind", ty.kind)
}

// Naive sequential (no field reordering) struct layout — natural C-style
// alignment, no explicit #packed support. Identical on SysV and Win64.
type_align_of :: proc(type_id: TypeId) -> int {
    ty := get_type(type_id)
    #partial switch ty.kind {
    case .UInt64, .Int64, .Flt64, .Pointer, .Function:
        return 8
    case .UInt32, .Int32, .Flt32, .Rune:
        return 4
    case .UInt16, .Int16, .Flt16:
        return 2
    case .UInt_8, .Int_8, .Flt_8, .Byte, .Bool:
        return 1
    case .Slice, .String, .Any:
        // `any` is boxed as { ptr, i64 }, same header shape as a slice/string.
        return 8
    case .FixedSizeArray:
        return type_align_of(ty.fixed_size_array.type)
    case .Struct:
        a := 1
        for f in ty.structure.fields {
            fa := type_align_of(f.type)
            if fa > a do a = fa
        }
        return a
    case .Union:
        a := 1
        for f in ty.union_fields {
            fa := type_align_of(f.type)
            if fa > a do a = fa
        }
        return a
    case:
        gala_panic("type_align_of: unhandled kind", ty.kind)
    }
    return 1
}

// Offsets of each field of a struct, laid out sequentially with natural
// alignment padding (no reordering).
struct_field_offsets :: proc(type_id: TypeId) -> []int {
    ty := get_type(type_id)
    assert(ty.kind == .Struct)
    offsets := make([]int, len(ty.structure.fields), allocator = context.temp_allocator)
    off := 0
    for f, i in ty.structure.fields {
        fa := type_align_of(f.type)
        if off % fa != 0 {
            off += fa - (off % fa)
        }
        offsets[i] = off
        off += type_size(f.type)
    }
    return offsets
}

abi_content_size :: proc(type_id: TypeId) -> int {
    ty := get_type(type_id)
    #partial switch ty.kind {
    case .Struct: {
        n := len(ty.structure.fields)
        if n == 0 do return 0
        offsets := struct_field_offsets(type_id)
        last := n - 1
        return offsets[last] + abi_content_size(ty.structure.fields[last].type)
    }
    case .Union: {
        biggest := 0
        for f in ty.union_fields {
            biggest = max(biggest, abi_content_size(f.type))
        }
        return biggest
    }
    case .FixedSizeArray: {
        if ty.fixed_size_array.size == 0 do return 0
        elem_size := type_size(ty.fixed_size_array.type)
        return (ty.fixed_size_array.size - 1) * elem_size + abi_content_size(ty.fixed_size_array.type)
    }
    case:
        return type_size(type_id)
    }
}

// ----------------------------------------------------------------------------
// LLVM type helpers
// ----------------------------------------------------------------------------

// Integer type covering exactly `nbytes` bytes. Clang uses odd-width
// integers (i24, i40, i48, i56) for 3/5/6/7-byte remainders so the coerced
// load/store never touches bytes past the end of the source object.
int_class_llvm_type :: proc(nbytes: int) -> string {
    switch nbytes {
    case 1: return "i8"
    case 2: return "i16"
    case 3: return "i24"
    case 4: return "i32"
    case 5: return "i40"
    case 6: return "i48"
    case 7: return "i56"
    case 8: return "i64"
    case: return "i64"
    }
}

// Scalar LLVM type strings, hardcoded rather than routed through
// ty_to_llvm_str/CGCtx — classification doesn't always have a CGCtx handy.
scalar_llvm_str :: proc(type_id: TypeId) -> string {
    ty := get_type(type_id)
    #partial switch ty.kind {
    case .Pointer, .Function: return "ptr"
    case .Flt64: return "double"
    case .Flt32: return "float"
    case .Flt16: return "half"
    case .Flt_8: return "i8" // no dedicated llvm 8-bit float type in use here
    case .Int64, .UInt64: return "i64"
    case .Int32, .UInt32: return "i32"
    case .Int16, .UInt16: return "i16"
    case .Int_8, .UInt_8, .Byte: return "i8"
    case .Rune: return "i32"
    case .Bool: return "i1"
    case:
        debugln(type_id, ty, get_span(type_id))
        highlight_lines(get_span(type_id))
        gala_panic("scalar_llvm_str: unhandled scalar kind", ty.kind)
    }
    return ""
}

// (alloc size, ABI alignment) on x86-64 for every type string this file can
// produce as a coercion part.
abi_llvm_type_size_align :: proc(t: string) -> (int, int) {
    switch t {
    case "i8":
        return 1, 1
    case "i16", "half":
        return 2, 2
    case "i24", "i32", "float", "<2 x half>":
        return 4, 4
    case "i40", "i48", "i56", "i64", "ptr", "double", "<2 x float>", "<4 x half>":
        return 8, 8
    }
    gala_panic("abi_llvm_type_size_align: unknown coercion type", t)
}

abi_align_up :: proc(x, a: int) -> int {
    return (x + a - 1) / a * a
}

// ----------------------------------------------------------------------------
// Scalar extension attributes
// ----------------------------------------------------------------------------

// SysV (clang): anything narrower than 32 bits is extended to 32 bits by the
// producer, and clang/gcc consumers rely on that.
//
//   bool, unsigned char/short -> "zeroext"
//   signed char/short         -> "signext"
//
// Flt_8 is lowered as a plain i8, so it is treated as unsigned.
abi_scalar_ext :: proc(type_id: TypeId) -> string {
    ty := get_type(type_id)
    #partial switch ty.kind {
    case .Bool, .Byte, .UInt_8, .UInt16, .Flt_8:
        return "zeroext"
    case .Int_8, .Int16:
        return "signext"
    }
    return ""
}

// MS x64 never extends small integers (upper bits are undefined), except
// that clang always marks bool as zeroext.
abi_scalar_ext_for :: proc(kind: AbiKind, type_id: TypeId) -> string {
    if abi_resolve_kind(kind) == .Win64 {
        return get_type(type_id).kind == .Bool ? "zeroext" : ""
    }
    return abi_scalar_ext(type_id)
}

// ----------------------------------------------------------------------------
// SysV classification
// ----------------------------------------------------------------------------

// A scalar leaf of a flattened aggregate. Union members all contribute
// leaves at the same offset.
AbiLeaf :: struct {
    offset: int,
    size:   int,
    kind:   TypeKind,
}

abi_collect_leaves :: proc(type_id: TypeId, base: int, out: ^[dynamic]AbiLeaf) {
    ty := get_type(type_id)
    #partial switch ty.kind {
    case .Struct:
        offsets := struct_field_offsets(type_id)
        for f, i in ty.structure.fields {
            abi_collect_leaves(f.type, base + offsets[i], out)
        }
    case .Union:
        for f in ty.union_fields {
            abi_collect_leaves(f.type, base, out)
        }
    case .FixedSizeArray:
        elem := ty.fixed_size_array.type
        elem_size := type_size(elem)
        for i in 0 ..< ty.fixed_size_array.size {
            abi_collect_leaves(elem, base + i * elem_size, out)
        }
    case .Slice, .String, .Any:
        // { ptr, i64 }. For `any` the second word is a typeid tag, but it is
        // still an 8-byte INTEGER word.
        append(out, AbiLeaf{offset = base, size = 8, kind = .Pointer})
        append(out, AbiLeaf{offset = base + 8, size = 8, kind = .Int64})
    case:
        append(out, AbiLeaf{offset = base, size = type_size(type_id), kind = ty.kind})
    }
}

abi_is_fp_kind :: proc(k: TypeKind) -> bool {
    return k == .Flt16 || k == .Flt32 || k == .Flt64
}

// True if no leaf has data in the byte range [start, end).
abi_no_user_data :: proc(leaves: []AbiLeaf, start, end: int) -> bool {
    for l in leaves {
        if l.size == 0 do continue
        if l.offset < end && l.offset + l.size > start do return false
    }
    return true
}

// Largest FP leaf starting exactly at `off` (clang's getFPTypeAtOffset).
abi_fp_leaf_at :: proc(leaves: []AbiLeaf, off: int) -> (TypeKind, bool) {
    best: TypeKind
    best_size := 0
    found := false
    for l in leaves {
        if l.offset != off || !abi_is_fp_kind(l.kind) do continue
        if !found || l.size > best_size {
            best = l.kind
            best_size = l.size
            found = true
        }
    }
    return best, found
}

abi_fp_kind_size :: proc(k: TypeKind) -> int {
    #partial switch k {
    case .Flt16: return 2
    case .Flt32: return 4
    }
    return 8
}

// Mirrors clang's X86_64ABIInfo::GetSSETypeAtOffset.
// `source_size` = total type size minus the eightbyte's start offset.
abi_sse_type_at_offset :: proc(leaves: []AbiLeaf, off: int, source_size: int) -> string {
    t0, ok0 := abi_fp_leaf_at(leaves, off)
    if !ok0 || t0 == .Flt64 do return "double"

    t0_size := abi_fp_kind_size(t0)
    t1: TypeKind
    ok1 := false
    if source_size > t0_size {
        t1, ok1 = abi_fp_leaf_at(leaves, off + t0_size)
    }
    if !ok1 {
        // half followed by a float: the float sits at +4 due to alignment.
        if t0 == .Flt16 && source_size > 4 {
            t1, ok1 = abi_fp_leaf_at(leaves, off + 4)
        }
        if !ok1 {
            return t0 == .Flt16 ? "half" : "float"
        }
    }

    if t0 == .Flt32 && t1 == .Flt32 do return "<2 x float>"

    if t0 == .Flt16 && t1 == .Flt16 {
        ok2 := false
        if source_size > 4 {
            _, ok2 = abi_fp_leaf_at(leaves, off + 4)
        }
        return ok2 ? "<4 x half>" : "<2 x half>"
    }

    if t0 == .Flt16 || t1 == .Flt16 do return "<4 x half>"

    return "double"
}

// Mirrors clang's X86_64ABIInfo::GetINTEGERTypeAtOffset.
abi_integer_type_at_offset :: proc(leaves: []AbiLeaf, off: int, total_size: int) -> string {
    best: AbiLeaf
    found := false
    for l in leaves {
        if l.offset != off || l.size == 0 do continue
        if !found || l.size > best.size {
            best = l
            found = true
        }
    }

    if found {
        #partial switch best.kind {
        case .Pointer, .Function:
            return "ptr"
        case .Int64, .UInt64:
            return "i64"
        }
        // A 1/2/4-byte integer can be used as-is only if the rest of this
        // eightbyte is padding (e.g. the i32 in {double, i32}).
        if !abi_is_fp_kind(best.kind) && (best.size == 1 || best.size == 2 || best.size == 4) {
            if abi_no_user_data(leaves, off + best.size, off + 8) {
                return int_class_llvm_type(best.size)
            }
        }
    }

    // Integer no larger than what remains of the object.
    return int_class_llvm_type(min(total_size - off, 8))
}

// SysV-classifies an aggregate (struct / fixed array / union / slice /
// string / any). Scalars never go through here.
classify_type :: proc(type_id: TypeId) -> AbiClassified {
    r: AbiClassified
    r.size = type_size(type_id)
    r.align = type_align_of(type_id)

    if r.size == 0 {
        r.is_empty = true
        return r
    }

    // MEMORY class: too big, or over-aligned past 16 bytes.
    if r.size > 16 || r.align > 16 {
        r.is_memory = true
        return r
    }

    leaves := make([dynamic]AbiLeaf, 0, 16, context.temp_allocator)
    abi_collect_leaves(type_id, 0, &leaves)

    r.num_eightbytes = r.size <= 8 ? 1 : 2

    classes: [2]AbiClass
    for l in leaves {
        if l.size == 0 do continue
        // Unaligned or eightbyte-straddling field -> MEMORY. Unreachable with
        // the natural layout above; kept for future #packed support.
        if l.offset % l.size != 0 || l.offset / 8 != (l.offset + l.size - 1) / 8 {
            r.is_memory = true
            r.num_eightbytes = 0
            return r
        }
        idx := l.offset / 8
        classes[idx] = merge_abi_class(classes[idx], scalar_abi_class(l.kind))
    }

    // Post-merger cleanup (psABI 3.2.3p2 step 5). There is no X87/SSEUP in
    // Gala, so only the MEMORY rule applies. A NoClass eightbyte can only be
    // pure padding, which natural layout with max align 8 never produces
    // inside a <=16-byte object; treat it as INTEGER to be safe.
    for i in 0 ..< r.num_eightbytes {
        class := classes[i]
        if class == .Memory {
            r.is_memory = true
            r.num_eightbytes = 0
            return r
        }
        if class == .NoClass do class = .Integer

        off := i * 8
        llvm: string
        if class == .Sse {
            llvm = abi_sse_type_at_offset(leaves[:], off, r.size - off)
            r.needed_sse += 1
        } else {
            llvm = abi_integer_type_at_offset(leaves[:], off, r.size)
            r.needed_int += 1
        }
        r.eightbytes[i] = AbiEightbyte{class = class, llvm_type = llvm}
    }

    if r.num_eightbytes == 1 {
        r.llvm_type = r.eightbytes[0].llvm_type
        r.coerced_size, _ = abi_llvm_type_size_align(r.llvm_type)
        return r
    }

    // clang's GetX86_64ByValArgumentPair: the high part must start at byte 8
    // of the LLVM struct. If the low part is narrow (e.g. {float, i32}, where
    // i32 would land at offset 4), widen it to a full eightbyte.
    lo := r.eightbytes[0].llvm_type
    hi := r.eightbytes[1].llvm_type
    lo_size, _ := abi_llvm_type_size_align(lo)
    _, hi_align := abi_llvm_type_size_align(hi)
    if abi_align_up(lo_size, hi_align) != 8 {
        switch lo {
        case "half", "float", "<2 x half>":
            lo = "double"
        case:
            lo = "i64"
        }
        r.eightbytes[0].llvm_type = lo
    }
    r.llvm_type = fmt.tprintf("{{ %s, %s }}", lo, hi)
    r.coerced_size = 16
    return r
}

// ----------------------------------------------------------------------------
// Lowering records
// ----------------------------------------------------------------------------

AbiArgMode :: enum {
    Direct,   // passed as a (possibly coerced) SSA value
    ByVal,    // SysV: `ptr byval(%T) align N`
    Indirect, // Win64: plain `ptr` to a caller-made temporary copy
    Ignore,   // SysV empty aggregate: not passed at all
}

// SysV argument-register budget. Ignored for Win64 (positional slots, and
// an aggregate never straddles register/stack there).
AbiRegState :: struct {
    free_int: int,
    free_sse: int,
}

abi_initial_regs :: proc(kind: AbiKind) -> AbiRegState {
    if abi_resolve_kind(kind) == .SysV {
        return AbiRegState{free_int = 6, free_sse = 8}
    }
    return AbiRegState{}
}

AbiArgLowering :: struct {
    orig_type:      TypeId,
    mode:           AbiArgMode,
    coerced_type:   string, // Direct only
    needs_coercion: bool,   // Direct only
    coerced_size:   int,    // Direct + needs_coercion: LLVM alloc size of coerced_type
    byval_align:    int,    // ByVal only
    indirect_align: int,    // Indirect only: alignment of the temp copy
    ext:            string, // Direct scalars only: "", "zeroext" or "signext".
                            // Emit as `<coerced_type> <ext>` in defs/decls/calls.
    promotion:      AbiVariadicPromotion, // C-variadic slots only
}

AbiRetMode :: enum {
    Direct,
    Indirect, // sret
    Ignore,   // SysV empty aggregate: returns void
}

AbiRetLowering :: struct {
    orig_type:      TypeId,
    mode:           AbiRetMode,
    coerced_type:   string, // Direct: llvm return type. Indirect/Ignore: "void"
    needs_coercion: bool,   // Direct only
    coerced_size:   int,    // Direct + needs_coercion
    sret_align:     int,    // Indirect only
    ext:            string, // Direct scalars only: "", "zeroext" or "signext".
                            // Emit as `<ext> <coerced_type>` in defs/decls/calls.
}

AbiSignature :: struct {
    kind:             AbiKind, // resolved, never .None
    llvm_cc:          string,  // "" or e.g. "win64cc " — see abi_llvm_callconv
    ret:              AbiRetLowering,
    args:             []AbiArgLowering,
    regs_after_fixed: AbiRegState, // seed for cg_abi_lower_c_variadic_arg
}

// ----------------------------------------------------------------------------
// Return values
// ----------------------------------------------------------------------------

abi_lower_return :: proc(kind: AbiKind, type_id: TypeId) -> AbiRetLowering {
    k := abi_resolve_kind(kind)
    ty := get_type(type_id)

    if ty.kind == .Void {
        return AbiRetLowering{orig_type = type_id, mode = .Direct, coerced_type = "void"}
    }

    if abi_is_scalar_kind(ty.kind) {
        return AbiRetLowering{
            orig_type    = type_id,
            mode         = .Direct,
            coerced_type = scalar_llvm_str(type_id),
            ext          = abi_scalar_ext_for(k, type_id),
        }
    }

    abi_require_aggregate(type_id)

    switch k {
    case .Win64:
        // RAX for 1/2/4/8-byte aggregates, otherwise caller-provided buffer
        // (pointer passed in RCX and echoed back in RAX — LLVM handles that).
        size := type_size(type_id)
        if size == 1 || size == 2 || size == 4 || size == 8 {
            return AbiRetLowering{
                orig_type      = type_id,
                mode           = .Direct,
                coerced_type   = int_class_llvm_type(size),
                needs_coercion = true,
                coerced_size   = size,
            }
        }
        return AbiRetLowering{
            orig_type    = type_id,
            mode         = .Indirect,
            coerced_type = "void",
            sret_align   = type_align_of(type_id),
        }

    case .SysV, .None:
        c := classify_type(type_id)
        if c.is_empty {
            return AbiRetLowering{orig_type = type_id, mode = .Ignore, coerced_type = "void"}
        }
        if c.is_memory {
            // sret uses natural alignment (not the >=8 byval minimum).
            return AbiRetLowering{
                orig_type    = type_id,
                mode         = .Indirect,
                coerced_type = "void",
                sret_align   = c.align,
            }
        }
        // Slices/strings/any already are `{ ptr, i64 }` as SSA values.
        needs := !abi_is_slice_like_kind(ty.kind)
        return AbiRetLowering{
            orig_type      = type_id,
            mode           = .Direct,
            coerced_type   = c.llvm_type,
            needs_coercion = needs,
            coerced_size   = needs ? c.coerced_size : 0,
        }
    }
    return AbiRetLowering{}
}

// ----------------------------------------------------------------------------
// Arguments
// ----------------------------------------------------------------------------

// Lowers one argument and (SysV) consumes registers from `regs`.
abi_lower_arg :: proc(kind: AbiKind, regs: ^AbiRegState, type_id: TypeId) -> AbiArgLowering {
    k := abi_resolve_kind(kind)
    ty := get_type(type_id)

    if abi_is_scalar_kind(ty.kind) {
        // Scalars are always Direct; once registers run out they simply go
        // on the stack, so we only need to keep the budget accurate.
        if k == .SysV {
            if scalar_abi_class(ty.kind) == .Sse {
                if regs.free_sse > 0 do regs.free_sse -= 1
            } else {
                if regs.free_int > 0 do regs.free_int -= 1
            }
        }
        return AbiArgLowering{
            orig_type    = type_id,
            mode         = .Direct,
            coerced_type = scalar_llvm_str(type_id),
            ext          = abi_scalar_ext_for(k, type_id),
        }
    }

    abi_require_aggregate(type_id)

    switch k {
    case .Win64:
        // "Any argument that doesn't fit in 8 bytes, or is not 1, 2, 4, or 8
        // bytes, must be passed by reference." Slices/strings/any (16 bytes)
        // therefore go by reference too.
        size := type_size(type_id)
        if size == 1 || size == 2 || size == 4 || size == 8 {
            return AbiArgLowering{
                orig_type      = type_id,
                mode           = .Direct,
                coerced_type   = int_class_llvm_type(size),
                needs_coercion = true,
                coerced_size   = size,
            }
        }
        return AbiArgLowering{
            orig_type      = type_id,
            mode           = .Indirect,
            indirect_align = max(type_align_of(type_id), 8),
        }

    case .SysV, .None:
        c := classify_type(type_id)
        if c.is_empty {
            return AbiArgLowering{orig_type = type_id, mode = .Ignore}
        }
        // psABI 3.2.3p3: if any eightbyte can't get a register, the whole
        // argument goes on the stack (no splitting between regs and stack).
        if !c.is_memory && regs.free_int >= c.needed_int && regs.free_sse >= c.needed_sse {
            regs.free_int -= c.needed_int
            regs.free_sse -= c.needed_sse
            needs := !abi_is_slice_like_kind(ty.kind)
            return AbiArgLowering{
                orig_type      = type_id,
                mode           = .Direct,
                coerced_type   = c.llvm_type,
                needs_coercion = needs,
                coerced_size   = needs ? c.coerced_size : 0,
            }
        }
        // byval args are always at least 8-byte aligned on x86-64 SysV.
        return AbiArgLowering{
            orig_type   = type_id,
            mode        = .ByVal,
            byval_align = max(c.align, 8),
        }
    }
    return AbiArgLowering{}
}

// The single entry point every function definition, declaration, and call
// goes through.
// The single entry point every function definition, declaration, and call
// goes through.
//
// Gala (non-extern) variadics travel as one trailing slice argument of
// `fn.gala_abi_ty`, so such a signature has len(fn.args) + 1 entries in
// `args`; the extra one is lowered exactly like any other aggregate.
// `is_extern` functions use a real C `...` instead and get no extra entry.
cg_abi_lower_signature :: proc(c: ^CGCtx, fn_type_id: TypeId, kind: AbiKind, is_extern := false) -> AbiSignature {
    _ = c

    k := abi_resolve_kind(kind)
    fn_ty := get_type(fn_type_id)

    sig: AbiSignature

    sig.kind = k
    sig.llvm_cc = abi_llvm_callconv(k)
    sig.ret = abi_lower_return(k, fn_ty.fn.ret_ty)

    regs := abi_initial_regs(k)
    // SysV: the hidden sret pointer occupies %rdi.

    if k == .SysV && sig.ret.mode == .Indirect {
        regs.free_int -= 1
    }

    has_gala_variadic := fn_ty.fn.is_variadic && !is_extern
    n := len(fn_ty.fn.args) + (1 if has_gala_variadic else 0)

    args := make([dynamic]AbiArgLowering, 0, n, allocator = get_ctx().allocator)
    for a in fn_ty.fn.args {
        append(&args, abi_lower_arg(k, &regs, a.type))
    }
    // seed for C-variadic extras: taken BEFORE the gala variadic slice
    sig.regs_after_fixed = regs

    if has_gala_variadic {
        append(&args, abi_lower_arg(k, &regs, fn_ty.fn.gala_abi_ty))

    }
    sig.args = args[:]
    return sig
}

// ----------------------------------------------------------------------------
// C variadics
// ----------------------------------------------------------------------------

// C "default argument promotions" (C11 §6.5.2.2p7), applied to every
// argument landing in a true C-variadic `...` slot — never to a fixed
// parameter, and never to a Gala `..T` slice-packed tail (which has a real
// declared element type the callee expects exactly).
//
// Decided before ABI classification: `float` becomes `double` and any
// integer narrower than `int` becomes `int`; the ABI then classifies the
// promoted type like any other. Same rules on SysV and Win64 (on Win64,
// LLVM additionally mirrors variadic FP args into the matching GPR).
AbiVariadicPromotion :: enum {
    None,
    FloatToDouble,   // fpext
    ZeroExtendToInt, // zext  (unsigned sub-int types, and bool)
    SignExtendToInt, // sext  (signed sub-int types)
}

// Returns the promotion (if any) required for scalar `type_id` in a
// C-variadic slot, plus the LLVM type string after promotion.
c_variadic_promote :: proc(type_id: TypeId) -> (AbiVariadicPromotion, string) {
    ty := get_type(type_id)
    #partial switch ty.kind {
    case .Flt32, .Flt16:
        return .FloatToDouble, "double"
    case .Bool, .Byte, .UInt_8, .UInt16, .Flt_8:
        // Flt_8 is lowered as an i8, so it widens like an unsigned 8-bit int
        // (an fpext from i8 would be invalid IR).
        return .ZeroExtendToInt, "i32"
    case .Int_8, .Int16:
        return .SignExtendToInt, "i32"
    case:
        // Int32/UInt32 and wider, Flt64, Rune, Pointer, Function: already
        // >= variadic minimum width.
        return .None, scalar_llvm_str(type_id)
    }
}

// Lowers one argument in a C `...` slot. Seed `regs` with
// `sig.regs_after_fixed` and reuse it for every extra argument of the call.
// Scalars come back with `promotion` set and `coerced_type` already the
// promoted type (no ext attribute — the promotion did the widening).
cg_abi_lower_c_variadic_arg :: proc(kind: AbiKind, regs: ^AbiRegState, type_id: TypeId) -> AbiArgLowering {
    if abi_is_scalar_kind(get_type(type_id).kind) {
        lowering := abi_lower_arg(kind, regs, type_id) // keeps register count right
        promo, llvm := c_variadic_promote(type_id)
        lowering.promotion = promo
        lowering.coerced_type = llvm
        lowering.ext = ""
        return lowering
    }
    return abi_lower_arg(kind, regs, type_id)
}

