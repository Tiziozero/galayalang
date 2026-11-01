package main

ty_to_llvm_cast_op :: proc(target_id, to_id: TypeId) -> (string, bool) {
    target := get_type(target_id)
    to := get_type(to_id)

    if target.kind == to.kind {
        #partial switch target.kind {
        case .Pointer: return "bitcast", true // safe even if often a no-op with opaque ptrs
        case:          return "", false        // identity — caller should skip emitting anything
        }
    }

    tk, ok := target.kind, to.kind

    // int-family -> int-family (Int/UInt/Byte/Rune/Bool, any combo)
    if is_int_kind(tk) && is_int_kind(ok) {
        from_w := bit_width_of(tk)
        to_w   := bit_width_of(ok)
        if from_w < to_w {
            return is_signed(tk) ? "sext" : "zext", true
        } else if from_w > to_w {
            return "trunc", true
        }
        return "", false // same width, different label (Int32<->UInt32, Int_8<->Byte...) — no-op
    }
    // int-family -> float
    if is_int_kind(tk) && is_float_kind(ok) {
        return is_signed(tk) ? "sitofp" : "uitofp", true
    }
    // float -> int-family
    if is_float_kind(tk) && is_int_kind(ok) {
        return is_signed(ok) ? "fptosi" : "fptoui", true
    }
    // float -> float, different width (Flt64/32/16/8 now distinct — no longer a no-op)
    if is_float_kind(tk) && is_float_kind(ok) {
        return bit_width_of(tk) < bit_width_of(ok) ? "fpext" : "fptrunc", true
    }
    // pointer <-> integer
    if tk == .Pointer && is_int_kind(ok) {
        return "ptrtoint", true
    }
    if is_int_kind(tk) && ok == .Pointer {
        return "inttoptr", true
    }

    gala_panic("ty_to_llvm_cast_op: no cast op for this pair — check can_cast_to table")
}

// NoOp / Instr / Memory instead of (string, bool): the old signature couldn't
// distinguish "identical repr, reuse the value" (NoOp) from "no direct op,
// roundtrip through memory" (Memory) — both returned ("", false).
TransmuteResult :: enum { NoOp, Instr, Memory }

ty_to_llvm_transmute_op :: proc(from_id, to_id: TypeId) -> (op: string, result: TransmuteResult) {
    from := get_type(from_id)
    to := get_type(to_id)

    if from.kind == to.kind do return "", .NoOp

    // int-family <-> int-family, same width, different label: pure relabel
    if is_int_kind(from.kind) && is_int_kind(to.kind) && bit_width_of(from.kind) == bit_width_of(to.kind) {
        return "", .NoOp
    }
    // int-family <-> float, same width: true bit reinterpretation
    if is_int_kind(from.kind) && is_float_kind(to.kind) && bit_width_of(from.kind) == bit_width_of(to.kind) {
        return "bitcast", .Instr
    }
    if is_float_kind(from.kind) && is_int_kind(to.kind) && bit_width_of(from.kind) == bit_width_of(to.kind) {
        return "bitcast", .Instr
    }
    // Pointer <-> Integer: NOT bitcast-legal in LLVM; ptrtoint/inttoptr
    // are already the bit-preserving ops for equal widths.
    if from.kind == .Pointer && is_int_kind(to.kind) {
        return "ptrtoint", .Instr
    }
    if is_int_kind(from.kind) && to.kind == .Pointer {
        return "inttoptr", .Instr
    }

    return "", .Memory // no single-instruction path — roundtrip through memory
}
