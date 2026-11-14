package main

import "core:fmt"
import "core:os"
import "core:strings"

// C backend over the IR. Needs GNU C (statement expressions, __builtin_memcpy),
// so gcc/clang only.
//
// Conventions:
//  - ints are emitted as UNSIGNED fixed-width types; signed ops cast to the
//    signed type of the same width (the IR keeps signedness in the ops)
//  - ptr -> void*, bool -> bool, fixed arrays -> struct { T v[N]; }
//  - locals/params are l0, l1, ...; struct fields are f0, f1, ...
//  - IRSeq -> ({ stmts; value; })
//  - gala `main` is emitted as gala_main plus a real C `int main`

CCGen :: struct {
    p:    ^IRProgram,
    sb:   strings.Builder,
    fn:   ^IRFunction,
    ind:  int,
    tmp:  int,
    done: []bool, // type definitions already written
}

cc_ws :: proc(g: ^CCGen, s: string) { strings.write_string(&g.sb, s) }
cc_wf :: proc(g: ^CCGen, format: string, args: ..any) { fmt.sbprintf(&g.sb, format, ..args) }
cc_pad :: proc(g: ^CCGen) {
    for _ in 0..<g.ind do strings.write_string(&g.sb, "    ")
}

// ===========================================================================
// Types
// ===========================================================================

cc_type :: proc(g: ^CCGen, id: IRTypeId) -> string {
    switch t in g.p.types[id] {
    case IRVoid:  return "void"
    case IRBool:  return "bool"
    case IRInt:
        switch t.bits {
        case 8:  return "uint8_t"
        case 16: return "uint16_t"
        case 32: return "uint32_t"
        }
        return "uint64_t"
    case IRFloat: return "float" if t.bits == 32 else "double"
    case IRPtr, IRFnType: return "void*"
    case IRStruct, IRBlob, IRArray: return fmt.aprintf("T%d", int(id))
    }
    return "void"
}

// signed twin of an int type, for signed div/rem/compare/shift/extend
cc_stype :: proc(g: ^CCGen, id: IRTypeId) -> string {
    if t, ok := g.p.types[id].(IRInt); ok {
        switch t.bits {
        case 8:  return "int8_t"
        case 16: return "int16_t"
        case 32: return "int32_t"
        }
    }
    return "int64_t"
}

cc_is_void :: proc(g: ^CCGen, id: IRTypeId) -> bool {
    _, ok := g.p.types[id].(IRVoid)
    return ok
}

cc_is_aggregate :: proc(g: ^CCGen, id: IRTypeId) -> bool {
    #partial switch _ in g.p.types[id] {
    case IRStruct, IRBlob, IRArray: return true
    }
    return false
}

// definitions go out dependencies first (by-value members). pointers are
// void*, so recursive structs never cycle
cc_type_def :: proc(g: ^CCGen, id: IRTypeId) {
    if g.done[id] do return
    g.done[id] = true

    switch t in g.p.types[id] {
    case IRStruct:
        for f in t.fields do cc_type_def(g, f)
        cc_wf(g, "// %s\n", t.name)
        cc_wf(g, "struct T%d ", int(id))
        cc_ws(g, "{\n")
        if len(t.fields) == 0 do cc_ws(g, "    char _e;\n")
        for f, i in t.fields {
            cc_wf(g, "    %s f%d;\n", cc_type(g, f), i)
        }
        cc_ws(g, "};\n")
    case IRArray:
        cc_type_def(g, t.elem)
        cc_wf(g, "struct T%d ", int(id))
        cc_ws(g, "{ ")
        cc_wf(g, "%s v[%d]; ", cc_type(g, t.elem), max(t.count, 1))
        cc_ws(g, "};\n")
    case IRBlob:
        cc_wf(g, "struct T%d ", int(id))
        cc_ws(g, "{ ")
        cc_wf(g, "_Alignas(%d) uint8_t v[%d]; ", max(t.align, 1), max(t.size, 1))
        cc_ws(g, "};\n")
    case IRVoid, IRBool, IRInt, IRFloat, IRPtr, IRFnType:
    }
}

// ===========================================================================
// Expressions
// ===========================================================================

cc_fn_sym :: proc(g: ^CCGen, id: IRFnId) -> string {
    s := g.p.fns[id].symbol
    return "gala_main" if s == "main" else s
}

cc_cstring :: proc(g: ^CCGen, s: string) {
    for i in 0..<len(s) {
        b := s[i]
        switch {
        case b == '"':             cc_ws(g, "\\\"")
        case b == '\\':            cc_ws(g, "\\\\")
        case b == '?':             cc_ws(g, "\\?") // dodge trigraphs
        case b >= 32 && b < 127:   strings.write_byte(&g.sb, b)
        case:                      cc_wf(g, "\\%03o", int(b))
        }
    }
}

// ((res)((lc)(l) op (rc)(r)))   lc/rc are optional operand casts
cc_bin :: proc(g: ^CCGen, res, lc, op, rc: string, l, r: ^IRExpr) {
    cc_wf(g, "((%s)(", res)
    if lc != "" do cc_wf(g, "(%s)", lc)
    cc_ws(g, "(")
    cc_expr(g, l^)
    cc_wf(g, ") %s ", op)
    if rc != "" do cc_wf(g, "(%s)", rc)
    cc_ws(g, "(")
    cc_expr(g, r^)
    cc_ws(g, ")))")
}

cc_expr :: proc(g: ^CCGen, e: IRExpr) {
    ty := cc_type(g, e.type)

    switch k in e.kind {
    case IRIntLit:   cc_wf(g, "((%s)%dULL)", ty, k.value)
    case IRFloatLit: cc_wf(g, "((%s)%v)", ty, k.value)
    case IRBoolLit:  cc_wf(g, "((bool)%d)", 1 if k.value else 0)
    case IRStrLit:
        // string/slice struct { ptr, len }
        cc_ws(g, "((")
        cc_ws(g, ty)
        cc_ws(g, ")")
        cc_ws(g, "{ (void*)\"")
        cc_cstring(g, k.bytes)
        cc_wf(g, "\", %d ", len(k.bytes))
        cc_ws(g, "})")
    case IRNullPtr:  cc_ws(g, "((void*)0)")
    case IRZero:
        if cc_is_aggregate(g, e.type) do cc_wf(g, "((%s)", ty)
        else                          do cc_wf(g, "((%s)", ty)
        if cc_is_aggregate(g, e.type) {
            cc_ws(g, "{ 0 })")
        } else {
            cc_ws(g, "0)")
        }
    case IRLocalRef:  cc_wf(g, "l%d", int(k.id))
    case IRGlobalRef: cc_ws(g, g.p.globals[k.id].symbol)
    case IRFnRef:     cc_wf(g, "((void*)%s)", cc_fn_sym(g, k.id))

    case IRDeref:
        cc_ws(g, "(*(")
        cc_ws(g, ty)
        cc_ws(g, "*)(")
        cc_expr(g, k.ptr^)
        cc_ws(g, "))")
    case IRField:
        cc_ws(g, "(")
        cc_expr(g, k.base^)
        cc_wf(g, ").f%d", int(k.index))
    case IRIndex:
        cc_ws(g, "(")
        cc_expr(g, k.base^)
        cc_ws(g, ").v[")
        cc_expr(g, k.index^)
        cc_ws(g, "]")
    case IRAddrOf:
        cc_ws(g, "((void*)&(")
        cc_expr(g, k.place^)
        cc_ws(g, "))")

    case IRUnary:
        switch k.op {
        case .Neg:    cc_wf(g, "((%s)(0u - (", ty)
        case .FNeg:   cc_wf(g, "((%s)(-(", ty)
        case .Not:    cc_ws(g, "((bool)(!(")
        case .BitNot: cc_wf(g, "((%s)(~(", ty)
        }
        cc_expr(g, k.operand^)
        cc_ws(g, ")))")

    case IRBinary:
        ot := cc_type(g, k.lhs.type)
        st := cc_stype(g, k.lhs.type)
        l, r := k.lhs, k.rhs
        switch k.op {
        case .Add:  cc_bin(g, ty, "", "+", "", l, r)
        case .Sub:  cc_bin(g, ty, "", "-", "", l, r)
        case .Mul:  cc_bin(g, ty, "", "*", "", l, r)
        case .FAdd: cc_bin(g, ty, "", "+", "", l, r)
        case .FSub: cc_bin(g, ty, "", "-", "", l, r)
        case .FMul: cc_bin(g, ty, "", "*", "", l, r)
        case .FDiv: cc_bin(g, ty, "", "/", "", l, r)
        case .SDiv: cc_bin(g, ty, st, "/", st, l, r)
        case .UDiv: cc_bin(g, ty, ot, "/", ot, l, r)
        case .SRem: cc_bin(g, ty, st, "%", st, l, r)
        case .URem: cc_bin(g, ty, ot, "%", ot, l, r)
        case .And:  cc_bin(g, ty, "", "&", "", l, r)
        case .Or:   cc_bin(g, ty, "", "|", "", l, r)
        case .Xor:  cc_bin(g, ty, "", "^", "", l, r)
        case .Shl:  cc_bin(g, ty, ot, "<<", "", l, r)
        case .AShr: cc_bin(g, ty, st, ">>", "", l, r)
        case .LShr: cc_bin(g, ty, ot, ">>", "", l, r)
        case .Eq:   cc_bin(g, "bool", "", "==", "", l, r)
        case .Ne:   cc_bin(g, "bool", "", "!=", "", l, r)
        case .SLt:  cc_bin(g, "bool", st, "<",  st, l, r)
        case .SLe:  cc_bin(g, "bool", st, "<=", st, l, r)
        case .SGt:  cc_bin(g, "bool", st, ">",  st, l, r)
        case .SGe:  cc_bin(g, "bool", st, ">=", st, l, r)
        case .ULt:  cc_bin(g, "bool", ot, "<",  ot, l, r)
        case .ULe:  cc_bin(g, "bool", ot, "<=", ot, l, r)
        case .UGt:  cc_bin(g, "bool", ot, ">",  ot, l, r)
        case .UGe:  cc_bin(g, "bool", ot, ">=", ot, l, r)
        case .FEq:  cc_bin(g, "bool", "", "==", "", l, r)
        case .FNe:  cc_bin(g, "bool", "", "!=", "", l, r)
        case .FLt:  cc_bin(g, "bool", "", "<",  "", l, r)
        case .FLe:  cc_bin(g, "bool", "", "<=", "", l, r)
        case .FGt:  cc_bin(g, "bool", "", ">",  "", l, r)
        case .FGe:  cc_bin(g, "bool", "", ">=", "", l, r)
        }

    case IRLogical:
        cc_ws(g, "((bool)((")
        cc_expr(g, k.lhs^)
        cc_ws(g, ")")
        cc_ws(g, " && " if k.is_and else " || ")
        cc_ws(g, "(")
        cc_expr(g, k.rhs^)
        cc_ws(g, ")))")

    case IRPtrOffset:
        // ptr +/- idx * sizeof(elem); the index is sign-extended
        cc_ws(g, "((void*)((uint8_t*)(")
        cc_expr(g, k.ptr^)
        cc_ws(g, ") - " if k.subtract else ") + ")
        cc_wf(g, "(int64_t)(%s)(", cc_stype(g, k.index.type))
        cc_expr(g, k.index^)
        cc_wf(g, ") * (int64_t)sizeof(%s)))", cc_type(g, k.elem))

    case IRConvert:
        src := k.operand.type
        switch k.kind {
        case .Trunc, .ZExt, .FPTrunc, .FPExt, .FPToUI, .UIToFP:
            cc_wf(g, "((%s)(", ty)
            cc_expr(g, k.operand^)
            cc_ws(g, "))")
        case .SExt, .SIToFP:
            cc_wf(g, "((%s)(%s)(", ty, cc_stype(g, src))
            cc_expr(g, k.operand^)
            cc_ws(g, "))")
        case .FPToSI:
            cc_wf(g, "((%s)(%s)(", ty, cc_stype(g, e.type))
            cc_expr(g, k.operand^)
            cc_ws(g, "))")
        case .PtrToInt:
            cc_wf(g, "((%s)(uintptr_t)(", ty)
            cc_expr(g, k.operand^)
            cc_ws(g, "))")
        case .IntToPtr:
            cc_ws(g, "((void*)(uintptr_t)(")
            cc_expr(g, k.operand^)
            cc_ws(g, "))")
        case .IntToBool:
            cc_ws(g, "((bool)((")
            cc_expr(g, k.operand^)
            cc_ws(g, ") != 0))")
        case .Bitcast:
            n := g.tmp
            g.tmp += 1
            cc_ws(g, "({ ")
            cc_wf(g, "%s bc%d_s = ", cc_type(g, src), n)
            cc_expr(g, k.operand^)
            cc_wf(g, "; %s bc%d_d; ", ty, n)
            cc_wf(g, "__builtin_memcpy(&bc%d_d, &bc%d_s, sizeof(bc%d_d)); bc%d_d; ", n, n, n, n)
            cc_ws(g, "})")
        }

    case IRAggregate:
        _, is_arr := g.p.types[e.type].(IRArray)
        cc_ws(g, "((")
        cc_ws(g, ty)
        cc_ws(g, ")")
        cc_ws(g, "{ ")
        if is_arr do cc_ws(g, "{ ")
        if len(k.elems) == 0 do cc_ws(g, "0")
        for el, i in k.elems {
            if i > 0 do cc_ws(g, ", ")
            cc_expr(g, el)
        }
        if is_arr do cc_ws(g, " }")
        cc_ws(g, " })")

    case IRCall:
        if id, ok := k.direct.?; ok {
            cc_ws(g, cc_fn_sym(g, id))
        } else {
            // cast the callee to a function pointer type built from the call site
            cc_wf(g, "((%s(*)(", ty)
            if len(k.args) == 0 do cc_ws(g, "void")
            for a, i in k.args {
                if i > 0 do cc_ws(g, ", ")
                cc_ws(g, cc_type(g, a.type))
            }
            cc_ws(g, "))(")
            cc_expr(g, k.indirect^)
            cc_ws(g, "))")
        }
        cc_ws(g, "(")
        for a, i in k.args {
            if i > 0 do cc_ws(g, ", ")
            cc_expr(g, a)
        }
        cc_ws(g, ")")

    case IRSeq:
        cc_ws(g, "({ ")
        saved := g.ind
        g.ind = 0
        for s in k.stmts do cc_stmt(g, s)
        g.ind = saved
        cc_expr(g, k.value^)
        cc_ws(g, "; })")
    }
}

// ===========================================================================
// Statements
// ===========================================================================

cc_stmts :: proc(g: ^CCGen, stmts: []IRStmt) {
    g.ind += 1
    for s in stmts do cc_stmt(g, s)
    g.ind -= 1
}

cc_stmt :: proc(g: ^CCGen, s: IRStmt) {
    switch v in s {
    case IRLet:
        // locals are declared at the top of the function
        if init, ok := v.init.?; ok {
            cc_pad(g)
            cc_wf(g, "l%d = ", int(v.local))
            cc_expr(g, init)
            cc_ws(g, ";\n")
        }
    case IRAssign:
        cc_pad(g)
        cc_expr(g, v.target^)
        cc_ws(g, " = ")
        cc_expr(g, v.value)
        cc_ws(g, ";\n")
    case IRExprStmt:
        cc_pad(g)
        cc_ws(g, "(void)(")
        cc_expr(g, v.expr)
        cc_ws(g, ");\n")
    case IRReturn:
        cc_pad(g)
        cc_ws(g, "return")
        if e, ok := v.value.?; ok {
            cc_ws(g, " ")
            cc_expr(g, e)
        }
        cc_ws(g, ";\n")
    case IRIf:
        cc_pad(g)
        cc_ws(g, "if (")
        cc_expr(g, v.cond)
        cc_ws(g, ") {\n")
        cc_stmts(g, v.then_body)
        if len(v.else_body) > 0 {
            cc_pad(g)
            cc_ws(g, "} else {\n")
            cc_stmts(g, v.else_body)
        }
        cc_pad(g)
        cc_ws(g, "}\n")
    case IRLoop:
        // for (;;) { if (!cond) break; body; Lc: ; step }  Lb: ;
        cc_pad(g)
        cc_ws(g, "for (;;) {\n")
        g.ind += 1
        if c, ok := v.cond.?; ok {
            cc_pad(g)
            cc_ws(g, "if (!(")
            cc_expr(g, c)
            cc_ws(g, ")) break;\n")
        }
        g.ind -= 1
        cc_stmts(g, v.body)
        g.ind += 1
        cc_pad(g)
        cc_wf(g, "L%d_cont: ;\n", int(v.id))
        g.ind -= 1
        cc_stmts(g, v.step)
        cc_pad(g)
        cc_ws(g, "}\n")
        cc_pad(g)
        cc_wf(g, "L%d_brk: ;\n", int(v.id))
    case IRBreak:
        cc_pad(g)
        cc_wf(g, "goto L%d_brk;\n", int(v.loop))
    case IRContinue:
        cc_pad(g)
        cc_wf(g, "goto L%d_cont;\n", int(v.loop))
    }
}

// ===========================================================================
// Functions and globals
// ===========================================================================

cc_fn_sig :: proc(g: ^CCGen, id: IRFnId, names: bool) {
    fn := &g.p.fns[id]
    ft := g.p.types[fn.type].(IRFnType)

    _, has_body := fn.body.?
    if !has_body {
        cc_ws(g, "extern ")
    } else {
        switch fn.linkage {
        case .Internal:    cc_ws(g, "static ")
        case .LinkOnceOdr: cc_ws(g, "__attribute__((weak)) ")
        case .External:
        }
    }
    cc_wf(g, "%s %s(", cc_type(g, ft.ret), cc_fn_sym(g, id))
    if len(ft.params) == 0 do cc_ws(g, "void")
    for p, i in ft.params {
        if i > 0 do cc_ws(g, ", ")
        cc_ws(g, cc_type(g, p))
        if names do cc_wf(g, " l%d", i)
    }
    cc_ws(g, ")")
}

cc_fn_def :: proc(g: ^CCGen, id: IRFnId) {
    fn := &g.p.fns[id]
    body, ok := fn.body.?
    if !ok do return
    g.fn = fn
    ft := g.p.types[fn.type].(IRFnType)

    cc_fn_sig(g, id, true)
    cc_ws(g, " {\n")
    g.ind = 1
    for j in len(ft.params)..<len(fn.locals) {
        lt := fn.locals[j].type
        if cc_is_void(g, lt) do continue
        cc_pad(g)
        cc_wf(g, "%s l%d; // %s\n", cc_type(g, lt), j, fn.locals[j].name)
    }
    for s in body do cc_stmt(g, s)
    g.ind = 0
    cc_ws(g, "}\n\n")
    g.fn = nil
}

// ===========================================================================
// Entry point
// ===========================================================================

emit_c :: proc(prog: IRProgram, path := "out.c") {
    p := prog
    g := CCGen{p = &p, sb = strings.builder_make()}
    g.done = make([]bool, len(p.types))

    cc_ws(&g, "// generated by galac\n")
    cc_ws(&g, "#include <stdint.h>\n#include <stdbool.h>\n\n")

    // types
    for t, i in p.types {
        #partial switch _ in t {
        case IRStruct, IRBlob, IRArray:
            cc_wf(&g, "typedef struct T%d T%d;\n", i, i)
        }
    }
    cc_ws(&g, "\n")
    for _, i in p.types do cc_type_def(&g, IRTypeId(i))
    cc_ws(&g, "\n")

    // prototypes
    for _, i in p.fns {
        cc_fn_sig(&g, IRFnId(i), false)
        cc_ws(&g, ";\n")
    }
    cc_ws(&g, "\n")

    // globals
    for &gl in p.globals {
        switch gl.linkage {
        case .Internal:    cc_ws(&g, "static ")
        case .LinkOnceOdr: cc_ws(&g, "__attribute__((weak)) ")
        case .External:
        }
        cc_wf(&g, "%s %s", cc_type(&g, gl.type), gl.symbol)
        if init, ok := gl.init.?; ok {
            cc_ws(&g, " = ")
            cc_expr(&g, init)
        }
        cc_ws(&g, ";\n")
    }
    cc_ws(&g, "\n")

    // bodies
    for _, i in p.fns do cc_fn_def(&g, IRFnId(i))

    // real C entry point around gala's main
    for fn, i in p.fns {
        if fn.symbol != "main" do continue
        if _, has := fn.body.?; !has do continue
        ft := p.types[fn.type].(IRFnType)
        cc_ws(&g, "int main(void) {\n")
        if cc_is_void(&g, ft.ret) {
            cc_ws(&g, "    gala_main();\n    return 0;\n")
        } else {
            cc_ws(&g, "    return (int)gala_main();\n")
        }
        cc_ws(&g, "}\n")
        break
    }

    data := transmute([]byte)strings.to_string(g.sb)
    _ = os.write_entire_file(path, data)
    fmt.println("wrote", path)
}
