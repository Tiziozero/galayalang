package main

import "core:fmt"
import "core:strings"

Dumper :: struct {
    prog: ^IRProgram,
    fn:   ^IRFunction, // for local names
    sb:   strings.Builder,
    ind:  int,
}

dump_program :: proc(prog: IRProgram) {
    p := prog
    d := Dumper{prog = &p, sb = strings.builder_make()}

    for t, i in p.types {
        if s, ok := t.(IRStruct); ok {
            fmt.sbprintf(&d.sb, "type t%d = struct %s {{", i, s.name)
            for f, j in s.fields {
                if j > 0 do strings.write_string(&d.sb, ", ")
                dump_type(&d, f)
            }
            strings.write_string(&d.sb, "}\n")
        }
    }
    strings.write_string(&d.sb, "\n")

    for &fn, i in p.fns {
        d.fn = &fn
        fmt.sbprintf(&d.sb, "fn f%d %s  [%v, %v, mod %d]\n", i, fn.symbol, fn.linkage, fn.call_conv, fn.mod)
        if ft, ok := p.types[fn.type].(IRFnType); ok {
            strings.write_string(&d.sb, "  (")
            for pt, j in ft.params {
                if j > 0 do strings.write_string(&d.sb, ", ")
                fmt.sbprintf(&d.sb, "%%%s.%d: ", fn.locals[j].name, j)
                dump_type(&d, pt)
            }
            strings.write_string(&d.sb, ") -> ")
            dump_type(&d, ft.ret)
            strings.write_string(&d.sb, "\n")
        }
        if body, ok := fn.body.?; ok {
            strings.write_string(&d.sb, "{\n")
            d.ind = 1
            dump_stmts(&d, body)
            strings.write_string(&d.sb, "}\n\n")
        } else {
            strings.write_string(&d.sb, "  <declaration>\n\n")
        }
    }
    fmt.print(strings.to_string(d.sb))
}

pad :: proc(d: ^Dumper) {
    for _ in 0..<d.ind do strings.write_string(&d.sb, "  ")
}

dump_type :: proc(d: ^Dumper, id: IRTypeId) {
    switch t in d.prog.types[id] {
    case IRVoid:   strings.write_string(&d.sb, "void")
    case IRBool:   strings.write_string(&d.sb, "bool")
    case IRInt:    fmt.sbprintf(&d.sb, "i%d", t.bits)
    case IRFloat:  fmt.sbprintf(&d.sb, "f%d", t.bits)
    case IRPtr:    strings.write_string(&d.sb, "ptr")
    case IRStruct: fmt.sbprintf(&d.sb, "%%%s", t.name) // name only: structs can be recursive
    case IRBlob:   fmt.sbprintf(&d.sb, "blob(%d,%d)", t.size, t.align)
    case IRArray:
        fmt.sbprintf(&d.sb, "[%d x ", t.count)
        dump_type(d, t.elem)
        strings.write_string(&d.sb, "]")
    case IRFnType: strings.write_string(&d.sb, "fnty")
    }
}

dump_stmts :: proc(d: ^Dumper, stmts: []IRStmt) {
    for s in stmts do dump_stmt(d, s)
}

dump_stmt :: proc(d: ^Dumper, s: IRStmt) {
    pad(d)
    switch v in s {
    case IRLet:
        l := d.fn.locals[v.local]
        fmt.sbprintf(&d.sb, "let %%%s.%d: ", l.name, v.local)
        dump_type(d, l.type)
        if e, ok := v.init.?; ok {
            strings.write_string(&d.sb, " = ")
            dump_expr(d, e)
        }
        strings.write_string(&d.sb, "\n")
    case IRAssign:
        dump_expr(d, v.target^)
        strings.write_string(&d.sb, " = ")
        dump_expr(d, v.value)
        strings.write_string(&d.sb, "\n")
    case IRExprStmt:
        dump_expr(d, v.expr)
        strings.write_string(&d.sb, "\n")
    case IRReturn:
        strings.write_string(&d.sb, "return")
        if e, ok := v.value.?; ok {
            strings.write_string(&d.sb, " ")
            dump_expr(d, e)
        }
        strings.write_string(&d.sb, "\n")
    case IRIf:
        strings.write_string(&d.sb, "if ")
        dump_expr(d, v.cond)
        strings.write_string(&d.sb, " {\n")
        d.ind += 1
        dump_stmts(d, v.then_body)
        d.ind -= 1
        if v.else_body != nil && len(v.else_body) > 0 {
            pad(d)
            strings.write_string(&d.sb, "} else {\n")
            d.ind += 1
            dump_stmts(d, v.else_body)
            d.ind -= 1
        }
        pad(d)
        strings.write_string(&d.sb, "}\n")
    case IRLoop:
        fmt.sbprintf(&d.sb, "loop#%d ", v.id)
        if c, ok := v.cond.?; ok {
            strings.write_string(&d.sb, "while ")
            dump_expr(d, c)
        }
        strings.write_string(&d.sb, " {\n")
        d.ind += 1
        dump_stmts(d, v.body)
        d.ind -= 1
        if len(v.step) > 0 {
            pad(d)
            strings.write_string(&d.sb, "} step {\n")
            d.ind += 1
            dump_stmts(d, v.step)
            d.ind -= 1
        }
        pad(d)
        strings.write_string(&d.sb, "}\n")
    case IRBreak:    fmt.sbprintf(&d.sb, "break loop#%d\n", v.loop)
    case IRContinue: fmt.sbprintf(&d.sb, "continue loop#%d\n", v.loop)
    }
}

// every expr prints as `(expr : type)` so wrong types jump out
dump_expr :: proc(d: ^Dumper, e: IRExpr) {
    strings.write_string(&d.sb, "(")
    dump_kind(d, e)
    strings.write_string(&d.sb, " : ")
    dump_type(d, e.type)
    strings.write_string(&d.sb, ")")
}

dump_kind :: proc(d: ^Dumper, e: IRExpr) {
    switch k in e.kind {
    case IRIntLit:    fmt.sbprintf(&d.sb, "%d", k.value)
    case IRFloatLit:  fmt.sbprintf(&d.sb, "%v", k.value)
    case IRBoolLit:   fmt.sbprintf(&d.sb, "%v", k.value)
    case IRStrLit:    fmt.sbprintf(&d.sb, "%q", k.bytes)
    case IRNullPtr:   strings.write_string(&d.sb, "null")
    case IRZero:      strings.write_string(&d.sb, "zero")
    case IRLocalRef:  fmt.sbprintf(&d.sb, "%%%s.%d", d.fn.locals[k.id].name, k.id)
    case IRGlobalRef: fmt.sbprintf(&d.sb, "@g%d", k.id)
    case IRFnRef:     fmt.sbprintf(&d.sb, "@%s", d.prog.fns[k.id].symbol)
    case IRDeref:
        strings.write_string(&d.sb, "*")
        dump_expr(d, k.ptr^)
    case IRField:
        dump_expr(d, k.base^)
        fmt.sbprintf(&d.sb, ".%d", k.index)
    case IRIndex:
        dump_expr(d, k.base^)
        strings.write_string(&d.sb, "[")
        dump_expr(d, k.index^)
        strings.write_string(&d.sb, "]")
    case IRAddrOf:
        strings.write_string(&d.sb, "&")
        dump_expr(d, k.place^)
    case IRUnary:
        fmt.sbprintf(&d.sb, "%v ", k.op)
        dump_expr(d, k.operand^)
    case IRBinary:
        dump_expr(d, k.lhs^)
        fmt.sbprintf(&d.sb, " %v ", k.op)
        dump_expr(d, k.rhs^)
    case IRLogical:
        dump_expr(d, k.lhs^)
        strings.write_string(&d.sb, " && " if k.is_and else " || ")
        dump_expr(d, k.rhs^)
    case IRPtrOffset:
        dump_expr(d, k.ptr^)
        strings.write_string(&d.sb, " - " if k.subtract else " + ")
        dump_expr(d, k.index^)
        strings.write_string(&d.sb, " * sizeof ")
        dump_type(d, k.elem)
    case IRConvert:
        fmt.sbprintf(&d.sb, "%v ", k.kind)
        dump_expr(d, k.operand^)
    case IRAggregate:
        strings.write_string(&d.sb, "{")
        for el, i in k.elems {
            if i > 0 do strings.write_string(&d.sb, ", ")
            dump_expr(d, el)
        }
        strings.write_string(&d.sb, "}")
    case IRCall:
        if id, ok := k.direct.?; ok {
            fmt.sbprintf(&d.sb, "call @%s", d.prog.fns[id].symbol)
        } else {
            strings.write_string(&d.sb, "call ")
            dump_expr(d, k.indirect^)
        }
        strings.write_string(&d.sb, "(")
        for a, i in k.args {
            if i > 0 do strings.write_string(&d.sb, ", ")
            dump_expr(d, a)
        }
        strings.write_string(&d.sb, ")")
    case IRSeq:
        strings.write_string(&d.sb, "seq { ")
        saved := d.ind
        d.ind = 0
        for s in k.stmts do dump_stmt(d, s)
        d.ind = saved
        dump_expr(d, k.value^)
        strings.write_string(&d.sb, " }")
    }
}
