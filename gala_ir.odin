// 100% ai
package main

import "core:fmt"
import "core:path/filepath"
import "core:strconv"
import "core:strings"

// ===========================================================================
// IR ids
// ===========================================================================

IRTypeId   :: distinct u32
IRFnId     :: distinct u32
IRGlobalId :: distinct u32
IRLocalId  :: distinct u32
IRLoopId   :: distinct u32

// ===========================================================================
// IR types: lowered, monomorphic, LLVM-shaped
// ===========================================================================

IRType :: union {
    IRVoid, IRBool, IRInt, IRFloat, IRPtr, IRStruct, IRBlob, IRArray, IRFnType,
}
IRVoid   :: struct {}
IRBool   :: struct {} // i1 in registers, i8 in memory
IRInt    :: struct { bits: u8 } // signedness lives in the ops
IRFloat  :: struct { bits: u8 }
IRPtr    :: struct {}
IRStruct :: struct { name: string, fields: []IRTypeId }
IRBlob   :: struct { size, align: u32 }
IRArray  :: struct { elem: IRTypeId, count: u64 }
// `variadic` is only set for C-convention fns (C `...`): extra args follow
// `params`, typed by the call site. Gala variadics are a plain trailing slice
// param, so they never set it.
IRFnType :: struct { params: []IRTypeId, ret: IRTypeId, variadic: bool }

// table entries refer to each other by table index, not IRTypeId
IRTypeField :: struct { name: string, type: u64, offset: u64 }
IRTypeInfo  :: struct {
    name:   string,
    kind:   u32,
    size:   u64,
    elem:   u64,
    count:  u64,
    fields: []IRTypeField,
}

// add to the IRExprKind union: yields a ptr to table entry `index`



CallConv :: enum { Gala, C }
Linkage  :: enum { Internal, External, LinkOnceOdr }

// ===========================================================================
// IR expressions
// ===========================================================================

IRExpr :: struct {
    type: IRTypeId,
    kind: IRExprKind,
}

IRTypeInfoRef :: struct { index: ^IRExpr }
IRExprKind :: union {
    IRIntLit, IRFloatLit, IRBoolLit, IRStrLit, IRNullPtr, IRZero,
    IRLocalRef, IRGlobalRef, IRFnRef,
    IRDeref, IRField, IRIndex, IRAddrOf,
    IRUnary, IRBinary, IRLogical, IRPtrOffset, IRConvert,
    IRAggregate, IRCall,
    IRSeq, IRTypeInfoRef
}

IRIntLit    :: struct { value: u64 }
IRFloatLit  :: struct { value: f64 }
IRBoolLit   :: struct { value: bool }
IRStrLit    :: struct { bytes: string }
IRNullPtr   :: struct {}
IRZero      :: struct {} // zeroinitializer of the expr's type
IRLocalRef  :: struct { id: IRLocalId }
IRGlobalRef :: struct { id: IRGlobalId }
IRFnRef     :: struct { id: IRFnId }
IRDeref     :: struct { ptr: ^IRExpr }
IRField     :: struct { base: ^IRExpr, index: u32 }
IRIndex     :: struct { base, index: ^IRExpr }
IRAddrOf    :: struct { place: ^IRExpr }

IRUnOp :: enum { Neg, FNeg, Not, BitNot }
IRUnary :: struct { op: IRUnOp, operand: ^IRExpr }

IRBinOp :: enum {
    Add, Sub, Mul,
    FAdd, FSub, FMul, FDiv,
    SDiv, UDiv, SRem, URem,
    And, Or, Xor, Shl, AShr, LShr,
    Eq, Ne, SLt, SLe, SGt, SGe, ULt, ULe, UGt, UGe,
    FEq, FNe, FLt, FLe, FGt, FGe,
}
IRBinary :: struct { op: IRBinOp, lhs, rhs: ^IRExpr }

IRLogical   :: struct { is_and: bool, lhs, rhs: ^IRExpr }
// result = ptr +/- index * sizeof(elem)
IRPtrOffset :: struct { ptr, index: ^IRExpr, elem: IRTypeId, subtract: bool }
IRConvKind :: enum {
    Trunc, ZExt, SExt,
    FPTrunc, FPExt, FPToSI, FPToUI, SIToFP, UIToFP,
    PtrToInt, IntToPtr, IntToBool, Bitcast,
}
IRConvert :: struct { kind: IRConvKind, operand: ^IRExpr }

IRAggregate :: struct { elems: []IRExpr }
IRCall :: struct {
    direct:   Maybe(IRFnId),
    indirect: ^IRExpr,
    args:     []IRExpr,
}
// run stmts, then yield value
IRSeq :: struct { stmts: []IRStmt, value: ^IRExpr }

// ===========================================================================
// IR statements
// ===========================================================================

IRStmt :: union {
    IRLet, IRAssign, IRExprStmt, IRReturn, IRIf, IRLoop, IRBreak, IRContinue,
}
IRLet      :: struct { local: IRLocalId, init: Maybe(IRExpr) }
IRAssign   :: struct { target: ^IRExpr, value: IRExpr }
IRExprStmt :: struct { expr: IRExpr }
IRReturn   :: struct { value: Maybe(IRExpr) }
IRIf       :: struct { cond: IRExpr, then_body, else_body: []IRStmt }
// `continue` jumps to `step`. `cond` is nil for an infinite loop
IRLoop     :: struct { id: IRLoopId, cond: Maybe(IRExpr), body, step: []IRStmt }
IRBreak    :: struct { loop: IRLoopId }
IRContinue :: struct { loop: IRLoopId }

// ===========================================================================
// IR items
// ===========================================================================

IRLocal :: struct {
    name: string,
    type: IRTypeId,
}

IRFunction :: struct {
    name:      string,
    symbol:    string,
    mod:       ModId,
    type:      IRTypeId, // IRFnType
    call_conv: CallConv,
    linkage:   Linkage,
    locals:    [dynamic]IRLocal, // params first (a gala variadic is the last param)
    body:      Maybe([]IRStmt),  // nil = declaration only
}

IRGlobal :: struct {
    name, symbol: string,
    mod:     ModId,
    type:    IRTypeId,
    linkage: Linkage,
    init:    Maybe(IRExpr),
}

IRProgram :: struct {
    types:      [dynamic]IRType,
    type_info:  [dynamic]IRTypeInfo,
    fns:        [dynamic]IRFunction,
    globals:    [dynamic]IRGlobal,
    type_info_ty, type_field_ty: IRTypeId,
}

// ===========================================================================
// Lowerer state
// ===========================================================================

// a generic instance clone: which fn type it has, and where it came from
InstInfo :: struct {
    ty:       TypeId,
    key:      int,
    template: TypeId,
}

PendingLambda :: struct {
    id:    IRFnId,
    block: Block,
    fty:   TypeId,
    mod:   ModId,
}

Lowerer :: struct {
    c:    ^Context,
    prog: IRProgram,

    type_cache:   map[TypeId]IRTypeId,
    fn_cache:     map[ItemId]IRFnId,
    global_cache: map[ItemId]IRGlobalId,
    obj_items:    map[ObjId]ItemId,
    inst_info:    map[ItemId]InstInfo, // cloned instance item -> info
    symbols:      map[string]struct{}, // uniqueness guard for mangling
    lambdas:      [dynamic]PendingLambda,
    n_lambdas:    int,

    t_void, t_bool, t_i8, t_i16, t_i32, t_i64, t_f32, t_f64, t_ptr: IRTypeId,
    t_slice: IRTypeId, // { ptr, i64 } (also string)
    t_any:   IRTypeId, // { ptr, i64 }: payload pointer + runtime type id
    // Lowerer
    type_index_of: map[TypeId]u64,
    type_queue:    [dynamic]TypeId,
    init_fn:   Maybe(IRFnId),
    init_body: [dynamic]IRStmt,
}

get_init_fn :: proc(l: ^Lowerer) -> IRFnId {
    if id, ok := l.init_fn.?; ok do return id
    fty := add_ir_type(l, IRFnType{ret = l.t_void})
    append(&l.prog.fns, IRFunction{
        name = "$init_globals", symbol = unique_symbol(l, "gala_init_globals"),
        type = fty, call_conv = .Gala, linkage = .External,
    })
    id := IRFnId(len(l.prog.fns) - 1)
    l.init_fn = id
    return id
}

LoopFrame :: struct { id: IRLoopId }

FnBuilder :: struct {
    l:         ^Lowerer,
    fn:        IRFnId,
    mod:       ModId,
    ret_gt:    TypeId,   // declared return type, for boxing `return x` into any
    arg_names: []string, // params are locals 0..n-1, matched to Argument objects by name
    local_of:  map[ObjId]IRLocalId,
    out:       ^[dynamic]IRStmt,
    loops:     [dynamic]LoopFrame,
    n_loops:   u32,
    is_global: bool,
}

// ===========================================================================
// Entry point
// ===========================================================================

// Lowers every non-generic item of every module. Generic templates are skipped;
// their instances are plain FnDecs in the template's module AST, so they come
// through the normal path. LLVM drops what's unused.
lower_program :: proc(c: ^Context, units: []ModuleUnit) -> IRProgram {
    l: Lowerer
    init_lowerer(&l, c)

    // pass 1: declare everything (so any body can reference any fn)
    for u in units {
        for id in u.ast.items[:] do declare_item(&l, u.mod, id)
    }
    // pass 2: bodies
    for u in units {
        for id in u.ast.items[:] do lower_item(&l, u.mod, id)
    }
    // lambdas are discovered inside bodies. lowering one may discover more
    for len(l.lambdas) > 0 {
        p := pop(&l.lambdas)
        lower_lambda_body(&l, p)
    }
    if id, ok := l.init_fn.?; ok do l.prog.fns[id].body = l.init_body[:]
    build_type_table(&l)
    return l.prog
}

init_lowerer :: proc(l: ^Lowerer, c: ^Context) {
    l.c = c
    for iid, oid in c.item_objects do l.obj_items[oid] = iid

    for tid, tmpl in c.fn_templates {
        for inst, i in tmpl.instances {
            if !inst.resolved {
                gala_panicf("generic instance {} of template {} was never resolved", i, tid)
            }
            l.inst_info[inst.resolved_item] = InstInfo{ty = inst.ty_id, key = i, template = tid}
        }
    }

    l.t_void = add_ir_type(l, IRVoid{})
    l.t_bool = add_ir_type(l, IRBool{})
    l.t_i8   = add_ir_type(l, IRInt{8})
    l.t_i16  = add_ir_type(l, IRInt{16})
    l.t_i32  = add_ir_type(l, IRInt{32})
    l.t_i64  = add_ir_type(l, IRInt{64})
    l.t_f32  = add_ir_type(l, IRFloat{32})
    l.t_f64  = add_ir_type(l, IRFloat{64})
    l.t_ptr  = add_ir_type(l, IRPtr{})

    fields := make([]IRTypeId, 2)
    fields[0], fields[1] = l.t_ptr, l.t_i64
    l.t_slice = add_ir_type(l, IRStruct{name = "slice", fields = fields})
    l.t_any   = add_ir_type(l, IRStruct{name = "any",   fields = fields})
    l.prog.type_info_ty  = lower_type(l, ty_from_name("TypeInfo"))
    l.prog.type_field_ty = lower_type(l, ty_from_name("TypeField"))
}

add_ir_type :: proc(l: ^Lowerer, t: IRType) -> IRTypeId {
    append(&l.prog.types, t)
    return IRTypeId(len(l.prog.types) - 1)
}

// ===========================================================================
// Type lowering
// ===========================================================================

lower_type :: proc(l: ^Lowerer, tid: TypeId) -> IRTypeId {
    if cached, ok := l.type_cache[tid]; ok do return cached
    t := get_type(tid)
    r: IRTypeId

    #partial switch t.kind {
    case .Generic:
        gala_panicf("generic type \"%s\" reached lowering (instances should be concrete)", t.name)

    case .Void:                         r = l.t_void
    case .Bool:                         r = l.t_bool
    case .UInt64, .Int64:               r = l.t_i64
    case .UInt32, .Int32:               r = l.t_i32
    case .UInt16, .Int16:               r = l.t_i16
    case .UInt_8, .Int_8, .Byte, .Rune: r = l.t_i8
    case .Flt64:                        r = l.t_f64
    case .Flt32:                        r = l.t_f32
    case .Pointer, .Function:           r = l.t_ptr
    case .Slice, .String:               r = l.t_slice
    case .Any:                          r = l.t_any

    // checker should've defaulted these
    case .UntypedInteger:               r = l.t_i64
    case .UntypedFloat:                 r = l.t_f64

    case .FixedSizeArray:
        elem := lower_type(l, t.fixed_size_array.type)
        r = add_ir_type(l, IRArray{elem = elem, count = u64(t.fixed_size_array.size)})

    case .Struct:
        // reserve first so self-referential structs (through pointers) resolve
        r = add_ir_type(l, IRStruct{name = t.name})
        l.type_cache[tid] = r
        fields := make([]IRTypeId, len(t.structure.fields))
        for f, i in t.structure.fields do fields[i] = lower_type(l, f.type)
        l.prog.types[r] = IRStruct{name = t.name, fields = fields}
        return r

    case .Union:
        r = add_ir_type(l, IRBlob{size = u32(type_size(tid)), align = u32(type_align(tid))})

    case:
        gala_panic("lower_type: unexpected kind", t.kind)
    }

    l.type_cache[tid] = r
    return r
}

// Variadics depend on the calling convention:
//   Gala: the variadic is one extra trailing param of type effective_variadic_ty
//         (a slice of the variadic type)
//   C:    no extra param, the IR fn type is marked variadic and call sites
//         pass the extra args as they are
lower_fn_type :: proc(l: ^Lowerer, tid: TypeId, cc: CallConv = .Gala) -> IRTypeId {
    t := get_type(tid)
    gala_variadic := t.fn.is_variadic && cc == .Gala

    n_fixed := len(t.fn.args)
    n := n_fixed + len(t.fn.default_args)
    if gala_variadic do n += 1
    params := make([]IRTypeId, n)
    for a, i in t.fn.args do params[i] = lower_type(l, a.ty)
    for d, i in t.fn.default_args do params[n_fixed + i] = lower_type(l, d.ty)
    if gala_variadic do params[n - 1] = lower_type(l, t.fn.effective_variadic_ty)

    return add_ir_type(l, IRFnType{
        params   = params,
        ret      = lower_type(l, t.fn.ret_ty),
        variadic = t.fn.is_variadic && cc == .C,
    })
}

// params first: the fixed args, then the defaults, then (Gala only) the variadic slice
add_param_locals :: proc(l: ^Lowerer, fn: ^IRFunction, fty: TypeId, cc: CallConv) {
    t := get_type(fty)
    for a in t.fn.args {
        append(&fn.locals, IRLocal{name = a.name, type = lower_type(l, a.ty)})
    }
    for d in t.fn.default_args {
        append(&fn.locals, IRLocal{name = d.name, type = lower_type(l, d.ty)})
    }
    if t.fn.is_variadic && cc == .Gala {
        append(&fn.locals, IRLocal{
            name = t.fn.variadic_name,
            type = lower_type(l, t.fn.effective_variadic_ty),
        })
    }
}

// bodies are always Gala-convention, so a variadic is the last param (a slice)
lower_fn_body :: proc(l: ^Lowerer, fid: IRFnId, mod: ModId, block: Block, fty: TypeId) {
    ft := get_type(fty)
    n_fixed := len(ft.fn.args)
    n := n_fixed + len(ft.fn.default_args)
    if ft.fn.is_variadic do n += 1
    names := make([]string, n)
    for a, i in ft.fn.args do names[i] = a.name
    for d, i in ft.fn.default_args do names[n_fixed + i] = d.name
    if ft.fn.is_variadic do names[n - 1] = ft.fn.variadic_name

    body: [dynamic]IRStmt
    fb := FnBuilder{l = l, fn = fid, mod = mod, ret_gt = ft.fn.ret_ty, arg_names = names, out = &body}
    lower_block(&fb, block)
    l.prog.fns[fid].body = body[:]
}

lower_call :: proc(fb: ^FnBuilder, id: ExprId, call: FnCall, ty: IRTypeId) -> IRExpr {
    l := fb.l
    out := IRCall{}
    callee_fty: TypeId

    if inst, ok := l.c.call_instances[id]; ok {
        // generic call: go straight to the instance's cloned item
        tmpl := l.c.fn_templates[inst.template]
        item := tmpl.instances[inst.instance_key].resolved_item
        fid, declared := l.fn_cache[item]
        if !declared {
            gala_panicf("generic instance {} of {} wasn't declared (is the clone in its module's ast.items?)",
                inst.instance_key, inst.template)
        }
        out.direct = fid
        callee_fty = tmpl.instances[inst.instance_key].ty_id
    } else {
        callee_fty = ir_expr_ty(l, call.target)
        callee := lower_expr(fb, call.target)
        if ref, is_fn := callee.kind.(IRFnRef); is_fn do out.direct = ref.id
        else                                           do out.indirect = new_expr_from(callee)
    }

    // the callee's convention decides how variadics are passed. function
    // pointers are always Gala
    cc := CallConv.Gala
    if fid, is_direct := out.direct.(IRFnId); is_direct do cc = l.prog.fns[fid].call_conv

    ft := get_type(callee_fty).fn
    m := match_call_args(id, call, ft)

    args: [dynamic]IRExpr
    for ci, i in m.fixed {
        append(&args, lower_expr_to(fb, call.args[ci].expr, ft.args[i].ty))
    }
    // an omitted default (-1) is lowered right here from its declaration's expression
    for ci, i in m.defaults {
        d := ft.default_args[i]
        src := d.expr if ci == -1 else call.args[ci].expr
        append(&args, lower_expr_to(fb, src, d.ty))
    }

    if ft.is_variadic {
        extra := make([]ExprId, len(m.variadic))
        for ci, i in m.variadic do extra[i] = call.args[ci].expr

        if cc == .Gala {
            append(&args, lower_variadic_pack(fb, ft.effective_variadic_ty, extra))
        } else {
            for ex in extra do append(&args, lower_c_vararg(fb, ex))
        }
    }
    out.args = args[:]
    return IRExpr{ty, out}
}
// ===========================================================================
// Type table
// ===========================================================================

// Every runtime type id is an index into the type table. Ids only ever come
// from box_any / type_id, so registering lazily there (plus the children of
// each entry, see build_type_table) covers everything type_info() can reach.
type_index :: proc(l: ^Lowerer, tid: TypeId) -> u64 {
    if i, ok := l.type_index_of[tid]; ok do return i
    i := u64(len(l.type_queue))
    l.type_index_of[tid] = i
    append(&l.type_queue, tid)
    return i
}

// the numbers gala sees as KIND_*. keep in sync with the constants.odin entries
TypeInfoKind :: enum u32 {
    Other   = 0,
    Int     = 1,
    UInt    = 2,
    Float   = 3,
    Bool    = 4,
    Byte    = 5,
    Rune    = 6,
    String  = 7,
    Pointer = 8,
    Any     = 9,
    Struct  = 10,
    Union   = 11,
    Array   = 12,
    Slice   = 13,
}

kind_const :: proc(k: TypeKind) -> u32 {
    #partial switch k {
    case .Int64, .Int32, .Int16, .Int_8:     return u32(TypeInfoKind.Int)
    case .UInt64, .UInt32, .UInt16, .UInt_8: return u32(TypeInfoKind.UInt)
    case .Flt64, .Flt32, .Flt16, .Flt_8:     return u32(TypeInfoKind.Float)
    case .Bool:                              return u32(TypeInfoKind.Bool)
    case .Byte:                              return u32(TypeInfoKind.Byte)
    case .Rune:                              return u32(TypeInfoKind.Rune)
    case .String:                            return u32(TypeInfoKind.String)
    case .Pointer:                           return u32(TypeInfoKind.Pointer)
    case .Any:                               return u32(TypeInfoKind.Any)
    case .Struct:                            return u32(TypeInfoKind.Struct)
    case .Union:                             return u32(TypeInfoKind.Union)
    case .FixedSizeArray:                    return u32(TypeInfoKind.Array)
    case .Slice:                             return u32(TypeInfoKind.Slice)
    }
    return u32(TypeInfoKind.Other) // fns, unions, void
}

// Worklist: building an entry registers its children, which appends to
// type_queue, so the loop keeps going until nothing new shows up. Entry i of
// prog.type_info belongs to type_queue[i].
build_type_table :: proc(l: ^Lowerer) {
    for i := 0; i < len(l.type_queue); i += 1 {
        tid := l.type_queue[i]
        t   := get_type(tid)
        info := IRTypeInfo{name = tts(tid), kind = kind_const(t.kind)}

        #partial switch t.kind {
        case .Void:
            info.size = 0

        case .Function:
            info.size = 8

        case .Pointer:
            info.size = 8
            info.elem = type_index(l, t.ptr)

        case .Slice:
            info.size = 16
            info.elem = type_index(l, t.slice.type)

        case .FixedSizeArray:
            info.size  = u64(type_size(tid))
            info.elem  = type_index(l, t.fixed_size_array.type)
            info.count = u64(t.fixed_size_array.size)

        case .Struct:
            info.size = u64(type_size(tid))
            fields := make([]IRTypeField, len(t.structure.fields))
            offset := 0
            for f, j in t.structure.fields {
                offset = align_up(offset, type_align(f.type))
                fields[j] = IRTypeField{
                    name   = f.name,
                    type   = type_index(l, f.type),
                    offset = u64(offset),
                }
                offset += type_size(f.type)
            }
            info.fields = fields
        case .Union:
            info.size = u64(type_size(tid))
            fields := make([]IRTypeField, len(t.union_fields))
            for f, j in t.union_fields {
                fields[j] = IRTypeField{name = f.name, type = type_index(l, f.type), offset = 0}
            }
            info.fields = fields

        case .Generic, .Invalid, .UntypedInteger, .UntypedFloat, .UntypedStruct:
            gala_panicf("type \"%s\" reached the type table (not concrete)", tts(tid))

        case:
            info.size = u64(type_size(tid)) // scalars, string, any
        }
        append(&l.prog.type_info, info)
    }
}


// ===========================================================================
// Naming
// ===========================================================================

sanitize :: proc(s: string) -> string {
    sb := strings.builder_make()
    for ch in s {
        switch {
        case ch >= 'a' && ch <= 'z', ch >= 'A' && ch <= 'Z', ch >= '0' && ch <= '9', ch == '_':
            strings.write_rune(&sb, ch)
        case ch == '^': strings.write_string(&sb, "P")
        case ch == '[': strings.write_string(&sb, "A")
        case ch == ']': // dropped
        case:           strings.write_rune(&sb, '_')
        }
    }
    return strings.to_string(sb)
}

mod_name :: proc(l: ^Lowerer, mod: ModId) -> string {
    return sanitize(filepath.stem(filepath.base(l.c.mods[mod].path)))
}

// _G<len><module><len><name>[I<len><type>...E]
// Length-prefixed so modules/names/types can't run into each other. Only
// [A-Za-z0-9_], so it's valid in any object format without quoting.
mangle :: proc(l: ^Lowerer, mod: ModId, name: string, targs: []TypeId = nil) -> string {
    m  := mod_name(l, mod)
    sb := strings.builder_make()
    fmt.sbprintf(&sb, "_G%d%s%d%s", len(m), m, len(name), name)
    if len(targs) > 0 {
        strings.write_string(&sb, "I")
        for t in targs {
            s := sanitize(tts(t))
            fmt.sbprintf(&sb, "%d%s", len(s), s)
        }
        strings.write_string(&sb, "E")
    }
    return unique_symbol(l, strings.to_string(sb))
}

unique_symbol :: proc(l: ^Lowerer, sym: string) -> string {
    s := sym
    if s in l.symbols do s = fmt.aprintf("%s_%d", sym, len(l.symbols))
    l.symbols[s] = {}
    return s
}

// ===========================================================================
// Declaration pass
// ===========================================================================

// type of an fn item: an instance's own type, or the object's resolved type
item_fn_type :: proc(l: ^Lowerer, id: ItemId) -> (TypeId, bool) {
    if info, ok := l.inst_info[id]; ok do return info.ty, true
    if oid, ok := l.c.item_objects[id]; ok {
        if t, has := get_obj(oid).type.(TypeId); has do return t, true
    }
    return 0, false
}

// generic templates produce no IR
is_template_fn :: proc(fty: TypeId) -> bool {
    return get_type(fty).fn.is_generic || is_generic(fty)
}

declare_item :: proc(l: ^Lowerer, mod: ModId, id: ItemId) {
    #partial switch it in get_item(id) {
    case FnDec:
        fty, ok := item_fn_type(l, id)
        if !ok do gala_panicf("fn \"%s\" has no resolved type", it.name)
        if is_template_fn(fty) do return

        sym:     string
        linkage: Linkage = .External
        if info, is_inst := l.inst_info[id]; is_inst {
            targs := make([]TypeId, len(get_type(fty).fn.args))
            for a, i in get_type(fty).fn.args do targs[i] = a.ty
            sym     = mangle(l, mod, it.name, targs)
            linkage = .LinkOnceOdr
            _ = info
        } else if it.name == "main" {
            sym = unique_symbol(l, "main")
        } else {
            sym = mangle(l, mod, it.name)
        }
        // TODO: `export` semantics (symbol name / linkage) once you've decided what it means
        declare_fn(l, id, mod, it.name, sym, fty, linkage, .Gala)

    case ExternFnDec:
        fty, ok := item_fn_type(l, id)
        if !ok do gala_panicf("extern fn \"%s\" has no resolved type", it.name)
        declare_fn(l, id, mod, it.name, it.name, fty, .External, .C)

    case StructDec:
        if len(it.generics) > 0 do return
        if tid, ok := l.c.item_types[id]; ok && !is_generic(tid) {
            lower_type(l, tid) // make sure the struct exists in the IR
        }

    case GlobalVarDec:
        declare_global(l, mod, id)
    case  ConstDec: {} // ignore

    case TypeAlias, Import, Forward:
        // nothing to emit
    }
}

declare_fn :: proc(l: ^Lowerer, id: ItemId, mod: ModId, name, symbol: string,
                   fty: TypeId, linkage: Linkage, cc: CallConv) -> IRFnId {
    fn := IRFunction{
        name = name, symbol = symbol, mod = mod,
        type = lower_fn_type(l, fty, cc), call_conv = cc, linkage = linkage,
    }
    add_param_locals(l, &fn, fty, cc)
    append(&l.prog.fns, fn)
    fid := IRFnId(len(l.prog.fns) - 1)
    l.fn_cache[id] = fid
    return fid
}

declare_global :: proc(l: ^Lowerer, mod: ModId, id: ItemId) {
    it := get_item(id).(GlobalVarDec)
    oid, ok := l.c.item_objects[id]
    if !ok do gala_panicf("global \"%s\" has no object", it.name)
    vt, has := get_obj(oid).type.(TypeId)
    if !has do gala_panicf("global \"%s\" has no type after typechecking", it.name)

    append(&l.prog.globals, IRGlobal{
        name    = it.name,
        symbol  = mangle(l, mod, it.name),
        mod     = mod,
        type    = lower_type(l, vt),
        linkage = .External,
    })
    l.global_cache[id] = IRGlobalId(len(l.prog.globals) - 1)
}

// ===========================================================================
// Bodies
// ===========================================================================

lower_item :: proc(l: ^Lowerer, mod: ModId, id: ItemId) {
    #partial switch it in get_item(id) {
    case FnDec:
        fid, ok := l.fn_cache[id]
        if !ok do return // template
        lower_fn_body(l, fid, mod, it.block, item_fn_type_must(l, id))

    case GlobalVarDec:
        // pass 2 so the initialiser can reference any fn or global
        gid, ok := l.global_cache[id]
        if !ok do return
        lower_global_init(l, mod, id, gid, it)
    }
}

// Globals are initialised statically (the C backend emits `T g = <init>;`), so
// the value has to be a constant expression. Without this every global stays
// zero: FD_STDOUT, CHAR_*, KIND_* and BUILDER_CAPACITY included, which is why
// println wrote nothing.
// NOTE: assumes GlobalVarDec has a `value: ExprId` like VarDec. Adjust the field
// name (and skip the call when a global has no initialiser) to match the AST.
lower_global_init :: proc(l: ^Lowerer, mod: ModId, id: ItemId, gid: IRGlobalId, it: GlobalVarDec) {
    oid := l.c.item_objects[id]
    vt, ok := get_obj(oid).type.(TypeId)
    if !ok do gala_panicf("global \"%s\" has no type after typechecking", it.name)

    init_fn := get_init_fn(l)
    locals_before := len(l.prog.fns[init_fn].locals)
    scratch: [dynamic]IRStmt
    fb := FnBuilder{l = l, fn = init_fn, mod = mod, out = &scratch, is_global = true}
    init := lower_expr_to(&fb, it.value, vt)

    // no side statements and no temps: a plain static initialiser
    if len(scratch) == 0 && len(l.prog.fns[init_fn].locals) == locals_before {
        #partial switch _ in init.kind {
        case IRIntLit, IRFloatLit, IRBoolLit:
            init.type = l.prog.globals[gid].type
        }
        l.prog.globals[gid].init = init
        return
    }

    // otherwise run it at startup
    for s in scratch do append(&l.init_body, s)
    target := new_expr_from(IRExpr{l.prog.globals[gid].type, IRGlobalRef{gid}})
    append(&l.init_body, IRAssign{target = target, value = init})
}

item_fn_type_must :: proc(l: ^Lowerer, id: ItemId) -> TypeId {
    t, ok := item_fn_type(l, id)
    assert(ok)
    return t
}


lower_lambda_body :: proc(l: ^Lowerer, p: PendingLambda) {
    lower_fn_body(l, p.id, p.mod, p.block, p.fty)
}

// ===========================================================================
// Statements
// ===========================================================================

emit :: proc(fb: ^FnBuilder, s: IRStmt) { append(fb.out, s) }

new_local :: proc(fb: ^FnBuilder, name: string, ty: IRTypeId) -> IRLocalId {
    locals := &fb.l.prog.fns[fb.fn].locals
    append(locals, IRLocal{name = name, type = ty})
    return IRLocalId(len(locals) - 1)
}

lower_nested :: proc(fb: ^FnBuilder, b: Block) -> []IRStmt {
    saved := fb.out
    list: [dynamic]IRStmt
    fb.out = &list
    lower_block(fb, b)
    fb.out = saved
    return list[:]
}

lower_block :: proc(fb: ^FnBuilder, b: Block) {
    for sid in b.stmts do lower_stmt(fb, sid)
}

// lowers an expression whose side-statements must run only when it's evaluated
// (loop conditions, rhs of && / ||): they get wrapped in an IRSeq
lower_expr_scoped :: proc(fb: ^FnBuilder, id: ExprId) -> IRExpr {
    saved := fb.out
    list: [dynamic]IRStmt
    fb.out = &list
    e := lower_expr(fb, id)
    fb.out = saved
    if len(list) == 0 do return e
    return IRExpr{e.type, IRSeq{list[:], new_expr_from(e)}}
}

lower_stmt :: proc(fb: ^FnBuilder, id: StmtId) {
    l := fb.l
    switch s in get_stmt(id) {
    case VarDec:
        oid := l.c.stmt_objects[id]
        vt, ok := get_obj(oid).type.(TypeId)
        if !ok do gala_panicf("variable \"%s\" has no type after typechecking", s.name)
        init := lower_expr_to(fb, s.value, vt)
        loc := new_local(fb, s.name, lower_type(l, vt))
        fb.local_of[oid] = loc
        emit(fb, IRLet{local = loc, init = init})

    case Assignment:
        // compound ops are already `a = a op b` in the AST: don't apply them again
        target_gt := ir_expr_ty(l, s.target)
        target := new_expr_from(lower_expr(fb, s.target))
        value := lower_expr_to(fb, s.value, target_gt)
        emit(fb, IRAssign{target = target, value = value})

    case Return:
        if e, ok := s.expr.(ExprId); ok do emit(fb, IRReturn{value = lower_expr_to(fb, e, fb.ret_gt)})
        else                           do emit(fb, IRReturn{})

    case ExprId:
        emit(fb, IRExprStmt{expr = lower_expr(fb, s)})

    case IfElse:
        lower_if(fb, s)

    case WhileLoop:
        lid := push_loop(fb)
        cond := lower_expr_scoped(fb, s.cond)
        body := lower_nested(fb, s.block)
        pop_loop(fb)
        emit(fb, IRLoop{id = lid, cond = cond, body = body})

    case ForLoop:
        lower_for(fb, s, id)

    case BreakStmt:
        emit(fb, IRBreak{loop = innermost_loop(fb)})

    case ContinueStmt:
        emit(fb, IRContinue{loop = innermost_loop(fb)})
    }
}

lower_if :: proc(fb: ^FnBuilder, s: IfElse) {
    // if / else if / else  =>  nested IRIf, built from the back. every `else if`
    // condition is lowered inside its own list so its temps run only when reached
    tail: []IRStmt
    if s.has_else_block do tail = lower_nested(fb, s.else_block)

    for i := len(s.alt) - 1; i >= 0; i -= 1 {
        alt := s.alt[i]
        saved := fb.out
        list: [dynamic]IRStmt
        fb.out = &list
        cond := lower_expr(fb, alt.cond)
        then_body := lower_nested(fb, alt.block)
        append(&list, IRIf{cond = cond, then_body = then_body, else_body = tail})
        fb.out = saved
        tail = list[:]
    }
    cond := lower_expr(fb, s.base_cond)
    then_body := lower_nested(fb, s.base_block)
    emit(fb, IRIf{cond = cond, then_body = then_body, else_body = tail})
}

// for a, b in iterable { body }   (arrays, slices, strings)
//   a = element, b = index (optional)
//   =>  let $iter = iterable; let $i = 0
//       loop ($i < len) { let a = $iter[$i]; body } step { $i = $i + 1 }
lower_for :: proc(fb: ^FnBuilder, s: ForLoop, id: StmtId) {
    l := fb.l
    if len(s.names) < 1 || len(s.names) > 2 {
        gala_panic("for: expected 1 or 2 names")
    }
    it_gt := ir_expr_ty(l, s.expr)
    kind := get_type(it_gt).kind

    iter_val := lower_expr(fb, s.expr)
    tmp := new_local(fb, "$iter", iter_val.type)
    emit(fb, IRLet{local = tmp, init = iter_val})
    idx := new_local(fb, "$i", l.t_i64)
    emit(fb, IRLet{local = idx, init = int_lit(l.t_i64, 0)})

    tmp_e := IRExpr{iter_val.type, IRLocalRef{tmp}}
    i_e   := IRExpr{l.t_i64, IRLocalRef{idx}}

    count_e: IRExpr
    #partial switch kind {
    case .FixedSizeArray:
        count_e = int_lit(l.t_i64, u64(get_type(it_gt).fixed_size_array.size))
    case .Slice, .String:
        count_e = IRExpr{l.t_i64, IRField{new_expr_from(tmp_e), 1}}
    case:
        gala_panic("for: cannot iterate", kind)
    }

    elem_gt: TypeId
    if kind == .String {
        elem_gt = ty_from_name("byte")
    } else {
        elem_gt, _ = index_elem_type(it_gt)
    }
    elem_ty := lower_type(l, elem_gt)

    elem_e: IRExpr
    if kind == .FixedSizeArray {
        elem_e = IRExpr{elem_ty, IRIndex{new_expr_from(tmp_e), new_expr_from(i_e)}}
    } else {
        data := IRExpr{l.t_ptr, IRField{new_expr_from(tmp_e), 0}}
        addr := IRExpr{l.t_ptr, IRPtrOffset{
            ptr = new_expr_from(data), index = new_expr_from(i_e), elem = elem_ty,
        }}
        elem_e = IRExpr{elem_ty, IRDeref{new_expr_from(addr)}}
    }

    cond := IRExpr{l.t_bool, IRBinary{.SLt, new_expr_from(i_e), new_expr_from(count_e)}}
    step := make([]IRStmt, 1)
    step[0] = IRAssign{
        target = new_expr_from(i_e),
        value  = IRExpr{l.t_i64, IRBinary{.Add, new_expr_from(i_e), new_expr_from(int_lit(l.t_i64, 1))}},
    }

    lid := push_loop(fb)
    saved := fb.out
    body: [dynamic]IRStmt
    fb.out = &body

    elem_loc := new_local(fb, s.names[0].name, elem_ty)
    fb.local_of[s.names[0].oid] = elem_loc
    // the index is just the loop counter. loop vars are immutable, so no copy
    if len(s.names) == 2 do fb.local_of[s.names[1].oid] = idx
    emit(fb, IRLet{local = elem_loc, init = elem_e})

    lower_block(fb, s.block)
    fb.out = saved
    pop_loop(fb)

    emit(fb, IRLoop{id = lid, cond = cond, body = body[:], step = step})
}

push_loop :: proc(fb: ^FnBuilder) -> IRLoopId {
    id := IRLoopId(fb.n_loops)
    fb.n_loops += 1
    append(&fb.loops, LoopFrame{id})
    return id
}
pop_loop :: proc(fb: ^FnBuilder) { pop(&fb.loops) }

// break/continue have no labels in the parser yet
innermost_loop :: proc(fb: ^FnBuilder) -> IRLoopId {
    if len(fb.loops) == 0 do gala_panic("break/continue outside of a loop")
    return fb.loops[len(fb.loops) - 1].id
}

// ===========================================================================
// Expressions
// ===========================================================================

// type of an expression as the checker saw it (casts/literals fall back to the
// type the resolver attached)
ir_expr_ty :: proc(l: ^Lowerer, id: ExprId) -> TypeId {
    if t, ok := l.c.tc_types[id]; ok do return t
    if t, ok := l.c.expr_types[id]; ok do return t
    highlight_lines(get_span(id))
    gala_panic("expression has no type after typechecking")
}

new_expr_from :: proc(e: IRExpr) -> ^IRExpr {
    p := new(IRExpr)
    p^ = e
    return p
}

int_lit :: proc(ty: IRTypeId, v: u64) -> IRExpr { return IRExpr{ty, IRIntLit{v}} }

// ---- any boxing ----
// The typed AST stays as the checker left it. Wherever a value flows into a
// slot of a known type (arg, vardec, assign, return, field, element), the
// lowerer boxes it if that slot is `any` and the value isn't already one.

needs_box :: proc(to, from: TypeId) -> bool {
    return get_type(to).kind == .Any && get_type(from).kind != .Any
}

lower_expr_to :: proc(fb: ^FnBuilder, id: ExprId, to_gt: TypeId) -> IRExpr {
    e := lower_expr(fb, id)
    from_gt := ir_expr_ty(fb.l, id)
    if !needs_box(to_gt, from_gt) do return e

    // numbers can still be untyped here (propagate_type skips Number):
    // box them as what `x := 1` would be
    if is_untyped(from_gt) {
        d, ok, msg := get_assignable_default_type(from_gt)
        if !ok {
            highlight_lines(get_span(id))
            gala_panicf("can't box into any: {}", msg)
        }
        from_gt = d
    }
    return box_any(fb, e, from_gt)
}

// any = { ptr to a stack copy of the value, type id }. The copy is a local of
// the current fn, so the any is only valid while that fn is running.
box_any :: proc(fb: ^FnBuilder, v: IRExpr, gt: TypeId) -> IRExpr {
    l := fb.l
    tmp := new_local(fb, "$box", v.type)
    stmts := make([]IRStmt, 1)
    stmts[0] = IRLet{local = tmp, init = v}

    place := IRExpr{v.type, IRLocalRef{tmp}}
    elems := make([]IRExpr, 2)
    elems[0] = IRExpr{l.t_ptr, IRAddrOf{new_expr_from(place)}}
    // runtime type id = index into the type table (registers gt and, via
    // build_type_table, everything reachable from it)
    elems[1] = int_lit(l.t_i64, type_index(l, gt))
    value := IRExpr{l.t_any, IRAggregate{elems}}
    return IRExpr{l.t_any, IRSeq{stmts, new_expr_from(value)}}
}

lower_expr :: proc(fb: ^FnBuilder, id: ExprId) -> IRExpr {
    l := fb.l
    gt := ir_expr_ty(l, id)
    ty := lower_type(l, gt)

    #partial switch e in get_expr(id) {
    case Number:
        if is_float(gt) || get_type(gt).kind == .UntypedFloat {
            v, _ := strconv.parse_f64(e.text)
            return IRExpr{ty, IRFloatLit{v}}
        }
        v, _ := strconv.parse_u64_maybe_prefixed(e.text)
        return IRExpr{ty, IRIntLit{v}}

    case BoolLitTrue:  return IRExpr{ty, IRBoolLit{true}}
    case BoolLitFalse: return IRExpr{ty, IRBoolLit{false}}
    case String:       return IRExpr{ty, IRStrLit{e.s}}

    case Symbol:
        return lower_symbol(fb, id, ty)

    case Binop:
        return lower_binop(fb, e, ty)

    case UnNegative:
        op := IRUnOp.FNeg if is_float(ir_expr_ty(l, e.expr)) else IRUnOp.Neg
        return IRExpr{ty, IRUnary{op, new_expr_from(lower_expr(fb, e.expr))}}

    case UnNot:
        return IRExpr{ty, IRUnary{.Not, new_expr_from(lower_expr(fb, e.expr))}}

    case Reference:
        return IRExpr{ty, IRAddrOf{new_expr_from(lower_expr(fb, e.expr))}}

    case Deref:
        return IRExpr{ty, IRDeref{new_expr_from(lower_expr(fb, e.expr))}}

    case FieldAccess:
        return lower_field_access(fb, e, ty)

    case Index:
        return lower_index(fb, e, ty)

    case TakeSlice:
        return lower_take_slice(fb, e, ty)

    case Cast:
        return lower_cast(fb, e.target, gt, ty)

    case Transmute:
        return IRExpr{ty, IRConvert{.Bitcast, new_expr_from(lower_expr(fb, e.target))}}

    case FnCall:
        return lower_call(fb, id, e, ty)

    case AggregateLit:
        return lower_aggregate(fb, id, e, gt, ty)

    case FixedSizeArray:
        return lower_array_lit(fb, e, gt, ty)

    case FnLit:
        return lower_fn_lit(fb, e, gt, ty)
    case GenericInst:
        // standalone `f::<A, B>`: a reference to the instance's cloned fn.
        // (as a call target, lower_call handles it through call_instances[call id])
        inst, ok := l.c.call_instances[id]
        if !ok {
            highlight_lines(get_span(id))
            gala_panic("generic instantiation was never typechecked")
        }
        item := l.c.fn_templates[inst.template].instances[inst.instance_key].resolved_item
        fid, declared := l.fn_cache[item]
        if !declared {
            gala_panicf("generic instance {} of {} wasn't declared (is the clone in its module's ast.items?)",
                inst.instance_key, inst.template)
        }
        return IRExpr{ty, IRFnRef{fid}}
    case SizeOf:
        return int_lit(ty, u64(type_size(queried_type(l, id, e.expr))))

    case TypeIdOf:
        // same number box_any stores in an any's type slot
        return int_lit(ty, type_index(l, queried_type(l, id, e.expr)))

    case Len:
        t := ir_expr_ty(l, e.target)
        if get_type(t).kind == .FixedSizeArray {
            return int_lit(ty, u64(get_type(t).fixed_size_array.size))
        }
        // slice / string: runtime read of the length field
        n := IRExpr{l.t_i64, IRField{new_expr_from(lower_expr(fb, e.target)), 1}}
        return from_i64(l, n, ty, gt)

    case TypeInfoOf:
        return IRExpr{ty, IRTypeInfoRef{new_expr_from(lower_expr(fb, e.expr))}}
    }
    highlight_lines(get_span(id))
    gala_panic("lower_expr: unhandled expression")
}

// the type a size_of / type_id asks about. The checker stores it in expr_types
// (type form: resolved by the resolver, expression form: defaulted by the checker)
queried_type :: proc(l: ^Lowerer, id: ExprId, expr: $E) -> TypeId {
    if t, ok := l.c.expr_types[id]; ok do return t
    if ex, ok := expr.(ExprId); ok {
        t, ok2, msg := get_assignable_default_type(ir_expr_ty(l, ex))
        if !ok2 do gala_panicf("can't take the type of this expression: {}", msg)
        return t
    }
    highlight_lines(get_span(id))
    gala_panic("size_of / type_id: no resolved type")
}

// a runtime i64 (like a slice's length) that the checker re-typed because it
// was untyped (x: i32 = len(s), foo(u8) etc.) needs a real conversion
from_i64 :: proc(l: ^Lowerer, n: IRExpr, to: IRTypeId, to_gt: TypeId) -> IRExpr {
    if to == l.t_i64 do return IRExpr{to, n.kind}
    if is_float(to_gt) do return IRExpr{to, IRConvert{.SIToFP, new_expr_from(n)}}
    return IRExpr{to, IRConvert{.Trunc, new_expr_from(n)}}
}
lower_symbol :: proc(fb: ^FnBuilder, id: ExprId, ty: IRTypeId) -> IRExpr {
    l := fb.l
    oid, has := l.c.expr_objects[id]
    if !has do gala_panic("symbol was never resolved")

    if loc, ok := fb.local_of[oid]; ok do return IRExpr{ty, IRLocalRef{loc}}

    o := get_obj(oid)
    if o.kind == .Argument {
        // params are locals 0..n-1; match by name (no shadowing, so names are unique)
        for n, i in fb.arg_names {
            if n == o.name do return IRExpr{ty, IRLocalRef{IRLocalId(i)}}
        }
        gala_panicf("argument \"%s\" isn't a parameter of this function", o.name)
    }

    item, ok := l.obj_items[oid]
    if !ok do gala_panicf("symbol \"%s\" is neither a local nor an item", o.name)

    if fid, ok := l.fn_cache[item]; ok do return IRExpr{ty, IRFnRef{fid}}
    if gid, ok := l.global_cache[item]; ok do return IRExpr{ty, IRGlobalRef{gid}}
    gala_panicf("item \"%s\" was never declared (is it a generic template used without a call?)", o.name)
}

lower_binop :: proc(fb: ^FnBuilder, b: Binop, ty: IRTypeId) -> IRExpr {
    l := fb.l
    operand_ty := ir_expr_ty(l, b.left)
    lhs := lower_expr(fb, b.left)

    #partial switch b.kind {
    case .LogicalAnd, .LogicalOr:
        rhs := lower_expr_scoped(fb, b.right)
        return IRExpr{ty, IRLogical{
            is_and = b.kind == .LogicalAnd, lhs = new_expr_from(lhs), rhs = new_expr_from(rhs),
        }}
    case .Addition, .Subtraction:
        if is_pointer(operand_ty) {
            pointee, _ := deref_type(operand_ty)
            return IRExpr{ty, IRPtrOffset{
                ptr = new_expr_from(lhs), index = new_expr_from(lower_expr(fb, b.right)),
                elem = lower_type(l, pointee), subtract = b.kind == .Subtraction,
            }}
        }
    }
    return make_binop(fb, b.kind, lhs, lower_expr(fb, b.right), operand_ty)
}

// picks the exact IR op from the *operand* type
make_binop :: proc(fb: ^FnBuilder, k: BinopKind, lhs, rhs: IRExpr, operand: TypeId) -> IRExpr {
    l := fb.l
    f   := is_float(operand) || get_type(operand).kind == .UntypedFloat
    sgn := is_integer_signed(operand) || get_type(operand).kind == .UntypedInteger
    op: IRBinOp
    cmp := false

    switch k {
    case .Addition:     op = .FAdd if f else .Add
    case .Subtraction:  op = .FSub if f else .Sub
    case .Multiply:     op = .FMul if f else .Mul
    case .Divide:       op = .FDiv if f else (.SDiv if sgn else .UDiv)
    case .Modulo:       op = .SRem if sgn else .URem
    case .BitAnd:       op = .And
    case .BitOr:        op = .Or
    case .BitXor:       op = .Xor
    case .ShiftLeft:    op = .Shl
    case .ShiftRight:   op = .AShr if sgn else .LShr
    case .Equal:        op = .FEq if f else .Eq;  cmp = true
    case .NotEqual:     op = .FNe if f else .Ne;  cmp = true
    case .Less:         op = .FLt if f else (.SLt if sgn else .ULt); cmp = true
    case .LessEqual:    op = .FLe if f else (.SLe if sgn else .ULe); cmp = true
    case .Greater:      op = .FGt if f else (.SGt if sgn else .UGt); cmp = true
    case .GreaterEqual: op = .FGe if f else (.SGe if sgn else .UGe); cmp = true
    case .LogicalAnd, .LogicalOr: gala_panic("logical ops go through IRLogical")
    }
    res_ty := l.t_bool if cmp else lhs.type
    return IRExpr{res_ty, IRBinary{op, new_expr_from(lhs), new_expr_from(rhs)}}
}

lower_field_access :: proc(fb: ^FnBuilder, e: FieldAccess, ty: IRTypeId) -> IRExpr {
    l := fb.l
    base_gt := ir_expr_ty(l, e.target)
    base := lower_expr(fb, e.target)

    // auto-deref through a pointer
    if is_pointer(base_gt) {
        pointee, _ := deref_type(base_gt)
        base = IRExpr{lower_type(l, pointee), IRDeref{new_expr_from(base)}}
        base_gt = pointee
    }
    
    if get_type(base_gt).kind == .Union {
        found := false
        for f in get_type(base_gt).union_fields {
            if f.name == e.field { found = true; break }
        }
        if !found do gala_panicf("no field \"%s\"", e.field)
        return union_field_ref(fb, base, ty)
    }

    fields, ok := struct_fields(base_gt)
    if !ok do gala_panicf("type {} has no fields", tts(base_gt))
    for f, i in fields {
        if f.name == e.field do return IRExpr{ty, IRField{new_expr_from(base), u32(i)}}
    }
    gala_panicf("no field \"%s\"", e.field)
}

// all union fields live at offset 0, so a field is the blob's address read as ^T.
// rvalue unions (call results, literals) are spilled so they have an address
union_field_ref :: proc(fb: ^FnBuilder, base: IRExpr, ty: IRTypeId) -> IRExpr {
    place := base
    if !ir_is_place(place) {
        tmp := new_local(fb, "$un", base.type)
        emit(fb, IRLet{local = tmp, init = base})
        place = IRExpr{base.type, IRLocalRef{tmp}}
    }
    addr := IRExpr{fb.l.t_ptr, IRAddrOf{new_expr_from(place)}}
    return IRExpr{ty, IRDeref{new_expr_from(addr)}}
}

// pointer to element `idx` of a slice/string value (evaluated once, via temps by the caller)
slice_elem_addr :: proc(l: ^Lowerer, base, idx: IRExpr, elem: IRTypeId) -> IRExpr {
    data := IRExpr{l.t_ptr, IRField{new_expr_from(base), 0}}
    return IRExpr{l.t_ptr, IRPtrOffset{
        ptr = new_expr_from(data), index = new_expr_from(idx), elem = elem,
    }}
}

lower_index :: proc(fb: ^FnBuilder, e: Index, ty: IRTypeId) -> IRExpr {
    l := fb.l
    base_gt := ir_expr_ty(l, e.target)
    base := lower_expr(fb, e.target)
    idx  := lower_expr(fb, e.index)

    // TODO: optional bounds check (IRIf + panic call)
    #partial switch get_type(base_gt).kind {
    case .FixedSizeArray:
        return IRExpr{ty, IRIndex{new_expr_from(base), new_expr_from(idx)}}
    case .Slice, .String:
        addr := slice_elem_addr(l, base, idx, ty)
        return IRExpr{ty, IRDeref{new_expr_from(addr)}}
    }
    gala_panic("lower_index: not indexable")
}

// can we take &expr at the IR level?
ir_is_place :: proc(e: IRExpr) -> bool {
    #partial switch k in e.kind {
    case IRLocalRef, IRGlobalRef, IRDeref: return true
    case IRField: return ir_is_place(k.base^)
    case IRIndex: return ir_is_place(k.base^)
    }
    return false
}

// t[a:b] => { data + a*sz, b - a }. base/bounds go in temps: each is used twice
lower_take_slice :: proc(fb: ^FnBuilder, e: TakeSlice, ty: IRTypeId) -> IRExpr {
    l := fb.l
    base_gt := ir_expr_ty(l, e.target)
    kind := get_type(base_gt).kind
    if kind != .Slice && kind != .String && kind != .FixedSizeArray {
        gala_panic("lower_take_slice: not sliceable", kind)
    }

    elem_gt: TypeId
    if kind == .String { elem_gt = ty_from_name("byte") } else { elem_gt, _ = index_elem_type(base_gt) }
    elem := lower_type(l, elem_gt)

    if fb.is_global {
        if kind != .FixedSizeArray {
            gala_panic("a global initialiser can only slice an array")
        }
        size := u64(get_type(base_gt).fixed_size_array.size)
        base := lower_expr(fb, e.target)

        place := base
        if !ir_is_place(place) {
            // array literal: give it its own static storage
            append(&l.prog.globals, IRGlobal{
                name    = "$backing",
                symbol  = unique_symbol(l, fmt.aprintf("_Gbacking%d", len(l.prog.globals))),
                mod     = fb.mod,
                type    = base.type,
                linkage = .Internal,
                init    = base,
            })
            place = IRExpr{base.type, IRGlobalRef{IRGlobalId(len(l.prog.globals) - 1)}}
        }

        ptr := IRExpr{l.t_ptr, IRAddrOf{new_expr_from(place)}}
        length := int_lit(l.t_i64, size)
        if !e.empty_end do length = lower_expr(fb, e.end)
        if !e.empty_start {
            start := lower_expr(fb, e.start)
            ptr = IRExpr{l.t_ptr, IRPtrOffset{
                ptr = new_expr_from(ptr), index = new_expr_from(start), elem = elem,
            }}
            length = IRExpr{l.t_i64, IRBinary{.Sub, new_expr_from(length), new_expr_from(start)}}
        }
        elems := make([]IRExpr, 2)
        elems[0], elems[1] = ptr, length
        return IRExpr{ty, IRAggregate{elems}}
    }

    base := lower_expr(fb, e.target)
    data, length: IRExpr

    if kind == .FixedSizeArray {
        place := base
        if !ir_is_place(place) {
            // rvalue array (literal, call result): spill so it has an address
            tmp := new_local(fb, "$arr", base.type)
            emit(fb, IRLet{local = tmp, init = base})
            place = IRExpr{base.type, IRLocalRef{tmp}}
        }
        data   = IRExpr{l.t_ptr, IRAddrOf{new_expr_from(place)}}
        length = int_lit(l.t_i64, u64(get_type(base_gt).fixed_size_array.size))
    } else {
        btmp := new_local(fb, "$sl", base.type)
        emit(fb, IRLet{local = btmp, init = base})
        base_e := IRExpr{base.type, IRLocalRef{btmp}}
        data   = IRExpr{l.t_ptr, IRField{new_expr_from(base_e), 0}}
        length = IRExpr{l.t_i64, IRField{new_expr_from(base_e), 1}}
    }

    bound :: proc(fb: ^FnBuilder, name: string, empty: bool, ex: ExprId, dflt: IRExpr) -> IRExpr {
        v := dflt if empty else lower_expr(fb, ex)
        t := new_local(fb, name, fb.l.t_i64)
        emit(fb, IRLet{local = t, init = v})
        return IRExpr{fb.l.t_i64, IRLocalRef{t}}
    }
    start := bound(fb, "$lo", e.empty_start, e.start, int_lit(l.t_i64, 0))
    end   := bound(fb, "$hi", e.empty_end,   e.end,   length)

    ptr := IRExpr{l.t_ptr, IRPtrOffset{
        ptr = new_expr_from(data), index = new_expr_from(start), elem = elem,
    }}
    n := IRExpr{l.t_i64, IRBinary{.Sub, new_expr_from(end), new_expr_from(start)}}
    elems := make([]IRExpr, 2)
    elems[0], elems[1] = ptr, n
    return IRExpr{ty, IRAggregate{elems}}
}

// U{ b = 1.0 } => { let $un = zeroinit; *(T*)&$un = value; yield $un }
lower_union_lit :: proc(fb: ^FnBuilder, e: AggregateLit, gt: TypeId, ty: IRTypeId) -> IRExpr {
    l := fb.l
    fields := get_type(gt).union_fields

    pick := -1
    init_id: ExprId
    if len(e.positional) > 0 {
        if len(e.positional) > 1 || len(fields) == 0 {
            gala_panic("union literal can initialise only one field")
        }
        pick, init_id = 0, e.positional[0].expr
    } else {
        for f, i in fields {
            if fi, ok := e.fields[f.name]; ok {
                if pick != -1 do gala_panic("union literal can initialise only one field")
                pick, init_id = i, fi.expr
            }
        }
    }
    if pick == -1 do return IRExpr{ty, IRZero{}} // U{}

    value := lower_expr_to(fb, init_id, fields[pick].type)
    tmp   := new_local(fb, "$un", ty)
    place := IRExpr{ty, IRLocalRef{tmp}}
    stmts := make([]IRStmt, 2)
    stmts[0] = IRLet{local = tmp, init = IRExpr{ty, IRZero{}}}
    stmts[1] = IRAssign{
        target = new_expr_from(union_field_ref(fb, place, lower_type(l, fields[pick].type))),
        value  = value,
    }
    return IRExpr{ty, IRSeq{stmts, new_expr_from(place)}}
}

lower_aggregate :: proc(fb: ^FnBuilder, id: ExprId, e: AggregateLit, gt: TypeId, ty: IRTypeId) -> IRExpr {
    l := fb.l
    st := get_type(gt)
    if st.kind == .Union do return lower_union_lit(fb, e, gt, ty)
    if st.kind != .Struct do gala_panic("aggregate literal of a non-struct type")

    elems := make([]IRExpr, len(st.structure.fields))
    for f, i in st.structure.fields {
        fty := lower_type(l, f.type)
        if len(e.positional) > 0 {
            if i < len(e.positional) {
                elems[i] = lower_expr_to(fb, e.positional[i].expr, f.type)
            } else {
                elems[i] = IRExpr{fty, IRZero{}}
            }
        } else if init, ok := e.fields[f.name]; ok {
            elems[i] = lower_expr_to(fb, init.expr, f.type)
        } else {
            elems[i] = IRExpr{fty, IRZero{}} // TODO: field defaults
        }
    }
    return IRExpr{ty, IRAggregate{elems}}
}

lower_array_lit :: proc(fb: ^FnBuilder, e: FixedSizeArray, gt: TypeId, ty: IRTypeId) -> IRExpr {
    l := fb.l
    at := get_type(gt).fixed_size_array
    if len(e.initialiser) == 0 do return IRExpr{ty, IRZero{}}

    elem_ty := lower_type(l, at.type)
    elems := make([]IRExpr, at.size)
    for i in 0..<at.size {
        if i < len(e.initialiser) {
            elems[i] = lower_expr_to(fb, e.initialiser[i], at.type)
        } else {
            elems[i] = IRExpr{elem_ty, IRZero{}}
        }
    }
    return IRExpr{ty, IRAggregate{elems}}
}

// lambdas are lifted to their own IRFunction (no captures yet)
lower_fn_lit :: proc(fb: ^FnBuilder, e: FnLit, gt: TypeId, ty: IRTypeId) -> IRExpr {
    l := fb.l
    name := fmt.aprintf("lambda%d", l.n_lambdas)
    l.n_lambdas += 1

    fn := IRFunction{
        name = name, symbol = mangle(l, fb.mod, name), mod = fb.mod,
        type = lower_fn_type(l, gt, .Gala), call_conv = .Gala, linkage = .Internal,
    }
    add_param_locals(l, &fn, gt, .Gala)
    append(&l.prog.fns, fn)
    fid := IRFnId(len(l.prog.fns) - 1)
    append(&l.lambdas, PendingLambda{id = fid, block = e.block, fty = gt, mod = fb.mod})
    return IRExpr{ty, IRFnRef{fid}}
}

lower_cast :: proc(fb: ^FnBuilder, target: ExprId, to_gt: TypeId, to: IRTypeId) -> IRExpr {
    l := fb.l
    from_gt := ir_expr_ty(l, target)
    operand := lower_expr(fb, target)

    from_f, to_f := is_float(from_gt), is_float(to_gt)
    from_i := is_integer(from_gt) || is_byte_like(from_gt)
    to_i   := is_integer(to_gt)   || is_byte_like(to_gt)
    kind: IRConvKind

    switch {
    case from_i && to_i:
        fs, ts := type_size(from_gt), type_size(to_gt)
        if fs == ts do return IRExpr{to, operand.kind} // same width: retag
        if fs > ts  do kind = .Trunc
        else        do kind = .SExt if is_integer_signed(from_gt) else .ZExt
    case from_i && to_f:
        kind = .SIToFP if is_integer_signed(from_gt) else .UIToFP
    case from_f && to_i:
        kind = .FPToSI if is_integer_signed(to_gt) else .FPToUI
    case from_f && to_f:
        kind = .FPExt if type_size(to_gt) > type_size(from_gt) else .FPTrunc
    case is_pointer(from_gt) && is_pointer(to_gt):
        return IRExpr{to, operand.kind} // opaque pointers
    case is_pointer(from_gt) && to_i:
        kind = .PtrToInt
    case from_i && is_pointer(to_gt):
        kind = .IntToPtr
    case from_i && is_bool(to_gt):
        kind = .IntToBool
    case is_bool(from_gt) && to_i:
        kind = .ZExt
    case:
        debugln(tts(expr_ty(target)), tts(to_gt))
        gala_panic("lower_cast: unsupported (checker should have rejected)")
    }
    return IRExpr{to, IRConvert{kind, new_expr_from(operand)}}
}


// Gala variadics: `..T` arrives as a []T (effective_variadic_ty), so the extra
// args go into a stack array and the callee gets { ptr, n }. The array is a
// local of the caller, so the slice is only valid for the duration of the call.
// It's built inside an IRSeq so it's evaluated at the arg's position.
lower_variadic_pack :: proc(fb: ^FnBuilder, eff: TypeId, ids: []ExprId) -> IRExpr {
    l := fb.l
    et := get_type(eff)
    if et.kind != .Slice do gala_panic("effective variadic type of a gala fn must be a slice", et.kind)
    slice_ty := lower_type(l, eff)

    elems := make([]IRExpr, 2)
    if len(ids) == 0 {
        elems[0] = IRExpr{l.t_ptr, IRNullPtr{}}
        elems[1] = int_lit(l.t_i64, 0)
        return IRExpr{slice_ty, IRAggregate{elems}}
    }

    elem_gt := et.slice.type // the variadic's element type (may be any)
    elem_ty := lower_type(l, elem_gt)
    items := make([]IRExpr, len(ids))
    for id, i in ids do items[i] = lower_expr_to(fb, id, elem_gt)

    arr_ty  := add_ir_type(l, IRArray{elem = elem_ty, count = u64(len(ids))})
    backing := new_local(fb, "$va", arr_ty)
    stmts := make([]IRStmt, 1)
    stmts[0] = IRLet{local = backing, init = IRExpr{arr_ty, IRAggregate{items}}}

    place := IRExpr{arr_ty, IRLocalRef{backing}}
    elems[0] = IRExpr{l.t_ptr, IRAddrOf{new_expr_from(place)}}
    elems[1] = int_lit(l.t_i64, u64(len(ids)))
    value := IRExpr{slice_ty, IRAggregate{elems}}
    return IRExpr{slice_ty, IRSeq{stmts, new_expr_from(value)}}
}

// C variadics: extra args are passed as they are, after C's default argument
// promotions (float -> double, small ints and bool -> int)
lower_c_vararg :: proc(fb: ^FnBuilder, id: ExprId) -> IRExpr {
    l := fb.l
    gt := ir_expr_ty(l, id)
    e := lower_expr(fb, id)

    #partial switch get_type(gt).kind {
    case .Flt32:
        return IRExpr{l.t_f64, IRConvert{.FPExt, new_expr_from(e)}}
    case .Int_8, .Int16:
        return IRExpr{l.t_i32, IRConvert{.SExt, new_expr_from(e)}}
    case .UInt_8, .UInt16, .Byte, .Rune, .Bool:
        return IRExpr{l.t_i32, IRConvert{.ZExt, new_expr_from(e)}}
    }
    return e
}
