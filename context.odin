package main

import "core:mem"
import "core:mem/virtual"
import "core:fmt"
import "core:os"
import "core:strings"


ExprId  :: distinct u64
StmtId  :: distinct u64
ItemId  :: distinct u64
TypeId  :: distinct u64
ObjId   :: distinct u64
ModId   :: distinct u64

// where a generic function was declared; its clones are resolved,
// type-checked and emitted there
GenericOwner :: struct {
    ast:  ^AST,
    mod:  ModId,
    file: string,
}

// a module that has been parsed, resolved and type-checked
ModuleUnit :: struct {
    ast:  ^AST,
    mod:  ModId,
    file: string,
}

Context :: struct {
    program_name:       string,
    arena:              virtual.Arena,
    allocator:          mem.Allocator,
    debug:              bool,
    current_file:       string,
    exprs:              [dynamic]Expr,
    stmts:              [dynamic]Stmt,
    items:              [dynamic]Item,
    mods:               [dynamic]Module,
    types:              [dynamic]Type,
    objs :              [dynamic]Object,
    // strings
    table: map[string]StringId,   // decoded content -> id
    data:  [dynamic]string,       // id -> decoded content
    // refs
    expr_types:         map[ExprId]TypeId,
    expr_objects:       map[ExprId]ObjId,

    item_types:         map[ItemId]TypeId,
    item_objects:       map[ItemId]ObjId,
    item_module:        map[ItemId]ModId, // import module ids

    stmt_objects:       map[StmtId]ObjId,
    stmt_types:         map[StmtId]TypeId,
    break_lables:       map[StmtId]StmtId, // includes continue

    obj_modules:        map[ObjId]ModId,
    ty_modules:         map[TypeId]ModId,
    // generics
    // functions shouldn't be interned, so one type per function
    // generics: call expr -> the instantiation it uses
    expr_fn_version:        map[ExprId]FnVersionRef,
    fn_generic_variations:  map[TypeId]struct{},

    // every generic fn item of every module, who owns it, and how many of
    // its versions have been instantiated so far
    generic_items:          [dynamic]ItemId,
    generic_owner:          map[ItemId]GenericOwner,
    generic_done:           map[ItemId]int,
    // type-checked modules, imports first
    units:                  [dynamic]ModuleUnit,

    // for other stuff that need to know things only available
    // at resolution phase
    // expr_cast_types:    map[ExprId]TypeId,
    // expr_struct_types:  map[ExprId]TypeId,
    expr_resolution_types:  map[ExprId]TypeId,

    base_mod:           ModuleScope,


    spans: struct {
        exprs: map[ExprId]SpanStruct,
        stmts: map[StmtId]SpanStruct,
        items: map[ItemId]SpanStruct,
        // includes item decs + regular vardecs since both use ObjId
        objs_decs: map[ObjId]SpanStruct,
        ty_decs: map[TypeId]SpanStruct,
    },
    files:                                  map[string]string,
    parsing:                                map[string]string,
    o_files:                                [dynamic]string,
    modules:                                map[string]ModId,

    llvm_ty:                                map[TypeId]string,
    cg_item_names:                          map[ItemId]string,
    cg_ty_names:                            map[TypeId]string,
    cg_module_prefix:                       map[ModId]string,
    links:                                  [dynamic]string,
    current_module_id:                      ModId,
    entry_file:                             string,
    object_only:                            bool, // -c: no C `main`, export gala_init_globals instead
    uses_type_table:                        bool,
    abi:                                    AbiKind,
}

FileLine :: struct {
    line_number: int,
    line:        string,
    start:       int, // byte offset of this line's first char in the source
    end:         int, // byte offset one past this line's last char (exclusive, no \n)
}

get_file_lines :: proc(file_name: string, span: Span) -> []FileLine {
    allocator := get_ctx().allocator;
    src, ok := get_ctx().files[file_name]
    if !ok {
        panic(fmt.tprintf("get_file_lines: unknown file %q", file_name))
    }
    if span.start < 0 || span.end > len(src) || span.start > span.end {
        panic(fmt.tprintf("get_file_lines: invalid span %v for file %q (len %d)", span, file_name, len(src)))
    }

    lines := make([dynamic]FileLine, allocator)
    line_no := 1
    line_start := 0

    for i := 0; i <= len(src); i += 1 {
        at_end := i == len(src)
        is_newline := !at_end && src[i] == '\n'

        if is_newline || at_end {
            line_end := i
            byte_range_hits_span := line_start <= span.end && line_end >= span.start
            if byte_range_hits_span {
                append(&lines, FileLine{line_no, src[line_start:line_end], line_start, line_end})
            }
            if at_end do break
            line_no += 1
            line_start = i + 1
        }
    }

    return lines[:]
}

highlight_lines_file_name :: proc(file_name:string, span:Span) {
    f := file_name
    fmt.printfln("in \"%s\":", f);
    lines := get_file_lines(f, span);
    print_lines(lines, span);
}
highlight_lines_span :: proc(span:Span) {
    f := get_ctx().current_file
    fmt.printfln("in \"%s\":", f);
    lines := get_file_lines(f, span);
    print_lines(lines, span);
}
highlight_lines_struct :: proc(span:SpanStruct) {
    f := span.file_name
    fmt.printfln("in \"%s\":", span.file_name);
    lines := get_file_lines(f, span.span);
    print_lines(lines, span.span);
}
highlight_lines :: proc {
    highlight_lines_file_name,
    highlight_lines_struct,
}
print_lines :: proc(lines: []FileLine, highlight: Span = {0, 0}) {
    has_highlight := highlight.start != highlight.end

    for l in lines {
        fmt.printfln(" %5d | %s", l.line_number, l.line)
        if !has_highlight do continue

        // does the highlight span touch this line at all?
        overlaps := highlight.start < l.end && highlight.end > l.start
        if !overlaps do continue

        // clip the span to this line's bounds, then convert to column offsets
        col_start := max(highlight.start, l.start) - l.start
        col_end   := min(highlight.end, l.end) - l.start
        if col_end <= col_start {
            col_end = col_start + 1 // guarantee at least one caret
        }

        gutter := "       | " // 5 digits + " | " prefix width, matches "%5d | "
        builder := strings.builder_make(get_ctx().allocator)
        strings.write_string(&builder, gutter)
        for _ in 0..<col_start {
            strings.write_rune(&builder, ' ')
        }
        for _ in 0..<(col_end - col_start) {
            strings.write_rune(&builder, '^')
        }
        fmt.println(strings.to_string(builder))
    }
}
new_stmt :: proc(stmt:=Stmt{}) -> StmtId {
    get_ctx := cast(^Context)context.user_ptr
    append(&get_ctx.stmts, stmt);
    return StmtId(len(get_ctx.stmts)-1)
}
new_expr :: proc(expr:=Expr{}) -> ExprId {
    get_ctx := cast(^Context)context.user_ptr
    append(&get_ctx.exprs, expr);
    return ExprId(len(get_ctx.exprs)-1)
}
new_item :: proc(item:=Item{}) -> ItemId {
    get_ctx := cast(^Context)context.user_ptr
    append(&get_ctx.items, item);
    return ItemId(len(get_ctx.items)-1)
}
get :: proc {
    get_expr,
    get_stmt,
    get_item,
    get_obj,
    get_type,
}
get_ctx :: proc() -> ^Context {
    return cast(^Context)context.user_ptr;
}
get_expr :: proc(id: ExprId) -> ^Expr {
    get_ctx := cast(^Context)context.user_ptr
    assert(int(id) < len(get_ctx.exprs));
    return &get_ctx.exprs[id]
}
get_stmt :: proc(id: StmtId) -> ^Stmt {
    get_ctx := cast(^Context)context.user_ptr
    assert(int(id) < len(get_ctx.stmts));
    return &get_ctx.stmts[id]
}
get_item :: proc(id: ItemId) -> ^Item {
    get_ctx := cast(^Context)context.user_ptr
    assert(int(id) < len(get_ctx.items));
    return &get_ctx.items[id]
}
get_type :: proc(id: TypeId) -> ^Type {
    get_ctx := cast(^Context)context.user_ptr
    assert(int(id) < len(get_ctx.types));
    return &get_ctx.types[id]
}
get_obj :: proc(id: ObjId) -> ^Object {
    get_ctx := cast(^Context)context.user_ptr
    assert(int(id) < len(get_ctx.objs));
    return &get_ctx.objs[id]
}
debug :: proc(args: ..any) {
    if !get_ctx().debug do return
    fmt.print(..args)
}

debugln :: proc(args: ..any) {
    if !get_ctx().debug do return
    fmt.println(..args)
}

debugf :: proc(format: string, args: ..any) {
    if !get_ctx().debug do return
    fmt.printf(format, ..args)
}

debugfln :: proc(format: string, args: ..any) {
    if !get_ctx().debug do return
    fmt.printfln(format, ..args)
}
get_span :: proc {
    get_span_expr,
    get_span_stmt,
    get_span_item,
    get_span_obj,
    get_span_ty,
}
SpanStruct :: struct{file_name: string, span: Span}
get_span_expr :: proc(id: ExprId) -> SpanStruct {
    get_ctx := get_ctx()
    s, ok := get_ctx.spans.exprs[id]
    if !ok {
        panic(fmt.tprintf("get_span_expr: no span recorded for %v", id))
    }
    return s
}

get_span_stmt :: proc(id: StmtId) -> SpanStruct {
    get_ctx := get_ctx()
    s, ok := get_ctx.spans.stmts[id]
    if !ok {
        panic(fmt.tprintf("get_span_stmt: no span recorded for %v", id))
    }
    return s
}

get_span_item :: proc(id: ItemId) -> SpanStruct {
    get_ctx := get_ctx()
    s, ok := get_ctx.spans.items[id]
    if !ok {
        panic(fmt.tprintf("get_span_item: no span recorded for %v", id))
    }
    return s
}

get_span_obj :: proc(id: ObjId) -> SpanStruct {
    get_ctx := get_ctx()
    s, ok := get_ctx.spans.objs_decs[id]
    if !ok {
        fmt.println(get_obj(id)^);
        panic(fmt.tprintf("get_span_obj: no span recorded for %v", id))
    }
    return s
}
get_span_ty :: proc(id: TypeId) -> SpanStruct {
    get_ctx := get_ctx()
    s, ok := get_ctx.spans.ty_decs[id]
    if !ok {
        fmt.println(get_type(id)^);
        panic(fmt.tprintf("get_span_ty: no span recorded for %v", id))
    }
    return s
}
gala_panic :: proc(args: ..any) -> ! {
    fmt.println(..args)
    // panic("");
    os.exit(1);
}
gala_panicf :: proc(f: string, args: ..any) -> ! {
    fmt.printfln(f, ..args)
    // panic("");
    os.exit(1);
}

gala_info :: proc(args: ..any) {
    fmt.println(..args)
}
gala_infof :: proc(f: string, args: ..any) {
    fmt.printfln(f, ..args)
}
