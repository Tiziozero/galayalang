package main

import "core:c"
import "core:fmt"
import "core:path/filepath"
import "core:strings"
import "core:mem"
import "core:os"
import "core:io"
import "core:time"
VERSION :: 0.01;

current_file := ""
current_module_id := ModId(0);
// Resolves `import_path` (as written in source) to a canonical path
// relative to the process's working directory.
//
//   "./x.gala", "../x.gala"  -> relative to the importing file's directory
//   "x.gala", "lib/x.gala"   -> working directory, then the directory the
//                               compiler executable lives in (stdlib)
//   "/abs/x.gala"            -> used as is
//
// `importer_path` may be "" (top-level file from the command line), in
// which case only the working directory is considered.
resolve_import_path :: proc(importer_path: string, import_path: string) -> string {
    joined: string

    is_relative := strings.has_prefix(import_path, "./") ||
                   strings.has_prefix(import_path, "../")

    if filepath.is_abs(import_path) {
        joined = import_path
    } else if is_relative && len(importer_path) > 0 {
        importer_dir := filepath.dir(importer_path)
        joined, _ = filepath.join({importer_dir, import_path})
    } else {
        // plain path: working directory first
        joined = import_path

        // not the entry file, and not found in cwd: try next to the compiler
        if len(importer_path) > 0 && !os.exists(import_path) {
            exe_dir, eerr := os.get_executable_directory(context.allocator)
            if eerr == nil {
                candidate, _ := filepath.join({exe_dir, import_path})
                if os.exists(candidate) {
                    joined = candidate
                }
            }
        }
    }


    abs_path, abs_ok := filepath.abs(joined)
    if abs_ok != nil {
        abs_path = joined
    }

    cleaned, cerr := filepath.clean(abs_path)
    assert(cerr == .None)

    cwd, _ := os.get_working_directory(context.allocator)

    rel, rerr := filepath.rel(cwd, cleaned)
    if rerr != filepath.Relative_Error.None {
        // Can't be made relative (e.g. different drive on Windows)
        return cleaned
    }

    return rel
}

handle_file :: proc(from, file_name: string) -> string {
    resolved := resolve_import_path(from, file_name)
    // debugln("handling:", resolved)

    if _, ok := get_ctx().parsing[resolved]; ok {
        gala_panicf("Cyclical imports with file: \"%s\".", resolved)
    }
    if _, ok := get_ctx().modules[resolved]; ok {
        // Already fully parsed/resolved (e.g. two files import the same
        // dependency) — nothing more to do.
        return resolved
    }

    data, err := os.read_entire_file(resolved, context.allocator)
    if err != io.Error.None {
        gala_panic("Failed to read file:", resolved)
    }

    get_ctx().files[resolved] = string(data)
    get_ctx().parsing[resolved] = string(data)
    p := current_file;
    current_file = resolved;

    tokens := lex_file(data)
    ast := new(AST)
    ast^ = parse_tokens(resolved, tokens[:])
    mid := resolve_module_ast(ast, resolved)
    typecheck_module(ast, mid, resolved)
    // append(&get_ctx().units, ModuleUnit{ast = ast, mod = mid, file = resolved})
    get_ctx().mods[mid].ast = ast   // the module holds a copy; sync it

    // get_ctx().modules[resolved] = mid
    delete_key(&get_ctx().parsing, resolved)
    current_file = p;
    return resolved
}


// ---------------------------------------------------------------------
// CLI
// ---------------------------------------------------------------------

print_usage :: proc() {
    gala_infof("usage: galac {} <file.gala> [-c] [-S -emit-llvm | -emit-llvm] [-o <name>] [--dev|--release] [--abi sysv|win64] [-l <library>]...", VERSION)
}

main :: proc() {
    general_arena := mem.Dynamic_Arena{};
    mem.dynamic_arena_init(&general_arena, );
    defer mem.dynamic_arena_destroy(&general_arena)
    context.allocator = mem.dynamic_arena_allocator(&general_arena);
    ctx := Context {};
    ctx.debug = true;
    context.user_ptr = &ctx;


    ctx.base_mod = new_module_scope()

    // integer types
    new_type(&ctx.base_mod, Type{name="i8", kind=.Int_8}, {});
    new_type(&ctx.base_mod, Type{name="i16", kind=.Int16}, {});
    new_type(&ctx.base_mod, Type{name="i32", kind=.Int32}, {});
    new_type(&ctx.base_mod, Type{name="i64", kind=.Int64}, {});

    new_type(&ctx.base_mod, Type{name="u8", kind=.UInt_8 }, {});
    new_type(&ctx.base_mod, Type{name="u16", kind=.UInt16}, {});
    new_type(&ctx.base_mod, Type{name="u32", kind=.UInt32}, {});
    new_type(&ctx.base_mod, Type{name="u64", kind=.UInt64}, {});

    new_type(&ctx.base_mod, Type{name="f8", kind=.Flt_8}, {});
    new_type(&ctx.base_mod, Type{name="f16", kind=.Flt16}, {});
    new_type(&ctx.base_mod, Type{name="f32", kind=.Flt32}, {});
    new_type(&ctx.base_mod, Type{name="f64", kind=.Flt64}, {});
    new_type(&ctx.base_mod, Type{name="void", kind=.Void}, {});
    new_type(&ctx.base_mod, Type{name="any", kind=.Any}, {});

    new_type(&ctx.base_mod, Type{name="bool", kind=.Bool}, {});
    new_type(&ctx.base_mod, Type{name="byte", kind=.Byte}, {});
    void_ty, _ := scope_find_type(&ctx.base_mod, "void");
    new_type(&ctx.base_mod, Type{name="rawptr", kind=.Pointer, ptr=void_ty}, {});
    new_type(&ctx.base_mod, Type{name="string", kind=.String}, {});

    get_ctx().entry_file = resolve_import_path("", "main.gala");
    handle_file("", get_ctx().entry_file);


    if get_ctx().debug do print_loc()

    /* debugln("modules:       ", len(get_ctx().mods));
    debugln("items:         ", len(get_ctx().items));
    debugln("stmts:         ", len(get_ctx().stmts));
    debugln("exprs:         ", len(get_ctx().exprs));
    debugln("objects:       ", len(get_ctx().objs));
    debugln("types:         ", len(get_ctx().types));
    dump_type_stats :: proc() {
        counts: map[TypeKind]int
        defer delete(counts)
        for t in get_ctx().types do counts[t.kind] += 1
        for k, n in counts do fmt.println(k, n)
    }
    if get_ctx().debug  do dump_type_stats() */
    debugln("Finished compiling");
}
