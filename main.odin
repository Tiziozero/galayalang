package main

import "core:c"
import "core:fmt"
import "core:path/filepath"
import "core:strings"
import "core:mem/virtual"
import "core:mem"
import "core:os"
import "core:io"
import "core:time"
VERSION :: 0.01;
phase :: proc(name: string, t: ^time.Tick) {
    debugfln("%-12s %v\n", name, time.tick_since(t^))
    t^ = time.tick_now()
}

init_context :: proc(debug := false) -> ^Context {
    ctx := new(Context)
    ctx.program_name = "main"; // overwrite eventually
    // NOTE: no context.user_ptr assignment here — it wouldn't survive return

    aerr := virtual.arena_init_growing(&ctx.arena)
    assert(aerr == virtual.Allocator_Error.None)
    ctx.allocator = virtual.arena_allocator(&ctx.arena)

    al := ctx.allocator
    ctx.debug = debug
    ctx.items = make([dynamic]Item, allocator = al)
    ctx.exprs = make([dynamic]Expr, allocator = al)
    ctx.stmts = make([dynamic]Stmt, allocator = al)
    ctx.objs  = make([dynamic]Object, allocator = al)
    ctx.types = make([dynamic]Type, allocator = al)

    ctx.data  = make([dynamic]string, allocator = al)
    ctx.table = make(map[string]StringId, allocator = al)

    ctx.obj_modules   = make(map[ObjId]ModId, allocator = al)
    ctx.expr_types   = make(map[ExprId]TypeId, allocator = al)
    ctx.expr_objects = make(map[ExprId]ObjId, allocator = al)
    ctx.item_types   = make(map[ItemId]TypeId, allocator = al)
    ctx.item_objects = make(map[ItemId]ObjId, allocator = al)
    ctx.item_module = make(map[ItemId]ModId, allocator = al)
    ctx.stmt_objects = make(map[StmtId]ObjId, allocator = al)
    ctx.stmt_types   = make(map[StmtId]TypeId, allocator = al)
    ctx.expr_resolution_types = make(map[ExprId]TypeId, allocator = al)

    ctx.generic_items = make([dynamic]ItemId, allocator = al)
    ctx.generic_owner = make(map[ItemId]GenericOwner, allocator = al)
    ctx.generic_done  = make(map[ItemId]int, allocator = al)
    ctx.units         = make([dynamic]ModuleUnit, allocator = al)

    ctx.spans.exprs = make(map[ExprId]SpanStruct, allocator = al)
    ctx.spans.items = make(map[ItemId]SpanStruct, allocator = al)
    ctx.spans.stmts = make(map[StmtId]SpanStruct, allocator = al)
    ctx.spans.objs_decs = make(map[ObjId]SpanStruct, allocator = al)
    ctx.spans.ty_decs = make(map[TypeId]SpanStruct, allocator = al)

    ctx.files = make(map[string]string, allocator = al)
    ctx.parsing = make(map[string]string, allocator = al)
    ctx.o_files = make([dynamic]string, allocator = al)

    ctx.base_mod = new_module_scope(allocator=ctx.allocator)
    ctx.llvm_ty = make(map[TypeId]string, allocator=ctx.allocator);
    ctx.cg_item_names = make(map[ItemId]string, allocator=ctx.allocator);
    ctx.cg_module_prefix =  make(map[ModId]string, allocator=ctx.allocator);
    ctx.links = make([dynamic]string, allocator=ctx.allocator);

    return ctx
}

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
        joined, _ = filepath.join({importer_dir, import_path}, allocator=get_ctx().allocator)
    } else {
        // plain path: working directory first
        joined = import_path

        // not the entry file, and not found in cwd: try next to the compiler
        if len(importer_path) > 0 && !os.exists(import_path) {
            exe_dir, eerr := os.get_executable_directory(get_ctx().allocator)
            if eerr == nil {
                candidate, _ := filepath.join({exe_dir, import_path}, allocator=get_ctx().allocator)
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

    cleaned, cerr := filepath.clean(abs_path, allocator=get_ctx().allocator)
    assert(cerr == .None)

    cwd, _ := os.get_working_directory(get_ctx().allocator)

    rel, rerr := filepath.rel(cwd, cleaned, allocator=get_ctx().allocator)
    if rerr != filepath.Relative_Error.None {
        // Can't be made relative (e.g. different drive on Windows)
        return cleaned
    }

    return rel
}

// `file_name` is the raw path as written (CLI arg or `import` string).
// It's resolved with resolve_import_path against `ctx.current_file`, and
// the resolved path is the key everywhere (ctx.files, ctx.parsing,
// ctx.modules). Returns the resolved path.
//
// Only parses, resolves and type-checks. Generic instantiation and
// lowering happen once the whole import graph is done (see main).
handle_file :: proc(file_name: string) -> string {
    resolved := resolve_import_path(get_ctx().current_file, file_name)
    // debugln("handling:", resolved)

    if _, ok := get_ctx().parsing[resolved]; ok {
        gala_panicf("Cyclical imports with file: \"%s\".", resolved)
    }
    if _, ok := get_ctx().modules[resolved]; ok {
        // Already fully parsed/resolved (e.g. two files import the same
        // dependency) — nothing more to do.
        return resolved
    }

    data, err := os.read_entire_file(resolved, get_ctx().allocator)
    if err != io.Error.None {
        gala_panic("Failed to read file:", resolved)
    }

    get_ctx().files[resolved] = string(data)
    get_ctx().parsing[resolved] = string(data)

    // Save/restore current_file so that once this imported file is done,
    // the importer resolves its own further imports relative to itself.
    prev_file := get_ctx().current_file
    get_ctx().current_file = resolved

    tokens := lex_file(data)
    ast := new(AST, get_ctx().allocator)
    ast^ = parse_tokens(resolved, tokens[:])
    mid := resolve_module_ast(ast, resolved)
    typecheck_module(ast, mid, resolved)
    append(&get_ctx().units, ModuleUnit{ast = ast, mod = mid, file = resolved})
    // get_ctx().mods[mid].ast = ast   // the module holds a copy; sync it

    get_ctx().modules[resolved] = mid
    delete_key(&get_ctx().parsing, resolved)

    get_ctx().current_file = prev_file
    return resolved
}

// `ctx_allocator` is the allocator `ctx` itself was created with (the
// heap, see main) — NOT ctx.allocator, which is the arena being destroyed
// here and can't free individual blocks anyway.
destroy_context :: proc(ctx: ^Context, ctx_allocator: mem.Allocator) {
    virtual.arena_destroy(&ctx.arena)
    free(ctx, ctx_allocator)
}

// ---------------------------------------------------------------------
// CLI
// ---------------------------------------------------------------------

print_usage :: proc() {
    gala_infof("usage: galac {} <file.gala> [-c] [-S -emit-llvm | -emit-llvm] [-o <name>] [--dev|--release] [--abi sysv|win64] [-l <library>]...", VERSION)
}

CliArgs :: struct {
    entry_file:   string,
    program_name: string,        // output path (executable, or object/library with -c)
    output_set:   bool,          // -o was given
    object_only:  bool,          // -c: emit one merged .o (.lib on win64), don't link
    emit_llvm:    bool,          // -emit-llvm: stop after merging modules into one IR file
    text_out:     bool,          // -S: with -emit-llvm, write textual .ll instead of .bc
    check_only:   bool,          // --check: no codegen
    debug:        bool,
    mode:         BuildMode,     // --dev (default) / --release
    extra_libs:   [dynamic]string, // paths passed via -l/--link
    abi:          AbiKind,       // --abi sysv|win64
}


// Validates the file name of `-o <path>`; the directory part is free-form.
// No leading '-' (option confusion in the tools we spawn), no leading '.'
// or digit; '.' and '-' are fine after the first character.
is_legal_program_name :: proc(path: string) -> bool {
    base := path
    for i := len(path) - 1; i >= 0; i -= 1 {
        if path[i] == '/' || path[i] == '\\' {
            base = path[i+1:]
            break
        }
    }
    if len(base) == 0 do return false // "" or a trailing separator

    for c, i in base {
        switch {
        case c >= 'a' && c <= 'z':
        case c >= 'A' && c <= 'Z':
        case c == '_':
        case (c == '-' || c == '.') && i > 0:
        case c >= '0' && c <= '9' && i > 0:
        case:
            return false
        }
    }
    return true
}

parse_cli_args :: proc(args: []string) -> CliArgs {
    result := CliArgs{program_name = "main", debug = false, abi = .SysV, mode = .DEV}
    result.extra_libs = make([dynamic]string)
    entry_file_set := false

    i := 0
    for i < len(args) {
        arg := args[i]

        switch {
        case arg == "-o":
            i += 1
            if i >= len(args) {
                gala_panic("expected program name after \"-o\".")
            }
            name := args[i]
            if !is_legal_program_name(name) {
                gala_panic("illegal program name:", name)
            }
            result.program_name = name
            result.output_set = true

        case arg == "-c":
            // Compile only: merge all module objects into one .o
            result.object_only = true
        case arg == "-emit-llvm":
            result.emit_llvm = true
        case arg == "-S":
            result.text_out = true
        case arg == "--check":
            // check only. no codegen
            result.check_only = true

        case arg == "--dev":
            // fast compile: no opt, llc -O0 (default)
            result.mode = .DEV
        case arg == "--release":
            // optimised: opt mem2reg + llc -O2
            result.mode = .RELEASE

        case arg == "-l" || arg == "--link":
            // Suffix is validated after all args are parsed (it depends on
            // --abi, which may come later on the command line).
            i += 1
            if i >= len(args) {
                gala_panic("expected a library path after \"-l\".")
            }
            append(&result.extra_libs, args[i])

        case arg == "-h" || arg == "--help":
            print_usage()
            os.exit(0)

        case arg == "-d" || arg == "--debug":
            result.debug = true

        case arg == "--abi":
            i += 1
            if i >= len(args) {
                gala_panic("expected \"sysv\" or \"win64\" after \"--abi\".")
            }
            switch args[i] {
            case "sysv":  result.abi = .SysV
            case "win64": result.abi = .Win64
            case: gala_panic("unknown abi:", args[i], "(expected sysv or win64)")
            }

        case strings.has_suffix(arg, ".gala") && len(arg) > len(".gala"): // "not just \".gala\""
            if entry_file_set {
                gala_panic("must specify exactly one \".gala\" file, got an extra one:", arg)
            }
            result.entry_file = arg
            entry_file_set = true

        case:
            gala_panic("unrecognized argument:", arg)
        }

        i += 1
    }

    if !entry_file_set {
        print_usage()
        gala_panic("must specify exactly one \".gala\" file.")
    }

    if result.text_out && !result.emit_llvm {
        gala_panic("\"-S\" is only supported together with \"-emit-llvm\".")
    }
    if result.emit_llvm && result.object_only {
        gala_panic("\"-emit-llvm\" can't be combined with \"-c\".")
    }

    // Linux links shared objects; the MSVC linker takes .lib files.
    lib_suffix := ".lib" if result.abi == .Win64 else ".so"
    for lib_path in result.extra_libs {
        /* if !strings.has_suffix(lib_path, lib_suffix) {
            gala_panic("expected a", lib_suffix, "file for this abi, got:", lib_path)
        } */
        if !os.exists(lib_path) {
            gala_panic("cannot find library to link:", lib_path)
        }
    }

    // default output names
    if !result.output_set {
        if result.emit_llvm {
            result.program_name = "main.ll" if result.text_out else "main.bc"
        } else if result.object_only {
            result.program_name = "main.lib" if result.abi == .Win64 else "main.o"
        } else if result.abi == .Win64 {
            result.program_name = "main.exe"
        }
    }

    return result
}

// Runs `command`, panics if it can't start or exits non-zero.
// `what` is only used in messages.
run_command :: proc(command: []string, what: string) {
    debugf("%s command: ", what)
    for a in command {
        debugf("%s ", a)
    }
    debugfln("")

    p, err := os.process_start({command = command})
    if err != nil {
        gala_panic("Failed to start process:", what, err)
    }

    p_state, werr := os.process_wait(p)
    if werr != nil {
        gala_panic("Failed to wait for process:", what, werr)
    }
    if p_state.exit_code != 0 {
        gala_panic("Process failed:", what, "exit code:", p_state.exit_code)
    }
}

// ---------------------------------------------------------------------
// MSVC toolchain
// ---------------------------------------------------------------------

// Quote a path for a .bat file / the Windows command interpreter.
msvc_quote :: proc(value: string) -> string {
    return fmt.aprintf("\"%s\"", value)
}

// Add a path as a quoted argument to a linker command line.
msvc_append_path :: proc(sb: ^strings.Builder, value: string) {
    strings.write_string(sb, msvc_quote(value))
    strings.write_byte(sb, ' ')
}

// Runs vswhere directly (no shell, so no quoting problems) and returns the
// first line of its output, or "" if it printed nothing / failed.
vswhere_query :: proc(vswhere: string, extra: []string) -> string {
    command := make([dynamic]string, allocator = get_ctx().allocator)
    append(&command, vswhere)
    append(&command, "-latest")
    append(&command, "-products")
    append(&command, "*")
    for a in extra do append(&command, a)
    append(&command, "-property")
    append(&command, "installationPath")

    state, stdout, _, err := os.process_exec({command = command[:]}, get_ctx().allocator)
    if err != nil || state.exit_code != 0 do return ""

    for line in strings.split_lines(string(stdout), get_ctx().allocator) {
        l := strings.trim_space(line)
        if len(l) > 0 do return l
    }
    return ""
}

// Locates vcvars64.bat via the Visual Studio Installer's vswhere.
msvc_vcvars_path :: proc(ctx: ^Context) -> string {
    pf86, found := os.lookup_env("ProgramFiles(x86)", ctx.allocator)
    if !found do pf86 = "C:\\Program Files (x86)"

    vswhere := fmt.aprintf("%s\\Microsoft Visual Studio\\Installer\\vswhere.exe", pf86)
    if !os.exists(vswhere) {
        gala_panic("cannot find vswhere.exe at:", vswhere)
    }

    // Prefer an install with the C++ x64 tools; fall back to any install
    // (Build Tools / prerelease sometimes don't register that component id).
    install := vswhere_query(vswhere, {"-requires", "Microsoft.VisualStudio.Component.VC.Tools.x86.x64"})
    if install == "" do install = vswhere_query(vswhere, {})
    if install == "" do install = vswhere_query(vswhere, {"-prerelease"})
    if install == "" {
        gala_panic("vswhere found no Visual Studio installation (install the \"Desktop development with C++\" workload)")
    }

    path := fmt.aprintf("%s\\VC\\Auxiliary\\Build\\vcvars64.bat", install)
    if !os.exists(path) {
        gala_panic("Visual Studio found at", install, "but vcvars64.bat is missing; install the MSVC x64 build tools")
    }
    return path
}

// Runs an MSVC tool after importing the Visual Studio x64 environment.
// The commands go into a .bat file so cmd.exe does its own parsing and
// nothing depends on how Odin quotes the process command line.
run_msvc_command :: proc(vcvars: string, command_line: string, what: string) {
    _ = os.make_directory(".gala_build")

    bat := ".gala_build\\msvc_step.bat"
    sb := strings.builder_make()
    strings.write_string(&sb, "@echo off\r\n")
    strings.write_string(&sb, fmt.aprintf("call \"%s\" >nul\r\n", vcvars))
    strings.write_string(&sb, "if errorlevel 1 exit /b 1\r\n")
    strings.write_string(&sb, command_line)
    strings.write_string(&sb, "\r\n")
    strings.write_string(&sb, "exit /b %errorlevel%\r\n")

    if werr := os.write_entire_file(bat, sb.buf[:]); werr != nil {
        gala_panic("Failed to write", bat)
    }
    run_command({"cmd.exe", "/D", "/C", bat}, what)
}

// ---------------------------------------------------------------------
// Linking
// ---------------------------------------------------------------------

link_executable_sysv :: proc(ctx: ^Context, extra_libs: []string) {
    command := make([dynamic]string, allocator = ctx.allocator)
    append(&command, "ld")
    append(&command, "-dynamic-linker")
    append(&command, "/lib64/ld-linux-x86-64.so.2")

    if os.exists("/usr/lib/crt1.o") {
        // Arch and similar
        append(&command, "/usr/lib/crt1.o")
        append(&command, "/usr/lib/crti.o")
        append(&command, "-L/usr/lib")
    } else {
        // Ubuntu/Debian
        append(&command, "/usr/lib/x86_64-linux-gnu/crt1.o")
        append(&command, "/usr/lib/x86_64-linux-gnu/crti.o")
        append(&command, "-L/usr/lib/x86_64-linux-gnu")
    }

    append(&command, "-lc")
    // Objects first: ld only pulls archive members for symbols that are
    // already undefined, so .a files have to come after the .o files.
    for f in ctx.o_files do append(&command, f)
    for f in ctx.links do append(&command, f)
    for lib in extra_libs do append(&command, lib)

    if os.exists("/usr/lib/crtn.o") {
        append(&command, "/usr/lib/crtn.o")
    } else {
        append(&command, "/usr/lib/x86_64-linux-gnu/crtn.o")
    }
    append(&command, "-o")
    append(&command, ctx.program_name)
    run_command(command[:], "link (ld)")
}

// true:  /MD  -> msvcrt.lib + vcruntime.lib + ucrt.lib (CRT in DLLs; needed
//                for libraries built with /MD, which is the default for raylib)
// false: /MT  -> libcmt.lib + libvcruntime.lib + libucrt.lib (CRT in the exe)
// Every object and library in one link must agree.
MSVC_DYNAMIC_CRT :: true

link_executable_msvc :: proc(ctx: ^Context, extra_libs: []string) {

    vcvars := msvc_vcvars_path(ctx)
    sb := strings.builder_make()
    strings.write_string(&sb, "link.exe /NOLOGO /MACHINE:X64 /SUBSYSTEM:CONSOLE /INCREMENTAL:NO ")
    strings.write_string(&sb, fmt.aprintf("/OUT:%s ", msvc_quote(ctx.program_name)))

    for f in ctx.o_files do msvc_append_path(&sb, f)

    // @link entries are either library paths or linker switches. Paths are
    // quoted; switches like /DEFAULTLIB:foo.lib pass through unchanged.
    for f in ctx.links {

        if strings.has_prefix(f, "/") || strings.has_prefix(f, "-") {
            strings.write_string(&sb, f)
            strings.write_byte(&sb, ' ')
        } else {

            msvc_append_path(&sb, f)
        }
    }
    for f in extra_libs do msvc_append_path(&sb, f)

    // CRT, selected explicitly: LLVM-generated objects don't carry the usual
    // /DEFAULTLIB directives.
    when MSVC_DYNAMIC_CRT {
        strings.write_string(&sb, "msvcrt.lib vcruntime.lib ucrt.lib kernel32.lib")
    } else {
        strings.write_string(&sb, "libcmt.lib libvcruntime.lib libucrt.lib kernel32.lib")
    }
    run_msvc_command(vcvars, strings.to_string(sb), "link (MSVC link.exe)")

}

link_executable :: proc(ctx: ^Context, extra_libs: []string) {
    if abi_resolve_kind(ctx.abi) == .Win64 {
        link_executable_msvc(ctx, extra_libs)
    } else {
        link_executable_sysv(ctx, extra_libs)
    }
}

// -c mode: SysV produces a relocatable object via `ld -r`. MSVC's librarian
// produces a static COFF library, not a single flattened .obj.
link_object :: proc(ctx: ^Context) {
    // the output must not clobber an input
    for f in ctx.o_files {
        if f == ctx.program_name {
            gala_panic("output name collides with an intermediate object:", f)
        }
    }

    if abi_resolve_kind(ctx.abi) == .Win64 {
        vcvars := msvc_vcvars_path(ctx)
        sb := strings.builder_make()
        strings.write_string(&sb, "lib.exe /NOLOGO ")
        strings.write_string(&sb, fmt.aprintf("/OUT:%s ", msvc_quote(ctx.program_name)))
        for f in ctx.o_files do msvc_append_path(&sb, f)
        run_msvc_command(vcvars, strings.to_string(sb), "archive objects (MSVC lib.exe)")
    } else {
        command := make([dynamic]string, allocator = ctx.allocator)
        append(&command, "ld")
        append(&command, "-r")
        for f in ctx.o_files do append(&command, f)
        append(&command, "-o")
        append(&command, ctx.program_name)
        run_command(command[:], "merge objects (ld -r)")
    }
}

// An object can't remember its libraries, so write them next to it as
// `<output>.links`, one path per line, for the driver's final link.
write_link_list :: proc(ctx: ^Context, extra_libs: []string) {
    sb := strings.builder_make()
    for f in ctx.links {
        strings.write_string(&sb, f)
        strings.write_byte(&sb, '\n')
    }
    for f in extra_libs {
        strings.write_string(&sb, f)
        strings.write_byte(&sb, '\n')
    }

    path := fmt.aprintf("%s.links", ctx.program_name)
    werr := os.write_entire_file(path, sb.buf[:])
    if werr != nil {
        gala_panic("Failed to write link list:", path)
    }
}

RtKind :: enum i64 {
    None,       // not a runtime type
    Int, Uint, Float, Bool, Byte, Rune, String,
    Pointer, Slice, Array, Struct, Union, Fn, Any,
}

rt_kind_of :: proc(k: TypeKind) -> RtKind {
    #partial switch k {
    case .Int_8, .Int16, .Int32, .Int64:     return .Int
    case .UInt_8, .UInt16, .UInt32, .UInt64: return .Uint
    case .Flt_8, .Flt16, .Flt32, .Flt64:     return .Float
    case .Bool:           return .Bool
    case .Byte:           return .Byte
    case .Rune:           return .Rune
    case .String:         return .String
    case .Pointer:        return .Pointer
    case .Slice:          return .Slice
    case .FixedSizeArray: return .Array
    case .Struct:         return .Struct
    case .Union:          return .Union
    case .Function:       return .Fn
    case .Any:            return .Any
    }
    return .None
}
main :: proc() { // odins context is passed down, not up, or some shi
    cli := parse_cli_args(os.args[1:]) // skip program name itself
    // extra_libs was made on the heap before we switch allocators below; a

    // dynamic array remembers its own allocator, so this frees it correctly
    // even though it runs after context.allocator has been changed.
    defer delete(cli.extra_libs)

    // `ctx` itself is allocated from the heap (it holds the arena, so it
    // can't live inside it). Remember that allocator so it can be freed at
    // the end.
    heap := context.allocator

    ctx := init_context(debug=cli.debug)
    ctx.mode = cli.mode
    context.user_ptr = ctx   // <-- set it here, so it's live for the rest of main's scope


    // The ABI for this compilation lives in the context and nowhere else
    // (abi_resolve_kind(.None) reads it). Set it before anything can ask.
    ctx.abi = cli.abi


    // Route EVERYTHING below main (every bare make/append/aprintf/
    // strings.builder_make/filepath.* call, and every lazily-allocated nil
    // map such as ctx.modules or ctx.ty_modules) into the arena, so it's all
    // released by one arena_destroy instead of showing up as a heap leak.
    context.allocator = ctx.allocator


    inject_kind_constants(&ctx.base_mod);
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
    new_type(&ctx.base_mod, Type{name="rawptr", kind=.Pointer, ptr=void_type()}, {});
    new_type(&ctx.base_mod, Type{name="string", kind=.String}, {});
    // type info
    i64_ty    := ty_from_name("i64")
    string_ty := ty_from_name("string")

    field_ty := new_type(&ctx.base_mod, Type{name="TypeField", kind=.Struct, structure={ fields = {
        {name="name",    type=string_ty},
        {name="typeid", type=i64_ty},
        {name="offset",  type=i64_ty},
    }}}, {})

    fields_slice_ty := intern_type(Type{kind=.Slice, slice={type=field_ty}})


    new_type(&ctx.base_mod, Type{name="TypeInfo", kind=.Struct, structure={ fields = {
        {name="name",   type=string_ty},
        {name="kind",   type=i64_ty},
        {name="size",   type=i64_ty},

        {name="elem",   type=i64_ty},   // ptr/slice/array element type id
        {name="count",  type=i64_ty},   // fixed array length
        {name="fields", type=fields_slice_ty},
    }}}, {})
    debugln("abi:", ctx.abi);

    ctx.program_name = cli.program_name
    ctx.object_only = cli.object_only

    // entry_file is compared against module.path in cg_module to decide
    // whether to emit the C `main` wrapper. module.path is the *resolved*
    // path (see handle_file), so the entry file has to go through the exact
    // same resolution, otherwise `galac /tmp/x.gala` never matches and no
    // entry point is emitted. ctx.current_file is "" here, same as it will
    // be when handle_file resolves it below.
    get_ctx().entry_file = resolve_import_path("", cli.entry_file)
    t := time.tick_now()
    phase("main", &t);
    handle_file(cli.entry_file)

    phase("generics", &t);
    // every module is type-checked now, so all generic calls are known
    typecheck_generics()
    phase("end", &t);



    phase("lower", &t);
    // lowering runs after generics so the clones get lowered too
    for u in get_ctx().units {
        get_ctx().current_file = u.file
        get_ctx().current_module_id = u.mod
        lower_module(u.ast)
    }

    if !cli.check_only {
        phase("codegen", &t);
        // then generate
        for m, mid in get_ctx().modules {
            // set for codegen
            get_ctx().current_file = m;
            debugln("CG MOD:", m, mid);
            cg_module(mid)
        }


        phase("compile", &t);
        if cli.emit_llvm {
            // one merged IR file, no llc, no linker
            compile_all(llvm_out = cli.program_name, llvm_text = cli.text_out)
        } else {
            // parallel compile
            compile_all();

            phase("link", &t);
            if cli.object_only {
                link_object(ctx)
                write_link_list(ctx, cli.extra_libs[:])
            } else {
                link_executable(ctx, cli.extra_libs[:])
            }
        }
    }
    phase("end", &t);
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
    destroy_context(ctx, heap);
    free_all(context.temp_allocator);
    debugln("Finished compiling");
}
