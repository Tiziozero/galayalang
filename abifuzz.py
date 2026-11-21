#!/usr/bin/env python3
"""
abi_fuzz.py - differential ABI tester for the Gala compiler.

For every test case it generates

  * test.c      random structs + functions + forwarding wrappers (clang's view)
  * main.gala   the same structs/functions in Gala

compiles the C with `clang -S -emit-llvm` and the Gala with
`galac -S -emit-llvm main.gala`, then compares, per function:

  1. the `define` line of  funcN      (return lowering, param lowering)
  2. the `define` line of  callN      (wrapper with the same signature)
  3. the `call` instruction in callN  (call-site lowering of args and result)

Both sides are normalised first (see normalise section) so only things that
matter for the ABI are compared: types, signext/zeroext, byval(T) align N,
sret(T) align N.  Value names, noundef, dso_local, #attrs, struct type naming
(%struct.S0 vs %S0) are ignored.  A coerced aggregate parameter `{ i64, i64 }`
is flattened to `i64, i64`, which is how clang spells the same thing.

Also, each generated C struct carries _Static_assert()s for sizeof / alignof /
offsetof computed by this script's layout model, so clang tells you if the
model (and therefore what Gala should do) is wrong.

Usage examples
--------------
  ./abi_fuzz.py                              # classic cases + 20 random cases, SysV
  ./abi_fuzz.py --target windows             # Win64 / MSVC
  ./abi_fuzz.py --cases 200 --funcs 60       # more
  ./abi_fuzz.py --seed 1234 --cases 1        # reproduce one random case
  ./abi_fuzz.py --no-galac                   # only generate + validate the C side
  ./abi_fuzz.py --galac ./build/galac --galac-args "-target x86_64-pc-windows-msvc"

Exit status is non-zero if any mismatch is found.

Things you probably have to adjust for Gala (all near the top of the file):
  SCALARS            Gala spelling of the scalar types
  --gala-main        text of the entry point if galac insists on one
  --gala-prefix      if Gala mangles symbol names (e.g. "main.")
  --positional       use `S{1, 2}` literals instead of `S{x=1, y=2}`
"""

import argparse
import random
import re
import shlex
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path

# ---------------------------------------------------------------------------
# Type model
# ---------------------------------------------------------------------------

# key: (C type, Gala type, size/alignment in bytes, Gala zero literal)
SCALARS = {
    "i8":   ("signed char",         "i8",    1, "0"),
    "u8":   ("unsigned char",       "u8",    1, "0"),
    "byte": ("unsigned char",       "byte",  1, "0"),
    "bool": ("_Bool",               "bool",  1, "false"),
    "i16":  ("short",               "i16",   2, "0"),
    "u16":  ("unsigned short",      "u16",   2, "0"),
    "i32":  ("int",                 "i32",   4, "0"),
    "u32":  ("unsigned int",        "u32",   4, "0"),
    "i64":  ("long long",           "i64",   8, "0"),
    "u64":  ("unsigned long long",  "u64",   8, "0"),
    "f32":  ("float",               "f32",   4, "0.0"),
    "f64":  ("double",              "f64",   8, "0.0"),
    "ptr":  ("void *",              "^i32",  8, "cast(^i32)0"),
}
DEFAULT_SCALARS = list(SCALARS)


def align_up(x, a):
    return (x + a - 1) // a * a


class Scalar:
    def __init__(self, key):
        self.key = key
        self.c, self.g, self.size, self.lit = SCALARS[key]
        self.align = self.size

    def c_name(self):
        return self.c

    def c_decl(self, name):
        return f"{self.c} {name}"

    def c_init(self):
        return "0"

    def gala(self):
        return self.g

    def gala_lit(self, named):
        return self.lit

    def structs(self):
        return []

    def desc(self):
        return self.key


class Arr:
    def __init__(self, elem, n):
        self.elem, self.n = elem, n
        self.size = elem.size * n
        self.align = elem.align

    def c_decl(self, name):
        return self.elem.c_decl(f"{name}[{self.n}]")

    def gala(self):
        return f"[{self.n}]{self.elem.gala()}"

    def gala_lit(self, named):
        item = self.elem.gala_lit(named)
        return f"{self.gala()}{{{', '.join([item] * self.n)}}}"

    def structs(self):
        return self.elem.structs()

    def desc(self):
        return f"{self.elem.desc()}[{self.n}]"


class Struct:
    def __init__(self, name, fields):
        self.name, self.fields = name, fields
        off, a, offs = 0, 1, []
        for f in fields:
            off = align_up(off, f.align)
            offs.append(off)
            off += f.size
            a = max(a, f.align)
        self.offsets = offs
        self.align = a
        self.size = align_up(off, a)

    def c_name(self):
        return self.name

    def c_decl(self, name):
        return f"{self.name} {name}"

    def c_init(self):
        return "{0}"

    def gala(self):
        return self.name

    def gala_lit(self, named):
        if named:
            inner = ", ".join(f"m{i}={f.gala_lit(named)}" for i, f in enumerate(self.fields))
        else:
            inner = ", ".join(f.gala_lit(named) for f in self.fields)
        return f"{self.name}{{{inner}}}"

    def structs(self):
        out = []
        for f in self.fields:
            for s in f.structs():
                if s not in out:
                    out.append(s)
        out.append(self)
        return out

    def desc(self):
        return self.name

    def layout_str(self):
        parts = ", ".join(f"m{i}:{f.desc()}@{o}" for i, (f, o) in enumerate(zip(self.fields, self.offsets)))
        return f"{self.name} size={self.size} align={self.align} {{ {parts} }}"


@dataclass
class Fn:
    idx: int
    ret: object  # type or None (void)
    params: list

    @property
    def name(self):
        return f"func{self.idx}"

    @property
    def wrapper(self):
        return f"call{self.idx}"


@dataclass
class Case:
    label: str
    structs: list
    fns: list


# ---------------------------------------------------------------------------
# Generation
# ---------------------------------------------------------------------------

CLASSIC = [
    # the "byte byte byte byte, then a bigger thing" family and friends
    ["i8", "i8", "i8", "i8", "f32"],
    ["f32", "f32"],
    ["f32", "i32"],
    ["i32", "f32"],
    ["f64", "i8"],
    ["i8", "f64"],
    ["i8", "i8", "i8"],
    ["f32", "f32", "f32"],
    ["f32", "f32", "f32", "f32"],
    ["f64", "f64", "f64"],
    ["f32", "f64"],
    ["f64", "f32"],
    ["i8", "f32", "f32"],
    ["f32", "f32", "i32"],
    ["i32", "f32", "f32"],
    ["bool", "f32"],
    ["bool", "bool", "bool", "bool", "bool", "bool", "bool", "bool", "bool"],
    ["i16", "i16", "i16"],
    ["u16", "u8"],
    ["f32[3]"],
    ["f32[2]", "i32"],
    ["i8[9]"],
    ["i8[17]"],
    ["i64", "i64"],
    ["i64", "f64"],
    ["f64", "i64"],
    ["i32", "i8"],
    ["i64", "i8"],
    ["i8", "i64"],
    ["ptr", "ptr"],
    ["ptr", "i8"],
    ["i8", "u8", "u16", "i32"],
    ["i8", "i8", "i8", "i8", "i8", "i8", "i8", "i8", "f32"],
    ["f32", "i8", "i8", "i8", "i8", "f32"],
]


def parse_classic(spec, scalars_ok):
    fields = []
    for item in spec:
        m = re.fullmatch(r"(\w+)\[(\d+)\]", item)
        key, n = (m.group(1), int(m.group(2))) if m else (item, 0)
        if key not in scalars_ok:
            return None
        fields.append(Arr(Scalar(key), n) if n else Scalar(key))
    return fields


def gen_field(rng, scalars, structs):
    r = rng.random()
    if structs and r < 0.20:
        return rng.choice(structs)
    if r < 0.35:
        return Arr(Scalar(rng.choice(scalars)), rng.randint(1, 5))
    if structs and r < 0.40:
        small = [s for s in structs if s.size <= 16]
        if small:
            return Arr(rng.choice(small), rng.randint(1, 3))
    return Scalar(rng.choice(scalars))


def gen_struct(rng, args, scalars, structs):
    name = f"S{len(structs)}"
    for _ in range(50):
        n = rng.randint(1, args.max_fields)
        st = Struct(name, [gen_field(rng, scalars, structs) for _ in range(n)])
        if st.size <= args.max_size:
            return st
    return Struct(name, [Scalar(rng.choice(scalars))])


def gen_fn(rng, args, idx, scalars, structs):
    r = rng.random()
    if r < 0.10:
        ret = None
    elif r < 0.40:
        ret = Scalar(rng.choice(scalars))
    else:
        ret = rng.choice(structs)
    params = []
    for _ in range(rng.randint(0, args.max_params)):
        if rng.random() < 0.35:
            params.append(Scalar(rng.choice(scalars)))
        else:
            params.append(rng.choice(structs))
    return Fn(idx, ret, params)


def make_random_case(seed, args, scalars):
    rng = random.Random(seed)
    structs = []
    for _ in range(args.structs):
        structs.append(gen_struct(rng, args, scalars, structs))
    fns = [gen_fn(rng, args, i, scalars, structs) for i in range(args.funcs)]
    return Case(f"seed_{seed}", structs, fns)


def make_classic_case(scalars):
    structs = []
    for spec in CLASSIC:
        fields = parse_classic(spec, scalars)
        if fields:
            structs.append(Struct(f"S{len(structs)}", fields))
    fns = []

    def add(ret, params):
        fns.append(Fn(len(fns), ret, params))

    i64, f64 = Scalar("i64"), Scalar("f64")
    have_i64, have_f64 = "i64" in scalars, "f64" in scalars
    for st in structs:
        add(st, [])                     # return only
        add(None, [st])                 # arg only
        add(st, [st, st])               # two in, one out
        if have_i64:
            add(None, [i64] * 5 + [st])      # 1 GPR left
            add(None, [i64] * 6 + [st])      # 0 GPRs left -> stack / byval
        if have_f64:
            add(None, [f64] * 7 + [st])      # 1 XMM left
            add(None, [f64] * 8 + [st])      # 0 XMM left
        if have_i64:
            add(st, [i64] * 5 + [st, i64])   # sret + register pressure
    return Case("classic", structs, fns)


# ---------------------------------------------------------------------------
# Source emission
# ---------------------------------------------------------------------------

def c_sig(fn, name):
    ret = fn.ret.c_name() if fn.ret else "void"
    params = ", ".join(f"{p.c_name()} p{i}" for i, p in enumerate(fn.params)) or "void"
    return f"{ret} {name}({params})"


def gala_sig(fn, name, export=False):
    params = ", ".join(f"p{i}: {p.gala()}" for i, p in enumerate(fn.params))
    ret = f": {fn.ret.gala()}" if fn.ret else ""
    # `export fn` makes Gala emit the unmangled symbol (define @func0, not
    # @gala_<n>._main.func0), which is what check_case() looks up.
    return f"{'export ' if export else ''}fn {name}({params}){ret}"


def emit_c(case):
    out = [f"// generated by abi_fuzz.py, case {case.label}", ""]
    for st in case.structs:
        out.append(f"struct {st.name} {{")
        for i, f in enumerate(st.fields):
            out.append(f"    {f.c_decl(f'm{i}')};")
        out.append("};")
        out.append(f"typedef struct {st.name} {st.name};")
        out.append(f'_Static_assert(sizeof({st.name}) == {st.size}, "size {st.name}");')
        out.append(f'_Static_assert(_Alignof({st.name}) == {st.align}, "align {st.name}");')
        for i, off in enumerate(st.offsets):
            out.append(
                f'_Static_assert(__builtin_offsetof({st.name}, m{i}) == {off}, "offset {st.name}.m{i}");'
            )
        out.append("")
    for fn in case.fns:
        out.append(f"{c_sig(fn, fn.name)} {{")
        if fn.ret is None:
            pass
        elif isinstance(fn.ret, Scalar):
            out.append("    return 0;")
        else:
            out.append(f"    {fn.ret.c_name()} r = {{0}};")
            out.append("    return r;")
        out.append("}")
        args = ", ".join(f"p{i}" for i in range(len(fn.params)))
        out.append(f"{c_sig(fn, fn.wrapper)} {{")
        out.append(f"    {'return ' if fn.ret else ''}{fn.name}({args});")
        out.append("}")
        out.append("")
    return "\n".join(out)


def emit_gala(case, args):
    named = not args.positional
    out = []
    if args.gala_prelude:
        out.append(Path(args.gala_prelude).read_text())
        out.append("")
    for st in case.structs:
        out.append(f"struct {st.name} {{")
        out.append(",\n".join(f"    m{i}: {f.gala()}" for i, f in enumerate(st.fields)))
        out.append("}")
        out.append("")
    for fn in case.fns:
        out.append(f"{gala_sig(fn, fn.name, export=True)} {{")
        if fn.ret is not None:
            out.append(f"    return {fn.ret.gala_lit(named)};")
        out.append("}")
        call_args = ", ".join(f"p{i}" for i in range(len(fn.params)))
        out.append(f"{gala_sig(fn, fn.wrapper, export=True)} {{")
        out.append(f"    {'return ' if fn.ret else ''}{fn.name}({call_args});")
        out.append("}")
        out.append("")
    if args.gala_main != "none":
        out.append(args.gala_main.replace("\\n", "\n"))
    return "\n".join(out)


# ---------------------------------------------------------------------------
# Running the compilers
# ---------------------------------------------------------------------------

TRIPLES = {
    "linux": "x86_64-pc-linux-gnu",
    "windows": "x86_64-pc-windows-msvc",
}
NATIVE_CC = {"linux": "x86_64_sysvcc", "windows": "win64cc"}


def run(cmd, cwd=None, timeout=300):
    try:
        return subprocess.run(cmd, cwd=cwd, capture_output=True, text=True, timeout=timeout)
    except FileNotFoundError:
        sys.exit(f"error: cannot run {cmd[0]!r} (not found). Use --clang / --galac to point at it.")


def compile_c(args, cfile):
    cmd = [args.clang, f"--target={TRIPLES[args.target]}", "-x", "c", "-std=c11", "-O0",
           "-S", "-emit-llvm", "-Wno-everything", "-o", "-", str(cfile)]
    p = run(cmd)
    if p.returncode != 0:
        print("clang failed (this is a bug in the generator or its layout model):", file=sys.stderr)
        print(p.stderr, file=sys.stderr)
        return None
    return p.stdout


def compile_gala(args, workdir):
    cmd = [args.galac, "-S", "-emit-llvm", *shlex.split(args.galac_args), "main.gala"]
    before = {p.name for p in Path(workdir).glob("*.ll")}
    p = run(cmd, cwd=workdir)
    if p.returncode != 0:
        print("galac failed:", file=sys.stderr)
        print(p.stdout + p.stderr, file=sys.stderr)
        return None
    if "define " in p.stdout:
        return p.stdout
    produced = [q for q in Path(workdir).glob("*.ll")]
    fresh = [q for q in produced if q.name not in before] or produced
    if fresh:
        return fresh[0].read_text()
    print("galac produced no IR on stdout and no .ll file in the working dir.", file=sys.stderr)
    print(p.stdout + p.stderr, file=sys.stderr)
    return None


# ---------------------------------------------------------------------------
# IR parsing + normalisation
# ---------------------------------------------------------------------------

DEF_NAME_RE = re.compile(r'@(?:"([^"]+)"|([\w.$-]+))\(')
EXT_RE = re.compile(r"\b(signext|zeroext)\b")
BYVAL_RE = re.compile(r"\bbyval\(([^)]*)\)")
SRET_RE = re.compile(r"\bsret\(([^)]*)\)")
ALIGN_RE = re.compile(r"\balign (\d+)\b")
PARAM_TYPE_RE = re.compile(r"\s*(\{[^{}]*\}|<[^<>]*>|\[[^\[\]]*\]|[A-Za-z_%][\w.%\"$-]*)")
RET_TYPE_RE = re.compile(r"\{[^{}]*\}|<[^<>]*>|\bvoid\b|\bptr\b|\bhalf\b|\bfloat\b|\bdouble\b|\bi\d+\b")


def balanced(s, open_idx):
    """s[open_idx] == '(' -> contents up to the matching ')'."""
    depth = 0
    for i in range(open_idx, len(s)):
        ch = s[i]
        if ch in "({[<":
            depth += 1
        elif ch in ")}]>":
            depth -= 1
            if depth == 0:
                return s[open_idx + 1:i]
    return s[open_idx + 1:]


def split_top(s):
    parts, depth, cur = [], 0, []
    for ch in s:
        if ch in "({[<":
            depth += 1
        elif ch in ")}]>":
            depth -= 1
        if ch == "," and depth == 0:
            parts.append("".join(cur).strip())
            cur = []
        else:
            cur.append(ch)
    tail = "".join(cur).strip()
    if tail:
        parts.append(tail)
    return parts


def tyname(t):
    t = t.strip().lstrip("%").strip('"')
    for pre in ("struct.", "union."):
        if t.startswith(pre):
            t = t[len(pre):]
    return "%" + t.split(".")[-1]


def norm_param(text):
    """One parameter / call argument -> list of canonical strings (flattened)."""
    text = text.strip()
    m = PARAM_TYPE_RE.match(text)
    ty = m.group(1) if m else text
    ext = EXT_RE.search(text)
    byval = BYVAL_RE.search(text)
    sret = SRET_RE.search(text)
    al = ALIGN_RE.search(text)
    if byval:
        return [f"{ty} byval({tyname(byval.group(1))}) align {al.group(1) if al else '?'}"]
    if sret:
        return [f"{ty} sret({tyname(sret.group(1))}) align {al.group(1) if al else '?'}"]
    if ty.startswith("{"):
        return split_top(ty[1:-1].strip())          # coerced pair == two scalar params
    return [ty + (f" {ext.group(1)}" if ext else "")]


def norm_params(text):
    out = []
    for p in split_top(text):
        if p == "...":
            out.append("...")
        else:
            out.extend(norm_param(p))
    return out


def norm_ret(pre):
    m = RET_TYPE_RE.search(pre)
    ty = m.group(0) if m else "?"
    ext = EXT_RE.search(pre)
    return ty + (f" {ext.group(1)}" if ext else "")


@dataclass
class Sig:
    ret: str
    params: list
    raw: str

    def show(self):
        return f"{self.ret} ({', '.join(self.params)})"


@dataclass
class IrFn:
    name: str
    define: str
    body: list


def parse_ir(text):
    fns, cur = {}, None
    for line in text.splitlines():
        if line.startswith("define "):
            m = DEF_NAME_RE.search(line)
            if not m:
                continue
            name = m.group(1) or m.group(2)
            cur = IrFn(name, line, [])
            fns[name] = cur
        elif cur is not None:
            if line.startswith("}"):
                cur = None
            else:
                cur.body.append(line)
    return fns


def sig_of_define(fn):
    line = fn.define
    m = DEF_NAME_RE.search(line)
    pre = line[len("define "):m.start()]
    params = balanced(line, m.end() - 1)
    return Sig(norm_ret(pre), norm_params(params), line.strip())


def sig_of_call(fn, callee):
    pat = re.compile(r'@"?(?:[^"@\s(]*\.)?' + re.escape(callee) + r'"?\(')
    for line in fn.body:
        if " call " not in f" {line.strip()} " and "call " not in line:
            continue
        m = pat.search(line)
        if not m:
            continue
        head = line[:m.start()]
        head = head[head.index("call ") + 5:] if "call " in head else head
        return Sig(norm_ret(head), norm_params(balanced(line, m.end() - 1)), line.strip())
    return None


# ---------------------------------------------------------------------------
# Comparison / reporting
# ---------------------------------------------------------------------------

def diff_sigs(a, b):
    if a.ret == b.ret and a.params == b.params:
        return None
    lines = []
    if a.ret != b.ret:
        lines.append(f"      return : clang={a.ret!r}  gala={b.ret!r}")
    if a.params != b.params:
        n = max(len(a.params), len(b.params))
        for i in range(n):
            x = a.params[i] if i < len(a.params) else "<missing>"
            y = b.params[i] if i < len(b.params) else "<missing>"
            if x != y:
                lines.append(f"      slot {i}  : clang={x!r}  gala={y!r}")
    return "\n".join(lines)


def structs_used(fn):
    seen = []
    for t in ([fn.ret] if fn.ret else []) + fn.params:
        for s in t.structs():
            if s not in seen:
                seen.append(s)
    return seen


def check_case(case, clang_ir, gala_ir, args):
    c_fns, g_fns = parse_ir(clang_ir), parse_ir(gala_ir)
    native = NATIVE_CC[args.target]
    failures, checked = [], 0

    def gname(n):
        return args.gala_prefix + n

    def gfind(n):
        """exact symbol, else a unique module-mangled one (gala_123._main.<n>)"""
        if gname(n) in g_fns:
            return g_fns[gname(n)]
        hits = [f for k, f in g_fns.items() if k.endswith("." + gname(n))]
        return hits[0] if len(hits) == 1 else None

    mangled = sum(1 for k in g_fns if "." in k and k.rsplit(".", 1)[-1].startswith(("func", "call")))
    if mangled:
        print(f"  note: {mangled} Gala symbols are module-mangled (e.g. {next(k for k in g_fns if '.' in k)!r}); "
              f"matching by suffix. `export fn` is not taking effect.", file=sys.stderr)

    for fn in case.fns:
        problems = []

        for what, sym in (("define", fn.name), ("define", fn.wrapper)):
            c, g = c_fns.get(sym), gfind(sym)
            if c is None:
                problems.append(f"  [{sym}] missing from clang output (generator bug?)")
                continue
            if g is None:
                problems.append(f"  [{sym}] missing from Gala output (looked for {gname(sym)!r}; "
                                f"Gala defines: {sorted(g_fns)[:6]}...)")
                continue
            cs, gs = sig_of_define(c), sig_of_define(g)
            gs.raw = gs.raw.replace(native + " ", "")
            checked += 1
            d = diff_sigs(cs, gs)
            if d:
                problems.append(f"  [{sym}] define differs\n{d}\n"
                                f"      clang: {cs.raw}\n      gala : {gs.raw}")

        c, g = c_fns.get(fn.wrapper), gfind(fn.wrapper)
        if c and g:
            cs = sig_of_call(c, fn.name)
            gs = sig_of_call(g, gname(fn.name))
            if cs is None or gs is None:
                problems.append(f"  [{fn.wrapper}] call to {fn.name} not found "
                                f"(clang={'yes' if cs else 'no'}, gala={'yes' if gs else 'no'})")
            else:
                checked += 1
                d = diff_sigs(cs, gs)
                if d:
                    problems.append(f"  [{fn.wrapper}] call site differs\n{d}\n"
                                    f"      clang: {cs.raw}\n      gala : {gs.raw}")

        if problems:
            header = f"FAIL {fn.name}\n  C   : {c_sig(fn, fn.name)}\n  Gala: {gala_sig(fn, fn.name, export=True)}"
            layouts = "\n".join(f"  layout {s.layout_str()}" for s in structs_used(fn))
            failures.append("\n".join([header, *problems, layouts]))
    return failures, checked


# ---------------------------------------------------------------------------
# Self test of the normaliser
# ---------------------------------------------------------------------------

def selftest():
    pairs = [
        # (clang, gala)
        ("define dso_local void @f(ptr dead_on_unwind noalias writable sret(%struct.S0) align 4 %agg.result, i64 %p0.coerce0, i64 %p0.coerce1) #0 {",
         "define void @f(ptr sret(%S0) align 4 %0, { i64, i64 } %1) {"),
        ("define dso_local zeroext i1 @g(i1 noundef zeroext %a, i8 noundef signext %b, ptr noundef byval(%struct.S1) align 8 %c) #0 {",
         "define zeroext i1 @g(i1 zeroext %a, i8 signext %b, ptr byval(%S1) align 8 %c) {"),
        ("define dso_local <2 x float> @h(double %x.coerce) #0 {",
         "define <2 x float> @h(double %x) {"),
    ]
    ok = True
    for c, g in pairs:
        name = DEF_NAME_RE.search(c).group(2)
        cs = sig_of_define(IrFn(name, c, []))
        gs = sig_of_define(IrFn(name, g, []))
        if cs.ret != gs.ret or cs.params != gs.params:
            ok = False
            print("selftest FAIL:", cs.show(), "!=", gs.show())
    c_call = IrFn("w", "", ["  %call = call void @f(ptr dead_on_unwind writable sret(%struct.S0) align 4 %tmp, i64 noundef %1, i64 noundef %2)"])
    g_call = IrFn("w", "", ["  call void @f(ptr sret(%S0) align 4 %t, { i64, i64 } %v)"])
    a, b = sig_of_call(c_call, "f"), sig_of_call(g_call, "f")
    if a.ret != b.ret or a.params != b.params:
        ok = False
        print("selftest FAIL (call):", a.show(), "!=", b.show())
    # a real mismatch must be detected
    x = sig_of_define(IrFn("f", "define void @f(i64 %a, i64 %b)", []))
    y = sig_of_define(IrFn("f", "define void @f(ptr byval(%S0) align 8 %a)", []))
    if diff_sigs(x, y) is None:
        ok = False
        print("selftest FAIL: mismatch not detected")
    print("selftest", "ok" if ok else "FAILED")
    return 0 if ok else 1


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    ap = argparse.ArgumentParser(description="Differential ABI fuzzer: clang vs galac LLVM output")
    ap.add_argument("--target", choices=list(TRIPLES), default="linux")
    ap.add_argument("--cases", type=int, default=20, help="random cases (default 20)")
    ap.add_argument("--funcs", type=int, default=40, help="functions per random case")
    ap.add_argument("--structs", type=int, default=10, help="structs per random case")
    ap.add_argument("--seed", type=int, default=None, help="base seed; case i uses seed+i")
    ap.add_argument("--max-fields", type=int, default=6)
    ap.add_argument("--max-params", type=int, default=10)
    ap.add_argument("--max-size", type=int, default=48, help="max struct size in bytes")
    ap.add_argument("--scalars", default=",".join(DEFAULT_SCALARS),
                    help=f"comma list of scalar keys, from: {','.join(SCALARS)}")
    ap.add_argument("--no-classic", action="store_true", help="skip the hand-picked classic case")
    ap.add_argument("--clang", default="clang")
    ap.add_argument("--galac", default="galac")
    ap.add_argument("--galac-args", default="", help="extra args for galac, e.g. a target flag")
    ap.add_argument("--gala-prefix", default="", help="symbol prefix Gala puts on names")
    ap.add_argument("--gala-prelude", default=None, help="file whose text is put at the top of main.gala")
    ap.add_argument("--gala-main", default="fn main() {\\n}",
                    help='entry point appended to main.gala ("none" to omit)')
    ap.add_argument("--positional", action="store_true", help="positional struct literals")
    ap.add_argument("--no-galac", action="store_true", help="only generate and validate the C side")
    ap.add_argument("--out", default="abi_fuzz_out", help="where failing cases are saved")
    ap.add_argument("--keep-all", action="store_true", help="save every case, not just failures")
    ap.add_argument("--selftest", action="store_true", help="test the IR normaliser and exit")
    args = ap.parse_args()

    if args.selftest:
        sys.exit(selftest())

    scalars = [s.strip() for s in args.scalars.split(",") if s.strip()]
    for s in scalars:
        if s not in SCALARS:
            sys.exit(f"unknown scalar {s!r}")
    base = args.seed if args.seed is not None else random.randrange(1 << 30)
    print(f"target={args.target} ({TRIPLES[args.target]})  base seed={base}")

    cases = []
    if not args.no_classic:
        cases.append(make_classic_case(scalars))
    cases += [make_random_case(base + i, args, scalars) for i in range(args.cases)]

    total_checked = total_fail = 0
    out_root = Path(args.out)
    for case in cases:
        with tempfile.TemporaryDirectory(prefix="abi_fuzz_") as tmp:
            tmp = Path(tmp)
            cfile = tmp / "test.c"
            cfile.write_text(emit_c(case))
            (tmp / "main.gala").write_text(emit_gala(case, args))

            clang_ir = compile_c(args, cfile)
            if clang_ir is None:
                save(out_root / case.label, tmp, None, None, "clang failed")
                sys.exit(1)
            if args.no_galac:
                print(f"{case.label:>14}: C side ok ({len(case.fns)} functions, {len(case.structs)} structs)")
                if args.keep_all:
                    save(out_root / case.label, tmp, clang_ir, None, "")
                continue

            gala_ir = compile_gala(args, tmp)
            if gala_ir is None:
                save(out_root / case.label, tmp, clang_ir, None, "galac failed")
                sys.exit(1)

            failures, checked = check_case(case, clang_ir, gala_ir, args)
            total_checked += checked
            total_fail += len(failures)
            status = "ok" if not failures else f"{len(failures)} FAILING functions"
            print(f"{case.label:>14}: {len(case.fns)} functions, {checked} comparisons, {status}")
            if failures or args.keep_all:
                report = "\n\n".join(failures)
                save(out_root / case.label, tmp, clang_ir, gala_ir, report)
                for f in failures[:5]:
                    print(f"\n{f}")
                if len(failures) > 5:
                    print(f"\n... {len(failures) - 5} more in {out_root / case.label / 'report.txt'}")
                print()

    if args.no_galac:
        return 0
    print(f"\n{total_checked} comparisons, {total_fail} failing functions")
    if total_fail:
        print(f"details saved under {out_root}/")
    return 1 if total_fail else 0


def save(dest, tmp, clang_ir, gala_ir, report):
    dest.mkdir(parents=True, exist_ok=True)
    for name in ("test.c", "main.gala"):
        (dest / name).write_text((tmp / name).read_text())
    if clang_ir is not None:
        (dest / "clang.ll").write_text(clang_ir)
    if gala_ir is not None:
        (dest / "gala.ll").write_text(gala_ir)
    (dest / "report.txt").write_text(report)


if __name__ == "__main__":
    sys.exit(main())
