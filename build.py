#!/usr/bin/env python3
"""Build galac, then compile and run gala programs.

  ./build.py                 build galac, compile main.gala, run it
  ./build.py build           build galac, compile main.gala
  ./build.py memcheck        like run, but under valgrind
  ./build.py test [filter]   run every tests/test_*.gala, then every negative case
  ./build.py neg [filter]    only the negative (must-fail) cases
  --no-build                 skip the `odin build` step

Negative cases live in tests/neg_*.txt, many programs per file:

  //### case_name | text the compiler error must contain (optional)
  <gala program>

Each one must fail to compile (and print that text, if given).
"""
import argparse
import glob
import os
import shutil
import subprocess
import sys

GALAC = "./galac"
MAIN_SRC = "main.gala"
PROGRAM = "program"
TEST_DIR = "tests"
TEST_BIN = os.path.join(TEST_DIR, "main")  # "main" is gitignored
NEG_SRC = os.path.join(TEST_DIR, ".neg_case.gala")  # written next to the tests so imports resolve the same way
TEST_TIMEOUT = 10  # seconds; catches infinite loops
NEG_TIMEOUT = 20   # seconds; catches a compiler that never terminates

ENV = os.environ.copy()
ENV["LD_LIBRARY_PATH"] = ".:/" + (":" + ENV["LD_LIBRARY_PATH"] if "LD_LIBRARY_PATH" in ENV else "")


def paint(text, code):
    return f"\033[{code}m{text}\033[0m" if sys.stdout.isatty() else text


def green(t): return paint(t, 32)
def red(t): return paint(t, 31)
def dim(t): return paint(t, 2)


def indent(text, prefix="    "):
    return "\n".join(prefix + line for line in text.rstrip().splitlines())


def as_text(x):
    if x is None:
        return ""
    return x.decode(errors="replace") if isinstance(x, bytes) else x


def run_captured(cmd, timeout=None):
    """Run cmd; return (returncode, stdout+stderr). returncode is None on timeout."""
    try:
        p = subprocess.run(
            cmd,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
            env=ENV,
            timeout=timeout,
        )
    except subprocess.TimeoutExpired as e:
        return None, as_text(e.stdout)
    return p.returncode, p.stdout


def build_galac():
    code, out = run_captured(["odin", "build", ".", "-out=galac", "-debug"])
    if out.strip():
        print(out.rstrip())
    print("odin returned", code)
    return code == 0


def compile_gala(src, out, timeout=None):
    """Compile src to out. Removes a stale binary first so a failed compile can't run old code."""
    if os.path.exists(out):
        os.remove(out)
    return run_captured([GALAC, "-o", out, src, "--abi", "win64"], timeout=timeout)


# ---------- run / memcheck ----------

def run_program():
    print("running.", flush=True)
    cmd = ["stdbuf", "-oL"] if shutil.which("stdbuf") else []
    p = subprocess.Popen(
        cmd + ["./" + PROGRAM],
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
        bufsize=1,
        env=ENV,
    )
    for line in p.stdout:
        print(line, end="", flush=True)
    p.stdout.close()
    result = p.wait()
    print(f"\nprogram returned {result}")
    return result


def memcheck():
    result = subprocess.run(
        ["valgrind", "--leak-check=full", "./" + PROGRAM], env=ENV
    ).returncode
    print("memcheck returned", result)
    return result


# ---------- positive tests ----------

def judge(code, output):
    """Return (passed, reason). The test programs exit 0 even when checks fail,
    so the output decides: no `FAIL:` lines and an `all passed` line."""
    if code is None:
        return False, f"timed out after {TEST_TIMEOUT}s"
    if code != 0:
        return False, f"exited with code {code}"
    lines = output.splitlines()
    if any(line.startswith("FAIL:") for line in lines):
        return False, "checks failed"
    if not any("all passed" in line for line in lines):
        return False, "no 'all passed' line (crashed early?)"
    return True, ""


def run_positive(name_filter):
    """Returns (number run, names that failed)."""
    files = sorted(glob.glob(os.path.join(TEST_DIR, "test_*.gala")))
    if name_filter:
        files = [f for f in files if name_filter in os.path.basename(f)]

    failed = []
    for src in files:
        name = os.path.basename(src)

        code, output = compile_gala(src, TEST_BIN)
        if code != 0:
            print(f"{red('FAIL')} {name}: compile error (code {code})")
            print(indent(output))
            failed.append(name)
            continue

        code, output = run_captured([TEST_BIN], timeout=TEST_TIMEOUT)
        passed, reason = judge(code, output)
        if passed:
            print(f"{green('ok')}   {name}")
            continue

        print(f"{red('FAIL')} {name}: {reason}")
        fails = [l for l in output.splitlines() if l.startswith("FAIL:")]
        print(indent("\n".join(fails) if fails else output))
        failed.append(name)
    return len(files), failed


# ---------- negative tests ----------

def load_neg_cases():
    """Split tests/neg_*.txt into [{name, expect, source}]."""
    cases = []
    for path in sorted(glob.glob(os.path.join(TEST_DIR, "neg_*.txt"))):
        group = os.path.basename(path)[len("neg_"):-len(".txt")]
        cur = None
        with open(path) as f:
            for line in f.read().splitlines():
                if line.startswith("//### "):
                    name, _, expect = line[len("//### "):].partition("|")
                    cur = {"name": f"{group}/{name.strip()}", "expect": expect.strip(), "lines": []}
                    cases.append(cur)
                elif cur is not None:
                    cur["lines"].append(line)
    for c in cases:
        c["source"] = "\n".join(c["lines"]) + "\n"
    return cases


def judge_neg(code, output, expect):
    """Return (passed, reason). The compiler must reject the program."""
    if code is None:
        return False, f"compiler hung (no exit after {NEG_TIMEOUT}s)"
    if code == 0:
        return False, "compiled, but should have failed"
    # a signal (negative code) with no diagnostic printed means a compiler crash,
    # not a clean rejection
    if code < 0 and not output.strip():
        return False, f"compiler crashed (signal {-code}) without an error message"
    if expect and expect.lower() not in output.lower():
        return False, f"wrong error (wanted \"{expect}\")"
    return True, ""


def run_negative(name_filter):
    """Returns (number run, names that failed)."""
    cases = load_neg_cases()
    if name_filter:
        cases = [c for c in cases if name_filter in c["name"]]

    failed = []
    for c in cases:
        with open(NEG_SRC, "w") as f:
            f.write(c["source"])
        try:
            code, output = compile_gala(NEG_SRC, TEST_BIN, timeout=NEG_TIMEOUT)
        finally:
            os.remove(NEG_SRC)

        passed, reason = judge_neg(code, output, c["expect"])
        if passed:
            print(f"{green('ok')}   {c['name']} {dim('(rejected)')}")
            continue

        print(f"{red('FAIL')} {c['name']}: {reason}")
        tail = "\n".join(output.rstrip().splitlines()[-4:])
        if tail:
            print(indent(tail))
        failed.append(c["name"])
    return len(cases), failed


def run_tests(name_filter, positive=True, negative=True):
    total, failed = 0, []
    if positive:
        n, f = run_positive(name_filter)
        total += n
        failed += f
    if negative:
        if positive:
            print(dim("-- negative cases --"))
        n, f = run_negative(name_filter)
        total += n
        failed += f

    if total == 0:
        print("no tests found")
        return False
    print()
    print(f"{total - len(failed)}/{total} passed")
    return not failed


# ---------- main ----------

def main():
    ap = argparse.ArgumentParser(description="Build and run gala programs.")
    ap.add_argument("command", nargs="?", default="run",
                    choices=["run", "build", "memcheck", "test", "neg"])
    ap.add_argument("filter", nargs="?", default="",
                    help="for `test`/`neg`: only run cases whose name contains this")
    ap.add_argument("--no-build", action="store_true",
                    help="skip the `odin build` step")
    args = ap.parse_args()

    if not args.no_build and not build_galac():
        sys.exit(1)

    if args.command == "test":
        sys.exit(0 if run_tests(args.filter) else 1)
    if args.command == "neg":
        sys.exit(0 if run_tests(args.filter, positive=False) else 1)

    code, output = compile_gala(MAIN_SRC, PROGRAM)
    if output.strip():
        print(output.rstrip())
    print("compiler returned", code)
    if code != 0:
        sys.exit(code)

    if args.command == "build":
        return

    try:
        result = memcheck() if args.command == "memcheck" else run_program()
    except KeyboardInterrupt:
        print("^C - canceling run.")
        return
    if result != 0:
        sys.exit(result)


if __name__ == "__main__":
    main()
