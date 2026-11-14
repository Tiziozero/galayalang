#!/usr/bin/python3
"""Build galac, then compile and run gala programs.

  ./build.py                 build galac, compile main.gala, run it
  ./build.py build           build galac, compile main.gala
  ./build.py debug           build galac, compile main.gala with -d, run it
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

# ".exe" only on Windows, empty everywhere else.
EXE = ".exe" if os.name == "nt" else ""

GALAC = "./galac" + EXE
MAIN_SRC = "main.gala"
PROGRAM = "program" + EXE
TEST_DIR = "tests"
TEST_BIN = os.path.join(TEST_DIR, "main" + EXE)
NEG_SRC = os.path.join(TEST_DIR, ".neg_case.gala")

TEST_TIMEOUT = 10
NEG_TIMEOUT = 20

ABI = "win64" if os.name == "nt" else "sysv"

ENV = os.environ.copy()
if os.name != "nt":
    ENV["LD_LIBRARY_PATH"] = ".:/" + (
        ":" + ENV["LD_LIBRARY_PATH"] if "LD_LIBRARY_PATH" in ENV else ""
    )


class ToolError(Exception):
    """Something outside our control failed (missing tool, bad file, ...)."""


def paint(text, code):
    return f"\033[{code}m{text}\033[0m" if sys.stdout.isatty() else text


def green(t):
    return paint(t, 32)


def red(t):
    return paint(t, 31)


def dim(t):
    return paint(t, 2)


def indent(text, prefix="    "):
    return "\n".join(prefix + line for line in text.rstrip().splitlines())


def as_text(x):
    if x is None:
        return ""
    return x.decode(errors="replace") if isinstance(x, bytes) else x


def describe_os_error(what, e):
    """Turn an OSError into a short, readable message."""
    if isinstance(e, FileNotFoundError):
        return f"{what}: not found"
    if isinstance(e, PermissionError):
        return f"{what}: permission denied"
    return f"{what}: {e}"


def run_captured(cmd, timeout=None):
    """Run cmd; return (returncode, stdout+stderr). returncode is None on timeout.

    Raises ToolError if the command can't be started at all.
    """
    try:
        p = subprocess.run(
            cmd,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
            encoding="utf-8",
            errors="replace",
            env=ENV,
            timeout=timeout,
        )
    except subprocess.TimeoutExpired as e:
        return None, as_text(e.stdout)
    except OSError as e:
        raise ToolError(describe_os_error(f"can't run {cmd[0]}", e)) from e

    return p.returncode, p.stdout


def remove_file(path):
    """Delete path if it exists. Raises ToolError if it can't be deleted."""
    try:
        os.remove(path)
    except FileNotFoundError:
        pass
    except OSError as e:
        raise ToolError(describe_os_error(f"can't delete {path}", e)) from e


def build_galac():
    try:
        code, out = run_captured(
            ["odin", "build", ".", "-out=galac" + EXE, "-debug"]
        )
    except ToolError as e:
        print(red(f"error: {e}"))
        print("is odin installed and on your PATH?")
        return False

    if code is None:
        print(red("error: odin build timed out"))
        return False

    if out.strip():
        print(out.rstrip())

    print("odin returned", code)
    return code == 0


def compile_gala(src, out, timeout=None, debug=False):
    """Compile src to out. Removes stale binaries before compiling."""
    remove_file(out)

    cmd = [
        GALAC,
        "-o",
        out,
        src,
        "--abi",
        ABI,
    ]

    if debug:
        cmd.append("-d")

    return run_captured(cmd, timeout=timeout)


# ---------- run / memcheck ----------

def run_program():
    if not os.path.exists(PROGRAM):
        print(red(f"error: {PROGRAM} doesn't exist (did the compile produce it?)"))
        return 1

    print("running.", flush=True)

    cmd = ["stdbuf", "-oL"] if shutil.which("stdbuf") else []

    try:
        p = subprocess.Popen(
            cmd + ["./" + PROGRAM],
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
            encoding="utf-8",
            errors="replace",
            bufsize=1,
            env=ENV,
        )
    except OSError as e:
        print(red("error: " + describe_os_error("can't start " + PROGRAM, e)))
        return 1

    try:
        for line in p.stdout:
            print(line, end="", flush=True)

        p.stdout.close()
        result = p.wait()
    except KeyboardInterrupt:
        # Don't leave the child running behind us.
        p.terminate()
        try:
            p.wait(timeout=2)
        except subprocess.TimeoutExpired:
            p.kill()
            p.wait()
        raise

    print(f"\nprogram returned {result}")
    return result


def memcheck():
    if not os.path.exists(PROGRAM):
        print(red(f"error: {PROGRAM} doesn't exist (did the compile produce it?)"))
        return 1

    try:
        result = subprocess.run(
            ["valgrind", "--leak-check=full", "./" + PROGRAM],
            env=ENV,
        ).returncode
    except FileNotFoundError:
        print(red("error: valgrind not found (is it installed?)"))
        return 1
    except OSError as e:
        print(red("error: " + describe_os_error("can't run valgrind", e)))
        return 1

    print("memcheck returned", result)
    return result


# ---------- positive tests ----------

def judge(code, output):
    """Return (passed, reason)."""
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
        files = [
            f for f in files
            if name_filter in os.path.basename(f)
        ]

    failed = []

    for src in files:
        name = os.path.basename(src)

        code, output = compile_gala(src, TEST_BIN)

        if code != 0:
            print(f"{red('FAIL')} {name}: compile error (code {code})")
            print(indent(output))
            failed.append(name)
            continue

        if not os.path.exists(TEST_BIN):
            print(f"{red('FAIL')} {name}: compiler succeeded but made no binary")
            failed.append(name)
            continue

        try:
            code, output = run_captured([TEST_BIN], timeout=TEST_TIMEOUT)
        except ToolError as e:
            print(f"{red('FAIL')} {name}: {e}")
            failed.append(name)
            continue

        passed, reason = judge(code, output)

        if passed:
            print(f"{green('ok')}   {name}")
            continue

        print(f"{red('FAIL')} {name}: {reason}")

        fails = [
            line for line in output.splitlines()
            if line.startswith("FAIL:")
        ]

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

        try:
            with open(path, encoding="utf-8", errors="replace") as f:
                content = f.read()
        except OSError as e:
            print(red("warning: " + describe_os_error("skipping " + path, e)))
            continue

        for line in content.splitlines():
            if line.startswith("//### "):
                name, _, expect = line[len("//### "):].partition("|")

                cur = {
                    "name": f"{group}/{name.strip()}",
                    "expect": expect.strip(),
                    "lines": [],
                }

                cases.append(cur)

            elif cur is not None:
                cur["lines"].append(line)

    for case in cases:
        case["source"] = "\n".join(case["lines"]) + "\n"

    return cases


def judge_neg(code, output, expect):
    """Return (passed, reason). The compiler must reject the program."""
    if code is None:
        return False, f"compiler hung (no exit after {NEG_TIMEOUT}s)"

    if code == 0:
        return False, "compiled, but should have failed"

    if code < 0 and not output.strip():
        return False, f"compiler crashed (signal {-code}) without an error message"

    if expect and expect.lower() not in output.lower():
        return False, f"wrong error (wanted \"{expect}\")"

    return True, ""


def run_negative(name_filter):
    """Returns (number run, names that failed)."""
    cases = load_neg_cases()

    if name_filter:
        cases = [
            case for case in cases
            if name_filter in case["name"]
        ]

    failed = []

    for case in cases:
        try:
            with open(NEG_SRC, "w", encoding="utf-8") as f:
                f.write(case["source"])
        except OSError as e:
            raise ToolError(
                describe_os_error(f"can't write {NEG_SRC}", e)
            ) from e

        try:
            code, output = compile_gala(
                NEG_SRC,
                TEST_BIN,
                timeout=NEG_TIMEOUT,
            )
        finally:
            try:
                remove_file(NEG_SRC)
            except ToolError:
                pass

        passed, reason = judge_neg(code, output, case["expect"])

        if passed:
            print(f"{green('ok')}   {case['name']} {dim('(rejected)')}")
            continue

        print(f"{red('FAIL')} {case['name']}: {reason}")

        tail = "\n".join(output.rstrip().splitlines()[-4:])

        if tail:
            print(indent(tail))

        failed.append(case["name"])

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
    ap = argparse.ArgumentParser(
        description="Build and run gala programs."
    )

    ap.add_argument(
        "command",
        nargs="?",
        default="run",
        choices=["run", "build", "debug", "memcheck", "test", "neg"],
    )

    ap.add_argument(
        "filter",
        nargs="?",
        default="",
        help="for `test`/`neg`: only run cases whose name contains this",
    )

    ap.add_argument(
        "--no-build",
        action="store_true",
        help="skip the `odin build` step",
    )

    args = ap.parse_args()

    if not args.no_build and not build_galac():
        sys.exit(1)

    if args.command == "test":
        sys.exit(0 if run_tests(args.filter) else 1)

    if args.command == "neg":
        sys.exit(
            0 if run_tests(args.filter, positive=False) else 1
        )

    debug = args.command == "debug"

    code, output = compile_gala(
        MAIN_SRC,
        PROGRAM,
        debug=debug,
    )

    if output.strip():
        print(output.rstrip())

    print("compiler returned", code)

    if code is None or code != 0:
        sys.exit(code or 1)

    if args.command == "build":
        return

    try:
        result = (
            memcheck()
            if args.command == "memcheck"
            else run_program()
        )

    except KeyboardInterrupt:
        print("^C - canceling run.")
        return

    if result != 0:
        sys.exit(result)


if __name__ == "__main__":
    try:
        main()
    except ToolError as e:
        print(red(f"error: {e}"))
        if GALAC in str(e):
            print("hint: run without --no-build so galac gets built first")
        sys.exit(1)
    except KeyboardInterrupt:
        print("\n^C")
        sys.exit(130)
