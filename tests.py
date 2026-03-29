import os
import subprocess

TEST_DIR = "tests"

files = [
    f for f in os.listdir(TEST_DIR)
    if os.path.isfile(os.path.join(TEST_DIR, f)) and f.endswith(".gala")
]

if not files:
    print("No .gala test files found.")
    exit(0)

fails = [];
for f in sorted(files):
    path = os.path.join(TEST_DIR, f)
    print(f"\n=== Running {f} ===")

    result = subprocess.run(
        ["./uq", path],
        # capture_output=True,
        # text=True,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )

    # print(result.stdout)

    if result.stderr:
        print("ERROR:")
        print(result.stderr)

    if result.returncode != 0:
        print(f"❌ {f} failed (exit code {result.returncode})")
        fails.append(path);
    else:
        print(f"✅ {f} passed")

print(f"Out of {len(files)}, there were {len(fails)} fails.");
if len(fails) > 0:
    if input("Show failed tests? [y/n] ") in ["y" ""]:
        for f in fails: print(f);
