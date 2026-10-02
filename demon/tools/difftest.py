#!/usr/bin/env python3
"""Diffs the OCaml port (tools/trace.exe) against sample/maxwell on random
move strings for every level, in compact and --dump modes."""
import glob, os, random, subprocess, sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
REF = os.path.join(ROOT, "..", "sample", "maxwell")
PORT = os.path.join(ROOT, "_build", "default", "tools", "trace.exe")
levels = sorted(glob.glob(os.path.join(ROOT, "assets", "levels", "*.txt")))
rng = random.Random(int(sys.argv[1]) if len(sys.argv) > 1 else 1)
runs = int(sys.argv[2]) if len(sys.argv) > 2 else 20
fails = total = 0
for lv in levels:
    for k in range(runs):
        n = rng.choice([0, 5, 20, 60, 150])
        # biased walks explore further than uniform ones
        moves = "".join(rng.choice("wasd" * 2 + rng.choice("wasd") * 3) for _ in range(n))
        for mode in ([], ["--dump"]):
            total += 1
            a = subprocess.run([REF, *mode, lv, moves], capture_output=True, text=True).stdout
            b = subprocess.run([PORT, *mode, lv, moves], capture_output=True, text=True).stdout
            if a != b:
                fails += 1
                if fails <= 3:
                    print("MISMATCH", os.path.basename(lv), mode, moves)
print(f"{total - fails}/{total} traces identical")
sys.exit(1 if fails else 0)
