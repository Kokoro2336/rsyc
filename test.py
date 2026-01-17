import argparse
import os
import subprocess
import sys

# Default values
DEFAULT_COMPILER = "./target/debug/sysy_compiler"
DEFAULT_MODE = "koopa"
DEFAULT_LEVEL = "1"
DEFAULT_TEST = "0_main"

def get_paths(args):
    compiler = args.compiler
    mode = args.mode
    level = args.level
    test = args.test
    
    source_file = f"./testcases/lv{level}/{test}.c"
    output_base = f"./testcases/lv{level}/{test}"
    
    return compiler, mode, source_file, output_base

def run_gdbserver(args):
    compiler, mode, source_file, output_base = get_paths(args)
    cmd = [
        "gdbserver", ":1234",
        compiler,
        f"-{mode}",
        source_file,
        "-o", f"{output_base}.out"
    ]
    print(f"Running: {' '.join(cmd)}")
    subprocess.run(cmd)

def run_gdb(args):
    compiler, mode, source_file, output_base = get_paths(args)
    cmd = [
        "rust-gdb", "--args",
        compiler,
        f"-{mode}",
        source_file,
        "-o", f"{output_base}.out"
    ]
    print(f"Running: {' '.join(cmd)}")
    subprocess.run(cmd)

def run_irgen(args):
    compiler, _, source_file, output_base = get_paths(args)
    # irgen forces koopa mode
    cmd = [
        compiler,
        "-koopa",
        source_file,
        "-o", f"{output_base}.ir"
    ]
    print(f"Running: {' '.join(cmd)}")
    subprocess.run(cmd)

def run_asmgen(args):
    compiler, _, source_file, output_base = get_paths(args)
    # asmgen forces riscv mode
    cmd = [
        compiler,
        "-riscv",
        source_file,
        "-o", f"{output_base}.out"
    ]
    print(f"Running: {' '.join(cmd)}")
    subprocess.run(cmd)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Test script ported from debug.sh")
    
    parser.add_argument("command", choices=["gdbserver", "gdb", "irgen", "asmgen"], help="Command to run")
    
    parser.add_argument("--compiler", default=os.environ.get("COMPILER", DEFAULT_COMPILER), help="Path to compiler")
    parser.add_argument("--mode", default=os.environ.get("MODE", DEFAULT_MODE), help="Compiler mode")
    parser.add_argument("--level", default=os.environ.get("LEVEL", DEFAULT_LEVEL), help="Test level")
    parser.add_argument("--test", default=os.environ.get("TEST", DEFAULT_TEST), help="Test name")

    args = parser.parse_args()

    # build the project before running any command
    print("Building the project...")
    subprocess.run(["cargo", "build"], check=True)

    if args.command == "gdbserver":
        run_gdbserver(args)
    elif args.command == "gdb":
        run_gdb(args)
    elif args.command == "irgen":
        run_irgen(args)
    elif args.command == "asmgen":
        run_asmgen(args)