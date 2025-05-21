import subprocess
import argparse

def run_sail(args):
    command = f"ARCH=RV32 SAIL_DIR={args.sail_dir} CGF_FILE='{' '.join(args.cgf)}' make"
    stdout = subprocess.DEVNULL
    stderr = subprocess.DEVNULL

    if args.verbose:
        stdout = None
        stderr = None

    print(f"\033[32m[INFO] Building model for {args.cgf}\033[0m")
    result = subprocess.run(command, stdout=stdout, stderr=stderr, shell=True)
    if result.returncode != 0:
        print(f"\033[91m[ERROR] Failed with exit code {result.returncode}. Re-run with --verbose for more details\033[0m")

def run_klee(args):
    command_klee = f"klee --libc=uclibc --check-overshift=false --silent-klee-assume --only-output-states-covering-new --dump-states-on-halt=none \
                --uninit-memory-test-multiplier=0 --max-time={args.max_time} --max-forks={args.max_forks} --output-dir={args.output_dir} c_emulator/riscv_sim_RV32.bc"
    command_rm = "rm -f *.gcov c_emulator/*.gcda"
    command_replay = f'for file in {args.output_dir}/*.ktest; do export KTEST_FILE="$(realpath "$file")" && klee-replay c_emulator/riscv_sim_RV32 $file; done'
    command_gcov = "gcov c_emulator/riscv_sim_RV32-riscv_model_RV32"

    stdout = subprocess.DEVNULL
    stderr = subprocess.DEVNULL

    if args.verbose:
        stdout = None
        stderr = None

    print(f"\033[32m[INFO] Generating tests with KLEEF\033[0m")
    result = subprocess.run(command_klee, stdout=stdout, stderr=stderr, shell=True)
    if result.returncode != 0:
        print(f"\033[91m[ERROR] Failed with exit code {result.returncode}. Re-run with --verbose for more details\033[0m")

    print(f"\033[32m[INFO] Replaying tests on the model\033[0m")
    subprocess.run(command_rm, stdout=stdout, stderr=stderr, shell=True)
    subprocess.run(command_replay, stdout=stdout, stderr=stderr, shell=True)
    print(f"\033[32m[INFO] Computing test coverage\033[0m")
    subprocess.run(command_gcov, shell=True)


def main():
    parser = argparse.ArgumentParser(description="Symbolic Test Generator for RISC-V")
    subparsers = parser.add_subparsers(required=True)

    sail_parser = subparsers.add_parser("model", help="Build Sail RISC-V model for CGF-file")
    sail_parser.add_argument('--cgf',  nargs='+', required=True, help="path to CGF Specification")
    sail_parser.add_argument('--sail-dir',  type=str, required=True, help="path to Sail dir")
    sail_parser.add_argument('--verbose', action='store_true', default=False, help="show command output")
    sail_parser.set_defaults(func=run_sail)

    klee_parser = subparsers.add_parser("test", help="Generate tests for model with KLEEF")
    klee_parser.add_argument('--max-forks',  type=int, default=300, help="forks limit for KLEEF (default=300)") 
    klee_parser.add_argument('--max-time',  type=str, default='30min', help="time limit for KLEEF (default=30min)")  
    klee_parser.add_argument('--output-dir',  type=str, required=True, help="directory for test output")
    klee_parser.add_argument('--verbose', action='store_true', default=False, help="show command output")
    klee_parser.set_defaults(func=run_klee)

    args = parser.parse_args()
    args.func(args)

if __name__ == "__main__":
    main()