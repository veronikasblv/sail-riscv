Symbolic Test Generator for RISC-V
==================================

This repository is a fork of [sail-riscv](README_sail-riscv.md) and is intended for generating tests based on [specifications](https://riscv-isac.readthedocs.io/en/stable/cgf.html) using symbolic execution.

Structure
---------

The test generator consists of the following core files:

```
sail-riscv
- generator.py            // CLI wrapper
- c_emulator               
  - cgf_parsing            
    - parsing.py          // сonverts CGF specifications into C code suitable for symbolic execution
```

Dependencies
------------

Required components:

- Python 3.8
- [Sail](https://github.com/veronikasblv/sail/) (modified version)
- [RISC-V ISAC](https://riscv-isac.readthedocs.io/en/stable/)
- [KLEEF](https://github.com/UnitTestBot/klee)
- The following environment variables must be defined:

```bash
export LLVM_COMPILER=clang
export CC=gcc
export CXX=wllvm++
export KLEE_BUILD_DIR=<path/to/klee_build/lib>
export LIB_KLEE=<path/to/kleef/include>
```

Usage
-----

Executing `python3 generator.py --help` should print the following on the terminal:

```
usage: generator.py [-h] {model,test} ...

Symbolic Test Generator for RISC-V

positional arguments:
  {model,test}
    model       Build Sail RISC-V model for CGF-file
    test        Generate tests for model with KLEEF

options:
  -h, --help    show this help message and exit
```

The generator has two commands: `model` and `test`. Help text for each command can be accessed by executing `python3 generator.py <command> --help`.