# PDB Decompiler

* [About](#about)
* [Usage](#usage)
* [TODO](#todo)

## About

A tool to decompile MSVC PDB files to C++ source code.

This tool is a work in progress and will most likely crash, have duplicate output, have invalid output, or miss output entirely. Feel free to file an issue or submit a pull request. Any assistance or valid criticism would be appreciated.

## Usage

```
cargo run --release -- --out=/path/to/out/ --pdb=/path/to/file.pdb --base-address=0x180000000 > file.pdb.log
```

| Short | Long | Description |
|-|-|-|
| `-o` | `--out` | The output directory to dump all C++ code to. |
| `-p` | `--pdb` | The file path to the MSVC PDB file to decompile. |
| `-b` | `--base-address` | The base address to add when resolving an RVA (optional). |

## TODO:

* Construct inline unions and structures based on field offsets (ouch)
* Propogate member method parameter names from `.cpp` files over into their corresponding class declarations in `.h/.hpp/.inl` files.
* Differentiate public vs private module members, determine when to add a declaration to a `.h/.hpp` or when to use static in `.c/.cpp`.
* Find the corresponding source files of toplevel user datatype symbols (`S_UDT`).
* Determine when to include constant declarations in source files.
* Figure out the format of float constants (?)
* Build namespace blocks where necessary (`.h/.hpp` files)
* Build a better C++ AST or improve the current implementation to a more workable point
* Handle any TODOs listed in source code
* Clean up everything
