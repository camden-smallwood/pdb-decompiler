# PDB Decompiler

* [About](#about)
* [Usage](#usage)
* [TODO](#todo)

## About

A tool to decompile MSVC PDB files to C++ source code.

This tool is a work in progress and will most likely crash, have duplicate output, have invalid output, or miss output entirely. Feel free to file an issue or submit a pull request. Any assistance or valid criticism would be appreciated.

## Usage

```
cargo run --release -- --out /path/to/out/ --pdb /path/to/file.pdb > file.pdb.log
```

## TODO:

- [ ] Construct inline unions and structures based on field offsets (ouch)
- [ ] Propogate member method parameter names from `.cpp` files over into their corresponding class declarations in `.h/.hpp/.inl` files.
- [ ] Differentiate public vs private module members, determine when to add a declaration to a `.h/.hpp` or when to use static in `.c/.cpp`.
- [ ] Find the corresponding source files of toplevel user datatype symbols (`S_UDT`).
- [ ] Determine when to include constant declarations.
- [ ] Figure out the format of float constants (?)
- [ ] Clean up (amount lots of other things...)
