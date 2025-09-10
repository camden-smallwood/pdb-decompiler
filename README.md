# PDB Decompiler

* [About](#about)
* [Usage](#usage)
* [Contributing](#contributing)

## About

A tool to decompile MSVC PDB files to C++ source code.

This tool is a work in progress and will most likely crash, have duplicate output, have invalid output, or miss output entirely. Feel free to file an issue or submit a pull request.

## Usage

```
cargo run --release -- --out=/path/to/out/ --pdb=/path/to/file.pdb --base-address=0x180000000 > file.pdb.log
```

### Flags

| Short | Long | Description |
|-|-|-|
| `-h` | `--help` | Prints help information. |
| `-r` | `--reorganize` | Whether to reorganize generated C++ code to Bungie's coding standards. (Experimental) |
| `-V` | `--version` | Prints version information. |
| `-u` | `--unroll-functions` | Whether to include scope information in decompiled function stubs. (Experimental) |
| `-f` | `--function-scopes-pdb` | The file path to the MSVC PDB file to decompile for extra function scope information. |

### Options

| Short | Long | Description |
|-|-|-|
| `-b` | `--base-address <base-address>` | The base address to add when resolving an RVA. (Optional) |
| `-o` | `--out <out>` | The output directory to dump all C++ code to. |
| `-p` | `--pdb <pdb>` | The file path to the MSVC PDB file to decompile. |

## Contributing

Any assistance or valid criticism would be appreciated. Please feel free to have a look at some of the open [issues](https://github.com/camden-smallwood/pdb-decompiler/issues), especially those tagged with [help wanted](https://github.com/camden-smallwood/pdb-decompiler/issues?q=is%3Aissue+is%3Aopen+label%3A"help+wanted").
