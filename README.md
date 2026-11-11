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

### Windows

The tool follows the standard Windows command-line quoting rules, so quoted
paths are fine. The one thing to avoid is a backslash immediately before a
closing double quote: `\"` is treated as a literal quote rather than the end of
the quoted string, which breaks argument parsing (typically reported as
`PDB path not provided`).

```
:: OK
pdb-decompiler.exe --out=F:\SINS_TEST --pdb=F:\SINS\Sins.pdb --base-address=0x180000000

:: OK (quoted)
pdb-decompiler.exe --out="F:\SINS_TEST" --pdb="F:\SINS\Sins.pdb" --base-address=0x180000000

:: WRONG - the trailing \" escapes the closing quote and swallows the next flag
pdb-decompiler.exe --out="F:\SINS_TEST\" --pdb="F:\SINS\Sins.pdb" --base-address=0x180000000
```

### Flags

| Short | Long | Description |
|-|-|-|
| `-c` | `--export-cpp` | Whether to export C++ code. |
| | `--export-function-types` | Whether to generate IDA script statements that try to set function types. |
| | `--export-pseudocode-to-files` | Whether to generate IDA script statements that export pseudocode to their appropriate source files. |
| | `--export-pseudocode-to-json` | Whether to generate IDA script statements that export pseudocode to a JSON mapping file. |
| | `--force-no-inlines` | Forces a `__declspec(noinline)` attribute for functions that were never inlined but did not have the attribute. |
| `-h` | `--help` | Prints help information. |
| `-r` | `--reorganize` | Whether to reorganize generated C++ code to Bungie's coding standards. (Experimental) |
| `-u` | `--unroll-functions` | Whether to include scope information in decompiled function stubs. (Experimental) |
| | `--verbose-blocks` | Whether to write extra block level information. |
| `-V` | `--version` | Prints version information. |

### Options

| Short | Long | Description |
|-|-|-|
| `-b` | `--base-address <base-address>` | The base address to add when resolving an RVA. (Optional) |
| | `--export-debug-blocks-json <export-debug-blocks-json>` | Dumps each procedure's unrolled scope tree (variable declarations in nested blocks) to a JSON file keyed by mangled function name. (Forces unrolling.) |
| | `--export-objects-json <export-objects-json>` | Writes a JSON manifest of the objects the image was linked from, in link order (see [Object manifest](#object-manifest)). |
| | `--function-scopes-out <function-scopes-out>` | The output directory to dump all function scopes C++ code to. |
| | `--function-scopes-pdb <function-scopes-pdb>` | The file path to the MSVC PDB file to decompile for extra function scope information. |
| `-o` | `--out <out>` | The output directory to dump all C++ code to. |
| `-p` | `--pdb <pdb>` | The file path to the MSVC PDB file to decompile. |
| | `--pseudocode-json-path <pseudocode-json-path>` | The file containing all function pseudocode in a JSON mapping. (Optional) |
| | `--project-name` | The file and folder name for used to generate the umbrella include header. The emitted include is `{project_name}/{project_name}.h` (e.g. `project/project.h`). |
| | `--include-compiler-generated` | Includes compiler-generated procedure definitions (implicit ctors/dtors/assignment, thunks, dynamic initializers) in output C++ code |
| | `--sdk-include <sdk-include>` | An SDK include directory. For PDBs without IPI, types and typedefs its headers define are included from the SDK instead of reconstructed (see [SDK headers](#sdk-headers)). Can be specified multiple times. |
| | `--source-path <source-path>` | A source file path (or directory) to filter which modules are decompiled. Only modules whose source path is equal to, or nested under, one of these paths are exported. Can be specified multiple times. For a directory path, output is rebased so the last folder becomes the output root (e.g. `--source-path .../FolderA/SubFolderA` writes `.../FolderA/SubFolderA/foo.cpp` to `<out>/SubFolderA/foo.cpp`). |

## Object manifest

`--export-objects-json <file>` writes the objects the image was linked from, in the
order the DBI lists them, which is the link order. For each object:

- `object` and `library`: the object file, and the library it came from, if any;
- `source`: the source file it was compiled from, when the PDB identifies one;
- `compiler`: the compiler's name and front-end and back-end versions;
- `build`: the build record (`cwd`, `cl`, `src`, `pdb`, `cmd`), from `S_BUILDINFO`,
  `S_COMPILE2` or `S_ENVBLOCK`;
- `ranges`: the address ranges it contributed, by section. A range is a run of
  section contributions with no other object's contribution inside it;
- `functions`: in address order, with the demangled `name`, decorated `symbol`
  (when there is a public symbol), `address`, `size`, and the `file` and `line` of
  its first line record. `kept_by` is set when the linker kept another object's
  identical copy, and is that object's index;
- `data`: its global and static data, with addresses.

Addresses include `--base-address`.

## Contributing

Any assistance or valid criticism would be appreciated. Please feel free to have a look at some of the open [issues](https://github.com/camden-smallwood/pdb-decompiler/issues), especially those tagged with [help wanted](https://github.com/camden-smallwood/pdb-decompiler/issues?q=is%3Aissue+is%3Aopen+label%3A"help+wanted").

## Legacy MSVC PDBs (without IPI)

Pre-IPI PDBs are detected from the PDB information stream's feature signatures.
Stream 4 can contain `/names` in these files; it must not be parsed as IPI.
The legacy path is selected by file capabilities, not compiler brand or target
architecture. It covers desktop MSVC as well as Xbox 360 XDK PDBs and recovers:

- Class, union, enum, and function types from TPI. Legacy PDBs record no
  declaration file for a type, so with `--export-cpp` each type is placed in the
  file the evidence points to (see below). The rest, forward-only types, and global
  typedefs are written to `exported.h`.
- Module source paths and build metadata from `S_COMPILE2` command blocks or
  `S_ENVBLOCK`, with DBI source-file lists and line records as fallbacks.
- C11 source-line tables with local filenames, plus aligned C13 subsections
  used by assemblers and other CodeView producers.
- Procedure names, addresses, parameters, locals, module typedefs, and data from
  module/global symbols using the existing C++ and IDA exporters.
- Original build environment strings, including historical and target-specific compiler switches, as
  `Build ...` comments. These switches are preserved rather than interpreted
  using the desktop compiler's argument grammar.

Modules with useful symbols but no identifiable source file are preserved under
`__pdb_modules/module_<stream>.cpp`. Source filters still apply to these paths.
`exported.h` is a shared type inventory and is not limited by source filters: a
type whose inferred file is filtered out stays there. Headers known only from line
records or file lists, where nothing could be recovered, are not written; their
paths are listed in `empty_headers.txt`, relative to the output directory.
Legacy procedures without source lines are retained: lack of line information
alone cannot distinguish handwritten assembly or stripped objects from
compiler-generated functions.

A PDB cannot supply information the compiler never emitted. In particular,
legacy records do not identify a type's original header/line (placement infers
one) or individual inline call sites. A header that held only declarations
appears nowhere in a legacy PDB, so its types cannot be placed in it. Forward-only types remain declarations. Function
bodies remain debug-scope stubs unless pseudocode is supplied through the IDA
workflow; this tool does not reconstruct machine-code behavior by itself.

### Type placement

A type is placed by the strongest evidence available:

1. the header holding line records of its member functions, since an in-class body is
   inside the class;
2. a known header named after the source file that defines its member functions;
3. the file of the type it is nested in, or of the only types holding it by value;
4. the only file whose code (locals, parameters, globals) uses it.

Nested types go with their outermost type and function-local types with their
function. Anonymous types and types without evidence stay in `exported.h`.
`placements.json` lists every placed type with its file, the rule that placed it
and the evidence, so an inferred location can be checked; it is not an original
header location unless the evidence says so.

A file includes the files that define what its types derive from or hold by value.
Member functions without a body are folded into their class's declaration when the
class is defined in the same file, since a member can't be redeclared outside its
class; the declaration keeps the address as a `// 0x...` comment.

### SDK headers

With `--sdk-include <dir>`, the headers under `dir` are scanned for the classes,
structs, unions, enums and typedefs they define at namespace scope. A type found
there, or nested in one or instantiated from a template found there, is not
reconstructed: the file it would have been in, and files holding it by value,
`#include <header>` instead, and `placements.json` lists it with the rule `sdk`.
Matching is by name, and the SDK may be a different version than the PDB was built
with, so `exported.h` ends with a `static_assert` of each such class's size as the
PDB records it. Enumerator values are not checked. Headers are scanned, not
compiled: both branches of a conditional count, and only the C++ library's
namespace macros and the COM interface macros are understood.

### Tests

Run unit tests with `cargo test`. A generated fixture test exercises the installed
XDK compiler and linker without redistributing SDK files:

```powershell
$env:XDK_ROOT = 'C:\Path\To\Installed\XDK'
cargo test --test xdk -- --ignored
```

The test deliberately gives the object a different filename from its source and
checks recovered types, build metadata, symbols, scope variables, and repeatable
shared-header output. It also removes `/names` and the build source key from a
generated copy to verify the DBI-only source lookup.

Inspect a PDB before exporting with:

```text
cargo run --example pdb_inventory -- path/to/input.pdb
```

This reports the target architecture, IPI availability, compiler versions, and
C11 coverage and top-level symbol kinds. C11 line recovery has also been checked against a desktop x86
PDB produced by MSVC 7.x; the XDK integration test is one compatibility case,
not a restriction on supported input.

The C11 layout uses per-file segment tables and parallel address/line arrays;
see the [Wine CodeView reader](https://github.com/wine-mirror/wine/blob/master/dlls/dbghelp/msc.c)
for another implementation of this format. Files from older MSF 2.0/VC6 or
16-bit CodeView dialects are not covered by the current integration tests.
