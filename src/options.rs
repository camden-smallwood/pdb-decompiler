use std::{num, path::PathBuf};
use structopt::StructOpt;

#[derive(Clone, Debug, Default, StructOpt)]
#[structopt(
    name = "pdb-decompiler",
    about = "A tool to decompile MSVC PDB files to C++ source code."
)]
pub struct Options {
    /// The output directory to export to.
    #[structopt(short, long, parse(from_os_str))]
    pub out: Option<PathBuf>,

    /// The file path to the MSVC PDB file to decompile.
    #[structopt(short, long, parse(from_os_str))]
    pub pdb: Option<PathBuf>,

    /// A source file path (or directory) to filter which modules are decompiled.
    /// Only modules whose source path is equal to, or nested under, one of these
    /// paths are exported. Can be specified multiple times.
    ///
    /// For a directory path, the output is rebased so the last folder of the path
    /// becomes the output root. E.g. `--source-path .../FolderA/SubFolderA` writes
    /// `.../FolderA/SubFolderA/foo.cpp` to `<out>/SubFolderA/foo.cpp`.
    #[structopt(long = "source-path", parse(from_os_str))]
    pub source_paths: Vec<PathBuf>,

    /// An SDK include directory. For PDBs without IPI, types and typedefs its headers define are
    /// included from the SDK instead of reconstructed, and exported.h checks their sizes. Can be
    /// specified multiple times.
    #[structopt(long = "sdk-include", parse(from_os_str))]
    pub sdk_includes: Vec<PathBuf>,

    /// The base address to add when resolving an RVA. (Optional)
    #[structopt(short, long, parse(try_from_str = parse_base_address))]
    pub base_address: Option<u64>,

    /// Whether to export C++ code to the output directory.
    #[structopt(long, short = "c")]
    pub export_cpp: bool,

    /// Whether to generate IDA script statements that export pseudocode to their appropriate source files.
    #[structopt(long)]
    pub export_pseudocode_to_files: bool,

    /// Whether to generate IDA script statements that export pseudocode to a JSON mapping file.
    #[structopt(long)]
    pub export_pseudocode_to_json: bool,

    /// Whether to generate IDA script statements that try to set function types.
    #[structopt(long)]
    pub export_function_types: bool,

    /// Whether to write extra block level information.
    #[structopt(long)]
    pub verbose_blocks: bool,

    /// Dumps each procedure's unrolled scope tree (variable declarations in nested
    /// blocks) to a JSON file keyed by mangled function name. (Forces unrolling.)
    #[structopt(long, parse(from_os_str))]
    pub export_debug_blocks_json: Option<PathBuf>,

    /// Writes a JSON manifest of the objects the image was linked from, in link order: each
    /// object's library, source file, compiler and build command, the address ranges it
    /// contributed, and its functions and data with their addresses and source lines.
    #[structopt(long, parse(from_os_str))]
    pub export_objects_json: Option<PathBuf>,

    /// The file containing all function pseudocode in a JSON mapping. (Optional)
    #[structopt(long, parse(from_os_str))]
    pub pseudocode_json_path: Option<PathBuf>,

    /// The loaded JSON value containing the mapping with all function pseudocode.
    pub pseudocode_json: Option<serde_json::Value>,

    /// Whether to include scope information in decompiled function stubs. (Experimental)
    #[structopt(short, long)]
    pub unroll_functions: bool,

    /// The file path to the MSVC PDB file to decompile for extra function scope information.
    #[structopt(long, parse(from_os_str))]
    pub function_scopes_pdb: Option<PathBuf>,

    /// The output directory to dump all function scopes C++ code to.
    #[structopt(long, parse(from_os_str))]
    pub function_scopes_out: Option<PathBuf>,

    /// Whether to reorganize generated C++ code to Bungie's coding standards. (Experimental)
    #[structopt(short, long)]
    pub reorganize: bool,

    /// The project name used to generate the umbrella include header. The emitted
    /// include is `{project_name}/{project_name}.h` (e.g. `project/project.h`).
    #[structopt(long = "project-name", default_value = "project")]
    pub project_name: String,

    /// Includes compiler-generated procedure definitions (implicit
    /// ctors/dtors/assignment, thunks, dynamic initializers) in output C++ code.
    #[structopt(long)]
    pub include_compiler_generated: bool,

    /// Appends a `// 0x...` address comment to every function, global and
    /// thread-local in output C++ code.
    #[structopt(long)]
    pub address_comments: bool,

    /// Forces a `__declspec(noinline)` attribute for functions that were never inlined but did not have the attribute.
    #[structopt(long)]
    pub force_no_inlines: bool,
}

fn parse_base_address(src: &str) -> Result<u64, num::ParseIntError> {
    u64::from_str_radix(src.trim_start_matches("0x"), 16)
}
