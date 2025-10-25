use std::{num, path::PathBuf};
use structopt::StructOpt;

#[derive(Clone, Debug, StructOpt)]
#[structopt(
    name = "pdb-decompiler",
    about = "A tool to decompile MSVC PDB files to C++ source code."
)]
pub struct Options {
    /// The output directory to dump all C++ code to.
    #[structopt(short, long, parse(from_os_str))]
    pub out: Option<PathBuf>,

    /// The file path to the MSVC PDB file to decompile.
    #[structopt(short, long)]
    pub pdb: Option<PathBuf>,

    /// The base address to add when resolving an RVA. (Optional)
    #[structopt(short, long, parse(try_from_str = parse_base_address))]
    pub base_address: Option<u64>,

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

    /// The file containing all function pseudocode in a JSON mapping. (Optional)
    #[structopt(long)]
    pub pseudocode_json_path: Option<PathBuf>,

    /// The loaded JSON value containing the mapping with all function pseudocode.
    pub pseudocode_json: Option<serde_json::Value>,

    /// Whether to include scope information in decompiled function stubs. (Experimental)
    #[structopt(short, long)]
    pub unroll_functions: bool,

    /// The file path to the MSVC PDB file to decompile for extra function scope information.
    #[structopt(long)]
    pub function_scopes_pdb: Option<PathBuf>,

    /// The output directory to dump all function scopes C++ code to.
    #[structopt(long)]
    pub function_scopes_out: Option<PathBuf>,

    /// Whether to reorganize generated C++ code to Bungie's coding standards. (Experimental)
    #[structopt(short, long)]
    pub reorganize: bool,
}

fn parse_base_address(src: &str) -> Result<u64, num::ParseIntError> {
    u64::from_str_radix(src.trim_start_matches("0x"), 16)
}
