mod cpp;
mod debug_blocks;
mod decompile;
mod namespaces;
mod objects;
mod legacy;
mod options;
mod param_const;
mod placement;
mod reorganize;
mod sdk;
mod tabbed;
pub mod utils;

use decompile::Decompiler;
use options::Options;
use std::error::Error;
use structopt::StructOpt;

fn main() -> Result<(), Box<dyn Error>> {
    let mut options = Options::from_args();

    // The output directory is consumed through `Path::join`, which handles a
    // missing trailing separator correctly on every platform, so the `--out`
    // value needs no manual normalization here.

    // If `--pdb` was passed but didn't parse as a flag, the most common cause
    // is a Windows path quoted with a trailing backslash before the closing
    // quote (e.g. --out="...\"), which escapes that quote and swallows the
    // following arguments. Point that out instead of leaving a bare
    // "PDB path not provided" error.
    if options.pdb.is_none() {
        let command_line = std::env::args_os()
            .skip(1)
            .map(|arg| arg.to_string_lossy().to_string())
            .collect::<Vec<_>>()
            .join(" ");

        if command_line.contains("--pdb") {
            eprintln!(
                "NOTE: '--pdb' appears in the command line but was not parsed as a flag. \
                 If the path is quoted, make sure it does not end with a backslash before \
                 the closing quote: use --pdb=\"C:\\path\\file.pdb\", not --pdb=\"C:\\path\\folder\\\"."
            );
        }
    }

    // Dumping debug blocks requires unrolled procedure bodies.
    if options.export_debug_blocks_json.is_some() {
        options.unroll_functions = true;
    }

    let mut function_scopes_modules = None;

    if options.function_scopes_pdb.is_some() {
        let mut options2 = options.clone();
        options2.pdb = options2.function_scopes_pdb.take();
        options2.export_cpp = false;
        options2.export_pseudocode_to_files = false;
        options2.export_pseudocode_to_json = false;
        options2.export_function_types = false;
        options2.verbose_blocks = false;
        options2.pseudocode_json_path = None;
        options2.pseudocode_json = None;
        options2.unroll_functions = true;
        options2.function_scopes_pdb = None;
        options2.function_scopes_out = None;
        options2.reorganize = false;
        options2.out = options.function_scopes_out.take();
        
        let mut decompiler = Decompiler::new(options2);

        match decompiler.decompile() {
            Ok(_) => function_scopes_modules = Some(decompiler.modules),
            Err(error) => println!("ERROR: could not decompile PDB because it {error}"),
        }
    }

    if let Some(pseudocode_json_path) = options.pseudocode_json_path.clone() {
        let file = std::fs::File::open(pseudocode_json_path)?;
        let pseudocode_json = serde_json::from_reader(file).ok();
        options.pseudocode_json = pseudocode_json.unwrap();
    }

    Decompiler::new(options).with_function_scopes_modules(function_scopes_modules).decompile()?;

    Ok(())
}
