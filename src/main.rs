mod cpp;
mod decompile;
mod options;
mod reorganize;
mod tabbed;
mod utils;

use decompile::Decompiler;
use options::Options;
use std::{error::Error, path::PathBuf};
use structopt::StructOpt;

fn main() -> Result<(), Box<dyn Error>> {
    let mut options = Options::from_args();
    
    if let Some(out) = options.out.as_mut() {
        if !out.is_dir() {
            *out = PathBuf::from(format!("{}/", out.to_string_lossy()));
        }

        *out = utils::canonicalize_path(out.to_str().unwrap_or(""), "", "", true);
    }

    let mut function_scopes_modules = None;

    if options.function_scopes_pdb.is_some() {
        let mut options2 = options.clone();
        options2.pdb = options2.function_scopes_pdb.take();
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

    if let Err(error) = Decompiler::new(options).with_function_scopes_modules(function_scopes_modules).decompile() {
        println!("ERROR: could not decompile PDB because it {error}");
    }

    Ok(())
}
