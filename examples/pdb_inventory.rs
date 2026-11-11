//! Inspect format capabilities without attempting C++ reconstruction.
use pdb2::FallibleIterator;
#[allow(dead_code)]
#[path = "../src/legacy.rs"]
mod legacy;
fn main() -> Result<(), Box<dyn std::error::Error>> {
    for path in std::env::args_os().skip(1) {
        let mut pdb = pdb2::PDB::open(std::fs::File::open(&path)?)?;
        let info = pdb
            .raw_stream(pdb2::StreamIndex(1))?
            .ok_or("missing PDB info")?;
        let dbi = pdb.debug_information()?;
        let modules = dbi.modules()?.collect::<Vec<_>>()?;
        let names = modules
            .iter()
            .map(|m| m.module_name().into_owned())
            .collect::<std::collections::HashSet<_>>();
        println!("Repeated module names: {}", modules.len() - names.len());
        let mut line_count = 0;
        let mut errors = 0;
        let mut kinds = std::collections::BTreeMap::<String, usize>::new();
        let mut compilers = std::collections::BTreeSet::new();
        for module in &modules {
            if module.info().lines_size > 0 {
                let raw = pdb
                    .raw_stream(module.info().stream)?
                    .ok_or(pdb2::Error::StreamNotFound(module.info().stream.0 as u32))?;
                let start = module.info().symbols_size as usize;
                let data = raw
                    .as_slice()
                    .get(start..start + module.info().lines_size as usize)
                    .ok_or(pdb2::Error::UnexpectedEof)?;
                match legacy::c11_lines(data, false) {
                    Ok(files) => line_count += files.values().map(|f| f.len()).sum::<usize>(),
                    Err(error) => {
                        errors += 1;
                        eprintln!("C11 {}: {error}", module.module_name());
                    }
                }
            }
            if let Some(info) = pdb.module_info(module)? {
                let mut symbols = info.symbols()?;
                let mut depth = 0usize;
                while let Some(symbol) = symbols.next()? {
                    if depth == 0 {
                        let name = match symbol.parse() {
                            Ok(data) => format!("{data:?}").split('(').next().unwrap().to_owned(),
                            Err(error) => format!("0x{:x}: {error}", symbol.raw_kind()),
                        };
                        *kinds.entry(name).or_default() += 1;
                    }
                    if symbol.starts_scope() {
                        depth += 1;
                    }
                    if symbol.ends_scope() {
                        depth = depth.saturating_sub(1);
                    }
                    if let Ok(pdb2::SymbolData::CompileFlags(flags)) = symbol.parse() {
                        compilers.insert(format!(
                            "{} {:?}",
                            flags.version_string, flags.backend_version
                        ));
                    }
                }
            }
        }
        println!(
            "{}: machine={:?}, IPI={:?}, modules={}, C11={}",
            path.to_string_lossy(),
            dbi.machine_type(),
            legacy::has_ipi(info.as_slice()),
            modules.len(),
            modules.iter().filter(|m| m.info().lines_size > 0).count()
        );
        println!("  C11 lines={line_count}, errors={errors}");
        println!("  Top-level symbols: {kinds:?}");
        for compiler in compilers {
            println!("  {compiler}");
        }
    }
    Ok(())
}
