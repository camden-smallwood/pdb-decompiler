//! A manifest of the objects an image was linked from, in link order (the order the DBI lists
//! its modules): what each object was built from and with, the address ranges it contributed,
//! and its functions and data. Nothing here depends on how the C++ output is laid out.

use crate::{cpp, decompile::DecompileContext};
use pdb2::FallibleIterator;
use serde_json::{json, Value};
use std::{collections::{BTreeMap, HashMap}, error::Error, fs::File, path::Path};

fn version(version: &pdb2::CompilerVersion) -> String {
    match version.qfe {
        Some(qfe) => format!("{}.{}.{}.{qfe}", version.major, version.minor, version.build),
        None => format!("{}.{}.{}", version.major, version.minor, version.build),
    }
}

fn hex(address: u64) -> String {
    format!("0x{address:X}")
}

fn stem(path: &str) -> String {
    let name = path.rsplit(['/', '\\']).next().unwrap_or(path);
    name.rsplit_once('.').map_or(name, |(stem, _)| stem).to_ascii_lowercase()
}

fn is_source(path: &str) -> bool {
    path.rsplit_once('.').is_some_and(|(_, extension)| cpp::SOURCE_FILE_EXTS.iter().any(|x| extension.eq_ignore_ascii_case(x)))
}

pub fn export_objects_json(context: &mut DecompileContext, base_address: u64, path: &Path) -> Result<(), Box<dyn Error>> {
    let address_map = context.address_map;
    let address = |offset: pdb2::PdbInternalSectionOffset| offset.to_rva(address_map).map(|rva| base_address + rva.0 as u64);

    let section_names = context.pdb.sections()?.unwrap_or_default()
        .iter().map(|section| section.name().to_string()).collect::<Vec<_>>();
    let section_name = |section: u16| section_names.get((section as usize).wrapping_sub(1)).cloned().unwrap_or_else(|| section.to_string());

    // Contributions in address order. A run with no other module's contribution inside is one range.
    let mut contributions = vec![];
    let mut iter = context.debug_info.section_contributions()?;
    while let Some(contribution) = iter.next()? {
        if contribution.size == 0 {
            continue;
        }
        if let Some(start) = address(contribution.offset) {
            contributions.push((start, start + contribution.size as u64, contribution.offset.section, contribution.module));
        }
    }
    contributions.sort();

    let mut ranges = HashMap::<usize, Vec<Value>>::new();
    let mut run = None::<(u64, u64, u16, usize)>;
    for &(start, end, section, module) in contributions.iter().chain([(u64::MAX, u64::MAX, 0, usize::MAX)].iter()) {
        if let Some(current) = run.as_mut() && current.2 == section && current.3 == module {
            current.1 = current.1.max(end);
            continue;
        }
        if let Some((start, end, section, module)) = run.replace((start, end, section, module)) {
            ranges.entry(module).or_default().push(json!({
                "section": section_name(section),
                "start": hex(start),
                "end": hex(end),
            }));
        }
    }

    // The owner of an address, for global data, which is recorded outside its module.
    let owner = |address: u64| {
        let i = contributions.partition_point(|c| c.0 <= address);
        (i > 0 && address < contributions[i - 1].1).then(|| contributions[i - 1].3)
    };

    // Decorated names by address, from the public symbols
    let mut publics = HashMap::<u64, String>::new();
    let mut global_data = HashMap::<usize, Vec<(u64, String)>>::new();
    let mut symbols = context.global_symbols.iter();
    while let Some(symbol) = symbols.next()? {
        match symbol.parse() {
            Ok(pdb2::SymbolData::Public(public)) => {
                if let Some(address) = address(public.offset) {
                    publics.entry(address).or_insert_with(|| public.name.to_string().into_owned());
                }
            }
            Ok(pdb2::SymbolData::Data(data)) => {
                if let Some(address) = address(data.offset) && let Some(module) = owner(address) {
                    global_data.entry(module).or_default().push((address, data.name.to_string().into_owned()));
                }
            }
            _ => {}
        }
    }

    let mut objects = vec![];
    let modules = context.debug_info.modules()?.collect::<Vec<_>>()?;

    for (index, module) in modules.iter().enumerate() {
        let module_name = module.module_name().into_owned();
        let object_file_name = module.object_file_name().into_owned();

        let mut compiler = Value::Null;
        let mut build = BTreeMap::new();
        let mut functions = vec![];
        let mut data = BTreeMap::<u64, Value>::new();

        if let Some(module_info) = context.pdb.module_info(module)? {
            let line_program = module_info.line_program();
            let lines = crate::legacy::module_source_lines(context.pdb, module, line_program.as_ref().ok(), context.string_table).unwrap_or_default();
            let mut symbols = module_info.symbols()?;

            while let Some(symbol) = symbols.next()? {
                build.extend(crate::legacy::build_environment(&symbol).unwrap_or_default());

                match symbol.parse() {
                    Ok(pdb2::SymbolData::CompileFlags(flags)) if compiler.is_null() => {
                        compiler = json!({
                            "name": flags.version_string.to_string(),
                            "frontend": version(&flags.frontend_version),
                            "backend": version(&flags.backend_version),
                            "language": format!("{:?}", flags.language),
                        });
                    }

                    Ok(pdb2::SymbolData::BuildInfo(info)) => {
                        let Some(id_finder) = context.id_finder.as_ref() else { continue };
                        let Ok(pdb2::IdData::BuildInfo(info)) = id_finder.find(info.id).and_then(|item| item.parse()) else { continue };
                        let keys: &[&str] = match info.arguments.len() {
                            2 => &["cwd", "src"],
                            _ => &["cwd", "cl", "src", "pdb", "cmd"],
                        };
                        for (key, value) in keys.iter().zip(cpp::build_info_arguments(id_finder, info)) {
                            build.entry(key.to_string()).or_insert(value);
                        }
                    }

                    Ok(pdb2::SymbolData::Procedure(procedure)) => {
                        let Some(address) = address(procedure.offset) else { continue };
                        let line = crate::legacy::line_at(&lines, procedure.offset);
                        let mut function = json!({
                            "name": procedure.name.to_string(),
                            "symbol": publics.get(&address),
                            "address": hex(address),
                            "size": procedure.len,
                            "file": line.map(|(file, _)| file),
                            "line": line.map(|(_, line)| line.line_start),
                        });
                        // The linker kept another object's identical copy, which the address is in
                        if let Some(owner) = owner(address).filter(|owner| *owner != index) {
                            function["kept_by"] = json!(owner);
                        }
                        functions.push((address, function));
                    }

                    Ok(pdb2::SymbolData::Data(symbol)) => {
                        let Some(address) = address(symbol.offset) else { continue };
                        data.entry(address).or_insert_with(|| json!({
                            "name": symbol.name.to_string(),
                            "symbol": publics.get(&address),
                            "address": hex(address),
                        }));
                    }

                    _ => {}
                }
            }
        }

        for (address, name) in global_data.remove(&index).unwrap_or_default() {
            data.entry(address).or_insert_with(|| json!({
                "name": name,
                "symbol": publics.get(&address),
                "address": hex(address),
            }));
        }

        functions.sort_by_key(|(address, _)| *address);

        // The source file the object was compiled from: the build record's, else the one source
        // file in the DBI file list named like the object, else its only source file
        let source = build.get("src").filter(|src| !src.is_empty()).map(|src| match build.get("cwd") {
            Some(cwd) if !src.contains(':') && !src.starts_with(['\\', '/']) => format!("{}\\{src}", cwd.trim_end_matches(['\\', '/'])),
            _ => src.clone(),
        }).or_else(|| {
            let files = context.source_files.get(index)?;
            let sources = files.iter().filter(|f| is_source(f)).collect::<Vec<_>>();
            let named = sources.iter().copied().filter(|f| stem(f) == stem(&module_name)).collect::<Vec<_>>();
            match (named.as_slice(), sources.as_slice()) {
                ([only], _) | ([], [only]) => Some(only.to_string()),
                _ => None,
            }
        });

        objects.push(json!({
            "index": index,
            "object": module_name,
            "library": (!object_file_name.eq_ignore_ascii_case(&module_name)).then_some(object_file_name),
            "source": source,
            "compiler": compiler,
            "build": build,
            "ranges": ranges.remove(&index).unwrap_or_default(),
            "functions": functions.into_iter().map(|(_, f)| f).collect::<Vec<_>>(),
            "data": data.into_values().collect::<Vec<_>>(),
        }));
    }

    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }

    let manifest = json!({
        "note": "Objects in link order. Addresses include the base address. A range is a run of contributions with no other object's inside it; functions are in address order. kept_by is the object whose identical copy of a function the linker kept.",
        "base_address": hex(base_address),
        "objects": objects,
    });

    serde_json::to_writer_pretty(File::create(path)?, &manifest)?;

    Ok(())
}
