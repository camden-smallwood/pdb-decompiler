mod cpp;
mod tabbed;

use pdb2::FallibleIterator;
use std::{
    collections::HashMap,
    error::Error,
    fs::{self, File, OpenOptions},
    io::Write,
    num,
    path::{Path, PathBuf},
};
use structopt::StructOpt;

#[derive(Clone, Debug, StructOpt)]
#[structopt(name = "pdb-decompiler", about = "A tool to decompile MSVC PDB files to C++ source code.")]
struct Options {
    /// The output directory to dump all C++ code to.
    #[structopt(short, long, parse(from_os_str))]
    out: Option<PathBuf>,

    /// The file path to the MSVC PDB file to decompile.
    #[structopt(short, long)]
    pdb: Option<String>,

    /// The base address to add when resolving an RVA (optional).
    #[structopt(short, long, parse(try_from_str = parse_base_address))]
    base_address: Option<u64>,

    /// Whether to include scope information in decompiled function stubs.
    #[structopt(short, long)]
    unroll_functions: bool,
}

fn parse_base_address(src: &str) -> Result<u64, num::ParseIntError> {
    u64::from_str_radix(src.trim_start_matches("0x"), 16)
}

#[inline(always)]
fn sanitize_path<S: AsRef<str>>(path: S) -> String {
    let mut result = path.as_ref().to_string().replace('\\', "/");

    if let Some((_, rest)) = result.split_once(':') {
        result = rest.to_string();
    }

    result
}

#[inline(always)]
pub fn canonicalize_path(out_path: &str, root_path: &str, path: &str, is_directory: bool) -> PathBuf {
    let path = PathBuf::from(format!(
        "{}/{}{}",
        out_path,
        if path.contains(':') {
            sanitize_path(path)
        } else {
            format!("{}/{}", sanitize_path(root_path), sanitize_path(path))
        },
        if is_directory { "/" } else { "" },
    ));

    if !path.exists() {
        if is_directory {
            if !path.exists() && let Err(e) = fs::create_dir_all(path.clone()) {
                panic!("Failed to create directory \"{}\": {e}", path.to_string_lossy());
            }
        } else {
            if let Some(parent_path) = path.parent()
                && let Err(e) = fs::create_dir_all(parent_path)
            {
                panic!(
                    "Failed to create parent directories for file \"{}\": {e}",
                    path.to_string_lossy(),
                );
            }
    
            if let Err(e) = File::create(path.clone()) {
                panic!("Failed to create file \"{}\": {e}", path.to_string_lossy());
            }
        }
    }

    match path.canonicalize() {
        Ok(x) => x.to_string_lossy().replace(out_path, "").trim_start_matches("\\\\?\\").into(),
        Err(e) => panic!("Failed to canonicalize path \"{}\": {e}", path.to_string_lossy())
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut options = Options::from_args();
    
    if let Some(out) = options.out.as_mut() {
        if !out.is_dir() {
            *out = PathBuf::from(format!("{}/", out.to_string_lossy()));
        }

        *out = canonicalize_path(out.to_str().unwrap_or(""), "", "", true);
    }

    let pdb_path = options.pdb.clone().ok_or("PDB path not provided")?;
    let pdb = pdb2::PDB::open(File::open(pdb_path)?)?;

    if let Err(error) = decompile_pdb(&options, pdb) {
        println!("ERROR: could not decompile PDB because it {error}");
    }

    Ok(())
}

#[inline(always)]
fn decompile_pdb(options: &Options, mut pdb: pdb2::PDB<File>) -> Result<(), Box<dyn Error>> {
    let debug_info = pdb.debug_information().map_err(|e| format!("does not contain debug information: {e}"))?;
    let machine_type = debug_info.machine_type().unwrap_or(pdb2::MachineType::Unknown);

    let address_map = pdb.address_map().map_err(|e| format!("does not contain an address map: {e}"))?;
    
    let string_table = pdb.string_table().ok(); // this isn't present in older versions, so we make it optional

    let type_info = pdb.type_information().map_err(|e| format!("does not contain type information: {e}"))?;
    let mut type_finder = type_info.finder();

    let id_info = pdb.id_information().map_err(|e| format!("does not contain id information: {e}"))?;
    let mut id_finder = id_info.finder();

    let global_symbols = pdb.global_symbols().map_err(|e| format!("does not contain global symbol information: {e}"))?;

    let mut modules = HashMap::new();

    let out_path = options.out.as_ref().ok_or_else(|| "Out path not supplied".to_string())?;
    fs::create_dir_all(out_path.clone())?;

    let mut script_file = File::create(out_path.join("ida_script.py"))?;
    writeln!(script_file, include_str!("../ida_script_base.py"))?;

    process_type_information(
        options,
        machine_type,
        &type_info,
        &mut type_finder,
        id_info.is_empty(),
    ).map_err(|e| format!("failed to process type info: {e}"))?;
    
    process_id_information(
        options,
        machine_type,
        string_table.as_ref(),
        &id_info,
        &mut id_finder,
        &type_info,
        &mut type_finder,
        &mut modules,
    )
    .map_err(|e| format!("failed to process id info: {e}"))?;

    process_modules(
        options,
        machine_type,
        options.base_address,
        &mut pdb,
        &address_map,
        string_table.as_ref(),
        &debug_info,
        &global_symbols,
        &mut id_finder,
        &type_info,
        &type_finder,
        &mut modules,
        &mut script_file
    )
}

#[inline(always)]
fn process_type_information<'a>(
    options: &Options,
    machine_type: pdb2::MachineType,
    type_info: &'a pdb2::TypeInformation,
    type_finder: &mut pdb2::TypeFinder<'a>,
    export_all: bool,
) -> pdb2::Result<()> {
    let mut type_iter = type_info.iter();

    let mut exported_forward_reference_indices = vec![];
    let mut exported_type_indices = vec![];

    while let Ok(Some(type_item)) = type_iter.next() {
        type_finder.update(&type_iter);
        
        if !export_all {
            continue;
        }
        
        let type_data = match type_item.parse() {
            Ok(x) => x,
            Err(_) => {
                // println!("WARNING: Failed to parse type: {e} - {type_item:#?}");
                continue;
            }
        };

        if let Some(forward_reference) = match type_data {
            pdb2::TypeData::Class(class_type) => Some(class_type.properties.forward_reference()),
            pdb2::TypeData::Enumeration(enum_type) => Some(enum_type.properties.forward_reference()),
            pdb2::TypeData::Union(union_type) => Some(union_type.properties.forward_reference()),
            _ => None,
        } {
            if forward_reference {
                exported_forward_reference_indices.push(type_item.index());
            } else {
                exported_type_indices.push(type_item.index());
            }
        }
    }

    if exported_type_indices.is_empty() && exported_forward_reference_indices.is_empty() {
        return Ok(());
    }

    let exported_path = PathBuf::from(format!("{}/exported.h", options.out.as_ref().unwrap().to_string_lossy()));

    let mut module = cpp::Module::default();
    module.path = exported_path.clone();

    for type_index in exported_forward_reference_indices {
        if let Err(e) = module.add_type_definition(machine_type, type_info, type_finder, type_index, 0) {
            panic!("{e}");
        }
    }

    for type_index in exported_type_indices {
        if let Err(e) = module.add_type_definition(machine_type, type_info, type_finder, type_index, 0) {
            panic!("{e}");
        }
    }

    if let Some(parent_path) = exported_path.parent()
        && let Err(e) = fs::create_dir_all(parent_path)
    {
        panic!("{e}");
    }

    let mut file = if exported_path.exists() {
        match OpenOptions::new().write(true).append(true).open(exported_path.clone()) {
            Ok(x) => x,
            Err(e) => panic!("{e}: {}", exported_path.to_string_lossy()),
        }
    } else {
        match File::create(exported_path.clone()) {
            Ok(x) => x,
            Err(e) => panic!("{e}: {}", exported_path.to_string_lossy()),
        }
    };

    if let Err(e) = write!(file, "{module}") {
        panic!("{e}");
    }

    Ok(())
}

#[inline(always)]
fn process_id_information<'a>(
    options: &Options,
    machine_type: pdb2::MachineType,
    string_table: Option<&pdb2::StringTable>,
    id_info: &'a pdb2::IdInformation,
    id_finder: &mut pdb2::IdFinder<'a>,
    type_info: &'a pdb2::TypeInformation,
    type_finder: &mut pdb2::TypeFinder<'a>,
    modules: &mut HashMap<String, cpp::Module>,
) -> pdb2::Result<()> {
    let mut id_iter = id_info.iter();

    while let Some(id) = id_iter.next()? {
        id_finder.update(&id_iter);

        let id_data = match id.parse() {
            Ok(id_data) => id_data,
            Err(_) => {
                // println!("WARNING: failed to parse id: {e}");
                continue;
            }
        };

        match id_data {
            pdb2::IdData::BuildInfo(build_info) => {
                let mut module = cpp::Module::default();
                module.add_build_info(options.out.as_ref().unwrap(), id_finder, build_info)?;

                let module_key = module.path.to_string_lossy().to_lowercase().to_string();
                if module_key.is_empty() {
                    continue;
                }
                
                if let Some(old_module) = modules.insert(module_key.clone(), module) {
                    let module = modules.get_mut(&module_key).unwrap();
                    module.headers = old_module.headers;
                    module.members = old_module.members;
                }
            }
            
            pdb2::IdData::UserDefinedTypeSource(data) => {
                let module_path = match data.source_file {
                    pdb2::UserDefinedTypeSourceFileRef::Local(id) => match id_finder.find(id) {
                        Ok(item) => match item.parse() {
                            Ok(pdb2::IdData::String(source_file)) => sanitize_path(&source_file.name.to_string()),
                            Ok(data) => panic!("invalid UDT source file id: {:#?}", data),
                            Err(error) => panic!("failed to parse UDT source file id: {error}"),
                        },

                        Err(error) => panic!("failed to find UDT source file id: {error}"),
                    },

                    pdb2::UserDefinedTypeSourceFileRef::Remote(_, source_line_ref) => {
                        sanitize_path(&source_line_ref.to_string_lossy(string_table.unwrap())?)
                    }
                };

                modules
                    .entry(module_path.to_lowercase())
                    .or_insert_with(|| cpp::Module::default().with_path(module_path.into()))
                    .add_type_definition(machine_type, type_info, type_finder, data.udt, data.line)?;
            }

            _ => {}
        }
    }

    Ok(())
}

#[inline(always)]
fn load_section_contributions(
    debug_info: &pdb2::DebugInformation,
) -> pdb2::Result<Vec<pdb2::DBISectionContribution>> {
    let mut result = vec![];
    let mut section_contributions = debug_info.section_contributions()?;

    while let Some(section_contribution) = section_contributions.next()? {
        result.push(section_contribution);
    }

    Ok(result)
}

#[inline(always)]
fn load_module_global_symbols<'a>(
    debug_info: &pdb2::DebugInformation,
    global_symbols: &'a pdb2::SymbolTable,
) -> Result<HashMap<String, Vec<pdb2::SymbolData<'a>>>, Box<dyn Error>> {
    let section_contributions = load_section_contributions(debug_info)?;
    
    let modules = debug_info.modules()?.collect::<Vec<_>>()?;

    let mut prev_module_name = None;

    let mut module_global_symbols = HashMap::new();

    let mut global_symbols_iter = global_symbols.iter();

    loop {
        let symbol = match global_symbols_iter.next() {
            Ok(Some(symbol)) => symbol,
            Ok(None) => break,
            Err(_) => {
                // println!("WARNING: failed to get next symbol: {e}");
                break;
            }
        };

        let symbol_data = match symbol.parse() {
            Ok(symbol_data) => symbol_data,
            Err(_) => {
                // println!("WARNING: failed to parse symbol data, skipping: {error}");
                continue;
            }
        };

        match symbol_data {
            pdb2::SymbolData::UserDefinedType(_) if prev_module_name.is_some() => {
                let module_name = prev_module_name.clone().unwrap();

                // println!("Inserting global UDT into previous module \"{module_name}\": {symbol_data:#?}");

                module_global_symbols.entry(module_name).or_insert_with(Vec::new).push(symbol_data.clone());
            }

            pdb2::SymbolData::ProcedureReference(pdb2::ProcedureReferenceSymbol { module: Some(module), .. }) => {
                let referenced_module = modules.get(module as usize).unwrap();

                let module_name = referenced_module.module_name().to_string();
                prev_module_name = Some(module_name.clone());

                // println!("Found referenced module \"{module_name}\" in global symbol {symbol_data:#?}");

                module_global_symbols.entry(module_name).or_insert_with(Vec::new).push(symbol_data.clone());
            }

            pdb2::SymbolData::Public(pdb2::PublicSymbol { offset, .. })
            | pdb2::SymbolData::Data(pdb2::DataSymbol { offset, .. })
            | pdb2::SymbolData::ThreadStorage(pdb2::ThreadStorageSymbol { offset, .. })
            | pdb2::SymbolData::Procedure(pdb2::ProcedureSymbol { offset, .. }) => {
                for contribution in section_contributions.iter() {
                    let contributing_module = modules.get(contribution.module as usize).unwrap();

                    if offset >= contribution.offset && offset < contribution.offset + contribution.size {
                        let module_name = contributing_module.module_name().to_string();
                        prev_module_name = Some(module_name.clone());

                        // println!("Found contributing module \"{module_name}\" in global symbol {symbol_data:#?}");

                        module_global_symbols.entry(module_name).or_insert_with(Vec::new).push(symbol_data.clone());
                        break;
                    }
                }
            }

            pdb2::SymbolData::Constant(_) => {}

            _ => {
                // println!("found global symbol {symbol_data:?}");
            }
        }
    }

    Ok(module_global_symbols)
}

#[inline(always)]
fn process_modules<'a>(
    options: &Options,
    machine_type: pdb2::MachineType,
    base_address: Option<u64>,
    pdb: &mut pdb2::PDB<File>,
    address_map: &pdb2::AddressMap,
    string_table: Option<&pdb2::StringTable>,
    debug_info: &pdb2::DebugInformation,
    global_symbols: &'a pdb2::SymbolTable,
    id_finder: &mut pdb2::IdFinder,
    type_info: &pdb2::TypeInformation,
    type_finder: &pdb2::TypeFinder,
    modules: &mut HashMap<String, cpp::Module>,
    script_file: &mut File,
) -> Result<(), Box<dyn Error>> {
    let module_global_symbols = load_module_global_symbols(debug_info, global_symbols)
        .map_err(|e| format!("failed to load module global symbol info: {e}"))?;

    let mut module_iter = debug_info.modules().map_err(|e| format!("does not contain debug module info: {e}"))?;

    while let Some(ref module) = module_iter.next().map_err(|e| format!("failed to get next debug module info: {e}"))? {
        let module_info = match pdb.module_info(module)? {
            Some(module_info) => module_info,
            None => {
                // println!("module has no info: {} - {}", module.module_name(), module.object_file_name());
                continue;
            }
        };

        if let Err(_) = process_module(
            options,
            machine_type,
            base_address,
            address_map,
            string_table,
            id_finder,
            type_info,
            type_finder,
            &module_global_symbols,
            modules,
            script_file,
            module,
            &module_info,
        ) {
            // println!("WARNING: failed to parse module, skipping: {e} - {module:#?}");
            continue;
        }
    }

    //
    // Finalize module debug information, clean up include paths
    //

    let module_paths = modules.keys().cloned().collect::<Vec<_>>();
    let mut new_modules = HashMap::new();

    for (_, module) in modules.iter_mut() {
        let mut headers = vec![];

        for (header_path, is_global) in module.headers.iter() {
            let header_module_key = header_path.to_string_lossy().to_lowercase().to_string();

            if !module_paths.contains(&header_module_key) && !new_modules.contains_key(&header_module_key) {
                new_modules.insert(header_module_key.clone(), cpp::Module::default().with_path(header_path.clone()));
            }

            if let Some(pch_file_name) = module.pch_file_name.as_ref() {
                let pch_module_key = pch_file_name.to_string_lossy().to_lowercase().to_string();
                if header_module_key == pch_module_key {
                    continue;
                }
            }

            let mut modified_path = None;

            for dir_path in module.additional_include_dirs.iter() {
                let dir_path = dir_path.to_string_lossy().to_lowercase().to_string();
                let dir_path_len = dir_path.len();

                if dir_path_len < header_module_key.len() && header_module_key.starts_with(dir_path.as_str()) {
                    modified_path = Some((header_module_key[dir_path_len..].to_string().into(), true));
                    break;
                }
            }

            headers.push(match modified_path {
                Some(x) => x,
                None => (header_path.clone(), *is_global),
            });
        }

        module.headers.clear();
        module.headers.extend(headers);
    }

    for (k, v) in new_modules {
        modules.entry(k).or_insert(v);
    }

    //
    // Write modules to file
    //

    for module in modules.values() {
        let path = PathBuf::from(sanitize_path(format!(
            "{}/{}",
            options.out.as_ref().unwrap().to_string_lossy(),
            module.path.to_string_lossy().trim_start_matches('/'),
        )));

        if let Some(parent_path) = path.parent() {
            fs::create_dir_all(parent_path)?;
        }

        let mut file = if path.exists() {
            OpenOptions::new().write(true).append(true).open(path)?
        } else {
            File::create(path)?
        };

        write!(file, "{module}")?;
    }

    Ok(())
}

#[inline(always)]
fn process_module(
    options: &Options,
    machine_type: pdb2::MachineType,
    base_address: Option<u64>,
    address_map: &pdb2::AddressMap,
    string_table: Option<&pdb2::StringTable>,
    id_finder: &mut pdb2::IdFinder,
    type_info: &pdb2::TypeInformation,
    type_finder: &pdb2::TypeFinder,
    module_global_symbols: &HashMap<String, Vec<pdb2::SymbolData>>,
    modules: &mut HashMap<String, cpp::Module>,
    script_file: &mut File,
    module: &pdb2::Module,
    module_info: &pdb2::ModuleInfo,
) -> pdb2::Result<()> {
    let obj_path = get_module_object_file_path(module)?;
    
    let line_program = module_info.line_program()?;
    let line_offsets = get_module_line_program_offsets(&line_program)?;
    
    let (Some(module_file_path), headers) = get_module_file_path_and_header_paths(
        options,
        id_finder,
        module_info,
        &line_program,
        string_table,
        &obj_path,
    )? else { return Ok(()) };

    process_module_global_symbols(
        options,
        machine_type,
        base_address,
        address_map,
        string_table,
        id_finder,
        type_info,
        type_finder,
        module_global_symbols,
        &line_offsets,
        modules,
        script_file,
        module,
        &module_file_path,
    )?;

    process_module_local_symbols(
        options,
        machine_type,
        base_address,
        address_map,
        string_table,
        id_finder,
        type_info,
        type_finder,
        &line_offsets,
        modules,
        script_file,
        module_info,
        &module_file_path,
    )?;

    let module_path = PathBuf::from(module_file_path);
    let module_key = module_path.to_string_lossy().to_lowercase();

    let mut module_headers = vec![];

    for header_path in headers.iter() {
        let header_key = header_path.to_string_lossy().to_lowercase().to_string();

        if !modules.contains_key(&header_key) {
            modules.insert(header_key.clone(), cpp::Module::default().with_path(header_path.clone()));
        }

        module_headers.push((header_path.clone(), false));
    }

    let module = modules.entry(module_key).or_insert_with(|| cpp::Module::default().with_path(module_path));
    
    for header in module_headers {
        if !module.headers.contains(&header) {
            module.headers.push(header);
        }
    }

    Ok(())
}

#[inline(always)]
fn get_module_object_file_path(
    module: &pdb2::Module,
) -> pdb2::Result<PathBuf> {
    let mut obj_path = PathBuf::from(sanitize_path(module.module_name()));

    if let Some(file_name) = obj_path.file_name() {
        let file_name = file_name.to_string_lossy();

        if file_name.ends_with(".o") || file_name.ends_with(".obj") {
            let mut parts = file_name.split('.').collect::<Vec<_>>();
            let mut index = None;

            for &ext in cpp::SOURCE_FILE_EXTS {
                if file_name.contains(format!(".{ext}.").as_str()) {
                    for (i, &part) in parts.iter().skip(1).enumerate() {
                        if part == ext {
                            index = Some(i);
                            break;
                        }
                    }
                }
            }

            if let Some(index) = index {
                parts.resize(index, "");
                parts.push("obj");
            }

            obj_path = parts.join(".").into();
        }
    }

    Ok(obj_path)
}

#[inline(always)]
fn get_module_line_program_offsets(
    line_program: &pdb2::LineProgram,
) -> pdb2::Result<HashMap<pdb2::StringRef, HashMap<pdb2::PdbInternalSectionOffset, pdb2::LineInfo>>> {
    let mut line_offsets = HashMap::new();

    for line in line_program.lines().collect::<Vec<_>>()? {
        let file = line_program.get_file_info(line.file_index)?;
        let offsets = line_offsets.entry(file.name).or_insert_with(HashMap::new);

        offsets.entry(line.offset).or_insert(line);
    }

    Ok(line_offsets)
}

#[inline(always)]
fn get_module_file_path_and_header_paths(
    options: &Options,
    id_finder: &mut pdb2::IdFinder,
    module_info: &pdb2::ModuleInfo,
    line_program: &pdb2::LineProgram,
    string_table: Option<&pdb2::StringTable>,
    obj_path: &Path,
) -> pdb2::Result<(Option<PathBuf>, Vec<PathBuf>)> {
    let mut module_file_path = None;
    let mut headers = vec![];

    for file in line_program.files().collect::<Vec<_>>()? {
        let path = PathBuf::from(sanitize_path(&file.name.to_string_lossy(string_table.unwrap())?));

        match path.extension() {
            Some(extension) if cpp::SOURCE_FILE_EXTS.contains(&extension.to_str().unwrap_or(""))
                && path.file_stem().map(|x| x.to_ascii_lowercase()) == obj_path.file_stem().map(|x| x.to_ascii_lowercase()) =>
            {
                module_file_path = Some(path);
            }

            _ => headers.push(path)
        }
    }

    // HACK: Attempt to find a build info symbol
    if module_file_path.is_none() {
        let mut module_symbols = module_info.symbols()?;

        while let Some(symbol_item) = module_symbols.next()? {
            let Ok(pdb2::SymbolData::BuildInfo(build_info)) = symbol_item.parse() else { continue };
            let Ok(id_item) = id_finder.find(build_info.id) else { continue };
            let Ok(pdb2::IdData::BuildInfo(build_info)) = id_item.parse() else { continue };
            
            let mut module = cpp::Module::default();
            module.add_build_info(options.out.as_ref().unwrap(), id_finder, build_info)?;

            if !module.path.as_os_str().is_empty() {
                module_file_path = Some(module.path.clone());
                break;
            }
        }
    }

    // TODO:
    // HACK: Skip compiler generated files or print a warning and skip
    // if module_file_path.is_none() {
    //     match module.module_name().to_string().as_str() {
    //         "* CIL *" | "* Linker *" | "* Linker Generated Manifest RES *" => (),
            
    //         x if x.to_lowercase().ends_with(".dll") => (),
            
    //         _ => println!("WARNING: module has no source file, skipping: {module:#?}")
    //     }

    //     return Ok(());
    // }

    Ok((module_file_path, headers))
}

#[inline(always)]
fn process_module_global_symbols(
    options: &Options,
    machine_type: pdb2::MachineType,
    base_address: Option<u64>,
    address_map: &pdb2::AddressMap,
    string_table: Option<&pdb2::StringTable>,
    id_finder: &mut pdb2::IdFinder,
    type_info: &pdb2::TypeInformation,
    type_finder: &pdb2::TypeFinder,
    module_global_symbols: &HashMap<String, Vec<pdb2::SymbolData>>,
    line_offsets: &HashMap<pdb2::StringRef, HashMap<pdb2::PdbInternalSectionOffset, pdb2::LineInfo>>,
    modules: &mut HashMap<String, cpp::Module>,
    script_file: &mut File,
    module: &pdb2::Module,
    module_file_path: &PathBuf,
) -> pdb2::Result<()> {
    if let Some(global_symbols) = module_global_symbols.get(&module.module_name().to_string()) {
        for symbol_data in global_symbols {
            process_module_symbol_data(
                options,
                machine_type,
                base_address,
                address_map,
                string_table,
                id_finder,
                type_info,
                type_finder,
                script_file,
                line_offsets,
                modules,
                module_file_path,
                None,
                symbol_data
            )?;
        }
    }
    
    Ok(())
}

#[inline(always)]
fn process_module_local_symbols(
    options: &Options,
    machine_type: pdb2::MachineType,
    base_address: Option<u64>,
    address_map: &pdb2::AddressMap,
    string_table: Option<&pdb2::StringTable>,
    id_finder: &mut pdb2::IdFinder,
    type_info: &pdb2::TypeInformation,
    type_finder: &pdb2::TypeFinder,
    line_offsets: &HashMap<pdb2::StringRef, HashMap<pdb2::PdbInternalSectionOffset, pdb2::LineInfo>>,
    modules: &mut HashMap<String, cpp::Module>,
    script_file: &mut File,
    module_info: &pdb2::ModuleInfo,
    module_file_path: &PathBuf,
) -> pdb2::Result<()> {
    let mut module_symbols = module_info.symbols()?;

    while let Some(symbol) = module_symbols.next()? {
        let symbol_data = match symbol.parse() {
            Ok(symbol_data) => symbol_data,
            Err(_) => {
                // println!("WARNING: failed to parse symbol data, skipping: {err}");
                continue;
            }
        };

        process_module_symbol_data(
            options,
            machine_type,
            base_address,
            address_map,
            string_table,
            id_finder,
            type_info,
            type_finder,
            script_file,
            line_offsets,
            modules,
            module_file_path,
            Some(&mut module_symbols),
            &symbol_data
        )?;
    }
    
    Ok(())
}

#[inline(always)]
fn process_module_symbol_data(
    options: &Options,
    machine_type: pdb2::MachineType,
    base_address: Option<u64>,
    address_map: &pdb2::AddressMap,
    string_table: Option<&pdb2::StringTable>,
    id_finder: &mut pdb2::IdFinder,
    type_info: &pdb2::TypeInformation,
    type_finder: &pdb2::TypeFinder,
    script_file: &mut File,
    line_offsets: &HashMap<pdb2::StringRef, HashMap<pdb2::PdbInternalSectionOffset, pdb2::LineInfo>>,
    modules: &mut HashMap<String, cpp::Module>,
    module_file_path: &PathBuf,
    module_symbols: Option<&mut pdb2::SymbolIter>,
    symbol_data: &pdb2::SymbolData,
) -> pdb2::Result<()> {
    match symbol_data {
        pdb2::SymbolData::UsingNamespace(symbol) => {
            let module_key = module_file_path.to_string_lossy().to_lowercase().to_string();
            let module = modules.entry(module_key).or_insert_with(|| cpp::Module::default().with_path(module_file_path.clone()));
            let using_namespace = cpp::ModuleMember::UsingNamespace(symbol.name.to_string().to_string());

            if !module.members.contains(&using_namespace) {
                module.members.push(using_namespace);
            }
        }

        pdb2::SymbolData::UserDefinedType(udt_symbol) => {
            let module_key = module_file_path.to_string_lossy().to_lowercase().to_string();
            let module = modules.entry(module_key).or_insert_with(|| cpp::Module::default().with_path(module_file_path.clone()));

            let user_defined_type = cpp::ModuleMember::UserDefinedType(format!(
                "typedef {};",
                cpp::type_name(
                    machine_type,
                    type_info,
                    type_finder,
                    udt_symbol.type_index,
                    Some(udt_symbol.name.to_string().to_string()),
                    None,
                    false
                )?
            ));

            if !module.members.contains(&user_defined_type) {
                module.members.push(user_defined_type);
            }
        }

        pdb2::SymbolData::Constant(constant_symbol) => {
            let module_key = module_file_path.to_string_lossy().to_lowercase().to_string();
            let module = modules.entry(module_key).or_insert_with(|| cpp::Module::default().with_path(module_file_path.clone()));

            let type_name = cpp::type_name(
                machine_type,
                type_info,
                type_finder,
                constant_symbol.type_index,
                Some(constant_symbol.name.to_string().to_string()),
                None,
                false,
            )?;

            if type_name.starts_with("const float ") {
                // println!("WARNING: failed to decompile constant float in \"{}\": {symbol_data:#?}", module_file_path.to_string_lossy());
                return Ok(());
            }

            let constant = cpp::ModuleMember::Constant(format!(
                "const {type_name} = {};",
                match constant_symbol.value {
                    pdb2::Variant::U8(x) => format!("0x{:X}", x),
                    pdb2::Variant::U16(x) => format!("0x{:X}", x),
                    pdb2::Variant::U32(x) => format!("0x{:X}", x),
                    pdb2::Variant::U64(x) => format!("0x{:X}", x),
                    pdb2::Variant::I8(x) => format!("0x{:X}", x),
                    pdb2::Variant::I16(x) => format!("0x{:X}", x),
                    pdb2::Variant::I32(x) => format!("0x{:X}", x),
                    pdb2::Variant::I64(x) => format!("0x{:X}", x),
                }
            ));

            if !module.members.contains(&constant) {
                module.members.push(constant);
            }
        }

        pdb2::SymbolData::Data(data_symbol) => {
            let mut file_name_ref = None;
            let mut line_info = None;

            for (name_ref, offsets) in line_offsets.iter() {
                if let Some(line) = offsets.get(&data_symbol.offset) {
                    file_name_ref = Some(*name_ref);
                    line_info = Some(line.clone());
                    break;
                }
            }

            let path = if let Some(file_name_ref) = file_name_ref {
                PathBuf::from(sanitize_path(file_name_ref.to_string_lossy(string_table.unwrap())?.to_string()))
            } else {
                module_file_path.clone()
            };

            let module_key = path.to_string_lossy().to_lowercase().to_string();
            let module = modules.entry(module_key).or_insert_with(|| cpp::Module::default().with_path(path));

            let rva = match data_symbol.offset.to_rva(address_map) {
                Some(rva) => rva,
                None => {
                    // println!("WARNING: no RVA found for data symbol: {data_symbol:?}");
                    return Ok(());
                }
            };

            let address = base_address.unwrap_or(0) + rva.0 as u64;

            if module.members.iter().any(|member| match member {
                cpp::ModuleMember::Data(_, a, _) if *a == address => true,
                _ => false,
            }) {
                return Ok(());
            }

            module.members.push(cpp::ModuleMember::Data(
                format!(
                    "{}; // 0x{address:X}",
                    cpp::type_name(
                        machine_type,
                        type_info,
                        type_finder,
                        data_symbol.type_index,
                        Some(data_symbol.name.to_string().to_string()),
                        None,
                        false
                    )?,
                ),
                address,
                line_info.map(|x| x.line_start),
            ));

            writeln!(script_file, "decompile_to_file(0x{address:X}, \"{}\")", module_file_path.to_string_lossy())?;
        }

        pdb2::SymbolData::ThreadStorage(thread_storage_symbol) => {
            let mut file_name_ref = None;
            let mut line_info = None;

            for (name_ref, offsets) in line_offsets.iter() {
                if let Some(line) = offsets.get(&thread_storage_symbol.offset) {
                    file_name_ref = Some(*name_ref);
                    line_info = Some(line.clone());
                    break;
                }
            }

            let path = if let Some(file_name_ref) = file_name_ref {
                PathBuf::from(sanitize_path(file_name_ref.to_string_lossy(string_table.unwrap())?.to_string()))
            } else {
                module_file_path.clone()
            };

            let module_key = path.to_string_lossy().to_lowercase().to_string();
            let module = modules.entry(module_key).or_insert_with(|| cpp::Module::default().with_path(path));

            let rva = match thread_storage_symbol.offset.to_rva(address_map) {
                Some(rva) => rva,
                None => {
                    // println!("WARNING: no RVA found for thread storage symbol: {thread_storage_symbol:?}");
                    return Ok(());
                }
            };

            let address = base_address.unwrap_or(0) + rva.0 as u64;

            if module.members.iter().any(|member| match member {
                cpp::ModuleMember::Data(_, a, _) if *a == address => true,
                _ => false,
            }) {
                return Ok(());
            }

            module.members.push(cpp::ModuleMember::ThreadStorage(
                format!(
                    "thread_local {}; // 0x{address:X}",
                    cpp::type_name(
                        machine_type,
                        type_info,
                        type_finder,
                        thread_storage_symbol.type_index,
                        Some(thread_storage_symbol.name.to_string().to_string()),
                        None,
                        false
                    )?,
                ),
                address,
                line_info.map(|x| x.line_start),
            ));

            writeln!(
                script_file,
                "decompile_to_file(0x{address:X}, \"{}\")",
                module_file_path.to_string_lossy(),
            )?;
        }

        pdb2::SymbolData::Procedure(procedure_symbol) => {
            // println!("Procedure symbol: {}", procedure_symbol.name.to_string());

            let module_symbols = match module_symbols {
                Some(x) => x,
                None => {
                    // println!("WARNING: unhandled global symbol: {symbol_data:?}");
                    return Ok(());
                }
            };

            let (
                register_variable_names,
                register_relative_names,
                body
            ) = parse_procedure_symbols(
                options,
                machine_type,
                base_address.clone(),
                address_map,
                id_finder,
                type_info,
                type_finder,
                module_symbols,
                procedure_symbol,
            )?;
            
            //
            // NOTE:
            //
            // Some PDBs have all the function parameter names in RegisterVariable symbols,
            // but still have extra RegisterRelative symbols.
            //
            // Other PDBs don't have any RegisterVariable symbols at all, and store all
            // the function parameter names in RegisterRelative symbols, but can have more
            // symbols than parameter names.
            //
            // In all cases observed so far, the parameter name symbols come first,
            // then the extra RegisterRelative symbols follow.
            //
            // Are these extra RegisterRelative symbols local variable names?
            //
            // HACK:
            //
            // If we don't have any RegisterVariable symbols, use RegisterRelative symbols.
            //

            let parameters = if register_variable_names.is_empty() {
                register_relative_names
            } else {
                register_variable_names
            };

            let mut file_name_ref = None;
            let mut line_info = None;

            for (name_ref, offsets) in line_offsets.iter() {
                if let Some(line) = offsets.get(&procedure_symbol.offset) {
                    file_name_ref = Some(*name_ref);
                    line_info = Some(line.clone());
                    break;
                }
            }

            let path = if let Some(file_name_ref) = file_name_ref {
                PathBuf::from(sanitize_path(file_name_ref.to_string_lossy(string_table.unwrap())?.to_string()))
            } else {
                module_file_path.clone()
            };

            let module_key = path.to_string_lossy().to_lowercase().to_string();
            let module = modules.entry(module_key).or_insert_with(|| cpp::Module::default().with_path(path));

            let rva = match procedure_symbol.offset.to_rva(address_map) {
                Some(rva) => rva,
                None => {
                    // println!("WARNING: no RVA found for procedure symbol: {procedure_symbol:?}");
                    return Ok(());
                }
            };

            let address = base_address.unwrap_or(0) + rva.0 as u64;

            if module.members.iter().any(|member| match member {
                cpp::ModuleMember::Data(_, a, _) if *a == address => true,
                _ => false,
            }) {
                return Ok(());
            }

            let procedure = cpp::type_name(
                machine_type,
                type_info,
                type_finder,
                procedure_symbol.type_index,
                Some(procedure_symbol.name.to_string().to_string()),
                Some(parameters),
                false,
            )?;

            if procedure.starts_with("...") || procedure.contains('$') || procedure.contains('`') {
                return Ok(());
            }

            module.members.push(
                cpp::ModuleMember::Procedure(cpp::Procedure {
                    address,
                    line: line_info.map(|x| x.line_start),
                    type_index: procedure_symbol.type_index,
                    signature: procedure,
                    body,
                }),
            );

            writeln!(
                script_file,
                "decompile_to_file(0x{address:X}, \"{}\")",
                module_file_path.to_string_lossy(),
            )?;
        }

        pdb2::SymbolData::Thunk(_) => {
            let module_symbols = match module_symbols {
                Some(x) => x,
                None => {
                    // println!("WARNING: unhandled global symbol: {symbol_data:?}");
                    return Ok(());
                }
            };
                            
            parse_thunk_symbols(module_symbols)
        }

        pdb2::SymbolData::SeparatedCode(_) => {
            let module_symbols = match module_symbols {
                Some(x) => x,
                None => {
                    // println!("WARNING: unhandled global symbol: {symbol_data:?}");
                    return Ok(());
                }
            };
            
            parse_separated_code_symbols(module_symbols)
        }

        pdb2::SymbolData::Public(_)
        | pdb2::SymbolData::ProcedureReference(_)
        | pdb2::SymbolData::ObjName(_)
        | pdb2::SymbolData::BuildInfo(_)
        | pdb2::SymbolData::CompileFlags(_)
        | pdb2::SymbolData::Export(_)
        | pdb2::SymbolData::Label(_)
        | pdb2::SymbolData::EnvBlock(_) => {
            // println!("WARNING: Unused {symbol_data:#?}");
        }

        ref data => panic!("Unhandled symbol data in process_module_symbol_data - {data:#?}"),
    }

    Ok(())
}

#[inline(always)]
fn parse_procedure_symbols(
    options: &Options,
    machine_type: pdb2::MachineType,
    base_address: Option<u64>,
    address_map: &pdb2::AddressMap,
    id_finder: &mut pdb2::IdFinder,
    type_info: &pdb2::TypeInformation,
    type_finder: &pdb2::TypeFinder,
    symbols: &mut pdb2::SymbolIter,
    procedure_symbol: &pdb2::ProcedureSymbol,
) -> pdb2::Result<(Vec<String>, Vec<String>, Option<cpp::Block>)> {
    use std::{cell::RefCell, rc::Rc};
    let register_variable_names = Rc::new(RefCell::new(vec![]));
    let register_relative_names = Rc::new(RefCell::new(vec![]));
    
    let mut block = cpp::Block {
        address: None,
        statements: parse_statement_symbols(
            machine_type,
            base_address,
            address_map,
            id_finder,
            type_info,
            type_finder,
            symbols,
            |symbol_data| {
                match symbol_data {
                    pdb2::SymbolData::RegisterVariable(x) => {
                        register_variable_names.borrow_mut().push(x.name.to_string().to_string());
                    }
        
                    pdb2::SymbolData::RegisterRelative(x) => {
                        register_relative_names.borrow_mut().push(x.name.to_string().to_string());
                    }
        
                    _ => {}
                }
        
                Ok(())
            },
        )?,
    };

    let mut flags = vec![];
    if procedure_symbol.flags.nofpo {
        flags.push("nofpo");
    }
    if procedure_symbol.flags.int {
        flags.push("int");
    }
    if procedure_symbol.flags.far {
        flags.push("far");
    }
    if procedure_symbol.flags.never {
        flags.push("never");
    }
    if procedure_symbol.flags.notreached {
        flags.push("notreached");
    }
    if procedure_symbol.flags.cust_call {
        flags.push("cust_call");
    }
    if procedure_symbol.flags.noinline {
        flags.push("noinline");
    }
    if procedure_symbol.flags.optdbginfo {
        flags.push("optdbginfo");
    }
    if !flags.is_empty() {
        block.statements.insert(0, cpp::Statement::Comment(format!(
            "procedure flags: {}",
            flags.join(", "),
        )));
    }

    let register_variable_names = register_variable_names.borrow().to_vec();
    let register_relative_names = register_relative_names.borrow().to_vec();

    Ok((
        register_variable_names,
        register_relative_names,
        if block.statements.is_empty() || !options.unroll_functions {
            None
        } else {
            Some(block)
        }
    ))
}

#[inline(always)]
fn parse_block_symbols(
    machine_type: pdb2::MachineType,
    base_address: Option<u64>,
    address_map: &pdb2::AddressMap,
    id_finder: &mut pdb2::IdFinder,
    type_info: &pdb2::TypeInformation,
    type_finder: &pdb2::TypeFinder,
    symbols: &mut pdb2::SymbolIter,
    address: Option<u64>,
) -> pdb2::Result<cpp::Block> {
    Ok(cpp::Block {
        address,
        statements: parse_statement_symbols(
            machine_type,
            base_address,
            address_map,
            id_finder,
            type_info,
            type_finder,
            symbols,
            |_| Ok(()),
        )?,
    })
}

#[inline(always)]
fn parse_inline_site_symbols(
    machine_type: pdb2::MachineType,
    base_address: Option<u64>,
    address_map: &pdb2::AddressMap,
    id_finder: &mut pdb2::IdFinder,
    type_info: &pdb2::TypeInformation,
    type_finder: &pdb2::TypeFinder,
    symbols: &mut pdb2::SymbolIter,
    inline_site: &pdb2::InlineSiteSymbol,
) -> pdb2::Result<Vec<cpp::Statement>> {
    let id_item = id_finder.find(inline_site.inlinee)?;
    let id_data = id_item.parse()?;
    
    let name = match id_data {
        pdb2::IdData::Function(x) => x.name.to_string().to_string(),
        pdb2::IdData::MemberFunction(x) => x.name.to_string().to_string(),
        _ => todo!(),
    };

    // for annotation in inline_site.annotations.iter().collect::<Vec<_>>()? {
    //     if annotation.emits_line_info() {
    //         todo!("{annotation:#?}");
    //     }
    // }

    let mut statements = parse_statement_symbols(
        machine_type,
        base_address,
        address_map,
        id_finder,
        type_info,
        type_finder,
        symbols,
        |_| Ok(()),
    )?;

    statements.insert(0, cpp::Statement::Comment(format!("inline site start: {name}")));
    statements.push(cpp::Statement::Comment(format!("inline site end: {name}")));

    Ok(statements)
}

fn parse_statement_symbols<F: Clone + FnMut(&pdb2::SymbolData) -> pdb2::Result<()>>(
    machine_type: pdb2::MachineType,
    base_address: Option<u64>,
    address_map: &pdb2::AddressMap,
    id_finder: &mut pdb2::IdFinder,
    type_info: &pdb2::TypeInformation,
    type_finder: &pdb2::TypeFinder,
    symbols: &mut pdb2::SymbolIter,
    f: F,
) -> pdb2::Result<Vec<cpp::Statement>> {
    let mut statements = vec![];

    loop {
        let symbol = match symbols.next() {
            Ok(symbol) => match symbol {
                Some(symbol) => symbol,
                None => break,
            },
            _ => continue,
        };

        let symbol_data = match symbol.parse() {
            Ok(symbol_data) => symbol_data,
            _ => continue,
        };

        f.clone()(&symbol_data)?;

        match symbol_data {
            pdb2::SymbolData::ScopeEnd | pdb2::SymbolData::InlineSiteEnd => break,

            pdb2::SymbolData::RegisterVariable(register_variable_symbol) => {
                statements.push(cpp::Statement::Variable(cpp::Variable {
                    signature: cpp::type_name(
                        machine_type,
                        type_info,
                        type_finder,
                        register_variable_symbol.type_index,
                        Some(register_variable_symbol.name.to_string().to_string()),
                        None,
                        false,
                    )?,
                    value: None,
                    comment: Some(format!("r{}", register_variable_symbol.register.0))
                }));
            }

            pdb2::SymbolData::RegisterRelative(register_relative_symbol) => {
                statements.push(cpp::Statement::Variable(cpp::Variable {
                    signature: cpp::type_name(
                        machine_type,
                        type_info,
                        type_finder,
                        register_relative_symbol.type_index,
                        Some(register_relative_symbol.name.to_string().to_string()),
                        None,
                        false,
                    )?,
                    value: None,
                    comment: Some(format!("r{} offset {}", register_relative_symbol.register.0, register_relative_symbol.offset))
                }));
            }

            pdb2::SymbolData::Block(block_symbol) => {
                statements.push(cpp::Statement::Block(parse_block_symbols(
                    machine_type,
                    base_address.clone(),
                    address_map,
                    id_finder,
                    type_info,
                    type_finder,
                    symbols,
                    block_symbol.offset.to_rva(address_map).map(|rva| base_address.unwrap_or(0) + rva.0 as u64),
                )?));
            }

            pdb2::SymbolData::InlineSite(inline_site_symbol) => {
                statements.extend(parse_inline_site_symbols(
                    machine_type,
                    base_address.clone(),
                    address_map,
                    id_finder,
                    type_info,
                    type_finder,
                    symbols,
                    &inline_site_symbol,
                )?);
            }

            pdb2::SymbolData::SeparatedCode(separated_code_symbol) => {
                let address = separated_code_symbol.offset
                    .to_rva(address_map)
                    .map(|rva| base_address.unwrap_or(0) + rva.0 as u64);

                if let Some(address) = address {
                    statements.push(cpp::Statement::Comment(format!(
                        "separated code start @ 0x{address:X}",
                    )));
                }

                parse_separated_code_symbols(symbols);
            }

            pdb2::SymbolData::Constant(constant_symbol) => {
                statements.push(cpp::Statement::Variable(cpp::Variable {
                    signature: cpp::type_name(
                        machine_type,
                        type_info,
                        type_finder,
                        constant_symbol.type_index,
                        Some(constant_symbol.name.to_string().to_string()),
                        None,
                        false,
                    )?,
                    value: None,
                    comment: Some(format!(
                        "constant == {}",
                        match &constant_symbol.value {
                            pdb2::Variant::U8(x) => format!("{x}"),
                            pdb2::Variant::U16(x) => format!("{x}"),
                            pdb2::Variant::U32(x) => format!("{x}"),
                            pdb2::Variant::U64(x) => format!("{x}"),
                            pdb2::Variant::I8(x) => format!("{x}"),
                            pdb2::Variant::I16(x) => format!("{x}"),
                            pdb2::Variant::I32(x) => format!("{x}"),
                            pdb2::Variant::I64(x) => format!("{x}"),
                        },
                    ))
                }));
            }

            pdb2::SymbolData::Data(data_symbol) => {
                statements.push(cpp::Statement::Variable(cpp::Variable {
                    signature: cpp::type_name(
                        machine_type,
                        type_info,
                        type_finder,
                        data_symbol.type_index,
                        Some(data_symbol.name.to_string().to_string()),
                        None,
                        false,
                    )?,
                    value: None,
                    comment: data_symbol.offset.to_rva(address_map).map(|rva| {
                        format!("data @ 0x{:X}", base_address.unwrap_or(0) + rva.0 as u64)
                    }),
                }));
            }

            pdb2::SymbolData::Local(local_symbol) => {
                statements.push(cpp::Statement::Variable(cpp::Variable {
                    signature: cpp::type_name(
                        machine_type,
                        type_info,
                        type_finder,
                        local_symbol.type_index,
                        Some(local_symbol.name.to_string().to_string()),
                        None,
                        false,
                    )?,
                    value: None,
                    comment: Some("local".into()),
                }));
            }

            pdb2::SymbolData::Label(label_symbol) => {
                match label_symbol.offset.to_rva(address_map) {
                    Some(rva) => {
                        let address = base_address.unwrap_or(0) + rva.0 as u64;
                        
                        statements.push(cpp::Statement::Label(cpp::Label {
                            name: label_symbol.name.to_string().to_string(),
                            address,
                        }));
                    }

                    None => {
                        // println!("WARNING: no RVA found for label symbol: {symbol_data:?}");
                    }
                }
            }
            
            pdb2::SymbolData::ThreadStorage(tls_symbol) => {
                statements.push(cpp::Statement::Variable(cpp::Variable {
                    signature: format!(
                        "thread_local {}",
                        cpp::type_name(
                            machine_type,
                            type_info,
                            type_finder,
                            tls_symbol.type_index,
                            Some(tls_symbol.name.to_string().to_string()),
                            None,
                            false,
                        )?,
                    ),
                    value: None,
                    comment: tls_symbol.offset.to_rva(address_map).map(|rva| {
                        format!("thread storage @ 0x{:X}", base_address.unwrap_or(0) + rva.0 as u64)
                    }),
                }));
            }

            pdb2::SymbolData::UserDefinedType(_) => {
                // println!("WARNING: Unused {symbol_data:#?}");
            }

            _ => panic!("Unhandled symbol data in parse_statement_symbols - {symbol_data:?}"),
        }
    }

    Ok(statements)
}

#[inline(always)]
fn parse_thunk_symbols(symbols: &mut pdb2::SymbolIter) {
    loop {
        let symbol = match symbols.next() {
            Ok(symbol) => match symbol {
                Some(symbol) => symbol,
                None => break,
            },
            _ => continue,
        };

        let symbol_data = match symbol.parse() {
            Ok(symbol_data) => symbol_data,
            _ => continue,
        };

        match symbol_data {
            pdb2::SymbolData::ScopeEnd => {
                break;
            }

            _ => panic!("Unhandled symbol data in parse_thunk_symbols - {symbol_data:?}"),
        }
    }
}

#[inline(always)]
fn parse_separated_code_symbols(symbols: &mut pdb2::SymbolIter) {
    loop {
        let symbol = match symbols.next() {
            Ok(symbol) => match symbol {
                Some(symbol) => symbol,
                None => break,
            },
            _ => continue,
        };

        let symbol_data = match symbol.parse() {
            Ok(symbol_data) => symbol_data,
            _ => continue,
        };

        match symbol_data {
            pdb2::SymbolData::ScopeEnd => {
                break;
            }

            _ => panic!("Unhandled symbol data in parse_separated_code_symbols - {symbol_data:?}"),
        }
    }
}
