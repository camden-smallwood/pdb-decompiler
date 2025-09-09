#![allow(dead_code, unused_imports)]

mod cpp;
mod tabbed;

use pdb2::FallibleIterator;
use std::{
    cell::RefCell, collections::HashMap, error::Error, fs::{self, File, OpenOptions}, io::Write, num, path::{Path, PathBuf}, rc::Rc
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

    /// The base address to add when resolving an RVA. (Optional)
    #[structopt(short, long, parse(try_from_str = parse_base_address))]
    base_address: Option<u64>,

    /// Whether to include scope information in decompiled function stubs. (Experimental)
    #[structopt(short, long)]
    unroll_functions: bool,

    /// Whether to reorganize generated C++ code to Bungie's coding standards. (Experimental)
    #[structopt(short, long)]
    reorganize: bool,
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
    
    let mut class_table = vec![];
    let mut type_sizes = HashMap::new();

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
        &mut class_table,
        &mut type_sizes,
        machine_type,
        &type_info,
        &mut type_finder,
        id_info.is_empty(),
    ).map_err(|e| format!("failed to process type info: {e}"))?;
    
    process_id_information(
        options,
        &mut class_table,
        &mut type_sizes,
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
        &mut class_table,
        &mut type_sizes,
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
    class_table: &mut Vec<Rc<RefCell<cpp::Class>>>,
    type_sizes: &mut HashMap<String, u64>,
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
        
        let type_data = match type_item.parse() {
            Ok(x) => x,
            Err(_) => {
                // println!("WARNING: Failed to parse type: {e} - {type_item:#?}");
                continue;
            }
        };

        if let Some((name, size)) = match type_data.clone() {
            pdb2::TypeData::Class(class_type) => Some((class_type.name.to_string().to_string(), class_type.size)),
            pdb2::TypeData::Union(union_type) => Some((union_type.name.to_string().to_string(), union_type.size)),
            _ => None,
        } {
            if size != 0 {
                *type_sizes.entry(name).or_default() = size;
            }
        }

        if let Some(forward_reference) = match type_data {
            pdb2::TypeData::Class(class_type) => Some(class_type.properties.forward_reference()),
            pdb2::TypeData::Enumeration(enum_type) => Some(enum_type.properties.forward_reference()),
            pdb2::TypeData::Union(union_type) => Some(union_type.properties.forward_reference()),
            _ => None,
        } {
            if forward_reference {
                if export_all {
                    exported_forward_reference_indices.push(type_item.index());
                }
            } else {
                if export_all {
                    exported_type_indices.push(type_item.index());
                }
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
        if let Err(e) = module.add_type_definition(class_table, type_sizes, machine_type, type_info, type_finder, type_index, 0) {
            panic!("{e}");
        }
    }

    for type_index in exported_type_indices {
        if let Err(e) = module.add_type_definition(class_table, type_sizes, machine_type, type_info, type_finder, type_index, 0) {
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
    class_table: &mut Vec<Rc<RefCell<cpp::Class>>>,
    type_sizes: &mut HashMap<String, u64>,
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
                    .add_type_definition(class_table, type_sizes, machine_type, type_info, type_finder, data.udt, data.line)?;
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
    class_table: &mut Vec<Rc<RefCell<cpp::Class>>>,
    type_sizes: &mut HashMap<String, u64>,
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
            class_table,
            type_sizes,
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

    fn find_enum_or_flags_typedef<'a>(
        modules: &mut HashMap<String, cpp::Module>,
        enum_data: &cpp::Enum,
    ) -> Option<cpp::TypeDefinition> {
        for module in modules.values() {
            for member in module.members.iter() {
                let cpp::ModuleMember::TypeDefinition(type_definition) = member else {
                    continue;
                };
                
                fn parse_it<T: std::str::FromStr + PartialEq>(input: T, max: T, value: &str) -> bool {
                    let min_value = match value.parse() {
                        Ok(x) => x,
                        Err(_) => {
                            if value == "-1" {
                                max
                            } else {
                                panic!("{}", value)
                            }
                        }
                    };
                    
                    if input == min_value {
                        return true;
                    }

                    false
                }

                if type_definition.type_name.starts_with(format!("c_enum<enum {},", enum_data.name).as_str()) {
                    let mut result = type_definition.clone();
                    result.type_name = format!("c_enum<{}", result.type_name.trim_start_matches("c_enum<enum "));

                    let Some((type_string, name_string)) = result.type_name.rsplit_once(' ') else {
                        panic!("Unexpected typedef type name string: \"{}\"", result.type_name);
                    };

                    let mut template_args = type_string
                        .trim_start_matches("c_enum<")
                        .trim_end_matches(">")
                        .split(",")
                        .map(|s| s.to_string())
                        .collect::<Vec<_>>();

                    assert!(template_args.len() == 4);

                    template_args.iter_mut().for_each(|a| *a = a.trim_start().trim_end().into());

                    let mut min_found = false;
                    let mut max_found = false;

                    for value in enum_data.values.iter() {
                        if min_found && max_found {
                            break;
                        }

                        match &value.value {
                            pdb2::Variant::U8(x) => {
                                if !min_found {
                                    if parse_it(*x, u8::MAX, template_args[2].as_str()) {
                                        template_args[2] = value.name.clone();
                                        min_found = true;
                                    }
                                }

                                if !max_found {
                                    if parse_it(*x, u8::MAX, template_args[3].as_str()) {
                                        template_args[3] = value.name.clone();
                                        max_found = true;
                                    }
                                }
                            }

                            pdb2::Variant::U16(x) => {
                                if !min_found {
                                    if parse_it(*x, u16::MAX, template_args[2].as_str()) {
                                        template_args[2] = value.name.clone();
                                        min_found = true;
                                    }
                                }

                                if !max_found {
                                    if parse_it(*x, u16::MAX, template_args[3].as_str()) {
                                        template_args[3] = value.name.clone();
                                        max_found = true;
                                    }
                                }
                            }

                            pdb2::Variant::U32(x) => {
                                if !min_found {
                                    if parse_it(*x, u32::MAX, template_args[2].as_str()) {
                                        template_args[2] = value.name.clone();
                                        min_found = true;
                                    }
                                }

                                if !max_found {
                                    if parse_it(*x, u32::MAX, template_args[3].as_str()) {
                                        template_args[3] = value.name.clone();
                                        max_found = true;
                                    }
                                }
                            }

                            pdb2::Variant::U64(x) => {
                                if !min_found {
                                    if parse_it(*x, u64::MAX, template_args[2].as_str()) {
                                        template_args[2] = value.name.clone();
                                        min_found = true;
                                    }
                                }

                                if !max_found {
                                    if parse_it(*x, u64::MAX, template_args[3].as_str()) {
                                        template_args[3] = value.name.clone();
                                        max_found = true;
                                    }
                                }
                            }

                            pdb2::Variant::I8(x) => {
                                if !min_found {
                                    if parse_it(*x, i8::MAX, template_args[2].as_str()) {
                                        template_args[2] = value.name.clone();
                                        min_found = true;
                                    }
                                }

                                if !max_found {
                                    if parse_it(*x, i8::MAX, template_args[3].as_str()) {
                                        template_args[3] = value.name.clone();
                                        max_found = true;
                                    }
                                }
                            }

                            pdb2::Variant::I16(x) => {
                                if !min_found {
                                    if parse_it(*x, i16::MAX, template_args[2].as_str()) {
                                        template_args[2] = value.name.clone();
                                        min_found = true;
                                    }
                                }

                                if !max_found {
                                    if parse_it(*x, i16::MAX, template_args[3].as_str()) {
                                        template_args[3] = value.name.clone();
                                        max_found = true;
                                    }
                                }
                            }

                            pdb2::Variant::I32(x) => {
                                if !min_found {
                                    if parse_it(*x, i32::MAX, template_args[2].as_str()) {
                                        template_args[2] = value.name.clone();
                                        min_found = true;
                                    }
                                }

                                if !max_found {
                                    if parse_it(*x, i32::MAX, template_args[3].as_str()) {
                                        template_args[3] = value.name.clone();
                                        max_found = true;
                                    }
                                }
                            }

                            pdb2::Variant::I64(x) => {
                                if !min_found {
                                    if parse_it(*x, i64::MAX, template_args[2].as_str()) {
                                        template_args[2] = value.name.clone();
                                        min_found = true;
                                    }
                                }

                                if !max_found {
                                    if parse_it(*x, i64::MAX, template_args[3].as_str()) {
                                        template_args[3] = value.name.clone();
                                        max_found = true;
                                    }
                                }
                            }
                        }
                    }

                    result.type_name = format!("c_enum<{}> {}", template_args.join(","), name_string);

                    return Some(result);
                }

                if type_definition.type_name.starts_with(format!("c_flags<enum {},", enum_data.name).as_str()) {
                    let mut result = type_definition.clone();
                    result.type_name = format!("c_flags<{}", result.type_name.trim_start_matches("c_flags<enum "));

                    let Some((type_string, name_string)) = result.type_name.rsplit_once(' ') else {
                        panic!("Unexpected typedef type name string: \"{}\"", result.type_name);
                    };

                    let mut template_args = type_string
                        .trim_start_matches("c_flags<")
                        .trim_end_matches(">")
                        .split(",")
                        .map(|s| s.to_string())
                        .collect::<Vec<_>>();
                    assert!(template_args.len() == 3);

                    template_args.iter_mut().for_each(|a| *a = a.trim_start().trim_end().into());

                    let mut max_found = false;

                    for value in enum_data.values.iter() {
                        if max_found {
                            break;
                        }

                        match &value.value {
                            pdb2::Variant::U8(x) => {
                                if !max_found {
                                    if parse_it(*x, u8::MAX, template_args[2].as_str()) {
                                        template_args[2] = value.name.clone();
                                        max_found = true;
                                    }
                                }
                            }

                            pdb2::Variant::U16(x) => {
                                if !max_found {
                                    if parse_it(*x, u16::MAX, template_args[2].as_str()) {
                                        template_args[2] = value.name.clone();
                                        max_found = true;
                                    }
                                }
                            }

                            pdb2::Variant::U32(x) => {
                                if !max_found {
                                    if parse_it(*x, u32::MAX, template_args[2].as_str()) {
                                        template_args[2] = value.name.clone();
                                        max_found = true;
                                    }
                                }
                            }

                            pdb2::Variant::U64(x) => {
                                if !max_found {
                                    if parse_it(*x, u64::MAX, template_args[2].as_str()) {
                                        template_args[2] = value.name.clone();
                                        max_found = true;
                                    }
                                }
                            }

                            pdb2::Variant::I8(x) => {
                                if !max_found {
                                    if parse_it(*x, i8::MAX, template_args[2].as_str()) {
                                        template_args[2] = value.name.clone();
                                        max_found = true;
                                    }
                                }
                            }

                            pdb2::Variant::I16(x) => {
                                if !max_found {
                                    if parse_it(*x, i16::MAX, template_args[2].as_str()) {
                                        template_args[2] = value.name.clone();
                                        max_found = true;
                                    }
                                }
                            }

                            pdb2::Variant::I32(x) => {
                                if !max_found {
                                    if parse_it(*x, i32::MAX, template_args[2].as_str()) {
                                        template_args[2] = value.name.clone();
                                        max_found = true;
                                    }
                                }
                            }

                            pdb2::Variant::I64(x) => {
                                if !max_found {
                                    if parse_it(*x, i64::MAX, template_args[2].as_str()) {
                                        template_args[2] = value.name.clone();
                                        max_found = true;
                                    }
                                }
                            }
                        }
                    }

                    result.type_name = format!("c_flags<{}> {}", template_args.join(","), name_string);

                    return Some(result);
                }

                if type_definition.type_name.starts_with(format!("c_flags_no_init<enum {},", enum_data.name).as_str()) {
                    let mut result = type_definition.clone();
                    result.type_name = format!("c_flags_no_init<{}", result.type_name.trim_start_matches("c_flags_no_init<enum "));

                    let Some((type_string, name_string)) = result.type_name.rsplit_once(' ') else {
                        panic!("Unexpected typedef type name string: \"{}\"", result.type_name);
                    };

                    let mut template_args = type_string
                        .trim_start_matches("c_flags_no_init<")
                        .trim_end_matches(">")
                        .split(",")
                        .map(|s| s.to_string())
                        .collect::<Vec<_>>();
                    assert!(template_args.len() == 3);

                    template_args.iter_mut().for_each(|a| *a = a.trim_start().trim_end().into());

                    let mut max_found = false;

                    for value in enum_data.values.iter() {
                        if max_found {
                            break;
                        }

                        match &value.value {
                            pdb2::Variant::U8(x) => {
                                if !max_found {
                                    if parse_it(*x, u8::MAX, template_args[2].as_str()) {
                                        template_args[2] = value.name.clone();
                                        max_found = true;
                                    }
                                }
                            }

                            pdb2::Variant::U16(x) => {
                                if !max_found {
                                    if parse_it(*x, u16::MAX, template_args[2].as_str()) {
                                        template_args[2] = value.name.clone();
                                        max_found = true;
                                    }
                                }
                            }

                            pdb2::Variant::U32(x) => {
                                if !max_found {
                                    if parse_it(*x, u32::MAX, template_args[2].as_str()) {
                                        template_args[2] = value.name.clone();
                                        max_found = true;
                                    }
                                }
                            }

                            pdb2::Variant::U64(x) => {
                                if !max_found {
                                    if parse_it(*x, u64::MAX, template_args[2].as_str()) {
                                        template_args[2] = value.name.clone();
                                        max_found = true;
                                    }
                                }
                            }

                            pdb2::Variant::I8(x) => {
                                if !max_found {
                                    if parse_it(*x, i8::MAX, template_args[2].as_str()) {
                                        template_args[2] = value.name.clone();
                                        max_found = true;
                                    }
                                }
                            }

                            pdb2::Variant::I16(x) => {
                                if !max_found {
                                    if parse_it(*x, i16::MAX, template_args[2].as_str()) {
                                        template_args[2] = value.name.clone();
                                        max_found = true;
                                    }
                                }
                            }

                            pdb2::Variant::I32(x) => {
                                if !max_found {
                                    if parse_it(*x, i32::MAX, template_args[2].as_str()) {
                                        template_args[2] = value.name.clone();
                                        max_found = true;
                                    }
                                }
                            }

                            pdb2::Variant::I64(x) => {
                                if !max_found {
                                    if parse_it(*x, i64::MAX, template_args[2].as_str()) {
                                        template_args[2] = value.name.clone();
                                        max_found = true;
                                    }
                                }
                            }
                        }
                    }

                    result.type_name = format!("c_flags_no_init<{}> {}", template_args.join(","), name_string);

                    return Some(result);
                }
            }
        }

        None
    }

    //
    // Find c_enum and c_flags typedefs
    //

    let mut compound_enums = vec![];

    if options.reorganize {
        for module in modules.values().cloned().collect::<Vec<_>>() {
            for member in module.members.clone() {
                if let cpp::ModuleMember::Enum(enum_definition) = member
                    && let Some(type_definition) = find_enum_or_flags_typedef(modules, &enum_definition)
                {
                    compound_enums.push((enum_definition.clone(), type_definition));
                }
            }
        }
    }
    
    for mut module in modules.values().cloned() {
        let path = PathBuf::from(sanitize_path(format!(
            "{}/{}",
            options.out.as_ref().unwrap().to_string_lossy(),
            module.path.to_string_lossy().trim_start_matches('/'),
        )));

        //
        // Sort module members by line number
        //

        let mut storage: Vec<(u32, cpp::ModuleMember)> = vec![];
        let mut prev_line = 0;

        for u in module.members.iter() {
            match u {
                cpp::ModuleMember::Class(x) => {
                    storage.push((x.borrow().line, u.clone()));
                    prev_line = x.borrow().line;
                }

                cpp::ModuleMember::Enum(x) => {
                    storage.push((x.line, u.clone()));
                    prev_line = x.line;
                }

                cpp::ModuleMember::Data { line: Some(line), .. } => {
                    storage.push((*line, u.clone()));
                    prev_line = *line;
                }

                cpp::ModuleMember::ThreadStorage { line: Some(line), .. } => {
                    storage.push((*line, u.clone()));
                    prev_line = *line;
                }

                cpp::ModuleMember::Procedure(cpp::Procedure { line: Some(line), .. }) => {
                    storage.push((*line, u.clone()));
                    prev_line = *line;
                }

                _ => {
                    prev_line += 1;
                    storage.push((prev_line, u.clone()));
                }
            }
        }

        storage.sort_by(|a, b| a.0.cmp(&b.0));
        let mut members = storage.iter().map(|m| m.1.clone()).collect::<Vec<_>>();

        //
        // Replace nested type declarations and anonymous field types with their full types
        //
        
        let cloned_members = members.clone();
        let mut new_members = vec![];
        let mut remove_class_names = vec![];
        let mut remove_enum_names = vec![];

        for (i, member) in members.iter_mut().enumerate() {
            match member {
                cpp::ModuleMember::Class(class_data) => {
                    fn fix_class(
                        module_members: &[cpp::ModuleMember],
                        i: usize,
                        class_data: Rc<RefCell<cpp::Class>>,
                        remove_class_names: &mut Vec<String>,
                        remove_enum_names: &mut Vec<String>,
                    ) {
                        let class_name = class_data.borrow().name.clone();
                        let mut new_members = class_data.borrow_mut().members.clone();

                        for class_member in new_members.iter_mut() {
                            match class_member {
                                cpp::ClassMember::Class(nested_class) => {
                                    let is_declaration = nested_class.borrow().is_declaration;
                                    let nested_name = nested_class.borrow().name.clone();
                                    let full_name = format!("{}::{}", class_name, nested_name);

                                    if is_declaration {
                                        let found = module_members.iter().enumerate().find(|(ii, m)| {
                                            if i == *ii {
                                                return false;
                                            }
                                            let cpp::ModuleMember::Class(other_class) = m else {
                                                return false;
                                            };
                                            other_class.borrow().name == full_name
                                        });

                                        if let Some((_, found_member)) = found {
                                            let cpp::ModuleMember::Class(other_class) = found_member else {
                                                unreachable!()
                                            };

                                            let mut nested_class = nested_class.borrow_mut();
                                            nested_class.is_declaration = false;
                                            nested_class.size = other_class.borrow().size;
                                            nested_class.base_classes = other_class.borrow().base_classes.clone();
                                            nested_class.members = other_class.borrow().members.clone();

                                            let nested_depth = nested_class.depth + 1;

                                            for member in nested_class.members.iter_mut() {
                                                match member {
                                                    cpp::ClassMember::Class(inner_class) => inner_class.borrow_mut().depth = nested_depth,
                                                    cpp::ClassMember::Enum(inner_enum) => inner_enum.depth = nested_depth,
                                                    _ => {}
                                                }
                                            }
                                        }
                                    }

                                    fix_class(module_members, i, nested_class.clone(), remove_class_names, remove_enum_names);
                                    
                                    if !remove_class_names.contains(&full_name) {
                                        remove_class_names.push(full_name);
                                    }
                                }

                                cpp::ClassMember::Enum(nested_enum) => {
                                    let full_name = format!("{}::{}", class_name, nested_enum.name);
                                    
                                    if nested_enum.is_declaration {
                                        let found = module_members.iter().enumerate().find(|(ii, m)| {
                                            if i == *ii {
                                                return false;
                                            }

                                            let cpp::ModuleMember::Enum(other_enum) = m else {
                                                return false;
                                            };

                                            other_enum.name == full_name
                                        });

                                        if let Some((_, found_member)) = found {
                                            let cpp::ModuleMember::Enum(other_enum) = found_member else {
                                                unreachable!()
                                            };

                                            nested_enum.is_declaration = false;
                                            nested_enum.underlying_type_name = other_enum.underlying_type_name.clone();
                                            nested_enum.size = other_enum.size;
                                            nested_enum.values = other_enum.values.clone();
                                        }
                                    }

                                    if !remove_enum_names.contains(&full_name) {
                                        remove_enum_names.push(full_name);
                                    }
                                }

                                cpp::ClassMember::Field(field) => {
                                    if field.type_name.contains("::<unnamed") {
                                        let parts = field.type_name.split("::").collect::<Vec<_>>();

                                        if !parts.is_empty() && parts.last().unwrap().starts_with("<unnamed") {
                                            let full_name = field.type_name.clone();

                                            let found = module_members.iter().enumerate().find(|(ii, m)| {
                                                if i == *ii {
                                                    return false;
                                                }
                                                let cpp::ModuleMember::Class(other_class) = m else {
                                                    return false;
                                                };
                                                other_class.borrow().name == full_name
                                            });

                                            if let Some((_, found_member)) = found {
                                                let cpp::ModuleMember::Class(other_class) = found_member else {
                                                    unreachable!()
                                                };

                                                fix_class(module_members, i, other_class.clone(), remove_class_names, remove_enum_names);

                                                let mut other_class = other_class.borrow().clone();
                                                other_class.name = String::new();
                                                other_class.depth = class_data.borrow().depth + 1;
                                                
                                                field.type_name = other_class
                                                    .to_string()
                                                    .trim_start()
                                                    .trim_end_matches(";")
                                                    .to_string();
                                            
                                                field.display = format!("{} {}", field.type_name, field.name);

                                                if !remove_class_names.contains(&full_name) {
                                                    remove_class_names.push(full_name);
                                                }
                                            }
                                        }
                                    }
                                }

                                _ => {}
                            }
                        }
                    
                        class_data.borrow_mut().members = new_members;
                    }
                
                    fix_class(cloned_members.as_slice(), i, class_data.clone(), &mut remove_class_names, &mut remove_enum_names);

                    new_members.push(cpp::ModuleMember::Class(class_data.clone()));
                }

                _ => {
                    new_members.push(member.clone());
                }
            }
        }

        members = new_members;

        //
        // Remove nested types that ended up in toplevel and are now unreferenced
        //

        for class_name in remove_class_names.into_iter().rev() {
            for i in (0..module.members.len()).rev() {
                if let cpp::ModuleMember::Class(class_data) = &module.members[i]
                    && class_data.borrow().name == class_name
                {
                    module.members.remove(i);
                }
            }
        }

        for enum_name in remove_enum_names.into_iter().rev() {
            for i in (0..module.members.len()).rev() {
                if let cpp::ModuleMember::Enum(enum_data) = &module.members[i]
                    && enum_data.name == enum_name
                {
                    module.members.remove(i);
                }
            }
        }

        if options.reorganize {
            //
            // Rebuild module under specific sections
            //

            let mut new_members = vec![];

            //--------------------------------------------------------------------------------
            // starting preprocessor statements
            //--------------------------------------------------------------------------------

            if module.is_header() {
                let stem = module.path
                    .file_stem()
                    .and_then(std::ffi::OsStr::to_str)
                    .unwrap_or("UNKNOWN")
                    .to_uppercase();

                new_members.push(cpp::ModuleMember::Preprocessor(format!("ifndef __{}_H__", stem)));
                new_members.push(cpp::ModuleMember::Preprocessor(format!("define __{}_H__", stem)));
                new_members.push(cpp::ModuleMember::Preprocessor("pragma once".into()));
                new_members.push(cpp::ModuleMember::EmptyLine);
            }
            
            //--------------------------------------------------------------------------------
            // headers
            //--------------------------------------------------------------------------------

            if !module.headers.is_empty() {
                new_members.push(cpp::ModuleMember::Comment("---------- headers".into()));
                new_members.push(cpp::ModuleMember::EmptyLine);

                for (path, global) in module.headers.iter() {
                    new_members.push(cpp::ModuleMember::Include(*global, path.clone()));
                }

                new_members.push(cpp::ModuleMember::EmptyLine);

                module.headers.clear();
            }

            //--------------------------------------------------------------------------------
            // using namespaces
            //--------------------------------------------------------------------------------

            let using_namespace_members = members.iter().filter(|m| match m {
                cpp::ModuleMember::UsingNamespace(_) => true,
                _ => false,
            }).cloned().collect::<Vec<_>>();

            if !using_namespace_members.is_empty() {
                new_members.extend(using_namespace_members);
                new_members.push(cpp::ModuleMember::EmptyLine);
            }

            //--------------------------------------------------------------------------------
            // enums, const vars, macros
            //--------------------------------------------------------------------------------

            let constant_members = members.iter().filter(|m| match m {
                cpp::ModuleMember::Enum(_) => true,
                // cpp::ModuleMember::Constant(_) => true,
                cpp::ModuleMember::Data { signature, .. } => signature.starts_with("const ") && !signature.contains("$"),
                cpp::ModuleMember::ThreadStorage { signature, .. } => signature.starts_with("const ") && !signature.contains("$"),
                _ => false,
            }).cloned().collect::<Vec<_>>();

            if !constant_members.is_empty() {
                new_members.push(cpp::ModuleMember::Comment("---------- constants".into()));
                new_members.push(cpp::ModuleMember::EmptyLine);

                for member in constant_members {
                    match member {
                        cpp::ModuleMember::Enum(enum_data) => {
                            new_members.push(cpp::ModuleMember::Enum(enum_data.clone()));

                            if let Some((_, type_definition)) = compound_enums.iter().find(|(e, _)| enum_data == *e) {
                                new_members.push(cpp::ModuleMember::TypeDefinition(type_definition.clone()));
                            }

                            new_members.push(cpp::ModuleMember::EmptyLine);
                        }

                        _ => new_members.push(member),
                    }
                }

                while let Some(cpp::ModuleMember::EmptyLine) = new_members.last() {
                    new_members.pop();
                }

                new_members.push(cpp::ModuleMember::EmptyLine);
            }

            //--------------------------------------------------------------------------------
            // structs/unions/classes/typedefs
            //--------------------------------------------------------------------------------

            let definition_members = members.iter().filter(|m| match m {
                cpp::ModuleMember::Class(_) => true,
                cpp::ModuleMember::TypeDefinition(_) => true,
                _ => false,
            }).cloned().collect::<Vec<_>>();

            let mut new_definition_members = vec![];

            for member in definition_members {
                match &member {
                    cpp::ModuleMember::Class(_) => {
                        if let Some(cpp::ModuleMember::TypeDefinition(_)) = new_definition_members.last() {
                            new_definition_members.push(cpp::ModuleMember::EmptyLine);
                        }

                        new_definition_members.push(member);
                        new_definition_members.push(cpp::ModuleMember::EmptyLine);
                    }

                    cpp::ModuleMember::TypeDefinition(_) => {
                        new_definition_members.push(member);
                    }

                    _ => todo!()
                }
            }

            if !new_definition_members.is_empty() {
                new_members.push(cpp::ModuleMember::Comment("---------- definitions".into()));
                new_members.push(cpp::ModuleMember::EmptyLine);

                while let Some(cpp::ModuleMember::EmptyLine) = new_definition_members.last() {
                    new_definition_members.pop();
                }

                new_members.extend(new_definition_members);
                new_members.push(cpp::ModuleMember::EmptyLine);
            }

            //--------------------------------------------------------------------------------
            // function prototypes
            //--------------------------------------------------------------------------------

            let public_code_members = members.iter().filter(|m| match m {
                cpp::ModuleMember::Procedure(procedure) => {
                    !procedure.is_static
                        && !procedure.signature.contains("`")
                        && !procedure.signature.contains("$")
                }
                _ => false,
            }).cloned().collect::<Vec<_>>();

            let private_code_members = members.iter().filter(|m| match m {
                cpp::ModuleMember::Procedure(procedure) => {
                    procedure.is_static
                        && !procedure.signature.contains("`")
                        && !procedure.signature.contains("$")
                }
                _ => false,
            }).cloned().collect::<Vec<_>>();

            let mut prototype_members = vec![];

            for member in public_code_members.iter() {
                if let cpp::ModuleMember::Procedure(procedure) = member {
                    let mut procedure = procedure.clone();

                    let is_member_function = matches!(
                        type_finder.find(procedure.type_index)?.parse()?,
                        pdb2::TypeData::MemberFunction(_)
                    );

                    procedure.body = None;
                    procedure.address = 0;
                    
                    if !is_member_function {
                        prototype_members.push(cpp::ModuleMember::Tagged(
                            "extern".into(),
                            Box::new(cpp::ModuleMember::Procedure(procedure)),
                        ));
                    }
                }
            }

            if !public_code_members.is_empty() && !private_code_members.is_empty() {
                prototype_members.push(cpp::ModuleMember::EmptyLine);
            }

            for member in private_code_members.iter() {
                if let cpp::ModuleMember::Procedure(procedure) = member {
                    let mut procedure = procedure.clone();

                    procedure.body = None;
                    procedure.address = 0;
                    procedure.is_static = false;

                    prototype_members.push(cpp::ModuleMember::Tagged(
                        "_static".into(),
                        Box::new(cpp::ModuleMember::Procedure(procedure)),
                    ));
                }
            }

            if !prototype_members.is_empty() {
                new_members.push(cpp::ModuleMember::Comment("---------- prototypes".into()));
                new_members.push(cpp::ModuleMember::EmptyLine);
                new_members.extend(prototype_members);
                new_members.push(cpp::ModuleMember::EmptyLine);
            }

            //--------------------------------------------------------------------------------
            // public mutable vars
            //--------------------------------------------------------------------------------

            let global_members = members.iter().filter(|m| match m {
                cpp::ModuleMember::Data { is_static: false, signature, .. } => {
                    !signature.starts_with("const ")
                        && !signature.contains("`")
                        && !signature.contains("$")
                }
                cpp::ModuleMember::ThreadStorage { signature, .. } => {
                    !signature.starts_with("const ")
                        && !signature.contains("`")
                        && !signature.contains("$")
                }
                _ => false,
            }).cloned().collect::<Vec<_>>();

            let mut new_global_members = vec![];

            for mut member in global_members {
                let mut create_empty_line = false;

                // Update thread_local variables
                if let cpp::ModuleMember::ThreadStorage { name, signature, address, .. } = &mut member {
                    if !matches!(new_global_members.last(), Some(cpp::ModuleMember::EmptyLine)) {
                        new_global_members.push(cpp::ModuleMember::EmptyLine);
                    }

                    new_global_members.push(cpp::ModuleMember::FunctionCall(
                        "static_warning".into(),
                        vec![
                            format!("\"TODO: fix {name} declaration\""),
                        ],
                    ));

                    let Some((declaration, _comment)) = signature.rsplit_once(';') else {
                        panic!("Malformed thread storage signature: \"{}\"", signature);
                    };

                    *signature = format!("{declaration} = tls_get<decltype({name})>(0x{address:X});");
                    
                    create_empty_line = true;
                }

                new_global_members.push(member);

                if create_empty_line {
                    new_global_members.push(cpp::ModuleMember::EmptyLine);
                }
            }

            if !new_global_members.is_empty() {
                new_members.push(cpp::ModuleMember::Comment("---------- globals".into()));
                new_members.push(cpp::ModuleMember::EmptyLine);

                while let Some(cpp::ModuleMember::EmptyLine) = new_global_members.last() {
                    new_global_members.pop();
                }

                new_members.extend(new_global_members);

                new_members.push(cpp::ModuleMember::EmptyLine);
            }
            
            //--------------------------------------------------------------------------------
            // private mutable vars
            //--------------------------------------------------------------------------------

            let private_variable_members = members.iter().filter(|m| match m {
                cpp::ModuleMember::Data { is_static: true, signature, .. } => {
                    !signature.starts_with("const ")
                        && !signature.contains("`")
                        && !signature.contains("$")
                }
                _ => false,
            }).cloned().collect::<Vec<_>>();

            let mut new_private_variable_members = vec![];

            for member in private_variable_members {
                new_private_variable_members.push(member);
            }

            if !new_private_variable_members.is_empty() {
                new_members.push(cpp::ModuleMember::Comment("---------- private variables".into()));
                new_members.push(cpp::ModuleMember::EmptyLine);

                while let Some(cpp::ModuleMember::EmptyLine) = new_private_variable_members.last() {
                    new_private_variable_members.pop();
                }

                for member in new_private_variable_members.iter_mut() {
                    let cpp::ModuleMember::Data { is_static, .. } = member else {
                        unreachable!("{:#?}", member)
                    };

                    *is_static = false;
                }

                new_members.push(cpp::ModuleMember::Tagged(
                    "_static".into(),
                    Box::new(cpp::ModuleMember::Block {
                        members: new_private_variable_members,
                    }),
                ));
            }
            
            //--------------------------------------------------------------------------------
            // public functions
            //--------------------------------------------------------------------------------

            fn comment_block(block: &mut cpp::Block) {
                for statement in block.statements.iter_mut() {
                    match statement {
                        cpp::Statement::Comment(_) | cpp::Statement::Commented(_) => {
                            continue;
                        }

                        cpp::Statement::Block(block) => comment_block(block),

                        _ => *statement = cpp::Statement::Commented(Box::new(statement.clone())),
                    }
                }
            }

            let mut new_public_code_members: Vec<cpp::ModuleMember> = vec![];

            for member in public_code_members.iter() {
                let cpp::ModuleMember::Procedure(procedure) = member else {
                    unreachable!("{member:#?}")
                };

                let mut procedure = procedure.clone();

                if procedure.body.is_none() {
                    procedure.body = Some(cpp::Block::default());
                }

                comment_block(procedure.body.as_mut().unwrap());
                
                if let Some((mangled_name, _address)) = module.mangled_symbols.iter()
                    .find(|(_, address)| *address == procedure.address)
                {
                    if procedure.body.is_none() {
                        procedure.body = Some(cpp::Block::default());
                    }

                    procedure.body.as_mut().unwrap().statements.insert(
                        0,
                        cpp::Statement::FunctionCall(
                            "mangled_assert".into(),
                            vec![
                                format!("\"{}\"", mangled_name),
                            ],
                        ),
                    );

                    procedure.body.as_mut().unwrap().statements.push(
                        cpp::Statement::FunctionCall("todo".into(), vec!["\"implement\"".into()]),
                    );
                }

                let return_type_str = procedure.return_type.map(|return_type| {
                    cpp::type_name(
                        class_table,
                        type_sizes,
                        machine_type,
                        type_info,
                        type_finder,
                        return_type,
                        None,
                        None,
                        None,
                        false,
                        true,
                    )
                    .unwrap()
                }).unwrap_or("void".to_string());

                if matches!(return_type_str.as_str(), "void" | "void const")
                {
                    procedure.body.as_mut().unwrap().statements.push(
                        cpp::Statement::FunctionCall(
                            format!("_sub_{:X}", procedure.address).into(),
                            procedure
                                .arguments
                                .iter()
                                .map(|a| a.1.clone().unwrap_or("arg".into()))
                                .collect(),
                        ),
                    );
                } else {
                    procedure.body.as_mut().unwrap().statements.push(
                        cpp::Statement::ReturnWithValue(cpp::ReturnWithValue {
                            signature: return_type_str,
                            value: Some(Box::new(cpp::Statement::FunctionCall(
                                format!("_sub_{:X}", procedure.address).into(),
                                procedure
                                    .arguments
                                    .iter()
                                    .map(|a| a.1.clone().unwrap_or("arg".into()))
                                    .collect(),
                            ))),
                        }),
                    );
                }

                new_public_code_members.push(cpp::ModuleMember::Tagged(
                    "_extern".into(),
                    Box::new(cpp::ModuleMember::Procedure(cpp::Procedure {
                        address: 0,
                        line: procedure.line,
                        type_index: procedure.type_index,
                        is_static: procedure.is_static,
                        signature: cpp::type_name(
                            class_table,
                            type_sizes,
                            machine_type,
                            type_info,
                            type_finder,
                            procedure.type_index,
                            None,
                            Some(format!("_sub_{:X}", procedure.address).into()),
                            None,
                            false,
                            true,
                        )?
                        .trim_end_matches(" const")
                        .trim_end_matches(" volatile")
                        .into(),
                        body: None,
                        return_type: procedure.return_type,
                        arguments: vec![],
                    })),
                ));

                if !procedure.is_static {
                    new_public_code_members.push(cpp::ModuleMember::Procedure(procedure));
                } else {
                    procedure.is_static = false;

                    new_public_code_members.push(cpp::ModuleMember::Tagged(
                        "_static".into(),
                        Box::new(cpp::ModuleMember::Procedure(procedure))));
                }

                new_public_code_members.push(cpp::ModuleMember::EmptyLine);
            }

            if !new_public_code_members.is_empty() {
                new_members.push(cpp::ModuleMember::Comment("---------- public code".into()));
                new_members.push(cpp::ModuleMember::EmptyLine);
                new_members.extend(new_public_code_members);
            }

            //--------------------------------------------------------------------------------
            // private functions
            //--------------------------------------------------------------------------------

            let mut new_private_code_members = vec![];

            for member in private_code_members.iter() {
                let cpp::ModuleMember::Procedure(procedure) = member else {
                    unreachable!("{member:#?}")
                };

                let mut procedure = procedure.clone();

                if procedure.body.is_none() {
                    procedure.body = Some(cpp::Block::default());
                }

                comment_block(procedure.body.as_mut().unwrap());

                if let Some((mangled_name, _address)) = module.mangled_symbols.iter()
                    .find(|(_, address)| *address == procedure.address)
                {
                    if procedure.body.is_none() {
                        procedure.body = Some(cpp::Block::default());
                    }

                    procedure.body.as_mut().unwrap().statements.insert(
                        0,
                        cpp::Statement::FunctionCall(
                            "mangled_assert".into(),
                            vec![
                                format!("\"{}\"", mangled_name),
                            ],
                        ),
                    );

                    procedure.body.as_mut().unwrap().statements.insert(
                        1,
                        cpp::Statement::FunctionCall(
                            "mangled_assert".into(),
                            vec![
                                format!("\"{}\"", mangled_name),
                            ],
                        ),
                    );

                    procedure.body.as_mut().unwrap().statements.push(
                        cpp::Statement::FunctionCall("todo".into(), vec!["\"implement\"".into()]),
                    );
                }

                let return_type_str = procedure.return_type.map(|return_type| {
                    cpp::type_name(
                        class_table,
                        type_sizes,
                        machine_type,
                        type_info,
                        type_finder,
                        return_type,
                        None,
                        None,
                        None,
                        false,
                        true,
                    )
                    .unwrap()
                }).unwrap_or("void".to_string());

                if matches!(return_type_str.as_str(), "void" | "void const")
                {
                    procedure.body.as_mut().unwrap().statements.push(
                        cpp::Statement::FunctionCall(
                            format!("_sub_{:X}", procedure.address).into(),
                            procedure
                                .arguments
                                .iter()
                                .map(|a| a.1.clone().unwrap_or("arg".into()))
                                .collect(),
                        ),
                    );
                } else {
                    procedure.body.as_mut().unwrap().statements.push(
                        cpp::Statement::ReturnWithValue(cpp::ReturnWithValue {
                            signature: return_type_str,
                            value: Some(Box::new(cpp::Statement::FunctionCall(
                                format!("_sub_{:X}", procedure.address).into(),
                                procedure
                                    .arguments
                                    .iter()
                                    .map(|a| a.1.clone().unwrap_or("arg".into()))
                                    .collect(),
                            ))),
                        }),
                    );
                }

                new_private_code_members.push(cpp::ModuleMember::Tagged(
                    "_extern".into(),
                    Box::new(cpp::ModuleMember::Procedure(cpp::Procedure {
                        address: 0,
                        line: procedure.line,
                        type_index: procedure.type_index,
                        is_static: false,
                        signature: cpp::type_name(
                            class_table,
                            type_sizes,
                            machine_type,
                            type_info,
                            type_finder,
                            procedure.type_index,
                            None,
                            Some(format!("_sub_{:X}", procedure.address).into()),
                            None,
                            false,
                            true,
                        )?
                        .trim_end_matches(" const")
                        .trim_end_matches(" volatile")
                        .into(),
                        body: None,
                        return_type: procedure.return_type,
                        arguments: vec![],
                    })),
                ));

                if !procedure.is_static {
                    new_private_code_members.push(cpp::ModuleMember::Procedure(procedure));
                } else {
                    procedure.is_static = false;

                    new_private_code_members.push(cpp::ModuleMember::Tagged(
                        "_static".into(),
                        Box::new(cpp::ModuleMember::Procedure(procedure)),
                    ));
                }

                new_private_code_members.push(cpp::ModuleMember::EmptyLine);
            }
            
            if !new_private_code_members.is_empty() {
                new_members.push(cpp::ModuleMember::Comment("---------- private code".into()));
                new_members.push(cpp::ModuleMember::EmptyLine);
                new_members.extend(new_private_code_members);
            }

            //--------------------------------------------------------------------------------
            // ending preprocessor statements
            //--------------------------------------------------------------------------------

            if module.is_header() {
                let stem = module.path
                    .file_stem()
                    .and_then(std::ffi::OsStr::to_str)
                    .unwrap_or("UNKNOWN")
                    .to_uppercase();

                new_members.push(cpp::ModuleMember::Preprocessor(format!("endif // __{}_H__", stem)));
            }

            while let Some(cpp::ModuleMember::EmptyLine) = new_members.last() {
                new_members.pop();
            }

            module.members = new_members;
        }

        //
        // Write the module out to its file
        //

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
    class_table: &mut Vec<Rc<RefCell<cpp::Class>>>,
    type_sizes: &mut HashMap<String, u64>,
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
        class_table,
        type_sizes,
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
        class_table,
        type_sizes,
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
    class_table: &mut Vec<Rc<RefCell<cpp::Class>>>,
    type_sizes: &mut HashMap<String, u64>,
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
                class_table,
                type_sizes,
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
    class_table: &mut Vec<Rc<RefCell<cpp::Class>>>,
    type_sizes: &mut HashMap<String, u64>,
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

        if let pdb2::SymbolData::UserDefinedType(_) = symbol_data {
            continue;
        }
        
        process_module_symbol_data(
            options,
            class_table,
            type_sizes,
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
    class_table: &mut Vec<Rc<RefCell<cpp::Class>>>,
    type_sizes: &mut HashMap<String, u64>,
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

            let user_defined_type = cpp::ModuleMember::TypeDefinition(cpp::TypeDefinition {
                type_name: cpp::type_name(
                    class_table,
                    type_sizes,
                    machine_type,
                    type_info,
                    type_finder,
                    udt_symbol.type_index,
                    None,
                    Some(udt_symbol.name.to_string().to_string()),
                    None,
                    false,
                    false
                )?,
                underlying_type: udt_symbol.type_index,
                field_attributes: None,
                pointer_attributes: None,
                containing_class: None,
            });

            if !module.members.contains(&user_defined_type) {
                module.members.push(user_defined_type);
            }
        }

        pdb2::SymbolData::Constant(constant_symbol) => {
            let module_key = module_file_path.to_string_lossy().to_lowercase().to_string();
            let module = modules.entry(module_key).or_insert_with(|| cpp::Module::default().with_path(module_file_path.clone()));

            let type_name = cpp::type_name(
                class_table,
                type_sizes,
                machine_type,
                type_info,
                type_finder,
                constant_symbol.type_index,
                None,
                Some(constant_symbol.name.to_string().to_string()),
                None,
                false,
                false,
            )?;

            if type_name.starts_with("float const ") {
                // println!("WARNING: failed to decompile constant float in \"{}\": {symbol_data:#?}", module_file_path.to_string_lossy());
                return Ok(());
            }

            let constant = cpp::ModuleMember::Constant(format!(
                "{}{type_name} = {};",
                if type_name.starts_with("const ") { "" } else { "const " },
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
                cpp::ModuleMember::Data { address: a, .. } if *a == address => true,
                _ => false,
            }) {
                return Ok(());
            }

            module.members.push(cpp::ModuleMember::Data {
                is_static: data_symbol.global,
                name: data_symbol.name.to_string().to_string(),
                signature: format!(
                    "{}; // 0x{address:X}",
                    cpp::type_name(
                        class_table,
                        type_sizes,
                        machine_type,
                        type_info,
                        type_finder,
                        data_symbol.type_index,
                        None,
                        Some(data_symbol.name.to_string().to_string()),
                        None,
                        false,
                        false
                    )?,
                ),
                address,
                line: line_info.map(|x| x.line_start),
            });

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
                cpp::ModuleMember::Data { address: a, .. } if *a == address => true,
                _ => false,
            }) {
                return Ok(());
            }

            module.members.push(cpp::ModuleMember::ThreadStorage {
                is_static: !thread_storage_symbol.global,
                name: thread_storage_symbol.name.to_string().to_string(),
                signature: format!(
                    "thread_local {}; // 0x{address:X}",
                    cpp::type_name(
                        class_table,
                        type_sizes,
                        machine_type,
                        type_info,
                        type_finder,
                        thread_storage_symbol.type_index,
                        None,
                        Some(thread_storage_symbol.name.to_string().to_string()),
                        None,
                        false,
                        false
                    )?,
                ),
                address,
                line: line_info.map(|x| x.line_start),
            });

            writeln!(
                script_file,
                "decompile_to_file(0x{address:X}, \"{}\")",
                module_file_path.to_string_lossy(),
            )?;
        }

        pdb2::SymbolData::Procedure(procedure_symbol) => {
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
                class_table,
                type_sizes,
                machine_type,
                base_address.clone(),
                address_map,
                id_finder,
                type_info,
                type_finder,
                module_symbols,
                procedure_symbol,
            )?;
            
            let parameters = if register_variable_names.is_empty() {
                register_relative_names
            } else {
                register_variable_names
            };

            let procedure_type = type_finder.find(procedure_symbol.type_index)?.parse()?;
            
            if let pdb2::TypeData::MemberFunction(member_function) = procedure_type {
                let procedure_name_full = procedure_symbol.name.to_string().to_string();
                let (mut procedure_class_name, end_offset) = cpp::parse_type_name(&procedure_name_full, true);
                let (mut procedure_name, _end_offset) = cpp::parse_type_name(&procedure_name_full[end_offset..], true);

                if procedure_name.is_empty() && procedure_class_name.contains("::") {
                    let mut parts = procedure_class_name.split("::").map(|s| s.to_string()).collect::<Vec<_>>();
                    assert!(parts.len() >= 2);
                    procedure_name = parts.pop().unwrap();
                    procedure_class_name = parts.join("::");
                }

                procedure_name = procedure_name
                    .trim_start_matches("::")
                    .replace(" struct ", " ")
                    .replace(" union ", " ")
                    .replace(" class ", " ")
                    .replace(" enum ", " ");

                let full_classes = class_table.iter().filter(|c| {
                    let c = c.borrow();
                    c.name == procedure_class_name && !c.is_declaration
                }).cloned().collect::<Vec<_>>();

                // let mut found = false;

                for class in full_classes.iter() {
                    for member in class.borrow_mut().members.iter_mut() {
                        let cpp::ClassMember::Method(class_method) = member else {
                            continue;
                        };

                        let class_member_function = type_finder.find(class_method.type_index)?.parse()?;
                        
                        let pdb2::TypeData::MemberFunction(class_member_function) = class_member_function else {
                            panic!("Expected member function, got: {:#?}", class_member_function);
                        };

                        let class_method_name = class_method.name
                            .chars()
                            .take_while(|c| *c != '(')
                            .collect::<String>()
                            .replace(" struct ", " ")
                            .replace(" union ", " ")
                            .replace(" class ", " ")
                            .replace(" enum ", " ");

                        if class_method_name == procedure_name {
                            let valid = if class_member_function.argument_list == member_function.argument_list {
                                true
                            } else {
                                let lhs = cpp::argument_list(class_table, type_sizes, machine_type, type_info, type_finder, None, class_member_function.argument_list, None)?;
                                let rhs = cpp::argument_list(class_table, type_sizes, machine_type, type_info, type_finder, None, member_function.argument_list, None)?;
                                lhs == rhs
                            };

                            if valid {
                                let mut parameters = parameters.clone();
                                
                                if !parameters.is_empty() && parameters[0] == "this" {
                                    parameters.remove(0);
                                }
                                
                                class_method.arguments = cpp::argument_list(
                                    class_table, 
                                    type_sizes,
                                    machine_type,
                                    type_info,
                                    type_finder,
                                    None,
                                    member_function.argument_list,
                                    Some(parameters.clone()),
                                )?;

                                class_method.modifier = cpp::type_modifier(&class_member_function, type_finder);

                                // found = true;
                                break;
                            }
                        }
                    }
                }

                // procedure_name = procedure_symbol.name.to_string().to_string();
                
                // if !found && !(procedure_name.contains("::[") || procedure_name.contains('`')) {
                //     println!("----------------");
                //     println!(
                //         "WARNING: \"{}\" not found in any \"{}\":",
                //         procedure_symbol.name,
                //         procedure_class_name,
                //         // class.borrow().members.iter()
                //         //     .filter(|m| matches!(m, cpp::ClassMember::Method(_)))
                //         //     .map(|m| {
                //         //         let cpp::ClassMember::Method(method) = m else { unreachable!() };
                //         //         format!("\"{}\"", method.name)
                //         //     })
                //         //     .collect::<Vec<_>>()
                //         //     .join(", ")
                //     );

                //     for class in full_classes {
                //         println!("{}", tabbed::TabbedDisplayer(&*class.borrow()));
                //     }
                //     println!("----------------");
                // }
            }

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
                cpp::ModuleMember::Data { address: a, .. } if *a == address => true,
                _ => false,
            }) {
                return Ok(());
            }

            let procedure = cpp::type_name(
                class_table,
                type_sizes,
                machine_type,
                type_info,
                type_finder,
                procedure_symbol.type_index,
                None,
                Some(procedure_symbol.name.to_string().to_string()),
                Some(parameters.clone()),
                false,
                false
            )?;

            if procedure.starts_with("...") || procedure.contains('$') || procedure.contains('`') {
                return Ok(());
            }
            
            let type_item = type_finder.find(procedure_symbol.type_index)?;
            let type_data = type_item.parse()?;
            let (this_pointer_type, argument_list) = match type_data {
                pdb2::TypeData::Procedure(data) => {
                    (None, data.argument_list)
                }
                pdb2::TypeData::MemberFunction(data) => {
                    (data.this_pointer_type, data.argument_list)
                }
                data => todo!("{data:#?}")
            };

            let arguments = cpp::argument_type_list(
                type_finder, 
                this_pointer_type,
                argument_list, 
                Some(parameters),
            ).unwrap();

            module.members.push(
                cpp::ModuleMember::Procedure(cpp::Procedure {
                    address,
                    line: line_info.map(|x| x.line_start),
                    type_index: procedure_symbol.type_index,
                    is_static: !procedure_symbol.global,
                    signature: procedure,
                    body,
                    return_type:match type_finder.find(procedure_symbol.type_index)?.parse()? {
                        pdb2::TypeData::Procedure(data) => {
                            data.return_type
                        }
                        pdb2::TypeData::MemberFunction(data) => {
                            Some(data.return_type)
                        }
                        data => todo!("{data:#?}")
                    },
                    arguments
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

        pdb2::SymbolData::Public(public_symbol) => {
            let mut file_name_ref = None;
            // let mut line_info = None;

            for (name_ref, offsets) in line_offsets.iter() {
                if let Some(_line) = offsets.get(&public_symbol.offset) {
                    file_name_ref = Some(*name_ref);
                    // line_info = Some(line.clone());
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

            let rva = match public_symbol.offset.to_rva(address_map) {
                Some(rva) => rva,
                None => {
                    // println!("WARNING: no RVA found for public symbol: {public_symbol:?}");
                    return Ok(());
                }
            };

            let address = base_address.unwrap_or(0) + rva.0 as u64;
            let entry = (public_symbol.name.to_string().to_string(), address);

            if !module.mangled_symbols.contains(&entry) {
                module.mangled_symbols.push(entry);
            }
        }

        pdb2::SymbolData::ProcedureReference(_)
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
    class_table: &mut Vec<Rc<RefCell<cpp::Class>>>,
    type_sizes: &mut HashMap<String, u64>,
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
            class_table,
            type_sizes,
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
    class_table: &mut Vec<Rc<RefCell<cpp::Class>>>,
    type_sizes: &mut HashMap<String, u64>,
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
            class_table,
            type_sizes,
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
    class_table: &mut Vec<Rc<RefCell<cpp::Class>>>,
    type_sizes: &mut HashMap<String, u64>,
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
        class_table,
        type_sizes,
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
    class_table: &mut Vec<Rc<RefCell<cpp::Class>>>,
    type_sizes: &mut HashMap<String, u64>,
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
                        class_table,
                        type_sizes,
                        machine_type,
                        type_info,
                        type_finder,
                        register_variable_symbol.type_index,
                        None,
                        Some(register_variable_symbol.name.to_string().to_string()),
                        None,
                        false,
                        false
                    )?,
                    value: None,
                    comment: Some(format!("r{}", register_variable_symbol.register.0))
                }));
            }

            pdb2::SymbolData::RegisterRelative(register_relative_symbol) => {
                statements.push(cpp::Statement::Variable(cpp::Variable {
                    signature: cpp::type_name(
                        class_table, 
                        type_sizes,
                        machine_type,
                        type_info,
                        type_finder,
                        register_relative_symbol.type_index,
                        None,
                        Some(register_relative_symbol.name.to_string().to_string()),
                        None,
                        false,
                        false
                    )?,
                    value: None,
                    comment: Some(format!("r{} offset {}", register_relative_symbol.register.0, register_relative_symbol.offset))
                }));
            }

            pdb2::SymbolData::Block(block_symbol) => {
                statements.push(cpp::Statement::Block(parse_block_symbols(
                    class_table,
                    type_sizes,
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
                    class_table,
                    type_sizes,
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
                        class_table,
                        type_sizes,
                        machine_type,
                        type_info,
                        type_finder,
                        constant_symbol.type_index,
                        None,
                        Some(constant_symbol.name.to_string().to_string()),
                        None,
                        false,
                        false
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
                        class_table,
                        type_sizes,
                        machine_type,
                        type_info,
                        type_finder,
                        data_symbol.type_index,
                        None,
                        Some(data_symbol.name.to_string().to_string()),
                        None,
                        false,
                        false
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
                        class_table,
                        type_sizes,
                        machine_type,
                        type_info,
                        type_finder,
                        local_symbol.type_index,
                        None,
                        Some(local_symbol.name.to_string().to_string()),
                        None,
                        false,
                        false
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
                            class_table,
                            type_sizes,
                            machine_type,
                            type_info,
                            type_finder,
                            tls_symbol.type_index,
                            None,
                            Some(tls_symbol.name.to_string().to_string()),
                            None,
                            false,
                            false
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
