mod cpp;

use pdb::FallibleIterator;
use std::{collections::{HashMap, HashSet}, error::Error, fs::{self, File}, io::Write, num, path::PathBuf};
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(name = "pdb-decompiler", about = "A tool to decompile MSVC PDB files to C++ source code.")]
struct Options {
    #[structopt(short, long, parse(from_os_str))]
    out: Option<PathBuf>,

    #[structopt(short, long)]
    pdb: Option<String>,

    #[structopt(short, long, parse(try_from_str = parse_base_address))]
    base_address: Option<u64>,
}

fn parse_base_address(src: &str) -> Result<u64, num::ParseIntError> {
    u64::from_str_radix(src.trim_start_matches("0x"), 16)
}

fn sanitize_path<S: AsRef<str>>(path: S) -> String {
    let mut result = path.as_ref().to_string().replace("\\", "/");

    if let Some((_, rest)) = result.split_once(":") {
        result = rest.to_string();
    }

    result
}

fn main() -> Result<(), Box<dyn Error>> {
    let options = Options::from_args();
    let pdb = pdb::PDB::open(File::open(options.pdb.clone().ok_or("PDB path not provided")?)?)?;
    
    if let Err(error) = decompile_pdb(options, pdb) {
        println!("ERROR: could not decompile PDB because it {error}");
    }

    Ok(())
}

fn decompile_pdb(options: Options, mut pdb: pdb::PDB<File>) -> Result<(), Box<dyn Error>> {
    let debug_info = pdb.debug_information().map_err(|e| format!("does not contain debug information: {e}"))?;
    let address_map = pdb.address_map().map_err(|e| format!("does not contain an address map: {e}"))?;
    let string_table = pdb.string_table().map_err(|e| format!("does not contain a string table: {e}"))?;

    let type_info = pdb.type_information().map_err(|e| format!("does not contain type information: {e}"))?;
    let mut type_finder = type_info.finder();
    
    let id_info = pdb.id_information().map_err(|e| format!("does not contain id information: {e}"))?;
    let mut id_finder = id_info.finder();

    let global_symbols = pdb.global_symbols().map_err(|e| format!("does not contain global symbol information: {e}"))?;

    let mut modules = HashMap::new();

    let out_path: PathBuf = options.out.ok_or("Out path not supplied".to_string())?;
    fs::create_dir_all(out_path.clone())?;
    
    let mut script_file = File::create(out_path.join("ida_script.py"))?;
    writeln!(script_file, include_str!("../ida_script_base.py"))?;

    process_type_information(&type_info, &mut type_finder).map_err(|e| format!("failed to process type info: {e}"))?;
    process_id_information(&mut modules, &string_table, &id_info, &mut id_finder, &type_info, &mut type_finder).map_err(|e| format!("failed to process id info: {e}"))?;

    let section_contributions = load_section_contributions(&debug_info).map_err(|e| format!("does not contain module section information: {e}"))?;
    let module_global_symbols = load_module_global_symbols(&debug_info, &global_symbols, section_contributions).map_err(|e| format!("failed to load module global symbol info: {e}"))?;
    
    //
    // Process module debug information
    //

    let mut module_iter = debug_info.modules().map_err(|e| format!("does not contain debug module info: {e}"))?;

    while let Some(ref module) = module_iter.next().map_err(|e| format!("failed to get next debug module info: {e}"))? {
        let module_info = match pdb.module_info(module)? {
            Some(module_info) => module_info,
            None => {
                // println!("module has no info: {} - {}", module.module_name(), module.object_file_name());
                continue;
            },
        };

        let (path, headers, members) = match parse_module(
            options.base_address,
            module,
            &module_info,
            &address_map,
            &string_table,
            &type_info,
            &type_finder,
            &id_info,
            &id_finder,
            &module_global_symbols,
            &mut script_file
        ) {
            Ok(x) => x,
            Err(e) => {
                println!("WARNING: failed to decompile module, skipping: {e} - {module:#?}");
                continue;
            }
        };

        let source_file = match path.as_ref() {
            Some(path) => path.to_string_lossy().to_string(),
            None => {
                match module.module_name().to_string().as_str() {
                    "* CIL *" |
                    "* Linker *" |
                    "* Linker Generated Manifest RES *" => (),
                    
                    x if x.to_lowercase().ends_with(".dll") => (),

                    _ => {
                        println!("WARNING: module has no source file, skipping: {module:#?}");
                    }
                }
                continue;
            }
        };

        let module = modules.entry(source_file.clone()).or_insert(cpp::Module::new().with_path(path.unwrap()));
        module.headers.extend(headers);

        for (source_file, member) in members {
            let module = modules
                .entry(source_file.to_string_lossy().to_string())
                .or_insert(cpp::Module::new().with_path(source_file));
            
            // TODO: check if module already contains member...
            module.members.push(member);
        }
    }

    //
    // Write modules to file
    //

    for entry in &modules {
        let path = PathBuf::from(sanitize_path(format!("{}{}", out_path.to_string_lossy(), entry.0).to_string()));
        
        if let Some(parent_path) = path.parent() {
            fs::create_dir_all(parent_path)?;
        }

        let mut file = if path.exists() { File::open(path)? } else { File::create(path)? };
        write!(file, "{}", entry.1)?;
    }

    Ok(())
}

fn load_section_contributions(
    debug_info: &pdb::DebugInformation,
) -> pdb::Result<Vec<pdb::DBISectionContribution>> {
    let mut result = vec![];
    let mut section_contributions = debug_info.section_contributions()?;
    
    while let Some(section_contribution) = section_contributions.next()? {
        result.push(section_contribution);
    }

    Ok(result)
}

fn process_type_information<'a>(
    type_info: &'a pdb::TypeInformation,
    type_finder: &mut pdb::TypeFinder<'a>
) -> pdb::Result<()> {
    let mut type_iter = type_info.iter();

    while type_iter.next()?.is_some() {
        type_finder.update(&type_iter);
    }

    Ok(())
}

fn process_id_information<'a>(
    modules: &mut HashMap<String, cpp::Module>,
    string_table: &pdb::StringTable,
    id_info: &'a pdb::IdInformation,
    id_finder: &mut pdb::IdFinder<'a>,
    type_info: &'a pdb::TypeInformation,
    type_finder: &mut pdb::TypeFinder<'a>,
) -> pdb::Result<()> {
    let mut id_iter = id_info.iter();

    while let Some(id) = id_iter.next()? {
        id_finder.update(&id_iter);

        match id.parse() {
            Ok(pdb::IdData::UserDefinedTypeSource(data)) => {
                let module_path = match data.source_file {
                    pdb::UserDefinedTypeSourceFileRef::Local(id) => match id_finder.find(id) {
                        Ok(item) => match item.parse() {
                            Ok(pdb::IdData::String(source_file)) => sanitize_path(source_file.name.to_string().to_string()),
                            Ok(data) => panic!("invalid UDT source file id: {:#?}", data),
                            Err(error) => panic!("failed to parse UDT source file id: {error}")
                        }
                        
                        Err(error) => panic!("failed to find UDT source file id: {error}")
                    }
    
                    pdb::UserDefinedTypeSourceFileRef::Remote(_, source_line_ref) => {
                        sanitize_path(source_line_ref.to_string_lossy(&string_table)?.to_string())
                    }
                };
                
                modules
                    .entry(module_path.clone())
                    .or_insert(cpp::Module::new().with_path(module_path.into()))
                    .add_type_definition(&type_info, &type_finder, data.udt, data.line)?;
            }

            Ok(_) => (),

            Err(e) => {
                eprintln!("WARNING: failed to parse id: {e}");
                continue;
            }
        }
    }

    Ok(())
}

fn load_module_global_symbols<'a>(
    debug_info: &pdb::DebugInformation,
    global_symbols: &'a pdb::SymbolTable,
    section_contributions: Vec<pdb::DBISectionContribution>,
) -> pdb::Result<HashMap<String, Vec<pdb::SymbolData<'a>>>> {
    let mut result = HashMap::new();

    let modules = debug_info.modules()?.collect::<Vec<_>>()?;
    let mut prev_module_name = None;

    let mut global_symbols_iter = global_symbols.iter();

    while let Some(symbol) = global_symbols_iter.next()? {
        let symbol_data = match symbol.parse() {
            Ok(symbol_data) => symbol_data,
            Err(error) => {
                println!("WARNING: failed to parse symbol data, skipping: {error}");
                continue;
            }
        };

        match symbol_data {
            pdb::SymbolData::UserDefinedType(_) if prev_module_name.is_some() => {
                let module_name = prev_module_name.clone().unwrap();
                // println!("Inserting global UDT into previous module \"{module_name}\": {symbol_data:#?}");

                result
                    .entry(module_name)
                    .or_insert(vec![])
                    .push(symbol_data.clone());
            }

            pdb::SymbolData::ProcedureReference(pdb::ProcedureReferenceSymbol { module, .. }) => {
                let referenced_module = modules.iter().nth(module as _).unwrap();
                let module_name = referenced_module.module_name().to_string();
                prev_module_name = Some(module_name.clone());

                // println!("Found referenced module \"{module_name}\" in global symbol {symbol_data:#?}");

                result
                    .entry(module_name)
                    .or_insert(vec![])
                    .push(symbol_data.clone());
            }

            pdb::SymbolData::Public(pdb::PublicSymbol { offset, .. }) |
            pdb::SymbolData::Data(pdb::DataSymbol { offset, .. }) |
            pdb::SymbolData::ThreadStorage(pdb::ThreadStorageSymbol { offset, .. }) |
            pdb::SymbolData::Procedure(pdb::ProcedureSymbol { offset, .. }) => {
                for contribution in section_contributions.iter() {
                    let contributing_module = modules.iter().nth(contribution.module as _).unwrap();

                    if offset >= contribution.offset && offset < contribution.offset + contribution.size {
                        let module_name = contributing_module.module_name().to_string();
                        prev_module_name = Some(module_name.clone());

                        // println!("Found contributing module \"{module_name}\" in global symbol {symbol_data:#?}");

                        result
                            .entry(module_name)
                            .or_insert(vec![])
                            .push(symbol_data.clone());
                        break;
                    }
                }
            }

            _ => {
                // println!("found global symbol {symbol_data:?}");
            }
        }
    }

    Ok(result)
}

fn parse_module(
    base_address: Option<u64>,
    module: &pdb::Module,
    module_info: &pdb::ModuleInfo,
    address_map: &pdb::AddressMap,
    string_table: &pdb::StringTable,
    type_info: &pdb::TypeInformation,
    type_finder: &pdb::TypeFinder,
    _id_info: &pdb::IdInformation,
    _id_finder: &pdb::IdFinder,
    module_global_symbols: &HashMap<String, Vec<pdb::SymbolData>>,
    script_file: &mut File,
) -> pdb::Result<(Option<PathBuf>, Vec<PathBuf>, Vec<(PathBuf, cpp::ModuleMember)>)> {
    let mut headers = vec![];
    let mut members = vec![];
    let mut line_offsets = HashMap::new();
    let mut module_file_name_ref = None;
    let mut module_file_path = None;

    let line_program = module_info.line_program()?;
    
    let mut obj_path = PathBuf::from(sanitize_path(module.module_name()));

    if let Some(file_name) = obj_path.file_name() {
        let file_name = file_name.to_string_lossy();

        if file_name.ends_with(".o") || file_name.ends_with(".obj") {
            let mut parts = file_name.split('.').collect::<Vec<_>>();
            let mut index = None;

            for ext in ["c", "cc", "cpp", "cxx", "pch", "masm", "asm", "res", "exp"] {
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

    let mut lines = line_program.lines();

    while let Some(line) = lines.next()? {
        let file = line_program.get_file_info(line.file_index)?;
        let offsets = line_offsets.entry(file.name).or_insert(HashSet::new());

        if !offsets.contains(&line.offset) {
            offsets.insert(line.offset);
        }
    }

    let mut files = line_program.files();

    while let Some(file) = files.next()? {
        let path = PathBuf::from(sanitize_path(file.name.to_string_lossy(string_table)?.to_string()));
        
        match path.extension() {
            Some(extension) => match extension.to_str().unwrap_or("") {
                "c" | "cc" | "cpp" | "cxx" | "pch" | "asm" | "fasm" | "masm" | "res" | "exp" if path.file_stem().map(|x| x.to_ascii_lowercase()) == obj_path.file_stem().map(|x| x.to_ascii_lowercase()) => {
                    module_file_name_ref = Some(file.name);
                    module_file_path = Some(path);
                }

                _ => {
                    headers.push(path);
                }
            }

            None => {
                headers.push(path);
            }
        }
    }

    if module_file_path.is_none() {
        return Ok((module_file_path, headers, members));
    }

    let module_file_name_ref = module_file_name_ref.unwrap();
    let module_file_path = module_file_path.unwrap().to_string_lossy().replace("\\", "\\\\");

    let mut process_symbol_data = |symbol_data: &pdb::SymbolData, module_symbols: &mut pdb::SymbolIter| -> pdb::Result<()> {
        match symbol_data {
            pdb::SymbolData::UsingNamespace(symbol) => members.push(
                (
                    PathBuf::from(sanitize_path(module_file_name_ref.to_string_lossy(string_table)?.to_string())),
                    cpp::ModuleMember::Declaration(
                        format!("using namespace {};", symbol.name.to_string().to_string())
                    )
                )
            ),

            pdb::SymbolData::Constant(symbol) => {
                let type_name = cpp::type_name(type_info, type_finder, symbol.type_index, Some(symbol.name.to_string().to_string()), None, false)?;
                
                if type_name.starts_with("const ") {
                    return Ok(())
                }

                members.push(
                    (
                        PathBuf::from(sanitize_path(module_file_name_ref.to_string_lossy(string_table)?.to_string())),
                        cpp::ModuleMember::Declaration(
                            format!(
                                "const {type_name} = {};",
                                match symbol.value {
                                    pdb::Variant::U8(x) => format!("0x{:X}", x),
                                    pdb::Variant::U16(x) => format!("0x{:X}", x),
                                    pdb::Variant::U32(x) => format!("0x{:X}", x),
                                    pdb::Variant::U64(x) => format!("0x{:X}", x),
                                    pdb::Variant::I8(x) => format!("0x{:X}", x),
                                    pdb::Variant::I16(x) => format!("0x{:X}", x),
                                    pdb::Variant::I32(x) => format!("0x{:X}", x),
                                    pdb::Variant::I64(x) => format!("0x{:X}", x),
                                }
                            )
                        )
                    )
                );
            }

            pdb::SymbolData::Data(data_symbol) => {
                match data_symbol.name.to_string().to_string().as_str() {
                    x if x.contains("`") || x.contains("$") => return Ok(()),
                    _ => ()
                }

                let mut file_name_ref = None;

                for (name_ref, offsets) in line_offsets.iter() {
                    if offsets.contains(&data_symbol.offset) {
                        file_name_ref = Some(name_ref.clone());
                        break;
                    }
                }

                let file_name_ref = match file_name_ref {
                    Some(file_name_ref) => file_name_ref,
                    None => module_file_name_ref,
                };

                let rva = match data_symbol.offset.to_rva(address_map) {
                    Some(rva) => rva,
                    None => {
                        println!("WARNING: no RVA found for data symbol: {data_symbol:?}");
                        return Ok(());
                    }
                };
                
                let address = base_address.unwrap_or(0) + rva.0 as u64;

                members.push(
                    (
                        PathBuf::from(sanitize_path(file_name_ref.to_string_lossy(string_table)?.to_string())),
                        cpp::ModuleMember::Declaration(
                            format!(
                                "{}; // 0x{address:X}",
                                cpp::type_name(
                                    type_info,
                                    type_finder,
                                    data_symbol.type_index,
                                    Some(data_symbol.name.to_string().to_string()),
                                    None,
                                    false
                                )?,
                            )
                        )
                    )
                );

                writeln!(
                    script_file,
                    "decompile_to_file(0x{address:X}, \"{module_file_path}\")",
                )?;
            }

            pdb::SymbolData::ThreadStorage(thread_storage_symbol) => {
                let mut file_name_ref = None;

                for (name_ref, offsets) in line_offsets.iter() {
                    if offsets.contains(&thread_storage_symbol.offset) {
                        file_name_ref = Some(name_ref.clone());
                        break;
                    }
                }

                let file_name_ref = file_name_ref.unwrap_or(module_file_name_ref);

                let rva = match thread_storage_symbol.offset.to_rva(address_map) {
                    Some(rva) => rva,
                    None => {
                        println!("WARNING: no RVA found for thread storage symbol: {thread_storage_symbol:?}");
                        return Ok(());
                    }
                };
                
                let address = base_address.unwrap_or(0) + rva.0 as u64;

                members.push(
                    (
                        PathBuf::from(sanitize_path(file_name_ref.to_string_lossy(string_table)?.to_string())),
                        cpp::ModuleMember::Declaration(
                            format!(
                                "thread_local {}; // 0x{address:X}",
                                cpp::type_name(
                                    type_info,
                                    type_finder,
                                    thread_storage_symbol.type_index,
                                    Some(thread_storage_symbol.name.to_string().to_string()),
                                    None,
                                    false
                                )?
                            )
                        )
                    )
                );

                writeln!(
                    script_file,
                    "decompile_to_file(0x{address:X}, \"{module_file_path}\")"
                )?;
            }

            pdb::SymbolData::Procedure(procedure_symbol) => {
                let parameters = parse_procedure_symbols(module_symbols);

                let procedure = cpp::type_name(
                    type_info,
                    type_finder,
                    procedure_symbol.type_index,
                    Some(procedure_symbol.name.to_string().to_string()),
                    Some(parameters.clone()),
                    false
                )?;
                
                if procedure.starts_with("...") || procedure.contains("$") || procedure.contains("`") {
                    return Ok(());
                }

                let mut file_name_ref = None;

                for (name_ref, offsets) in line_offsets.iter() {
                    if offsets.contains(&procedure_symbol.offset) {
                        file_name_ref = Some(name_ref.clone());
                        break;
                    }
                }

                let file_name_ref = match file_name_ref {
                    Some(file_name_ref) => file_name_ref,
                    None => module_file_name_ref,
                };

                let rva = match procedure_symbol.offset.to_rva(address_map) {
                    Some(rva) => rva,
                    None => {
                        println!("WARNING: no RVA found for procedure symbol: {procedure_symbol:?}");
                        return Ok(());
                    }
                };
                
                let address = base_address.unwrap_or(0) + rva.0 as u64;

                members.push(
                    (
                        PathBuf::from(sanitize_path(file_name_ref.to_string_lossy(string_table)?.to_string())),
                        cpp::ModuleMember::Declaration(
                            format!("{procedure}; // 0x{address:X}")
                        )
                    )
                );

                writeln!(
                    script_file,
                    "decompile_to_file(0x{address:X}, \"{module_file_path}\")",
                )?;
            }

            pdb::SymbolData::Thunk(_) => parse_thunk_symbols(module_symbols),

            pdb::SymbolData::Public(_) => {
                // println!("found public symbol in module: {symbol_data:?}");
            }

            pdb::SymbolData::ProcedureReference(_) => {
                // println!("found procedure reference in module: {symbol_data:?}");
            }

            pdb::SymbolData::ObjName(_) => {
                // println!("found obj name in module: {symbol_data:?}");
            }

            pdb::SymbolData::BuildInfo(_) => {
                // println!("found build info in module: {:?}", id_finder.find(build_info.id)?.parse()?);
            }

            pdb::SymbolData::CompileFlags(_) => {
                // println!("found compile flags in module: {symbol_data:?}");
            }

            pdb::SymbolData::UserDefinedType(udt_symbol) => {
                members.push(
                    (
                        PathBuf::from(sanitize_path(module_file_name_ref.to_string_lossy(string_table)?.to_string())),
                        cpp::ModuleMember::Declaration(
                            format!("typedef {};", cpp::type_name(
                                type_info,
                                type_finder,
                                udt_symbol.type_index,
                                Some(udt_symbol.name.to_string().to_string()),
                                None,
                                false
                            )?)
                        )
                    )
                );
            }

            pdb::SymbolData::Export(_) => {
                // println!("found export in module: {symbol_data:?}");
            }

            pdb::SymbolData::Label(_) => {
                // println!("found label in module: {symbol_data:?}");
            }

            pdb::SymbolData::SeparatedCode(_) => parse_separated_code_symbols(module_symbols),

            ref data => panic!("Unhandled module symbol: {:#?}", data)
        }
    
        Ok(())
    };

    let mut module_symbols = module_info.symbols()?;

    if let Some(global_symbols) = module_global_symbols.get(&module.module_name().to_string()) {
        for symbol_data in global_symbols {
            process_symbol_data(symbol_data, &mut module_symbols)?;
        }
    }

    while let Some(symbol) = module_symbols.next()? {
        let symbol_data = match symbol.parse() {
            Ok(symbol_data) => symbol_data,
            Err(err) => {
                println!("WARNING: failed to parse symbol data, skipping: {err}");
                continue;
            }
        };

        process_symbol_data(&symbol_data, &mut module_symbols)?;
    }

    Ok((Some(PathBuf::from(module_file_path)), headers, members))
}

fn parse_procedure_symbols(symbols: &mut pdb::SymbolIter) -> Vec<String> {
    let mut parameter_names = vec![];

    loop {
        let symbol = match symbols.next() {
            Ok(symbol) => match symbol {
                Some(symbol) => symbol,
                None => break
            }
            _ => continue
        };

        let symbol_data = match symbol.parse() {
            Ok(symbol_data) => symbol_data,
            _ => continue
        };

        match symbol_data {
            pdb::SymbolData::ScopeEnd => break,

            pdb::SymbolData::Constant(_) => {
                // println!("found constant symbol in procedure: {symbol_data:?}");
            }

            pdb::SymbolData::Data(_) => {
                // println!("found data symbol in procedure: {symbol_data:?}");
            }

            pdb::SymbolData::Local(_) => (),
            pdb::SymbolData::Label(_) => (),

            pdb::SymbolData::UserDefinedType(_) => {
                // println!("found UDT symbol in procedure: {symbol_data:?}");
            }

            pdb::SymbolData::Block(_) => parse_block_symbols(symbols),
            pdb::SymbolData::InlineSite(_) => parse_inline_site_symbols(symbols),
            pdb::SymbolData::RegisterRelative(x) => parameter_names.push(x.name.to_string().to_string()),
            pdb::SymbolData::RegisterVariable(_) => (),

            pdb::SymbolData::ThreadStorage(_) => {
                // println!("found thread storage symbol in procedure: {symbol_data:?}");
            }

            pdb::SymbolData::SeparatedCode(_) => parse_separated_code_symbols(symbols),

            data => panic!("Unhandled symbol data in parse_procedure_symbols - {data:?}")
        }
    }

    parameter_names
}

fn parse_block_symbols(symbols: &mut pdb::SymbolIter) {
    loop {
        let symbol = match symbols.next() {
            Ok(symbol) => match symbol {
                Some(symbol) => symbol,
                None => break
            }
            _ => continue
        };

        let symbol_data = match symbol.parse() {
            Ok(symbol_data) => symbol_data,
            _ => continue
        };

        match symbol_data {
            pdb::SymbolData::ScopeEnd => break,

            pdb::SymbolData::Constant(_) => {
                // println!("found constant symbol in block: {symbol_data:?}");
            }

            pdb::SymbolData::Data(_) => {
                // println!("found data symbol in block: {symbol_data:?}");
            }

            pdb::SymbolData::Local(_) => (),
            pdb::SymbolData::RegisterRelative(_) => (),
            pdb::SymbolData::Label(_) => (),

            pdb::SymbolData::UserDefinedType(_) => {
                // println!("found UDT symbol in block: {symbol_data:?}");
            }
            
            pdb::SymbolData::Block(_) => parse_block_symbols(symbols),
            pdb::SymbolData::InlineSite(_) => parse_inline_site_symbols(symbols),
            pdb::SymbolData::SeparatedCode(_) => parse_separated_code_symbols(symbols),

            data => panic!("Unhandled symbol data in parse_block_symbols - {data:?}")
        }
    }
}

fn parse_inline_site_symbols(symbols: &mut pdb::SymbolIter) {
    loop {
        let symbol = match symbols.next() {
            Ok(symbol) => match symbol {
                Some(symbol) => symbol,
                None => break
            }
            _ => continue
        };

        let symbol_data = match symbol.parse() {
            Ok(symbol_data) => symbol_data,
            _ => continue
        };

        match symbol_data {
            pdb::SymbolData::InlineSiteEnd => break,

            pdb::SymbolData::Constant(_) => {
                // println!("found constant symbol in inline site: {symbol_data:?}");
            }

            pdb::SymbolData::Data(_) => {
                // println!("found data symbol in inline site: {symbol_data:?}");
            }

            pdb::SymbolData::Local(_) => (),
            pdb::SymbolData::RegisterRelative(_) => (),
            pdb::SymbolData::Label(_) => (),

            pdb::SymbolData::UserDefinedType(_) => {
                // println!("found UDT symbol in inline site: {symbol_data:?}");
            }
            
            pdb::SymbolData::Block(_) => parse_block_symbols(symbols),
            pdb::SymbolData::InlineSite(_) => parse_inline_site_symbols(symbols),
            pdb::SymbolData::SeparatedCode(_) => parse_separated_code_symbols(symbols),
            
            _ => panic!("Unhandled symbol data in parse_inline_site_symbols - {symbol_data:?}")
        }
    }
}

fn parse_thunk_symbols(symbols: &mut pdb::SymbolIter) {
    loop {
        let symbol = match symbols.next() {
            Ok(symbol) => match symbol {
                Some(symbol) => symbol,
                None => break
            }
            _ => continue
        };

        let symbol_data = match symbol.parse() {
            Ok(symbol_data) => symbol_data,
            _ => continue
        };

        match symbol_data {
            pdb::SymbolData::ScopeEnd => break,
            _ => panic!("Unhandled symbol data in parse_thunk_symbols - {symbol_data:?}")
        }
    }
}

fn parse_separated_code_symbols(symbols: &mut pdb::SymbolIter) {
    loop {
        let symbol = match symbols.next() {
            Ok(symbol) => match symbol {
                Some(symbol) => symbol,
                None => break
            }
            _ => continue
        };

        let symbol_data = match symbol.parse() {
            Ok(symbol_data) => symbol_data,
            _ => continue
        };

        match symbol_data {
            pdb::SymbolData::ScopeEnd => break,
            _ => panic!("Unhandled symbol data in parse_separated_code_symbols - {symbol_data:?}")
        }
    }
}
