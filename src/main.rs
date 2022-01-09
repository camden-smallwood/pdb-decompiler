mod cpp;

use pdb::{FallibleIterator, IdData, PDB};
use std::{
    collections::{HashMap, HashSet},
    error::Error,
    fs::{self, File},
    io::Write,
    path::PathBuf,
};
use structopt::StructOpt;

fn parse_base_address(src: &str) -> Result<u64, std::num::ParseIntError> {
    u64::from_str_radix(src.trim_start_matches("0x"), 16)
}

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

fn main() -> Result<(), Box<dyn Error>> {
    let options = Options::from_args();

    let out_path: PathBuf = options.out.ok_or("Out path not supplied".to_string())?;
    println!("out: {}", out_path.to_string_lossy());
    
    let mut pdb = PDB::open(File::open(options.pdb.ok_or("PDB path not provided")?)?)?;
    let dbi = pdb.debug_information()?;
    let address_map = pdb.address_map()?;
    let string_table = pdb.string_table()?;

    let mut modules = HashMap::new();

    let mut script_file = File::create(out_path.join("ida_script.py"))?;
    writeln!(script_file, include_str!("../ida_script_base.py"))?;

    //
    // Process type information
    //

    let type_info = pdb.type_information()?;
    let mut type_finder = type_info.finder();
    let mut type_iter = type_info.iter();

    loop {
        type_finder.update(&type_iter);

        match type_iter.next() {
            Ok(Some(_)) => (),
            Ok(None) => break,
            Err(err) => panic!("Failed to get next type - {}", err)
        }
    }

    //
    // Process id information
    //

    let id_info = pdb.id_information()?;
    let mut id_finder = id_info.finder();
    let mut id_iter = id_info.iter();

    while let Some(id) = id_iter.next()? {
        id_finder.update(&id_iter);

        if let IdData::UserDefinedTypeSource(data) = id.parse()? {
            let mut module_path = match data.source_file {
                pdb::UserDefinedTypeSourceFileRef::Local(id) => match id_finder.find(id) {
                    Ok(item) => match item.parse() {
                        Ok(IdData::String(source_file)) => source_file.name.to_string().to_string().replace("\\", "/"),
                        Ok(data) => panic!("invalid UDT source file id: {:#?}", data),
                        Err(error) => panic!("failed to parse UDT source file id: {}", error)
                    }
                    
                    Err(error) => panic!("failed to find UDT source file id: {}", error)
                }

                pdb::UserDefinedTypeSourceFileRef::Remote(_, source_line_ref) => {
                    source_line_ref.to_string_lossy(&string_table)?.to_string().replace("\\", "/")
                }
            };

            if let Some((_, fixed_module_path)) = module_path.split_once(":/") {
                module_path = fixed_module_path.to_string();
            }

            println!("found UDT (local): {:?}", data);
            
            modules
                .entry(module_path.clone())
                .or_insert(cpp::Module::new().with_path(module_path.into()))
                .add_type_definition(&type_info, &type_finder, data.udt, data.line)?;
        }
    }

    let global_symbols = pdb.global_symbols()?;
    let mut global_symbols_iter = global_symbols.iter();

    while let Some(symbol) = global_symbols_iter.next()? {
        let symbol_data = match symbol.parse() {
            Ok(symbol_data) => symbol_data,
            Err(error) => {
                println!("failed to parse symbol data: {}", error);
                continue;
            }
        };

        if let pdb::SymbolData::UserDefinedType(_) = symbol_data {
            println!("found UDT (global): {:?}", symbol_data);
        }
    }
    
    //
    // Process module debug information
    //

    let mut module_iter = dbi.modules()?;

    while let Some(ref module) = module_iter.next()? {
        let module_info = match pdb.module_info(module)? {
            Some(module_info) => module_info,
            None => {
                println!("module has no info: {} - {}", module.module_name(), module.object_file_name());
                continue;
            },
        };

        let line_program = match module_info.line_program() {
            Ok(line_program) => line_program,
            Err(err) => {
                println!("failed to get module line program: {} - {:?}", err, module);
                continue;
            }
        };

        let mut symbols = match module_info.symbols() {
            Ok(symbols) => symbols,
            Err(err) => panic!("failed to parse module symbols: {}", err)
        };

        let (path, headers, members) = parse_module(
            options.base_address,
            module,
            &address_map,
            &string_table,
            &type_info,
            &type_finder,
            &id_info,
            &id_finder,
            &line_program,
            &mut dbi.modules()?,
            &mut symbols,
            &mut pdb.global_symbols()?.iter(),
            &mut dbi.section_contributions()?,
            &mut script_file
        )?;

        let source_file = match path.as_ref() {
            Some(path) => path.to_string_lossy().to_string(),
            None => {
                println!("module has no source file: {:#?}", module);
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
        let path = PathBuf::from(format!("{}{}", out_path.to_string_lossy(), entry.0).replace("\\", "/"));
        
        if let Some(parent_path) = path.parent() {
            fs::create_dir_all(parent_path)?;
        }

        let mut file = File::create(path)?;
        write!(file, "{}", entry.1)?;
    }

    Ok(())
}

fn parse_module(
    // TODO: map this stuff into a struct...
    base_address: Option<u64>,
    module: &pdb::Module,
    address_map: &pdb::AddressMap,
    string_table: &pdb::StringTable,
    type_info: &pdb::TypeInformation,
    type_finder: &pdb::TypeFinder,
    _id_info: &pdb::IdInformation,
    id_finder: &pdb::IdFinder,
    line_program: &pdb::LineProgram,
    modules: &mut pdb::ModuleIter,
    module_symbols: &mut pdb::SymbolIter,
    global_symbols: &mut pdb::SymbolIter,
    section_contributions: &mut pdb::DBISectionContributionIter,
    script_file: &mut File,
) -> pdb::Result<(Option<PathBuf>, Vec<PathBuf>, Vec<(PathBuf, cpp::ModuleMember)>)> {
    let mut headers = vec![];
    let mut members = vec![];
    let mut line_offsets = HashMap::new();
    let mut module_file_name_ref = None;
    let mut module_file_path = None;

    let obj_path = PathBuf::from(module.module_name().replace("\\", "/")[2..].to_string());

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
        let path = PathBuf::from(file.name.to_string_lossy(string_table).unwrap_or("".into()).replace("\\", "/")[2..].to_string());
        
        match path.extension() {
            Some(extension) => match extension.to_str().unwrap_or("") {
                "c" | "cc" | "cpp" | "cxx" | "pch" | "masm" | "asm" if path.file_stem().map(|x| x.to_ascii_lowercase()) == obj_path.file_stem().map(|x| x.to_ascii_lowercase()) => {
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
    let module_file_path = module_file_path.unwrap();

    while let Some(symbol) = global_symbols.next()? {
        let symbol_data = match symbol.parse() {
            Ok(symbol_data) => symbol_data,
            Err(error) => {
                println!("failed to parse symbol data: {}", error);
                continue;
            }
        };

        match symbol_data {
            pdb::SymbolData::Data(pdb::DataSymbol { global: true, name, offset, .. }) |
            pdb::SymbolData::ThreadStorage(pdb::ThreadStorageSymbol { global: true, name, offset, .. }) |
            pdb::SymbolData::Procedure(pdb::ProcedureSymbol { global: true, name, offset, .. }) => {
                while let Some(contribution) = section_contributions.next()? {
                    if let Some(contributing_module) = modules.nth(contribution.module as _)? {
                        if module.module_name() == contributing_module.module_name() && offset >= contribution.offset && offset < contribution.offset + contribution.size {
                            match name.to_string().to_string().as_str() {
                                x if x.contains("@") || x.contains("`") || x.contains("$") => (),
                                _ => {
                                    match symbol.parse()? {
                                        pdb::SymbolData::Data(data_symbol) => {
                                            match data_symbol.name.to_string().to_string().as_str() {
                                                x if x.contains("`") || x.contains("$") => {
                                                    println!("skipping compiler-generated data: {:?}", data_symbol);
                                                    continue;
                                                }
                            
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
                            
                                            let address = base_address.unwrap_or(0) + data_symbol.offset.to_rva(address_map).unwrap().0 as u64;
                            
                                            members.push(
                                                (
                                                    PathBuf::from(file_name_ref.to_string_lossy(string_table).unwrap_or("".into()).replace("\\", "/")[2..].to_string()),
                                                    cpp::ModuleMember::Declaration(
                                                        format!(
                                                            "{}; // 0x{:X}",
                                                            cpp::type_name(
                                                                type_info,
                                                                type_finder,
                                                                data_symbol.type_index,
                                                                Some(data_symbol.name.to_string().to_string()),
                                                                None,
                                                                false
                                                            )?,
                                                            address
                                                        )
                                                    )
                                                )
                                            );
                            
                                            writeln!(
                                                script_file,
                                                "decompile_to_file(0x{:X}, \"{}\")",
                                                address,
                                                module_file_path.to_string_lossy().replace("\\", "\\\\")
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
                                            let address = base_address.unwrap_or(0) + thread_storage_symbol.offset.to_rva(address_map).unwrap().0 as u64;
                        
                                            members.push(
                                                (
                                                    PathBuf::from(file_name_ref.to_string_lossy(string_table).unwrap_or("".into()).replace("\\", "/")[2..].to_string()),
                                                    cpp::ModuleMember::Declaration(
                                                        format!(
                                                            "thread_local {}; // 0x{:X}",
                                                            cpp::type_name(
                                                                type_info,
                                                                type_finder,
                                                                thread_storage_symbol.type_index,
                                                                Some(thread_storage_symbol.name.to_string().to_string()),
                                                                None,
                                                                false
                                                            )?,
                                                            address
                                                        )
                                                    )
                                                )
                                            );
                        
                                            writeln!(
                                                script_file,
                                                "decompile_to_file(0x{:X}, \"{}\")",
                                                address,
                                                module_file_path.to_string_lossy().replace("\\", "\\\\")
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
                                                println!("skipping compiler-generated procedure: {}", procedure);
                                                continue;
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
                            
                                            let address = base_address.unwrap_or(0) + procedure_symbol.offset.to_rva(address_map).unwrap().0 as u64;
                            
                                            members.push(
                                                (
                                                    PathBuf::from(file_name_ref.to_string_lossy(string_table).unwrap_or("".into()).replace("\\", "/")[2..].to_string()),
                                                    cpp::ModuleMember::Declaration(
                                                        format!("{}; // 0x{:X}", procedure, address)
                                                    )
                                                )
                                            );
                            
                                            writeln!(
                                                script_file,
                                                "decompile_to_file(0x{:X}, \"{}\")",
                                                address,
                                                module_file_path.to_string_lossy().replace("\\", "\\\\")
                                            )?;
                                        }
                                        
                                        _ => ()
                                    }
                                }
                            }
                        }
                    }
                }
            }

            _ => ()
        }
    }

    while let Some(symbol) = module_symbols.next()? {
        let symbol_data = match symbol.parse() {
            Ok(symbol_data) => symbol_data,
            Err(err) => {
                println!("debug: failed to parse symbol data: {}", err);
                continue;
            }
        };

        match symbol_data {
            pdb::SymbolData::UsingNamespace(symbol) => members.push(
                (
                    PathBuf::from(module_file_name_ref.to_string_lossy(string_table).unwrap_or("".into()).replace("\\", "/")[2..].to_string()),
                    cpp::ModuleMember::Declaration(
                        format!("using namespace {};", symbol.name.to_string().to_string())
                    )
                )
            ),

            pdb::SymbolData::Constant(symbol) => members.push(
                (
                    PathBuf::from(module_file_name_ref.to_string_lossy(string_table).unwrap_or("".into()).replace("\\", "/")[2..].to_string()),
                    cpp::ModuleMember::Declaration(
                        format!(
                            "const {} = {};",
                            cpp::type_name(type_info, type_finder, symbol.type_index, Some(symbol.name.to_string().to_string()), None, false)?,
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
            ),

            pdb::SymbolData::Data(data_symbol) => {
                match data_symbol.name.to_string().to_string().as_str() {
                    x if x.contains("`") || x.contains("$") => {
                        println!("skipping compiler-generated data: {:?}", data_symbol);
                        continue;
                    }

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
                        println!("warning: no RVA found for data symbol: {:?}", data_symbol);
                        continue;
                    }
                };
                
                let address = base_address.unwrap_or(0) + rva.0 as u64;

                members.push(
                    (
                        PathBuf::from(file_name_ref.to_string_lossy(string_table).unwrap_or("".into()).replace("\\", "/")[2..].to_string()),
                        cpp::ModuleMember::Declaration(
                            format!(
                                "{}; // 0x{:X}",
                                cpp::type_name(
                                    type_info,
                                    type_finder,
                                    data_symbol.type_index,
                                    Some(data_symbol.name.to_string().to_string()),
                                    None,
                                    false
                                )?,
                                address
                            )
                        )
                    )
                );

                writeln!(
                    script_file,
                    "decompile_to_file(0x{:X}, \"{}\")",
                    address,
                    module_file_path.to_string_lossy().replace("\\", "\\\\")
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
                let address = base_address.unwrap_or(0) + thread_storage_symbol.offset.to_rva(address_map).unwrap().0 as u64;

                members.push(
                    (
                        PathBuf::from(file_name_ref.to_string_lossy(string_table).unwrap_or("".into()).replace("\\", "/")[2..].to_string()),
                        cpp::ModuleMember::Declaration(
                            format!(
                                "thread_local {}; // 0x{:X}",
                                cpp::type_name(
                                    type_info,
                                    type_finder,
                                    thread_storage_symbol.type_index,
                                    Some(thread_storage_symbol.name.to_string().to_string()),
                                    None,
                                    false
                                )?,
                                address
                            )
                        )
                    )
                );

                writeln!(
                    script_file,
                    "decompile_to_file(0x{:X}, \"{}\")",
                    address,
                    module_file_path.to_string_lossy().replace("\\", "\\\\")
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
                    println!("skipping compiler-generated procedure: {}", procedure);
                    continue;
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

                let address = base_address.unwrap_or(0) + procedure_symbol.offset.to_rva(address_map).unwrap().0 as u64;

                members.push(
                    (
                        PathBuf::from(file_name_ref.to_string_lossy(string_table).unwrap_or("".into()).replace("\\", "/")[2..].to_string()),
                        cpp::ModuleMember::Declaration(
                            format!("{}; // 0x{:X}", procedure, address)
                        )
                    )
                );

                writeln!(
                    script_file,
                    "decompile_to_file(0x{:X}, \"{}\")",
                    address,
                    module_file_path.to_string_lossy().replace("\\", "\\\\")
                )?;
            }

            pdb::SymbolData::Thunk(_) => parse_thunk_symbols(module_symbols),

            pdb::SymbolData::ObjName(_) => println!("found obj name in module: {:?}", symbol_data),
            pdb::SymbolData::BuildInfo(build_info) => println!("found build info in module: {:?}", id_finder.find(build_info.id)?.parse()?),
            pdb::SymbolData::CompileFlags(_) => println!("found compile flags in module: {:?}", symbol_data),
            pdb::SymbolData::UserDefinedType(_) => println!("found user defined type in module: {:?}", symbol_data),
            pdb::SymbolData::Export(_) => println!("found export in module: {:?}", symbol_data),
            pdb::SymbolData::Label(_) => println!("found label in module: {:?}", symbol_data),

            pdb::SymbolData::SeparatedCode(_) => parse_separated_code_symbols(module_symbols),

            ref data => panic!("Unhandled module symbol: {:#?}", data)
        }
    }

    Ok((Some(module_file_path), headers, members))
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

            pdb::SymbolData::Constant(_) => println!("found constant symbol in procedure: {:?}", symbol_data),
            pdb::SymbolData::Data(_) => println!("found data symbol in procedure: {:?}", symbol_data),
            pdb::SymbolData::Local(_) => (),
            pdb::SymbolData::Label(_) => (),
            pdb::SymbolData::UserDefinedType(_) => println!("found UDT symbol in procedure: {:?}", symbol_data),
            pdb::SymbolData::Block(_) => parse_block_symbols(symbols),
            pdb::SymbolData::InlineSite(_) => parse_inline_site_symbols(symbols),
            pdb::SymbolData::RegisterRelative(x) => parameter_names.push(x.name.to_string().to_string()),
            pdb::SymbolData::RegisterVariable(_) => (),
            pdb::SymbolData::ThreadStorage(_) => println!("found thread storage symbol in procedure: {:?}", symbol_data),
            pdb::SymbolData::SeparatedCode(_) => parse_separated_code_symbols(symbols),

            data => panic!("Unhandled symbol data in parse_procedure_symbols - {:?}", data)
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

            pdb::SymbolData::Constant(_) => println!("found constant symbol in block: {:?}", symbol_data),
            pdb::SymbolData::Data(_) => println!("found data symbol in block: {:?}", symbol_data),
            pdb::SymbolData::Local(_) => (),
            pdb::SymbolData::RegisterRelative(_) => (),
            pdb::SymbolData::Label(_) => (),
            pdb::SymbolData::UserDefinedType(_) => println!("found UDT symbol in block: {:?}", symbol_data),
            
            pdb::SymbolData::Block(_) => parse_block_symbols(symbols),
            pdb::SymbolData::InlineSite(_) => parse_inline_site_symbols(symbols),
            pdb::SymbolData::SeparatedCode(_) => parse_separated_code_symbols(symbols),

            data => panic!("Unhandled symbol data in parse_block_symbols - {:?}", data)
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

            pdb::SymbolData::Constant(_) => println!("found constant symbol in inline site: {:?}", symbol_data),
            pdb::SymbolData::Data(_) => println!("found data symbol in inline site: {:?}", symbol_data),
            pdb::SymbolData::Local(_) => (),
            pdb::SymbolData::RegisterRelative(_) => (),
            pdb::SymbolData::Label(_) => (),
            pdb::SymbolData::UserDefinedType(_) => println!("found UDT symbol in inline site: {:?}", symbol_data),
            
            pdb::SymbolData::Block(_) => parse_block_symbols(symbols),
            pdb::SymbolData::InlineSite(_) => parse_inline_site_symbols(symbols),
            pdb::SymbolData::SeparatedCode(_) => parse_separated_code_symbols(symbols),
            
            _ => panic!("Unhandled symbol data in parse_inline_site_symbols - {:?}", symbol_data)
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
            _ => panic!("Unhandled symbol data in parse_thunk_symbols - {:?}", symbol_data)
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
            _ => panic!("Unhandled symbol data in parse_separated_code_symbols - {:?}", symbol_data)
        }
    }
}
