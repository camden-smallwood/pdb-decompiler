use crate::{cpp, options::Options};
use pdb2::FallibleIterator;
use std::{
    cell::RefCell,
    collections::HashMap,
    error::Error,
    fs::{File, OpenOptions},
    io::Write,
    path::PathBuf,
    rc::Rc,
};

pub struct Decompiler {
    pub options: Options,
    pub class_table: Vec<Rc<RefCell<cpp::Class>>>,
    pub type_sizes: HashMap<String, u64>,
    pub modules: HashMap<String, Rc<RefCell<cpp::Module>>>,
    pub function_scopes_modules: Option<HashMap<String, Rc<RefCell<cpp::Module>>>>,
}

pub struct DecompileContext<'a, 's> {
    pub pdb: &'a mut pdb2::PDB<'s, File>,
    pub debug_info: &'a pdb2::DebugInformation<'s>,
    pub machine_type: pdb2::MachineType,
    pub address_map: &'a pdb2::AddressMap<'s>,
    pub string_table: Option<&'a pdb2::StringTable<'s>>,
    pub id_info: &'a pdb2::IdInformation<'s>,
    pub id_finder: pdb2::IdFinder<'a>,
    pub type_info: &'a pdb2::TypeInformation<'s>,
    pub type_finder: pdb2::TypeFinder<'a>,
    pub global_symbols: &'a pdb2::SymbolTable<'s>,
    pub script_file: &'a mut File,
}

impl Decompiler {
    #[inline(always)]
    pub fn new(options: Options) -> Self {
        Self {
            options,
            class_table: vec![],
            type_sizes: HashMap::new(),
            modules: HashMap::new(),
            function_scopes_modules: None,
        }
    }

    #[inline(always)]
    pub fn with_function_scopes_modules(mut self, function_scopes_modules: Option<HashMap<String, Rc<RefCell<cpp::Module>>>>) -> Self {
        self.function_scopes_modules = function_scopes_modules;
        self
    }

    #[inline(always)]
    pub fn decompile(&mut self) -> Result<(), Box<dyn Error>> {
        let pdb_path = self.options.pdb.clone()
            .ok_or("PDB path not provided")?;

        let mut pdb = pdb2::PDB::open(File::open(pdb_path)?)?;

        let mut debug_info = pdb.debug_information()
            .map_err(|e| format!("does not contain debug information: {e}"))?;

        let machine_type = debug_info.machine_type()
            .unwrap_or(pdb2::MachineType::Unknown);
        
        let mut address_map = pdb.address_map()
            .map_err(|e| format!("does not contain an address map: {e}"))?;
        
        let string_table = pdb.string_table()
            .ok(); // this isn't present in older versions, so we make it optional

        let type_info = pdb.type_information()
            .map_err(|e| format!("does not contain type information: {e}"))?;

        let id_info = pdb.id_information()
            .map_err(|e| format!("does not contain id information: {e}"))?;

        let global_symbols = pdb.global_symbols()
            .map_err(|e| format!("does not contain global symbol information: {e}"))?;
        
        let type_finder = type_info.finder();
        let id_finder = id_info.finder();

        let out_path = self.options.out.as_ref()
            .ok_or_else(|| "Out path not supplied".to_string())?;

        std::fs::create_dir_all(out_path.clone())?;

        let mut script_file = File::create(out_path.join("ida_script.py"))?;
        writeln!(script_file, "{}", include_str!("../ida_script_base.py"))?;

        let mut context = DecompileContext {
            pdb: &mut pdb,
            debug_info: &mut debug_info,
            machine_type,
            address_map: &mut address_map,
            string_table: string_table.as_ref(),
            id_info: &id_info,
            id_finder: id_finder,
            type_info: &type_info,
            type_finder: type_finder,
            global_symbols: &global_symbols,
            script_file: &mut script_file,
        };
        
        self.process_type_information(&mut context)?;
        self.process_id_information(&mut context)?;
        self.process_modules(&mut context)?;

        if self.options.export_pseudocode_to_json {
            writeln!(script_file)?;
            writeln!(script_file, "with open('pseudocode.json', 'wb') as outfile:")?;
            writeln!(script_file, "    outfile.write('{{'.encode('utf-8'))")?;
            writeln!(script_file, "    if len(json_lines) > 0:")?;
            writeln!(script_file, "        last_line = json_lines[-1]")?;
            writeln!(script_file, "        if last_line.endswith(',\\n'):")?;
            writeln!(script_file, "            json_lines[-1] = last_line.removesuffix(',\\n') + '\\n'")?;
            writeln!(script_file, "        for json_line in json_lines:")?;
            writeln!(script_file, "            outfile.write(json_line.encode('utf-8'))")?;
            writeln!(script_file, "    outfile.write('}}'.encode('utf-8'))")?;
            writeln!(script_file)?;
        }

        writeln!(script_file, "print(\"Done.\")")?;

        Ok(())
    }

    #[inline(always)]
    fn process_type_information<'a, 's>(&mut self, context: &mut DecompileContext<'a, 's>) -> pdb2::Result<()> {
        let export_all = context.id_info.is_empty();

        let mut type_iter = context.type_info.iter();

        let mut exported_forward_reference_indices = vec![];
        let mut exported_type_indices = vec![];

        while let Ok(Some(type_item)) = type_iter.next() {
            context.type_finder.update(&type_iter);
            
            let type_data = match type_item.parse() {
                Ok(x) => x,
                Err(e) => {
                    println!("WARNING: Failed to parse type: {e} - {type_item:#?}");
                    continue;
                }
            };

            if let Some((name, size)) = match &type_data {
                pdb2::TypeData::Class(class_type) => Some((class_type.name.to_string().to_string(), class_type.size)),
                pdb2::TypeData::Union(union_type) => Some((union_type.name.to_string().to_string(), union_type.size)),
                _ => None,
            } {
                if size != 0 {
                    *self.type_sizes.entry(name).or_default() = size;
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

        let exported_path = PathBuf::from(format!("{}/exported.h", self.options.out.as_ref().unwrap().to_string_lossy()));

        let mut module = cpp::Module::default();
        module.path = exported_path.clone();

        for type_index in exported_forward_reference_indices {
            if let Err(e) = module.add_type_definition(&mut self.class_table, &mut self.type_sizes, context.machine_type, &context.type_info, &context.type_finder, type_index, 0) {
                panic!("{e}");
            }
        }

        for type_index in exported_type_indices {
            if let Err(e) = module.add_type_definition(&mut self.class_table, &mut self.type_sizes, context.machine_type, &context.type_info, &context.type_finder, type_index, 0) {
                panic!("{e}");
            }
        }

        if let Some(parent_path) = exported_path.parent()
            && let Err(e) = std::fs::create_dir_all(parent_path)
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
    fn process_id_information<'a, 's>(&mut self, context: &mut DecompileContext<'a, 's>) -> pdb2::Result<()> {
        let mut id_iter = context.id_info.iter();

        while let Some(id) = id_iter.next()? {
            context.id_finder.update(&id_iter);

            let id_data = match id.parse() {
                Ok(id_data) => id_data,
                Err(e) => {
                    println!("WARNING: failed to parse id: {e}");
                    continue;
                }
            };

            match id_data {
                pdb2::IdData::BuildInfo(build_info) => {
                    let mut module = cpp::Module::default();
                    module.add_build_info(self.options.out.as_ref().unwrap(), &mut context.id_finder, build_info)?;

                    let module_key = module.path.to_string_lossy().to_lowercase().to_string();
                    if module_key.is_empty() {
                        continue;
                    }
                    
                    if let Some(old_module) = self.modules.insert(module_key.clone(), Rc::new(RefCell::new(module))) {
                        let module = self.modules.get_mut(&module_key).unwrap();
                        module.borrow_mut().headers = old_module.borrow().headers.clone();
                        module.borrow_mut().members = old_module.borrow().members.clone();
                    }
                }
                
                pdb2::IdData::UserDefinedTypeSource(data) => {
                    let module_path = match data.source_file {
                        pdb2::UserDefinedTypeSourceFileRef::Local(id) => match context.id_finder.find(id) {
                            Ok(item) => match item.parse() {
                                Ok(pdb2::IdData::String(source_file)) => crate::utils::sanitize_path(&source_file.name.to_string()),
                                Ok(data) => panic!("invalid UDT source file id: {:#?}", data),
                                Err(error) => panic!("failed to parse UDT source file id: {error}"),
                            },

                            Err(error) => panic!("failed to find UDT source file id: {error}"),
                        },

                        pdb2::UserDefinedTypeSourceFileRef::Remote(_, source_line_ref) => {
                            crate::utils::sanitize_path(&source_line_ref.to_string_lossy(context.string_table.unwrap())?)
                        }
                    };

                    self.modules
                        .entry(module_path.to_lowercase())
                        .or_insert_with(|| Rc::new(RefCell::new(cpp::Module::default().with_path(module_path.into()))))
                        .borrow_mut()
                        .add_type_definition(
                            &mut self.class_table,
                            &mut self.type_sizes,
                            context.machine_type,
                            context.type_info,
                            &context.type_finder,
                            data.udt,
                            data.line,
                        )?;
                }

                _ => {}
            }
        }

        Ok(())
    }

    #[inline(always)]
    fn process_modules<'a, 's>(&mut self, context: &mut DecompileContext<'a, 's>) -> Result<(), Box<dyn Error>> {
        let module_global_symbols = self.load_module_global_symbols(context)?;

        let mut module_iter = context.debug_info.modules().map_err(|e| format!("does not contain debug module info: {e}"))?;

        while let Some(module) = module_iter.next().map_err(|e| format!("failed to get next debug module info: {e}"))? {
            if let Err(_) = self.process_module(context, &module, &module_global_symbols) {
                // println!("WARNING: failed to parse module, skipping: {e} - {module:#?}");
                continue;
            }
        }

        //
        // Finalize module debug information
        //

        let mut header_modules = HashMap::new();

        for (_, module) in self.modules.iter_mut() {
            //
            // Clean up include paths and generate modules for them
            //

            let mut module = module.borrow_mut();
            let mut headers = vec![];

            for (header_path, is_global) in module.headers.iter() {
                let header_module_key = header_path.to_string_lossy().to_lowercase().to_string();

                if !header_modules.contains_key(&header_module_key) {
                    header_modules.insert(header_module_key.clone(), cpp::Module::default().with_path(header_path.clone()));
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
            module.members = storage.iter().map(|m| m.1.clone()).collect::<Vec<_>>();

            //
            // Replace nested type declarations and anonymous field types with their full types
            //
            
            let mut new_members = vec![];
            let mut remove_class_names = vec![];
            let mut remove_enum_names = vec![];

            for i in 0..module.members.len() {
                match &module.members[i] {
                    cpp::ModuleMember::Class(class_data) => {
                        Self::fix_nested_types(module.members.as_slice(), i, class_data.clone(), &mut remove_class_names, &mut remove_enum_names);
                        new_members.push(cpp::ModuleMember::Class(class_data.clone()));
                    }

                    _ => {
                        new_members.push(module.members[i].clone());
                    }
                }
            }

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
            
            for i in (0..module.members.len()).rev() {
                match &module.members[i] {
                    cpp::ModuleMember::Class(class_data) => {
                        let is_nested_type = {
                            let class_data = class_data.borrow();
                            if let Some(properties) = class_data.properties.as_ref() {
                                properties.is_nested_type()
                            } else {
                                false
                            }
                        };

                        if is_nested_type {
                            // let class_kind = match class_data.borrow().kind {
                            //     Some(pdb2::ClassKind::Class) => "class",
                            //     Some(pdb2::ClassKind::Struct) => "struct",
                            //     Some(pdb2::ClassKind::Interface) => "interface",
                            //     None if class_data.borrow().is_union => "union",
                            //     None => todo!(),
                            // };
                            // println!("WARNING: Removing unreferenced nested {class_kind} in toplevel: \"{}\" in \"{}\"", class_data.borrow().name, module.path.display());
                            module.members.remove(i);
                        }
                    }
                    
                    cpp::ModuleMember::Enum(enum_data) => {
                        if enum_data.properties.is_nested_type() {
                            // println!("WARNING: Removing unreferenced nested enum in toplevel: \"{}\" in \"{}\"", enum_data.name, module.path.display());
                            module.members.remove(i);
                        }
                    }

                    _ => {}
                }
            }
        }

        for (k, v) in header_modules {
            if !self.modules.contains_key(&k) {
                self.modules.insert(k, Rc::new(RefCell::new(v)));
            }
        }

        //
        // Include extra function scopes if present
        //

        if let Some(function_scopes_modules) = self.function_scopes_modules.as_ref() {
            for function_scopes_module in function_scopes_modules.values() {
                for module in self.modules.values_mut() {
                    let mut module = module.borrow_mut();

                    if module.path.file_stem() == function_scopes_module.borrow().path.file_stem() {
                        for function_scope_member in function_scopes_module.borrow().members.iter() {
                            let cpp::ModuleMember::Procedure(function_scope_procedure) = function_scope_member else {
                                continue;
                            };

                            if function_scope_procedure.body.as_ref().map(|b| b.statements.is_empty()).unwrap_or(true) {
                                continue;
                            }

                            let Some(member) = module.members.iter_mut().find(|m| {
                                let cpp::ModuleMember::Procedure(p) = m else {
                                    return false;
                                };

                                p.body.is_some() && p.signature == function_scope_procedure.signature
                            }) else {
                                continue;
                            };

                            let cpp::ModuleMember::Procedure(procedure) = member else {
                                unreachable!()
                            };

                            let Some(procedure_body) = procedure.body.as_mut() else {
                                unreachable!();
                            };
                            
                            if !procedure_body.statements.is_empty() {
                                procedure_body.statements.push(cpp::Statement::EmptyLine);
                            }

                            procedure_body.statements.push(cpp::Statement::Comment("debug:".to_string()));
                            procedure_body.statements.push(cpp::Statement::Commented(Box::new(
                                cpp::Statement::Block(cpp::Block {
                                    address: if self.options.verbose_blocks {
                                        function_scope_procedure.body.as_ref().unwrap().address.clone()
                                    } else {
                                        None
                                    },
                                    statements: function_scope_procedure.body.as_ref().unwrap().statements.clone(),
                                }),
                            )));
                        }
                    }
                }
            }
        }

        //
        // Find `c_enum` and `c_flags` typedefs
        //

        let mut compound_enums = vec![];

        if self.options.reorganize {
            crate::reorganize::collect_compound_enums(&mut self.modules, &mut compound_enums);
        }
        
        //
        // Post-process and write modules to file
        //

        for module in self.modules.values_mut() {
            let mut module = module.borrow_mut();

            let path = PathBuf::from(crate::utils::sanitize_path(format!(
                "{}/{}",
                self.options.out.as_ref().unwrap().to_string_lossy(),
                module.path.to_string_lossy().trim_start_matches('/'),
            )));

            //
            // Reorganize module members if requested
            //

            if self.options.reorganize {
                crate::reorganize::reorganize_module_members(
                    &mut self.class_table,
                    &mut self.type_sizes,
                    context.machine_type,
                    context.type_info,
                    &context.type_finder,
                    &mut module,
                    compound_enums.as_slice(),
                )?;
            }

            //
            // Write the module out to its file
            //

            if let Some(parent_path) = path.parent() {
                if let Err(e) = std::fs::create_dir_all(parent_path) {
                    panic!("Failed to create directory: \"{}\" - {e}", parent_path.display())
                }
            }

            let mut file = if path.exists() {
                match OpenOptions::new().write(true).append(true).open(path.clone()) {
                    Ok(x) => x,
                    Err(e) => panic!("Failed to append to file: \"{}\" - {e}", path.display()),
                }
            } else {
                match File::create(path.clone()) {
                    Ok(x) => x,
                    Err(e) => panic!("Failed to create file: \"{}\" - {e}", path.display()),
                }
            };

            if let Err(e) = write!(file, "{module}") {
                panic!("Failed to write to file: \"{}\" - {e}", path.display());
            }
        }

        Ok(())
    }

    #[inline(always)]
    fn load_module_global_symbols<'a, 's>(
        &mut self,
        context: &mut DecompileContext<'a, 's>
    ) -> Result<HashMap<String, Vec<pdb2::SymbolData<'a>>>, Box<dyn Error>> {
        let mut section_contributions = vec![];
        let mut section_contributions_iter = context.debug_info.section_contributions()?;

        while let Some(section_contribution) = section_contributions_iter.next()? {
            section_contributions.push(section_contribution);
        }

        let modules = context.debug_info.modules()?.collect::<Vec<_>>()?;

        let mut global_symbols_iter = context.global_symbols.iter();
        let mut module_global_symbols = HashMap::new();
        let mut prev_module_name = None;

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
                Err(e) => {
                    println!("WARNING: failed to parse symbol data, skipping 1: {e}");
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
                    println!("WARNING: found unused global symbol {symbol_data:?}");
                }
            }
        }

        Ok(module_global_symbols)
    }

    #[inline(always)]
    fn process_module<'a, 's>(
        &mut self,
        context: &mut DecompileContext<'a, 's>,
        pdb_module: &pdb2::Module,
        module_global_symbols: &HashMap<String, Vec<pdb2::SymbolData>>,
    ) -> Result<(), Box<dyn Error>> {
        let module_info = match context.pdb.module_info(pdb_module)? {
            Some(module_info) => module_info,
            None => {
                // println!("module has no info: {} - {}", module.module_name(), module.object_file_name());
                return Ok(());
            }
        };

        let line_program = module_info.line_program()?;
        let line_offsets = self.get_module_line_program_offsets(&line_program)?;
        
        let Some(module) = self.get_or_create_cpp_module(
            context,
            pdb_module,
            &module_info,
            &line_program,
        )? else { return Ok(()) };

        // Process module global symbols
        if let Some(global_symbols) = module_global_symbols.get(&pdb_module.module_name().to_string()).cloned() {
            for symbol_data in global_symbols {
                self.process_module_symbol_data(
                    context,
                    &line_offsets,
                    module.clone(),
                    None,
                    &symbol_data,
                )?;
            }
        }
        
        // Process module local symbols
        let mut module_symbols = module_info.symbols()?;

        while let Some(symbol) = module_symbols.next()? {
            let symbol_data = match symbol.parse() {
                Ok(symbol_data) => symbol_data,
                Err(e) => {
                    println!("WARNING: failed to parse symbol data, skipping 2: {e}");
                    continue;
                }
            };

            if let pdb2::SymbolData::UserDefinedType(_) = symbol_data {
                continue;
            }
            
            self.process_module_symbol_data(
                context,
                &line_offsets,
                module.clone(),
                Some(&mut module_symbols),
                &symbol_data
            )?;
        }

        Ok(())
    }

    #[inline(always)]
    fn get_module_object_file_path(&mut self, module: &pdb2::Module) -> pdb2::Result<PathBuf> {
        let mut obj_path = PathBuf::from(crate::utils::sanitize_path(module.module_name()));

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
        &mut self,
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
    fn get_or_create_cpp_module<'a, 's>(
        &mut self,
        context: &mut DecompileContext<'a, 's>,
        module: &pdb2::Module,
        module_info: &pdb2::ModuleInfo,
        line_program: &pdb2::LineProgram,
    ) -> pdb2::Result<Option<Rc<RefCell<cpp::Module>>>> {
        let obj_path = self.get_module_object_file_path(module)?;
        
        let mut module_file_path = None;
        let mut headers = vec![];

        for file in line_program.files().collect::<Vec<_>>()? {
            let path = PathBuf::from(crate::utils::sanitize_path(&file.name.to_string_lossy(context.string_table.unwrap())?));

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
                let Ok(id_item) = context.id_finder.find(build_info.id) else { continue };
                let Ok(pdb2::IdData::BuildInfo(build_info)) = id_item.parse() else { continue };
                
                let mut module = cpp::Module::default();
                module.add_build_info(self.options.out.as_ref().unwrap(), &context.id_finder, build_info)?;

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

        let Some(module_file_path) = module_file_path else {
            return Ok(None);
        };

        let mut module_headers = vec![];

        for header_path in headers.iter() {
            let header_module_key = header_path.to_string_lossy().to_lowercase().to_string();

            if !self.modules.contains_key(&header_module_key) {
                self.modules.insert(header_module_key.clone(), Rc::new(RefCell::new(cpp::Module::default().with_path(header_path.clone()))));
            }

            module_headers.push((header_path.clone(), false));
        }

        let module_key = module_file_path.to_string_lossy().to_lowercase();
        let module = self.modules.entry(module_key).or_insert_with(|| Rc::new(RefCell::new(cpp::Module::default().with_path(module_file_path))));
        
        for header in module_headers {
            if !module.borrow().headers.contains(&header) {
                module.borrow_mut().headers.push(header);
            }
        }

        Ok(Some(module.clone()))
    }

    #[inline(always)]
    fn process_module_symbol_data<'d, 'a, 's>(
        &'d mut self,
        context: &mut DecompileContext<'a, 's>,
        line_offsets: &HashMap<pdb2::StringRef, HashMap<pdb2::PdbInternalSectionOffset, pdb2::LineInfo>>,
        module: Rc<RefCell<cpp::Module>>,
        module_symbols: Option<&mut pdb2::SymbolIter>,
        symbol_data: &pdb2::SymbolData,
    ) -> pdb2::Result<()> {
        match symbol_data {
            pdb2::SymbolData::UsingNamespace(symbol) => {
                let using_namespace = cpp::ModuleMember::UsingNamespace(symbol.name.to_string().to_string());

                if !module.borrow().members.contains(&using_namespace) {
                    module.borrow_mut().members.push(using_namespace);
                }
            }

            pdb2::SymbolData::UserDefinedType(udt_symbol) => {
                let type_name = cpp::type_name(
                    &mut self.class_table,
                    &mut self.type_sizes,
                    context.machine_type,
                    &context.type_info,
                    &context.type_finder,
                    udt_symbol.type_index,
                    None,
                    Some(udt_symbol.name.to_string().to_string()),
                    None,
                    None,
                    false
                )?;

                let user_defined_type = cpp::ModuleMember::TypeDefinition(cpp::TypeDefinition {
                    type_name,
                    underlying_type: udt_symbol.type_index,
                    field_attributes: None,
                    pointer_attributes: None,
                    containing_class: None,
                });

                if !module.borrow().members.contains(&user_defined_type) {
                    module.borrow_mut().members.push(user_defined_type);
                }
            }

            pdb2::SymbolData::Constant(constant_symbol) => {
                let type_name = cpp::type_name(
                    &mut self.class_table,
                    &mut self.type_sizes,
                    context.machine_type,
                    &context.type_info,
                    &context.type_finder,
                    constant_symbol.type_index,
                    None,
                    Some(constant_symbol.name.to_string().to_string()),
                    None,
                    None,
                    false
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

                if !module.borrow().members.contains(&constant) {
                    module.borrow_mut().members.push(constant);
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

                let module = if let Some(file_name_ref) = file_name_ref {
                    let path = PathBuf::from(crate::utils::sanitize_path(file_name_ref.to_string_lossy(context.string_table.unwrap())?.to_string()));
                    let module_key = path.to_string_lossy().to_lowercase().to_string();
                    self.modules.entry(module_key).or_insert_with(|| Rc::new(RefCell::new(cpp::Module::default().with_path(path)))).clone()
                } else {
                    module
                };

                let rva = match data_symbol.offset.to_rva(context.address_map) {
                    Some(rva) => rva,
                    None => {
                        // println!("WARNING: no RVA found for data symbol: {data_symbol:?}");
                        return Ok(());
                    }
                };

                let address = self.options.base_address.unwrap_or(0) + rva.0 as u64;

                if module.borrow().members.iter().any(|member| match member {
                    cpp::ModuleMember::Data { address: a, .. } if *a == address => true,
                    _ => false,
                }) {
                    return Ok(());
                }

                module.borrow_mut().members.push(cpp::ModuleMember::Data {
                    is_static: !data_symbol.global,
                    name: data_symbol.name.to_string().to_string(),
                    signature: format!(
                        "{}; // 0x{address:X}",
                        cpp::type_name(
                            &mut self.class_table,
                            &mut self.type_sizes,
                            context.machine_type,
                            &context.type_info,
                            &context.type_finder,
                            data_symbol.type_index,
                            None,
                            Some(data_symbol.name.to_string().to_string()),
                            None,
                            None,
                            false
                        )?,
                    ),
                    address,
                    line: line_info.map(|x| x.line_start),
                });

                if self.options.export_pseudocode_to_files {
                    writeln!(context.script_file, "decompile_to_file(0x{address:X}, \"{}\")", module.borrow().path.to_string_lossy())?;
                }
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

                let module = if let Some(file_name_ref) = file_name_ref {
                    let path = PathBuf::from(crate::utils::sanitize_path(file_name_ref.to_string_lossy(context.string_table.unwrap())?.to_string()));
                    let module_key = path.to_string_lossy().to_lowercase().to_string();
                    self.modules.entry(module_key).or_insert_with(|| Rc::new(RefCell::new(cpp::Module::default().with_path(path)))).clone()
                } else {
                    module
                };

                let rva = match thread_storage_symbol.offset.to_rva(context.address_map) {
                    Some(rva) => rva,
                    None => {
                        // println!("WARNING: no RVA found for thread storage symbol: {thread_storage_symbol:?}");
                        return Ok(());
                    }
                };

                let address = self.options.base_address.unwrap_or(0) + rva.0 as u64;

                if module.borrow().members.iter().any(|member| match member {
                    cpp::ModuleMember::Data { address: a, .. } if *a == address => true,
                    _ => false,
                }) {
                    return Ok(());
                }

                // Check if this is a thread_local variable defined in a class/struct/union.
                let name = thread_storage_symbol.name.to_string().to_string();
                if name.contains("::") {
                    let (class_name, variable_name) = name.rsplit_once("::").unwrap();
                    
                    let full_classes = self.class_table.iter().filter(|c| {
                        let c = c.borrow();
                        c.name == class_name && !c.is_declaration
                    }).collect::<Vec<_>>();

                    for class in full_classes {
                        let mut class = class.borrow_mut();

                        for member in class.members.iter_mut() {
                            if let cpp::ClassMember::Field(cpp::Field { name, signature, .. }) = member
                                && name == variable_name
                            {
                                *signature = format!("static thread_local {}", signature.trim_start_matches("static "));
                            }
                        }
                    }
                }

                module.borrow_mut().members.push(cpp::ModuleMember::ThreadStorage {
                    is_static: !thread_storage_symbol.global,
                    name: thread_storage_symbol.name.to_string().to_string(),
                    signature: format!(
                        "thread_local {}; // 0x{address:X}",
                        cpp::type_name(
                            &mut self.class_table,
                            &mut self.type_sizes,
                            context.machine_type,
                            &context.type_info,
                            &context.type_finder,
                            thread_storage_symbol.type_index,
                            None,
                            Some(thread_storage_symbol.name.to_string().to_string()),
                            None,
                            None,
                            false
                        )?,
                    ),
                    address,
                    line: line_info.map(|x| x.line_start),
                });

                if self.options.export_pseudocode_to_files {
                    writeln!(context.script_file, "decompile_to_file(0x{address:X}, \"{}\")", module.borrow().path.to_string_lossy())?;
                }
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
                    frame_procedure,
                    mut body
                ) = self.parse_procedure_symbols(
                    context,
                    module_symbols,
                    procedure_symbol,
                )?;
                
                let is_static = !procedure_symbol.global;
                let is_inline = frame_procedure.as_ref().map(|x| x.flags.inline_spec).unwrap_or(false);

                let mut declspecs = vec![];

                if let Some(frame_procedure) = frame_procedure.as_ref() {
                    if frame_procedure.flags.naked {
                        declspecs.push("naked".into());
                    }

                    if frame_procedure.flags.gs_check {
                        declspecs.push("struct_gs_check".into());
                    }

                    if frame_procedure.flags.safe_buffers {
                        declspecs.push("safebuffers".into());
                    }
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

                let parameters = if register_variable_names.is_empty() {
                    register_relative_names
                } else {
                    register_variable_names
                };

                let procedure_type = context.type_finder.find(procedure_symbol.type_index)?.parse()?;

                let mut member_method_data = None;
                
                // If the procedure is a member function, propagate its attributes
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
                    
                    let full_classes = self.class_table.iter().filter(|c| {
                        let c = c.borrow();
                        c.name == procedure_class_name && !c.is_declaration
                    }).cloned().collect::<Vec<_>>();

                    for class in full_classes.iter() {
                        let mut found_method = None;

                        for (i, member) in class.borrow().members.iter().enumerate() {
                            let cpp::ClassMember::Method(class_method) = member else {
                                continue;
                            };

                            let class_member_function = context.type_finder.find(class_method.type_index)?.parse()?;
                            
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

                            if class_method_name != procedure_name {
                                continue;
                            }

                            let mut valid = (class_member_function.argument_list == member_function.argument_list)
                                && (class_member_function.return_type == member_function.return_type);

                            if !valid {
                                let lhs_return_type = cpp::type_name(
                                    &mut self.class_table,
                                    &mut self.type_sizes,
                                    context.machine_type,
                                    &context.type_info,
                                    &context.type_finder,
                                    class_member_function.return_type,
                                    None,
                                    None,
                                    None,
                                    None,
                                    false,
                                )?;

                                let lhs_arg_list = cpp::argument_list(
                                    &mut self.class_table,
                                    &mut self.type_sizes,
                                    context.machine_type,
                                    &context.type_info,
                                    &context.type_finder,
                                    None,
                                    class_member_function.argument_list,
                                    None,
                                )?;

                                let rhs_return_type = cpp::type_name(
                                    &mut self.class_table,
                                    &mut self.type_sizes,
                                    context.machine_type,
                                    &context.type_info,
                                    &context.type_finder,
                                    member_function.return_type,
                                    None,
                                    None,
                                    None,
                                    None,
                                    false,
                                )?;

                                let rhs_arg_list = cpp::argument_list(
                                    &mut self.class_table,
                                    &mut self.type_sizes,
                                    context.machine_type,
                                    &context.type_info,
                                    &context.type_finder,
                                    None,
                                    member_function.argument_list,
                                    None,
                                )?;

                                valid = (lhs_return_type == rhs_return_type) && (lhs_arg_list == rhs_arg_list);
                            }

                            if valid {
                                found_method = Some(i);
                                break;
                            }
                        }

                        if let Some(class_method_index) = found_method {
                            //
                            // Set up member method data
                            //

                            let class_type = class.borrow().name.clone();

                            let declaring_class = if let Some(found_base_class) = cpp::find_class_declaring_intro_method(
                                &mut self.class_table,
                                &mut self.type_sizes,
                                context.machine_type,
                                &context.type_info,
                                &context.type_finder,
                                class.clone(),
                                &procedure_name,
                            )? {
                                found_base_class.borrow().name.clone()
                            } else {
                                class_type.clone()
                            };
                            
                            let this_pointer_type = member_function.this_pointer_type.as_ref().map(|t| {
                                cpp::type_name(
                                    &mut self.class_table,
                                    &mut self.type_sizes,
                                    context.machine_type,
                                    &context.type_info,
                                    &context.type_finder,
                                    *t,
                                    None,
                                    None,
                                    None,
                                    None,
                                    false,
                                ).unwrap()
                            });

                            let this_adjustment = member_function.this_adjustment;

                            member_method_data = Some(cpp::MemberMethodData {
                                class_type,
                                declaring_class,
                                this_pointer_type,
                                this_adjustment,
                            });

                            //
                            // Propagate method attributes into their declaration
                            //

                            let mut borrowed_class = class.borrow_mut();

                            let cpp::ClassMember::Method(class_method) = &mut borrowed_class.members[class_method_index] else {
                                unreachable!()
                            };

                            let mut parameters = parameters.clone();
                            
                            if !parameters.is_empty() && parameters[0] == "this" {
                                parameters.remove(0);
                            }

                            class_method.is_inline = is_inline;
                            class_method.declspecs = declspecs.clone();
                            class_method.signature = cpp::type_name(
                                &mut self.class_table,
                                &mut self.type_sizes,
                                context.machine_type,
                                &context.type_info,
                                &context.type_finder,
                                class_method.type_index,
                                class_method.modifier.as_ref(),
                                Some(class_method.name.clone()),
                                Some(parameters.clone()),
                                None,
                                false
                            )?;
                        }
                    }
                }

                let mut file_name_ref = None;
                let mut line_info = None;

                for (name_ref, offsets) in line_offsets.iter() {
                    if let Some(line) = offsets.get(&procedure_symbol.offset) {
                        file_name_ref = Some(*name_ref);
                        line_info = Some(line.clone());
                        break;
                    }
                }

                let module = if let Some(file_name_ref) = file_name_ref {
                    let path = PathBuf::from(crate::utils::sanitize_path(file_name_ref.to_string_lossy(context.string_table.unwrap())?.to_string()));
                    let module_key = path.to_string_lossy().to_lowercase().to_string();
                    self.modules.entry(module_key).or_insert_with(|| Rc::new(RefCell::new(cpp::Module::default().with_path(path)))).clone()
                } else {
                    module
                };

                let rva = match procedure_symbol.offset.to_rva(context.address_map) {
                    Some(rva) => rva,
                    None => {
                        // println!("WARNING: no RVA found for procedure symbol: {procedure_symbol:?}");
                        return Ok(());
                    }
                };

                let address = self.options.base_address.unwrap_or(0) + rva.0 as u64;

                if let Some(pseudocode_value) = self.options.pseudocode_json.as_ref() {
                    let serde_json::Value::Object(pseudocode_map) = pseudocode_value else {
                        panic!("Invalid pseudocode map");
                    };

                    if let Some(pseudocode_entry) = pseudocode_map.get(&format!("0x{address:X}")) {
                        let serde_json::Value::String(pseudocode_string) = pseudocode_entry else {
                            panic!("Invalid pseudocode entry");
                        };

                        if body.is_none() {
                            body = Some(Default::default());
                        }

                        let mut lines = pseudocode_string.lines();

                        // Skip opening lines until we encounter the functions opening brace
                        while let Some(line) = lines.next() {
                            if line == "{" {
                                break;
                            }
                        }

                        let mut line_count = 0;
                        let mut pseudocode = String::new();

                        while let Some(line) = lines.next() {
                            // Don't collect the function's closing brace or anything after it
                            if line == "}" {
                                break;
                            }

                            // Skip ineffectual toplevel statements (empty functions)
                            if line == "  ;" {
                                continue;
                            }

                            if line_count > 0 {
                                pseudocode.push_str("\n");
                            }

                            // IDA uses 2 spaces, so prepend 2 more to match ours
                            pseudocode.push_str("  ");
                            
                            pseudocode.push_str(line);
                            line_count += 1;
                        }

                        let body = body.as_mut().unwrap();
                        
                        if line_count > 0 {
                            // body.statements.push(cpp::Statement::Comment(format!("line count: {line_count}")));
                        
                            if line_count <= 15 {
                                body.statements.push(cpp::Statement::EmptyLine);
                        
                                if line_count <= 5 {
                                    body.statements.push(cpp::Statement::Comment(format!("small_function")));
                                }
                                if !pseudocode.is_empty() {
                                    body.statements.push(cpp::Statement::Commented(Box::new(
                                        cpp::Statement::String(format!("pseudocode:\n{pseudocode}")),
                                    )));
                                }
                            }
                        }
                    }
                }

                if module.borrow().members.iter().any(|member| match member {
                    cpp::ModuleMember::Procedure(cpp::Procedure { address: a, .. }) if *a == address => true,
                    _ => false,
                }) {
                    return Ok(());
                }

                let procedure_signature = cpp::type_name(
                    &mut self.class_table,
                    &mut self.type_sizes,
                    context.machine_type,
                    &context.type_info,
                    &context.type_finder,
                    procedure_symbol.type_index,
                    None,
                    Some(procedure_symbol.name.to_string().to_string()),
                    Some(parameters.clone()),
                    None,
                    false
                )?;

                if procedure_signature.starts_with("...") || procedure_signature.contains('$') || procedure_signature.contains('`') {
                    return Ok(());
                }

                let ida_procedure_signature = cpp::type_name(
                    &mut self.class_table,
                    &mut self.type_sizes,
                    context.machine_type,
                    &context.type_info,
                    &context.type_finder,
                    procedure_symbol.type_index,
                    None,
                    Some(procedure_symbol.name.to_string().to_string()),
                    Some(parameters.clone()),
                    member_method_data.as_ref().map(|m| m.declaring_class.clone()).or(Some(String::new())),
                    true
                )?;
                
                let type_item = context.type_finder.find(procedure_symbol.type_index)?;
                let type_data = type_item.parse()?;
                
                let (this_pointer_type_index, argument_list_type_index) = match type_data {
                    pdb2::TypeData::Procedure(data) => {
                        (None, data.argument_list)
                    }
                    pdb2::TypeData::MemberFunction(data) => {
                        (data.this_pointer_type, data.argument_list)
                    }
                    data => todo!("{data:#?}")
                };

                let arguments = cpp::argument_type_list(
                    &context.type_finder,
                    this_pointer_type_index,
                    argument_list_type_index, 
                    Some(parameters),
                ).unwrap();

                let procedure = cpp::Procedure {
                    address,
                    line: line_info.map(|x| x.line_start),
                    type_index: procedure_symbol.type_index,
                    is_static,
                    is_inline,
                    member_method_data,
                    declspecs,
                    name: procedure_symbol.name.to_string().to_string(),
                    signature: procedure_signature,
                    ida_signature: Some(ida_procedure_signature),
                    body,
                    return_type:match context.type_finder.find(procedure_symbol.type_index)?.parse()? {
                        pdb2::TypeData::Procedure(data) => {
                            data.return_type
                        }
                        pdb2::TypeData::MemberFunction(data) => {
                            Some(data.return_type)
                        }
                        data => todo!("{data:#?}")
                    },
                    arguments
                };

                if self.options.export_pseudocode_to_files {
                    writeln!(context.script_file, "decompile_to_file(0x{address:X}, \"{}\")", module.borrow().path.to_string_lossy())?;
                }

                if self.options.export_pseudocode_to_json {
                    writeln!(context.script_file, "decompile_to_json(0x{address:X})")?;
                }

                if self.options.export_function_types {
                    if let Some(ida_signature) = procedure.ida_signature.clone()
                    {
                        writeln!(context.script_file, "set_function_type(0x{:X}, \"{}\")", address, ida_signature)?;
                    }
                }

                module.borrow_mut().members.push(cpp::ModuleMember::Procedure(procedure));
            }

            pdb2::SymbolData::Thunk(_) => {
                let module_symbols = match module_symbols {
                    Some(x) => x,
                    None => {
                        // println!("WARNING: unhandled global symbol: {symbol_data:?}");
                        return Ok(());
                    }
                };
                
                self.parse_thunk_symbols(module_symbols)
            }

            pdb2::SymbolData::SeparatedCode(_) => {
                let module_symbols = match module_symbols {
                    Some(x) => x,
                    None => {
                        // println!("WARNING: unhandled global symbol: {symbol_data:?}");
                        return Ok(());
                    }
                };
                
                self.parse_separated_code_symbols(module_symbols)
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

                let module = if let Some(file_name_ref) = file_name_ref {
                    let path = PathBuf::from(crate::utils::sanitize_path(file_name_ref.to_string_lossy(context.string_table.unwrap())?.to_string()));
                    let module_key = path.to_string_lossy().to_lowercase().to_string();
                    self.modules.entry(module_key).or_insert_with(|| Rc::new(RefCell::new(cpp::Module::default().with_path(path)))).clone()
                } else {
                    module
                };

                let rva = match public_symbol.offset.to_rva(context.address_map) {
                    Some(rva) => rva,
                    None => {
                        // println!("WARNING: no RVA found for public symbol: {public_symbol:?}");
                        return Ok(());
                    }
                };

                let address = self.options.base_address.unwrap_or(0) + rva.0 as u64;
                let entry = (public_symbol.name.to_string().to_string(), address);

                if !module.borrow().mangled_symbols.contains(&entry) {
                    module.borrow_mut().mangled_symbols.push(entry);
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
    fn parse_procedure_symbols<'a, 's>(
        &mut self,
        context: &mut DecompileContext<'a, 's>,
        symbols: &mut pdb2::SymbolIter,
        procedure_symbol: &pdb2::ProcedureSymbol,
    ) -> pdb2::Result<(Vec<String>, Vec<String>, Option<pdb2::FrameProcedureSymbol>, Option<cpp::Block>)> {
        let register_variable_names = Rc::new(RefCell::new(vec![]));
        let register_relative_names = Rc::new(RefCell::new(vec![]));
        let frame_procedures = Rc::new(RefCell::new(vec![]));
        
        let mut block = cpp::Block {
            address: None,
            statements: self.parse_statement_symbols(context, symbols, |symbol_data| {
                match symbol_data {
                    pdb2::SymbolData::RegisterVariable(x) => {
                        register_variable_names.borrow_mut().push(x.name.to_string().to_string());
                    }
        
                    pdb2::SymbolData::RegisterRelative(x) => {
                        register_relative_names.borrow_mut().push(x.name.to_string().to_string());
                    }

                    pdb2::SymbolData::FrameProcedure(x) => {
                        frame_procedures.borrow_mut().push(x.clone());
                    }
        
                    _ => {}
                }
        
                Ok(())
            })?,
        };

        let frame_procedures = frame_procedures.borrow().clone();

        if !frame_procedures.is_empty() {
            assert!(frame_procedures.len() == 1);
        }

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

        // Remove arguments from scope
        let argument_count = match context.type_finder.find(procedure_symbol.type_index)?.parse()? {
            pdb2::TypeData::Procedure(procedure_type) => match context.type_finder.find(procedure_type.argument_list)?.parse()? {
                pdb2::TypeData::ArgumentList(data) => data.arguments.len(),

                data => todo!("{data:?}"),
            }

            pdb2::TypeData::MemberFunction(member_function_type) => match context.type_finder.find(member_function_type.argument_list)?.parse()? {
                pdb2::TypeData::ArgumentList(data) => data.arguments.len() + if member_function_type.this_pointer_type.is_some() { 1 } else { 0 },

                data => todo!("{data:?}"),
            }

            pdb2::TypeData::Primitive(_) => 0,

            data => todo!("{data:?}"),
        };

        if block.statements.len() >= argument_count {
            (0..argument_count).for_each(|_| {
                block.statements.remove(0);
            });
        }
        
        Ok((
            register_variable_names,
            register_relative_names,
            frame_procedures.last().cloned(),
            if !self.options.unroll_functions {
                None
            } else {
                Some(block)
            }
        ))
    }

    #[inline(always)]
    fn parse_block_symbols<'a, 's>(
        &mut self,
        context: &mut DecompileContext<'a, 's>,
        symbols: &mut pdb2::SymbolIter,
        address: Option<u64>,
    ) -> pdb2::Result<cpp::Block> {
        Ok(cpp::Block {
            address: if self.options.verbose_blocks { address } else { None },
            statements: self.parse_statement_symbols(context, symbols, |_| Ok(()))?,
        })
    }

    #[inline(always)]
    fn parse_inline_site_symbols<'a, 's>(
        &mut self,
        context: &mut DecompileContext<'a, 's>,
        symbols: &mut pdb2::SymbolIter,
        inline_site: &pdb2::InlineSiteSymbol,
    ) -> pdb2::Result<Vec<cpp::Statement>> {
        let id_item = context.id_finder.find(inline_site.inlinee)?;
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

        let mut statements = self.parse_statement_symbols(context, symbols, |_| Ok(()))?;

        statements.insert(0, cpp::Statement::Comment(format!("inline site start: {name}")));
        statements.push(cpp::Statement::Comment(format!("inline site end: {name}")));

        Ok(statements)
    }

    fn parse_statement_symbols<'a, 's, F: Clone + FnMut(&pdb2::SymbolData) -> pdb2::Result<()>>(
        &mut self,
        context: &mut DecompileContext<'a, 's>,
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
                Err(e) => {
                    println!("WARNING: failed to parse symbol data, skipping 3: {e}");
                    continue;
                }
            };

            f.clone()(&symbol_data)?;

            match symbol_data {
                pdb2::SymbolData::ScopeEnd | pdb2::SymbolData::InlineSiteEnd => break,

                pdb2::SymbolData::RegisterVariable(register_variable_symbol) => {
                    statements.push(cpp::Statement::Variable(cpp::Variable {
                        signature: cpp::type_name(
                            &mut self.class_table,
                            &mut self.type_sizes,
                            context.machine_type,
                            &context.type_info,
                            &context.type_finder,
                            register_variable_symbol.type_index,
                            None,
                            Some(register_variable_symbol.name.to_string().to_string()),
                            None,
                            None,
                            false
                        )?,
                        value: None,
                        comment: Some(format!("r{}", register_variable_symbol.register.0)),
                    }));
                }

                pdb2::SymbolData::RegisterRelative(register_relative_symbol) => {
                    statements.push(cpp::Statement::Variable(cpp::Variable {
                        signature: cpp::type_name(
                            &mut self.class_table,
                            &mut self.type_sizes,
                            context.machine_type,
                            &context.type_info,
                            &context.type_finder,
                            register_relative_symbol.type_index,
                            None,
                            Some(register_relative_symbol.name.to_string().to_string()),
                            None,
                            None,
                            false
                        )?,
                        value: None,
                        // comment: Some(format!("r{} offset {}", register_relative_symbol.register.0, register_relative_symbol.offset))
                        comment: None,
                    }));
                }

                pdb2::SymbolData::Block(block_symbol) => {
                    statements.push(cpp::Statement::Block(self.parse_block_symbols(
                        context,
                        symbols,
                        block_symbol.offset.to_rva(context.address_map).map(|rva| self.options.base_address.unwrap_or(0) + rva.0 as u64),
                    )?));
                }

                pdb2::SymbolData::InlineSite(inline_site_symbol) => {
                    statements.extend(self.parse_inline_site_symbols(
                        context,
                        symbols,
                        &inline_site_symbol,
                    )?);
                }

                pdb2::SymbolData::SeparatedCode(separated_code_symbol) => {
                    let address = separated_code_symbol.offset
                        .to_rva(context.address_map)
                        .map(|rva| self.options.base_address.unwrap_or(0) + rva.0 as u64);

                    if let Some(address) = address {
                        statements.push(cpp::Statement::Comment(format!(
                            "separated code start @ 0x{address:X}",
                        )));
                    }

                    self.parse_separated_code_symbols(symbols);
                }

                pdb2::SymbolData::Constant(constant_symbol) => {
                    statements.push(cpp::Statement::Variable(cpp::Variable {
                        signature: format!("constexpr {}", cpp::type_name(
                            &mut self.class_table,
                            &mut self.type_sizes,
                            context.machine_type,
                            &context.type_info,
                            &context.type_finder,
                            constant_symbol.type_index,
                            None,
                            Some(constant_symbol.name.to_string().to_string()),
                            None,
                            None,
                            false
                        )?),
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
                        )),
                    }));
                }

                pdb2::SymbolData::Data(data_symbol) => {
                    statements.push(cpp::Statement::Variable(cpp::Variable {
                        signature: format!("static {}", cpp::type_name(
                            &mut self.class_table,
                            &mut self.type_sizes,
                            context.machine_type,
                            &context.type_info,
                            &context.type_finder,
                            data_symbol.type_index,
                            None,
                            Some(data_symbol.name.to_string().to_string()),
                            None,
                            None,
                            false
                        )?),
                        value: None,
                        comment: data_symbol.offset.to_rva(context.address_map).map(|rva| {
                            format!("0x{:X}", self.options.base_address.unwrap_or(0) + rva.0 as u64)
                        }),
                    }));
                }

                pdb2::SymbolData::Local(local_symbol) => {
                    statements.push(cpp::Statement::Variable(cpp::Variable {
                        signature: cpp::type_name(
                            &mut self.class_table,
                            &mut self.type_sizes,
                            context.machine_type,
                            &context.type_info,
                            &context.type_finder,
                            local_symbol.type_index,
                            None,
                            Some(local_symbol.name.to_string().to_string()),
                            None,
                            None,
                            false
                        )?,
                        value: None,
                        comment: Some("local".into()),
                    }));
                }

                pdb2::SymbolData::Label(label_symbol) => {
                    match label_symbol.offset.to_rva(context.address_map) {
                        Some(rva) => {
                            let address = self.options.base_address.unwrap_or(0) + rva.0 as u64;
                            
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
                                &mut self.class_table,
                                &mut self.type_sizes,
                                context.machine_type,
                                &context.type_info,
                                &context.type_finder,
                                tls_symbol.type_index,
                                None,
                                Some(tls_symbol.name.to_string().to_string()),
                                None,
                                None,
                                false
                            )?,
                        ),
                        value: None,
                        comment: tls_symbol.offset.to_rva(context.address_map).map(|rva| {
                            format!("0x{:X}", self.options.base_address.unwrap_or(0) + rva.0 as u64)
                        }),
                    }));
                }

                pdb2::SymbolData::CallSiteInfo(call_site_info) => {
                    let _address = call_site_info.offset
                        .to_rva(context.address_map)
                        .map(|rva| self.options.base_address.unwrap_or(0) + rva.0 as u64)
                        .unwrap();

                    let type_name = cpp::type_name(
                        &mut self.class_table,
                        &mut self.type_sizes,
                        context.machine_type,
                        &context.type_info,
                        &context.type_finder,
                        call_site_info.type_index,
                        None,
                        None,
                        None,
                        None,
                        false
                    )?;

                    //statements.push(cpp::Statement::Comment(format!("function call @ 0x{:X}: {}", address, type_name)));
                    statements.push(cpp::Statement::Comment(format!("function call : {}", type_name)));
                }

                pdb2::SymbolData::Callees(function_list) => {
                    let mut function_types = vec![];

                    for function_type_index in function_list.functions.iter() {
                        let function_type_data = context.type_finder.find(*function_type_index)?.parse()?;
                        function_types.push(function_type_data);
                    }

                    statements.push(cpp::Statement::Comment(format!(
                        "function callee list:\n{}",
                        function_types.iter().map(|t| format!("{t:#?}")).collect::<Vec<_>>().join("\n"),
                    )));
                }

                pdb2::SymbolData::Callers(function_list) => {
                    let mut function_types = vec![];

                    for function_type_index in function_list.functions.iter() {
                        let function_type_data = context.type_finder.find(*function_type_index)?.parse()?;
                        function_types.push(function_type_data);
                    }

                    statements.push(cpp::Statement::Comment(format!(
                        "function caller list:\n{}",
                        function_types.iter().map(|t| format!("{t:#?}")).collect::<Vec<_>>().join("\n"),
                    )));
                }

                pdb2::SymbolData::UserDefinedType(_)
                | pdb2::SymbolData::DefRange(_)
                | pdb2::SymbolData::DefRangeSubField(_)
                | pdb2::SymbolData::DefRangeRegister(_)
                | pdb2::SymbolData::DefRangeFramePointerRelative(_)
                | pdb2::SymbolData::DefRangeFramePointerRelativeFullScope(_)
                | pdb2::SymbolData::DefRangeSubFieldRegister(_)
                | pdb2::SymbolData::DefRangeRegisterRelative(_)
                | pdb2::SymbolData::FrameProcedure(_)
                | pdb2::SymbolData::FrameCookie(_)
                | pdb2::SymbolData::FileStatic(_)
                | pdb2::SymbolData::Inlinees(_)
                | pdb2::SymbolData::HeapAllocationSite(_) => {
                    // println!("WARNING: Unused symbol data in parse_statement_symbols: {symbol_data:#?}");
                }
                
                _ => panic!("Unhandled symbol data in parse_statement_symbols - {symbol_data:#?}"),
            }
        }

        Ok(statements)
    }

    #[inline(always)]
    fn parse_thunk_symbols(&mut self, symbols: &mut pdb2::SymbolIter) {
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
                Err(e) => {
                    println!("WARNING: failed to parse symbol data, skipping 4: {e}");
                    continue;
                }
            };

            match symbol_data {
                pdb2::SymbolData::ScopeEnd => {
                    break;
                }

                pdb2::SymbolData::UserDefinedType(_)
                | pdb2::SymbolData::DefRange(_)
                | pdb2::SymbolData::DefRangeSubField(_)
                | pdb2::SymbolData::DefRangeRegister(_)
                | pdb2::SymbolData::DefRangeFramePointerRelative(_)
                | pdb2::SymbolData::DefRangeFramePointerRelativeFullScope(_)
                | pdb2::SymbolData::DefRangeSubFieldRegister(_)
                | pdb2::SymbolData::DefRangeRegisterRelative(_)
                | pdb2::SymbolData::FrameProcedure(_)
                | pdb2::SymbolData::CallSiteInfo(_)
                | pdb2::SymbolData::FrameCookie(_)
                | pdb2::SymbolData::Callees(_)
                | pdb2::SymbolData::Callers(_)
                | pdb2::SymbolData::FileStatic(_)
                | pdb2::SymbolData::Inlinees(_)
                | pdb2::SymbolData::HeapAllocationSite(_) => {
                    // println!("WARNING: Unused symbol data in parse_thunk_symbols: {symbol_data:#?}");
                }

                _ => panic!("Unhandled symbol data in parse_thunk_symbols: {symbol_data:#?}"),
            }
        }
    }

    #[inline(always)]
    fn parse_separated_code_symbols(&mut self, symbols: &mut pdb2::SymbolIter) {
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
                Err(e) => {
                    println!("WARNING: failed to parse symbol data, skipping 5: {e}");
                    continue;
                }
            };

            match symbol_data {
                pdb2::SymbolData::ScopeEnd => {
                    break;
                }

                pdb2::SymbolData::UserDefinedType(_)
                | pdb2::SymbolData::DefRange(_)
                | pdb2::SymbolData::DefRangeSubField(_)
                | pdb2::SymbolData::DefRangeRegister(_)
                | pdb2::SymbolData::DefRangeFramePointerRelative(_)
                | pdb2::SymbolData::DefRangeFramePointerRelativeFullScope(_)
                | pdb2::SymbolData::DefRangeSubFieldRegister(_)
                | pdb2::SymbolData::DefRangeRegisterRelative(_)
                | pdb2::SymbolData::FrameProcedure(_)
                | pdb2::SymbolData::CallSiteInfo(_)
                | pdb2::SymbolData::FrameCookie(_)
                | pdb2::SymbolData::Callees(_)
                | pdb2::SymbolData::Callers(_)
                | pdb2::SymbolData::FileStatic(_)
                | pdb2::SymbolData::Inlinees(_)
                | pdb2::SymbolData::HeapAllocationSite(_) => {
                    // println!("WARNING: Unused symbol data in parse_separated_code_symbols: {symbol_data:#?}");
                }

                _ => panic!("Unhandled symbol data in parse_separated_code_symbols - {symbol_data:#?}"),
            }
        }
    }

    fn fix_nested_types(
        module_members: &[cpp::ModuleMember],
        module_member_index: usize,
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
                        let found = module_members.iter().enumerate().find(|&(i, m)| {
                            if module_member_index == i {
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

                    Self::fix_nested_types(module_members, module_member_index, nested_class.clone(), remove_class_names, remove_enum_names);
                    
                    if !remove_class_names.contains(&full_name) {
                        remove_class_names.push(full_name);
                    }
                }

                cpp::ClassMember::Enum(nested_enum) => {
                    let full_name = format!("{}::{}", class_name, nested_enum.name);
                    
                    if nested_enum.is_declaration {
                        let found = module_members.iter().enumerate().find(|&(i, m)| {
                            if module_member_index == i {
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

                            let found = module_members.iter().enumerate().find(|&(i, m)| {
                                if module_member_index == i {
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

                                Self::fix_nested_types(module_members, module_member_index, other_class.clone(), remove_class_names, remove_enum_names);

                                let mut other_class = other_class.borrow().clone();
                                other_class.name = String::new();
                                other_class.depth = class_data.borrow().depth + 1;
                                
                                field.type_name = other_class
                                    .to_string()
                                    .trim_start()
                                    .trim_end_matches(";")
                                    .to_string();
                            
                                field.signature = format!("{} {}", field.type_name, field.name);

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
}
