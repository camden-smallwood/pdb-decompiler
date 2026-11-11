use crate::{cpp, options::Options};
use pdb2::FallibleIterator;
use std::{
    cell::RefCell,
    collections::{BTreeMap, HashMap, HashSet},
    error::Error,
    fs::{File, OpenOptions},
    io::Write,
    path::{Component, Path, PathBuf},
    rc::Rc,
};

pub struct Decompiler {
    pub options: Options,
    pub class_table: Vec<Rc<RefCell<cpp::Class>>>,
    pub type_sizes: HashMap<String, u64>,
    pub type_names: cpp::TypeNames,
    /// Fully-qualified names of every known class/union/enum, used to tell
    /// namespace qualifiers apart from class scopes during namespace reconstruction.
    pub type_name_set: HashSet<String>,
    /// Mangled (decorated) public symbol name keyed by resolved address, used to
    /// recover top-level pointer `const` on parameters that the type record drops.
    pub mangled_by_address: HashMap<u64, String>,
    /// For each type name that has multiple full (non-forward-reference) records left
    /// by incremental refactoring, the single canonical `TypeIndex` IDA/DIA resolve to:
    /// the TPI hash-adjuster entry if present, else the highest-index (latest) record.
    /// Names with a single definition are absent.
    pub canonical_type_index: HashMap<String, pdb2::TypeIndex>,
    pub modules: HashMap<String, Rc<RefCell<cpp::Module>>>,
    pub function_scopes_modules: Option<HashMap<String, Rc<RefCell<cpp::Module>>>>,
    pub script_file: Option<File>,
}

pub struct DecompileContext<'a, 's> {
    pub pdb: &'a mut pdb2::PDB<'s, File>,
    pub debug_info: &'a pdb2::DebugInformation<'s>,
    pub machine_type: pdb2::MachineType,
    pub address_map: &'a pdb2::AddressMap<'s>,
    pub string_table: Option<&'a pdb2::StringTable<'s>>,
    pub id_info: Option<&'a pdb2::IdInformation<'s>>,
    pub id_finder: Option<pdb2::IdFinder<'a>>,
    pub source_files: HashMap<u16, Vec<String>>,
    pub type_info: &'a pdb2::TypeInformation<'s>,
    pub type_finder: pdb2::TypeFinder<'a>,
    pub global_symbols: &'a pdb2::SymbolTable<'s>,
}

impl Decompiler {
    #[inline(always)]
    pub fn new(options: Options) -> Self {
        Self {
            options,
            class_table: vec![],
            type_sizes: HashMap::new(),
            type_names: cpp::TypeNames::default(),
            type_name_set: HashSet::new(),
            mangled_by_address: HashMap::new(),
            canonical_type_index: HashMap::new(),
            modules: HashMap::new(),
            function_scopes_modules: None,
            script_file: None,
        }
    }

    #[inline(always)]
    pub fn with_function_scopes_modules(mut self, function_scopes_modules: Option<HashMap<String, Rc<RefCell<cpp::Module>>>>) -> Self {
        self.function_scopes_modules = function_scopes_modules;
        self
    }

    /// Computes the output path for a module, rebased relative to a matching
    /// `--source-path` filter. This is used **only** when writing output `.cpp`
    /// files; the module's own `path` (used for internal references) is left
    /// untouched.
    ///
    /// When no `--source-path` filters are configured, the module path is
    /// returned unchanged. Otherwise, the module is kept only if its (sanitized)
    /// path is equal to, or nested under, one of the given paths (case-insensitive,
    /// tolerating trailing slashes). The returned path is the last folder of the
    /// matched source path followed by the module's components after that prefix,
    /// so `.../FolderA/SubFolderA/foo.cpp` becomes `SubFolderA/foo.cpp`. Returns
    /// `None` when the module does not match any filter.
    #[inline(always)]
    fn rebase_output_path(source_paths: &[PathBuf], module_path: &Path) -> Option<PathBuf> {
        if source_paths.is_empty() {
            return Some(module_path.to_path_buf());
        }

        let sanitized = crate::utils::sanitize_path(module_path.to_string_lossy());
        let components = sanitized
            .split(['/', '\\'])
            .filter(|component| !component.is_empty())
            .map(str::to_string)
            .collect::<Vec<_>>();
        let lowercased = components
            .iter()
            .map(|component| component.to_lowercase())
            .collect::<Vec<_>>();

        for source_path in source_paths {
            let sp_sanitized = crate::utils::sanitize_path(source_path.to_string_lossy());
            let sp_components = sp_sanitized
                .split(['/', '\\'])
                .filter(|component| !component.is_empty())
                .map(str::to_string)
                .collect::<Vec<_>>();
            let Some(last_component) = sp_components.last() else {
                continue;
            };
            let sp_lowercased = sp_components
                .iter()
                .map(|component| component.to_lowercase())
                .collect::<Vec<_>>();

            if lowercased.len() < sp_lowercased.len() {
                continue;
            }

            if lowercased[..sp_lowercased.len()] != sp_lowercased[..] {
                continue;
            }

            let rebased = std::iter::once(last_component.as_str())
                .chain(components[sp_lowercased.len()..].iter().map(String::as_str))
                .collect::<Vec<_>>()
                .join("/");

            return Some(PathBuf::from(rebased));
        }

        None
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

        let info_stream = pdb.raw_stream(pdb2::StreamIndex(1))?
            .ok_or("PDB information stream missing")?;
        let id_info = if crate::legacy::has_ipi(info_stream.as_slice())? {
            Some(pdb.id_information()?)
        } else {
            None
        };
        let dbi_stream = pdb.raw_stream(pdb2::StreamIndex(3))?
            .ok_or("DBI stream missing")?;
        let files = crate::legacy::source_files(dbi_stream.as_slice())?;
        let source_files = debug_info.modules()?.collect::<Vec<_>>()?.iter()
            .zip(files).map(|(module, files)| (module.info().stream.0, files))
            .collect();

        let global_symbols = pdb.global_symbols()
            .map_err(|e| format!("does not contain global symbol information: {e}"))?;
        
        let type_finder = type_info.finder();
        let id_finder = id_info.as_ref().map(|info| info.finder());

        // Map each public symbol's resolved address to its mangled name, so parameter
        // `const` lost from the type records can be recovered from the mangled name.
        {
            let mut symbols = global_symbols.iter();
            while let Some(symbol) = symbols.next()? {
                if let Ok(pdb2::SymbolData::Public(public_symbol)) = symbol.parse()
                    && let Some(rva) = public_symbol.offset.to_rva(&address_map)
                {
                    let address = self.options.base_address.unwrap_or(0) + rva.0 as u64;
                    self.mangled_by_address
                        .entry(address)
                        .or_insert_with(|| public_symbol.name.to_string().to_string());
                }
            }
        }

        // Resolve which record is canonical for each type name that has duplicates. MSVC's
        // linker never rewrites a type record: when a type is redefined across (incremental)
        // builds it appends a new record at a higher index and leaves the stale ones behind.
        // The definition IDA/DIA select is the TPI hash-adjuster entry if one exists, otherwise
        // the highest-index (latest) non-forward-reference record. We keep only that one when
        // emitting, dropping the stale duplicates.
        {
            let adjusters = pdb.type_adjusters().unwrap_or_default();

            let mut groups: HashMap<String, Vec<pdb2::TypeIndex>> = HashMap::new();
            // Anonymous type qualified name -> a full-definition index (with a field list). Lets
            // `type_name` resolve a forward-reference reference to an anonymous type back to its
            // inlinable definition (references reach the fieldless forward-ref record).
            let mut anonymous_definitions: HashMap<String, pdb2::TypeIndex> = HashMap::new();
            let mut iter = type_info.iter();
            while let Some(item) = iter.next()? {
                if !matches!(
                    item.raw_kind(),
                    pdb2::LF_CLASS
                        | pdb2::LF_CLASS_ST
                        | pdb2::LF_STRUCTURE
                        | pdb2::LF_STRUCTURE_ST
                        | pdb2::LF_INTERFACE
                        | pdb2::LF_UNION
                        | pdb2::LF_UNION_ST
                        | pdb2::LF_ENUM
                        | pdb2::LF_ENUM_ST,
                ) {
                    continue;
                }

                let (name, forward_reference, inlinable_anonymous) = match item.parse() {
                    Ok(pdb2::TypeData::Class(class_type)) => {
                        let name = class_type.name.to_string().to_string();
                        let inlinable = class_type.fields.is_some() && cpp::is_anonymous_type_name(&name);
                        (name, class_type.properties.forward_reference(), inlinable)
                    }
                    Ok(pdb2::TypeData::Union(union_type)) => {
                        let name = union_type.name.to_string().to_string();
                        let inlinable = cpp::is_anonymous_type_name(&name);
                        (name, union_type.properties.forward_reference(), inlinable)
                    }
                    Ok(pdb2::TypeData::Enumeration(enum_type)) => {
                        let name = enum_type.name.to_string().to_string();
                        let inlinable = cpp::is_anonymous_type_name(&name);
                        (name, enum_type.properties.forward_reference(), inlinable)
                    }
                    _ => continue,
                };

                if forward_reference {
                    continue;
                }

                if inlinable_anonymous {
                    anonymous_definitions.entry(name.clone()).or_insert_with(|| item.index());
                }

                groups.entry(name).or_default().push(item.index());
            }

            self.type_names.set_anonymous_definitions(anonymous_definitions);

            for (name, mut indices) in groups {
                if indices.len() < 2 {
                    continue;
                }

                let canonical = adjusters.get(&name).copied().unwrap_or_else(|| {
                    indices.sort_by_key(|index| index.0);
                    *indices.last().unwrap()
                });

                self.canonical_type_index.insert(name, canonical);
            }
        }

        let out_path = self.options.out.as_ref()
            .ok_or_else(|| "Out path not supplied".to_string())?;

        self.script_file = if
            self.options.export_pseudocode_to_files ||
            self.options.export_pseudocode_to_json ||
            self.options.export_function_types
        {
            std::fs::create_dir_all(out_path.clone())?;
            let mut script_file = File::create(out_path.join("ida_script.py"))?;
            writeln!(script_file, "{}", include_str!("../ida_script_base.py"))?;
            Some(script_file)
        } else {
            None
        };

        let mut context = DecompileContext {
            pdb: &mut pdb,
            debug_info: &mut debug_info,
            machine_type,
            address_map: &mut address_map,
            string_table: string_table.as_ref(),
            id_info: id_info.as_ref(),
            source_files,
            id_finder: id_finder,
            type_info: &type_info,
            type_finder: type_finder,
            global_symbols: &global_symbols,
        };
        
        self.process_type_information(&mut context)?;
        self.process_id_information(&mut context)?;
        self.process_modules(&mut context)?;

        if let Some(path) = self.options.export_objects_json.as_ref() {
            crate::objects::export_objects_json(&mut context, self.options.base_address.unwrap_or(0), path)?;
        }

        if let Some(script_file) = self.script_file.as_mut()
            && self.options.export_pseudocode_to_json
        {
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
            writeln!(script_file, "print(\"Done.\")")?;
        }

        Ok(())
    }

    #[inline(always)]
    fn process_type_information<'a, 's>(&mut self, context: &mut DecompileContext<'a, 's>) -> pdb2::Result<()> {
        // println!("Processing type information...");

        let export_all = self.options.export_cpp && context.id_info.is_none_or(|info| info.is_empty());

        let mut type_iter = context.type_info.iter();

        let mut exported_forward_reference_indices = vec![];
        let mut exported_type_indices = vec![];

        while let Some(type_item) = type_iter.next()? {
            context.type_finder.update(&type_iter);
            
            // Don't attempt to parse types we don't care about
            if !matches!(
                type_item.raw_kind(),
                pdb2::LF_CLASS
                    | pdb2::LF_CLASS_ST
                    | pdb2::LF_STRUCTURE
                    | pdb2::LF_STRUCTURE_ST
                    | pdb2::LF_INTERFACE
                    | pdb2::LF_UNION
                    | pdb2::LF_UNION_ST
                    | pdb2::LF_ENUM
                    | pdb2::LF_ENUM_ST,
            ) {
                continue;
            }

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

            // Record every known type's fully-qualified name so namespace
            // reconstruction can distinguish namespace qualifiers from class scopes.
            if let Some(type_name) = match &type_data {
                pdb2::TypeData::Class(class_type) => Some(class_type.name.to_string().to_string()),
                pdb2::TypeData::Union(union_type) => Some(union_type.name.to_string().to_string()),
                pdb2::TypeData::Enumeration(enum_type) => Some(enum_type.name.to_string().to_string()),
                _ => None,
            } {
                self.type_name_set.insert(crate::namespaces::normalize_anon(&type_name));
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

        if !export_all {
            return Ok(());
        }

        let exported_path = PathBuf::from(format!("{}/exported.h", self.options.out.as_ref().unwrap().to_string_lossy()));

        let mut module = cpp::Module::default();
        module.path = exported_path.clone();

        // Types an SDK's headers define are included from the SDK instead of reconstructed
        let sdk = crate::sdk::Sdk::scan(&self.options.sdk_includes).map_err(pdb2::Error::IoError)?;

        let full_names = exported_type_indices.iter().filter_map(|index| {
            context.type_finder.find(*index).ok()?.parse().ok()?.name().map(|n| n.to_string().into_owned())
        }).collect::<HashSet<_>>();
        for type_index in exported_forward_reference_indices {
            if context.type_finder.find(type_index)?.parse()?.name()
                .is_some_and(|name| full_names.contains(name.to_string().as_ref()) || sdk.header(&name.to_string()).is_some()) { continue; }
            if let Err(e) = module.add_type_definition(&mut self.class_table, &mut self.type_sizes, &mut self.type_names, context.machine_type, &context.type_info, &context.type_finder, type_index, 0) {
                panic!("{e}");
            }
        }

        let mut canonical_type_indices = vec![];
        for type_index in exported_type_indices {
            if let Some(name) = context.type_finder.find(type_index)?.parse()?.name()
                && self.canonical_type_index.get(name.to_string().as_ref())
                    .is_some_and(|canonical| *canonical != type_index) { continue; }
            canonical_type_indices.push(type_index);
        }

        // Without IPI no type records its declaration file; infer one where the evidence allows
        let placements = crate::placement::place_types(context, &canonical_type_indices)?;
        let mut placement_entries = vec![];
        let mut placed_paths = HashMap::new();
        let mut sdk_headers = HashMap::new();
        let mut sdk_layouts = vec![];

        for type_index in canonical_type_indices {
            let type_data = context.type_finder.find(type_index)?.parse()?;
            let name = type_data.name().map(|n| n.to_string().into_owned()).unwrap_or_default();

            // A type whose file is filtered out stays in the shared inventory
            let placed = placements.get(&type_index).and_then(|placement| {
                let path = PathBuf::from(crate::utils::sanitize_path(&placement.file));
                Self::rebase_output_path(&self.options.source_paths, &path).map(|_| (path, placement))
            });

            if let Some(header) = sdk.header(&name) {
                let header = PathBuf::from(header.to_string_lossy().replace('\\', "/"));

                // The PDB's layout of a type the SDK defines directly, to check the SDK against
                let size = match &type_data {
                    pdb2::TypeData::Class(class_type) => class_type.size,
                    pdb2::TypeData::Union(union_type) => union_type.size,
                    _ => 0,
                };

                if size != 0 && sdk.defines(&name) {
                    sdk_layouts.push((name.clone(), size));
                }

                placement_entries.push((format!("<{}>", header.display()), 0, name, type_index, "sdk", "defined by an SDK header".into()));
                sdk_headers.insert(type_index, (header, placed.map(|(path, _)| path)));
                continue;
            }

            let Some((path, placement)) = placed else {
                if let Err(e) = module.add_type_definition(&mut self.class_table, &mut self.type_sizes, &mut self.type_names, context.machine_type, context.type_info, &context.type_finder, type_index, 0) {
                    panic!("{e}");
                }
                continue;
            };

            let target = self.modules.entry(path.to_string_lossy().to_lowercase())
                .or_insert_with(|| Rc::new(RefCell::new(cpp::Module::default().with_path(path.clone()))))
                .clone();

            if let Err(e) = target.borrow_mut().add_type_definition(&mut self.class_table, &mut self.type_sizes, &mut self.type_names, context.machine_type, context.type_info, &context.type_finder, type_index, placement.line) {
                panic!("{e}");
            }

            placement_entries.push((placement.file.clone(), placement.line, name, type_index, placement.rule, placement.evidence.clone()));
            placed_paths.insert(type_index, path);
        }

        // A file includes the files defining what its types derive from or hold by value. SDK
        // headers are included as such, by the file a type the SDK defines would have been in.
        let mut includes = BTreeMap::<String, (PathBuf, BTreeMap<String, (PathBuf, bool)>)>::new();
        let mut include = |path: &PathBuf, header: &PathBuf, is_global: bool| {
            let key = path.to_string_lossy().to_lowercase();
            let header_key = header.to_string_lossy().to_lowercase().replace('\\', "/");

            // Not the file itself, nor, for an SDK header, a copy of it in the PDB's source tree
            if header_key == key.replace('\\', "/") || (is_global && key.replace('\\', "/").ends_with(&format!("/{header_key}"))) {
                return;
            }

            includes.entry(key).or_insert_with(|| (path.clone(), BTreeMap::new())).1.entry(header_key).or_insert_with(|| (header.clone(), is_global));
        };

        for (type_index, path) in placed_paths.iter() {
            for dependency in placements[type_index].requires.iter() {
                if let Some(dependency_path) = placed_paths.get(dependency) {
                    include(path, dependency_path, false);
                } else if let Some((header, _)) = sdk_headers.get(dependency) {
                    include(path, header, true);
                }
            }
        }

        let mut exported_sdk_headers = std::collections::BTreeSet::new();

        for (header, placed) in sdk_headers.values() {
            match placed {
                Some(path) => include(path, header, true),
                None => {
                    exported_sdk_headers.insert(header.clone());
                }
            }
        }

        for (key, (path, headers)) in includes {
            // A file only SDK types would have been in has no module yet
            let mut module = self.modules.entry(key)
                .or_insert_with(|| Rc::new(RefCell::new(cpp::Module::default().with_path(path))))
                .borrow_mut();

            for (_, (header_path, is_global)) in headers {
                if !module.headers.iter().any(|(path, _)| path.to_string_lossy().eq_ignore_ascii_case(&header_path.to_string_lossy())) {
                    module.headers.push((header_path, is_global));
                }
            }
        }

        for header in exported_sdk_headers {
            module.headers.push((header, true));
        }

        placement_entries.sort_by(|a, b| (&a.0, a.1, &a.2).cmp(&(&b.0, b.1, &b.2)));

        let placement_index = serde_json::json!({
            "note": "Inferred declaration files for types in a PDB without IPI, which records none. Types the SDK defines have the rule sdk and are included from its header. Types not listed are in exported.h.",
            "types": placement_entries.iter().map(|(file, _, name, type_index, rule, evidence)| serde_json::json!({
                "type": name,
                "type_index": format!("0x{:X}", type_index.0),
                "file": file,
                "rule": rule,
                "evidence": evidence,
            })).collect::<Vec<_>>(),
        });

        let module = Rc::new(RefCell::new(module));
        let mut symbols = context.global_symbols.iter();
        while let Some(symbol) = symbols.next()? {
            if let Ok(data @ pdb2::SymbolData::UserDefinedType(_)) = symbol.parse() {
                if let pdb2::SymbolData::UserDefinedType(udt_symbol) = &data && sdk.defines(&udt_symbol.name.to_string()) {
                    continue;
                }

                self.process_module_symbol_data(context, &HashMap::new(), module.clone(), None, &data)?;
            }
        }
        if let Some(parent_path) = exported_path.parent()
            && let Err(e) = std::fs::create_dir_all(parent_path)
        {
            panic!("{e}");
        }

        let mut file = File::create(&exported_path).map_err(pdb2::Error::IoError)?;

        if let Err(e) = write!(file, "{}", cpp::compact_std_templates(&cpp::shorten_std_aliases(&module.borrow().to_string()))) {
            panic!("{e}");
        }

        // The SDK may not be the version the PDB was built with: check the layouts it records
        if !sdk_layouts.is_empty() {
            sdk_layouts.sort();
            sdk_layouts.dedup();

            writeln!(file, "\n// Layouts this PDB records for types the SDK defines").map_err(pdb2::Error::IoError)?;

            for (name, size) in sdk_layouts {
                writeln!(file, "static_assert(sizeof({name}) == {size}, \"Invalid {name} size\");").map_err(pdb2::Error::IoError)?;
            }
        }

        let index_file = File::create(exported_path.with_file_name("placements.json")).map_err(pdb2::Error::IoError)?;
        serde_json::to_writer_pretty(index_file, &placement_index).map_err(|e| pdb2::Error::IoError(e.into()))?;

        Ok(())
    }

    #[inline(always)]
    fn process_id_information<'a, 's>(&mut self, context: &mut DecompileContext<'a, 's>) -> pdb2::Result<()> {
        // println!("Processing id information...");

        let Some(id_info) = context.id_info else { return Ok(()); };
        let id_finder = context.id_finder.as_mut().unwrap();
        let mut id_iter = id_info.iter();

        while let Some(id) = id_iter.next()? {
            id_finder.update(&id_iter);

            let (pdb2::LF_BUILDINFO | pdb2::LF_UDT_SRC_LINE | pdb2::LF_UDT_MOD_SRC_LINE) = id.raw_kind() else {
                continue;
            };

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
                    module.add_build_info(id_finder, build_info)?;

                    let Some(Component::Normal(_)) = module.path.components().last() else {
                        continue;
                    };

                    // println!("Found build info for module \"{}\"", module.path.display());

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
                        pdb2::UserDefinedTypeSourceFileRef::Local(id) => match id_finder.find(id) {
                            Ok(item) => match item.parse() {
                                Ok(pdb2::IdData::String(source_file)) => crate::utils::sanitize_path(&source_file.name.to_string()),
                                Ok(data) => panic!("invalid UDT source file id: {:#?}", data),
                                Err(error) => panic!("failed to parse UDT source file id: {error}"),
                            },

                            Err(error) => panic!("failed to find UDT source file id: {error}"),
                        },

                        pdb2::UserDefinedTypeSourceFileRef::Remote(_, source_line_ref) => {
                            crate::utils::sanitize_path(&source_line_ref.to_string_lossy(context.string_table.ok_or(pdb2::Error::StreamNameNotFound)?)?)
                        }
                    };

                    // println!("Adding UDT 0x{:X} to module \"{}\"", data.udt.0, module_path);

                    self.modules
                        .entry(module_path.to_lowercase())
                        .or_insert_with(|| Rc::new(RefCell::new(cpp::Module::default().with_path(module_path.into()))))
                        .borrow_mut()
                        .add_type_definition(
                            &mut self.class_table,
                            &mut self.type_sizes,
                            &mut self.type_names,
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

        // println!("Done processing id information");

        Ok(())
    }

    #[inline(always)]
    fn process_modules<'a, 's>(&mut self, context: &mut DecompileContext<'a, 's>) -> Result<(), Box<dyn Error>> {
        // Snapshot the canonical index map so the per-module dedup below can read it while
        // `self.modules` is borrowed mutably.
        let canonical_type_index = self.canonical_type_index.clone();

        let module_global_symbols = self.load_module_global_symbols(context)?;

        let mut module_iter = context.debug_info.modules().map_err(|e| format!("does not contain debug module info: {e}"))?;

        while let Some(module) = module_iter.next().map_err(|e| format!("failed to get next debug module info: {e}"))? {
            if let Err(error) = self.process_module(context, &module, &module_global_symbols) {
                println!("WARNING: failed to process module {}: {error}", module.module_name());
                // println!("WARNING: failed to parse module, skipping: {e} - {module:#?}");
                continue;
            }
        }

        //
        // Check for duplicate modules
        //

        let module_keys = self.modules.keys().cloned().collect::<Vec<_>>();

        for i in 0..module_keys.len() {
            let module_1 = self.modules.get(&module_keys[i]).unwrap().borrow();

            for j in 0..module_keys.len() {
                if i == j {
                    continue;
                }

                let module_2 = self.modules.get(&module_keys[j]).unwrap().borrow();

                if module_1.is_header() || module_2.is_header() {
                    continue;
                }

                if module_1.path.file_name().unwrap().to_ascii_lowercase() == module_2.path.file_name().unwrap().to_ascii_lowercase() {
                    println!("WARNING: Found duplicate module file names:");
                    println!(" -> \"{}\"", module_1.path.display());
                    println!(" -> \"{}\"", module_2.path.display());
                }
            }
        }

        //
        // Create module path tree and resolve proper file paths
        //

        let mut path_tree = crate::utils::PathTree::new();

        for module_key in module_keys.iter() {
            let module = self.modules.get(module_key).unwrap().borrow();

            path_tree.add_path(&module.path, false);
            path_tree.add_path(&module.compiler_path, false);
            path_tree.add_path(&module.pdb_path, false);

            // A global include is already an include path, such as an SDK header's
            for (header_path, _) in module.headers.iter().filter(|(_, is_global)| !is_global) {
                path_tree.add_path(&header_path, false);
            }

            if let Some(compiler_response_file) = module.compiler_response_file.as_ref() {
                path_tree.add_path(&compiler_response_file, false);
            }

            if let Some(pch_file_name) = module.pch_file_name.as_ref() {
                path_tree.add_path(&pch_file_name, false);
            }

            if let Some(precompiled_header_file_name) = module.precompiled_header_file_name.as_ref() {
                path_tree.add_path(&precompiled_header_file_name, false);
            }

            if let Some(fid_file) = module.fid_file.as_ref() {
                path_tree.add_path(&fid_file, false);
            }

            if let Some(experimental_logs_file) = module.experimental_logs_file.as_ref() {
                path_tree.add_path(&experimental_logs_file, false);
            }

            if let Some(object_file) = module.object_file.as_ref() {
                path_tree.add_path(&object_file, false);
            }

            if let Some(program_database_file) = module.program_database_file.as_ref() {
                path_tree.add_path(&program_database_file, false);
            }

            if let Some(doc_file) = module.doc_file.as_ref() {
                path_tree.add_path(&doc_file, false);
            }

            if let Some(listing_file) = module.listing_file.as_ref() {
                path_tree.add_path(&listing_file, false);
            }

            for additional_include_dir in module.additional_include_dirs.iter() {
                path_tree.add_path(&additional_include_dir, true);
            }

            for using_directive_dir in module.using_directive_dirs.iter() {
                path_tree.add_path(&using_directive_dir, true);
            }

            for forced_using_directive in module.forced_using_directives.iter() {
                path_tree.add_path(&forced_using_directive, false);
            }

            for preprocess_include_file in module.preprocess_include_files.iter() {
                path_tree.add_path(&preprocess_include_file, false);
            }

            for trim_file in module.trim_files.iter() {
                path_tree.add_path(&trim_file, false);
            }
        }

        for module_key in module_keys.iter() {
            let mut module = self.modules.get(module_key).unwrap().borrow_mut();

            module.path = path_tree.resolve_path(&module.path, false).unwrap();
            module.compiler_path = path_tree.resolve_path(&module.compiler_path, false).unwrap();
            module.pdb_path = path_tree.resolve_path(&module.pdb_path, false).unwrap();

            for (header_path, _) in module.headers.iter_mut().filter(|(_, is_global)| !*is_global) {
                *header_path = path_tree.resolve_path(&header_path, false).unwrap();
            }

            if let Some(compiler_response_file) = module.compiler_response_file.as_mut() {
                *compiler_response_file = path_tree.resolve_path(&compiler_response_file, false).unwrap();
            }

            if let Some(pch_file_name) = module.pch_file_name.as_mut() {
                *pch_file_name = path_tree.resolve_path(&pch_file_name, false).unwrap();
            }

            if let Some(precompiled_header_file_name) = module.precompiled_header_file_name.as_mut() {
                *precompiled_header_file_name = path_tree.resolve_path(&precompiled_header_file_name, false).unwrap();
            }

            if let Some(fid_file) = module.fid_file.as_mut() {
                *fid_file = path_tree.resolve_path(&fid_file, false).unwrap();
            }

            if let Some(experimental_logs_file) = module.experimental_logs_file.as_mut() {
                *experimental_logs_file = path_tree.resolve_path(&experimental_logs_file, false).unwrap();
            }

            if let Some(object_file) = module.object_file.as_mut() {
                *object_file = path_tree.resolve_path(&object_file, false).unwrap();
            }

            if let Some(program_database_file) = module.program_database_file.as_mut() {
                *program_database_file = path_tree.resolve_path(&program_database_file, false).unwrap();
            }

            if let Some(doc_file) = module.doc_file.as_mut() {
                *doc_file = path_tree.resolve_path(&doc_file, false).unwrap();
            }

            if let Some(listing_file) = module.listing_file.as_mut() {
                *listing_file = path_tree.resolve_path(&listing_file, false).unwrap();
            }

            for additional_include_dir in module.additional_include_dirs.iter_mut() {
                *additional_include_dir = path_tree.resolve_path(&additional_include_dir, true).unwrap();
            }

            for using_directive_dir in module.using_directive_dirs.iter_mut() {
                *using_directive_dir = path_tree.resolve_path(&using_directive_dir, true).unwrap();
            }

            for forced_using_directive in module.forced_using_directives.iter_mut() {
                *forced_using_directive = path_tree.resolve_path(&forced_using_directive, false).unwrap();
            }

            for preprocess_include_file in module.preprocess_include_files.iter_mut() {
                *preprocess_include_file = path_tree.resolve_path(&preprocess_include_file, false).unwrap();
            }

            for trim_file in module.trim_files.iter_mut() {
                *trim_file = path_tree.resolve_path(&trim_file, false).unwrap();
            }
        }

        //
        // Finalize module debug information
        //

        for module_key in module_keys {
            //
            // Clean up include paths and generate modules for them
            //
            let (mut module_headers, module_include_dirs, module_pch_file_name) = {
                let module = self.modules.get(&module_key).unwrap().borrow();
                (module.headers.clone(), module.additional_include_dirs.clone(), module.pch_file_name.clone())
            };

            let mut pch_header_index = None;

            for (i, (header_path, is_global)) in module_headers.iter_mut().enumerate() {
                if *is_global {
                    continue;
                }

                let header_module_key = header_path.to_string_lossy().to_lowercase().to_string();

                // Skip the PCH include
                if let Some(pch_file_name) = module_pch_file_name.as_ref() {
                    let pch_module_key = pch_file_name.to_string_lossy().to_lowercase().to_string();
                    
                    if header_module_key == pch_module_key {
                        pch_header_index = Some(i);
                        continue;
                    }
                }

                // Create a module with the full path if we haven't already
                if !self.modules.contains_key(&header_module_key) {
                    self.modules.insert(header_module_key.clone(), Rc::new(RefCell::new(cpp::Module::default().with_path(header_path.clone()))));
                }

                // Remove additional include dirs from header path
                for dir_path in module_include_dirs.iter() {
                    let dir_path = dir_path.to_string_lossy().to_lowercase().to_string();
                    let dir_path_len = dir_path.len();

                    if dir_path_len < header_module_key.len() && header_module_key.starts_with(dir_path.as_str()) {
                        *header_path = header_path.to_string_lossy()[dir_path_len..].to_string().into();
                        *is_global = true;
                        break;
                    }
                }
            }

            // Remove the implicit PCH include
            if let Some(pch_header_index) = pch_header_index {
                module_headers.remove(pch_header_index);
            }

            self.modules.get(&module_key).unwrap().borrow_mut().headers = module_headers;

            //
            // Sort module members by line number
            //

            let mut module = self.modules.get_mut(&module_key).unwrap().borrow_mut();

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
                        Self::fix_nested_types(module.members.as_slice(), i, class_data.clone(), None, &mut remove_class_names, &mut remove_enum_names);
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
            
            // A nested type defined inside its parent here is redundant at the top level. One whose
            // parent is elsewhere, or that its parent records under another name (a member template
            // such as `?$rebind@D`), keeps its top-level definition, as it is the only one.
            let mut nested_definitions = HashSet::new();

            for member in module.members.iter() {
                if let cpp::ModuleMember::Class(class_data) = member {
                    Self::collect_nested_definitions(&class_data.borrow(), &mut nested_definitions);
                }
            }

            for i in (0..module.members.len()).rev() {
                match &module.members[i] {
                    cpp::ModuleMember::Class(class_data) => {
                        let is_nested_type = {
                            let class_data = class_data.borrow();
                            // Anonymous types are inlined into their parent field, so drop
                            // the redundant top-level definition too.
                            cpp::is_anonymous_type_name(&class_data.name)
                                || (class_data.properties.as_ref().map(|p| p.is_nested_type()).unwrap_or(false)
                                    && nested_definitions.contains(&class_data.index))
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
                    
                    cpp::ModuleMember::Enum(enum_data)
                        if cpp::is_anonymous_type_name(&enum_data.name)
                            || (enum_data.properties.is_nested_type() && nested_definitions.contains(&enum_data.index)) =>
                    {
                        // println!("WARNING: Removing unreferenced nested enum in toplevel: \"{}\" in \"{}\"", enum_data.name, module.path.display());
                        module.members.remove(i);
                    }

                    _ => {}
                }
            }

            //
            // Deduplicate types that share a qualified name. Incremental refactoring leaves
            // several stale records for one type in the PDB; keep only the definition IDA/DIA
            // resolve to, matched through `canonical_type_index` (the TPI hash-adjuster entry,
            // else the latest record). When the canonical record didn't reach this module we
            // fall back to the highest index present, so a type is never dropped entirely.
            // Types in different namespaces have different qualified names, so distinct types
            // like `e_map_id` and `halo_interface::e_map_id` are preserved.
            //

            {
                let member_identity = |member: &cpp::ModuleMember| -> Option<(String, pdb2::TypeIndex)> {
                    match member {
                        cpp::ModuleMember::Class(class_data) => {
                            let class_data = class_data.borrow();
                            Some((class_data.name.clone(), class_data.index))
                        }
                        cpp::ModuleMember::Enum(enum_data) => Some((enum_data.name.clone(), enum_data.index)),
                        _ => None,
                    }
                };

                let mut present: HashMap<String, Vec<pdb2::TypeIndex>> = HashMap::new();
                for member in module.members.iter() {
                    if let Some((name, index)) = member_identity(member) {
                        present.entry(name).or_default().push(index);
                    }
                }

                let mut keep_index: HashMap<String, pdb2::TypeIndex> = HashMap::new();
                for (name, indices) in present.iter() {
                    let chosen = match canonical_type_index.get(name) {
                        Some(canonical) if indices.contains(canonical) => *canonical,
                        _ => *indices.iter().max_by_key(|index| index.0).unwrap(),
                    };
                    keep_index.insert(name.clone(), chosen);
                }

                let mut kept: HashSet<String> = HashSet::new();
                module.members.retain(|member| {
                    let Some((name, index)) = member_identity(member) else {
                        return true;
                    };

                    keep_index.get(&name) == Some(&index) && kept.insert(name)
                });
            }

            //
            // A member function can't be redeclared outside its class. When the class is defined
            // here, fold a definition without a body into its declaration, keeping the address
            //

            if !self.options.reorganize {
                let mut classes = HashMap::new();

                // A function with local types keeps its definition, which they will be embedded in
                let mut has_local_types = HashSet::new();

                for member in module.members.iter() {
                    let name = match member {
                        cpp::ModuleMember::Class(class_data) => {
                            Self::collect_defined_classes(class_data, None, &mut classes);
                            class_data.borrow().name.clone()
                        }
                        cpp::ModuleMember::Enum(enum_data) => enum_data.name.clone(),
                        _ => continue,
                    };

                    if let Some((function, _)) = crate::namespaces::split_local_scope(&name) {
                        has_local_types.insert(function);
                    }
                }

                module.members.retain(|member| {
                    let cpp::ModuleMember::Procedure(cpp::Procedure { body: None, member_method_data: Some(data), address, name, .. }) = member else {
                        return true;
                    };

                    if has_local_types.contains(name) {
                        return true;
                    }

                    let Some(class_data) = classes.get(&data.class_type) else {
                        return true;
                    };

                    let mut class_data = class_data.borrow_mut();

                    let Some(method) = class_data.members.iter_mut().find_map(|m| match m {
                        cpp::ClassMember::Method(method) if method.name == data.method_name && method.type_index == data.method_type => Some(method),
                        _ => None,
                    }) else {
                        return true;
                    };

                    if *address != 0 && !method.addresses.contains(address) {
                        method.addresses.push(*address);
                    }

                    false
                });
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

        let out_path = self.options.out.as_ref().unwrap();

        let out_prefix = match out_path.components().nth(0) {
            component @ Some(Component::Prefix(_)) => component.clone(),
            _ => None,
        };

        let source_paths = &self.options.source_paths;
        let mut empty_headers = vec![];

        for module in self.modules.values_mut() {
            // Compute the rebased output path (skip modules that don't match a filter).
            let rebased_path = {
                let module_ref = module.borrow();
                Self::rebase_output_path(source_paths, &module_ref.path)
            };

            let Some(rebased_path) = rebased_path else {
                continue;
            };

            let mut module = module.borrow_mut();

            // A header known only from line records may have nothing recoverable. Its path still
            // records the original source tree, so list it instead of writing it out empty
            if module.is_header() && module.headers.is_empty() && module.members.iter().all(|m| matches!(m, cpp::ModuleMember::EmptyLine | cpp::ModuleMember::Preprocessor(_))) {
                empty_headers.push(rebased_path.to_string_lossy().trim_start_matches(['/', '\\']).to_string());
                continue;
            }

            let mut path = PathBuf::new();
            
            // Preserve the output path prefix so it doesn't get sanitized out
            if let Some(out_prefix) = out_prefix.as_ref() {
                path.push(out_prefix.clone());
            }

            path.extend(
                PathBuf::from(crate::utils::sanitize_path(format!(
                    "{}/{}",
                    out_path.display(),
                    rebased_path.to_string_lossy().trim_start_matches('/'),
                )))
                    .components(),
            );

            //
            // Reorganize module members if requested
            //

            if self.options.reorganize {
                crate::reorganize::reorganize_module_members(
                    &context.type_finder,
                    &mut module,
                    compound_enums.as_slice(),
                    self.options.project_name.as_str(),
                )?;
            }

            //
            // Reconstruct namespace blocks from fully-qualified names
            //

            let embedded = crate::namespaces::embed_function_local_types(
                std::mem::take(&mut module.members),
            );
            let grouped = crate::namespaces::group_module_members(
                embedded,
                &self.type_name_set,
            );
            module.members = grouped;

            //
            // Write the module out to its file
            //

            if self.options.export_cpp {
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

                if let Err(e) = write!(
                    file, "{}", cpp::compact_std_templates(&cpp::shorten_std_aliases(&module.to_string()))
                ) {
                    panic!("Failed to write to file: \"{}\" - {e}", path.display());
                }
            }
        }

        //
        // List the headers that were not written, one output-relative path per line
        //

        if self.options.export_cpp && !empty_headers.is_empty() {
            empty_headers.sort_by_key(|path| path.to_lowercase());
            empty_headers.dedup_by_key(|path| path.to_lowercase());
            std::fs::create_dir_all(out_path)?;
            std::fs::write(out_path.join("empty_headers.txt"), empty_headers.join("\n") + "\n")?;
        }

        //
        // Dump debug blocks (unrolled scope trees) to JSON if requested
        //

        if let Some(path) = self.options.export_debug_blocks_json.as_ref() {
            crate::debug_blocks::export_debug_blocks_json(&self.modules, path)?;
        }

        Ok(())
    }

    #[inline(always)]
    fn load_module_global_symbols<'a, 's>(
        &mut self,
        context: &mut DecompileContext<'a, 's>
    ) -> Result<HashMap<u16, Vec<pdb2::SymbolData<'a>>>, Box<dyn Error>> {
        // println!("Loading module global symbols...");

        let mut section_contributions = vec![];
        let mut section_contributions_iter = context.debug_info.section_contributions()?;

        while let Some(section_contribution) = section_contributions_iter.next()? {
            section_contributions.push(section_contribution);
        }

        let modules = context.debug_info.modules()?.collect::<Vec<_>>()?;

        let mut global_symbols_iter = context.global_symbols.iter();
        let mut module_global_symbols = HashMap::new();
        let mut prev_module_name = None;
        let mut unhandled_symbol_kinds = HashSet::new();

        loop {
            let symbol = match global_symbols_iter.next() {
                Ok(Some(symbol)) => symbol,
                Ok(None) => break,
                Err(_) => {
                    // println!("WARNING: failed to get next symbol: {e}");
                    break;
                }
            };

            // Don't attempt to parse symbols we don't care about
            if !matches!(
                symbol.raw_kind(),
                pdb2::S_UDT
                    | pdb2::S_UDT_ST
                    | pdb2::S_PROCREF
                    | pdb2::S_PROCREF_ST
                    | pdb2::S_LPROCREF
                    | pdb2::S_LPROCREF_ST
                    | pdb2::S_PUB32
                    | pdb2::S_PUB32_ST
                    | pdb2::S_LDATA32
                    | pdb2::S_LDATA32_ST
                    | pdb2::S_GDATA32
                    | pdb2::S_GDATA32_ST
                    | pdb2::S_LMANDATA
                    | pdb2::S_LMANDATA_ST
                    | pdb2::S_GMANDATA
                    | pdb2::S_GMANDATA_ST
                    | pdb2::S_LTHREAD32
                    | pdb2::S_LTHREAD32_ST
                    | pdb2::S_GTHREAD32
                    | pdb2::S_GTHREAD32_ST
                    | pdb2::S_GPROC32
                    | pdb2::S_GPROC32_ST
                    | pdb2::S_LPROC32
                    | pdb2::S_LPROC32_ST
                    | pdb2::S_LPROC32_DPC
                    | pdb2::S_GPROC32_ID
                    | pdb2::S_LPROC32_ID
                    | pdb2::S_LPROC32_DPC_ID
            ) {
                continue;
            }

            let symbol_data = match symbol.parse() {
                Ok(symbol_data) => symbol_data,
                Err(e) => {
                    println!("WARNING: failed to parse symbol data, skipping 1: {e}");
                    continue;
                }
            };

            match symbol_data {
                pdb2::SymbolData::UserDefinedType(_) if context.id_info.is_none() => {}
                pdb2::SymbolData::UserDefinedType(_) if prev_module_name.is_some() => {
                    let module_name = prev_module_name.clone().unwrap();

                    // println!(
                    //     "Inserting global UDT into previous module \"{module_name}\": \"{}\"",
                    //     symbol_data.name().map(|s| s.to_string().to_string()).unwrap_or_else(|| "<UNNAMED>".to_string()),
                    // );

                    module_global_symbols.entry(module_name).or_insert_with(Vec::new).push(symbol_data.clone());
                }

                pdb2::SymbolData::ProcedureReference(pdb2::ProcedureReferenceSymbol { module: Some(module), .. }) => {
                    let referenced_module = modules.get(module as usize).unwrap();

                    let module_name = referenced_module.info().stream.0;
                    prev_module_name = Some(module_name.clone());

                    // println!(
                    //     "Found reference to global symbol \"{}\" in module \"{module_name}\"",
                    //     symbol_data.name().map(|s| s.to_string().to_string()).unwrap_or_else(|| "<UNNAMED>".to_string()),
                    // );

                    module_global_symbols.entry(module_name).or_insert_with(Vec::new).push(symbol_data.clone());
                }

                pdb2::SymbolData::Public(pdb2::PublicSymbol { offset, .. })
                | pdb2::SymbolData::Data(pdb2::DataSymbol { offset, .. })
                | pdb2::SymbolData::ThreadStorage(pdb2::ThreadStorageSymbol { offset, .. })
                | pdb2::SymbolData::Procedure(pdb2::ProcedureSymbol { offset, .. }) => {
                    for contribution in section_contributions.iter() {
                        let contributing_module = modules.get(contribution.module as usize).unwrap();

                        if offset >= contribution.offset && offset < contribution.offset + contribution.size {
                            let module_name = contributing_module.info().stream.0;
                            prev_module_name = Some(module_name.clone());

                            // println!(
                            //     "Found global symbol \"{}\" in module \"{module_name}\"",
                            //     symbol_data.name().map(|s| s.to_string().to_string()).unwrap_or_else(|| "<UNNAMED>".to_string()),
                            // );

                            module_global_symbols.entry(module_name).or_insert_with(Vec::new).push(symbol_data.clone());
                            break;
                        }
                    }
                }

                // A global UDT encountered before any module attribution cannot be
                // assigned to a module, so there is nowhere to put it. Skip it
                // rather than aborting the whole run.
                pdb2::SymbolData::UserDefinedType(_) => {}

                _ => {
                    if unhandled_symbol_kinds.insert(std::mem::discriminant(&symbol_data)) {
                        println!("WARNING: unhandled global symbol kind, skipping: {symbol_data:#?}");
                    }
                }
            }
        }

        // println!("Done loading module global symbols");
        
        Ok(module_global_symbols)
    }

    #[inline(always)]
    fn process_module<'a, 's>(
        &mut self,
        context: &mut DecompileContext<'a, 's>,
        pdb_module: &pdb2::Module,
        module_global_symbols: &HashMap<u16, Vec<pdb2::SymbolData>>,
    ) -> Result<(), Box<dyn Error>> {
        let module_info = match context.pdb.module_info(pdb_module)? {
            Some(module_info) => module_info,
            None => {
                // println!("module has no info: {} - {}", module.module_name(), module.object_file_name());
                return Ok(());
            }
        };

        let line_program = module_info.line_program();
        let line_offsets = crate::legacy::module_source_lines(context.pdb, pdb_module, line_program.as_ref().ok(), context.string_table)?;

        let Some(module) = self.get_or_create_cpp_module(
            context,
            pdb_module,
            &module_info,
            line_program.as_ref().ok(),
            &line_offsets,
            module_global_symbols.get(&pdb_module.info().stream.0).is_some_and(|symbols|
                symbols.iter().any(|symbol| matches!(symbol, pdb2::SymbolData::Data(_)
                    | pdb2::SymbolData::ThreadStorage(_) | pdb2::SymbolData::Procedure(_)))),
        )? else {
            println!(
                "WARNING: Failed to find source file path for module \"{}\" in \"{}\", skipping...",
                pdb_module.module_name(),
                pdb_module.object_file_name(),
            );
            return Ok(())
        };

        // println!("Processing module: \"{}\"", module.borrow().path.display());

        // Process module global symbols
        if let Some(global_symbols) = module_global_symbols.get(&pdb_module.info().stream.0).cloned() {
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

            if context.id_info.is_some() && matches!(symbol_data, pdb2::SymbolData::UserDefinedType(_)) {
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
    fn get_or_create_cpp_module<'a, 's>(
        &mut self,
        context: &mut DecompileContext<'a, 's>,
        module: &pdb2::Module,
        module_info: &pdb2::ModuleInfo,
        line_program: Option<&pdb2::LineProgram>,
        source_lines: &crate::legacy::SourceLines,
        has_global_definitions: bool,
    ) -> pdb2::Result<Option<Rc<RefCell<cpp::Module>>>> {
        let mut module_symbols = module_info.symbols()?;
        let mut module_file_path = None;
        
        // Try to find build info for the module to get the module's source file path
        while let Some(symbol_item) = module_symbols.next()? {
            let Ok(pdb2::SymbolData::BuildInfo(build_info)) = symbol_item.parse() else { continue };
            let Some(id_finder) = context.id_finder.as_ref() else { continue };
            let Ok(id_item) = id_finder.find(build_info.id) else { continue };
            let Ok(pdb2::IdData::BuildInfo(build_info)) = id_item.parse() else { continue };
            
            let mut module = cpp::Module::default();
            module.add_build_info(id_finder, build_info)?;

            if !module.path.as_os_str().is_empty() {
                module_file_path = Some(module.path.clone());
                break;
            }
        }

        let mut environment = std::collections::BTreeMap::new();
        let mut symbols = module_info.symbols()?;
        while let Some(symbol) = symbols.next()? {
            environment.extend(crate::legacy::build_environment(&symbol)?);
        }
        let cwd = environment.get("cwd").cloned().unwrap_or_default();
        if module_file_path.is_none() {
            if let Some(source) = environment.get("src").filter(|s| !s.is_empty()) {
                module_file_path = Some(crate::utils::canonicalize_path(&cwd, source, false));
            }
        }

        let mut source_files = context.source_files.get(&module.info().stream.0)
            .cloned().unwrap_or_default().into_iter()
            .map(|path| PathBuf::from(crate::utils::sanitize_path(path))).collect::<Vec<_>>();
        if let (Some(strings), Some(line_program)) = (context.string_table, line_program) {
            for file in line_program.files().collect::<Vec<_>>()? {
                let path = PathBuf::from(crate::utils::sanitize_path(file.name.to_string_lossy(strings)?));
                if !source_files.contains(&path) { source_files.push(path); }
            }
        }
        let mut line_paths = source_lines.keys().collect::<Vec<_>>();
        line_paths.sort();
        for name in line_paths {
            let path = PathBuf::from(crate::utils::sanitize_path(name));
            if !source_files.contains(&path) { source_files.push(path); }
        }
        // DBI file lists are available even when line records or /names are absent.
        if module_file_path.is_none() {
            let obj_path = self.get_module_object_file_path(module)?;
            let candidates = source_files.iter().filter(|path| path.extension()
                .is_some_and(|e| cpp::SOURCE_FILE_EXTS.iter().any(|ext| e.eq_ignore_ascii_case(ext))))
                .collect::<Vec<_>>();
            module_file_path = candidates.iter().find(|path|
                path.file_stem().map(|x| x.to_ascii_lowercase()) == obj_path.file_stem().map(|x| x.to_ascii_lowercase()))
                .map(|path| (*path).clone());
            if module_file_path.is_none() && candidates.len() == 1 {
                module_file_path = Some(candidates[0].clone());
            }
        }

        // If we couldn't find the module's source file path, we can't decompile it
        // under an original filename. Preserve useful symbols under an explicit fallback.
        if module_file_path.is_none() && context.id_info.is_none() && has_global_definitions {
            module_file_path = Some(PathBuf::from(format!("__pdb_modules/module_{}.cpp", module.info().stream.0)));
            environment.insert("object".into(), module.module_name().into_owned());
        }
        if module_file_path.is_none() && context.id_info.is_none() {
            let mut symbols = module_info.symbols()?;
            while let Some(symbol) = symbols.next()? {
                if matches!(symbol.parse(), Ok(pdb2::SymbolData::Procedure(_)
                    | pdb2::SymbolData::Data(_) | pdb2::SymbolData::ThreadStorage(_)
                    | pdb2::SymbolData::UserDefinedType(_) | pdb2::SymbolData::Constant(_))) {
                    module_file_path = Some(PathBuf::from(format!("__pdb_modules/module_{}.cpp", module.info().stream.0)));
                    environment.insert("object".into(), module.module_name().into_owned());
                    break;
                }
            }
        }
        let Some(module_file_path) = module_file_path else { return Ok(None); };

        let Some(Component::Normal(_)) = module_file_path.components().last() else {
            return Ok(None);
        };

        let module_key = module_file_path.to_string_lossy().to_lowercase();
        let module = self.modules.entry(module_key).or_insert_with(|| Rc::new(RefCell::new(cpp::Module::default().with_path(module_file_path.clone()))));
        
        if !environment.is_empty() {
            let mut module = module.borrow_mut();
            if let Some(compiler) = environment.get("cl") {
                module.compiler_path = crate::utils::canonicalize_path(&cwd, compiler, false);
            }
            if let Some(pdb) = environment.get("pdb") {
                module.pdb_path = crate::utils::canonicalize_path(&cwd, pdb, false);
            }
            // Preserve flags verbatim: Xbox switches are not the desktop MSVC grammar.
            for (key, value) in &environment {
                let comment = cpp::ModuleMember::Comment(format!("Build {key}: {value}").replace("*/", "* /"));
                if !module.members.contains(&comment) { module.members.push(comment); }
            }
        }
        for mut file_path in source_files {
            // Skip the source file of the module
            if file_path.file_name().as_ref().map(|f| f.to_ascii_lowercase()) == module_file_path.file_name().as_ref().map(|f| f.to_ascii_lowercase()) {
                continue;
            }

            // Prefer the module's casing for its own header, which must sit in the same directory
            let same_directory = file_path.parent().zip(module_file_path.parent()).is_some_and(|(a, b)| {
                crate::utils::sanitize_path(a.to_string_lossy()).eq_ignore_ascii_case(&crate::utils::sanitize_path(b.to_string_lossy()))
            });

            if same_directory && file_path.file_stem().as_ref().map(|f| f.to_ascii_lowercase()) == module_file_path.file_stem().as_ref().map(|f| f.to_ascii_lowercase()) {
                let extension = file_path.extension().as_ref().map(|e| e.to_string_lossy().to_string()).unwrap_or_else(|| String::new());
                file_path = module_file_path.with_extension(extension);
            }

            let header = (file_path.clone(), false);

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
        line_offsets: &crate::legacy::SourceLines,
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
                // Drop self-referential typedefs (`typedef X X;`) that MSVC emits for a
                // type's own name (common for anonymous-namespace classes).
                if let Ok(target) = context.type_finder.find(udt_symbol.type_index).and_then(|t| t.parse()) {
                    let target_name = match target {
                        pdb2::TypeData::Class(c) => Some(c.name.to_string().to_string()),
                        pdb2::TypeData::Enumeration(e) => Some(e.name.to_string().to_string()),
                        pdb2::TypeData::Union(u) => Some(u.name.to_string().to_string()),
                        _ => None,
                    };

                    if let Some(target_name) = target_name
                        && crate::namespaces::normalize_anon(&target_name)
                            == crate::namespaces::normalize_anon(&udt_symbol.name.to_string())
                    {
                        return Ok(());
                    }
                }

                let signature = cpp::type_name(
                    &mut self.class_table,
                    &mut self.type_sizes,
                    &mut self.type_names,
                    context.machine_type,
                    &context.type_info,
                    &context.type_finder,
                    cpp::TypeNameQuery {
                        type_index: udt_symbol.type_index,
                        modifier: None,
                        declaration_name: Some(udt_symbol.name.to_string().to_string()),
                        parameter_names: None,
                        include_this: None,
                        force_return_type: false,
                    },
                )?;

                let user_defined_type = cpp::ModuleMember::TypeDefinition(cpp::TypeDefinition {
                    signature,
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
                    &mut self.type_names,
                    context.machine_type,
                    &context.type_info,
                    &context.type_finder,
                    cpp::TypeNameQuery {
                        type_index: constant_symbol.type_index,
                        modifier: None,
                        declaration_name: Some(constant_symbol.name.to_string().to_string()),
                        parameter_names: None,
                        include_this: None,
                        force_return_type: false,
                    },
                )?;

                if type_name.starts_with("float const ") {
                    // println!("WARNING: failed to decompile constant float in \"{}\": {symbol_data:#?}", module_file_path.to_string_lossy());
                    return Ok(());
                }

                // Drop enumerators of anonymous enums (`const enum { x = 7 } x = 0x7;`) —
                // the anonymous type has no valid spelling and the value already lives in
                // the enum definition. `type_name()` now inlines the anonymous enum body
                // (so the rendered string no longer carries `<unnamed…>`); inspect the type
                // record directly instead, peeling any leading modifier.
                let mut constant_type_index = constant_symbol.type_index;
                if let Ok(pdb2::TypeData::Modifier(modifier)) = context.type_finder.find(constant_type_index).and_then(|t| t.parse()) {
                    constant_type_index = modifier.underlying_type;
                }
                let is_anonymous_constant = match context.type_finder.find(constant_type_index).and_then(|t| t.parse()) {
                    Ok(pdb2::TypeData::Enumeration(e)) => cpp::is_anonymous_type_name(&e.name.to_string()),
                    Ok(pdb2::TypeData::Class(c)) => cpp::is_anonymous_type_name(&c.name.to_string()),
                    Ok(pdb2::TypeData::Union(u)) => cpp::is_anonymous_type_name(&u.name.to_string()),
                    _ => false,
                };
                if type_name.contains("<unnamed") || is_anonymous_constant {
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
                let found = crate::legacy::line_at(line_offsets, data_symbol.offset);
                let file_name_ref = found.map(|(name_ref, _)| name_ref);
                let line_info = found.map(|(_, line)| line.clone());

                let module = if let Some(file_name_ref) = file_name_ref {
                    let path = PathBuf::from(crate::utils::sanitize_path(file_name_ref));
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

                // Drop compiler-generated artifacts (dynamic-initializer thunks like
                // `…$initializer$`, EH funclets, local scopes) and out-of-line members of
                // anonymous types — none were written in source. Mirrors the guard on
                // procedures below.
                let data_name = data_symbol.name.to_string().to_string();
                if data_name.contains("<unnamed")
                    || matches!(
                        crate::namespaces::classify(&data_name, &self.type_name_set),
                        crate::namespaces::NameClass::CompilerGenerated,
                    )
                {
                    return Ok(());
                }

                if module.borrow().members.iter().any(|member| match member {
                    cpp::ModuleMember::Data { address: a, .. } if *a == address => true,
                    _ => false,
                }) {
                    return Ok(());
                }

                // Strip the namespace prefix from the variable's own qualified name so an
                // out-of-line definition reads `locale::id num_put<…>::id;` inside
                // `namespace std { }` rather than the redundant `std::num_put<…>::id`. The
                // `name` field keeps its full qualification so namespace grouping can still
                // classify it. Parameter/type references stay fully qualified (still valid).
                let declaration_name = crate::namespaces::strip_namespace_prefix(
                    &data_name,
                    &self.type_name_set,
                );

                module.borrow_mut().members.push(cpp::ModuleMember::Data {
                    is_static: !data_symbol.global,
                    is_extern: false,
                    name: data_name,
                    signature: format!(
                        "{}; // 0x{address:X}",
                        cpp::type_name(
                            &mut self.class_table,
                            &mut self.type_sizes,
                            &mut self.type_names,
                            context.machine_type,
                            &context.type_info,
                            &context.type_finder,
                            cpp::TypeNameQuery {
                                type_index: data_symbol.type_index,
                                modifier: None,
                                declaration_name: Some(declaration_name),
                                parameter_names: None,
                                include_this: None,
                                force_return_type: false,
                            },
                        )?,
                    ),
                    address,
                    line: line_info.map(|x| x.line_start),
                });

                if let Some(script_file) = self.script_file.as_mut()
                    && self.options.export_pseudocode_to_files
                {
                    writeln!(script_file, "decompile_to_file(0x{address:X}, \"{}\")", module.borrow().path.to_string_lossy())?;
                }
            }

            pdb2::SymbolData::ThreadStorage(thread_storage_symbol) => {
                let found = crate::legacy::line_at(line_offsets, thread_storage_symbol.offset);
                let file_name_ref = found.map(|(name_ref, _)| name_ref);
                let line_info = found.map(|(_, line)| line.clone());

                let module = if let Some(file_name_ref) = file_name_ref {
                    let path = PathBuf::from(crate::utils::sanitize_path(file_name_ref));
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

                // Check if this is a thread_local variable defined in a class/struct/union.
                let name = thread_storage_symbol.name.to_string().to_string();

                // Drop compiler-generated artifacts, as with plain data above.
                if name.contains("<unnamed")
                    || matches!(
                        crate::namespaces::classify(&name, &self.type_name_set),
                        crate::namespaces::NameClass::CompilerGenerated,
                    )
                {
                    return Ok(());
                }

                if module.borrow().members.iter().any(|member| match member {
                    cpp::ModuleMember::Data { address: a, .. } if *a == address => true,
                    _ => false,
                }) {
                    return Ok(());
                }

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

                // Strip the namespace prefix from the variable's own qualified name (see
                // the plain-data path above); the `name` field keeps full qualification.
                let declaration_name = crate::namespaces::strip_namespace_prefix(
                    &name,
                    &self.type_name_set,
                );

                module.borrow_mut().members.push(cpp::ModuleMember::ThreadStorage {
                    is_static: !thread_storage_symbol.global,
                    name: name.clone(),
                    signature: format!(
                        "thread_local {}; // 0x{address:X}",
                        cpp::type_name(
                            &mut self.class_table,
                            &mut self.type_sizes,
                            &mut self.type_names,
                            context.machine_type,
                            &context.type_info,
                            &context.type_finder,
                            cpp::TypeNameQuery {
                                type_index: thread_storage_symbol.type_index,
                                modifier: None,
                                declaration_name: Some(declaration_name),
                                parameter_names: None,
                                include_this: None,
                                force_return_type: false,
                            },
                        )?,
                    ),
                    address,
                    line: line_info.map(|x| x.line_start),
                });

                if let Some(script_file) = self.script_file.as_mut()
                    && self.options.export_pseudocode_to_files
                {
                    writeln!(script_file, "decompile_to_file(0x{address:X}, \"{}\")", module.borrow().path.to_string_lossy())?;
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

                let (return_type, arguments) = match procedure_type {
                    pdb2::TypeData::Procedure(data) => (
                        data.return_type,
                        cpp::argument_type_list(
                            &context.type_finder,
                            None,
                            data.argument_list, 
                            Some(parameters.clone()),
                        ).unwrap()
                    ),

                    pdb2::TypeData::MemberFunction(data) => (
                        Some(data.return_type),
                        cpp::argument_type_list(
                            &context.type_finder,
                            data.this_pointer_type,
                            data.argument_list, 
                            Some(parameters.clone()),
                        ).unwrap()
                    ),

                    pdb2::TypeData::Primitive(_)
                    | pdb2::TypeData::FieldList(_)
                    | pdb2::TypeData::Pointer(_)
                    | pdb2::TypeData::ArgumentList(_)
                    | pdb2::TypeData::Class(_)
                    | pdb2::TypeData::Union(_)
                    | pdb2::TypeData::Enumeration(_)
                    | pdb2::TypeData::MethodList(_)
                    | pdb2::TypeData::Array(_)
                    | pdb2::TypeData::Bitfield(_)
                    | pdb2::TypeData::VirtualTableShape(_) => {
                        // println!("WARNING: Skipping unexpected procedure type: {procedure_type:#?}");
                        return Ok(());
                    }

                    pdb2::TypeData::Modifier(modifier) => match context.type_finder.find(modifier.underlying_type)?.parse()? {
                        pdb2::TypeData::Procedure(data) => (
                            data.return_type,
                            cpp::argument_type_list(
                                &context.type_finder,
                                None,
                                data.argument_list, 
                                Some(parameters.clone()),
                            ).unwrap()
                        ),

                        pdb2::TypeData::MemberFunction(data) => (
                            Some(data.return_type),
                            cpp::argument_type_list(
                                &context.type_finder,
                                data.this_pointer_type,
                                data.argument_list, 
                                Some(parameters.clone()),
                            ).unwrap()
                        ),

                        procedure_type if matches!(
                            procedure_type,
                            pdb2::TypeData::Primitive(_)
                            | pdb2::TypeData::FieldList(_)
                            | pdb2::TypeData::Pointer(_)
                            | pdb2::TypeData::ArgumentList(_)
                            | pdb2::TypeData::Class(_)
                            | pdb2::TypeData::Union(_)
                            | pdb2::TypeData::Enumeration(_)
                            | pdb2::TypeData::MethodList(_)
                            | pdb2::TypeData::Array(_)
                            | pdb2::TypeData::Bitfield(_)
                            | pdb2::TypeData::VirtualTableShape(_)
                        ) => {
                            // println!("WARNING: Skipping unexpected modifier procedure type: {procedure_type:#?}");
                            return Ok(());
                        }

                        data => todo!("{data:#?}")
                    }

                    data => todo!("{data:#?}")
                };

                let found = crate::legacy::line_at(line_offsets, procedure_symbol.offset);
                let file_name_ref = found.map(|(name_ref, _)| name_ref);
                let line_info = found.map(|(_, line)| line.clone());

                let rva = match procedure_symbol.offset.to_rva(context.address_map) {
                    Some(rva) => rva,
                    None => {
                        // println!("WARNING: no RVA found for procedure symbol: {procedure_symbol:?}");
                        return Ok(());
                    }
                };

                let address = self.options.base_address.unwrap_or(0) + rva.0 as u64;

                let module = if let Some(file_name_ref) = file_name_ref {
                    let path = PathBuf::from(crate::utils::sanitize_path(file_name_ref));
                    let module_key = path.to_string_lossy().to_lowercase().to_string();
                    self.modules.entry(module_key).or_insert_with(|| Rc::new(RefCell::new(cpp::Module::default().with_path(path)))).clone()
                } else {
                    module
                };

                if module.borrow().members.iter().any(|member| match member {
                    cpp::ModuleMember::Procedure(cpp::Procedure { address: a, .. }) if *a == address => true,
                    _ => false,
                }) {
                    return Ok(());
                }

                // Build the signature with the namespace prefix stripped from the
                // function's own qualified name, so an out-of-line definition reads
                // `void c_foo::bar(...)` inside `namespace blah { }` rather than the
                // invalid `void blah::c_foo::bar(...)`. Parameter/return references
                // keep their full qualification (still valid within the block).
                let declaration_name = crate::namespaces::strip_namespace_prefix(
                    &procedure_symbol.name.to_string(),
                    &self.type_name_set,
                );

                let procedure_signature = cpp::type_name(
                    &mut self.class_table,
                    &mut self.type_sizes,
                    &mut self.type_names,
                    context.machine_type,
                    &context.type_info,
                    &context.type_finder,
                    cpp::TypeNameQuery {
                        type_index: procedure_symbol.type_index,
                        modifier: None,
                        declaration_name: Some(declaration_name),
                        parameter_names: Some(parameters.clone()),
                        include_this: None,
                        force_return_type: false,
                    },
                )?;

                // Recover top-level pointer `const` on parameters. MSVC drops it from
                // the type records for pointer-to-primitive params, but it survives in
                // the mangled name; transfer it back by position.
                let procedure_signature = match self.mangled_by_address.get(&address) {
                    Some(mangled) => crate::param_const::patch_signature(&procedure_signature, mangled),
                    None => procedure_signature,
                };

                // Drop compiler-generated artifacts (EH funclets, dynamic initializers,
                // local scopes). Real global-scope and namespaced procedures are kept;
                // namespaced ones get gathered into `namespace { }` blocks before output.
                // Also drop the out-of-line special members (ctor/dtor/assignment) of an
                // anonymous type: their mangled name carries the `<unnamed-type-*>` placeholder,
                // which was never valid C++ — those members never existed in source.
                if procedure_signature.starts_with("...")
                    || procedure_symbol.name.to_string().contains("<unnamed")
                    || matches!(
                        crate::namespaces::classify(&procedure_symbol.name.to_string(), &self.type_name_set),
                        crate::namespaces::NameClass::CompilerGenerated,
                    )
                {
                    return Ok(());
                }

                let is_static = !procedure_symbol.global;
                let is_inline = frame_procedure.as_ref().map(|x| x.flags.inline_spec).unwrap_or(false);
                let was_inlined = frame_procedure.as_ref().map(|x| x.flags.was_inlined).unwrap_or(false);

                let mut declspecs = vec![];

                if let Some(frame_procedure) = frame_procedure.as_ref() {
                    if self.options.force_no_inlines && !was_inlined {
                        declspecs.push("noinline".into());
                    }

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
                                    &mut self.type_names,
                                    context.machine_type,
                                    &context.type_info,
                                    &context.type_finder,
                                    cpp::TypeNameQuery {
                                        type_index: class_member_function.return_type,
                                        modifier: None,
                                        declaration_name: None,
                                        parameter_names: None,
                                        include_this: None,
                                        force_return_type: false,
                                    },
                                )?;

                                let lhs_arg_list = cpp::argument_list(
                                    &mut self.class_table,
                                    &mut self.type_sizes,
                                    &mut self.type_names,
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
                                    &mut self.type_names,
                                    context.machine_type,
                                    &context.type_info,
                                    &context.type_finder,
                                    cpp::TypeNameQuery {
                                        type_index: member_function.return_type,
                                        modifier: None,
                                        declaration_name: None,
                                        parameter_names: None,
                                        include_this: None,
                                        force_return_type: false,
                                    },
                                )?;

                                let rhs_arg_list = cpp::argument_list(
                                    &mut self.class_table,
                                    &mut self.type_sizes,
                                    &mut self.type_names,
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
                                &mut self.type_names,
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
                                    &mut self.type_names,
                                    context.machine_type,
                                    &context.type_info,
                                    &context.type_finder,
                                    cpp::TypeNameQuery {
                                        type_index: *t,
                                        modifier: None,
                                        declaration_name: None,
                                        parameter_names: None,
                                        include_this: None,
                                        force_return_type: false,
                                    },
                                ).unwrap()
                            });

                            let this_adjustment = member_function.this_adjustment;

                            let (method_name, method_type) = match &class.borrow().members[class_method_index] {
                                cpp::ClassMember::Method(class_method) => (class_method.name.clone(), class_method.type_index),
                                _ => unreachable!(),
                            };

                            member_method_data = Some(cpp::MemberMethodData {
                                class_type,
                                declaring_class,
                                this_pointer_type,
                                this_adjustment,
                                method_name,
                                method_type,
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
                                &mut self.type_names,
                                context.machine_type,
                                &context.type_info,
                                &context.type_finder,
                                cpp::TypeNameQuery {
                                    type_index: class_method.type_index,
                                    modifier: class_method.modifier.clone(),
                                    declaration_name: Some(class_method.name.to_string().to_string()),
                                    parameter_names: Some(parameters.clone()),
                                    include_this: None,
                                    force_return_type: false,
                                },
                            )?;
                        }
                    }
                }

                let ida_procedure_signature = cpp::type_name(
                    &mut self.class_table,
                    &mut self.type_sizes,
                    &mut self.type_names,
                    context.machine_type,
                    &context.type_info,
                    &context.type_finder,
                    cpp::TypeNameQuery {
                        type_index: procedure_symbol.type_index,
                        modifier: None,
                        declaration_name: Some(procedure_symbol.name.to_string().to_string()),
                        parameter_names: Some(parameters.clone()),
                        include_this: member_method_data.as_ref().map(|m| m.declaring_class.clone()).or(Some(String::new())),
                        force_return_type: true,
                    },
                )?;
                
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

                let procedure = cpp::Procedure {
                    address,
                    line: line_info.map(|x| x.line_start),
                    type_index: procedure_symbol.type_index,
                    is_static,
                    is_inline,
                    is_extern: false,
                    member_method_data,
                    declspecs,
                    name: procedure_symbol.name.to_string().to_string(),
                    signature: procedure_signature,
                    ida_signature: Some(ida_procedure_signature),
                    body,
                    return_type,
                    arguments
                };

                // Drop compiler-generated procedure definitions (implicit ctors/dtors/assignment,
                // thunks, dynamic initializers): they have no source line and were never written
                // in source. This matches dropping their declarations in `Class::add_member`.
                // Legacy PDBs also omit lines for handwritten assembly and stripped
                // objects, so missing line information is not evidence of generation.
                if !self.options.include_compiler_generated && context.id_info.is_some() && procedure.line.is_none() {
                    return Ok(());
                }

                if let Some(script_file) = self.script_file.as_mut() {
                    if self.options.export_pseudocode_to_files {
                        writeln!(script_file, "decompile_to_file(0x{address:X}, \"{}\")", module.borrow().path.to_string_lossy())?;
                    }

                    if self.options.export_pseudocode_to_json {
                        writeln!(script_file, "decompile_to_json(0x{address:X})")?;
                    }

                    if self.options.export_function_types
                        && let Some(ida_signature) = &procedure.ida_signature
                    {
                        writeln!(script_file, "set_function_type(0x{:X}, \"{}\")", address, ida_signature)?;
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
                let file_name_ref = crate::legacy::line_at(line_offsets, public_symbol.offset).map(|(name_ref, _)| name_ref);

                let module = if let Some(file_name_ref) = file_name_ref {
                    let path = PathBuf::from(crate::utils::sanitize_path(file_name_ref));
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

            pdb2::SymbolData::Section(_) | pdb2::SymbolData::CoffGroup(_)
            | pdb2::SymbolData::Trampoline(_) => {
                // Linker modules can own globals even without a source filename.
                // Keep their section metadata without trying to turn it into C++ declarations.
                module.borrow_mut().members.push(cpp::ModuleMember::Comment(
                    format!("PDB linker metadata: {symbol_data:?}").replace("*/", "* /"),
                ));
            }

            ref data => panic!("Unhandled symbol data in process_module_symbol_data - {data:#?}"),
        }

        Ok(())
    }

    /// Number of parameters a procedure/member-function type takes (including the
    /// implicit `this` for member functions). Used to drop leading parameter symbols
    /// in PDBs that lack `S_LOCAL`.
    fn procedure_argument_count<'a, 's>(&self, context: &DecompileContext<'a, 's>, type_index: pdb2::TypeIndex) -> usize {
        fn arglist_len(finder: &pdb2::TypeFinder, arglist: pdb2::TypeIndex) -> usize {
            match finder.find(arglist).and_then(|t| t.parse()) {
                Ok(pdb2::TypeData::ArgumentList(d)) => d.arguments.len(),
                _ => 0,
            }
        }

        match context.type_finder.find(type_index).and_then(|t| t.parse()) {
            Ok(pdb2::TypeData::Procedure(d)) => arglist_len(&context.type_finder, d.argument_list),
            Ok(pdb2::TypeData::MemberFunction(d)) => {
                arglist_len(&context.type_finder, d.argument_list)
                    + if d.this_pointer_type.is_some() { 1 } else { 0 }
            }
            Ok(pdb2::TypeData::Modifier(m)) => self.procedure_argument_count(context, m.underlying_type),
            _ => 0,
        }
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
        let saw_local_symbol = Rc::new(RefCell::new(false));

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

                    pdb2::SymbolData::BasePointerRelative(x) => {
                        register_relative_names.borrow_mut().push(x.name.to_string().to_string());
                    }

                    pdb2::SymbolData::FrameProcedure(x) => {
                        frame_procedures.borrow_mut().push(x.clone());
                    }

                    pdb2::SymbolData::Local(_) => {
                        *saw_local_symbol.borrow_mut() = true;
                    }

                    _ => {}
                }

                Ok(())
            })?,
        };

        // In PDBs without `S_LOCAL`, parameters and locals are all register/BP-relative
        // symbols with no flag distinguishing them; by CodeView convention the parameters
        // lead the scope, so drop that many leading statements. `S_LOCAL` PDBs identify
        // parameters by the `isparam` flag instead (handled in parse_statement_symbols).
        if !*saw_local_symbol.borrow() {
            let argument_count = self.procedure_argument_count(context, procedure_symbol.type_index);
            for _ in 0..argument_count.min(block.statements.len()) {
                block.statements.remove(0);
            }
        }

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
        let id_finder = context.id_finder.as_ref().ok_or(pdb2::Error::StreamNotFound(4))?;
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

        // Per-scope dedup of block variables. Modern PDBs declare a local once as
        // `S_LOCAL` and again as a register/BP-relative location record; we keep the
        // `S_LOCAL` one and drop the duplicate location record. Parameters (S_LOCAL
        // `isparam`) are tracked here so their register/BP records are dropped too —
        // they already appear in the signature. Functions with no `S_LOCAL` at all
        // fall through and keep their register/BP locals.
        let mut parameter_names: std::collections::HashSet<String> = std::collections::HashSet::new();
        let mut emitted_local_names: std::collections::HashSet<String> = std::collections::HashSet::new();

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
                    // Drop if this name was already declared via S_LOCAL (a duplicate
                    // location record) or is a parameter.
                    let name = register_variable_symbol.name.to_string().to_string();
                    if parameter_names.contains(&name) || !emitted_local_names.insert(name) {
                        continue;
                    }

                    statements.push(cpp::Statement::Variable(cpp::Variable {
                        signature: cpp::type_name(
                            &mut self.class_table,
                            &mut self.type_sizes,
                            &mut self.type_names,
                            context.machine_type,
                            &context.type_info,
                            &context.type_finder,
                            cpp::TypeNameQuery {
                                type_index: register_variable_symbol.type_index,
                                modifier: None,
                                declaration_name: Some(register_variable_symbol.name.to_string().to_string()),
                                parameter_names: None,
                                include_this: None,
                                force_return_type: false,
                            },
                        )?,
                        value: None,
                        comment: Some(format!("r{}", register_variable_symbol.register.0)),
                    }));
                }

                pdb2::SymbolData::RegisterRelative(register_relative_symbol) => {
                    let name = register_relative_symbol.name.to_string().to_string();
                    if parameter_names.contains(&name) || !emitted_local_names.insert(name) {
                        continue;
                    }

                    statements.push(cpp::Statement::Variable(cpp::Variable {
                        signature: cpp::type_name(
                            &mut self.class_table,
                            &mut self.type_sizes,
                            &mut self.type_names,
                            context.machine_type,
                            &context.type_info,
                            &context.type_finder,
                            cpp::TypeNameQuery {
                                type_index: register_relative_symbol.type_index,
                                modifier: None,
                                declaration_name: Some(register_relative_symbol.name.to_string().to_string()),
                                parameter_names: None,
                                include_this: None,
                                force_return_type: false,
                            },
                        )?,
                        value: None,
                        // comment: Some(format!("r{} offset {}", register_relative_symbol.register.0, register_relative_symbol.offset))
                        comment: None,
                    }));
                }

                pdb2::SymbolData::BasePointerRelative(base_pointer_relative_symbol) => {
                    let name = base_pointer_relative_symbol.name.to_string().to_string();
                    if parameter_names.contains(&name) || !emitted_local_names.insert(name) {
                        continue;
                    }

                    statements.push(cpp::Statement::Variable(cpp::Variable {
                        signature: cpp::type_name(
                            &mut self.class_table,
                            &mut self.type_sizes,
                            &mut self.type_names,
                            context.machine_type,
                            &context.type_info,
                            &context.type_finder,
                            cpp::TypeNameQuery {
                                type_index: base_pointer_relative_symbol.type_index,
                                modifier: None,
                                declaration_name: Some(base_pointer_relative_symbol.name.to_string().to_string()),
                                parameter_names: None,
                                include_this: None,
                                force_return_type: false,
                            },
                        )?,
                        value: None,
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
                            &mut self.type_names,
                            context.machine_type,
                            &context.type_info,
                            &context.type_finder,
                            cpp::TypeNameQuery {
                                type_index: constant_symbol.type_index,
                                modifier: None,
                                declaration_name: Some(constant_symbol.name.to_string().to_string()),
                                parameter_names: None,
                                include_this: None,
                                force_return_type: false,
                            },
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
                            &mut self.type_names,
                            context.machine_type,
                            &context.type_info,
                            &context.type_finder,
                            cpp::TypeNameQuery {
                                type_index: data_symbol.type_index,
                                modifier: None,
                                declaration_name: Some(data_symbol.name.to_string().to_string()),
                                parameter_names: None,
                                include_this: None,
                                force_return_type: false,
                            },
                        )?),
                        value: None,
                        comment: data_symbol.offset.to_rva(context.address_map).map(|rva| {
                            format!("0x{:X}", self.options.base_address.unwrap_or(0) + rva.0 as u64)
                        }),
                    }));
                }

                pdb2::SymbolData::Local(local_symbol) => {
                    let name = local_symbol.name.to_string().to_string();

                    // Parameters already appear in the signature (record their names so
                    // the matching register/BP location records are dropped too);
                    // compiler-generated locals are noise. Emit only true source locals.
                    if local_symbol.flags.isparam {
                        parameter_names.insert(name);
                        continue;
                    }

                    if local_symbol.flags.compgenx || !emitted_local_names.insert(name) {
                        continue;
                    }

                    statements.push(cpp::Statement::Variable(cpp::Variable {
                        signature: cpp::type_name(
                            &mut self.class_table,
                            &mut self.type_sizes,
                            &mut self.type_names,
                            context.machine_type,
                            &context.type_info,
                            &context.type_finder,
                            cpp::TypeNameQuery {
                                type_index: local_symbol.type_index,
                                modifier: None,
                                declaration_name: Some(local_symbol.name.to_string().to_string()),
                                parameter_names: None,
                                include_this: None,
                                force_return_type: false,
                            },
                        )?,
                        value: None,
                        comment: None,
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
                                &mut self.type_names,
                                context.machine_type,
                                &context.type_info,
                                &context.type_finder,
                                cpp::TypeNameQuery {
                                    type_index: tls_symbol.type_index,
                                    modifier: None,
                                    declaration_name: Some(tls_symbol.name.to_string().to_string()),
                                    parameter_names: None,
                                    include_this: None,
                                    force_return_type: false,
                                },
                            )?,
                        ),
                        value: None,
                        comment: tls_symbol.offset.to_rva(context.address_map).map(|rva| {
                            format!("0x{:X}", self.options.base_address.unwrap_or(0) + rva.0 as u64)
                        }),
                    }));
                }

                pdb2::SymbolData::CallSiteInfo(_call_site_info) => {
                    // let _address = call_site_info.offset
                    //     .to_rva(context.address_map)
                    //     .map(|rva| self.options.base_address.unwrap_or(0) + rva.0 as u64)
                    //     .unwrap();

                    // let type_name = cpp::type_name(
                    //     &mut self.class_table,
                    //     &mut self.type_sizes,
                    //     &mut self.type_names,
                    //     context.machine_type,
                    //     &context.type_info,
                    //     &context.type_finder,
                    //     cpp::TypeNameQuery {
                    //         type_index: call_site_info.type_index,
                    //         modifier: None,
                    //         declaration_name: None,
                    //         parameter_names: None,
                    //         include_this: None,
                    //         force_return_type: false,
                    //     },
                    // )?;

                    //statements.push(cpp::Statement::Comment(format!("function call @ 0x{:X}: {}", address, type_name)));
                    // statements.push(cpp::Statement::Comment(format!("function call : {}", type_name)));
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
                | pdb2::SymbolData::HeapAllocationSite(_)
                | pdb2::SymbolData::Annotation(_)
                | pdb2::SymbolData::ArmSwitchTable(_) => {
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
                | pdb2::SymbolData::HeapAllocationSite(_)
                | pdb2::SymbolData::Annotation(_)
                | pdb2::SymbolData::ArmSwitchTable(_) => {
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
                | pdb2::SymbolData::HeapAllocationSite(_)
                | pdb2::SymbolData::Annotation(_)
                | pdb2::SymbolData::ArmSwitchTable(_) => {
                    // println!("WARNING: Unused symbol data in parse_separated_code_symbols: {symbol_data:#?}");
                }

                _ => panic!("Unhandled symbol data in parse_separated_code_symbols - {symbol_data:#?}"),
            }
        }
    }

    /// Adds the classes defined in `class_data`, itself included, by qualified name.
    fn collect_defined_classes(
        class_data: &Rc<RefCell<cpp::Class>>,
        scope: Option<&str>,
        classes: &mut HashMap<String, Rc<RefCell<cpp::Class>>>,
    ) {
        if class_data.borrow().is_declaration {
            return;
        }

        // A nested class stores its own name only
        let name = match scope {
            Some(scope) => format!("{scope}::{}", class_data.borrow().name),
            None => class_data.borrow().name.clone(),
        };

        for member in class_data.borrow().members.iter() {
            if let cpp::ClassMember::Class(nested) = member {
                Self::collect_defined_classes(nested, Some(&name), classes);
            }
        }

        classes.entry(name).or_insert_with(|| class_data.clone());
    }

    /// Adds the indices of nested types defined inside `class_data`, at any depth.
    fn collect_nested_definitions(class_data: &cpp::Class, indices: &mut HashSet<pdb2::TypeIndex>) {
        for member in class_data.members.iter() {
            match member {
                cpp::ClassMember::Class(nested) => {
                    let nested = nested.borrow();

                    if !nested.is_declaration && indices.insert(nested.index) {
                        Self::collect_nested_definitions(&nested, indices);
                    }
                }

                cpp::ClassMember::Enum(nested) if !nested.is_declaration => {
                    indices.insert(nested.index);
                }

                _ => {}
            }
        }
    }

    fn fix_nested_types(
        module_members: &[cpp::ModuleMember],
        module_member_index: usize,
        class_data: Rc<RefCell<cpp::Class>>,
        scope: Option<&str>,
        remove_class_names: &mut Vec<String>,
        remove_enum_names: &mut Vec<String>,
    ) {
        // A nested class stores its own name only; qualify it to find its nested types' definitions
        let class_name = match scope {
            Some(scope) => format!("{scope}::{}", class_data.borrow().name),
            None => class_data.borrow().name.clone(),
        };
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

                            let nested_depth = nested_class.member_indent_level();

                            for member in nested_class.members.iter_mut() {
                                match member {
                                    cpp::ClassMember::Class(inner_class) => inner_class.borrow_mut().depth = nested_depth,
                                    cpp::ClassMember::Enum(inner_enum) => inner_enum.depth = nested_depth,
                                    _ => {}
                                }
                            }
                        }
                    }

                    Self::fix_nested_types(module_members, module_member_index, nested_class.clone(), Some(&class_name), remove_class_names, remove_enum_names);
                    
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

                                Self::fix_nested_types(module_members, module_member_index, other_class.clone(), None, remove_class_names, remove_enum_names);

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
