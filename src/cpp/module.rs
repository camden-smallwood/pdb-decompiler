use super::{type_name, Class, Enum};
use std::{fmt, path::PathBuf};

pub static SOURCE_FILE_EXTS: &[&str] = &[
    "c", "cc", "cpp", "cxx", "pch", "asm", "fasm", "masm", "res", "exp",
];

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModuleMember {
    Class(Class),
    Enum(Enum),
    UserDefinedType(String),
    UsingNamespace(String),
    Constant(String),
    Data(String, u64, Option<u32>),
    ThreadStorage(String, u64, Option<u32>),
    Procedure(String, u64, Option<u32>),
}

impl fmt::Display for ModuleMember {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Class(c) => c.fmt(f),
            Self::Enum(e) => e.fmt(f),
            Self::UserDefinedType(u) => u.fmt(f),
            Self::UsingNamespace(n) => f.write_fmt(format_args!("using namespace {n};")),
            Self::Constant(c) => c.fmt(f),
            Self::Data(d, _, _) => d.fmt(f),
            Self::ThreadStorage(t, _, _) => t.fmt(f),
            Self::Procedure(p, _, _) => p.fmt(f),
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Module {
    pub path: PathBuf,
    pub compiler_path: PathBuf,
    pub pdb_path: PathBuf,

    pub headers: Vec<(PathBuf, bool)>,
    pub members: Vec<ModuleMember>,

    pub pack_structure_members: Option<String>,
    pub pch_file_name: Option<PathBuf>,
    pub precompiled_header_file_name: Option<String>,
    pub output_warning_level: Option<usize>,
    pub minimum_cpu_architecture: Option<String>,
    pub optimize_for_cpu_architecture: Option<String>,
    pub floating_point_model: Option<String>,
    pub floating_point_conversions: Option<String>,
    pub error_report: Option<String>,
    pub guard_string: Option<String>,
    pub hash_algorithm: Option<String>,
    pub disable_warnings_after_version: Option<String>,
    pub openmp: Option<String>,
    pub inline_function_expansion: Option<usize>,

    pub additional_include_dirs: Vec<PathBuf>,
    pub using_directive_dirs: Vec<PathBuf>,
    pub forced_using_directives: Vec<PathBuf>,
    pub preprocessor_definitions: Vec<(String, Option<String>)>,
    pub preprocess_include_files: Vec<String>,
    pub disabled_warnings: Vec<String>,
    pub warnings_as_errors: Vec<String>,
    pub warnings_level1: Vec<String>,
    pub warnings_level2: Vec<String>,
    pub warnings_level3: Vec<String>,
    pub warnings_level4: Vec<String>,
    pub feature_toggles: Vec<String>,
    pub pch_references: Vec<String>,
    pub d1_args: Vec<String>,
    pub d2_args: Vec<String>,

    pub compile_without_linking: bool,
    pub remove_default_library_name: bool,
    pub function_level_linking: bool,
    pub enable_all_warnings: bool,
    pub treat_warnings_as_errors: bool,
    pub copy_preprocessor_output_to_stdout: bool,
    pub enable_cpp_exceptions: bool,
    pub enable_seh_exceptions: bool,
    pub extern_c_defaults_to_nothrow: bool,
    pub always_generate_noexcept_checks: bool,
    pub enable_rtti: bool,
    pub enable_string_pooling: bool,
    pub check_buffer_security: bool,
    pub enable_whole_program_optimization: bool,
    pub enable_whole_program_data_optimization: bool,
    pub optimization_disabled: bool,
    pub favors_small_code: bool,
    pub favors_fast_code: bool,
    pub generate_c7_debug_info: bool,
    pub generate_full_debug_info: bool,
    pub generate_richer_debug_info_for_optimized_code: bool,
    pub inject_pch_reference: bool,
    pub ignore_standard_include_dir: bool,
    pub nologo: bool,
    pub no_thread_safe_statics: bool,
    pub enable_sdl: bool,
    pub create_small_code: bool,
    pub create_fast_code: bool,
    pub generate_intrinsic_functions: bool,
    pub enable_minimal_rebuild: bool,
    pub enable_code_analysis: bool,
    pub enable_coroutines: bool,
    pub enable_coroutines_strict: bool,
    pub consider_floating_point_contractions: bool,
    pub consider_floating_point_exceptions: bool,
    pub use_cdecl_calling_convention: bool,
    pub windows_runtime_compilation: bool,
    pub windows_runtime_compilation_nostdlib: bool,
    pub multiple_process_support: bool,
    pub frame_pointer_emission: bool,
    pub full_source_path_in_diagnostics: bool,
    pub use_byte_strings: bool,
    pub create_kernel_mode_binary: bool,
    pub use_c_source_file_type: bool,
    pub use_cpp_source_file_type: bool,
    pub enable_loop_parallelization: bool,
    pub enable_fast_runtime_checks: bool,
    pub convert_to_smaller_type_check_at_runtime: bool,
    pub enable_stack_frame_runtime_checks: bool,
    pub enable_uninitialized_local_usage_checks: bool,
    pub bigobj: bool,
    pub build_inline: bool,
    pub serialize_pdb_with_mspdbsrv: bool,
    pub unknown_compiler_options_are_errors: bool,
}

impl Module {
    pub fn with_path(mut self, path: PathBuf) -> Self {
        self.path = path;
        self
    }

    pub fn add_type_definition(
        &mut self,
        machine_type: pdb::MachineType,
        type_info: &pdb::TypeInformation,
        type_finder: &pdb::TypeFinder,
        type_index: pdb::TypeIndex,
        line: u32,
    ) -> pdb::Result<()> {
        if self.members.iter().position(|x| match x {
            ModuleMember::Class(c) => c.index == type_index,
            ModuleMember::Enum(e) => e.index == type_index,
            _ => false,
        }).is_some() {
            return Ok(());
        }

        let type_item = match type_finder.find(type_index) {
            Ok(type_item) => type_item,
            Err(e) => {
                eprintln!("WARNING: failed to find type: {e}");
                return Ok(());
            }
        };

        match type_item.parse() {
            Ok(pdb::TypeData::Class(data)) => {
                let mut definition = Class {
                    kind: Some(data.kind),
                    is_union: false,
                    is_declaration: false,
                    name: data.name.to_string().to_string(),
                    index: type_index,
                    depth: 0,
                    line,
                    size: data.size,
                    base_classes: vec![],
                    members: vec![],
                    field_attributes: None,
                };

                if let Some(derived_from) = data.derived_from {
                    if let Err(e) = definition.add_derived_from(type_finder, derived_from) {
                        eprintln!("WARNING: failed to add class derived from: {e}");
                    }
                }

                if data.properties.forward_reference() {
                    definition.is_declaration = true;
                } else if let Some(fields) = data.fields {
                    if let Err(e) = definition.add_members(machine_type, type_info, type_finder, fields) {
                        eprintln!("WARNING: failed to add class members: {e}");
                    }
                }

                let mut exists = false;

                for member in self.members.iter() {
                    if let ModuleMember::Class(other_definition) = member {
                        if definition.kind == other_definition.kind
                            && definition.name == other_definition.name
                            && definition.size == other_definition.size
                            && definition.base_classes.eq(&other_definition.base_classes)
                            && definition.members.eq(&other_definition.members)
                        {
                            exists = true;
                            break;
                        }
                    }
                }

                if !exists {
                    self.members.push(ModuleMember::Class(definition));
                }
            }

            Ok(pdb::TypeData::Union(data)) => {
                let mut definition = Class {
                    kind: None,
                    is_union: true,
                    is_declaration: false,
                    name: data.name.to_string().to_string(),
                    index: type_index,
                    depth: 0,
                    line,
                    size: data.size,
                    base_classes: vec![],
                    members: vec![],
                    field_attributes: None,
                };

                if data.properties.forward_reference() {
                    definition.is_declaration = true;
                } else if let Err(e) = definition.add_members(machine_type, type_info, type_finder, data.fields) {
                    eprintln!("WARNING: failed to add union members: {e}");
                }

                let mut exists = false;

                for member in self.members.iter() {
                    if let ModuleMember::Class(other_definition) = member {
                        if definition.kind == other_definition.kind
                            && definition.name == other_definition.name
                            && definition.size == other_definition.size
                            && definition.base_classes.eq(&other_definition.base_classes)
                            && definition.members.eq(&other_definition.members)
                        {
                            exists = true;
                            break;
                        }
                    }
                }

                if !exists {
                    self.members.push(ModuleMember::Class(definition));
                }
            }

            Ok(pdb::TypeData::Enumeration(data)) => {
                let underlying_type_name = match type_name(
                    machine_type,
                    type_info,
                    type_finder,
                    data.underlying_type,
                    None,
                    None,
                    true,
                ) {
                    Ok(name) => name,
                    Err(e) => {
                        eprintln!("WARNING: failed to get enum type name: {e}");
                        return Ok(());
                    }
                };

                let mut definition = Enum {
                    name: data.name.to_string().to_string(),
                    index: type_index,
                    depth: 0,
                    line,
                    underlying_type_name,
                    is_declaration: false,
                    values: vec![],
                    field_attributes: None,
                };

                if data.properties.forward_reference() {
                    definition.is_declaration = true;
                } else if let Err(e) = definition.add_members(type_finder, data.fields) {
                    eprintln!("WARNING: failed to add enum members: {e}");
                }

                let mut exists = false;

                for member in self.members.iter() {
                    if let ModuleMember::Enum(other_definition) = member {
                        if definition.name == other_definition.name
                            && definition.values.eq(&other_definition.values)
                        {
                            exists = true;
                            break;
                        }
                    }
                }

                if !exists {
                    self.members.push(ModuleMember::Enum(definition));
                }
            }

            Ok(other) => panic!(
                "Unhandled type data in SourceData::add_type_definition - {:?}",
                other
            ),

            Err(err) => panic!(
                "Unhandled error in SourceData::add_type_definition - {}",
                err
            ),
        }

        Ok(())
    }

    pub fn add_build_info(
        &mut self,
        out_path: &std::path::Path,
        id_finder: &pdb::IdFinder,
        build_info: pdb::BuildInfoId,
    ) -> pdb::Result<()> {
        let mut args = vec![];

        for id_index in build_info.arguments {
            let id_item = match id_finder.find(id_index) {
                Ok(id_item) => id_item,
                Err(e) => {
                    eprintln!("WARNING: failed to find id {id_index}: {e}");
                    continue;
                }
            };

            let id_data = match id_item.parse() {
                Ok(pdb::IdData::String(id_data)) => id_data,
                Ok(id_data) => panic!("Failed to parse id {id_index}: Expected String, got {id_data:?}"),
                Err(e) => {
                    eprintln!("WARNING: failed to parse id {id_index}: {e}");
                    continue;
                }
            };

            let mut arg = String::new();

            if let Some(id_index) = id_data.substrings {
                let id_item = match id_finder.find(id_index) {
                    Ok(id_item) => id_item,
                    Err(e) => {
                        eprintln!("WARNING: failed to find id {id_index}: {e}");
                        continue;
                    }
                };

                let id_data = match id_item.parse() {
                    Ok(pdb::IdData::StringList(id_data)) => id_data,

                    Ok(id_data) => panic!("Failed to parse id {id_index}: Expected StringList, got {id_data:?}"),

                    Err(e) => {
                        eprintln!("WARNING: failed to parse id {id_index}: {e}");
                        continue;
                    }
                };

                for type_index in id_data.substrings {
                    // TODO: remove this hack when pdb crate fixes type
                    let id_index = pdb::IdIndex(type_index.0);

                    let id_item = match id_finder.find(id_index) {
                        Ok(id_item) => id_item,
                        Err(e) => {
                            eprintln!("WARNING: failed to find id {id_index}: {e}");
                            continue;
                        }
                    };

                    match id_item.parse() {
                        Ok(pdb::IdData::String(id_data)) => {
                            arg = format!("{}{}", arg, id_data.name.to_string());
                        }

                        Ok(id_data) => panic!("Failed to parse id {id_index}: Expected String, got {id_data:?}"),
                        
                        Err(e) => {
                            eprintln!("WARNING: failed to parse id {id_index}: {e}");
                            continue;
                        }
                    }
                }
            }
            
            arg = format!("{}{}", arg, id_data.name.to_string());

            args.push(arg);
        }

        let mut args_iter = args.iter();

        let root_path = args_iter.next().unwrap();
        let compiler_path = args_iter.next().unwrap();
        let module_path = args_iter.next().unwrap();
        let pdb_path = args_iter.next().unwrap();
        let args_string = args_iter.next().unwrap();

        self.path = crate::canonicalize_file_path(out_path.to_str().unwrap_or(""), root_path.as_str(), module_path.as_str());
        self.compiler_path = compiler_path.into();
        self.pdb_path = pdb_path.into();

        println!("Module arguments: {args_string}");

        let mut chars_iter = args_string.chars();

        let parse_arg_string = |chars_iter: &mut std::str::Chars| -> Option<String> {
            let mut string = String::new();
            let mut quoted = false;

            match chars_iter.next() {
                Some(' ') => (),
                Some('"') => quoted = true,
                Some(c) => string.push(c),
                None => return None,
            }

            while let Some(c) = chars_iter.next() {
                match c {
                    '"' if quoted => match chars_iter.next() {
                        None | Some(' ') => break,
                        Some(c) => panic!("Unexpected character after string: '{c}'"),
                    },
                    ' ' if !quoted => break,
                    _ => string.push(c),
                }
            }

            Some(string)
        };

        let parse_arg_binding = |chars_iter: &mut std::str::Chars| -> Option<(String, Option<String>)> {
            let mut name = String::new();
            let mut quoted = false;

            match chars_iter.next() {
                Some(' ') => (),
                Some('"') => quoted = true,
                Some(c) => name.push(c),
                None => return None,
            }

            loop {
                match chars_iter.next() {
                    None => break,
                    Some(c) => match c {
                        '"' if quoted => (),
                        ' ' if !quoted => break,
                        '=' => return Some((name, parse_arg_string(chars_iter))),
                        _ => name.push(c),
                    }
                }
            }

            Some((name, None))
        };

        loop {
            match chars_iter.next() {
                None => break,
                Some('-' | '/') => (),
                Some(c) => panic!("Unexpected character in build info arg: '{c}'"),
            }

            match chars_iter.next() {
                None => break,

                Some('a') => match parse_arg_string(&mut chars_iter) {
                    Some(s) if s == "nalyze" => self.enable_code_analysis = true,
                    Some(s) if s.starts_with("rch:") => self.minimum_cpu_architecture = Some(s[4..].to_owned()),
                    Some(s) if s == "wait" => self.enable_coroutines = true,
                    Some(s) if s == "wait:strict" => self.enable_coroutines_strict = true,
                    Some(s) => panic!("Unexpected characters in build info arg: 'a{s}...'"),
                    None => panic!("Unexpected character in build info arg: 'a'"),
                }

                Some('A') => match chars_iter.next() {
                    Some('I') => match parse_arg_string(&mut chars_iter) {
                        Some(s) => self.using_directive_dirs.push(crate::canonicalize_dir_path(out_path.to_str().unwrap_or(""), root_path.as_str(), s.as_str())),
                        None => panic!("Missing directory for using directive"),
                    }

                    Some(c) => todo!("arg switch 'A{c}...'"),
                    None => panic!("Unexpected character in build info arg: 'A'"),
                }

                Some('b') => match parse_arg_string(&mut chars_iter) {
                    Some(s) if s == "igobj" => self.bigobj = true,
                    Some(s) => panic!("Unexpected characters in build info arg: 'b{s}...'"),
                    None => panic!("Unexpected characters in build info arg: 'b'"),
                }

                Some('B') => match parse_arg_string(&mut chars_iter) {
                    Some(s) if s == "inl" => self.build_inline = true,
                    Some(s) => panic!("Unexpected characters in build info arg: 'B{s}...'"),
                    None => panic!("Unexpected characters in build info arg: 'B'"),
                }

                Some('c') => match chars_iter.next() {
                    None | Some(' ') => self.compile_without_linking = true,
                    
                    Some('b') => match parse_arg_string(&mut chars_iter) {
                        Some(s) if s == "string" => self.use_byte_strings = true,
                        Some(s) => panic!("Unexpected characters in build info arg: 'cb{s}...'"),
                        None => panic!("Unexpected characters in build info arg: 'cb'"),
                    }

                    Some(c) => todo!("arg switch 'c{c}...'")
                }

                Some('d') => match chars_iter.next() {
                    Some('1') => match parse_arg_string(&mut chars_iter) {
                        Some(s) => self.d1_args.push(s),
                        None => panic!("Unexpected characters in build info arg: 'd1'"),
                    }

                    Some('2') => match parse_arg_string(&mut chars_iter) {
                        Some(s) => self.d2_args.push(s),
                        None => panic!("Unexpected characters in build info arg: 'd2'"),
                    }

                    Some(c) => todo!("arg switch 'd{c}...'"),
                    None => panic!("Unexpected character in build info arg: 'd'"),
                }

                Some('D') => match parse_arg_binding(&mut chars_iter) {
                    Some(x) => self.preprocessor_definitions.push(x),
                    None => panic!("Missing name from preprocessor definition"),
                }

                Some('e') => match parse_arg_string(&mut chars_iter) {
                    Some(s) if s.to_lowercase() == "rrorreport" => self.error_report = Some("".to_string()),
                    Some(s) if s.to_lowercase() == "rrorreport:none" => self.error_report = Some("none".to_string()),
                    Some(s) if s.to_lowercase() == "rrorreport:prompt" => self.error_report = Some("prompt".to_string()),
                    Some(s) if s.to_lowercase() == "rrorreport:queue" => self.error_report = Some("queue".to_string()),
                    Some(s) if s.to_lowercase() == "rrorreport:send" => self.error_report = Some("send".to_string()),
                    Some(_) => todo!(),
                    None => todo!(),
                }

                Some('E') => match chars_iter.next() {
                    Some('H') => match chars_iter.next() {
                        Some('a') => match chars_iter.next() {
                            None | Some(' ') => {
                                self.enable_cpp_exceptions = true;
                                self.enable_seh_exceptions = true;
                            },
                            
                            Some('-') => match chars_iter.next() {
                                None | Some(' ') => {
                                    self.enable_cpp_exceptions = false;
                                    self.enable_seh_exceptions = false;
                                },
                                Some(c) => panic!("Unexpected characters in build info arg: 'EHa-{c}...'"),
                            }

                            Some(c) => panic!("Unexpected characters in build info arg: 'EHa{c}...'"),
                        }

                        Some('c') => match chars_iter.next() {
                            None | Some(' ') => self.extern_c_defaults_to_nothrow = true,
                            
                            Some('-') => match chars_iter.next() {
                                None | Some(' ') => self.extern_c_defaults_to_nothrow = false,
                                Some(c) => panic!("Unexpected characters in build info arg: 'EHc-{c}...'"),
                            }

                            Some(c) => panic!("Unexpected characters in build info arg: 'EHc{c}...'"),
                        }

                        Some('r') => match chars_iter.next() {
                            None | Some(' ') => self.always_generate_noexcept_checks = true,
                            
                            Some('-') => match chars_iter.next() {
                                None | Some(' ') => self.always_generate_noexcept_checks = false,
                                Some(c) => panic!("Unexpected characters in build info arg: 'EHr-{c}...'"),
                            }

                            Some(c) => panic!("Unexpected characters in build info arg: 'EHr{c}...'"),
                        }

                        Some('s') => match chars_iter.next() {
                            None | Some(' ') => {
                                self.enable_cpp_exceptions = true;
                                self.enable_seh_exceptions = false;
                            },
                            
                            Some('-') => match chars_iter.next() {
                                None | Some(' ') => {
                                    self.enable_cpp_exceptions = false;
                                    self.enable_seh_exceptions = false;
                                },
                                Some(c) => panic!("Unexpected characters in build info arg: 'EHs-{c}...'"),
                            }

                            Some(c) => panic!("Unexpected characters in build info arg: 'EHs{c}...'"),
                        }

                        Some(c) => panic!("Unexpected characters in build info arg: 'EH{c}...'"),
                        None => panic!("Unexpected characters in build info arg: 'EH'"),
                    }

                    Some('P') => match chars_iter.next() {
                        None | Some(' ') => self.copy_preprocessor_output_to_stdout = true,
                        Some(c) => panic!("Unexpected characters in build info arg: 'EP{c}...'"),
                    }

                    None => self.copy_preprocessor_output_to_stdout = true,

                    Some(c) => panic!("Unexpected characters in build info arg: 'E{c}...'"),
                }

                Some('f') => match parse_arg_string(&mut chars_iter) {
                    Some(s) if s.starts_with("avor:") => self.optimize_for_cpu_architecture = Some(s[5..].to_owned()),
                    Some(s) if s == "p:contract" => self.consider_floating_point_contractions = true,
                    Some(s) if s == "p:except" => self.consider_floating_point_exceptions = true,
                    Some(s) if s == "p:except-" => self.consider_floating_point_exceptions = false,
                    Some(s) if s == "p:fast" => self.floating_point_model = Some("fast".to_string()),
                    Some(s) if s == "p:precise" => self.floating_point_model = Some("precise".to_string()),
                    Some(s) if s == "p:strict" => self.floating_point_model = Some("strict".to_string()),
                    Some(s) if s == "pcvt:BC" => self.floating_point_conversions = Some("BC".to_string()),
                    Some(s) if s == "pcvt:IA" => self.floating_point_conversions = Some("IA".to_string()),
                    Some(s) => panic!("Unexpected characters in build info arg: 'f{s}...'"),
                    None => panic!("Unexpected character in build info arg: 'f'"),
                }

                Some('F') => match chars_iter.next() {
                    Some('C') => match chars_iter.next() {
                        None | Some(' ') => self.full_source_path_in_diagnostics = true,
                        Some(c) => panic!("Unexpected characters in build info arg: 'FC{c}...'"),
                    }

                    Some('I') => match parse_arg_string(&mut chars_iter) {
                        Some(s) => self.preprocess_include_files.push(s),
                        None => panic!("Missing string from preprocess include file arg"),
                    }

                    Some('p') => match parse_arg_string(&mut chars_iter) {
                        Some(s) => self.pch_file_name = Some(crate::canonicalize_file_path(out_path.to_str().unwrap_or(""), root_path.as_str(), s.as_str())),
                        None => self.pch_file_name = Some(PathBuf::new()),
                    }

                    Some('S') => match chars_iter.next() {
                        Some(' ') | None => self.serialize_pdb_with_mspdbsrv = true,
                        Some(c) => panic!("Unexpected characters in build info arg: 'FS{c}...'"),
                    }

                    Some('U') => match parse_arg_string(&mut chars_iter) {
                        Some(s) => self.forced_using_directives.push(crate::canonicalize_file_path(out_path.to_str().unwrap_or(""), root_path.as_str(), s.as_str())),
                        None => panic!("Unexpected characters in build info arg: 'FU'"),
                    }

                    Some(c) => todo!("arg switch 'F{c}...'"),
                    None => panic!("Unexpected character in build info arg: 'F'"),
                }

                Some('g') => match parse_arg_string(&mut chars_iter) {
                    Some(s) if s.starts_with("uard") => self.guard_string = Some(s.trim_start_matches("uard").to_string()),
                    Some(s) => panic!("Unexpected characters in build info arg: 'g{s}...'"),
                    None => panic!("Unexpected character in build info arg: 'g'"),
                }

                Some('G') => match chars_iter.next() {
                    Some('d') => match chars_iter.next() {
                        None | Some(' ') => self.use_cdecl_calling_convention = true,
                        Some(c) => panic!("Unexpected characters in build info arg: 'Gd{c}...'"),
                    }

                    Some('F') => match chars_iter.next() {
                        None | Some(' ') => self.enable_string_pooling = true,
                        Some('-') => match chars_iter.next() {
                            None | Some(' ') => self.enable_string_pooling = false,
                            Some(c) => panic!("Unexpected characters in build info arg: 'GF-{c}...'"),
                        }
                        Some(c) => panic!("Unexpected characters in build info arg: 'GF{c}...'"),
                    }

                    Some('L') => match chars_iter.next() {
                        None | Some(' ') => self.enable_whole_program_optimization = true,
                        Some('-') => match chars_iter.next() {
                            None | Some(' ') => self.enable_whole_program_optimization = false,
                            Some(c) => panic!("Unexpected characters in build info arg: 'GL-{c}...'"),
                        }
                        Some(c) => panic!("Unexpected characters in build info arg: 'GL{c}...'"),
                    }

                    Some('m') => match chars_iter.next() {
                        None | Some(' ') => self.enable_minimal_rebuild = true,
                        Some('-') => match chars_iter.next() {
                            None | Some(' ') => self.enable_minimal_rebuild = false,
                            Some(c) => panic!("Unexpected characters in build info arg: 'Gm-{c}...'"),
                        }
                        Some(c) => panic!("Unexpected characters in build info arg: 'Gm{c}...'"),
                    }

                    Some('R') => match chars_iter.next() {
                        None | Some(' ') => self.enable_rtti = true,
                        Some('-') => match chars_iter.next() {
                            None | Some(' ') => self.enable_rtti = false,
                            Some(c) => panic!("Unexpected characters in build info arg: 'GR-{c}...'"),
                        }
                        Some(c) => panic!("Unexpected characters in build info arg: 'GR{c}...'"),
                    }

                    Some('S') => match chars_iter.next() {
                        None | Some(' ') => self.check_buffer_security = true,
                        Some('-') => match chars_iter.next() {
                            None | Some(' ') => self.check_buffer_security = false,
                            Some(c) => panic!("Unexpected characters in build info arg: 'GS-{c}...'"),
                        }
                        Some(c) => panic!("Unexpected characters in build info arg: 'GS{c}...'"),
                    }

                    Some('w') => match chars_iter.next() {
                        None | Some(' ') => self.enable_whole_program_data_optimization = true,
                        Some('+') => match chars_iter.next() {
                            None | Some(' ') => self.enable_whole_program_data_optimization = true,
                            Some(c) => panic!("Unexpected characters in build info arg: 'Gw+{c}...'"),
                        }
                        Some('-') => match chars_iter.next() {
                            None | Some(' ') => self.enable_whole_program_data_optimization = false,
                            Some(c) => panic!("Unexpected characters in build info arg: 'Gw-{c}...'"),
                        }
                        Some(c) => panic!("Unexpected characters in build info arg: 'Gw{c}...'"),
                    }

                    Some('y') => match chars_iter.next() {
                        None | Some(' ') => self.function_level_linking = true,
                        Some('-') => match chars_iter.next() {
                            None | Some(' ') => self.function_level_linking = false,
                            Some(c) => panic!("Unexpected characters in build info arg: 'Gy-{c}...'"),
                        }
                        Some(c) => todo!("arg switch 'Gy{c}...'"),
                    }

                    Some(c) => todo!("arg switch 'G{c}...'"),
                    None => panic!("Unexpected character in build info arg: 'G'"),
                }

                Some('k') => match parse_arg_string(&mut chars_iter) {
                    Some(s) if s == "ernel" => self.create_kernel_mode_binary = true,
                    Some(s) => panic!("Unexpected characters in build info arg: 'k{s}...'"),
                    None => panic!("Unexpected character in build info arg: 'k'"),
                }

                Some('I') => match parse_arg_string(&mut chars_iter) {
                    Some(x) => self.additional_include_dirs.push(crate::canonicalize_dir_path(out_path.to_str().unwrap_or(""), root_path.as_str(), x.as_str())),
                    None => panic!("Missing string from additional include directory arg"),
                }

                Some('L') => match chars_iter.next() {
                    Some('D') => match chars_iter.next() {
                        None | Some(' ') => self.feature_toggles.push("LD".to_string()),
                        Some('d') => match chars_iter.next() {
                            None | Some(' ') => self.feature_toggles.push("LDd".to_string()),
                            Some(c) => panic!("Unexpected characters in build info arg: 'LDd{c}...'"),
                        }
                        Some(c) => panic!("Unexpected characters in build info arg: 'LD{c}...'"),
                    }

                    Some(c) => todo!("arg switch 'L{c}...'"),
                    None => panic!("Unexpected character in build info arg: 'L'"),
                }

                Some('M') => match chars_iter.next() {
                    Some('D') => match chars_iter.next() {
                        None | Some(' ') => self.feature_toggles.push("MD".to_string()),
                        Some('d') => match chars_iter.next() {
                            None | Some(' ') => self.feature_toggles.push("MDd".to_string()),
                            Some(c) => panic!("Unexpected characters in build info arg: 'MDd{c}...'"),
                        }
                        Some(c) => panic!("Unexpected characters in build info arg: 'MD{c}...'"),
                    }

                    Some('P') => match chars_iter.next() {
                        None | Some(' ') => self.multiple_process_support = true,
                        Some(c) => panic!("Unexpected characters in build info arg: 'MP{c}...'"),
                    }

                    Some('T') => match chars_iter.next() {
                        None | Some(' ') => self.feature_toggles.push("MT".to_string()),
                        Some('d') => match chars_iter.next() {
                            None | Some(' ') => self.feature_toggles.push("MTd".to_string()),
                            Some(c) => panic!("Unexpected characters in build info arg: 'MTd{c}...'"),
                        }
                        Some(c) => panic!("Unexpected characters in build info arg: 'MT{c}...'"),
                    }

                    Some(c) => todo!("arg switch 'M{c}...'"),
                    None => panic!("Unexpected character in build info arg: 'M'"),
                }

                Some('n') => match parse_arg_string(&mut chars_iter) {
                    Some(s) if s == "ologo" => self.nologo = true,
                    Some(s) if s == "othreadsafestatics" => self.no_thread_safe_statics = true,
                    Some(s) => panic!("Unexpected characters in build info arg: 'n{s}...'"),
                    None => panic!("Unexpected character in build info arg: 'n'"),
                }

                Some('o') => match parse_arg_string(&mut chars_iter) {
                    Some(s) if s == "penmp" => self.openmp = Some("omp".to_string()),
                    Some(s) if s == "penmp-" => self.openmp = None,
                    Some(s) if s == "penmp:experimental" => self.openmp = Some("experimental".to_string()),
                    Some(s) if s == "penmp:llvm" => self.openmp = Some("llvm".to_string()),
                    Some(s) if s == "ptions:strict" => self.unknown_compiler_options_are_errors = true,
                    Some(s) => panic!("Unexpected characters in build info arg: 'o{s}...'"),
                    None => panic!("Unexpected character in build info arg: 'o'"),
                }

                Some('O') => match chars_iter.next() {
                    Some('1') => match chars_iter.next() {
                        None | Some(' ') => self.create_small_code = true,
                        Some(c) => panic!("Unexpected characters in build info arg: 'O1{c}...'"),
                    }

                    Some('2') => match chars_iter.next() {
                        None | Some(' ') => self.create_fast_code = true,
                        Some(c) => panic!("Unexpected characters in build info arg: 'O2{c}...'"),
                    }

                    Some('b') => match chars_iter.next() {
                        Some('0') => match chars_iter.next() {
                            None | Some(' ') => self.inline_function_expansion = Some(0),
                            Some(c) => panic!("Unexpected characters in build info arg: 'Ob0{c}...'"),
                        }
                        
                        Some('1') => match chars_iter.next() {
                            None | Some(' ') => self.inline_function_expansion = Some(1),
                            Some(c) => panic!("Unexpected characters in build info arg: 'Ob1{c}...'"),
                        }

                        Some('2') => match chars_iter.next() {
                            None | Some(' ') => self.inline_function_expansion = Some(2),
                            Some(c) => panic!("Unexpected characters in build info arg: 'Ob2{c}...'"),
                        }
                        
                        Some('3') => match chars_iter.next() {
                            None | Some(' ') => self.inline_function_expansion = Some(3),
                            Some(c) => panic!("Unexpected characters in build info arg: 'Ob3{c}...'"),
                        }
                        
                        Some(c) => panic!("Unexpected characters in build info arg: 'Ob{c}...'"),
                        None => panic!("Unexpected characters in build info arg: 'Ob'"),
                    }

                    Some('d') => match chars_iter.next() {
                        None | Some(' ') => self.optimization_disabled = true,
                        Some(c) => panic!("Unexpected characters in build info arg: 'Od{c}...'"),
                    }

                    Some('i') => match chars_iter.next() {
                        None | Some(' ') => self.generate_intrinsic_functions = true,
                        Some('-') => match chars_iter.next() {
                            None | Some(' ') => self.generate_intrinsic_functions = false,
                            Some(c) => panic!("Unexpected characters in build info arg: 'Oi-{c}...'"),
                        }
                        Some(c) => panic!("Unexpected characters in build info arg: 'Oi{c}...'"),
                    }

                    Some('s') => match chars_iter.next() {
                        None | Some(' ') => self.favors_small_code = true,
                        Some(c) => panic!("Unexpected characters in build info arg: 'Os{c}...'"),
                    }

                    Some('t') => match chars_iter.next() {
                        None | Some(' ') => self.favors_fast_code = true,
                        Some(c) => panic!("Unexpected characters in build info arg: 'Ot{c}...'"),
                    }

                    Some('x') => match chars_iter.next() {
                        None | Some(' ') => {
                            // TODO: O2 without GF or Gy
                        }
                        Some('-') => match chars_iter.next() {
                            None | Some(' ') => {
                                // TODO: O2 without GF or Gy
                            }
                            Some(c) => panic!("Unexpected characters in build info arg: 'Ox-{c}...'"),
                        }
                        Some(c) => panic!("Unexpected characters in build info arg: 'Ox{c}...'"),
                    }

                    Some('y') => match chars_iter.next() {
                        None | Some(' ') => self.frame_pointer_emission = true,
                        Some('-') => match chars_iter.next() {
                            None | Some(' ') => self.frame_pointer_emission = false,
                            Some(c) => panic!("Unexpected characters in build info arg: 'Oy-{c}...'"),
                        }
                        Some(c) => panic!("Unexpected characters in build info arg: 'Oy{c}...'"),
                    }

                    Some(c) => todo!("arg switch 'O{c}...'"),
                    None => panic!("Unexpected character in build info arg: 'O'"),
                }

                Some('Q') => match parse_arg_string(&mut chars_iter) {
                    Some(s) if s == "par" => self.enable_loop_parallelization = true,
                    Some(s) => panic!("Unexpected characters in build info arg: 'Q{s}...'"),
                    None => panic!("Unexpected character in build info arg: 'Q'"),
                }

                Some('R') => match parse_arg_string(&mut chars_iter) {
                    Some(s) if s == "TC1" || s == "TCsu" => self.enable_fast_runtime_checks = true,
                    Some(s) if s == "TCc" => self.convert_to_smaller_type_check_at_runtime = true,
                    Some(s) if s == "TCs" => self.enable_stack_frame_runtime_checks = true,
                    Some(s) if s == "TCu" => self.enable_uninitialized_local_usage_checks = true,
                    Some(s) => panic!("Unexpected characters in build info arg: 'R{s}...'"),
                    None => panic!("Unexpected characters in build info arg: 'R'"),
                }

                Some('s') => match chars_iter.next() {
                    Some('d') => match parse_arg_string(&mut chars_iter) {
                        Some(s) if s == "l" => self.enable_sdl = true,
                        Some(s) if s == "l-" => self.enable_sdl = false,
                        Some(s) => panic!("Unexpected characters in build info arg: 'sd{s}...'"),
                        None => panic!("Unexpected characters in build info arg: 'sd'"),
                    }

                    Some(c) => todo!("arg switch 's{c}...'"),
                    None => panic!("Unexpected character in build info arg: 's'"),
                }

                Some('T') => match chars_iter.next() {
                    Some('C') => match chars_iter.next() {
                        None | Some(' ') => self.use_c_source_file_type = true,
                        Some(c) => panic!("Unexpected characters in build info arg: 'TC{c}...'"),
                    }

                    Some('P') => match chars_iter.next() {
                        None | Some(' ') => self.use_cpp_source_file_type = true,
                        Some(c) => panic!("Unexpected characters in build info arg: 'TP{c}...'"),
                    }

                    Some(c) => todo!("arg switch 'T{c}...'"),
                    None => panic!("Unexpected character in build info arg: 'T'"),
                }

                Some('w') => match chars_iter.next() {
                    Some('1') => self.warnings_level1.push(parse_arg_string(&mut chars_iter).unwrap_or("".into())),
                    Some('2') => self.warnings_level1.push(parse_arg_string(&mut chars_iter).unwrap_or("".into())),
                    Some('3') => self.warnings_level1.push(parse_arg_string(&mut chars_iter).unwrap_or("".into())),
                    Some('4') => self.warnings_level1.push(parse_arg_string(&mut chars_iter).unwrap_or("".into())),

                    Some('d') => match parse_arg_string(&mut chars_iter) {
                        Some(s) => self.disabled_warnings.push(s),
                        None => panic!("Unexpected characters in build info arg: 'wd'"),
                    }

                    Some('e') => match parse_arg_string(&mut chars_iter) {
                        Some(s) => self.warnings_as_errors.push(s),
                        None => panic!("Unexpected characters in build info arg: 'we'"),
                    }

                    Some(s) => todo!("arg switch 'w{s}...'"),
                    None => panic!("Missing warning number to disable"),
                }

                Some('W') => match chars_iter.next() {
                    Some('0') => match chars_iter.next() {
                        None | Some(' ') => self.output_warning_level = Some(0),
                        Some(c) => panic!("Unexpected characters in build info arg: 'W0{c}...'"),
                    }

                    Some('1') => match chars_iter.next() {
                        None | Some(' ') => self.output_warning_level = Some(1),
                        Some(c) => panic!("Unexpected characters in build info arg: 'W1{c}...'"),
                    }

                    Some('2') => match chars_iter.next() {
                        None | Some(' ') => self.output_warning_level = Some(2),
                        Some(c) => panic!("Unexpected characters in build info arg: 'W2{c}...'"),
                    }

                    Some('3') => match chars_iter.next() {
                        None | Some(' ') => self.output_warning_level = Some(3),
                        Some(c) => panic!("Unexpected characters in build info arg: 'W3{c}...'"),
                    }

                    Some('4') => match chars_iter.next() {
                        None | Some(' ') => self.output_warning_level = Some(4),
                        Some(c) => panic!("Unexpected characters in build info arg: 'W4{c}...'"),
                    }

                    Some('a') => match parse_arg_string(&mut chars_iter) {
                        Some(s) if s == "ll" => self.enable_all_warnings = true,
                        x => panic!("Unexpected characters in build info arg: 'Wa{}'", x.unwrap_or(String::new())),
                    }

                    Some('v') => match parse_arg_string(&mut chars_iter) {
                        Some(s) if s.starts_with(":") => self.disable_warnings_after_version = Some(s.to_string()),
                        Some(s) => panic!("Unexpected characters in build info arg: 'Wv{s}...'"),
                        None => panic!("Unexpected characters in build info arg: 'Wv'"),
                    }

                    Some('X') => match chars_iter.next() {
                        None | Some(' ') => self.treat_warnings_as_errors = true,
                        Some('-') => match chars_iter.next() {
                            None | Some(' ') => self.treat_warnings_as_errors = false,
                            Some(c) => panic!("Unexpected characters in build info arg: 'WX-{c}...'"),
                        }
                        Some(c) => panic!("Unexpected characters in build info arg: 'WX{c}...'"),
                    }

                    Some(s) => todo!("arg switch 'W{s}...'"),
                    None => panic!("Unexpected character in build info arg: 'W'"),
                }

                Some('X') => match chars_iter.next() {
                    None | Some(' ') => self.ignore_standard_include_dir = true,
                    Some(c) => panic!("Unexpected characters in build info arg: 'X{c}...'"),
                }

                Some('Y') => match chars_iter.next() {
                    Some('c') => match parse_arg_string(&mut chars_iter) {
                        Some(s) => self.pch_file_name = Some(crate::canonicalize_file_path(out_path.to_str().unwrap_or(""), root_path.as_str(), s.as_str())),
                        None => self.pch_file_name = Some(PathBuf::new()),
                    }

                    Some('l') => match parse_arg_string(&mut chars_iter) {
                        None => self.inject_pch_reference = true,
                        Some(s) if s == "" => self.inject_pch_reference = true,
                        Some(s) if s == "-" => self.inject_pch_reference = false,
                        Some(s) => self.pch_references.push(s),
                    }

                    Some('u') => match parse_arg_string(&mut chars_iter) {
                        Some(s) => self.precompiled_header_file_name = Some(s),
                        None => panic!("Unexpected characters in build info arg: 'Yu'"),
                    }

                    Some(c) => todo!("arg switch 'Y{c}...'"),
                    None => panic!("Unexpected character in build info arg: 'Y'"),
                }

                Some('Z') => match chars_iter.next() {
                    Some('7') => match chars_iter.next() {
                        None | Some(' ') => self.generate_c7_debug_info = true,
                        Some(c) => panic!("Unexpected characters in build info arg: 'Z7{c}...'"),
                    }

                    Some('c') => match chars_iter.next() {
                        Some(':') => match parse_arg_string(&mut chars_iter) {
                            Some(x) => self.feature_toggles.push(x),
                            None => panic!("Missing identifier from feature toggle"),
                        }

                        Some(c) => todo!("arg switch 'Zc{c}...'"),
                        None => panic!("Unexpected characters in build info arg: 'Zc'"),
                    }

                    Some('H') => match parse_arg_string(&mut chars_iter) {
                        Some(s) if s.starts_with(":") => self.hash_algorithm = Some(s.trim_start_matches(":").to_string()),
                        Some(s) => panic!("Unexpected characters in build info arg: 'ZH{s}...'"),
                        None => panic!("Unexpected characters in build info arg: 'ZH'"),
                    }

                    Some('i') => match chars_iter.next() {
                        None | Some(' ') => self.generate_full_debug_info = true,
                        Some('+') => match chars_iter.next() {
                            None | Some(' ') => self.generate_full_debug_info = true,
                            Some(c) => panic!("Unexpected characters in build info arg: 'Zi+{c}...'"),
                        }
                        Some('-') => match chars_iter.next() {
                            None | Some(' ') => self.generate_full_debug_info = false,
                            Some(c) => panic!("Unexpected characters in build info arg: 'Zi-{c}...'"),
                        }
                        Some(c) => panic!("Unexpected characters in build info arg: 'Zi{c}...'"),
                    }

                    Some('l') => match chars_iter.next() {
                        None | Some(' ') => self.remove_default_library_name = true,
                        Some(c) => todo!("arg switch 'Zl{c}...'"),
                    }

                    Some('o') => match chars_iter.next() {
                        None | Some(' ') => self.generate_richer_debug_info_for_optimized_code = true,
                        Some('-') => match chars_iter.next() {
                            None | Some(' ') => self.generate_richer_debug_info_for_optimized_code = false,
                            Some(c) => panic!("Unexpected characters in build info arg: 'Zo-{c}...'"),
                        }
                        Some(c) => panic!("Unexpected characters in build info arg: 'Zo{c}...'"),
                    }

                    Some('p') => match parse_arg_string(&mut chars_iter) {
                        Some(x) => self.pack_structure_members = Some(x),
                        None => panic!("Missing identifier from feature toggle"),
                    }

                    Some('W') => match chars_iter.next() {
                        None | Some(' ') => self.windows_runtime_compilation = true,
                        
                        Some(':') => match parse_arg_string(&mut chars_iter) {
                            Some(s) if s == "nostdlib" => self.windows_runtime_compilation_nostdlib = true,
                            Some(s) => panic!("Unexpected characters in build info arg: 'ZW:{s}...'"),
                            None => panic!("Unexpected characters in build info arg: 'ZW:'"),
                        }

                        Some(c) => panic!("Unexpected characters in build info arg: 'ZW{c}...'"),
                    }

                    Some(c) => todo!("arg switch 'Z{c}...'"),
                    None => panic!("Unexpected character in build info arg: 'Z'"),
                }

                Some(c) => panic!("Unexpected character in build info arg: '{c}...'"),
            }
        }

        // Sort additional include directories from longest to shortest
        self.additional_include_dirs.sort_by(|a, b| a.to_string_lossy().len().cmp(&b.to_string_lossy().len()));
        self.additional_include_dirs.reverse();

        Ok(())
    }
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut storage: Vec<(u32, ModuleMember)> = vec![];
        let mut prev_line = 0;

        let mut is_header = false;

        match self.path.extension().and_then(std::ffi::OsStr::to_str) {
            Some(ext) if SOURCE_FILE_EXTS.contains(&ext) => (),
            _ => is_header = true,
        }

        if is_header {
            writeln!(f, "#pragma once")?;
        }

        for (header, is_global) in self.headers.iter() {
            if *is_global {
                writeln!(f, "#include <{}>", header.to_string_lossy().trim_start_matches("/"))?;
            } else {
                writeln!(f, "#include \"{}\"", header.to_string_lossy().trim_start_matches("/"))?;
            }
        }

        for u in &self.members {
            match u {
                ModuleMember::Class(x) => {
                    storage.push((x.line, u.clone()));
                    prev_line = x.line;
                }

                ModuleMember::Enum(x) => {
                    storage.push((x.line, u.clone()));
                    prev_line = x.line;
                }

                ModuleMember::Data(_, _, Some(line)) => {
                    storage.push((line.clone(), u.clone()));
                    prev_line = line.clone();
                }

                ModuleMember::ThreadStorage(_, _, Some(line)) => {
                    storage.push((line.clone(), u.clone()));
                    prev_line = line.clone();
                }

                ModuleMember::Procedure(_, _, Some(line)) => {
                    storage.push((line.clone(), u.clone()));
                    prev_line = line.clone();
                }

                _ => {
                    prev_line += 1;
                    storage.push((prev_line, u.clone()));
                }
            }
        }

        storage.sort_by(|a, b| a.0.cmp(&b.0));

        for item in storage {
            writeln!(f)?;
            item.1.fmt(f)?;
        }

        Ok(())
    }
}
