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

#[repr(u64)]
pub enum ModuleFlags {
    CompileWithoutLinking = 1 << 0,
    RemoveDefaultLibraryName = 1 << 1,
    FunctionLevelLinking = 1 << 2,
    EnableAllWarnings = 1 << 3,
    TreatWarningsAsErrors = 1 << 4,
    CopyPreprocessorOutputToStdout = 1 << 5,
    EnableCppExceptions = 1 << 6,
    EnableSehExceptions = 1 << 7,
    ExternCDefaultsToNothrow = 1 << 8,
    AlwaysGenerateNoexceptChecks = 1 << 9,
    EnableRtti = 1 << 10,
    EnableStringPooling = 1 << 11,
    CheckBufferSecurity = 1 << 12,
    EnableWholeProgramOptimization = 1 << 13,
    EnableWholeProgramDataOptimization = 1 << 14,
    OptimizationDisabled = 1 << 15,
    FavorsSmallCode = 1 << 16,
    FavorsFastCode = 1 << 17,
    GenerateC7DebugInfo = 1 << 18,
    GenerateFullDebugInfo = 1 << 19,
    GenerateRicherDebugInfoForOptimizedCode = 1 << 20,
    InjectPchReference = 1 << 21,
    IgnoreStandardIncludeDir = 1 << 22,
    Nologo = 1 << 23,
    NoThreadSafeStatics = 1 << 24,
    EnableSdl = 1 << 25,
    CreateSmallCode = 1 << 26,
    CreateFastCode = 1 << 27,
    GenerateIntrinsicFunctions = 1 << 28,
    EnableMinimalRebuild = 1 << 29,
    EnableCodeAnalysis = 1 << 30,
    EnableCoroutines = 1 << 31,
    EnableCoroutinesStrict = 1 << 32,
    ConsiderFloatingPointContractions = 1 << 33,
    ConsiderFloatingPointExceptions = 1 << 34,
    UseCdeclCallingConvention = 1 << 35,
    WindowsRuntimeCompilation = 1 << 36,
    WindowsRuntimeCompilationNostdlib = 1 << 37,
    FramePointerEmission = 1 << 38,
    FullSourcePathInDiagnostics = 1 << 39,
    UseByteStrings = 1 << 40,
    PreserveCommentsDuringPreprocessing = 1 << 41,
    CreateKernelModeBinary = 1 << 42,
    UseCSourceFileType = 1 << 43,
    UseCppSourceFileType = 1 << 44,
    EnableLoopParallelization = 1 << 45,
    EnableFastRuntimeChecks = 1 << 46,
    ConvertToSmallerTypeCheckAtRuntime = 1 << 47,
    EnableStackFrameRuntimeChecks = 1 << 48,
    EnableUninitializedLocalUsageChecks = 1 << 49,
    Bigobj = 1 << 50,
    BuildInline = 1 << 51,
    SerializePdbWithMspdbsrv = 1 << 52,
    UnknownCompilerOptionsAreErrors = 1 << 53,
    ReproducableOutput = 1 << 54,
    MitigateSpectreVulnerabilities = 1 << 55,
    Permissive = 1 << 56,
    ExternalAngleBracketsHeaders = 1 << 57,
    FasterPdbGeneration = 1 << 58,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Module {
    pub path: PathBuf,
    pub compiler_path: PathBuf,
    pub pdb_path: PathBuf,
    pub flags: u64,

    pub headers: Vec<(PathBuf, bool)>,
    pub members: Vec<ModuleMember>,

    pub compiler_response_file: Option<PathBuf>,
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
    pub language_standard: Option<String>,
    pub diagnostics: Option<String>,
    pub external_headers: Vec<String>,
    pub external_headers_var: Option<String>,
    pub external_headers_warning_level: Option<usize>,
    pub fid_file: Option<String>,
    pub inline_function_expansion: Option<usize>,
    pub code_generation_threads: Option<usize>,
    pub build_process_count: Option<usize>,

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
}

impl Module {
    pub fn with_path(mut self, path: PathBuf) -> Self {
        self.path = path;
        self
    }

    pub fn set_flag(&mut self, flag: ModuleFlags, on: bool) {
        if on {
            self.flags |= flag as u64;
        } else {
            self.flags ^= flag as u64;
        }
    }

    pub fn test_flag(&self, flag: ModuleFlags) -> bool {
        (self.flags & (flag as u64)) != 0
    }

    pub fn add_type_definition(
        &mut self,
        machine_type: pdb::MachineType,
        type_info: &pdb::TypeInformation,
        type_finder: &pdb::TypeFinder,
        type_index: pdb::TypeIndex,
        line: u32,
    ) -> pdb::Result<()> {
        if self.members.iter().any(|x| match x {
            ModuleMember::Class(c) => c.index == type_index,
            ModuleMember::Enum(e) => e.index == type_index,
            _ => false,
        }) {
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

        self.path = crate::canonicalize_path(out_path.to_str().unwrap_or(""), root_path.as_str(), module_path.as_str(), false);
        self.compiler_path = compiler_path.into();
        self.pdb_path = pdb_path.into();

        // println!("Module arguments: {args_string}");

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
                Some(c) => panic!("Unexpected character in build info arg at character {}: '{c}'", args_string.len() - chars_iter.size_hint().0),
            }

            match chars_iter.next() {
                None => break,

                Some('@') => match parse_arg_string(&mut chars_iter) {
                    Some(s) => self.compiler_response_file = Some(crate::canonicalize_path(out_path.to_str().unwrap_or(""), root_path.as_str(), s.as_str(), false)),
                    None => panic!("Unexpected character in build info arg: '@'"),
                }

                Some('a') => match parse_arg_string(&mut chars_iter) {
                    Some(s) if s == "nalyze" => self.set_flag(ModuleFlags::EnableCodeAnalysis, true),
                    Some(s) if s == "nalyze-" => self.set_flag(ModuleFlags::EnableCodeAnalysis, false),
                    Some(s) if s.starts_with("rch:") => self.minimum_cpu_architecture = Some(s[4..].to_owned()),
                    Some(s) if s == "wait" => self.set_flag(ModuleFlags::EnableCoroutines, true),
                    Some(s) if s == "wait:strict" => self.set_flag(ModuleFlags::EnableCoroutinesStrict, true),
                    Some(s) => panic!("Unhandled characters in build info arg: 'a{s}'"),
                    None => panic!("Unexpected character in build info arg: 'a'"),
                }

                Some('A') => match chars_iter.next() {
                    Some('I') => match parse_arg_string(&mut chars_iter) {
                        Some(s) => self.using_directive_dirs.push(crate::canonicalize_path(out_path.to_str().unwrap_or(""), root_path.as_str(), s.as_str(), true)),
                        None => panic!("Missing directory for using directive"),
                    }

                    Some(c) => todo!("arg switch 'A{c}...'"),
                    None => panic!("Unexpected character in build info arg: 'A'"),
                }

                Some('b') => match parse_arg_string(&mut chars_iter) {
                    Some(s) if s == "igobj" => self.set_flag(ModuleFlags::Bigobj, true),
                    Some(s) => panic!("Unhandled characters in build info arg: 'b{s}'"),
                    None => panic!("Unhandled characters in build info arg: 'b'"),
                }

                Some('B') => match parse_arg_string(&mut chars_iter) {
                    Some(s) if s == "inl" => self.set_flag(ModuleFlags::BuildInline, true),
                    Some(s) if s == "repro" => self.set_flag(ModuleFlags::ReproducableOutput, true),
                    Some(s) => panic!("Unhandled characters in build info arg: 'B{s}'"),
                    None => panic!("Unhandled characters in build info arg: 'B'"),
                }

                Some('c') => match chars_iter.next() {
                    None | Some('-') | Some(' ') => self.set_flag(ModuleFlags::CompileWithoutLinking, true),
                    Some('b') => match parse_arg_string(&mut chars_iter) {
                        Some(s) if s == "string" => self.set_flag(ModuleFlags::UseByteStrings, true),
                        None => panic!("Unhandled characters in build info arg: 'cb'"),
                        Some(s) => panic!("Unhandled characters in build info arg: 'cb{s}'"),
                    }
                    Some('g') => match parse_arg_string(&mut chars_iter) {
                        Some(s) if s.starts_with("threads") => self.code_generation_threads = Some(s.trim_start_matches("threads").parse().expect("Invalid code generation thread count")),
                        None => panic!("Unhandled characters in build info arg: 'cg'"),
                        Some(s) => panic!("Unhandled characters in build info arg: 'cg{s}'"),
                    }
                    Some(c) => panic!("Unhandled characters in build info arg: 'c{c}...'"),
                }

                Some('C') => match chars_iter.next() {
                    None | Some(' ') => self.set_flag(ModuleFlags::PreserveCommentsDuringPreprocessing, true),
                    Some(c) => panic!("Unhandled characters in build info arg: 'C{c}...'"),
                }

                Some('d') => match chars_iter.next() {
                    Some('1') => match parse_arg_string(&mut chars_iter) {
                        Some(s) => self.d1_args.push(s),
                        None => panic!("Unhandled characters in build info arg: 'd1'"),
                    }

                    Some('2') => match parse_arg_string(&mut chars_iter) {
                        Some(s) => self.d2_args.push(s),
                        None => panic!("Unhandled characters in build info arg: 'd2'"),
                    }

                    Some('i') => match parse_arg_string(&mut chars_iter) {
                        Some(s) if s.starts_with("agnostics:") => self.diagnostics = Some(s.trim_start_matches("agnostics:").to_string()),
                        Some(s) => panic!("Unhandled characters in build info arg: 'di{s}'"),
                        None => panic!("Unhandled characters in build info arg: 'di'"),
                    }

                    Some(c) => todo!("arg switch 'd{c}...'"),
                    None => panic!("Unexpected character in build info arg: 'd'"),
                }

                Some('D') => match parse_arg_binding(&mut chars_iter) {
                    Some(x) => self.preprocessor_definitions.push(x),
                    None => panic!("Missing name from preprocessor definition"),
                }

                Some('e') => match chars_iter.next() {
                    Some('r') => match parse_arg_string(&mut chars_iter) {
                        Some(s) if s.to_lowercase() == "rorreport" => self.error_report = Some("".to_string()),
                        Some(s) if s.to_lowercase() == "rorreport:none" => self.error_report = Some("none".to_string()),
                        Some(s) if s.to_lowercase() == "rorreport:prompt" => self.error_report = Some("prompt".to_string()),
                        Some(s) if s.to_lowercase() == "rorreport:queue" => self.error_report = Some("queue".to_string()),
                        Some(s) if s.to_lowercase() == "rorreport:send" => self.error_report = Some("send".to_string()),
                        Some(s) => panic!("Unhandled characters in build info arg: 'er{s}'"),
                        None => panic!("Unhandled characters in build info arg: 'er'"),
                    }
                    Some('x') => match chars_iter.by_ref().take_while(|c| *c != ':').collect::<String>().as_str() {
                        "ternal" => match chars_iter.next() {
                            Some('a') => match parse_arg_string(&mut chars_iter) {
                                Some(s) if s == "nglebrackets" => self.set_flag(ModuleFlags::ExternalAngleBracketsHeaders, true),
                                Some(s) => panic!("Unhandled characters in build info arg: 'external:a{s}"),
                                None => panic!("Unhandled characters in build info arg: 'external:a"),
                            }
                            Some('e') => match chars_iter.by_ref().take_while(|c| *c != ':').collect::<String>().as_str() {
                                "nv" => match parse_arg_string(&mut chars_iter) {
                                    Some(s) => self.external_headers_var = Some(s.to_string()),
                                    None => panic!("Unhandled characters in build info arg: 'external:env:'"),
                                }
                                s => panic!("Unhandled characters in build info arg: 'external:e{s}"),
                            }
                            Some('I') => match parse_arg_string(&mut chars_iter) {
                                Some(s) => self.external_headers.push(s),
                                None => panic!("Unhandled characters in build info arg: 'external:I'"),
                            }
                            Some('W') => match parse_arg_string(&mut chars_iter) {
                                Some(s) if s == "0" => self.external_headers_warning_level = Some(0),
                                Some(s) if s == "1" => self.external_headers_warning_level = Some(1),
                                Some(s) if s == "2" => self.external_headers_warning_level = Some(2),
                                Some(s) if s == "3" => self.external_headers_warning_level = Some(3),
                                Some(s) if s == "4" => self.external_headers_warning_level = Some(4),
                                Some(s) => panic!("Unhandled characters in build info arg: 'external:W{s}'"),
                                None => panic!("Unhandled characters in build info arg: 'external:W'"),
                            }
                            Some(c) => panic!("Unhandled characters in build info arg: 'external:{c}'"),
                            None => panic!("Unhandled characters in build info arg: 'external:'"),
                        }
                        s => panic!("Unhandled characters in build info arg: 'ex{s}'"),
                    }
                    Some(c) => panic!("Unhandled characters in build info arg: 'e{c}'"),
                    None => panic!("Unhandled characters in build info arg: 'e'"),
                }

                Some('E') => match chars_iter.next() {
                    Some('H') => match chars_iter.next() {
                        Some('a') => match chars_iter.next() {
                            None | Some(' ') => {
                                self.set_flag(ModuleFlags::EnableCppExceptions, true);
                                self.set_flag(ModuleFlags::EnableSehExceptions, true);
                            },
                            
                            Some('-') => match chars_iter.next() {
                                None | Some(' ') => {
                                    self.set_flag(ModuleFlags::EnableCppExceptions, false);
                                    self.set_flag(ModuleFlags::EnableSehExceptions, false);
                                },
                                Some(c) => panic!("Unhandled characters in build info arg: 'EHa-{c}...'"),
                            }

                            Some(c) => panic!("Unhandled characters in build info arg: 'EHa{c}...'"),
                        }

                        Some('c') => match chars_iter.next() {
                            None | Some(' ') => self.set_flag(ModuleFlags::ExternCDefaultsToNothrow, true),
                            
                            Some('-') => match chars_iter.next() {
                                None | Some(' ') => self.set_flag(ModuleFlags::ExternCDefaultsToNothrow, false),
                                Some(c) => panic!("Unhandled characters in build info arg: 'EHc-{c}...'"),
                            }

                            Some(c) => panic!("Unhandled characters in build info arg: 'EHc{c}...'"),
                        }

                        Some('r') => match chars_iter.next() {
                            None | Some(' ') => self.set_flag(ModuleFlags::AlwaysGenerateNoexceptChecks, true),
                            
                            Some('-') => match chars_iter.next() {
                                None | Some(' ') => self.set_flag(ModuleFlags::AlwaysGenerateNoexceptChecks, false),
                                Some(c) => panic!("Unhandled characters in build info arg: 'EHr-{c}...'"),
                            }

                            Some(c) => panic!("Unhandled characters in build info arg: 'EHr{c}...'"),
                        }

                        Some('s') => match chars_iter.next() {
                            None | Some(' ') => {
                                self.set_flag(ModuleFlags::EnableCppExceptions, true);
                                self.set_flag(ModuleFlags::EnableSehExceptions, false);
                            },
                            
                            Some('-') => match chars_iter.next() {
                                None | Some(' ') => {
                                    self.set_flag(ModuleFlags::EnableCppExceptions, false);
                                    self.set_flag(ModuleFlags::EnableSehExceptions, false);
                                },
                                Some(c) => panic!("Unhandled characters in build info arg: 'EHs-{c}...'"),
                            }

                            Some(c) => panic!("Unhandled characters in build info arg: 'EHs{c}...'"),
                        }

                        Some(c) => panic!("Unhandled characters in build info arg: 'EH{c}...'"),
                        None => panic!("Unhandled characters in build info arg: 'EH'"),
                    }

                    Some('P') => match chars_iter.next() {
                        None | Some(' ') => self.set_flag(ModuleFlags::CopyPreprocessorOutputToStdout, true),
                        Some(c) => panic!("Unhandled characters in build info arg: 'EP{c}...'"),
                    }

                    None => self.set_flag(ModuleFlags::CopyPreprocessorOutputToStdout, true),

                    Some(c) => panic!("Unhandled characters in build info arg: 'E{c}...'"),
                }

                Some('f') => match parse_arg_string(&mut chars_iter) {
                    Some(s) if s.starts_with("avor:") => self.optimize_for_cpu_architecture = Some(s[5..].to_owned()),
                    Some(s) if s == "p:contract" => self.set_flag(ModuleFlags::ConsiderFloatingPointContractions, true),
                    Some(s) if s == "p:except" => self.set_flag(ModuleFlags::ConsiderFloatingPointExceptions, true),
                    Some(s) if s == "p:except-" => self.set_flag(ModuleFlags::ConsiderFloatingPointExceptions, false),
                    Some(s) if s == "p:fast" => self.floating_point_model = Some("fast".to_string()),
                    Some(s) if s == "p:precise" => self.floating_point_model = Some("precise".to_string()),
                    Some(s) if s == "p:strict" => self.floating_point_model = Some("strict".to_string()),
                    Some(s) if s == "pcvt:BC" => self.floating_point_conversions = Some("BC".to_string()),
                    Some(s) if s == "pcvt:IA" => self.floating_point_conversions = Some("IA".to_string()),
                    Some(s) => panic!("Unhandled characters in build info arg: 'f{s}'"),
                    None => panic!("Unexpected character in build info arg: 'f'"),
                }

                Some('F') => match chars_iter.next() {
                    Some('C') => match chars_iter.next() {
                        None | Some(' ') => self.set_flag(ModuleFlags::FullSourcePathInDiagnostics, true),
                        Some(c) => panic!("Unhandled characters in build info arg: 'FC{c}...'"),
                    }

                    Some('I') => match parse_arg_string(&mut chars_iter) {
                        Some(s) => self.preprocess_include_files.push(s),
                        None => panic!("Missing string from preprocess include file arg"),
                    }

                    Some('p') => match parse_arg_string(&mut chars_iter) {
                        Some(s) => self.pch_file_name = Some(crate::canonicalize_path(out_path.to_str().unwrap_or(""), root_path.as_str(), s.as_str(), false)),
                        None => self.pch_file_name = Some(PathBuf::new()),
                    }

                    Some('S') => match chars_iter.next() {
                        Some(' ') | None => self.set_flag(ModuleFlags::SerializePdbWithMspdbsrv, true),
                        Some(c) => panic!("Unhandled characters in build info arg: 'FS{c}...'"),
                    }

                    Some('U') => match parse_arg_string(&mut chars_iter) {
                        Some(s) => self.forced_using_directives.push(crate::canonicalize_path(out_path.to_str().unwrap_or(""), root_path.as_str(), s.as_str(), false)),
                        None => panic!("Unhandled characters in build info arg: 'FU'"),
                    }

                    Some(c) => todo!("arg switch 'F{c}...'"),
                    None => panic!("Unexpected character in build info arg: 'F'"),
                }

                Some('g') => match parse_arg_string(&mut chars_iter) {
                    Some(s) if s.starts_with("uard") => self.guard_string = Some(s.trim_start_matches("uard").to_string()),
                    Some(s) => panic!("Unhandled characters in build info arg: 'g{s}'"),
                    None => panic!("Unexpected character in build info arg: 'g'"),
                }

                Some('G') => match chars_iter.next() {
                    Some('d') => match chars_iter.next() {
                        None | Some(' ') => self.set_flag(ModuleFlags::UseCdeclCallingConvention, true),
                        Some(c) => panic!("Unhandled characters in build info arg: 'Gd{c}...'"),
                    }

                    Some('F') => match chars_iter.next() {
                        None | Some(' ') => self.set_flag(ModuleFlags::EnableStringPooling, true),
                        Some('-') => match chars_iter.next() {
                            None | Some(' ') => self.set_flag(ModuleFlags::EnableStringPooling, false),
                            Some(c) => panic!("Unhandled characters in build info arg: 'GF-{c}...'"),
                        }
                        Some(c) => panic!("Unhandled characters in build info arg: 'GF{c}...'"),
                    }

                    Some('L') => match chars_iter.next() {
                        None | Some(' ') => self.set_flag(ModuleFlags::EnableWholeProgramOptimization, true),
                        Some('-') => match chars_iter.next() {
                            None | Some(' ') => self.set_flag(ModuleFlags::EnableWholeProgramOptimization, false),
                            Some(c) => panic!("Unhandled characters in build info arg: 'GL-{c}...'"),
                        }
                        Some(c) => panic!("Unhandled characters in build info arg: 'GL{c}...'"),
                    }

                    Some('m') => match chars_iter.next() {
                        None | Some(' ') => self.set_flag(ModuleFlags::EnableMinimalRebuild, true),
                        Some('-') => match chars_iter.next() {
                            None | Some(' ') => self.set_flag(ModuleFlags::EnableMinimalRebuild, false),
                            Some(c) => panic!("Unhandled characters in build info arg: 'Gm-{c}...'"),
                        }
                        Some(c) => panic!("Unhandled characters in build info arg: 'Gm{c}...'"),
                    }

                    Some('R') => match chars_iter.next() {
                        None | Some(' ') => self.set_flag(ModuleFlags::EnableRtti, true),
                        Some('-') => match chars_iter.next() {
                            None | Some(' ') => self.set_flag(ModuleFlags::EnableRtti, false),
                            Some(c) => panic!("Unhandled characters in build info arg: 'GR-{c}...'"),
                        }
                        Some(c) => panic!("Unhandled characters in build info arg: 'GR{c}...'"),
                    }

                    Some('S') => match chars_iter.next() {
                        None | Some(' ') => self.set_flag(ModuleFlags::CheckBufferSecurity, true),
                        Some('-') => match chars_iter.next() {
                            None | Some(' ') => self.set_flag(ModuleFlags::CheckBufferSecurity, false),
                            Some(c) => panic!("Unhandled characters in build info arg: 'GS-{c}...'"),
                        }
                        Some(c) => panic!("Unhandled characters in build info arg: 'GS{c}...'"),
                    }

                    Some('w') => match chars_iter.next() {
                        None | Some(' ') => self.set_flag(ModuleFlags::EnableWholeProgramDataOptimization, true),
                        Some('+') => match chars_iter.next() {
                            None | Some(' ') => self.set_flag(ModuleFlags::EnableWholeProgramDataOptimization, true),
                            Some(c) => panic!("Unhandled characters in build info arg: 'Gw+{c}...'"),
                        }
                        Some('-') => match chars_iter.next() {
                            None | Some(' ') => self.set_flag(ModuleFlags::EnableWholeProgramDataOptimization, false),
                            Some(c) => panic!("Unhandled characters in build info arg: 'Gw-{c}...'"),
                        }
                        Some(c) => panic!("Unhandled characters in build info arg: 'Gw{c}...'"),
                    }

                    Some('y') => match chars_iter.next() {
                        None | Some(' ') => self.set_flag(ModuleFlags::FunctionLevelLinking, true),
                        Some('-') => match chars_iter.next() {
                            None | Some(' ') => self.set_flag(ModuleFlags::FunctionLevelLinking, false),
                            Some(c) => panic!("Unhandled characters in build info arg: 'Gy-{c}...'"),
                        }
                        Some(c) => todo!("arg switch 'Gy{c}...'"),
                    }

                    Some(c) => todo!("arg switch 'G{c}...'"),
                    None => panic!("Unexpected character in build info arg: 'G'"),
                }

                Some('k') => match parse_arg_string(&mut chars_iter) {
                    Some(s) if s == "ernel" => self.set_flag(ModuleFlags::CreateKernelModeBinary, true),
                    Some(s) => panic!("Unhandled characters in build info arg: 'k{s}'"),
                    None => panic!("Unexpected character in build info arg: 'k'"),
                }

                Some('I') => match parse_arg_string(&mut chars_iter) {
                    Some(x) => self.additional_include_dirs.push(crate::canonicalize_path(out_path.to_str().unwrap_or(""), root_path.as_str(), x.as_str(), true)),
                    None => panic!("Missing string from additional include directory arg"),
                }

                Some('L') => match chars_iter.next() {
                    Some('D') => match chars_iter.next() {
                        None | Some(' ') => self.feature_toggles.push("LD".to_string()),
                        Some('d') => match chars_iter.next() {
                            None | Some(' ') => self.feature_toggles.push("LDd".to_string()),
                            Some(c) => panic!("Unhandled characters in build info arg: 'LDd{c}...'"),
                        }
                        Some(c) => panic!("Unhandled characters in build info arg: 'LD{c}...'"),
                    }

                    Some(c) => todo!("arg switch 'L{c}...'"),
                    None => panic!("Unexpected character in build info arg: 'L'"),
                }

                Some('M') => match parse_arg_string(&mut chars_iter) {
                    Some(s) if s.starts_with("D") => match &s[1..] {
                        s if s.is_empty() => self.feature_toggles.push("MD".to_string()),
                        "d" => self.feature_toggles.push("MDd".to_string()),
                        s => panic!("Unhandled characters in build info arg: 'MD{s}'"),
                    }

                    Some(s) if s.starts_with("P") => match &s[1..] {
                        s if s.is_empty() => self.build_process_count = Some(1),
                        s => match s.parse::<usize>() {
                            Ok(x) => self.build_process_count = Some(x),
                            Err(_) => panic!("Unhandled characters in build info arg: 'MP{s}'"),
                        }
                    }

                    Some(s) if s.starts_with("T") => match &s[1..] {
                        s if s.is_empty() => self.feature_toggles.push("MT".to_string()),
                        "d" => self.feature_toggles.push("MTd".to_string()),
                        s => panic!("Unhandled characters in build info arg: 'MT{s}'"),
                    }

                    Some(s) => todo!("arg switch 'M{s}'"),
                    None => panic!("Unexpected character in build info arg: 'M'"),
                }

                Some('n') => match parse_arg_string(&mut chars_iter) {
                    Some(s) if s == "ologo" => self.set_flag(ModuleFlags::Nologo, true),
                    Some(s) if s == "othreadsafestatics" => self.set_flag(ModuleFlags::NoThreadSafeStatics, true),
                    Some(s) => panic!("Unhandled characters in build info arg: 'n{s}'"),
                    None => panic!("Unexpected character in build info arg: 'n'"),
                }

                Some('o') => match parse_arg_string(&mut chars_iter) {
                    Some(s) if s == "penmp" => self.openmp = Some("omp".to_string()),
                    Some(s) if s == "penmp-" => self.openmp = None,
                    Some(s) if s == "penmp:experimental" => self.openmp = Some("experimental".to_string()),
                    Some(s) if s == "penmp:llvm" => self.openmp = Some("llvm".to_string()),
                    Some(s) if s == "ptions:strict" => self.set_flag(ModuleFlags::UnknownCompilerOptionsAreErrors, true),
                    Some(s) => panic!("Unhandled characters in build info arg: 'o{s}'"),
                    None => panic!("Unexpected character in build info arg: 'o'"),
                }

                Some('O') => match chars_iter.next() {
                    Some('1') => match chars_iter.next() {
                        None | Some(' ') => self.set_flag(ModuleFlags::CreateSmallCode, true),
                        Some(c) => panic!("Unhandled characters in build info arg: 'O1{c}...'"),
                    }

                    Some('2') => match chars_iter.next() {
                        None | Some(' ') => self.set_flag(ModuleFlags::CreateFastCode, true),
                        Some(c) => panic!("Unhandled characters in build info arg: 'O2{c}...'"),
                    }

                    Some('b') => match chars_iter.next() {
                        Some('0') => match chars_iter.next() {
                            None | Some(' ') => self.inline_function_expansion = Some(0),
                            Some(c) => panic!("Unhandled characters in build info arg: 'Ob0{c}...'"),
                        }
                        
                        Some('1') => match chars_iter.next() {
                            None | Some(' ') => self.inline_function_expansion = Some(1),
                            Some(c) => panic!("Unhandled characters in build info arg: 'Ob1{c}...'"),
                        }

                        Some('2') => match chars_iter.next() {
                            None | Some(' ') => self.inline_function_expansion = Some(2),
                            Some(c) => panic!("Unhandled characters in build info arg: 'Ob2{c}...'"),
                        }
                        
                        Some('3') => match chars_iter.next() {
                            None | Some(' ') => self.inline_function_expansion = Some(3),
                            Some(c) => panic!("Unhandled characters in build info arg: 'Ob3{c}...'"),
                        }
                        
                        Some(c) => panic!("Unhandled characters in build info arg: 'Ob{c}...'"),
                        None => panic!("Unhandled characters in build info arg: 'Ob'"),
                    }

                    Some('d') => match chars_iter.next() {
                        None | Some(' ') => self.set_flag(ModuleFlags::OptimizationDisabled, true),
                        Some(c) => panic!("Unhandled characters in build info arg: 'Od{c}...'"),
                    }

                    Some('i') => match chars_iter.next() {
                        None | Some(' ') => self.set_flag(ModuleFlags::GenerateIntrinsicFunctions, true),
                        Some('-') => match chars_iter.next() {
                            None | Some(' ') => self.set_flag(ModuleFlags::GenerateIntrinsicFunctions, false),
                            Some(c) => panic!("Unhandled characters in build info arg: 'Oi-{c}...'"),
                        }
                        Some(c) => panic!("Unhandled characters in build info arg: 'Oi{c}...'"),
                    }

                    Some('s') => match chars_iter.next() {
                        None | Some(' ') => self.set_flag(ModuleFlags::FavorsSmallCode, true),
                        Some(c) => panic!("Unhandled characters in build info arg: 'Os{c}...'"),
                    }

                    Some('t') => match chars_iter.next() {
                        None | Some(' ') => self.set_flag(ModuleFlags::FavorsFastCode, true),
                        Some(c) => panic!("Unhandled characters in build info arg: 'Ot{c}...'"),
                    }

                    Some('x') => match chars_iter.next() {
                        None | Some(' ') => {
                            // TODO: O2 without GF or Gy
                        }
                        Some('-') => match chars_iter.next() {
                            None | Some(' ') => {
                                // TODO: O2 without GF or Gy
                            }
                            Some(c) => panic!("Unhandled characters in build info arg: 'Ox-{c}...'"),
                        }
                        Some(c) => panic!("Unhandled characters in build info arg: 'Ox{c}...'"),
                    }

                    Some('y') => match chars_iter.next() {
                        None | Some(' ') => self.set_flag(ModuleFlags::FramePointerEmission, true),
                        Some('-') => match chars_iter.next() {
                            None | Some(' ') => self.set_flag(ModuleFlags::FramePointerEmission, false),
                            Some(c) => panic!("Unhandled characters in build info arg: 'Oy-{c}...'"),
                        }
                        Some(c) => panic!("Unhandled characters in build info arg: 'Oy{c}...'"),
                    }

                    Some(c) => todo!("arg switch 'O{c}...'"),
                    None => panic!("Unexpected character in build info arg: 'O'"),
                }

                Some('p') => match parse_arg_string(&mut chars_iter) {
                    Some(s) if s == "ermissive" => self.set_flag(ModuleFlags::Permissive, true),
                    Some(s) if s == "ermissive-" => self.set_flag(ModuleFlags::Permissive, false),
                    Some(s) => panic!("Unhandled characters in build info arg: 'p{s}'"),
                    None => panic!("Unexpected character in build info arg: 'p'"),
                }

                Some('Q') => match parse_arg_string(&mut chars_iter) {
                    Some(s) if s == "par" => self.set_flag(ModuleFlags::EnableLoopParallelization, true),
                    Some(s) if s == "spectre" => self.set_flag(ModuleFlags::MitigateSpectreVulnerabilities, true),
                    Some(s) => panic!("Unhandled characters in build info arg: 'Q{s}'"),
                    None => panic!("Unexpected character in build info arg: 'Q'"),
                }

                Some('R') => match parse_arg_string(&mut chars_iter) {
                    Some(s) if s == "TC1" || s == "TCsu" => self.set_flag(ModuleFlags::EnableFastRuntimeChecks, true),
                    Some(s) if s == "TCc" => self.set_flag(ModuleFlags::ConvertToSmallerTypeCheckAtRuntime, true),
                    Some(s) if s == "TCs" => self.set_flag(ModuleFlags::EnableStackFrameRuntimeChecks, true),
                    Some(s) if s == "TCu" => self.set_flag(ModuleFlags::EnableUninitializedLocalUsageChecks, true),
                    Some(s) => panic!("Unhandled characters in build info arg: 'R{s}'"),
                    None => panic!("Unhandled characters in build info arg: 'R'"),
                }

                Some('s') => match parse_arg_string(&mut chars_iter) {
                    Some(s) if s == "dl" => self.set_flag(ModuleFlags::EnableSdl, true),
                    Some(s) if s == "dl-" => self.set_flag(ModuleFlags::EnableSdl, false),
                    Some(s) if s.starts_with("td:") => self.language_standard = Some(s.trim_start_matches("td:").to_string()),
                    Some(s) => panic!("Unhandled characters in build info arg: 's{s}'"),
                    None => panic!("Unhandled characters in build info arg: 's'"),
                }

                Some('T') => match chars_iter.next() {
                    Some('C') => match chars_iter.next() {
                        None | Some(' ') => self.set_flag(ModuleFlags::UseCSourceFileType, true),
                        Some(c) => panic!("Unhandled characters in build info arg: 'TC{c}...'"),
                    }

                    Some('P') => match chars_iter.next() {
                        None | Some(' ') => self.set_flag(ModuleFlags::UseCppSourceFileType, true),
                        Some(c) => panic!("Unhandled characters in build info arg: 'TP{c}...'"),
                    }

                    Some(c) => todo!("arg switch 'T{c}...'"),
                    None => panic!("Unexpected character in build info arg: 'T'"),
                }

                Some('w') => match chars_iter.next() {
                    Some('1') => self.warnings_level1.push(parse_arg_string(&mut chars_iter).unwrap_or_default()),
                    Some('2') => self.warnings_level1.push(parse_arg_string(&mut chars_iter).unwrap_or_default()),
                    Some('3') => self.warnings_level1.push(parse_arg_string(&mut chars_iter).unwrap_or_default()),
                    Some('4') => self.warnings_level1.push(parse_arg_string(&mut chars_iter).unwrap_or_default()),

                    Some('d') => match parse_arg_string(&mut chars_iter) {
                        Some(s) => self.disabled_warnings.push(s),
                        None => panic!("Unhandled characters in build info arg: 'wd'"),
                    }

                    Some('e') => match parse_arg_string(&mut chars_iter) {
                        Some(s) => self.warnings_as_errors.push(s),
                        None => panic!("Unhandled characters in build info arg: 'we'"),
                    }

                    Some(s) => todo!("arg switch 'w{s}'"),
                    None => panic!("Missing warning number to disable"),
                }

                Some('W') => match chars_iter.next() {
                    Some('0') => match chars_iter.next() {
                        None | Some(' ') => self.output_warning_level = Some(0),
                        Some(c) => panic!("Unhandled characters in build info arg: 'W0{c}...'"),
                    }

                    Some('1') => match chars_iter.next() {
                        None | Some(' ') => self.output_warning_level = Some(1),
                        Some(c) => panic!("Unhandled characters in build info arg: 'W1{c}...'"),
                    }

                    Some('2') => match chars_iter.next() {
                        None | Some(' ') => self.output_warning_level = Some(2),
                        Some(c) => panic!("Unhandled characters in build info arg: 'W2{c}...'"),
                    }

                    Some('3') => match chars_iter.next() {
                        None | Some(' ') => self.output_warning_level = Some(3),
                        Some(c) => panic!("Unhandled characters in build info arg: 'W3{c}...'"),
                    }

                    Some('4') => match chars_iter.next() {
                        None | Some(' ') => self.output_warning_level = Some(4),
                        Some(c) => panic!("Unhandled characters in build info arg: 'W4{c}...'"),
                    }

                    Some('a') => match parse_arg_string(&mut chars_iter) {
                        Some(s) if s == "ll" => self.set_flag(ModuleFlags::EnableAllWarnings, true),
                        x => panic!("Unhandled characters in build info arg: 'Wa{}'", x.unwrap_or_default()),
                    }

                    Some('v') => match parse_arg_string(&mut chars_iter) {
                        Some(s) if s.starts_with(':') => self.disable_warnings_after_version = Some(s.to_string()),
                        Some(s) => panic!("Unhandled characters in build info arg: 'Wv{s}'"),
                        None => panic!("Unhandled characters in build info arg: 'Wv'"),
                    }

                    Some('X') => match chars_iter.next() {
                        None | Some(' ') => self.set_flag(ModuleFlags::TreatWarningsAsErrors, true),
                        Some('-') => match chars_iter.next() {
                            None | Some(' ') => self.set_flag(ModuleFlags::TreatWarningsAsErrors, false),
                            Some(c) => panic!("Unhandled characters in build info arg: 'WX-{c}...'"),
                        }
                        Some(c) => panic!("Unhandled characters in build info arg: 'WX{c}...'"),
                    }

                    Some(s) => todo!("arg switch 'W{s}'"),
                    None => panic!("Unexpected character in build info arg: 'W'"),
                }

                Some('X') => match chars_iter.next() {
                    None | Some(' ') => self.set_flag(ModuleFlags::IgnoreStandardIncludeDir, true),
                    Some(c) => panic!("Unhandled characters in build info arg: 'X{c}...'"),
                }

                Some('Y') => match chars_iter.next() {
                    Some('c') => match parse_arg_string(&mut chars_iter) {
                        Some(s) => self.pch_file_name = Some(crate::canonicalize_path(out_path.to_str().unwrap_or(""), root_path.as_str(), s.as_str(), false)),
                        None => self.pch_file_name = Some(PathBuf::new()),
                    }

                    Some('l') => match parse_arg_string(&mut chars_iter) {
                        None => self.set_flag(ModuleFlags::InjectPchReference, true),
                        Some(s) if s.is_empty() => self.set_flag(ModuleFlags::InjectPchReference, true),
                        Some(s) if s == "-" => self.set_flag(ModuleFlags::InjectPchReference, false),
                        Some(s) => self.pch_references.push(s),
                    }

                    Some('u') => match parse_arg_string(&mut chars_iter) {
                        Some(s) => self.precompiled_header_file_name = Some(s),
                        None => panic!("Unhandled characters in build info arg: 'Yu'"),
                    }

                    Some(c) => todo!("arg switch 'Y{c}...'"),
                    None => panic!("Unexpected character in build info arg: 'Y'"),
                }

                Some('Z') => match chars_iter.next() {
                    Some('7') => match chars_iter.next() {
                        None | Some(' ') => self.set_flag(ModuleFlags::GenerateC7DebugInfo, true),
                        Some(c) => panic!("Unhandled characters in build info arg: 'Z7{c}...'"),
                    }

                    Some('c') => match chars_iter.next() {
                        Some(':') => match parse_arg_string(&mut chars_iter) {
                            Some(x) => self.feature_toggles.push(x),
                            None => panic!("Missing identifier from feature toggle"),
                        }

                        Some(c) => todo!("arg switch 'Zc{c}...'"),
                        None => panic!("Unhandled characters in build info arg: 'Zc'"),
                    }

                    Some('f') => match chars_iter.next() {
                        None | Some(' ') => self.set_flag(ModuleFlags::FasterPdbGeneration, true),
                        Some(c) => panic!("Unhandled characters in build info arg: 'Zf{c}...'"),
                    }

                    Some('H') => match parse_arg_string(&mut chars_iter) {
                        Some(s) if s.starts_with(':') => self.hash_algorithm = Some(s.trim_start_matches(':').to_string()),
                        Some(s) => panic!("Unhandled characters in build info arg: 'ZH{s}'"),
                        None => panic!("Unhandled characters in build info arg: 'ZH'"),
                    }

                    Some('i') => match chars_iter.next() {
                        None | Some(' ') => self.set_flag(ModuleFlags::GenerateFullDebugInfo, true),
                        Some('+') => match chars_iter.next() {
                            None | Some(' ') => self.set_flag(ModuleFlags::GenerateFullDebugInfo, true),
                            Some(c) => panic!("Unhandled characters in build info arg: 'Zi+{c}...'"),
                        }
                        Some('-') => match chars_iter.next() {
                            None | Some(' ') => self.set_flag(ModuleFlags::GenerateFullDebugInfo, false),
                            Some(c) => panic!("Unhandled characters in build info arg: 'Zi-{c}...'"),
                        }
                        Some(c) => panic!("Unhandled characters in build info arg: 'Zi{c}...'"),
                    }

                    Some('l') => match chars_iter.next() {
                        None | Some(' ') => self.set_flag(ModuleFlags::RemoveDefaultLibraryName, true),
                        Some(c) => todo!("arg switch 'Zl{c}...'"),
                    }

                    Some('o') => match chars_iter.next() {
                        None | Some(' ') => self.set_flag(ModuleFlags::GenerateRicherDebugInfoForOptimizedCode, true),
                        Some('-') => match chars_iter.next() {
                            None | Some(' ') => self.set_flag(ModuleFlags::GenerateRicherDebugInfoForOptimizedCode, false),
                            Some(c) => panic!("Unhandled characters in build info arg: 'Zo-{c}...'"),
                        }
                        Some(c) => panic!("Unhandled characters in build info arg: 'Zo{c}...'"),
                    }

                    Some('p') => match parse_arg_string(&mut chars_iter) {
                        Some(x) => self.pack_structure_members = Some(x),
                        None => panic!("Missing identifier from feature toggle"),
                    }

                    Some('W') => match chars_iter.next() {
                        None | Some(' ') => self.set_flag(ModuleFlags::WindowsRuntimeCompilation, true),
                        
                        Some(':') => match parse_arg_string(&mut chars_iter) {
                            Some(s) if s == "nostdlib" => self.set_flag(ModuleFlags::WindowsRuntimeCompilationNostdlib, true),
                            Some(s) => panic!("Unhandled characters in build info arg: 'ZW:{s}'"),
                            None => panic!("Unhandled characters in build info arg: 'ZW:'"),
                        }

                        Some(c) => panic!("Unhandled characters in build info arg: 'ZW{c}...'"),
                    }

                    Some(c) => todo!("arg switch 'Z{c}...'"),
                    None => panic!("Unexpected character in build info arg: 'Z'"),
                }

                Some(c) => panic!("Unexpected character in build info arg: '{c}...'"),
            }
        }

        // Sort additional include directories from longest to shortest
        self.additional_include_dirs.sort_by_key(|a| a.to_string_lossy().len());
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
                writeln!(f, "#include <{}>", header.to_string_lossy().trim_start_matches('/'))?;
            } else {
                writeln!(f, "#include \"{}\"", header.to_string_lossy().trim_start_matches('/'))?;
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
                    storage.push((*line, u.clone()));
                    prev_line = *line;
                }

                ModuleMember::ThreadStorage(_, _, Some(line)) => {
                    storage.push((*line, u.clone()));
                    prev_line = *line;
                }

                ModuleMember::Procedure(_, _, Some(line)) => {
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

        for item in storage {
            writeln!(f)?;
            item.1.fmt(f)?;
        }

        Ok(())
    }
}
