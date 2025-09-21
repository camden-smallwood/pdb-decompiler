use crate::{cpp, tabbed::TabbedDisplay};
use std::{
    cell::RefCell,
    collections::HashMap,
    fmt,
    iter::Peekable,
    path::PathBuf,
    rc::Rc,
    str::Chars,
};

pub static SOURCE_FILE_EXTS: &[&str] = &[
    "c", "cc", "cpp", "cxx", "pch", "asm", "fasm", "masm", "res", "exp",
];

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModuleMember {
    EmptyLine,
    Preprocessor(String),
    Include(bool, PathBuf),
    Comment(String),
    Block {
        members: Vec<ModuleMember>,
    },
    Class(Rc<RefCell<cpp::Class>>),
    Enum(cpp::Enum),
    TypeDefinition(cpp::TypeDefinition),
    UsingNamespace(String),
    Constant(String),
    Data {
        is_static: bool,
        name: String,
        signature: String,
        address: u64,
        line: Option<u32>,
    },
    ThreadStorage {
        is_static: bool,
        name: String,
        signature: String,
        address: u64,
        line: Option<u32>,
    },
    Procedure(cpp::Procedure),
    Tagged(String, Box<ModuleMember>),
    FunctionCall(String, Vec<String>),
}

impl fmt::Display for ModuleMember {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EmptyLine => write!(f, ""),
            Self::Preprocessor(p) => write!(f, "#{p}"),
            Self::Include(global, path) => write!(
                f,
                "#include {}",
                if *global {
                    format!("<{}>", path.to_string_lossy().trim_start_matches('/'))
                } else {
                    format!("\"{}\"", path.to_string_lossy().trim_start_matches('/'))
                },
            ),
            Self::Comment(c) => write!(f, "/* {c} */"),
            Self::Block { members } => {
                writeln!(f, "{{")?;

                for member in members.iter() {
                    member.tabbed_fmt(1, f)?;
                    writeln!(f)?;
                }

                writeln!(f, "}}")
            },
            Self::Class(c) => c.borrow().fmt(f),
            Self::Enum(e) => e.fmt(f),
            Self::TypeDefinition(u) => u.fmt(f),
            Self::UsingNamespace(n) => f.write_fmt(format_args!("using namespace {n};")),
            Self::Constant(c) => c.fmt(f),
            Self::Data { is_static, signature, .. } => {
                write!(
                    f,
                    "{}{}",
                    if *is_static {
                        "static "
                    } else {
                        ""
                    },
                    signature,
                )
            },
            Self::ThreadStorage { is_static, signature, .. } => {
                write!(
                    f,
                    "{}{}",
                    if *is_static {
                        "static "
                    } else {
                        ""
                    },
                    signature,
                )
            },
            Self::Procedure(p) => p.fmt(f),
            Self::Tagged(tag, m) => {
                if let cpp::ModuleMember::Block { .. } = m.as_ref() {
                    writeln!(f, "{tag}")?;
                    write!(f, "{m}")?;
                } else {
                    write!(f, "{tag} {m}")?;
                }
                Ok(())
            },
            Self::FunctionCall(function, parameters) => write!(f, "{}({});", function, parameters.join(", ")),
        }
    }
}

#[repr(u64)]
pub enum ModulePrimaryFlags {
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
    ExperimentalModuleSupport = 1 << 59,
    ExperimentalDeterministic = 1 << 60,
    FiberSafeTLS = 1 << 61,
    ImportNoRegistry = 1 << 62,
    IgnorePragmaWarningError = 1 << 63,
}

#[repr(u64)]
pub enum ModuleSecondaryFlags {
    NoDateTime = 1 << 0,
    AllowCompatibleILVersions = 1 << 1,
    DisableVecMathLib = 1 << 2,
    VecSSE2Only = 1 << 3,
    FastTranscendentals = 1 << 4,
    SuppressFloatToIntegralHelperCall = 1 << 5,
    RemoveFwaitInTryBlocks = 1 << 6,
    MitigateIntelJccErratumUpdatePerformance = 1 << 7,
    SafeFpLoads = 1 << 8,
    GenerateSerializingForLoads = 1 << 9,
    GenerateSerializingForControlFlowLoads = 1 << 10,
    DisableSlpVec = 1 << 11,
    DisableVec = 1 << 12,
    WarningLKG171 = 1 << 13,
    VersionLKG171 = 1 << 14,
    Vc7DName = 1 << 15,
    BuildingVCCorlib = 1 << 16,
    BuildingMSVCDLL = 1 << 17,
    WindowsRuntimeNoMetadata = 1 << 18,
    GenerateEditAndContinueDebugInfo = 1 << 19,
    ShowIncludes = 1 << 20,
    UseStdcallCallingConvention = 1 << 21,
    CreateHotpatchableImage = 1 << 22,
    NoCfgRngChk = 1 << 23,
    ValidateUtf8Charset = 1 << 24,
    GenerateArm64ECCompatibleAbi = 1 << 25,
    ClrCompatible = 1 << 26,
    ClrImplicitKeepAlive = 1 << 27,
    ClrInitialAppDomain = 1 << 28,
    ClrNetCore = 1 << 29,
    ClrNoAssembly = 1 << 30,
    ClrNoStdImport = 1 << 31,
    ClrNoStdLib = 1 << 32,
    ClrPure = 1 << 33,
    ClrSafe = 1 << 34,
    JustMyCode = 1 << 35,
    GenerateXmlDocumentation = 1 << 36,
    IdeMinimalRebuild = 1 << 37,
    ListingFileAssemblerOnly = 1 << 38,
    ListingFileMachineCode = 1 << 39,
    ListingFileSourceCode = 1 << 40,
    ListingFileUtf8 = 1 << 41,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Module {
    pub path: PathBuf,
    pub compiler_path: PathBuf,
    pub pdb_path: PathBuf,
    pub flags_primary: u64,
    pub flags_secondary: u64,

    pub headers: Vec<(PathBuf, bool)>,
    pub members: Vec<ModuleMember>,
    pub mangled_symbols: Vec<(String, u64)>,

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
    pub external_headers: Vec<String>,
    pub external_headers_var: Option<String>,
    pub external_headers_warning_level: Option<usize>,
    pub fid_file: Option<String>,
    pub inline_function_expansion: Option<usize>,
    pub code_generation_threads: Option<usize>,
    pub build_process_count: Option<usize>,
    pub experimental_logs_file: Option<String>,
    pub func_cache: Option<usize>,
    pub loop_parallelization_report_level: Option<usize>,
    pub vec_report_level: Option<usize>,
    pub precompiled_header_memory: Option<usize>,
    pub object_file: Option<String>,
    pub program_database_file: Option<String>,
    pub stack_probe_threshold: Option<usize>,
    pub source_charset: Option<String>,
    pub execution_charset: Option<String>,
    pub constexpr_backtrace: Option<usize>,
    pub constexpr_depth: Option<usize>,
    pub constexpr_steps: Option<usize>,
    pub doc_file: Option<String>,
    pub listing_file: Option<PathBuf>,

    pub additional_include_dirs: Vec<PathBuf>,
    pub using_directive_dirs: Vec<PathBuf>,
    pub forced_using_directives: Vec<PathBuf>,
    pub preprocessor_definitions: Vec<(String, Option<String>)>,
    pub preprocess_include_files: Vec<String>,
    pub disabled_warnings: Vec<String>,
    pub logged_warnings: Vec<String>,
    pub warnings_as_errors: Vec<String>,
    pub warnings_level1: Vec<String>,
    pub warnings_level2: Vec<String>,
    pub warnings_level3: Vec<String>,
    pub warnings_level4: Vec<String>,
    pub feature_toggles: Vec<String>,
    pub pch_references: Vec<String>,
    pub d1_args: Vec<String>,
    pub d2_args: Vec<String>,
    pub trim_files: Vec<String>,
    pub diagnostics: Vec<String>,
}

impl Module {
    #[inline(always)]
    pub fn is_header(&self) -> bool {
        match self.path.extension().and_then(std::ffi::OsStr::to_str) {
            Some(ext) if SOURCE_FILE_EXTS.contains(&ext) => false,
            _ => true,
        }
    }

    #[inline(always)]
    pub fn with_path(mut self, path: PathBuf) -> Self {
        self.path = path;
        self
    }

    #[inline(always)]
    pub fn set_primary_flag(&mut self, flag: ModulePrimaryFlags, on: bool) {
        if on {
            self.flags_primary |= flag as u64;
        } else {
            self.flags_primary ^= flag as u64;
        }
    }

    #[allow(dead_code)]
    #[inline(always)]
    pub fn test_primary_flag(&self, flag: ModulePrimaryFlags) -> bool {
        (self.flags_primary & (flag as u64)) != 0
    }

    #[inline(always)]
    pub fn set_secondary_flag(&mut self, flag: ModuleSecondaryFlags, on: bool) {
        if on {
            self.flags_secondary |= flag as u64;
        } else {
            self.flags_secondary ^= flag as u64;
        }
    }

    #[allow(dead_code)]
    #[inline(always)]
    pub fn test_secondary_flag(&self, flag: ModuleSecondaryFlags) -> bool {
        (self.flags_secondary & (flag as u64)) != 0
    }

    #[inline(always)]
    pub fn add_type_definition(
        &mut self,
        class_table: &mut Vec<Rc<RefCell<cpp::Class>>>,
        type_sizes: &mut HashMap<String, u64>,
        machine_type: pdb2::MachineType,
        type_info: &pdb2::TypeInformation,
        type_finder: &pdb2::TypeFinder,
        type_index: pdb2::TypeIndex,
        line: u32,
    ) -> pdb2::Result<()> {
        if self.members.iter().any(|x| match x {
            ModuleMember::Class(c) => c.borrow().index == type_index,
            ModuleMember::Enum(e) => e.index == type_index,
            _ => false,
        }) {
            return Ok(());
        }

        let type_item = match type_finder.find(type_index) {
            Ok(type_item) => type_item,
            Err(_) => {
                // println!("WARNING: failed to find type: {e}");
                return Ok(());
            }
        };

        match type_item.parse() {
            Ok(data) if matches!(data, pdb2::TypeData::Class(_) | pdb2::TypeData::Union(_)) => {
                let (name, size, properties, fields, derived_from) = match &data {
                    pdb2::TypeData::Class(class_data) => {
                        (class_data.name, class_data.size, class_data.properties, class_data.fields, class_data.derived_from)
                    }

                    pdb2::TypeData::Union(union_data) => {
                        (union_data.name, union_data.size, union_data.properties, Some(union_data.fields), None)
                    }

                    _ => unreachable!(),
                };

                if properties.forward_reference() {
                    println!("WARNING: Skipping forward reference in toplevel: \"{}\" in \"{}\"", name, self.path.display());
                    return Ok(());
                }

                let definition = Rc::new(RefCell::new(cpp::Class {
                    kind: match &data {
                        pdb2::TypeData::Class(class_data) => Some(class_data.kind),
                        _ => None
                    },
                    is_union: matches!(data, pdb2::TypeData::Union(_)),
                    is_declaration: false,
                    name: name.to_string().to_string(),
                    index: type_index,
                    depth: 0,
                    line,
                    size: size,
                    base_classes: vec![],
                    members: vec![],
                    properties: Some(properties),
                    field_attributes: None,
                }));

                if !class_table.iter().any(|c| c.borrow().index == type_index) {
                    class_table.push(definition.clone());
                }

                if let Some(derived_from) = derived_from
                    && let Err(_) = definition.borrow_mut().add_derived_from(type_finder, derived_from)
                {
                    // println!("WARNING: failed to add class derived from: {e}");
                }

                let Some(fields) = fields else {
                    panic!("Failed to get fields of \"{}\"", name);
                };
                
                {
                    let mut temp = definition.borrow().clone();
                    temp.add_members(class_table, type_sizes, machine_type, type_info, type_finder, fields)?;
                    *definition.borrow_mut() = temp;
                }

                let mut exists = false;

                for member in self.members.iter() {
                    if let ModuleMember::Class(other_definition) = member
                        && definition.borrow().kind == other_definition.borrow().kind
                        && definition.borrow().is_union == other_definition.borrow().is_union
                        && definition.borrow().name == other_definition.borrow().name
                        && definition.borrow().size == other_definition.borrow().size
                        && definition.borrow().base_classes.eq(&other_definition.borrow().base_classes)
                        && definition.borrow().members.eq(&other_definition.borrow().members)
                    {
                        exists = true;
                        break;
                    }
                }

                if !exists {
                    self.members.push(ModuleMember::Class(definition.clone()));
                }
            }

            Ok(pdb2::TypeData::Enumeration(data)) => {
                if data.properties.forward_reference() {
                    println!("WARNING: Skipping forward reference in toplevel: \"{}\" in \"{}\"", data.name, self.path.display());
                    return Ok(());
                }

                let underlying_type_name = match cpp::type_name(
                    class_table,
                    type_sizes, 
                    machine_type,
                    type_info,
                    type_finder,
                    data.underlying_type,
                    None,
                    None,
                    None,
                    false
                ) {
                    Ok(name) => name,
                    Err(_) => {
                        // println!("WARNING: failed to get enum type name: {e}");
                        return Ok(());
                    }
                };

                let size = cpp::type_size(class_table, type_sizes, machine_type, type_info, type_finder, data.underlying_type)?;

                let mut definition = cpp::Enum {
                    name: data.name.to_string().to_string(),
                    index: type_index,
                    depth: 0,
                    line,
                    underlying_type_name,
                    size,
                    is_declaration: false,
                    values: vec![],
                    properties: data.properties,
                    field_attributes: None,
                };

                if let Err(_) = definition.add_members(type_finder, data.fields) {
                    // println!("WARNING: failed to add enum members: {e}");
                }

                let mut exists = false;

                for member in self.members.iter() {
                    if let ModuleMember::Enum(other_definition) = member
                        && definition.name == other_definition.name
                        && definition.values.eq(&other_definition.values)
                    {
                        exists = true;
                        break;
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

    #[inline(always)]
    pub fn add_build_info(
        &mut self,
        out_path: &std::path::Path,
        id_finder: &pdb2::IdFinder,
        build_info: pdb2::BuildInfoId,
    ) -> pdb2::Result<()> {
        let mut args = vec![];

        for id_index in build_info.arguments {
            let id_item = match id_finder.find(id_index) {
                Ok(id_item) => id_item,
                Err(_) => {
                    // println!("WARNING: failed to find id {id_index}: {e}");
                    continue;
                }
            };

            let id_data = match id_item.parse() {
                Ok(pdb2::IdData::String(id_data)) => id_data,
                Ok(id_data) => panic!("Failed to parse id {id_index}: Expected String, got {id_data:?}"),
                Err(e) => {
                    println!("WARNING: failed to parse id {id_index}: {e}");
                    continue;
                }
            };

            let mut arg = String::new();

            if let Some(id_index) = id_data.substrings {
                let id_item = match id_finder.find(id_index) {
                    Ok(id_item) => id_item,
                    Err(_) => {
                        // println!("WARNING: failed to find id {id_index}: {e}");
                        continue;
                    }
                };

                let id_data = match id_item.parse() {
                    Ok(pdb2::IdData::StringList(id_data)) => id_data,

                    Ok(id_data) => panic!("Failed to parse id {id_index}: Expected StringList, got {id_data:?}"),

                    Err(e) => {
                        println!("WARNING: failed to parse id {id_index}: {e}");
                        continue;
                    }
                };

                for type_index in id_data.substrings {
                    // TODO: remove this hack when pdb crate fixes type
                    let id_index = pdb2::IdIndex(type_index.0);

                    let id_item = match id_finder.find(id_index) {
                        Ok(id_item) => id_item,
                        Err(_) => {
                            // println!("WARNING: failed to find id {id_index}: {e}");
                            continue;
                        }
                    };

                    match id_item.parse() {
                        Ok(pdb2::IdData::String(id_data)) => {
                            arg = format!("{}{}", arg, id_data.name.to_string());
                        }

                        Ok(id_data) => panic!("Failed to parse id {id_index}: Expected String, got {id_data:?}"),
                        
                        Err(e) => {
                            println!("WARNING: failed to parse id {id_index}: {e}");
                            continue;
                        }
                    }
                }
            }
            
            arg = format!("{}{}", arg, id_data.name.to_string());

            args.push(arg);
        }

        // println!("Module build info args: [");
        // for arg in args.iter() {
        //     println!("    \"{arg}\",");
        // }
        // println!("]");

        if args.len() != 2 && args.len() != 5 {
            panic!(
                "Unexpected build info arguments: [{}]",
                args.iter().map(|x| format!("\"{x}\"")).collect::<Vec<_>>().join(", "),
            );
        }

        let arg_count = args.len();
        let mut args_iter = args.into_iter();

        let root_path = args_iter.next().unwrap();
        let compiler_path = if arg_count == 2 { String::new() } else { args_iter.next().unwrap() };
        let module_path = args_iter.next().unwrap();
        let pdb_path = if arg_count == 2 { String::new() } else { args_iter.next().unwrap() };
        let args_string = if arg_count == 2 { String::new() } else { args_iter.next().unwrap() };

        self.path = crate::canonicalize_path(out_path.to_str().unwrap_or(""), root_path.as_str(), module_path.as_str(), false);
        self.compiler_path = compiler_path.into();
        self.pdb_path = pdb_path.into();

        // println!("Module arguments: {args_string}");

        let mut chars_iter = args_string.chars().peekable();
        self.parse_arguments(out_path, root_path.as_str(), args_string.as_str(), &mut chars_iter);

        // Sort additional include directories from longest to shortest
        self.additional_include_dirs.sort_by_key(|a| a.to_string_lossy().len());
        self.additional_include_dirs.reverse();

        Ok(())
    }

    #[inline(always)]
    fn parse_arguments(
        &mut self,
        out_path: &std::path::Path,
        root_path: &str,
        args_string: &str,
        chars_iter: &mut Peekable<Chars>,
    ) {
        fn parse_arg_string(chars_iter: &mut Peekable<Chars>) -> Option<String> {
            let mut string = String::new();
            let mut quoted = false;

            match chars_iter.next() {
                Some('"') => quoted = true,
                Some(c) if c.is_whitespace() => (),
                Some(c) => string.push(c),
                None => return None,
            }

            while let Some(c) = chars_iter.next() {
                match c {
                    '\\' if quoted => match chars_iter.next() {
                        Some(c) => {
                            string.push('\\');
                            string.push(c);
                        }
                        
                        None => panic!("Expected character after quoted escape sequence"),
                    }

                    '"' if quoted => match chars_iter.next() {
                        None | Some(' ') => break,
                        Some(c) => panic!("Unexpected character after string \"{string}\": '{c}'"),
                    },

                    c if c.is_whitespace() && !quoted => break,

                    _ => string.push(c),
                }
            }

            if string.is_empty() {
                None
            } else {
                Some(string)
            }
        }

        fn parse_arg_binding(chars_iter: &mut Peekable<Chars>) -> Option<(String, Option<String>)> {
            let mut name = String::new();
            let mut quoted = false;

            match chars_iter.next() {
                Some('"') => quoted = true,
                Some(c) if c.is_whitespace() => (),
                Some(c) => name.push(c),
                None => return None,
            }

            loop {
                match chars_iter.next() {
                    None => break,
                    Some(c) if c.is_whitespace() && !quoted => break,
                    Some('"') if quoted => (),
                    Some('=') => return Some((name, parse_arg_string(chars_iter))),

                    Some('\\') if quoted => match chars_iter.next() {
                        Some(c) => {
                            name.push('\\');
                            name.push(c);
                        }
                        
                        None => panic!("Expected character after quoted escape sequence"),
                    }

                    Some(c) => name.push(c),
                }
            }

            if name.is_empty() {
                None
            } else {
                Some((name, None))
            }
        }

        loop {
            match chars_iter.next() {
                None => break,
                Some('-' | '/') => (),
                Some(c) if c.is_whitespace() => continue,
                Some(c) => panic!("Unexpected character in build info arg at character {}: '{c}'; Next character: '{}'; Data: \"{args_string}\"", args_string.len() - chars_iter.size_hint().0, chars_iter.peek().unwrap_or(&' ')),
            }

            match chars_iter.next() {
                None => break,

                Some('@') => match parse_arg_string(chars_iter) {
                    Some(s) => self.compiler_response_file = Some(crate::canonicalize_path(out_path.to_str().unwrap_or(""), root_path, s.as_str(), false)),
                    None => panic!("Unexpected character in build info arg: '@'; Data: \"{args_string}\""),
                }

                Some('a') => match parse_arg_string(chars_iter) {
                    Some(s) if s == "nalyze" => self.set_primary_flag(ModulePrimaryFlags::EnableCodeAnalysis, true),
                    Some(s) if s == "nalyze-" => self.set_primary_flag(ModulePrimaryFlags::EnableCodeAnalysis, false),
                    Some(s) if s.starts_with("rch:") => self.minimum_cpu_architecture = Some(s[4..].to_owned()),
                    Some(s) if s == "rm64EC" => self.set_secondary_flag(ModuleSecondaryFlags::GenerateArm64ECCompatibleAbi, true),
                    Some(s) if s == "wait" => self.set_primary_flag(ModulePrimaryFlags::EnableCoroutines, true),
                    Some(s) if s == "wait:strict" => self.set_primary_flag(ModulePrimaryFlags::EnableCoroutinesStrict, true),
                    Some(s) => panic!("Unhandled characters in build info arg: 'a{s}'; Data: \"{args_string}\""),
                    None => panic!("Unexpected character in build info arg: 'a'; Data: \"{args_string}\""),
                }

                Some('A') => match chars_iter.next() {
                    Some('I') => match parse_arg_string(chars_iter) {
                        Some(s) => self.using_directive_dirs.push(crate::canonicalize_path(out_path.to_str().unwrap_or(""), root_path, s.as_str(), true)),
                        None => panic!("Missing directory for using directive"),
                    }

                    Some('l') => match parse_arg_string(chars_iter) {
                        Some(s) if s == "lowCompatibleILVersions" => self.set_secondary_flag(ModuleSecondaryFlags::AllowCompatibleILVersions, true),
                        Some(s) => panic!("Unhandled characters in build info arg: 'Al{s}'; Data: \"{args_string}\""),
                        None => panic!("Unexpected characters in build info arg: 'Al'; Data: \"{args_string}\""),
                    }

                    Some(c) => panic!("Unhandled characters in build info arg: 'A{c}...'; Data: \"{args_string}\""),
                    None => panic!("Unexpected character in build info arg: 'A'; Data: \"{args_string}\""),
                }

                Some('b') => match parse_arg_string(chars_iter) {
                    Some(s) if s == "igobj" => self.set_primary_flag(ModulePrimaryFlags::Bigobj, true),
                    Some(s) => panic!("Unhandled characters in build info arg: 'b{s}'; Data: \"{args_string}\""),
                    None => panic!("Unhandled characters in build info arg: 'b'; Data: \"{args_string}\""),
                }

                Some('B') => match parse_arg_string(chars_iter) {
                    Some(s) if s == "inl" => self.set_primary_flag(ModulePrimaryFlags::BuildInline, true),
                    Some(s) if s == "repro" => self.set_primary_flag(ModulePrimaryFlags::ReproducableOutput, true),
                    Some(s) if s == "uildingMSVCDLL" => self.set_secondary_flag(ModuleSecondaryFlags::BuildingMSVCDLL, true),
                    Some(s) if s == "uildingVCCorlib" => self.set_secondary_flag(ModuleSecondaryFlags::BuildingVCCorlib, true),
                    Some(s) => panic!("Unhandled characters in build info arg: 'B{s}'; Data: \"{args_string}\""),
                    None => panic!("Unhandled characters in build info arg: 'B'; Data: \"{args_string}\""),
                }

                Some('c') => {
                    match chars_iter.peek() {
                        Some(c) if c.is_whitespace() => {
                            self.set_primary_flag(ModulePrimaryFlags::CompileWithoutLinking, true);
                            continue;
                        }
                        
                        _ => {}
                    }

                    match parse_arg_string(chars_iter) {
                        None => self.set_primary_flag(ModulePrimaryFlags::CompileWithoutLinking, true),
                        Some(s) if s == "-" => self.set_primary_flag(ModulePrimaryFlags::CompileWithoutLinking, false),
                        Some(s) if s == "bstring" => self.set_primary_flag(ModulePrimaryFlags::UseByteStrings, true),
                        Some(s) if s.starts_with("gthreads") => self.code_generation_threads = Some(s.trim_start_matches("threads").parse().expect("Invalid code generation thread count")),
                        Some(s) if s == "lr" => self.set_secondary_flag(ModuleSecondaryFlags::ClrCompatible, true),
                        Some(s) if s == "lr:implicitKeepAlive" => self.set_secondary_flag(ModuleSecondaryFlags::ClrImplicitKeepAlive, true),
                        Some(s) if s == "lr:implicitKeepAlive-" => self.set_secondary_flag(ModuleSecondaryFlags::ClrImplicitKeepAlive, false),
                        Some(s) if s == "lr:initialAppDomain" => self.set_secondary_flag(ModuleSecondaryFlags::ClrInitialAppDomain, true),
                        Some(s) if s == "lr:netcore" => self.set_secondary_flag(ModuleSecondaryFlags::ClrNetCore, true),
                        Some(s) if s == "lr:noAssembly" => self.set_secondary_flag(ModuleSecondaryFlags::ClrNoAssembly, true),
                        Some(s) if s == "lr:nostdimport" => self.set_secondary_flag(ModuleSecondaryFlags::ClrNoStdImport, true),
                        Some(s) if s == "lr:nostdlib" => self.set_secondary_flag(ModuleSecondaryFlags::ClrNoStdLib, true),
                        Some(s) if s == "lr:pure" => self.set_secondary_flag(ModuleSecondaryFlags::ClrPure, true),
                        Some(s) if s == "lr:safe" => self.set_secondary_flag(ModuleSecondaryFlags::ClrSafe, true),
                        Some(s) if s.starts_with("onstexpr:backtrace") => self.constexpr_backtrace = Some(s.trim_start_matches("onstexpr:backtrace").parse().expect("Invalid constexpr backtrace level")),
                        Some(s) if s.starts_with("onstexpr:depth") => self.constexpr_depth = Some(s.trim_start_matches("onstexpr:depth").parse().expect("Invalid constexpr depth level")),
                        Some(s) if s.starts_with("onstexpr:steps") => self.constexpr_steps = Some(s.trim_start_matches("onstexpr:steps").parse().expect("Invalid constexpr step count")),
                        Some(s) => panic!("Unhandled characters in build info arg: 'c{s}'; Data: \"{args_string}\""),
                    }
                }

                Some('C') => match chars_iter.next() {
                    None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::PreserveCommentsDuringPreprocessing, true),
                    Some(c) => panic!("Unhandled characters in build info arg: 'C{c}...'; Data: \"{args_string}\""),
                }

                Some('d') => match chars_iter.next() {
                    Some('1') => match parse_arg_string(chars_iter) {
                        Some(s) => self.d1_args.push(s),
                        None => panic!("Unhandled characters in build info arg: 'd1'; Data: \"{args_string}\""),
                    }

                    Some('2') => match parse_arg_string(chars_iter) {
                        Some(s) => self.d2_args.push(s),
                        None => panic!("Unhandled characters in build info arg: 'd2'; Data: \"{args_string}\""),
                    }

                    Some('i') => match parse_arg_string(chars_iter) {
                        Some(s) if s == "agnostics" => self.diagnostics.push("column".into()),
                        Some(s) if s.starts_with("agnostics:") => self.diagnostics.push(s.trim_start_matches("agnostics:").into()),
                        Some(s) => panic!("Unhandled characters in build info arg: 'di{s}'; Data: \"{args_string}\""),
                        None => panic!("Unhandled characters in build info arg: 'di'; Data: \"{args_string}\""),
                    }

                    Some('o') => match chars_iter.next() {
                        Some('c') => match parse_arg_string(chars_iter) {
                            Some(s) => self.doc_file = Some(s),
                            None => self.set_secondary_flag(ModuleSecondaryFlags::GenerateXmlDocumentation, true),
                        }
                        Some(c) => panic!("Unhandled characters in build info arg: 'do{c}...'; Data: \"{args_string}\""),
                        None => panic!("Unhandled characters in build info arg: 'do'; Data: \"{args_string}\""),
                    }

                    Some(c) => panic!("Unhandled characters in build info arg: 'd{c}...'; Data: \"{args_string}\""),
                    None => panic!("Unexpected character in build info arg: 'd'; Data: \"{args_string}\""),
                }

                Some('D') => {
                    let mut parse_preprocessor_definition = |chars_iter: &mut Peekable<Chars>| {
                        match parse_arg_binding(chars_iter) {
                            Some(x) => self.preprocessor_definitions.push(x),
                            None => panic!("Missing name from preprocessor definition"),
                        }
                    };

                    if let Some('"') = chars_iter.peek() {
                        let string = parse_arg_string(chars_iter);

                        if let Some(string) = string {
                            let mut chars_iter = string.chars().peekable();
                            parse_preprocessor_definition(&mut chars_iter);
                        }
                    } else {
                        parse_preprocessor_definition(chars_iter);
                    }
                }

                Some('e') => match chars_iter.next() {
                    Some('r') => match parse_arg_string(chars_iter) {
                        Some(s) if s.to_lowercase() == "rorreport" => self.error_report = Some("".to_string()),
                        Some(s) if s.to_lowercase() == "rorreport:none" => self.error_report = Some("none".to_string()),
                        Some(s) if s.to_lowercase() == "rorreport:prompt" => self.error_report = Some("prompt".to_string()),
                        Some(s) if s.to_lowercase() == "rorreport:queue" => self.error_report = Some("queue".to_string()),
                        Some(s) if s.to_lowercase() == "rorreport:send" => self.error_report = Some("send".to_string()),
                        Some(s) => panic!("Unhandled characters in build info arg: 'er{s}'; Data: \"{args_string}\""),
                        None => panic!("Unhandled characters in build info arg: 'er'; Data: \"{args_string}\""),
                    }
                    Some('x') => match chars_iter.by_ref().take_while(|c| *c != ':' && !c.is_whitespace()).collect::<String>().as_str() {
                        "ecution-charset" => match parse_arg_string(chars_iter) {
                            Some(s) => self.execution_charset = Some(s.into()),
                            None => panic!("Unhandled characters in build info arg: 'execution-charset:'; Data: \"{args_string}\""),
                        }

                        "perimental" => match parse_arg_string(chars_iter) {
                            Some(s) if s == "log" => match parse_arg_string(chars_iter) {
                                Some(s) => self.experimental_logs_file = Some(s),
                                None => panic!("Build info arg '/experimental:log' missing file path"),
                            }

                            Some(s) if s == "deterministic" => self.set_primary_flag(ModulePrimaryFlags::ExperimentalDeterministic, true),
                            Some(s) if s == "deterministic-" => self.set_primary_flag(ModulePrimaryFlags::ExperimentalDeterministic, false),

                            Some(s) if s == "module" => self.set_primary_flag(ModulePrimaryFlags::ExperimentalModuleSupport, true),
                            Some(s) if s == "module-" => self.set_primary_flag(ModulePrimaryFlags::ExperimentalModuleSupport, false),

                            Some(s) => panic!("Unhandled characters in build info arg: 'experimental:{s}'; Data: \"{args_string}\""),
                            None => panic!("Unhandled characters in build info arg: 'experimental:'; Data: \"{args_string}\""),
                        }

                        "ternal" => match chars_iter.next() {
                            Some('a') => match parse_arg_string(chars_iter) {
                                Some(s) if s == "nglebrackets" => self.set_primary_flag(ModulePrimaryFlags::ExternalAngleBracketsHeaders, true),
                                Some(s) => panic!("Unhandled characters in build info arg: 'external:a{s}; Data: \"{args_string}\""),
                                None => panic!("Unhandled characters in build info arg: 'external:a; Data: \"{args_string}\""),
                            }
                            Some('e') => match chars_iter.by_ref().take_while(|c| *c != ':').collect::<String>().as_str() {
                                "nv" => match parse_arg_string(chars_iter) {
                                    Some(s) => self.external_headers_var = Some(s.to_string()),
                                    None => panic!("Unhandled characters in build info arg: 'external:env:'; Data: \"{args_string}\""),
                                }
                                s => panic!("Unhandled characters in build info arg: 'external:e{s}; Data: \"{args_string}\""),
                            }
                            Some('I') => match parse_arg_string(chars_iter) {
                                Some(s) => self.external_headers.push(s),
                                None => panic!("Unhandled characters in build info arg: 'external:I'; Data: \"{args_string}\""),
                            }
                            Some('W') => match parse_arg_string(chars_iter) {
                                Some(s) if s == "0" => self.external_headers_warning_level = Some(0),
                                Some(s) if s == "1" => self.external_headers_warning_level = Some(1),
                                Some(s) if s == "2" => self.external_headers_warning_level = Some(2),
                                Some(s) if s == "3" => self.external_headers_warning_level = Some(3),
                                Some(s) if s == "4" => self.external_headers_warning_level = Some(4),
                                Some(s) => panic!("Unhandled characters in build info arg: 'external:W{s}'; Data: \"{args_string}\""),
                                None => panic!("Unhandled characters in build info arg: 'external:W'; Data: \"{args_string}\""),
                            }
                            Some(c) => panic!("Unhandled characters in build info arg: 'external:{c}'; Data: \"{args_string}\""),
                            None => panic!("Unhandled characters in build info arg: 'external:'; Data: \"{args_string}\""),
                        }
                        s => panic!("Unhandled characters in build info arg: 'ex{s}'; Data: \"{args_string}\""),
                    }
                    Some(c) => panic!("Unhandled characters in build info arg: 'e{c}'; Data: \"{args_string}\""),
                    None => panic!("Unhandled characters in build info arg: 'e'; Data: \"{args_string}\""),
                }

                Some('E') => match chars_iter.next() {
                    Some('H') => match chars_iter.next() {
                        Some('a') => match chars_iter.next() {
                            None | Some(' ') => {
                                self.set_primary_flag(ModulePrimaryFlags::EnableCppExceptions, true);
                                self.set_primary_flag(ModulePrimaryFlags::EnableSehExceptions, true);
                            },
                            
                            Some('-') => match chars_iter.next() {
                                None | Some(' ') => {
                                    self.set_primary_flag(ModulePrimaryFlags::EnableCppExceptions, false);
                                    self.set_primary_flag(ModulePrimaryFlags::EnableSehExceptions, false);
                                },
                                Some(c) => panic!("Unhandled characters in build info arg: 'EHa-{c}...'; Data: \"{args_string}\""),
                            }

                            Some(c) => panic!("Unhandled characters in build info arg: 'EHa{c}...'; Data: \"{args_string}\""),
                        }

                        Some('c') => match chars_iter.next() {
                            None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::ExternCDefaultsToNothrow, true),
                            
                            Some('-') => match chars_iter.next() {
                                None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::ExternCDefaultsToNothrow, false),
                                Some(c) => panic!("Unhandled characters in build info arg: 'EHc-{c}...'; Data: \"{args_string}\""),
                            }

                            Some(c) => panic!("Unhandled characters in build info arg: 'EHc{c}...'; Data: \"{args_string}\""),
                        }

                        Some('r') => match chars_iter.next() {
                            None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::AlwaysGenerateNoexceptChecks, true),
                            
                            Some('-') => match chars_iter.next() {
                                None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::AlwaysGenerateNoexceptChecks, false),
                                Some(c) => panic!("Unhandled characters in build info arg: 'EHr-{c}...'; Data: \"{args_string}\""),
                            }

                            Some(c) => panic!("Unhandled characters in build info arg: 'EHr{c}...'; Data: \"{args_string}\""),
                        }

                        Some('s') => match chars_iter.next() {
                            None | Some(' ') => {
                                self.set_primary_flag(ModulePrimaryFlags::EnableCppExceptions, true);
                                self.set_primary_flag(ModulePrimaryFlags::EnableSehExceptions, false);
                            },
                            
                            Some('-') => match chars_iter.next() {
                                None | Some(' ') => {
                                    self.set_primary_flag(ModulePrimaryFlags::EnableCppExceptions, false);
                                    self.set_primary_flag(ModulePrimaryFlags::EnableSehExceptions, false);
                                },
                                Some(c) => panic!("Unhandled characters in build info arg: 'EHs-{c}...'; Data: \"{args_string}\""),
                            }

                            Some(c) => panic!("Unhandled characters in build info arg: 'EHs{c}...'; Data: \"{args_string}\""),
                        }

                        Some(c) => panic!("Unhandled characters in build info arg: 'EH{c}...'; Data: \"{args_string}\""),
                        None => panic!("Unhandled characters in build info arg: 'EH'; Data: \"{args_string}\""),
                    }

                    Some('P') => match chars_iter.next() {
                        None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::CopyPreprocessorOutputToStdout, true),
                        Some(c) => panic!("Unhandled characters in build info arg: 'EP{c}...'; Data: \"{args_string}\""),
                    }

                    None => self.set_primary_flag(ModulePrimaryFlags::CopyPreprocessorOutputToStdout, true),

                    Some(c) => panic!("Unhandled characters in build info arg: 'E{c}...'; Data: \"{args_string}\""),
                }

                Some('f') => match parse_arg_string(chars_iter) {
                    Some(s) if s.starts_with("avor:") => self.optimize_for_cpu_architecture = Some(s[5..].to_owned()),
                    Some(s) if s == "p:contract" => self.set_primary_flag(ModulePrimaryFlags::ConsiderFloatingPointContractions, true),
                    Some(s) if s == "p:except" => self.set_primary_flag(ModulePrimaryFlags::ConsiderFloatingPointExceptions, true),
                    Some(s) if s == "p:except-" => self.set_primary_flag(ModulePrimaryFlags::ConsiderFloatingPointExceptions, false),
                    Some(s) if s == "p:fast" => self.floating_point_model = Some("fast".to_string()),
                    Some(s) if s == "p:precise" => self.floating_point_model = Some("precise".to_string()),
                    Some(s) if s == "p:strict" => self.floating_point_model = Some("strict".to_string()),
                    Some(s) if s == "pcvt:BC" => self.floating_point_conversions = Some("BC".to_string()),
                    Some(s) if s == "pcvt:IA" => self.floating_point_conversions = Some("IA".to_string()),
                    Some(s) => panic!("Unhandled characters in build info arg: 'f{s}'; Data: \"{args_string}\""),
                    None => panic!("Unexpected character in build info arg: 'f'; Data: \"{args_string}\""),
                }

                Some('F') => match chars_iter.next() {
                    Some('a') => match parse_arg_string(chars_iter) {
                        None => self.listing_file = Some("source.asm".into()),
                        Some(s) => self.listing_file = Some(s.into()),
                    }

                    Some('A') => match chars_iter.next() {
                        None | Some(' ') => self.set_secondary_flag(ModuleSecondaryFlags::ListingFileAssemblerOnly, true),
                        Some('c') => match chars_iter.next() {
                            None | Some(' ') => self.set_secondary_flag(ModuleSecondaryFlags::ListingFileMachineCode, true),
                            Some(c) => panic!("Unhandled characters in build info arg: 'FAc{c}...'; Data: \"{args_string}\""),
                        }
                        Some('s') => match chars_iter.next() {
                            None | Some(' ') => self.set_secondary_flag(ModuleSecondaryFlags::ListingFileSourceCode, true),
                            Some(c) => panic!("Unhandled characters in build info arg: 'FAs{c}...'; Data: \"{args_string}\""),
                        }
                        Some('u') => match chars_iter.next() {
                            None | Some(' ') => self.set_secondary_flag(ModuleSecondaryFlags::ListingFileUtf8, true),
                            Some(c) => panic!("Unhandled characters in build info arg: 'FAu{c}...'; Data: \"{args_string}\""),
                        }
                        Some(c) => panic!("Unhandled characters in build info arg: 'FA{c}...'; Data: \"{args_string}\""),
                    }

                    Some('C') => match chars_iter.next() {
                        None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::FullSourcePathInDiagnostics, true),
                        Some(c) => panic!("Unhandled characters in build info arg: 'FC{c}...'; Data: \"{args_string}\""),
                    }

                    Some('d') => match parse_arg_string(chars_iter) {
                        Some(s) => self.program_database_file = Some(s),
                        None => panic!("Unhandled characters in build info arg: 'Fd'; Data: \"{args_string}\""),
                    }

                    Some('D') => match chars_iter.next() {
                        None | Some(' ') => self.set_secondary_flag(ModuleSecondaryFlags::IdeMinimalRebuild, true),
                        Some(c) => panic!("Unhandled characters in build info arg: 'FD{c}...'; Data: \"{args_string}\""),
                    }

                    Some('I') => {
                        if let Some(' ') = chars_iter.peek() {
                            chars_iter.next();
                        }

                        match parse_arg_string(chars_iter) {
                            Some(s) => self.preprocess_include_files.push(s),
                            None => panic!("Missing string from preprocess include file arg"),
                        }
                    }

                    Some('o') => match parse_arg_string(chars_iter) {
                        Some(s) => self.object_file = Some(s),
                        None => panic!("Unhandled characters in build info arg: 'Fo'; Data: \"{args_string}\""),
                    }

                    Some('p') => match parse_arg_string(chars_iter) {
                        Some(s) => self.pch_file_name = Some(crate::canonicalize_path(out_path.to_str().unwrap_or(""), root_path, s.as_str(), false)),
                        None => self.pch_file_name = Some(PathBuf::new()),
                    }

                    Some('S') => match chars_iter.next() {
                        Some(' ') | None => self.set_primary_flag(ModulePrimaryFlags::SerializePdbWithMspdbsrv, true),
                        Some(c) => panic!("Unhandled characters in build info arg: 'FS{c}...'; Data: \"{args_string}\""),
                    }

                    Some('u') => match parse_arg_string(chars_iter) {
                        Some(s) if s.starts_with("ncCache") => self.func_cache = Some(s.trim_start_matches("ncCache").parse().unwrap()),
                        Some(s) => panic!("Unexpected characters in build info arg: 'Fu{s}'; Data: \"{args_string}\""),
                        None => panic!("Unexpected characters in build info arg: 'Fu'; Data: \"{args_string}\""),
                    }

                    Some('U') => match parse_arg_string(chars_iter) {
                        Some(s) => self.forced_using_directives.push(crate::canonicalize_path(out_path.to_str().unwrap_or(""), root_path, s.as_str(), false)),
                        None => panic!("Unhandled characters in build info arg: 'FU'; Data: \"{args_string}\""),
                    }

                    Some(c) => panic!("Unhandled characters in build info arg: 'F{c}...'; Data: \"{args_string}\""),
                    None => panic!("Unexpected character in build info arg: 'F'; Data: \"{args_string}\""),
                }

                Some('g') => match parse_arg_string(chars_iter) {
                    Some(s) if s.starts_with("uard") => self.guard_string = Some(s.trim_start_matches("uard").to_string()),
                    Some(s) => panic!("Unhandled characters in build info arg: 'g{s}'; Data: \"{args_string}\""),
                    None => panic!("Unexpected character in build info arg: 'g'; Data: \"{args_string}\""),
                }

                Some('G') => match chars_iter.next() {
                    Some('d') => match chars_iter.next() {
                        None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::UseCdeclCallingConvention, true),
                        Some(c) => panic!("Unhandled characters in build info arg: 'Gd{c}...'; Data: \"{args_string}\""),
                    }

                    Some('F') => match chars_iter.next() {
                        None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::EnableStringPooling, true),
                        Some('-') => match chars_iter.next() {
                            None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::EnableStringPooling, false),
                            Some(c) => panic!("Unhandled characters in build info arg: 'GF-{c}...'; Data: \"{args_string}\""),
                        }
                        Some(c) => panic!("Unhandled characters in build info arg: 'GF{c}...'; Data: \"{args_string}\""),
                    }

                    Some('L') => match chars_iter.next() {
                        None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::EnableWholeProgramOptimization, true),
                        Some('-') => match chars_iter.next() {
                            None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::EnableWholeProgramOptimization, false),
                            Some(c) => panic!("Unhandled characters in build info arg: 'GL-{c}...'; Data: \"{args_string}\""),
                        }
                        Some(c) => panic!("Unhandled characters in build info arg: 'GL{c}...'; Data: \"{args_string}\""),
                    }

                    Some('m') => match chars_iter.next() {
                        None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::EnableMinimalRebuild, true),
                        Some('-') => match chars_iter.next() {
                            None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::EnableMinimalRebuild, false),
                            Some(c) => panic!("Unhandled characters in build info arg: 'Gm-{c}...'; Data: \"{args_string}\""),
                        }
                        Some(c) => panic!("Unhandled characters in build info arg: 'Gm{c}...'; Data: \"{args_string}\""),
                    }

                    Some('R') => {
                        match chars_iter.next() {
                            None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::EnableRtti, true),
                            Some('-') => match chars_iter.next() {
                                None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::EnableRtti, false),
                                Some(c) => panic!("Unhandled characters in build info arg: 'GR-{c}...'; Data: \"{args_string}\""),
                            }
                            Some(c) => panic!("Unhandled characters in build info arg: 'GR{c}...'; Data: \"{args_string}\""),
                        }
                    }

                    Some('s') => {
                        let mut s = String::new();

                        while let Some(c) = chars_iter.next() {
                            if c.is_whitespace() {
                                break;
                            }

                            s.push(c);
                        }

                        if s.is_empty() {
                            s = "0".into();
                        }

                        match s.parse() {
                            Ok(x) => self.stack_probe_threshold = Some(x),
                            Err(_) => panic!("Unhandled characters in build info arg: 'Gs{s}'; Data: \"{args_string}\""),
                        }
                    }

                    Some('S') => match chars_iter.next() {
                        None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::CheckBufferSecurity, true),
                        Some('-') => match chars_iter.next() {
                            None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::CheckBufferSecurity, false),
                            Some(c) => panic!("Unhandled characters in build info arg: 'GS-{c}...'; Data: \"{args_string}\""),
                        }
                        Some(c) => panic!("Unhandled characters in build info arg: 'GS{c}...'; Data: \"{args_string}\""),
                    }

                    Some('T') => match chars_iter.next() {
                        None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::FiberSafeTLS, true),
                        Some(c) => panic!("Unexpected characters in build info arg: 'GT{c}...'; Data: \"{args_string}\""),
                    }

                    Some('w') => match chars_iter.next() {
                        None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::EnableWholeProgramDataOptimization, true),
                        Some('+') => match chars_iter.next() {
                            None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::EnableWholeProgramDataOptimization, true),
                            Some(c) => panic!("Unhandled characters in build info arg: 'Gw+{c}...'; Data: \"{args_string}\""),
                        }
                        Some('-') => match chars_iter.next() {
                            None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::EnableWholeProgramDataOptimization, false),
                            Some(c) => panic!("Unhandled characters in build info arg: 'Gw-{c}...'; Data: \"{args_string}\""),
                        }
                        Some(c) => panic!("Unhandled characters in build info arg: 'Gw{c}...'; Data: \"{args_string}\""),
                    }

                    Some('y') => match chars_iter.next() {
                        None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::FunctionLevelLinking, true),
                        Some('-') => match chars_iter.next() {
                            None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::FunctionLevelLinking, false),
                            Some(c) => panic!("Unhandled characters in build info arg: 'Gy-{c}...'; Data: \"{args_string}\""),
                        }
                        Some(c) => panic!("Unhandled characters in build info arg: 'Gy{c}...'; Data: \"{args_string}\""),
                    }

                    Some('z') => match chars_iter.next() {
                        None | Some(' ') => self.set_secondary_flag(ModuleSecondaryFlags::UseStdcallCallingConvention, true),
                        Some('+') => match chars_iter.next() {
                            None | Some(' ') => self.set_secondary_flag(ModuleSecondaryFlags::UseStdcallCallingConvention, true),
                            Some(c) => panic!("Unhandled characters in build info arg: 'Gz+{c}...'; Data: \"{args_string}\""),
                        }
                        Some('-') => match chars_iter.next() {
                            None | Some(' ') => self.set_secondary_flag(ModuleSecondaryFlags::UseStdcallCallingConvention, false),
                            Some(c) => panic!("Unhandled characters in build info arg: 'Gz-{c}...'; Data: \"{args_string}\""),
                        }
                        Some(c) => panic!("Unhandled characters in build info arg: 'Gz{c}...'; Data: \"{args_string}\""),
                    }

                    Some(c) => panic!("Unhandled characters in build info arg: 'G{c}...'; Data: \"{args_string}\""),
                    None => panic!("Unexpected character in build info arg: 'G'; Data: \"{args_string}\""),
                }

                Some('h') => match parse_arg_string(chars_iter) {
                    Some(s) if s == "otpatch" => self.set_secondary_flag(ModuleSecondaryFlags::CreateHotpatchableImage, true),
                    Some(s) => panic!("Unhandled characters in build info arg: 'h{s}'; Data: \"{args_string}\""),
                    None => panic!("Unexpected character in build info arg: 'h'; Data: \"{args_string}\""),
                }

                Some('k') => match parse_arg_string(chars_iter) {
                    Some(s) if s == "ernel" => self.set_primary_flag(ModulePrimaryFlags::CreateKernelModeBinary, true),
                    Some(s) => panic!("Unhandled characters in build info arg: 'k{s}'; Data: \"{args_string}\""),
                    None => panic!("Unexpected character in build info arg: 'k'; Data: \"{args_string}\""),
                }

                Some('i') => match parse_arg_string(chars_iter) {
                    Some(s) if s == "mport_no_registry" => self.set_primary_flag(ModulePrimaryFlags::ImportNoRegistry, true),
                    Some(s) if s == "gnorePragmaWarningError" => self.set_primary_flag(ModulePrimaryFlags::IgnorePragmaWarningError, true),
                    Some(s) => panic!("Unexpected characters in build info arg: 'i{s}'; Data: \"{args_string}\""),
                    None => panic!("Unexpected character in build info arg: 'i'; Data: \"{args_string}\""),
                }

                Some('I') => match parse_arg_string(chars_iter) {
                    Some(x) => self.additional_include_dirs.push(crate::canonicalize_path(out_path.to_str().unwrap_or(""), root_path, x.as_str(), true)),
                    None => panic!("Missing string from additional include directory arg"),
                }

                Some('J') => match parse_arg_string(chars_iter) {
                    Some(s) if s == "MC" => self.set_secondary_flag(ModuleSecondaryFlags::JustMyCode, true),
                    Some(s) => panic!("Unexpected characters in build info arg: 'J{s}'; Data: \"{args_string}\""),
                    None => panic!("Unexpected character in build info arg: 'J'; Data: \"{args_string}\""),
                }

                Some('L') => match chars_iter.next() {
                    Some('D') => match chars_iter.next() {
                        None | Some(' ') => self.feature_toggles.push("LD".to_string()),
                        Some('d') => match chars_iter.next() {
                            None | Some(' ') => self.feature_toggles.push("LDd".to_string()),
                            Some(c) => panic!("Unhandled characters in build info arg: 'LDd{c}...'; Data: \"{args_string}\""),
                        }
                        Some(c) => panic!("Unhandled characters in build info arg: 'LD{c}...'; Data: \"{args_string}\""),
                    }

                    Some(c) => panic!("Unhandled characters in build info arg: 'L{c}...'; Data: \"{args_string}\""),
                    None => panic!("Unexpected character in build info arg: 'L'; Data: \"{args_string}\""),
                }

                Some('M') => match parse_arg_string(chars_iter) {
                    Some(s) if s.starts_with("D") => match &s[1..] {
                        s if s.is_empty() => self.feature_toggles.push("MD".to_string()),
                        "d" => self.feature_toggles.push("MDd".to_string()),
                        s => panic!("Unhandled characters in build info arg: 'MD{s}'; Data: \"{args_string}\""),
                    }

                    Some(s) if s.starts_with("P") => match &s[1..] {
                        s if s.is_empty() => self.build_process_count = Some(1),
                        s => match s.parse::<usize>() {
                            Ok(x) => self.build_process_count = Some(x),
                            Err(_) => panic!("Unhandled characters in build info arg: 'MP{s}'; Data: \"{args_string}\""),
                        }
                    }

                    Some(s) if s.starts_with("T") => match &s[1..] {
                        s if s.is_empty() => self.feature_toggles.push("MT".to_string()),
                        "d" => self.feature_toggles.push("MTd".to_string()),
                        s => panic!("Unhandled characters in build info arg: 'MT{s}'; Data: \"{args_string}\""),
                    }

                    Some(c) => panic!("Unhandled characters in build info arg: 'M{c}...'; Data: \"{args_string}\""),
                    None => panic!("Unexpected character in build info arg: 'M'; Data: \"{args_string}\""),
                }

                Some('n') => match parse_arg_string(chars_iter) {
                    Some(s) if s == "ocfgrngchk" => self.set_secondary_flag(ModuleSecondaryFlags::NoCfgRngChk, true),
                    Some(s) if s == "ologo" => self.set_primary_flag(ModulePrimaryFlags::Nologo, true),
                    Some(s) if s == "odatetime" => self.set_secondary_flag(ModuleSecondaryFlags::NoDateTime, true),
                    Some(s) if s == "othreadsafestatics" => self.set_primary_flag(ModulePrimaryFlags::NoThreadSafeStatics, true),
                    Some(s) => panic!("Unhandled characters in build info arg: 'n{s}'; Data: \"{args_string}\""),
                    None => panic!("Unexpected character in build info arg: 'n'; Data: \"{args_string}\""),
                }

                Some('o') => match parse_arg_string(chars_iter) {
                    Some(s) if s == "penmp" => self.openmp = Some("omp".to_string()),
                    Some(s) if s == "penmp-" => self.openmp = None,
                    Some(s) if s == "penmp:experimental" => self.openmp = Some("experimental".to_string()),
                    Some(s) if s == "penmp:llvm" => self.openmp = Some("llvm".to_string()),
                    Some(s) if s == "ptions:strict" => self.set_primary_flag(ModulePrimaryFlags::UnknownCompilerOptionsAreErrors, true),
                    Some(s) => panic!("Unhandled characters in build info arg: 'o{s}'; Data: \"{args_string}\""),
                    None => panic!("Unexpected character in build info arg: 'o'; Data: \"{args_string}\""),
                }

                Some('O') => match chars_iter.next() {
                    Some('1') => match chars_iter.next() {
                        None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::CreateSmallCode, true),
                        Some(c) => panic!("Unhandled characters in build info arg: 'O1{c}...'; Data: \"{args_string}\""),
                    }

                    Some('2') => match chars_iter.next() {
                        None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::CreateFastCode, true),
                        Some(c) => panic!("Unhandled characters in build info arg: 'O2{c}...'; Data: \"{args_string}\""),
                    }

                    Some('b') => match chars_iter.next() {
                        Some('0') => match chars_iter.next() {
                            None | Some(' ') => self.inline_function_expansion = Some(0),
                            Some(c) => panic!("Unhandled characters in build info arg: 'Ob0{c}...'; Data: \"{args_string}\""),
                        }
                        
                        Some('1') => match chars_iter.next() {
                            None | Some(' ') => self.inline_function_expansion = Some(1),
                            Some(c) => panic!("Unhandled characters in build info arg: 'Ob1{c}...'; Data: \"{args_string}\""),
                        }

                        Some('2') => match chars_iter.next() {
                            None | Some(' ') => self.inline_function_expansion = Some(2),
                            Some(c) => panic!("Unhandled characters in build info arg: 'Ob2{c}...'; Data: \"{args_string}\""),
                        }
                        
                        Some('3') => match chars_iter.next() {
                            None | Some(' ') => self.inline_function_expansion = Some(3),
                            Some(c) => panic!("Unhandled characters in build info arg: 'Ob3{c}...'; Data: \"{args_string}\""),
                        }
                        
                        Some(c) => panic!("Unhandled characters in build info arg: 'Ob{c}...'; Data: \"{args_string}\""),
                        None => panic!("Unhandled characters in build info arg: 'Ob'; Data: \"{args_string}\""),
                    }

                    Some('d') => match chars_iter.next() {
                        None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::OptimizationDisabled, true),
                        Some(c) => panic!("Unhandled characters in build info arg: 'Od{c}...'; Data: \"{args_string}\""),
                    }

                    Some('i') => match chars_iter.next() {
                        None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::GenerateIntrinsicFunctions, true),
                        Some('-') => match chars_iter.next() {
                            None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::GenerateIntrinsicFunctions, false),
                            Some(c) => panic!("Unhandled characters in build info arg: 'Oi-{c}...'; Data: \"{args_string}\""),
                        }
                        Some(c) => panic!("Unhandled characters in build info arg: 'Oi{c}...'; Data: \"{args_string}\""),
                    }

                    Some('s') => match chars_iter.next() {
                        None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::FavorsSmallCode, true),
                        Some(c) => panic!("Unhandled characters in build info arg: 'Os{c}...'; Data: \"{args_string}\""),
                    }

                    Some('t') => match chars_iter.next() {
                        None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::FavorsFastCode, true),
                        Some(c) => panic!("Unhandled characters in build info arg: 'Ot{c}...'; Data: \"{args_string}\""),
                    }

                    Some('x') => match chars_iter.next() {
                        None | Some(' ') => {
                            // TODO: O2 without GF or Gy
                        }
                        Some('-') => match chars_iter.next() {
                            None | Some(' ') => {
                                // TODO: O2 without GF or Gy
                            }
                            Some(c) => panic!("Unhandled characters in build info arg: 'Ox-{c}...'; Data: \"{args_string}\""),
                        }
                        Some(c) => panic!("Unhandled characters in build info arg: 'Ox{c}...'; Data: \"{args_string}\""),
                    }

                    Some('y') => match chars_iter.next() {
                        None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::FramePointerEmission, true),
                        Some('-') => match chars_iter.next() {
                            None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::FramePointerEmission, false),
                            Some(c) => panic!("Unhandled characters in build info arg: 'Oy-{c}...'; Data: \"{args_string}\""),
                        }
                        Some(c) => panic!("Unhandled characters in build info arg: 'Oy{c}...'; Data: \"{args_string}\""),
                    }

                    Some(c) => panic!("Unhandled characters in build info arg: 'O{c}...'; Data: \"{args_string}\""),
                    None => panic!("Unexpected character in build info arg: 'O'; Data: \"{args_string}\""),
                }

                Some('p') => match parse_arg_string(chars_iter) {
                    Some(s) if s == "ermissive" => self.set_primary_flag(ModulePrimaryFlags::Permissive, true),
                    Some(s) if s == "ermissive-" => self.set_primary_flag(ModulePrimaryFlags::Permissive, false),
                    Some(s) => panic!("Unhandled characters in build info arg: 'p{s}'; Data: \"{args_string}\""),
                    None => panic!("Unexpected character in build info arg: 'p'; Data: \"{args_string}\""),
                }

                Some('Q') => match parse_arg_string(chars_iter) {
                    Some(s) if s == "fast_transcendentals" => self.set_secondary_flag(ModuleSecondaryFlags::FastTranscendentals, true),
                    Some(s) if s == "imprecise_fwaits" => self.set_secondary_flag(ModuleSecondaryFlags::RemoveFwaitInTryBlocks, true),
                    Some(s) if s == "Ifist" => self.set_secondary_flag(ModuleSecondaryFlags::SuppressFloatToIntegralHelperCall, true),
                    Some(s) if s == "Intel-jcc-erratum" => self.set_secondary_flag(ModuleSecondaryFlags::MitigateIntelJccErratumUpdatePerformance, true),
                    Some(s) if s.starts_with("par-report:") => self.loop_parallelization_report_level = Some(s.trim_start_matches("par-report:").parse().unwrap()),
                    Some(s) if s == "par" => self.set_primary_flag(ModulePrimaryFlags::EnableLoopParallelization, true),
                    Some(s) if s == "safe_fp_loads" => self.set_secondary_flag(ModuleSecondaryFlags::SafeFpLoads, true),
                    Some(s) if s == "slpvec-" => self.set_secondary_flag(ModuleSecondaryFlags::DisableSlpVec, true),
                    Some(s) if s == "spectre" => self.set_primary_flag(ModulePrimaryFlags::MitigateSpectreVulnerabilities, true),
                    Some(s) if s == "spectre-" => self.set_primary_flag(ModulePrimaryFlags::MitigateSpectreVulnerabilities, false),
                    Some(s) if s == "spectre-load" => self.set_secondary_flag(ModuleSecondaryFlags::GenerateSerializingForLoads, true),
                    Some(s) if s == "spectre-load-cf" => self.set_secondary_flag(ModuleSecondaryFlags::GenerateSerializingForControlFlowLoads, true),
                    Some(s) if s == "vec-" => self.set_secondary_flag(ModuleSecondaryFlags::DisableVec, true),
                    Some(s) if s == "vec-mathlib-" => self.set_secondary_flag(ModuleSecondaryFlags::DisableVecMathLib, true),
                    Some(s) if s.starts_with("vec-report:") => self.vec_report_level = Some(s.trim_start_matches("vec-report:").parse().unwrap()),
                    Some(s) if s == "vec-sse2only" => self.set_secondary_flag(ModuleSecondaryFlags::VecSSE2Only, true),
                    Some(s) => panic!("Unhandled characters in build info arg: 'Q{s}'; Data: \"{args_string}\""),
                    None => panic!("Unexpected character in build info arg: 'Q'; Data: \"{args_string}\""),
                }

                Some('R') => match parse_arg_string(chars_iter) {
                    Some(s) if s == "TC1" || s == "TCsu" => self.set_primary_flag(ModulePrimaryFlags::EnableFastRuntimeChecks, true),
                    Some(s) if s == "TCc" => self.set_primary_flag(ModulePrimaryFlags::ConvertToSmallerTypeCheckAtRuntime, true),
                    Some(s) if s == "TCs" => self.set_primary_flag(ModulePrimaryFlags::EnableStackFrameRuntimeChecks, true),
                    Some(s) if s == "TCu" => self.set_primary_flag(ModulePrimaryFlags::EnableUninitializedLocalUsageChecks, true),
                    Some(s) => panic!("Unhandled characters in build info arg: 'R{s}'; Data: \"{args_string}\""),
                    None => panic!("Unhandled characters in build info arg: 'R'; Data: \"{args_string}\""),
                }

                Some('s') => match parse_arg_string(chars_iter) {
                    Some(s) if s == "dl" => self.set_primary_flag(ModulePrimaryFlags::EnableSdl, true),
                    Some(s) if s == "dl-" => self.set_primary_flag(ModulePrimaryFlags::EnableSdl, false),
                    Some(s) if s == "howIncludes" => self.set_secondary_flag(ModuleSecondaryFlags::ShowIncludes, true),
                    Some(s) if s.starts_with("ource-charset:") => self.source_charset = Some(s.trim_start_matches("ource-charset:").into()),
                    Some(s) if s.starts_with("td:") => self.language_standard = Some(s.trim_start_matches("td:").to_string()),
                    Some(s) => panic!("Unhandled characters in build info arg: 's{s}'; Data: \"{args_string}\""),
                    None => panic!("Unhandled characters in build info arg: 's'; Data: \"{args_string}\""),
                }

                Some('t') => match parse_arg_string(chars_iter) {
                    Some(s) if s.starts_with("rimfile:") => self.trim_files.push(s.trim_start_matches("rimfile:").into()),
                    Some(s) => panic!("Unhandled characters in build info arg: 't{s}'; Data: \"{args_string}\""),
                    None => panic!("Unexpected character in build info arg: 't'; Data: \"{args_string}\""),
                }
                
                Some('T') => match chars_iter.next() {
                    Some('C') => match chars_iter.next() {
                        None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::UseCSourceFileType, true),
                        Some(c) => panic!("Unhandled characters in build info arg: 'TC{c}...'; Data: \"{args_string}\""),
                    }

                    Some('P') => match chars_iter.next() {
                        None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::UseCppSourceFileType, true),
                        Some(c) => panic!("Unhandled characters in build info arg: 'TP{c}...'; Data: \"{args_string}\""),
                    }

                    Some(c) => panic!("Unhandled characters in build info arg: 'T{c}...'; Data: \"{args_string}\""),
                    None => panic!("Unexpected character in build info arg: 'T'; Data: \"{args_string}\""),
                }

                Some('u') => match parse_arg_string(chars_iter) {
                    Some(s) if s == "tf-8" => {
                        self.source_charset = Some("utf-8".into());
                        self.execution_charset = Some("utf-8".into());
                    }
                    Some(s) => panic!("Unhandled characters in build info arg: 'u{s}'; Data: \"{args_string}\""),
                    None => panic!("Unexpected character in build info arg: 'u'; Data: \"{args_string}\""),
                }

                Some('v') => match parse_arg_string(chars_iter) {
                    Some(s) if s == "alidate-charset" => self.set_secondary_flag(ModuleSecondaryFlags::ValidateUtf8Charset, true),
                    Some(s) if s == "c7dname" => self.set_secondary_flag(ModuleSecondaryFlags::Vc7DName, true),
                    Some(s) if s == "ersionLKG171" => self.set_secondary_flag(ModuleSecondaryFlags::VersionLKG171, true),
                    Some(s) => panic!("Unhandled characters in build info arg: 'v{s}'; Data: \"{args_string}\""),
                    None => panic!("Unhandled character in build info arg: 'v'; Data: \"{args_string}\""),
                }

                Some('w') => match chars_iter.next() {
                    Some('1') => self.warnings_level1.push(parse_arg_string(chars_iter).unwrap_or_default()),
                    Some('2') => self.warnings_level1.push(parse_arg_string(chars_iter).unwrap_or_default()),
                    Some('3') => self.warnings_level1.push(parse_arg_string(chars_iter).unwrap_or_default()),
                    Some('4') => self.warnings_level1.push(parse_arg_string(chars_iter).unwrap_or_default()),

                    Some('a') => match parse_arg_string(chars_iter) {
                        Some(s) if s == "rningLKG171" => self.set_secondary_flag(ModuleSecondaryFlags::WarningLKG171, true),
                        Some(s) => panic!("Unhandled characters in build info arg: 'wa{s}'; Data: \"{args_string}\""),
                        None => panic!("Unhandled characters in build info arg: 'wa'; Data: \"{args_string}\""),
                    }

                    Some('d') => match parse_arg_string(chars_iter) {
                        Some(s) => self.disabled_warnings.push(s),
                        None => panic!("Unhandled characters in build info arg: 'wd'; Data: \"{args_string}\""),
                    }

                    Some('l') => match parse_arg_string(chars_iter) {
                        Some(s) => self.logged_warnings.push(s),
                        None => panic!("Unhandled characters in build info arg: 'wl'; Data: \"{args_string}\""),
                    }

                    Some('e') => match parse_arg_string(chars_iter) {
                        Some(s) => self.warnings_as_errors.push(s),
                        None => panic!("Unhandled characters in build info arg: 'we'; Data: \"{args_string}\""),
                    }

                    Some(c) => panic!("Unhandled characters in build info arg: 'w{c}...'; Data: \"{args_string}\""),
                    None => panic!("Unhandled character in build info arg: 'w'; Data: \"{args_string}\""),
                }

                Some('W') => match chars_iter.next() {
                    Some('0') => match chars_iter.next() {
                        None | Some(' ') => self.output_warning_level = Some(0),
                        Some(c) => panic!("Unhandled characters in build info arg: 'W0{c}...'; Data: \"{args_string}\""),
                    }

                    Some('1') => match chars_iter.next() {
                        None | Some(' ') => self.output_warning_level = Some(1),
                        Some(c) => panic!("Unhandled characters in build info arg: 'W1{c}...'; Data: \"{args_string}\""),
                    }

                    Some('2') => match chars_iter.next() {
                        None | Some(' ') => self.output_warning_level = Some(2),
                        Some(c) => panic!("Unhandled characters in build info arg: 'W2{c}...'; Data: \"{args_string}\""),
                    }

                    Some('3') => match chars_iter.next() {
                        None | Some(' ') => self.output_warning_level = Some(3),
                        Some(c) => panic!("Unhandled characters in build info arg: 'W3{c}...'; Data: \"{args_string}\""),
                    }

                    Some('4') => match chars_iter.next() {
                        None | Some(' ') => self.output_warning_level = Some(4),
                        Some(c) => panic!("Unhandled characters in build info arg: 'W4{c}...'; Data: \"{args_string}\""),
                    }

                    Some('a') => match parse_arg_string(chars_iter) {
                        Some(s) if s == "ll" => self.set_primary_flag(ModulePrimaryFlags::EnableAllWarnings, true),
                        x => panic!("Unhandled characters in build info arg: 'Wa{}'; Data: \"{args_string}\"", x.unwrap_or_default()),
                    }

                    Some('v') => match parse_arg_string(chars_iter) {
                        Some(s) if s.starts_with(':') => self.disable_warnings_after_version = Some(s.to_string()),
                        Some(s) => panic!("Unhandled characters in build info arg: 'Wv{s}'; Data: \"{args_string}\""),
                        None => panic!("Unhandled characters in build info arg: 'Wv'; Data: \"{args_string}\""),
                    }

                    Some('X') => match chars_iter.next() {
                        None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::TreatWarningsAsErrors, true),
                        Some('-') => match chars_iter.next() {
                            None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::TreatWarningsAsErrors, false),
                            Some(c) => panic!("Unhandled characters in build info arg: 'WX-{c}...'; Data: \"{args_string}\""),
                        }
                        Some(c) => panic!("Unhandled characters in build info arg: 'WX{c}...'; Data: \"{args_string}\""),
                    }

                    Some(c) => panic!("Unhandled characters in build info arg: 'W{c}...'; Data: \"{args_string}\""),
                    None => panic!("Unexpected character in build info arg: 'W'; Data: \"{args_string}\""),
                }

                Some('X') => match chars_iter.next() {
                    None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::IgnoreStandardIncludeDir, true),
                    Some(c) => panic!("Unhandled characters in build info arg: 'X{c}...'; Data: \"{args_string}\""),
                }

                Some('Y') => match chars_iter.next() {
                    Some('c') => match parse_arg_string(chars_iter) {
                        Some(s) => self.pch_file_name = Some(crate::canonicalize_path(out_path.to_str().unwrap_or(""), root_path, s.as_str(), false)),
                        None => self.pch_file_name = Some(PathBuf::new()),
                    }

                    Some('l') => match parse_arg_string(chars_iter) {
                        None => self.set_primary_flag(ModulePrimaryFlags::InjectPchReference, true),
                        Some(s) if s.is_empty() => self.set_primary_flag(ModulePrimaryFlags::InjectPchReference, true),
                        Some(s) if s == "-" => self.set_primary_flag(ModulePrimaryFlags::InjectPchReference, false),
                        Some(s) => self.pch_references.push(s),
                    }

                    Some('u') => match parse_arg_string(chars_iter) {
                        Some(s) => self.precompiled_header_file_name = Some(s),
                        None => panic!("Unhandled characters in build info arg: 'Yu'; Data: \"{args_string}\""),
                    }

                    Some(c) => panic!("Unhandled characters in build info arg: 'Y{c}...'; Data: \"{args_string}\""),
                    None => panic!("Unexpected character in build info arg: 'Y'; Data: \"{args_string}\""),
                }

                Some('Z') => match chars_iter.next() {
                    Some('7') => match chars_iter.next() {
                        None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::GenerateC7DebugInfo, true),
                        Some(c) => panic!("Unhandled characters in build info arg: 'Z7{c}...'; Data: \"{args_string}\""),
                    }

                    Some('c') => match chars_iter.next() {
                        Some(':') => match parse_arg_string(chars_iter) {
                            Some(x) => self.feature_toggles.push(x),
                            None => panic!("Missing identifier from feature toggle"),
                        }

                        Some(c) => panic!("Unhandled characters in build info arg: 'Zc{c}...'; Data: \"{args_string}\""),
                        None => panic!("Unhandled characters in build info arg: 'Zc'; Data: \"{args_string}\""),
                    }

                    Some('f') => match chars_iter.next() {
                        None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::FasterPdbGeneration, true),
                        Some(c) => panic!("Unhandled characters in build info arg: 'Zf{c}...'; Data: \"{args_string}\""),
                    }

                    Some('H') => match parse_arg_string(chars_iter) {
                        Some(s) if s.starts_with(':') => self.hash_algorithm = Some(s.trim_start_matches(':').to_string()),
                        Some(s) => panic!("Unhandled characters in build info arg: 'ZH{s}'; Data: \"{args_string}\""),
                        None => panic!("Unhandled characters in build info arg: 'ZH'; Data: \"{args_string}\""),
                    }

                    Some('i') => match chars_iter.next() {
                        None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::GenerateFullDebugInfo, true),
                        Some('+') => match chars_iter.next() {
                            None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::GenerateFullDebugInfo, true),
                            Some(c) => panic!("Unhandled characters in build info arg: 'Zi+{c}...'; Data: \"{args_string}\""),
                        }
                        Some('-') => match chars_iter.next() {
                            None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::GenerateFullDebugInfo, false),
                            Some(c) => panic!("Unhandled characters in build info arg: 'Zi-{c}...'; Data: \"{args_string}\""),
                        }
                        Some(c) => panic!("Unhandled characters in build info arg: 'Zi{c}...'; Data: \"{args_string}\""),
                    }

                    Some('I') => match chars_iter.next() {
                        None | Some(' ') => self.set_secondary_flag(ModuleSecondaryFlags::GenerateEditAndContinueDebugInfo, true),
                        Some('+') => match chars_iter.next() {
                            None | Some(' ') => self.set_secondary_flag(ModuleSecondaryFlags::GenerateEditAndContinueDebugInfo, true),
                            Some(c) => panic!("Unhandled characters in build info arg: 'ZI+{c}...'; Data: \"{args_string}\""),
                        }
                        Some('-') => match chars_iter.next() {
                            None | Some(' ') => self.set_secondary_flag(ModuleSecondaryFlags::GenerateEditAndContinueDebugInfo, false),
                            Some(c) => panic!("Unhandled characters in build info arg: 'ZI-{c}...'; Data: \"{args_string}\""),
                        }
                        Some(c) => panic!("Unhandled characters in build info arg: 'ZI{c}...'; Data: \"{args_string}\""),
                    }

                    Some('l') => match chars_iter.next() {
                        None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::RemoveDefaultLibraryName, true),
                        Some(c) => panic!("Unhandled characters in build info arg: 'Zl{c}...'; Data: \"{args_string}\""),
                    }

                    Some('m') => match parse_arg_string(chars_iter) {
                        Some(s) => match s.parse::<usize>() {
                            Ok(x) => self.precompiled_header_memory = Some(x),
                            Err(_) => panic!("Unhandled characters in build info arg: 'Zm{s}'; Data: \"{args_string}\""),
                        }
                        None => panic!("Unexpected characters in build info arg: 'Zm'; Data: \"{args_string}\""),
                    }

                    Some('o') => match chars_iter.next() {
                        None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::GenerateRicherDebugInfoForOptimizedCode, true),
                        Some('-') => match chars_iter.next() {
                            None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::GenerateRicherDebugInfoForOptimizedCode, false),
                            Some(c) => panic!("Unhandled characters in build info arg: 'Zo-{c}...'; Data: \"{args_string}\""),
                        }
                        Some(c) => panic!("Unhandled characters in build info arg: 'Zo{c}...'; Data: \"{args_string}\""),
                    }

                    Some('p') => match parse_arg_string(chars_iter) {
                        Some(x) => self.pack_structure_members = Some(x),
                        None => panic!("Missing identifier from feature toggle"),
                    }

                    Some('W') => match chars_iter.next() {
                        None | Some(' ') => self.set_primary_flag(ModulePrimaryFlags::WindowsRuntimeCompilation, true),
                        
                        Some(':') => match parse_arg_string(chars_iter) {
                            Some(s) if s == "nostdlib" => self.set_primary_flag(ModulePrimaryFlags::WindowsRuntimeCompilationNostdlib, true),
                            Some(s) if s == "nometadata" => self.set_secondary_flag(ModuleSecondaryFlags::WindowsRuntimeNoMetadata, true),
                            Some(s) => panic!("Unhandled characters in build info arg: 'ZW:{s}'; Data: \"{args_string}\""),
                            None => panic!("Unhandled characters in build info arg: 'ZW:'; Data: \"{args_string}\""),
                        }

                        Some(c) => panic!("Unhandled characters in build info arg: 'ZW{c}...'; Data: \"{args_string}\""),
                    }

                    Some(c) => panic!("Unhandled characters in build info arg: 'Z{c}...'; Data: \"{args_string}\""),
                    None => panic!("Unexpected character in build info arg: 'Z'; Data: \"{args_string}\""),
                }

                Some(c) => panic!("Unexpected character in build info arg: '{c}...'; Data: \"{args_string}\""),
            }
        }
    }
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let has_explicit_empty_lines = self.members.iter().any(|m| matches!(m, ModuleMember::EmptyLine));
        let mut skip_empty_line = true;

        if self.is_header() && !self.members.iter().any(|m| {
            let ModuleMember::Preprocessor(p) = m else {
                return false;
            };
            p == "pragma once"
        }) {
            if !has_explicit_empty_lines {
                skip_empty_line = false;
            }
            writeln!(f, "#pragma once")?;
        }

        for (header, is_global) in self.headers.iter() {
            if *is_global {
                writeln!(f, "#include <{}>", header.to_string_lossy().trim_start_matches('/'))?;
            } else {
                writeln!(f, "#include \"{}\"", header.to_string_lossy().trim_start_matches('/'))?;
            }
        }

        let mut prev_item: Option<&ModuleMember> = None;

        for item in self.members.iter() {
            if !matches!(
                (prev_item, item),
                (
                    Some(ModuleMember::Tagged(_, _)),
                    ModuleMember::Procedure(cpp::Procedure { .. }) | ModuleMember::Tagged(_, _)
                )
            ) {
                if !matches!(
                    (prev_item, item),
                    (
                        Some(ModuleMember::Preprocessor(_)),
                        ModuleMember::Preprocessor(_)
                    ) | (
                        Some(ModuleMember::Include(_, _)),
                        ModuleMember::Include(_, _)
                    ) | (
                        Some(ModuleMember::UsingNamespace(_)),
                        ModuleMember::UsingNamespace(_)
                    ) | (
                        Some(ModuleMember::Procedure(cpp::Procedure { body: None, .. })),
                        ModuleMember::Procedure(cpp::Procedure { body: None, .. })
                    ) | (
                        Some(ModuleMember::TypeDefinition(_)),
                        ModuleMember::TypeDefinition(_)
                    ) | (
                        Some(
                            ModuleMember::Constant(_)
                                | ModuleMember::Data { .. }
                                | ModuleMember::ThreadStorage { .. }
                        ),
                        ModuleMember::Constant(_)
                            | ModuleMember::Data { .. }
                            | ModuleMember::ThreadStorage { .. }
                    )
                ) {
                    if !skip_empty_line {
                        writeln!(f)?;
                    }
                    
                    if !has_explicit_empty_lines {
                        skip_empty_line = false;
                    }
                }
            }
            

            item.fmt(f)?;
            writeln!(f)?;

            prev_item = Some(item);
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test() {
        let args_string = "";
        let mut chars_iter = args_string.chars().peekable();
        let mut module = super::Module::default();
        module.parse_arguments(
            &std::path::PathBuf::from("/Users/camden/Source/pdb-decompiler/out/test/"),
            "",
            args_string,
            &mut chars_iter
        );
    }
}
