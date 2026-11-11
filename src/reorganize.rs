use crate::cpp;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[inline(always)]
pub fn collect_compound_enums(
    modules: &mut HashMap<String, Rc<RefCell<cpp::Module>>>,
    compound_enums: &mut Vec<(cpp::Enum, cpp::TypeDefinition)>,
) {
    for module in modules.values().cloned().collect::<Vec<_>>() {
        for member in module.borrow().members.clone() {
            if let cpp::ModuleMember::Enum(enum_definition) = member
                && let Some(type_definition) = find_enum_or_flags_typedef(modules, &enum_definition)
            {
                compound_enums.push((enum_definition.clone(), type_definition));
            }
        }
    }
}

#[inline(always)]
fn find_enum_or_flags_typedef<'a>(
    modules: &mut HashMap<String, Rc<RefCell<cpp::Module>>>,
    enum_data: &cpp::Enum,
) -> Option<cpp::TypeDefinition> {
    for module in modules.values() {
        for member in module.borrow().members.iter() {
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

            if type_definition.signature.starts_with(format!("c_enum<enum {},", enum_data.name).as_str()) {
                let mut result = type_definition.clone();
                result.signature = format!("c_enum<{}", result.signature.trim_start_matches("c_enum<enum "));

                let Some((type_string, name_string)) = result.signature.rsplit_once(' ') else {
                    panic!("Unexpected typedef type name string: \"{}\"", result.signature);
                };

                let (type_string, suffix) = type_string.trim_start_matches("c_enum<").rsplit_once('>').unwrap();

                let mut template_args = type_string
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

                result.signature = format!("c_enum<{}>{} {}", template_args.join(","), suffix, name_string);

                return Some(result);
            }

            if type_definition.signature.starts_with(format!("c_flags<enum {},", enum_data.name).as_str()) {
                let mut result = type_definition.clone();
                result.signature = format!("c_flags<{}", result.signature.trim_start_matches("c_flags<enum "));

                let Some((type_string, name_string)) = result.signature.rsplit_once(' ') else {
                    panic!("Unexpected typedef type name string: \"{}\"", result.signature);
                };

                let (type_string, suffix) = type_string.trim_start_matches("c_flags<").rsplit_once('>').unwrap();

                let mut template_args = type_string
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

                result.signature = format!("c_flags<{}>{} {}", template_args.join(","), suffix, name_string);

                return Some(result);
            }

            if type_definition.signature.starts_with(format!("c_flags_no_init<enum {},", enum_data.name).as_str()) {
                let mut result = type_definition.clone();
                result.signature = format!("c_flags_no_init<{}", result.signature.trim_start_matches("c_flags_no_init<enum "));

                let Some((type_string, name_string)) = result.signature.rsplit_once(' ') else {
                    panic!("Unexpected typedef type name string: \"{}\"", result.signature);
                };

                let (type_string, suffix) = type_string.trim_start_matches("c_flags_no_init<").rsplit_once('>').unwrap();

                let mut template_args = type_string
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

                result.signature = format!("c_flags_no_init<{}>{} {}", template_args.join(","), suffix, name_string);

                return Some(result);
            }
        }
    }

    None
}

/// Extracts the variable name from a rendered C++ declaration.
///
/// The name is the rightmost identifier-like run of characters, handling simple
/// declarations as well as pointers/references and function-pointer declarators:
///
/// e.g. `s_model_variant &variant` -> `variant`,
///      `Server::Voting::AbstractVotingSystem *elem` -> `elem`,
///      `unsigned __int32 result` -> `result`,
///      `void (*&handler)()` -> `handler`,
///      `char buffer[8192]` -> `buffer`.
fn variable_identifier(signature: &str) -> String {
    fn is_start(c: char) -> bool {
        c.is_alphabetic() || c == '_' || c == '$' || c == '<'
    }
    fn is_continue(c: char) -> bool {
        c.is_alphanumeric() || c == '_' || c == '$' || c == '<' || c == '>'
    }

    let chars: Vec<char> = signature.trim_end().chars().collect();
    let mut i = chars.len();

    while i > 0 {
        if !is_continue(chars[i - 1]) {
            i -= 1;
            continue;
        }

        // Walk back to the start of this run of name-continue characters.
        let mut start = i - 1;
        while start > 0 && is_continue(chars[start - 1]) {
            start -= 1;
        }

        // The run is only a valid name if it begins with a name-start character
        // (this skips pure numbers such as an array size).
        if is_start(chars[start]) {
            return chars[start..i].iter().collect();
        }

        i = start;
    }

    String::new()
}

/// Returns the declaration's type by stripping the trailing variable name token.
///
/// e.g. `std::vector<int> values` -> `std::vector<int>`,
///      `unsigned __int32 i` -> `unsigned __int32`,
///      `s_model_variant &variant` -> `s_model_variant`.
fn declaration_type(signature: &str) -> String {
    let trimmed = signature.trim();
    match trimmed.rfind(' ') {
        Some(pos) => trimmed[..pos].trim_end().to_string(),
        None => String::new(),
    }
}

/// Recognizes the temporaries MSVC emits for a range-based `for` loop:
/// `<range>$L0`, `<begin>$L0` and `<end>$L0`. The marker is searched for in the
/// full signature (named iterator temporaries can otherwise be embedded in a
/// function-pointer declarator such as `void (**<begin>$L0)()`).
///
/// Returns `(marker, scope, full_name)` where `marker` is one of
/// `range`/`begin`/`end`, `scope` is the `$L<digits>` portion shared by all three
/// declarations (used to match them up when several loops live in one function),
/// and `full_name` is the whole matched temporary name (e.g. `<range>$L0`).
fn iterator_marker(signature: &str) -> Option<(&'static str, String, String)> {
    for marker in ["range", "begin", "end"] {
        let tag = format!("<{marker}>");
        if let Some(pos) = signature.find(&tag) {
            let after = &signature[pos + tag.len()..];
            let scope = after
                .strip_prefix("$L")
                .map(|rest| {
                    let digits: String = rest
                        .chars()
                        .take_while(|c| c.is_ascii_digit())
                        .collect();
                    format!("$L{digits}")
                })
                .unwrap_or_default();
            let full_name = format!("{tag}{scope}");
            return Some((marker, scope, full_name));
        }
    }
    None
}

/// Returns true if `name` is a MSVC scope temporary (`$S<N>`), the lifetime
/// extended storage the compiler materializes when a loop iterates over a
/// temporary collection.
fn is_scope_temp(name: &str) -> bool {
    name.strip_prefix("$S")
        .map(|rest| !rest.is_empty() && rest.bytes().all(|b| b.is_ascii_digit()))
        .unwrap_or(false)
}

/// Chooses the collection's length accessor for a C-style `for` loop based on the
/// collection type: std containers use `.size()`, rapidjson uses `.Size()`, and
/// Bungie/Halo tag blocks expose a `.count` field.
fn size_accessor(collection_type: &str) -> &'static str {
    if collection_type.starts_with("std::") {
        ".size()"
    } else if collection_type.contains("rapidjson") {
        ".Size()"
    } else {
        ".count"
    }
}

/// Builds a `type -> name` map for the module's global/static variables so that
/// a range-for's collection can be resolved back to the variable it iterates.
fn collect_global_names(module: &cpp::Module) -> HashMap<String, String> {
    let mut map = HashMap::new();

    for member in &module.members {
        let (name, signature) = match member {
            cpp::ModuleMember::Data { name, signature, .. }
            | cpp::ModuleMember::ThreadStorage { name, signature, .. } => (name, signature),
            _ => continue,
        };

        // Signatures carry a trailing `"; // 0x..."` address comment.
        let declaration = signature.split(';').next().unwrap_or(signature).trim();
        let ty = declaration_type(declaration);
        if !ty.is_empty() {
            map.insert(ty, name.clone());
        }
    }

    map
}

/// Builds a `type -> name` map for the local variables declared in a function
/// body. Used as a fallback when a range-for's collection is a local rather than
/// a global.
fn collect_local_names(block: &cpp::Block, map: &mut HashMap<String, String>) {
    for statement in &block.statements {
        match statement {
            cpp::Statement::Variable(variable) => {
                // Range-for temporaries (`<range>$L0`, `<begin>$L0`, `<end>$L0`)
                // share the collection's type via a reference; their compiler
                // generated names are meaningless, so skip them rather than let
                // them shadow the real collection name (or the `collection`
                // fallback).
                if iterator_marker(&variable.signature).is_some() {
                    continue;
                }

                let name = variable_identifier(&variable.signature);
                // Scope temporaries (`$S<N>`) are lifetime-extended copies of a
                // temporary collection; their names are meaningless too.
                if is_scope_temp(&name) {
                    continue;
                }

                let ty = declaration_type(&variable.signature);
                if !ty.is_empty() {
                    map.entry(ty).or_insert_with(|| name);
                }
            }
            cpp::Statement::Block(child) => collect_local_names(child, map),
            _ => {}
        }
    }
}

/// Detects a collapsed `for` loop at the start of `statements` and, when found,
/// returns the loop's comment line and its (still populated) body.
///
/// A loop scope contains the three MSVC temporaries — `<range>`, `<begin>`,
/// `<end>` — and, when iterating a temporary collection, a `$S<N>` scope
/// temporary. All of these may appear in any order (register allocation), followed
/// by a nested block that is the loop body.
fn detect_range_for(
    statements: &[cpp::Statement],
    names: &HashMap<String, String>,
) -> Option<(String, cpp::Block)> {
    let mut range_scope: Option<String> = None;
    let mut begin_scope: Option<String> = None;
    let mut end_scope: Option<String> = None;
    let mut collection_type: Option<String> = None;
    let mut has_scope_temp = false;
    let mut seen = std::collections::HashSet::new();

    // Consume the leading declarations (iterator temporaries and, optionally, a
    // `$S<N>` scope temporary) until the loop body block.
    let mut index = 0;
    while let Some(cpp::Statement::Variable(variable)) = statements.get(index) {
        if let Some((marker, scope, full_name)) = iterator_marker(&variable.signature) {
            if !seen.insert(marker) {
                return None;
            }

            match marker {
                "range" => {
                    range_scope = Some(scope);

                    // The range is a reference to the collection; strip the
                    // reference and name to recover the collection's type.
                    let suffix = format!("&{full_name}");
                    collection_type = Some(
                        variable
                            .signature
                            .trim_end()
                            .strip_suffix(&suffix)
                            .unwrap_or(variable.signature.trim_end())
                            .trim_end()
                            .to_string(),
                    );
                }
                "begin" => begin_scope = Some(scope),
                "end" => end_scope = Some(scope),
                _ => unreachable!(),
            }
        } else if is_scope_temp(&variable_identifier(&variable.signature)) {
            if has_scope_temp {
                return None;
            }
            has_scope_temp = true;
        } else {
            // An unrelated declaration — not a loop scope.
            return None;
        }

        index += 1;
    }

    let cpp::Statement::Block(body) = statements.get(index)? else {
        return None;
    };

    // All three temporaries must be present and share a scope.
    let range_scope = range_scope?;
    let begin_scope = begin_scope?;
    let end_scope = end_scope?;
    if range_scope != begin_scope || range_scope != end_scope {
        return None;
    }

    let collection_type = collection_type?;
    let collection = names
        .get(&collection_type)
        .cloned()
        .unwrap_or_else(|| "collection".to_string());

    if has_scope_temp {
        // A C-style index loop: the body's first declaration (`collection[i]`)
        // stays in the body; the loop variable is the index, not that element.
        let accessor = size_accessor(&collection_type);
        Some((
            format!("for (int i = 0; i < {collection}{accessor}; i++)"),
            body.clone(),
        ))
    } else {
        // A range-for: the loop variable is the first declaration in the body scope.
        let element = match body.statements.first() {
            Some(cpp::Statement::Variable(variable)) => variable_identifier(&variable.signature),
            _ => return None,
        };

        // Drop the loop variable from the body (it is now the `for` header).
        let mut new_body = body.clone();
        new_body.statements.remove(0);

        Some((format!("for (auto {element} : {collection})"), new_body))
    }
}

/// Recursively collapses range-based `for` loop scopes into a single
/// `// for (auto element : collection)` comment followed by the loop body.
///
/// MSVC wraps each loop in a scope block whose contents are the three
/// temporaries (`<range>`, `<begin>`, `<end>`) followed by the loop body block.
/// That scope block is flattened away, leaving the comment and body as siblings.
fn transform_iterator_loops(statements: &mut Vec<cpp::Statement>, names: &HashMap<String, String>) {
    // Recurse into nested blocks first so inner loops collapse before outer ones.
    for statement in statements.iter_mut() {
        if let cpp::Statement::Block(child) = statement {
            transform_iterator_loops(&mut child.statements, names);
        }
    }

    let mut i = 0;
    while i < statements.len() {
        if let cpp::Statement::Block(child) = &statements[i]
            && let Some((comment, body)) = detect_range_for(&child.statements, names)
        {
            statements.splice(
                i..=i,
                [
                    cpp::Statement::Comment(comment),
                    cpp::Statement::Block(body),
                ],
            );
            i += 2;
        } else {
            i += 1;
        }
    }
}

#[inline(always)]
pub fn reorganize_module_members(
    type_finder: &pdb2::TypeFinder,
    module: &mut cpp::Module,
    compound_enums: &[(cpp::Enum, cpp::TypeDefinition)],
    project_name: &str,
) -> pdb2::Result<()> {
    //
    // Rebuild module under specific sections
    //

    for member in module.members.iter_mut() {
        let cpp::ModuleMember::Class(class) = member else {
            continue;
        };

        let mut class = class.borrow_mut();

        fn check_subclass(class: Rc<RefCell<cpp::Class>>) {
            let mut class = class.borrow_mut();

            for member in class.members.iter_mut() {
                if let cpp::ClassMember::Class(subclass) = member {
                    check_subclass(subclass.clone());
                    continue;
                }
                
                let cpp::ClassMember::Method(method) = member else {
                    continue;
                };

                let is_inline = method.is_inline;
                method.is_inline = false;
                
                let is_static = method.signature.starts_with("static ");
                method.signature = method.signature.trim_start_matches("static ").into();

                method.signature = format!(
                    "{}{}",
                    if is_static && is_inline {
                        "static inline "
                    } else if is_static {
                        "static "
                    } else if is_inline {
                        "inline "
                    } else {
                        ""
                    },
                    method.signature,
                );
            }
        }
        
        for member in class.members.iter_mut() {
            if let cpp::ClassMember::Class(subclass) = member {
                check_subclass(subclass.clone());
                continue;
            }

            let cpp::ClassMember::Method(method) = member else {
                continue;
            };

            let is_inline = method.is_inline;
            method.is_inline = false;
            
            let is_static = method.signature.starts_with("static ");
            method.signature = method.signature.trim_start_matches("static ").into();

            method.signature = format!(
                "{}{}",
                if is_static && is_inline {
                    "static inline "
                } else if is_static {
                    "static "
                } else if is_inline {
                    "inline "
                } else {
                    ""
                },
                method.signature,
            );
        }
    }

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

    new_members.push(cpp::ModuleMember::Comment("---------- headers".into()));
    new_members.push(cpp::ModuleMember::EmptyLine);

    if !module.is_header() {
        new_members.push(cpp::ModuleMember::Include(false, format!("{project_name}/{project_name}.h").into()));
    }

    if !module.headers.is_empty() {
        for (path, global) in module.headers.iter() {
            new_members.push(cpp::ModuleMember::Include(*global, path.clone()));
        }

        new_members.push(cpp::ModuleMember::EmptyLine);

        module.headers.clear();
    }

    //--------------------------------------------------------------------------------
    // using namespaces
    //--------------------------------------------------------------------------------

    let using_namespace_members = module.members.iter().filter(|m| match m {
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

    let mut constant_members = module.members.iter().filter(|m| match m {
        cpp::ModuleMember::Enum(_) => true,
        // cpp::ModuleMember::Constant(_) => true,
        cpp::ModuleMember::Data { signature, .. } => signature.starts_with("const ") && !signature.contains("$"),
        cpp::ModuleMember::ThreadStorage { signature, .. } => signature.starts_with("const ") && !signature.contains("$"),
        _ => false,
    }).cloned().collect::<Vec<_>>();

    constant_members.insert(0, cpp::ModuleMember::EmptyLine);
    constant_members.insert(0, cpp::ModuleMember::Comment("---------- constants".into()));

    if !constant_members.is_empty() {
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

    let definition_members = module.members.iter().filter(|m| match m {
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

    new_definition_members.insert(0, cpp::ModuleMember::EmptyLine);
    new_definition_members.insert(0, cpp::ModuleMember::Comment("---------- definitions".into()));

    if !new_definition_members.is_empty() {
        while let Some(cpp::ModuleMember::EmptyLine) = new_definition_members.last() {
            new_definition_members.pop();
        }

        new_members.extend(new_definition_members);
        new_members.push(cpp::ModuleMember::EmptyLine);
    }

    //--------------------------------------------------------------------------------
    // function prototypes
    //--------------------------------------------------------------------------------

    let public_code_members = module.members.iter().filter(|m| match m {
        cpp::ModuleMember::Procedure(procedure) => {
            !procedure.is_static
                && !procedure.signature.contains("`")
                && !procedure.signature.contains("$")
        }
        _ => false,
    }).cloned().collect::<Vec<_>>();

    let private_code_members = module.members.iter().filter(|m| match m {
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
            if let Some(member_method_data) = procedure.member_method_data.as_mut() {
                if member_method_data.this_adjustment == 0 {
                    member_method_data.declaring_class.clear();
                }
            }

            let is_member_function = matches!(
                type_finder.find(procedure.type_index)?.parse()?,
                pdb2::TypeData::MemberFunction(_)
            );

            procedure.body = None;
            procedure.address = 0;

            if !is_member_function {
                procedure.is_extern = true;
                prototype_members.push(cpp::ModuleMember::Procedure(procedure));
            }
        }
    }

    if !public_code_members.is_empty() && !private_code_members.is_empty() {
        prototype_members.push(cpp::ModuleMember::EmptyLine);
    }

    for member in private_code_members.iter() {
        if let cpp::ModuleMember::Procedure(procedure) = member {
            let mut procedure = procedure.clone();
            if let Some(member_method_data) = procedure.member_method_data.as_mut() {
                if member_method_data.this_adjustment == 0 {
                    member_method_data.declaring_class.clear();
                }
            }

            procedure.body = None;
            procedure.address = 0;

            prototype_members.push(cpp::ModuleMember::Procedure(procedure));
        }
    }

    new_members.push(cpp::ModuleMember::Comment("---------- prototypes".into()));
    new_members.push(cpp::ModuleMember::EmptyLine);

    if !prototype_members.is_empty() {
        new_members.extend(prototype_members);
        new_members.push(cpp::ModuleMember::EmptyLine);
    }

    //--------------------------------------------------------------------------------
    // variables — globals and file-local statics mixed, sorted by address
    //--------------------------------------------------------------------------------

    let mut variable_members = module.members.iter().filter(|m| match m {
        cpp::ModuleMember::Data { signature, .. }
        | cpp::ModuleMember::ThreadStorage { signature, .. } => {
            !signature.starts_with("const ")
                && !signature.contains("`")
                && !signature.contains("$")
        }
        _ => false,
    }).cloned().collect::<Vec<_>>();

    // Order the whole chunk by address so externs and statics interleave in memory
    // order with no separation between them.
    variable_members.sort_by_key(|m| match m {
        cpp::ModuleMember::Data { address, .. }
        | cpp::ModuleMember::ThreadStorage { address, .. } => *address,
        _ => 0,
    });

    let mut new_variable_members = vec![];

    for mut member in variable_members {
        match &mut member {
            // Globals get `extern`; file-local statics keep `static`.
            cpp::ModuleMember::Data { is_static, is_extern, .. } => {
                *is_extern = !*is_static;
            }

            cpp::ModuleMember::ThreadStorage { name, signature, address, .. } => {
                let name = name.clone();
                let address = *address;

                new_variable_members.push(cpp::ModuleMember::FunctionCall(
                    "static_warning".into(),
                    vec![
                        format!("\"TODO: fix {name} declaration\""),
                    ],
                ));

                let Some((declaration, _comment)) = signature.rsplit_once(';') else {
                    panic!("Malformed thread storage signature: \"{}\"", signature);
                };

                *signature = format!("{declaration} = tls_get<decltype({name})>(0x{address:X});");
            }

            _ => {}
        }

        new_variable_members.push(member);
    }

    if !new_variable_members.is_empty() {
        new_members.push(cpp::ModuleMember::Comment("---------- variables".into()));
        new_members.push(cpp::ModuleMember::EmptyLine);
        new_members.append(&mut new_variable_members);
        new_members.push(cpp::ModuleMember::EmptyLine);
    }

    //--------------------------------------------------------------------------------
    // public functions
    //--------------------------------------------------------------------------------

    fn comment_block(block: &mut cpp::Block) {
        for statement in block.statements.iter_mut() {
            match statement {
                cpp::Statement::Comment(_) | cpp::Statement::Commented(_) | cpp::Statement::EmptyLine => {
                    continue;
                }

                cpp::Statement::Block(block) => comment_block(block),

                _ => *statement = cpp::Statement::Commented(Box::new(statement.clone())),
            }
        }
    }

    // Map used to resolve a range-for's collection back to a named variable (a
    // global/static first, a function-local second, `collection` as a fallback).
    let global_names = collect_global_names(module);

    let mut new_public_code_members: Vec<cpp::ModuleMember> = vec![];

    for member in public_code_members.iter() {
        let cpp::ModuleMember::Procedure(procedure) = member else {
            unreachable!("{member:#?}")
        };

        let mut procedure = procedure.clone();
        if let Some(member_method_data) = procedure.member_method_data.as_mut() {
            if member_method_data.this_adjustment == 0 {
                member_method_data.declaring_class.clear();
            }
        }

        if procedure.body.is_none() {
            procedure.body = Some(cpp::Block::default());
        }

        if let Some(cpp::MemberMethodData { class_type, this_adjustment, .. }) = procedure.member_method_data.as_ref()
            && *this_adjustment != 0
        {
            if procedure.body.is_none() {
                procedure.body = Some(cpp::Block::default());
            }

            procedure.body.as_mut().unwrap().statements.insert(
                0,
                cpp::Statement::FunctionCall("__shifted".into(), vec![
                    class_type.clone(),
                    this_adjustment.to_string(),
                ]),
            );
        }
        
        let mut names = global_names.clone();
        if let Some(body) = procedure.body.as_ref() {
            collect_local_names(body, &mut names);
        }
        if let Some(body) = procedure.body.as_mut() {
            transform_iterator_loops(&mut body.statements, &names);
        }

        comment_block(procedure.body.as_mut().unwrap());

        if let Some((mangled_name, _address)) = module.mangled_symbols.iter()
            .find(|(_, address)| *address == procedure.address)
        {
            procedure.body.as_mut().unwrap().statements.insert(
                0,
                cpp::Statement::FunctionCall(
                    "mangled_x64".into(),
                    vec![
                        "MANGLED_DEFAULT".into(),
                        format!("\"{}\"", mangled_name),
                    ],
                ),
            );
        }

        if procedure.line.is_none() {
            procedure.body.as_mut().unwrap().statements.push(
                cpp::Statement::FunctionCall("compiler_generated".into(), vec![]),
            );
        }

        // The body is a placeholder stub: `TODO_IMPLEMENT()` expands to `__assume(0)`,
        // so it is the final statement (the real code is copied in by a separate
        // process). No `_sub_` trampoline call, return, or extern prototype is emitted.
        procedure.body.as_mut().unwrap().statements.push(
            cpp::Statement::FunctionCall("TODO_IMPLEMENT".into(), vec![]),
        );

        // The definition keeps its own `static`/`inline` specifiers.
        new_public_code_members.push(cpp::ModuleMember::Procedure(procedure));

        new_public_code_members.push(cpp::ModuleMember::EmptyLine);
    }

    new_public_code_members.insert(0, cpp::ModuleMember::EmptyLine);
    new_public_code_members.insert(0, cpp::ModuleMember::Comment("---------- public code".into()));

    if !new_public_code_members.is_empty() {
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
        if let Some(member_method_data) = procedure.member_method_data.as_mut() {
            if member_method_data.this_adjustment == 0 {
                member_method_data.declaring_class.clear();
            }
        }

        if procedure.body.is_none() {
            procedure.body = Some(cpp::Block::default());
        }

        if let Some(cpp::MemberMethodData { class_type, this_adjustment, .. }) = procedure.member_method_data.as_ref()
            && *this_adjustment != 0
        {
            if procedure.body.is_none() {
                procedure.body = Some(cpp::Block::default());
            }

            procedure.body.as_mut().unwrap().statements.insert(
                0,
                cpp::Statement::FunctionCall("__shifted".into(), vec![
                    class_type.clone(),
                    this_adjustment.to_string(),
                ]),
            );
        }
        
        let mut names = global_names.clone();
        if let Some(body) = procedure.body.as_ref() {
            collect_local_names(body, &mut names);
        }
        if let Some(body) = procedure.body.as_mut() {
            transform_iterator_loops(&mut body.statements, &names);
        }

        comment_block(procedure.body.as_mut().unwrap());

        if procedure.body.is_none() {
            procedure.body = Some(cpp::Block::default());
        }

        if let Some((mangled_name, _address)) = module.mangled_symbols.iter()
            .find(|(_, address)| *address == procedure.address)
        {
            procedure.body.as_mut().unwrap().statements.insert(
                0,
                cpp::Statement::FunctionCall(
                    "mangled_x64".into(),
                    vec![
                        "MANGLED_DEFAULT".into(),
                        format!("\"{}\"", mangled_name),
                    ],
                ),
            );
        }

        if procedure.line.is_none() {
            procedure.body.as_mut().unwrap().statements.push(
                cpp::Statement::FunctionCall("compiler_generated".into(), vec![]),
            );
        }

        // The body is a placeholder stub: `TODO_IMPLEMENT()` expands to `__assume(0)`,
        // so it is the final statement (the real code is copied in by a separate
        // process). No `_sub_` trampoline call, return, or extern prototype is emitted.
        procedure.body.as_mut().unwrap().statements.push(
            cpp::Statement::FunctionCall("TODO_IMPLEMENT".into(), vec![]),
        );

        // The definition keeps its own `static`/`inline` specifiers.
        new_private_code_members.push(cpp::ModuleMember::Procedure(procedure));

        new_private_code_members.push(cpp::ModuleMember::EmptyLine);
    }

    new_private_code_members.insert(0, cpp::ModuleMember::EmptyLine);
    new_private_code_members.insert(0, cpp::ModuleMember::Comment("---------- private code".into()));

    if !new_private_code_members.is_empty() {
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

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn var(signature: &str) -> cpp::Statement {
        cpp::Statement::Variable(cpp::Variable {
            signature: signature.to_string(),
            value: None,
            comment: None,
        })
    }

    fn block(statements: Vec<cpp::Statement>) -> cpp::Statement {
        cpp::Statement::Block(cpp::Block {
            address: None,
            statements,
        })
    }

    #[test]
    fn collapses_range_for_with_global_collection() {
        // Mirrors `for (auto elem : VotingSystems)` over
        // `extern std::vector<AbstractVotingSystem *> VotingSystems`.
        let mut statements = vec![
            var("s_model_definition *model_definition"),
            var("long variant_count"),
            block(vec![
                var("std::vector<Server::Voting::AbstractVotingSystem *> &<range>$L0"),
                var("Server::Voting::AbstractVotingSystem **<begin>$L0"),
                var("Server::Voting::AbstractVotingSystem **<end>$L0"),
                block(vec![var("Server::Voting::AbstractVotingSystem *elem")]),
            ]),
        ];

        let mut names = HashMap::new();
        names.insert(
            "std::vector<Server::Voting::AbstractVotingSystem *>".to_string(),
            "VotingSystems".to_string(),
        );

        transform_iterator_loops(&mut statements, &names);

        assert_eq!(statements.len(), 4);
        assert!(matches!(statements[0], cpp::Statement::Variable(_)));
        assert!(matches!(statements[1], cpp::Statement::Variable(_)));
        assert_eq!(
            statements[2],
            cpp::Statement::Comment("for (auto elem : VotingSystems)".to_string())
        );
        assert_eq!(statements[3], block(vec![]));
    }

    #[test]
    fn falls_back_to_collection_when_unnamed() {
        let mut statements = vec![block(vec![
            var("c_tag_block<s_model_variant> &<range>$L0"),
            var("s_model_variant *<begin>$L0"),
            var("s_model_variant *<end>$L0"),
            block(vec![var("s_model_variant &variant")]),
        ])];

        transform_iterator_loops(&mut statements, &HashMap::new());

        assert_eq!(statements.len(), 2);
        assert_eq!(
            statements[0],
            cpp::Statement::Comment("for (auto variant : collection)".to_string())
        );
        assert_eq!(statements[1], block(vec![]));
    }

    #[test]
    fn resolves_local_collection_name() {
        let body = cpp::Block {
            address: None,
            statements: vec![
                // A local collection followed by the loop itself.
                var("std::vector<int> values"),
                block(vec![
                    var("std::vector<int> &<range>$L0"),
                    var("int *<begin>$L0"),
                    var("int *<end>$L0"),
                    block(vec![var("int value")]),
                ]),
            ],
        };

        let mut names = HashMap::new();
        collect_local_names(&body, &mut names);

        let mut statements = body.statements;
        transform_iterator_loops(&mut statements, &names);

        assert_eq!(statements.len(), 3);
        assert!(matches!(statements[0], cpp::Statement::Variable(_)));
        assert_eq!(
            statements[1],
            cpp::Statement::Comment("for (auto value : values)".to_string())
        );
        assert_eq!(statements[2], block(vec![]));
    }

    #[test]
    fn handles_reordered_range_begin_end() {
        // The three temporaries may appear in any order due to register allocation.
        let mut statements = vec![block(vec![
            var("int *<end>$L0"),
            var("std::vector<int> &<range>$L0"),
            var("int *<begin>$L0"),
            block(vec![var("int item")]),
        ])];

        let mut names = HashMap::new();
        names.insert("std::vector<int>".to_string(), "items".to_string());

        transform_iterator_loops(&mut statements, &names);

        assert_eq!(statements.len(), 2);
        assert_eq!(
            statements[0],
            cpp::Statement::Comment("for (auto item : items)".to_string())
        );
    }

    #[test]
    fn leaves_incomplete_pattern_untouched() {
        // Only two of the three temporaries — not a range-for.
        let original = vec![block(vec![
            var("std::vector<int> &<range>$L0"),
            var("int *<begin>$L0"),
            block(vec![var("int item")]),
        ])];

        let mut statements = original.clone();
        transform_iterator_loops(&mut statements, &HashMap::new());

        assert_eq!(statements, original);
    }

    #[test]
    fn collapses_range_for_over_function_pointers() {
        // Mirrors `for (auto&& handler : defaultHandlers)` over a
        // `std::vector<void (__cdecl*)(void)>`. The begin/end temporaries are
        // function-pointer declarators and the loop variable is a reference.
        let mut statements = vec![block(vec![
            var("std::vector<void (__cdecl*)(void)> &<range>$L0"),
            var("void (**<begin>$L0)()"),
            var("void (**<end>$L0)()"),
            block(vec![var("void (*&handler)()")]),
        ])];

        let mut names = HashMap::new();
        names.insert(
            "std::vector<void (__cdecl*)(void)>".to_string(),
            "defaultHandlers".to_string(),
        );

        transform_iterator_loops(&mut statements, &names);

        assert_eq!(statements.len(), 2);
        assert_eq!(
            statements[0],
            cpp::Statement::Comment("for (auto handler : defaultHandlers)".to_string())
        );
        assert_eq!(statements[1], block(vec![]));
    }

    #[test]
    fn range_temporaries_do_not_shadow_collection_fallback() {
        // The collection is a temporary/expression with no discoverable name. The
        // `<range>$L0` temporary shares its type, so it must not supply the
        // collection name — the loop should fall back to `collection`.
        let body = cpp::Block {
            address: None,
            statements: vec![block(vec![
                var("std::vector<int> &<range>$L0"),
                var("int *<begin>$L0"),
                var("int *<end>$L0"),
                block(vec![var("int arg")]),
            ])],
        };

        let mut names = HashMap::new();
        collect_local_names(&body, &mut names);

        let mut statements = body.statements;
        transform_iterator_loops(&mut statements, &names);

        assert_eq!(
            statements[0],
            cpp::Statement::Comment("for (auto arg : collection)".to_string())
        );
    }

    #[test]
    fn infers_collection_size_accessor() {
        assert_eq!(size_accessor("std::vector<int>"), ".size()");
        assert_eq!(
            size_accessor("rapidjson::GenericArray<0,rapidjson::Value>"),
            ".Size()"
        );
        assert_eq!(size_accessor("c_tag_block<s_model_variant>"), ".count");
    }

    #[test]
    fn collapses_c_style_for_loop() {
        // A temporary collection (`$S9`) collapses to a C-style index loop whose
        // body keeps its `collection[i]` element declaration.
        let mut statements = vec![block(vec![
            var("rapidjson::GenericArray<0,rapidjson::Value> &<range>$L0"),
            var("rapidjson::Value *<begin>$L0"),
            var("rapidjson::Value *<end>$L0"),
            var("rapidjson::GenericArray<0,rapidjson::Value> $S9"),
            block(vec![
                var("rapidjson::Value const &mapObject"),
                block(vec![var("Server::Voting::HaloMap haloMap")]),
            ]),
        ])];

        transform_iterator_loops(&mut statements, &HashMap::new());

        assert_eq!(statements.len(), 2);
        assert_eq!(
            statements[0],
            cpp::Statement::Comment("for (int i = 0; i < collection.Size(); i++)".to_string())
        );
        assert_eq!(
            statements[1],
            block(vec![
                var("rapidjson::Value const &mapObject"),
                block(vec![var("Server::Voting::HaloMap haloMap")]),
            ])
        );
    }

    #[test]
    fn handles_reordered_scope_temporary() {
        // The `$S<N>` temporary and the three iterators can appear in any order.
        // (Mirrors LoadVotingJson: `$S10`, `end`, `begin`, `range`.)
        let mut statements = vec![block(vec![
            var("rapidjson::GenericArray<0,rapidjson::Value> $S10"),
            var("rapidjson::Value *<end>$L1"),
            var("rapidjson::Value *<begin>$L1"),
            var("rapidjson::GenericArray<0,rapidjson::Value> &<range>$L1"),
            block(vec![
                var("rapidjson::Value const &typeObject"),
                block(vec![var("Server::Voting::HaloType ht")]),
            ]),
        ])];

        transform_iterator_loops(&mut statements, &HashMap::new());

        assert_eq!(statements.len(), 2);
        assert_eq!(
            statements[0],
            cpp::Statement::Comment("for (int i = 0; i < collection.Size(); i++)".to_string())
        );
        assert_eq!(
            statements[1],
            block(vec![
                var("rapidjson::Value const &typeObject"),
                block(vec![var("Server::Voting::HaloType ht")]),
            ])
        );
    }

    #[test]
    fn collapses_nested_range_fors() {
        // The inner loop lives inside the outer loop's body, after the outer
        // loop variable.
        let mut statements = vec![block(vec![
            var("std::vector<int> &<range>$L0"),
            var("int *<begin>$L0"),
            var("int *<end>$L0"),
            block(vec![
                var("int &outer"),
                block(vec![
                    var("std::vector<float> &<range>$L1"),
                    var("float *<begin>$L1"),
                    var("float *<end>$L1"),
                    block(vec![var("float &inner")]),
                ]),
            ]),
        ])];

        let mut names = HashMap::new();
        names.insert("std::vector<int>".to_string(), "outers".to_string());
        names.insert("std::vector<float>".to_string(), "inners".to_string());

        transform_iterator_loops(&mut statements, &names);

        assert_eq!(statements.len(), 2);
        assert_eq!(
            statements[0],
            cpp::Statement::Comment("for (auto outer : outers)".to_string())
        );
        assert_eq!(
            statements[1],
            block(vec![
                cpp::Statement::Comment("for (auto inner : inners)".to_string()),
                block(vec![]),
            ])
        );
    }
}
