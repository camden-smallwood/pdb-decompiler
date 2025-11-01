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

            if type_definition.type_name.starts_with(format!("c_enum<enum {},", enum_data.name).as_str()) {
                let mut result = type_definition.clone();
                result.type_name = format!("c_enum<{}", result.type_name.trim_start_matches("c_enum<enum "));

                let Some((type_string, name_string)) = result.type_name.rsplit_once(' ') else {
                    panic!("Unexpected typedef type name string: \"{}\"", result.type_name);
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

                result.type_name = format!("c_enum<{}>{} {}", template_args.join(","), suffix, name_string);

                return Some(result);
            }

            if type_definition.type_name.starts_with(format!("c_flags<enum {},", enum_data.name).as_str()) {
                let mut result = type_definition.clone();
                result.type_name = format!("c_flags<{}", result.type_name.trim_start_matches("c_flags<enum "));

                let Some((type_string, name_string)) = result.type_name.rsplit_once(' ') else {
                    panic!("Unexpected typedef type name string: \"{}\"", result.type_name);
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

                result.type_name = format!("c_flags<{}>{} {}", template_args.join(","), suffix, name_string);

                return Some(result);
            }

            if type_definition.type_name.starts_with(format!("c_flags_no_init<enum {},", enum_data.name).as_str()) {
                let mut result = type_definition.clone();
                result.type_name = format!("c_flags_no_init<{}", result.type_name.trim_start_matches("c_flags_no_init<enum "));

                let Some((type_string, name_string)) = result.type_name.rsplit_once(' ') else {
                    panic!("Unexpected typedef type name string: \"{}\"", result.type_name);
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

                result.type_name = format!("c_flags_no_init<{}>{} {}", template_args.join(","), suffix, name_string);

                return Some(result);
            }
        }
    }

    None
}

#[inline(always)]
pub fn reorganize_module_members(
    class_table: &mut Vec<Rc<RefCell<cpp::Class>>>,
    type_sizes: &mut HashMap<String, u64>,
    machine_type: pdb2::MachineType,
    type_info: &pdb2::TypeInformation,
    type_finder: &pdb2::TypeFinder,
    module: &mut cpp::Module,
    compound_enums: &[(cpp::Enum, cpp::TypeDefinition)],
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
                        "static _inline "
                    } else if is_static {
                        "static "
                    } else if is_inline {
                        "_inline "
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
                    "static _inline "
                } else if is_static {
                    "static "
                } else if is_inline {
                    "_inline "
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
        new_members.push(cpp::ModuleMember::Include(false, "ares/ares.h".into()));
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

            let is_inline = procedure.is_inline;
            procedure.is_inline = false;
            
            if !is_member_function {
                prototype_members.push(cpp::ModuleMember::Tagged(
                    if is_inline { "extern _inline" } else { "extern" }.into(),
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
            if let Some(member_method_data) = procedure.member_method_data.as_mut() {
                if member_method_data.this_adjustment == 0 {
                    member_method_data.declaring_class.clear();
                }
            }

            procedure.body = None;
            procedure.address = 0;
            procedure.is_static = false;

            let is_inline = procedure.is_inline;
            procedure.is_inline = false;

            prototype_members.push(cpp::ModuleMember::Tagged(
                if is_inline { "_static _inline" } else { "_static" }.into(),
                Box::new(cpp::ModuleMember::Procedure(procedure)),
            ));
        }
    }

    new_members.push(cpp::ModuleMember::Comment("---------- prototypes".into()));
    new_members.push(cpp::ModuleMember::EmptyLine);

    if !prototype_members.is_empty() {
        new_members.extend(prototype_members);
        new_members.push(cpp::ModuleMember::EmptyLine);
    }

    //--------------------------------------------------------------------------------
    // public mutable vars
    //--------------------------------------------------------------------------------

    let global_members = module.members.iter().filter(|m| match m {
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

    while let Some(cpp::ModuleMember::EmptyLine) = new_global_members.last() {
        new_global_members.pop();
    }

    new_members.push(cpp::ModuleMember::Comment("---------- globals".into()));
    new_members.push(cpp::ModuleMember::EmptyLine);

    if !new_global_members.is_empty() {
        for member in new_global_members {
            if let cpp::ModuleMember::Data { name, signature, address, line, .. } = member
            {
                new_members.push(cpp::ModuleMember::Tagged(
                    "extern".into(),
                    Box::new(cpp::ModuleMember::Data {
                        is_static: false,
                        name: name,
                        signature: signature,
                        address: address,
                        line: line
                    }),
                ));
            }
            else
            {
                new_members.push(member)
            }
        }

        while let Some(cpp::ModuleMember::EmptyLine) = new_members.last() {
            new_members.pop();
        }

        new_members.push(cpp::ModuleMember::EmptyLine);
    }
    
    //--------------------------------------------------------------------------------
    // private mutable vars
    //--------------------------------------------------------------------------------

    let private_variable_members = module.members.iter().filter(|m| match m {
        cpp::ModuleMember::Data { is_static: true, signature, .. } => {
            !signature.starts_with("const ")
                && !signature.contains("`")
                && !signature.contains("$")
        }
        _ => false,
    }).cloned().collect::<Vec<_>>();

    let mut new_private_variable_members = vec![];

    for member in private_variable_members {
        let cpp::ModuleMember::Data { name, signature, address, line, .. } = member else {
            unreachable!("{:#?}", member)
        };

        new_private_variable_members.push(cpp::ModuleMember::Tagged(
            "extern".into(),
            Box::new(cpp::ModuleMember::Data {
                is_static: false,
                name: name,
                signature: signature,
                address: address,
                line: line
            }),
        ));
    }

    while let Some(cpp::ModuleMember::EmptyLine) = new_private_variable_members.last() {
        new_private_variable_members.pop();
    }

    if !new_private_variable_members.is_empty() || !module.is_header() {
        new_members.push(cpp::ModuleMember::Comment("---------- private variables".into()));
        new_members.push(cpp::ModuleMember::EmptyLine);
    }

    if !new_private_variable_members.is_empty() {
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
                cpp::Statement::Comment(_) | cpp::Statement::Commented(_) | cpp::Statement::EmptyLine => {
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

            if procedure.line.is_none() {
                procedure.body.as_mut().unwrap().statements.push(
                    cpp::Statement::FunctionCall("compiler_generated".into(), vec![]),
                );
            }

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
                procedure.member_method_data.as_ref().map(|m| m.declaring_class.clone()).or(Some(String::new())),
                false
            )
            .unwrap()
        }).unwrap_or("void".to_string());

        if matches!(return_type_str.as_str(), "void" | "void const") {
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

        let is_inline = procedure.is_inline;
        procedure.is_inline = false;

        new_public_code_members.push(cpp::ModuleMember::Tagged(
            "_extern".into(),
            Box::new(cpp::ModuleMember::Procedure(cpp::Procedure {
                address: 0,
                line: procedure.line,
                type_index: procedure.type_index,
                is_static: false,
                is_inline: false,
                member_method_data: procedure.member_method_data.clone(),
                declspecs: procedure.declspecs.clone(),
                name: format!("_sub_{:X}", procedure.address).into(),
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
                    procedure.member_method_data.as_ref().map(|m| m.declaring_class.clone()).or(Some(String::new())),
                    false
                )?
                .trim_end_matches(" const")
                .trim_end_matches(" volatile")
                .into(),
                ida_signature: None,
                body: None,
                return_type: procedure.return_type,
                arguments: vec![],
            })),
        ));

        if !procedure.is_static {
            if is_inline {
                new_public_code_members.push(cpp::ModuleMember::Tagged("_inline".into(), Box::new(cpp::ModuleMember::Procedure(procedure))));
            } else {
                new_public_code_members.push(cpp::ModuleMember::Procedure(procedure));
            }
        } else {
            procedure.is_static = false;

            new_public_code_members.push(cpp::ModuleMember::Tagged(
                if is_inline { "_static _inline" } else { "_static" }.into(),
                Box::new(cpp::ModuleMember::Procedure(procedure))));
        }

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
        
        comment_block(procedure.body.as_mut().unwrap());

        if procedure.body.is_none() {
            procedure.body = Some(cpp::Block::default());
        }

        procedure.body.as_mut().unwrap().statements.insert(
            0,
            cpp::Statement::FunctionCall(
                "mangled_assert".into(),
                vec![
                    format!("\"{}\"", procedure.name),
                ],
            ),
        );

        if procedure.line.is_none() {
            procedure.body.as_mut().unwrap().statements.push(
                cpp::Statement::FunctionCall("compiler_generated".into(), vec![]),
            );
        }

        procedure.body.as_mut().unwrap().statements.push(
            cpp::Statement::FunctionCall("todo".into(), vec!["\"implement\"".into()]),
        );

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
                procedure.member_method_data.as_ref().map(|m| m.declaring_class.clone()).or(Some(String::new())),
                false
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

        let is_inline = procedure.is_inline;
        procedure.is_inline = false;

        new_private_code_members.push(cpp::ModuleMember::Tagged(
            "_extern".into(),
            Box::new(cpp::ModuleMember::Procedure(cpp::Procedure {
                address: 0,
                line: procedure.line,
                type_index: procedure.type_index,
                is_static: false,
                is_inline: false,
                member_method_data: procedure.member_method_data.clone(),
                declspecs: procedure.declspecs.clone(),
                name: format!("_sub_{:X}", procedure.address).into(),
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
                    procedure.member_method_data.as_ref().map(|m| m.declaring_class.clone()).or(Some(String::new())),
                    false
                )?
                .trim_end_matches(" const")
                .trim_end_matches(" volatile")
                .into(),
                ida_signature: None,
                body: None,
                return_type: procedure.return_type,
                arguments: vec![],
            })),
        ));

        if !procedure.is_static {
            if is_inline {
                new_private_code_members.push(cpp::ModuleMember::Tagged("_inline".into(), Box::new(cpp::ModuleMember::Procedure(procedure))));
            } else {
                new_private_code_members.push(cpp::ModuleMember::Procedure(procedure));
            }
        } else {
            procedure.is_static = false;

            new_private_code_members.push(cpp::ModuleMember::Tagged(
                if is_inline { "_static _inline" } else { "_static" }.into(),
                Box::new(cpp::ModuleMember::Procedure(procedure)),
            ));
        }

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
