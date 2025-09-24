mod class;
mod r#enum;
mod module;
mod procedure;
mod typedef;

use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub use self::{
    class::*, r#enum::*, module::*, procedure::*, typedef::*
};

use pdb2::{FallibleIterator, PrimitiveKind};

pub fn argument_list<'p>(
    class_table: &mut Vec<Rc<RefCell<Class>>>,
    type_sizes: &mut HashMap<String, u64>,
    machine_type: pdb2::MachineType,
    type_info: &pdb2::TypeInformation,
    type_finder: &pdb2::TypeFinder<'p>,
    this_pointer_type: Option<pdb2::TypeIndex>,
    type_index: pdb2::TypeIndex,
    mut names: Option<Vec<String>>,
) -> pdb2::Result<Vec<String>> {
    match type_finder.find(type_index)?.parse()? {
        pdb2::TypeData::ArgumentList(data) => {
            let mut args: Vec<String> = vec![];

            if let Some(type_index) = this_pointer_type {
                let this_name = names.as_mut().map(|names| names.first().cloned()).flatten();
                if !names.as_ref().map(|names| names.is_empty()).unwrap_or(false) {
                    names.as_mut().map(|names| names.remove(0));
                }

                args.push(type_name(class_table, type_sizes, machine_type, type_info, type_finder, type_index, None, this_name, None, false, false)?);
            }

            for (i, &arg_type) in data.arguments.iter().enumerate() {
                let arg_name = match names.as_ref() {
                    Some(names) if i < names.len() => Some(names[i].clone()),
                    _ => None
                };

                args.push(type_name(class_table, type_sizes, machine_type, type_info, type_finder, arg_type, None, arg_name, None, false, false)?);
            }

            Ok(args)
        }

        _ => Err(pdb2::Error::UnimplementedFeature("argument list of non-argument-list type".into()))
    }
}

pub fn argument_type_list<'p>(
    type_finder: &pdb2::TypeFinder<'p>,
    this_pointer_type: Option<pdb2::TypeIndex>,
    type_index: pdb2::TypeIndex,
    mut names: Option<Vec<String>>,
) -> pdb2::Result<Vec<(pdb2::TypeIndex, Option<String>)>> {
    match type_finder.find(type_index)?.parse()? {
        pdb2::TypeData::ArgumentList(data) => {
            let mut args = vec![];

            if let Some(this_pointer_type) = this_pointer_type {
                let this_name = names.as_mut().map(|names| names.first().cloned()).flatten().unwrap_or("this".to_string());
                names.as_mut().map(|names| names.remove(0));

                args.push((this_pointer_type, Some(this_name)));
            }

            for (i, &arg_type) in data.arguments.iter().enumerate() {
                let arg_name = match names.as_ref() {
                    Some(names) if i < names.len() => Some(names[i].clone()),
                    _ => None
                };

                args.push((arg_type, arg_name));
            }

            Ok(args)
        }

        data => panic!("argument list of non-argument-list type : {:#?}", data)
    }
}

pub fn primitive_name(kind: PrimitiveKind) -> &'static str {
    match kind {
        pdb2::PrimitiveKind::NoType => "...",

        pdb2::PrimitiveKind::Void => "void",

        pdb2::PrimitiveKind::Char | pdb2::PrimitiveKind::RChar => "char",
        pdb2::PrimitiveKind::UChar => "unsigned char",
        
        pdb2::PrimitiveKind::Short => "short",
        pdb2::PrimitiveKind::UShort => "unsigned short",
        
        pdb2::PrimitiveKind::Long => "long",
        pdb2::PrimitiveKind::ULong => "unsigned long",

        pdb2::PrimitiveKind::Quad => "long long",
        pdb2::PrimitiveKind::UQuad => "unsigned long long",

        pdb2::PrimitiveKind::F32 => "float",
        pdb2::PrimitiveKind::F64 => "double",
        pdb2::PrimitiveKind::F80 => "long double",

        pdb2::PrimitiveKind::Bool8 => "bool",

        pdb2::PrimitiveKind::I8 => "__int8",
        pdb2::PrimitiveKind::U8 => "unsigned __int8",

        pdb2::PrimitiveKind::I16 => "__int16",
        pdb2::PrimitiveKind::U16 => "unsigned __int16",

        pdb2::PrimitiveKind::I32 => "__int32",
        pdb2::PrimitiveKind::U32 => "unsigned __int32",

        pdb2::PrimitiveKind::I64 => "__int64",
        pdb2::PrimitiveKind::U64 => "unsigned __int64",
        
        pdb2::PrimitiveKind::I128 => "__int128",
        pdb2::PrimitiveKind::U128 => "unsigned __int128",

        pdb2::PrimitiveKind::WChar => "wchar_t",
        pdb2::PrimitiveKind::Char8 => "char8_t",
        pdb2::PrimitiveKind::RChar16 => "char16_t",
        pdb2::PrimitiveKind::RChar32 => "char32_t",

        pdb2::PrimitiveKind::HRESULT => "HRESULT",

        _ => panic!("Unhandled primitive kind: {kind:#?}"),
    }
}

pub fn type_name<'p>(
    class_table: &mut Vec<Rc<RefCell<Class>>>,
    type_sizes: &mut HashMap<String, u64>,
    machine_type: pdb2::MachineType,
    type_info: &pdb2::TypeInformation,
    type_finder: &pdb2::TypeFinder<'p>,
    type_index: pdb2::TypeIndex,
    modifier: Option<&pdb2::ModifierType>,
    declaration_name: Option<String>,
    parameter_names: Option<Vec<String>>,
    include_this: bool,
    force_return_type: bool,
) -> pdb2::Result<String> {
    let type_item = type_finder.find(type_index)?;
    let type_data = type_item.parse()?;

    let mut name = match &type_data {
        pdb2::TypeData::Modifier(data) => {
            type_name(
                class_table,
                type_sizes,
                machine_type,
                type_info,
                type_finder,
                data.underlying_type,
                Some(data),
                declaration_name,
                None,
                false,
                false
            )?
        }

        pdb2::TypeData::Bitfield(data) => {
            type_name(class_table, type_sizes, machine_type, type_info, type_finder, data.underlying_type, modifier, declaration_name, None, false, false)?
        }

        pdb2::TypeData::Primitive(data) => {
            let mut name = primitive_name(data.kind).to_string();

            if let Some(modifier) = modifier {
                if modifier.constant {
                    name.push_str(" const");
                }

                if modifier.volatile {
                    name.push_str(" volatile");
                }
            }

            if data.indirection.is_some() {
                if name.ends_with(' ') {
                    name.push('*');
                }
                name.push_str(" *");
            }

            if name != "..."
                && let Some(field_name) = declaration_name
            {
                name.push(' ');
                name.push_str(field_name.as_str());
            }

            name
        }

        pdb2::TypeData::Class(pdb2::ClassType { name, .. })
        | pdb2::TypeData::Enumeration(pdb2::EnumerationType { name, .. })
        | pdb2::TypeData::Union(pdb2::UnionType { name, .. }) => {
            let mut name = name.to_string().to_string();

            if let Some(modifier) = modifier {
                if modifier.constant {
                    name.push_str(" const");
                }

                if modifier.volatile {
                    name.push_str(" volatile");
                }
            }

            if let Some(field_name) = declaration_name {
                name.push(' ');
                name.push_str(field_name.as_str());
            }

            name
        }

        pdb2::TypeData::Array(data) => {
            let mut name = declaration_name.clone().unwrap_or_default();
            let mut element_size = type_size(class_table, type_sizes, machine_type, type_info, type_finder, data.element_type)?;
            
            if element_size == 0 {
                element_size = type_size_explicit(class_table, type_sizes, machine_type, type_info, type_finder, data.element_type)?;
            }

            assert!(element_size != 0);

            for &size in data.dimensions.iter() {
                name = format!("{}[{}]", name, if element_size == 0 { size } else { size / element_size as u32 });
                element_size = size as usize;
            }
            
            type_name(class_table, type_sizes, machine_type, type_info, type_finder, data.element_type, modifier, Some(name), None, false)?
        }

        pdb2::TypeData::Procedure(data) => {
            let mut name = if let Some(declaration_name) = declaration_name.as_ref() {
                declaration_name.clone()
            } else {
                String::new()
            };

            name.push_str(
                format!(
                    "({})",
                    argument_list(
                        class_table,
                        type_sizes,
                        machine_type,
                        type_info,
                        type_finder,
                        None,
                        data.argument_list,
                        parameter_names,
                    )?.join(", ")
                ).as_str()
            );

            if data.attributes.is_constructor() || name.starts_with('~') || name.contains("::~") {
                name
            } else {
                match data.return_type {
                    Some(type_index) => type_name(
                        class_table,
                        type_sizes,
                        machine_type,
                        type_info,
                        type_finder,
                        type_index,
                        None,
                        Some(name),
                        None,
                        false,
                        false
                    )?,

                    None => name,
                }
            }
        }

        pdb2::TypeData::MemberFunction(data) => {
            let mut name = if let Some(declaration_name) = declaration_name.as_ref() {
                declaration_name.clone()
            } else {
                String::new()
            };

            let mut parameter_names = parameter_names;

            if include_this && let Some(this_pointer_type) = data.this_pointer_type {
                name = format!("{}({})", name, argument_list(class_table, type_sizes, machine_type, type_info, type_finder, Some(this_pointer_type), data.argument_list, parameter_names)?.join(", "));
            }
            else {
                if let Some(parameter_names) = parameter_names.as_mut()
                    && data.this_pointer_type.is_some()
                    && !parameter_names.is_empty()
                    && parameter_names[0] == "this"
                {
                    parameter_names.remove(0);
                }

                name = format!("{}({})", name, argument_list(class_table, type_sizes, machine_type, type_info, type_finder, None, data.argument_list, parameter_names)?.join(", "));
            }

            if let Some(modifier) = get_member_function_modifier(data, type_finder) {
                if modifier.constant {
                    name.push_str(" const");
                }

                if modifier.volatile {
                    name.push_str(" volatile");
                }
            }
            
            if !force_return_type && (data.attributes.is_constructor() || name.starts_with('~') || name.contains("::~")) {
                name
            } else if force_return_type && data.attributes.is_constructor() {
                let type_item = type_finder.find(data.class_type)?;
                let type_data = type_item.parse()?;

                match &type_data {
                    pdb2::TypeData::Class(class_name) => {
                        name = format!("{}* {}", class_name.name, name);

                        name
                    }

                    _ => name
                }
            } else {
                type_name(
                    class_table,
                    type_sizes,
                    machine_type,
                    type_info,
                    type_finder,
                    data.return_type,
                    None,
                    Some(name),
                    None,
                    false,
                    false
                )?
            }
        }

        pdb2::TypeData::Pointer(data) => {
            let apply_pointer_attributes = |name: &mut String| {
                let mut attribute_string = String::new();

                if data.attributes.is_const() || modifier.as_ref().map(|x| x.constant).unwrap_or(false) {
                    attribute_string.push_str("const ");
                }

                if data.attributes.is_volatile() || modifier.as_ref().map(|x| x.volatile).unwrap_or(false) {
                    attribute_string.push_str("volatile ");
                }

                if data.attributes.is_unaligned() {
                    attribute_string.push_str("__unaligned ");
                }

                if data.attributes.is_restrict() {
                    attribute_string.push_str("__restrict ");
                }

                if declaration_name.is_none() {
                    attribute_string = attribute_string.trim_end().into();
                }

                name.push_str(attribute_string.as_str());
            };

            match type_finder.find(data.underlying_type)?.parse() {
                Ok(pdb2::TypeData::Array(array_type)) => {
                    let mut dimensions = vec![];
                    let mut element_type = array_type.element_type;

                    match type_finder.find(array_type.element_type)?.parse()? {
                        pdb2::TypeData::Array(array_type) => {
                            match type_finder.find(array_type.element_type)?.parse()? {
                                pdb2::TypeData::Array(_) => todo!(),
                                
                                pdb2::TypeData::Modifier(modifier_type) => match type_finder.find(modifier_type.underlying_type)?.parse()? {
                                    pdb2::TypeData::Array(_) => todo!(),
                                    pdb2::TypeData::Modifier(_) => todo!(),

                                    _ => {}
                                }

                                _ => {}
                            }

                            element_type = array_type.element_type;

                            let mut element_size = type_size(class_table, type_sizes, machine_type, type_info, type_finder, array_type.element_type)?;
                            
                            if element_size == 0 {
                                element_size = type_size_explicit(class_table, type_sizes, machine_type, type_info, type_finder, array_type.element_type)?;
                            }

                            assert!(element_size != 0);
                            
                            for &size in array_type.dimensions.iter() {
                                dimensions.push(if element_size == 0 { size } else { size / element_size as u32 });
                                element_size = size as usize;
                            }
                        }

                        pdb2::TypeData::Modifier(modifier_data) => match type_finder.find(modifier_data.underlying_type)?.parse()? {
                            pdb2::TypeData::Array(_) => todo!(),
                            pdb2::TypeData::Modifier(_) => todo!(),
                            _ => {}
                        }

                        _ => {}
                    }

                    let mut name = declaration_name.clone().unwrap_or_default();

                    if data.attributes.is_reference() {
                        name.push_str(" (&");
                    } else {
                        name.push_str(" (*");
                    }

                    apply_pointer_attributes(&mut name);

                    if let Some(field_name) = declaration_name {
                        name.push_str(field_name.as_str());
                    }
                    
                    name.push_str(format!("){}", dimensions.iter().map(|d| format!("[{d}]")).collect::<Vec<_>>().join("")).as_str());

                    let mut element_size = type_size(class_table, type_sizes, machine_type, type_info, type_finder, array_type.element_type)?;
                    
                    if element_size == 0 {
                        element_size = type_size_explicit(class_table, type_sizes, machine_type, type_info, type_finder, array_type.element_type)?;
                    }

                    assert!(element_size != 0);
                    
                    for &size in array_type.dimensions.iter() {
                        name = format!("{}[{}]", name, if element_size == 0 { size } else { size / element_size as u32 });
                        element_size = size as usize;
                    }
                    
                    type_name(class_table, type_sizes, machine_type, type_info, type_finder, element_type, modifier, Some(name), None, false)?
                }

                Ok(pdb2::TypeData::Procedure(procedure_data)) => {
                    let mut name = match procedure_data.return_type {
                        Some(index) => type_name(class_table, type_sizes, machine_type, type_info, type_finder, index, modifier, None, None, false, false)?,
                        None => String::new(),
                    };

                    if data.attributes.is_reference() {
                        name.push_str("(&");
                    } else {
                        name.push_str("(*");
                    }

                    apply_pointer_attributes(&mut name);

                    if let Some(field_name) = declaration_name {
                        name.push_str(field_name.as_str());
                    }

                    name.push(')');

                    name.push_str(
                        format!(
                            "({})",
                            argument_list(class_table, type_sizes, machine_type, type_info, type_finder, None, procedure_data.argument_list, parameter_names)?.join(", ")
                        ).as_str()
                    );

                    name
                }

                Ok(pdb2::TypeData::MemberFunction(member_function_data)) => {
                    let mut name = type_name(class_table, type_sizes, machine_type, type_info, type_finder, member_function_data.return_type, modifier, None, None, false, false)?;

                    if data.attributes.is_reference() {
                        name.push_str("(&");
                    } else {
                        name.push_str("(*");
                    }

                    apply_pointer_attributes(&mut name);

                    if let Some(field_name) = declaration_name {
                        name.push_str(field_name.as_str());
                    }

                    name.push(')');

                    name = format!("{}({})", name, argument_list(
                        class_table,
                        type_sizes,
                        machine_type,
                        type_info,
                        type_finder,
                        member_function_data.this_pointer_type,
                        member_function_data.argument_list,
                        None,
                    )?.join(", "));

                    if let Some(modifier) = get_member_function_modifier(&member_function_data, type_finder) {
                        if modifier.constant {
                            name.push_str(" const");
                        }

                        if modifier.volatile {
                            name.push_str(" volatile");
                        }
                    }

                    name
                }
                
                Ok(_) => {
                    let mut name = type_name(class_table, type_sizes, machine_type, type_info, type_finder, data.underlying_type, modifier, None, None, false)?;

                    if data.attributes.is_reference() {
                        name.push_str(" &");
                    } else {
                        name.push_str(" *");
                    }

                    apply_pointer_attributes(&mut name);

                    if let Some(field_name) = declaration_name {
                        name.push_str(field_name.as_str());
                    }

                    name
                }

                Err(e) => {
                    panic!("failed to parse type data: {e}");
                }
            }
        }

        pdb2::TypeData::MethodList(_) => {
            // println!("WARNING: Encountered method list in type_name, using `...`");
            format!("/* TODO: method list */ ...")
        }

        pdb2::TypeData::ArgumentList(_) => {
            // println!("WARNING: Encountered argument list in type_name, using `...`");
            format!("/* TODO: argument list */ ...")
        }

        pdb2::TypeData::FieldList(_) => {
            // println!("WARNING: Encountered field list in type_name, using `...`");
            format!("/* TODO: field list */ ...")
        }

        pdb2::TypeData::VirtualTableShape(_) => {
            // println!("WARNING: Encountered virtual table shape in type_name, using `...`");
            format!("/* TODO: virtual table shape */ ...")
        }

        _ => panic!(
            "Unhandled type data for type_name at index {}: {:#?}",
            type_index, type_data
        )
    };

    // TODO: search and replace std:: patterns
    if name == "std::basic_string<char,std::char_traits<char>,std::allocator<char> >" {
        name = "std::string".to_string();
    }

    Ok(name)
}

pub fn type_size_explicit<'p>(
    class_table: &mut Vec<Rc<RefCell<Class>>>,
    type_sizes: &mut HashMap<String, u64>,
    machine_type: pdb2::MachineType,
    type_info: &pdb2::TypeInformation,
    type_finder: &pdb2::TypeFinder<'p>,
    type_index: pdb2::TypeIndex,
) -> pdb2::Result<usize> {

    let element_type_data = type_finder.find(type_index)?.parse()?;

    let mut type_iter = type_info.iter();

    loop {
        let current_type_item = match type_iter.next() {
            Ok(Some(current_type_item)) => current_type_item,
            Ok(None) | Err(_) => return Ok(0),
        };

        let current_type_data = match current_type_item.parse() {
            Ok(current_type_data) => current_type_data,
            Err(e) => {
                println!("WARNING: failed to parse type data, skipping: {e}");
                continue;
            }
        };

        match &current_type_data {
            pdb2::TypeData::Primitive(_) if matches!(element_type_data, pdb2::TypeData::Primitive(_)) => (),
            pdb2::TypeData::Class(_) if matches!(element_type_data, pdb2::TypeData::Class(_)) => (),
            pdb2::TypeData::Member(_) if matches!(element_type_data, pdb2::TypeData::Member(_)) => (),
            pdb2::TypeData::MemberFunction(_) if matches!(element_type_data, pdb2::TypeData::MemberFunction(_)) => (),
            pdb2::TypeData::OverloadedMethod(_) if matches!(element_type_data, pdb2::TypeData::OverloadedMethod(_)) => (),
            pdb2::TypeData::Method(_) if matches!(element_type_data, pdb2::TypeData::Method(_)) => (),
            pdb2::TypeData::StaticMember(_) if matches!(element_type_data, pdb2::TypeData::StaticMember(_)) => (),
            pdb2::TypeData::Nested(_) if matches!(element_type_data, pdb2::TypeData::Nested(_)) => (),
            pdb2::TypeData::BaseClass(_) if matches!(element_type_data, pdb2::TypeData::BaseClass(_)) => (),
            pdb2::TypeData::VirtualBaseClass(_) if matches!(element_type_data, pdb2::TypeData::VirtualBaseClass(_)) => (),
            pdb2::TypeData::VirtualFunctionTablePointer(_) if matches!(element_type_data, pdb2::TypeData::VirtualFunctionTablePointer(_)) => (),
            pdb2::TypeData::Procedure(_) if matches!(element_type_data, pdb2::TypeData::Procedure(_)) => (),
            pdb2::TypeData::Pointer(_) if matches!(element_type_data, pdb2::TypeData::Pointer(_)) => (),
            pdb2::TypeData::Modifier(_) if matches!(element_type_data, pdb2::TypeData::Modifier(_)) => (),
            pdb2::TypeData::Enumeration(_) if matches!(element_type_data, pdb2::TypeData::Enumeration(_)) => (),
            pdb2::TypeData::Enumerate(_) if matches!(element_type_data, pdb2::TypeData::Enumerate(_)) => (),
            pdb2::TypeData::Array(_) if matches!(element_type_data, pdb2::TypeData::Array(_)) => (),
            pdb2::TypeData::Union(_) if matches!(element_type_data, pdb2::TypeData::Union(_)) => (),
            pdb2::TypeData::Bitfield(_) if matches!(element_type_data, pdb2::TypeData::Bitfield(_)) => (),
            pdb2::TypeData::FieldList(_) if matches!(element_type_data, pdb2::TypeData::FieldList(_)) => (),
            pdb2::TypeData::ArgumentList(_) if matches!(element_type_data, pdb2::TypeData::ArgumentList(_)) => (),
            pdb2::TypeData::MethodList(_) if matches!(element_type_data, pdb2::TypeData::MethodList(_)) => (),
            _ => continue
        }
        
        if current_type_data.name() == element_type_data.name()
            && let Ok(current_type_size) = type_size(class_table, type_sizes, machine_type, type_info, type_finder, current_type_item.index())
            && current_type_size != 0
        {
            return Ok(current_type_size);
        }
    }
}

pub fn type_size<'p>(
    class_table: &mut Vec<Rc<RefCell<Class>>>,
    type_sizes: &mut HashMap<String, u64>,
    machine_type: pdb2::MachineType,
    _type_info: &pdb2::TypeInformation,
    type_finder: &pdb2::TypeFinder<'p>,
    type_index: pdb2::TypeIndex,
) -> pdb2::Result<usize> {
    let type_item = type_finder.find(type_index)?;
    let type_data = type_item.parse()?;

    match &type_data {
        pdb2::TypeData::Primitive(pdb2::PrimitiveType {
            indirection: Some(_),
            ..
        })
        | pdb2::TypeData::Pointer(_)
        | pdb2::TypeData::Procedure(_) => match machine_type {
            pdb2::MachineType::Unknown | pdb2::MachineType::X86 => Ok(4),
            pdb2::MachineType::Amd64 => Ok(8),
            _ => panic!("Unhandled machine type: {machine_type}")
        }

        pdb2::TypeData::Primitive(pdb2::PrimitiveType {
            kind: pdb2::PrimitiveKind::NoType,
            indirection: None,
        }) => Ok(0),

        pdb2::TypeData::Primitive(pdb2::PrimitiveType {
            kind: pdb2::PrimitiveKind::Void,
            indirection: None,
        }) => Ok(0),

        pdb2::TypeData::Primitive(pdb2::PrimitiveType {
            kind: pdb2::PrimitiveKind::Bool8,
            indirection: None,
        }) => Ok(1),

        pdb2::TypeData::Primitive(pdb2::PrimitiveType {
            kind: pdb2::PrimitiveKind::Bool16,
            indirection: None,
        }) => Ok(2),

        pdb2::TypeData::Primitive(pdb2::PrimitiveType {
            kind: pdb2::PrimitiveKind::Bool32,
            indirection: None,
        }) => Ok(4),

        pdb2::TypeData::Primitive(pdb2::PrimitiveType {
            kind: pdb2::PrimitiveKind::Char,
            indirection: None,
        }) => Ok(1),

        pdb2::TypeData::Primitive(pdb2::PrimitiveType {
            kind: pdb2::PrimitiveKind::UChar,
            indirection: None,
        }) => Ok(1),

        pdb2::TypeData::Primitive(pdb2::PrimitiveType {
            kind: pdb2::PrimitiveKind::WChar,
            indirection: None,
        }) => Ok(2),

        pdb2::TypeData::Primitive(pdb2::PrimitiveType {
            kind: pdb2::PrimitiveKind::RChar,
            indirection: None,
        }) => Ok(1),

        pdb2::TypeData::Primitive(pdb2::PrimitiveType {
            kind: pdb2::PrimitiveKind::Char8,
            indirection: None,
        }) => Ok(1),

        pdb2::TypeData::Primitive(pdb2::PrimitiveType {
            kind: pdb2::PrimitiveKind::RChar16,
            indirection: None,
        }) => Ok(2),

        pdb2::TypeData::Primitive(pdb2::PrimitiveType {
            kind: pdb2::PrimitiveKind::RChar32,
            indirection: None,
        }) => Ok(4),

        pdb2::TypeData::Primitive(pdb2::PrimitiveType {
            kind: pdb2::PrimitiveKind::I8,
            indirection: None,
        }) => Ok(1),

        pdb2::TypeData::Primitive(pdb2::PrimitiveType {
            kind: pdb2::PrimitiveKind::U8,
            indirection: None,
        }) => Ok(1),

        pdb2::TypeData::Primitive(pdb2::PrimitiveType {
            kind: pdb2::PrimitiveKind::Short,
            indirection: None,
        }) => Ok(2),

        pdb2::TypeData::Primitive(pdb2::PrimitiveType {
            kind: pdb2::PrimitiveKind::UShort,
            indirection: None,
        }) => Ok(2),

        pdb2::TypeData::Primitive(pdb2::PrimitiveType {
            kind: pdb2::PrimitiveKind::I16,
            indirection: None,
        }) => Ok(2),

        pdb2::TypeData::Primitive(pdb2::PrimitiveType {
            kind: pdb2::PrimitiveKind::U16,
            indirection: None,
        }) => Ok(2),

        pdb2::TypeData::Primitive(pdb2::PrimitiveType {
            kind: pdb2::PrimitiveKind::Long,
            indirection: None,
        }) => Ok(4),

        pdb2::TypeData::Primitive(pdb2::PrimitiveType {
            kind: pdb2::PrimitiveKind::ULong,
            indirection: None,
        }) => Ok(4),

        pdb2::TypeData::Primitive(pdb2::PrimitiveType {
            kind: pdb2::PrimitiveKind::I32,
            indirection: None,
        }) => Ok(4),

        pdb2::TypeData::Primitive(pdb2::PrimitiveType {
            kind: pdb2::PrimitiveKind::U32,
            indirection: None,
        }) => Ok(4),

        pdb2::TypeData::Primitive(pdb2::PrimitiveType {
            kind: pdb2::PrimitiveKind::HRESULT,
            indirection: None,
        }) => Ok(4),

        pdb2::TypeData::Primitive(pdb2::PrimitiveType {
            kind: pdb2::PrimitiveKind::Quad,
            indirection: None,
        }) => Ok(8),

        pdb2::TypeData::Primitive(pdb2::PrimitiveType {
            kind: pdb2::PrimitiveKind::UQuad,
            indirection: None,
        }) => Ok(8),

        pdb2::TypeData::Primitive(pdb2::PrimitiveType {
            kind: pdb2::PrimitiveKind::I64,
            indirection: None,
        }) => Ok(8),

        pdb2::TypeData::Primitive(pdb2::PrimitiveType {
            kind: pdb2::PrimitiveKind::U64,
            indirection: None,
        }) => Ok(8),

        pdb2::TypeData::Primitive(pdb2::PrimitiveType {
            kind: pdb2::PrimitiveKind::F32,
            indirection: None,
        }) => Ok(4),

        pdb2::TypeData::Primitive(pdb2::PrimitiveType {
            kind: pdb2::PrimitiveKind::F64,
            indirection: None,
        }) => Ok(8),

        pdb2::TypeData::Primitive(pdb2::PrimitiveType {
            kind: pdb2::PrimitiveKind::F80,
            indirection: None,
        }) => Ok(16),

        pdb2::TypeData::Primitive(primitive_type) => {
            unimplemented!("primitive type size: {:?}", primitive_type)
        }

        pdb2::TypeData::Class(data) => {
            if data.size == 0 {
                if let Some(size) = type_sizes.get(&data.name.to_string().to_string()) {
                    return Ok(*size as _);
                }
            }

            Ok(data.size as usize)
        }

        pdb2::TypeData::Enumeration(data) => type_size(class_table, type_sizes, machine_type, _type_info, type_finder, data.underlying_type),
        
        pdb2::TypeData::Union(data) => {
            if data.size == 0 {
                if let Some(size) = type_sizes.get(&data.name.to_string().to_string()) {
                    return Ok(*size as _);
                }
            }

            Ok(data.size as usize)
        }

        pdb2::TypeData::Modifier(data) => type_size(class_table, type_sizes, machine_type, _type_info, type_finder, data.underlying_type),
        pdb2::TypeData::Bitfield(data) => type_size(class_table, type_sizes, machine_type, _type_info, type_finder, data.underlying_type),
        pdb2::TypeData::Array(data) => Ok(*data.dimensions.last().unwrap() as usize),

        _ => panic!(
            "Unhandled type data for type_size at index {}: {:#?}",
            type_index, type_data
        )
    }
}

pub fn get_member_function_modifier<'p>(
    member_func_type: &pdb2::MemberFunctionType,
    type_finder: &pdb2::TypeFinder<'p>,
) -> Option<pdb2::ModifierType> {
    if let Some(this_pointer_type) = member_func_type.this_pointer_type {
        match type_finder.find(this_pointer_type).ok()?.parse().ok()? {
            pdb2::TypeData::Pointer(data) => {
                match type_finder.find(data.underlying_type).ok()?.parse().ok()? {
                    pdb2::TypeData::Modifier(data) => Some(data),
                    _ => {
                        // no modifier on this_pointer_type
                        None
                    }
                }
            }
            _ => {
                // this_pointer_type is not a pointer
                None
            }
        }
    } else {
        // no this pointer
        None
    }
}

pub fn parse_type_name(name: &str, keep_template: bool) -> (String, usize) {
    let mut result = String::new();
    let mut depth = 0;
    let mut first = true;
    let mut offset = 0;

    for (i, c) in name.chars().enumerate() {
        offset = i;

        match c {
            '<' => {
                depth += 1;
                first = false;
                if keep_template {
                    result.push(c);
                }
                continue;
            }

            '>' => {
                if depth == 0 {
                    result.push(c);
                    continue;
                }

                depth -= 1;
                
                if depth == 0 && !first {
                    if keep_template {
                        result.push(c);
                    }
                    break;
                }
            }

            _ => {}
        }

        if depth == 0 || keep_template {
            result.push(c);
        }
    }

    if keep_template {
        depth = 0;
    }

    if depth != 0 {
        let chunk = name.chars().rev().take_while(|c| *c == '<').collect::<String>();
        assert!(!chunk.is_empty());
        result.extend(chunk.chars());
        depth = 0;
    }

    assert!(depth == 0);

    // HACK: these are only on the function implementation for some reason
    result = result.replace(" __ptr64", "");

    (result, offset + 1)
}
