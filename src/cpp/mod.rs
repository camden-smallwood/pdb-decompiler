mod class;
mod r#enum;
mod module;
mod procedure;
mod typedef;

pub use self::{
    class::*, r#enum::*, module::*, procedure::*, typedef::*
};

use pdb2::{FallibleIterator, PrimitiveKind};

pub fn argument_list<'p>(
    machine_type: pdb2::MachineType,
    type_info: &pdb2::TypeInformation,
    type_finder: &pdb2::TypeFinder<'p>,
    type_index: pdb2::TypeIndex,
    names: Option<Vec<String>>,
) -> pdb2::Result<Vec<String>> {
    match type_finder.find(type_index)?.parse()? {
        pdb2::TypeData::ArgumentList(data) => {
            let mut args: Vec<String> = vec![];

            for (i, &arg_type) in data.arguments.iter().enumerate() {
                let arg_name = match names.as_ref() {
                    Some(names) if i < names.len() => Some(names[i].clone()),
                    _ => None
                };

                args.push(type_name(machine_type, type_info, type_finder, arg_type, arg_name, None, true)?);
            }

            Ok(args)
        }

        _ => Err(pdb2::Error::UnimplementedFeature("argument list of non-argument-list type".into()))
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

        pdb2::PrimitiveKind::I8 => "int8_t",
        pdb2::PrimitiveKind::U8 => "uint8_t",

        pdb2::PrimitiveKind::I16 => "int16_t",
        pdb2::PrimitiveKind::U16 => "uint16_t",

        pdb2::PrimitiveKind::I32 => "int32_t",
        pdb2::PrimitiveKind::U32 => "uint32_t",

        pdb2::PrimitiveKind::I64 => "int64_t",
        pdb2::PrimitiveKind::U64 => "uint64_t",

        pdb2::PrimitiveKind::WChar => "wchar_t",
        pdb2::PrimitiveKind::Char8 => "char8_t",
        pdb2::PrimitiveKind::RChar16 => "char16_t",
        pdb2::PrimitiveKind::RChar32 => "char32_t",

        pdb2::PrimitiveKind::HRESULT => "HRESULT",

        _ => panic!("Unhandled primitive kind: {kind:#?}"),
    }
}

pub fn type_name<'p>(
    machine_type: pdb2::MachineType,
    type_info: &pdb2::TypeInformation,
    type_finder: &pdb2::TypeFinder<'p>,
    type_index: pdb2::TypeIndex,
    declaration_name: Option<String>,
    parameter_names: Option<Vec<String>>,
    is_pointer: bool,
) -> pdb2::Result<String> {
    let type_item = type_finder.find(type_index)?;
    let type_data = type_item.parse()?;

    let mut name = match &type_data {
        pdb2::TypeData::Primitive(data) => {
            let mut name = primitive_name(data.kind).to_string();

            if data.indirection.is_some() {
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

        pdb2::TypeData::Class(data) => {
            let mut name = data.name.to_string().to_string();

            if let Some(field_name) = declaration_name {
                name.push(' ');
                name.push_str(field_name.as_str());
            }

            name
        }

        pdb2::TypeData::Enumeration(data) => {
            let mut name = data.name.to_string().to_string();

            if let Some(field_name) = declaration_name {
                name.push(' ');
                name.push_str(field_name.as_str());
            }

            name
        }

        pdb2::TypeData::Union(data) => {
            let mut name = data.name.to_string().to_string();

            if let Some(field_name) = declaration_name {
                name.push(' ');
                name.push_str(field_name.as_str());
            }

            name
        }

        pdb2::TypeData::Pointer(data) => match type_finder.find(data.underlying_type)?.parse() {
            Ok(pdb2::TypeData::Procedure(_)) | Ok(pdb2::TypeData::MemberFunction(_)) => {
                type_name(machine_type, type_info, type_finder, data.underlying_type, declaration_name, None, true)?
            }

            _ => {
                let mut name = format!(
                    "{} {}",
                    type_name(machine_type, type_info, type_finder, data.underlying_type, None, None, true)?,
                    if data.attributes.is_reference() {
                        "&"
                    } else {
                        "*"
                    }
                );

                if let Some(field_name) = declaration_name {
                    name.push(' ');
                    name.push_str(field_name.as_str());
                }

                name
            }
        },

        pdb2::TypeData::Modifier(data) => {
            if data.constant {
                format!(
                    "const {}",
                    type_name(machine_type, type_info, type_finder, data.underlying_type, declaration_name, None, true)?
                )
            } else if data.volatile {
                format!(
                    "volatile {}",
                    type_name(machine_type, type_info, type_finder, data.underlying_type, declaration_name, None, true)?
                )
            } else {
                type_name(machine_type, type_info, type_finder, data.underlying_type, declaration_name, None, true)?
            }
        }

        pdb2::TypeData::Array(data) => {
            let mut name = type_name(machine_type, type_info, type_finder, data.element_type, declaration_name, None, true)?;
            let mut element_size = type_size(machine_type, type_info, type_finder, data.element_type)?;

            if element_size == 0 {
                let element_type_data = type_finder.find(data.element_type)?.parse()?;

                let mut type_iter = type_info.iter();

                loop {
                    let current_type_item = match type_iter.next() {
                        Ok(Some(current_type_item)) => current_type_item,
                        Ok(None) | Err(_) => break,
                    };

                    let current_type_data = match current_type_item.parse() {
                        Ok(current_type_data) => current_type_data,
                        Err(_) => continue,
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
                        && let Ok(current_type_size) = type_size(machine_type, type_info, type_finder, current_type_item.index())
                        && current_type_size != 0
                    {
                        element_size = current_type_size;
                        break;
                    }
                }
            }

            for &size in data.dimensions.iter() {
                name = format!("{}[{}]", name, if element_size == 0 { size } else { size / element_size as u32 });
                element_size = size as usize;
            }
            
            name
        }

        pdb2::TypeData::Bitfield(data) => {
            let name = type_name(machine_type, type_info, type_finder, data.underlying_type, declaration_name, None, true)?;
            format!("{} : {}", name, data.length)
        }

        pdb2::TypeData::Procedure(data) => {
            let mut name = if data.attributes.is_constructor() || declaration_name.as_ref().map(|x| x.contains('~')).unwrap_or(false) {
                if let Some(field_name) = declaration_name.as_ref() {
                    field_name.clone()
                } else {
                    String::new()
                }
            } else {
                let mut name = match data.return_type {
                    Some(index) => type_name(machine_type, type_info, type_finder, index, None, None, true)?,
                    None => String::new(),
                };

                if is_pointer {
                    name.push_str("(*");
                } else if data.return_type.is_some() {
                    name.push(' ');
                }

                if let Some(field_name) = declaration_name {
                    name.push_str(field_name.as_str());
                }

                if is_pointer {
                    name.push(')');
                }

                name
            };

            name.push_str(
                format!(
                    "({})",
                    argument_list(machine_type, type_info, type_finder, data.argument_list, parameter_names)?.join(", ")
                ).as_str()
            );

            name
        }

        pdb2::TypeData::MemberFunction(data) => {
            let name = if data.attributes.is_constructor() || declaration_name.as_ref().map(|x| x.contains('~')).unwrap_or(false) {
                if let Some(field_name) = declaration_name.as_ref() {
                    field_name.clone()
                } else {
                    String::new()
                }
            } else {
                let mut name = type_name(machine_type, type_info, type_finder, data.return_type, None, None, true)?;

                if is_pointer {
                    name.push_str("(*");
                } else {
                    name.push(' ');
                }

                if let Some(field_name) = declaration_name {
                    name.push_str(field_name.as_str());
                }

                if is_pointer {
                    name.push(')');
                }

                name
            };

            let mut parameter_names = parameter_names;

            if let Some(parameter_names) = parameter_names.as_mut()
                && data.this_pointer_type.is_some()
                && !parameter_names.is_empty()
                && parameter_names[0] == "this"
            {
                parameter_names.remove(0);
            }

            format!("{}({})", name, argument_list(machine_type, type_info, type_finder, data.argument_list, parameter_names)?.join(", "))
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

pub fn type_size<'p>(
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

        pdb2::TypeData::Class(data) => Ok(data.size as usize),
        pdb2::TypeData::Enumeration(data) => type_size(machine_type, _type_info, type_finder, data.underlying_type),
        pdb2::TypeData::Union(data) => Ok(data.size as usize),
        pdb2::TypeData::Modifier(data) => type_size(machine_type, _type_info, type_finder, data.underlying_type),
        pdb2::TypeData::Bitfield(data) => type_size(machine_type, _type_info, type_finder, data.underlying_type),
        pdb2::TypeData::Array(data) => Ok(*data.dimensions.last().unwrap() as usize),

        _ => panic!(
            "Unhandled type data for type_size at index {}: {:#?}",
            type_index, type_data
        )
    }
}
