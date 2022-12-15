mod class;
mod r#enum;
mod module;
mod typedef;

pub use self::{
    class::*, r#enum::*, module::*, typedef::*
};

use pdb::{FallibleIterator, PrimitiveKind};

pub fn argument_list<'p>(
    machine_type: pdb::MachineType,
    type_info: &pdb::TypeInformation,
    type_finder: &pdb::TypeFinder<'p>,
    type_index: pdb::TypeIndex,
    names: Option<Vec<String>>,
) -> pdb::Result<Vec<String>> {
    match type_finder.find(type_index)?.parse()? {
        pdb::TypeData::ArgumentList(data) => {
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

        _ => Err(pdb::Error::UnimplementedFeature("argument list of non-argument-list type"))
    }
}

pub fn primitive_name(kind: PrimitiveKind) -> &'static str {
    match kind {
        pdb::PrimitiveKind::NoType => "...",

        pdb::PrimitiveKind::Void => "void",

        pdb::PrimitiveKind::I8 | pdb::PrimitiveKind::Char | pdb::PrimitiveKind::RChar => "char",
        pdb::PrimitiveKind::U8 | pdb::PrimitiveKind::UChar => "unsigned char",
        
        pdb::PrimitiveKind::WChar => "wchar_t",
        pdb::PrimitiveKind::RChar16 => "char16_t",
        pdb::PrimitiveKind::RChar32 => "char32_t",
        
        pdb::PrimitiveKind::I16 | pdb::PrimitiveKind::Short => "short",
        pdb::PrimitiveKind::U16 | pdb::PrimitiveKind::UShort => "unsigned short",
        
        pdb::PrimitiveKind::I32 | pdb::PrimitiveKind::Long => "long",
        pdb::PrimitiveKind::U32 | pdb::PrimitiveKind::ULong => "unsigned long",

        pdb::PrimitiveKind::I64 | pdb::PrimitiveKind::Quad => "long long",
        pdb::PrimitiveKind::U64 | pdb::PrimitiveKind::UQuad => "unsigned long long",

        pdb::PrimitiveKind::F32 => "float",
        pdb::PrimitiveKind::F64 => "double",
        pdb::PrimitiveKind::F80 => "long double",

        pdb::PrimitiveKind::Bool8 => "bool",

        pdb::PrimitiveKind::HRESULT => "HRESULT",

        _ => panic!("Unhandled primitive kind: {kind:#?}"),
    }
}

pub fn type_name<'p>(
    machine_type: pdb::MachineType,
    type_info: &pdb::TypeInformation,
    type_finder: &pdb::TypeFinder<'p>,
    type_index: pdb::TypeIndex,
    declaration_name: Option<String>,
    parameter_names: Option<Vec<String>>,
    is_pointer: bool,
) -> pdb::Result<String> {
    let type_item = type_finder.find(type_index)?;
    let type_data = type_item.parse()?;

    let mut name = match &type_data {
        pdb::TypeData::Primitive(data) => {
            let mut name = primitive_name(data.kind).to_string();

            if data.indirection.is_some() {
                name.push_str(" *");
            }

            if name != "..." {
                if let Some(field_name) = declaration_name {
                    name.push(' ');
                    name.push_str(field_name.as_str());
                }
            }

            name
        }

        pdb::TypeData::Class(data) => {
            let mut name = data.name.to_string().to_string();

            if let Some(field_name) = declaration_name {
                name.push(' ');
                name.push_str(field_name.as_str());
            }

            name
        }

        pdb::TypeData::Enumeration(data) => {
            let mut name = data.name.to_string().to_string();

            if let Some(field_name) = declaration_name {
                name.push(' ');
                name.push_str(field_name.as_str());
            }

            name
        }

        pdb::TypeData::Union(data) => {
            let mut name = data.name.to_string().to_string();

            if let Some(field_name) = declaration_name {
                name.push(' ');
                name.push_str(field_name.as_str());
            }

            name
        }

        pdb::TypeData::Pointer(data) => match type_finder.find(data.underlying_type)?.parse() {
            Ok(pdb::TypeData::Procedure(_)) | Ok(pdb::TypeData::MemberFunction(_)) => {
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

        pdb::TypeData::Modifier(data) => {
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

        pdb::TypeData::Array(data) => {
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
                        pdb::TypeData::Primitive(_) if matches!(element_type_data, pdb::TypeData::Primitive(_)) => (),
                        pdb::TypeData::Class(_) if matches!(element_type_data, pdb::TypeData::Class(_)) => (),
                        pdb::TypeData::Member(_) if matches!(element_type_data, pdb::TypeData::Member(_)) => (),
                        pdb::TypeData::MemberFunction(_) if matches!(element_type_data, pdb::TypeData::MemberFunction(_)) => (),
                        pdb::TypeData::OverloadedMethod(_) if matches!(element_type_data, pdb::TypeData::OverloadedMethod(_)) => (),
                        pdb::TypeData::Method(_) if matches!(element_type_data, pdb::TypeData::Method(_)) => (),
                        pdb::TypeData::StaticMember(_) if matches!(element_type_data, pdb::TypeData::StaticMember(_)) => (),
                        pdb::TypeData::Nested(_) if matches!(element_type_data, pdb::TypeData::Nested(_)) => (),
                        pdb::TypeData::BaseClass(_) if matches!(element_type_data, pdb::TypeData::BaseClass(_)) => (),
                        pdb::TypeData::VirtualBaseClass(_) if matches!(element_type_data, pdb::TypeData::VirtualBaseClass(_)) => (),
                        pdb::TypeData::VirtualFunctionTablePointer(_) if matches!(element_type_data, pdb::TypeData::VirtualFunctionTablePointer(_)) => (),
                        pdb::TypeData::Procedure(_) if matches!(element_type_data, pdb::TypeData::Procedure(_)) => (),
                        pdb::TypeData::Pointer(_) if matches!(element_type_data, pdb::TypeData::Pointer(_)) => (),
                        pdb::TypeData::Modifier(_) if matches!(element_type_data, pdb::TypeData::Modifier(_)) => (),
                        pdb::TypeData::Enumeration(_) if matches!(element_type_data, pdb::TypeData::Enumeration(_)) => (),
                        pdb::TypeData::Enumerate(_) if matches!(element_type_data, pdb::TypeData::Enumerate(_)) => (),
                        pdb::TypeData::Array(_) if matches!(element_type_data, pdb::TypeData::Array(_)) => (),
                        pdb::TypeData::Union(_) if matches!(element_type_data, pdb::TypeData::Union(_)) => (),
                        pdb::TypeData::Bitfield(_) if matches!(element_type_data, pdb::TypeData::Bitfield(_)) => (),
                        pdb::TypeData::FieldList(_) if matches!(element_type_data, pdb::TypeData::FieldList(_)) => (),
                        pdb::TypeData::ArgumentList(_) if matches!(element_type_data, pdb::TypeData::ArgumentList(_)) => (),
                        pdb::TypeData::MethodList(_) if matches!(element_type_data, pdb::TypeData::MethodList(_)) => (),
                        _ => continue
                    }
                    
                    if current_type_data.name() == element_type_data.name() {
                        if let Ok(current_type_size) = type_size(machine_type, type_info, type_finder, current_type_item.index()) {
                            if current_type_size != 0 {
                                element_size = current_type_size;
                                break;
                            }
                        }
                    }
                }
            }

            for &size in data.dimensions.iter() {
                name = format!("{}[{}]", name, if element_size == 0 { size } else { size / element_size as u32 });
                element_size = size as usize;
            }
            
            name
        }

        pdb::TypeData::Bitfield(data) => {
            let name = type_name(machine_type, type_info, type_finder, data.underlying_type, declaration_name, None, true)?;
            format!("{} : {}", name, data.length)
        }

        pdb::TypeData::Procedure(data) => {
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

        pdb::TypeData::MemberFunction(data) => {
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

            if let Some(parameter_names) = parameter_names.as_mut() {
                if data.this_pointer_type.is_some() && !parameter_names.is_empty() && parameter_names[0] == "this" {
                    parameter_names.remove(0);
                }
            }

            format!("{}({})", name, argument_list(machine_type, type_info, type_finder, data.argument_list, parameter_names)?.join(", "))
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
    machine_type: pdb::MachineType,
    _type_info: &pdb::TypeInformation,
    type_finder: &pdb::TypeFinder<'p>,
    type_index: pdb::TypeIndex,
) -> pdb::Result<usize> {
    let type_item = type_finder.find(type_index)?;
    let type_data = type_item.parse()?;

    match &type_data {
        pdb::TypeData::Primitive(pdb::PrimitiveType {
            indirection: Some(_),
            ..
        })
        | pdb::TypeData::Pointer(_)
        | pdb::TypeData::Procedure(_) => match machine_type {
            pdb::MachineType::X86 => Ok(4),
            pdb::MachineType::Amd64 => Ok(8),
            _ => panic!("Unhandled machine type: {machine_type}")
        }

        pdb::TypeData::Primitive(pdb::PrimitiveType {
            kind: pdb::PrimitiveKind::NoType,
            indirection: None,
        }) => Ok(0),

        pdb::TypeData::Primitive(pdb::PrimitiveType {
            kind: pdb::PrimitiveKind::Void,
            indirection: None,
        }) => Ok(0),

        pdb::TypeData::Primitive(pdb::PrimitiveType {
            kind: pdb::PrimitiveKind::Bool8,
            indirection: None,
        }) => Ok(1),

        pdb::TypeData::Primitive(pdb::PrimitiveType {
            kind: pdb::PrimitiveKind::Bool16,
            indirection: None,
        }) => Ok(2),

        pdb::TypeData::Primitive(pdb::PrimitiveType {
            kind: pdb::PrimitiveKind::Bool32,
            indirection: None,
        }) => Ok(4),

        pdb::TypeData::Primitive(pdb::PrimitiveType {
            kind: pdb::PrimitiveKind::Char,
            indirection: None,
        }) => Ok(1),

        pdb::TypeData::Primitive(pdb::PrimitiveType {
            kind: pdb::PrimitiveKind::UChar,
            indirection: None,
        }) => Ok(1),

        pdb::TypeData::Primitive(pdb::PrimitiveType {
            kind: pdb::PrimitiveKind::WChar,
            indirection: None,
        }) => Ok(2),

        pdb::TypeData::Primitive(pdb::PrimitiveType {
            kind: pdb::PrimitiveKind::RChar,
            indirection: None,
        }) => Ok(1),

        pdb::TypeData::Primitive(pdb::PrimitiveType {
            kind: pdb::PrimitiveKind::RChar16,
            indirection: None,
        }) => Ok(2),

        pdb::TypeData::Primitive(pdb::PrimitiveType {
            kind: pdb::PrimitiveKind::RChar32,
            indirection: None,
        }) => Ok(4),

        pdb::TypeData::Primitive(pdb::PrimitiveType {
            kind: pdb::PrimitiveKind::I8,
            indirection: None,
        }) => Ok(1),

        pdb::TypeData::Primitive(pdb::PrimitiveType {
            kind: pdb::PrimitiveKind::U8,
            indirection: None,
        }) => Ok(1),

        pdb::TypeData::Primitive(pdb::PrimitiveType {
            kind: pdb::PrimitiveKind::Short,
            indirection: None,
        }) => Ok(2),

        pdb::TypeData::Primitive(pdb::PrimitiveType {
            kind: pdb::PrimitiveKind::UShort,
            indirection: None,
        }) => Ok(2),

        pdb::TypeData::Primitive(pdb::PrimitiveType {
            kind: pdb::PrimitiveKind::I16,
            indirection: None,
        }) => Ok(2),

        pdb::TypeData::Primitive(pdb::PrimitiveType {
            kind: pdb::PrimitiveKind::U16,
            indirection: None,
        }) => Ok(2),

        pdb::TypeData::Primitive(pdb::PrimitiveType {
            kind: pdb::PrimitiveKind::Long,
            indirection: None,
        }) => Ok(4),

        pdb::TypeData::Primitive(pdb::PrimitiveType {
            kind: pdb::PrimitiveKind::ULong,
            indirection: None,
        }) => Ok(4),

        pdb::TypeData::Primitive(pdb::PrimitiveType {
            kind: pdb::PrimitiveKind::I32,
            indirection: None,
        }) => Ok(4),

        pdb::TypeData::Primitive(pdb::PrimitiveType {
            kind: pdb::PrimitiveKind::U32,
            indirection: None,
        }) => Ok(4),

        pdb::TypeData::Primitive(pdb::PrimitiveType {
            kind: pdb::PrimitiveKind::HRESULT,
            indirection: None,
        }) => Ok(4),

        pdb::TypeData::Primitive(pdb::PrimitiveType {
            kind: pdb::PrimitiveKind::Quad,
            indirection: None,
        }) => Ok(8),

        pdb::TypeData::Primitive(pdb::PrimitiveType {
            kind: pdb::PrimitiveKind::UQuad,
            indirection: None,
        }) => Ok(8),

        pdb::TypeData::Primitive(pdb::PrimitiveType {
            kind: pdb::PrimitiveKind::I64,
            indirection: None,
        }) => Ok(8),

        pdb::TypeData::Primitive(pdb::PrimitiveType {
            kind: pdb::PrimitiveKind::U64,
            indirection: None,
        }) => Ok(8),

        pdb::TypeData::Primitive(pdb::PrimitiveType {
            kind: pdb::PrimitiveKind::F32,
            indirection: None,
        }) => Ok(4),

        pdb::TypeData::Primitive(pdb::PrimitiveType {
            kind: pdb::PrimitiveKind::F64,
            indirection: None,
        }) => Ok(8),

        pdb::TypeData::Primitive(pdb::PrimitiveType {
            kind: pdb::PrimitiveKind::F80,
            indirection: None,
        }) => Ok(16),

        pdb::TypeData::Primitive(primitive_type) => {
            unimplemented!("primitive type size: {:?}", primitive_type)
        }

        pdb::TypeData::Class(data) => Ok(data.size as usize),
        pdb::TypeData::Enumeration(data) => type_size(machine_type, _type_info, type_finder, data.underlying_type),
        pdb::TypeData::Union(data) => Ok(data.size as usize),
        pdb::TypeData::Modifier(data) => type_size(machine_type, _type_info, type_finder, data.underlying_type),
        pdb::TypeData::Bitfield(data) => type_size(machine_type, _type_info, type_finder, data.underlying_type),
        pdb::TypeData::Array(data) => Ok(*data.dimensions.last().unwrap() as usize),

        _ => panic!(
            "Unhandled type data for type_size at index {}: {:#?}",
            type_index, type_data
        )
    }
}
