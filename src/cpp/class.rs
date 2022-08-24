use std::fmt;
use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BaseClass {
    pub type_name: String,
    pub index: pdb::TypeIndex,
    pub offset: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ClassMember {
    Class(Class),
    Enum(Enum),
    Field(Field),
    Method(Method),
    TypeDefinition(TypeDefinition),
}

impl fmt::Display for ClassMember {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Class(data) => write!(f, "{}", data),
            Self::Enum(data) => write!(f, "{}", data),
            Self::Field(data) => write!(f, "{}", data),
            Self::Method(data) => write!(f, "{}", data),
            Self::TypeDefinition(data) => write!(f, "{}", data),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Field {
    pub type_name: String,
    pub name: String,
    pub offset: Option<u64>,
    pub attributes: pdb::FieldAttributes
}

impl fmt::Display for Field {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.attributes.is_static() {
            write!(f, "static ")?;
        }

        if self.attributes.is_virtual() {
            write!(f, "virtual ")?;
        }

        write!(f, "{}", self.type_name)?;

        if self.attributes.is_pure_virtual() {
            write!(f, " = 0")?;
        }

        write!(f, "; // 0x{:X}", self.offset.unwrap_or(std::u64::MAX))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Method {
    pub name: String,
    pub return_type_name: String,
    pub arguments: Vec<String>,
    pub field_attributes: Option<pdb::FieldAttributes>,
    pub function_attributes: pdb::FunctionAttributes
}

impl fmt::Display for Method {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}{}{}({}){};",

            match self.field_attributes {
                Some(field_attributes) => if field_attributes.is_virtual() {
                    "virtual "
                } else if field_attributes.is_static() {
                    "static "
                } else {
                    ""
                },

                None => ""
            },

            if !(self.function_attributes.is_constructor() || self.name.contains('~')) {
                format!("{} ", self.return_type_name)
            } else {
                "".to_string()
            },

            self.name,

            self.arguments.join(", "),

            match self.field_attributes {
                Some(field_attributes) => if field_attributes.is_pure_virtual() {
                    " = 0"
                } else {
                    ""
                },

                None => ""
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Class {
    pub kind: Option<pdb::ClassKind>,
    pub is_union: bool,
    pub is_declaration: bool,
    pub name: String,
    pub index: pdb::TypeIndex,
    pub depth: u32,
    pub line: u32,
    pub size: u64,
    pub base_classes: Vec<BaseClass>,
    pub members: Vec<ClassMember>,
    pub field_attributes: Option<pdb::FieldAttributes>,
}

impl Class {
    pub fn add_derived_from(
        &mut self,
        _: &pdb::TypeFinder,
        _: pdb::TypeIndex
    ) -> pdb::Result<()> {
        // TODO
        Ok(())
    }

    pub fn add_members(
        &mut self,
        machine_type: pdb::MachineType,
        type_info: &pdb::TypeInformation,
        type_finder: &pdb::TypeFinder,
        type_index: pdb::TypeIndex
    ) -> pdb::Result<()> {
        match type_finder.find(type_index)?.parse() {
            Ok(pdb::TypeData::FieldList(data)) if self.is_union => {
                let mut members: Vec<ClassMember> = vec![];
                let mut prev_offset = 0;

                for field in &data.fields {
                    match field {
                        pdb::TypeData::Member(data) => {
                            if !members.is_empty() && data.offset <= prev_offset {
                                self.members.push(match members.len() {
                                    1 => members[0].clone(),
                                    _ => ClassMember::Class(Class {
                                        kind: Some(pdb::ClassKind::Struct),
                                        is_union: true,
                                        is_declaration: false,
                                        name: String::new(),
                                        index: pdb::TypeIndex(0),
                                        depth: 1,
                                        line: self.line,
                                        size: 0,
                                        base_classes: vec![],
                                        members,
                                        field_attributes: None,
                                    })
                                });

                                members = vec![];
                            }

                            members.push(ClassMember::Field(Field {
                                type_name: type_name(
                                    machine_type,
                                    type_info,
                                    type_finder,
                                    data.field_type,
                                    Some(data.name.to_string().to_string()),
                                    None,
                                    true,
                                )?,
                                name: data.name.to_string().to_string(),
                                offset: Some(data.offset),
                                attributes: data.attributes
                            }));

                            prev_offset = data.offset;
                        }

                        pdb::TypeData::Nested(ref nested_data) => {
                            let nested_type_item = type_finder.find(nested_data.nested_type)?;
                            let nested_type_data = nested_type_item.parse()?;
                            
                            match &nested_type_data {
                                pdb::TypeData::Class(data) => {
                                    let mut definition = Class {
                                        kind: Some(data.kind),
                                        is_union: false,
                                        is_declaration: false,
                                        name: nested_data.name.to_string().to_string(),
                                        index: nested_data.nested_type,
                                        depth: self.depth + 1,
                                        line: 0,
                                        size: data.size,
                                        base_classes: vec![],
                                        members: vec![],
                                        field_attributes: Some(nested_data.attributes),
                                    };
                    
                                    if let Some(derived_from) = data.derived_from {
                                        definition.add_derived_from(type_finder, derived_from)?;
                                    }
                    
                                    if data.properties.forward_reference() {
                                        definition.is_declaration = true;
                                    } else if let Some(fields) = data.fields {
                                        definition.add_members(machine_type, type_info, type_finder, fields)?;
                                    }
                                    
                                    let mut exists = false;
                                    
                                    for member in members.iter() {
                                        if let ClassMember::Class(other_definition) = member {
                                            if definition.kind == other_definition.kind
                                            && definition.name == other_definition.name
                                            && definition.size == other_definition.size
                                            && definition.base_classes.eq(&other_definition.base_classes)
                                            && definition.members.eq(&other_definition.members) {
                                                exists = true;
                                                break;
                                            }
                                        }
                                    }
                                    
                                    if !exists {
                                        members.push(ClassMember::Class(definition));
                                    }
                                }
                    
                                pdb::TypeData::Enumeration(data) => {
                                    let mut definition = Enum {
                                        name: nested_data.name.to_string().to_string(),
                                        index: nested_data.nested_type,
                                        depth: self.depth + 1,
                                        line: 0,
                                        underlying_type_name: type_name(machine_type, type_info, type_finder, data.underlying_type, None, None, true)?,
                                        is_declaration: false,
                                        values: vec![],
                                        field_attributes: Some(nested_data.attributes),
                                    };
                    
                                    if data.properties.forward_reference() {
                                        definition.is_declaration = true;
                                    } else {
                                        definition.add_members(type_finder, data.fields)?;
                                    }

                                    let mut exists = false;
                    
                                    for member in members.iter() {
                                        if let ClassMember::Enum(other_definition) = member {
                                            if definition.name == other_definition.name
                                            && definition.values.eq(&other_definition.values) {
                                                exists = true;
                                                break;
                                            }
                                        }
                                    }
                    
                                    if !exists {
                                        members.push(ClassMember::Enum(definition));
                                    }
                                }
                    
                                pdb::TypeData::Union(data) => {
                                    let mut definition = Class {
                                        kind: None,
                                        is_union: true,
                                        is_declaration: false,
                                        name: nested_data.name.to_string().to_string(),
                                        index: nested_data.nested_type,
                                        depth: self.depth + 1,
                                        line: 0,
                                        size: data.size,
                                        base_classes: vec![],
                                        members: vec![],
                                        field_attributes: Some(nested_data.attributes),
                                    };
                                    
                                    if data.properties.forward_reference() {
                                        definition.is_declaration = true;
                                    } else {
                                        definition.add_members(machine_type, type_info, type_finder, data.fields)?;
                                    }
                                    
                                    let mut exists = false;
                                    
                                    for member in members.iter() {
                                        if let ClassMember::Class(other_definition) = member {
                                            if definition.kind == other_definition.kind
                                            && definition.name == other_definition.name
                                            && definition.size == other_definition.size
                                            && definition.base_classes.eq(&other_definition.base_classes)
                                            && definition.members.eq(&other_definition.members) {
                                                exists = true;
                                                break;
                                            }
                                        }
                                    }
                                    
                                    if !exists {
                                        members.push(ClassMember::Class(definition));
                                    }
                                }
                    
                                pdb::TypeData::Pointer(data) => {
                                    let type_name = type_name(machine_type, type_info, type_finder, data.underlying_type, Some(nested_data.name.to_string().to_string()), None, true)?;

                                    members.push(ClassMember::TypeDefinition(TypeDefinition {
                                        type_name,
                                        underlying_type: data.underlying_type,
                                        field_attributes: nested_data.attributes,
                                        pointer_attributes: Some(data.attributes),
                                        containing_class: data.containing_class,
                                    }));
                                }

                                pdb::TypeData::Modifier(data) => {
                                    let mut type_name = String::new();

                                    if data.constant {
                                        type_name.push_str("const ");
                                    }

                                    if data.volatile {
                                        type_name.push_str("volatile ");
                                    }

                                    type_name.push_str(self::type_name(machine_type, type_info, type_finder, data.underlying_type, Some(nested_data.name.to_string().to_string()), None, true)?.as_str());

                                    members.push(ClassMember::TypeDefinition(TypeDefinition {
                                        type_name,
                                        underlying_type: data.underlying_type,
                                        field_attributes: nested_data.attributes,
                                        pointer_attributes: None,
                                        containing_class: None,
                                    }));
                                }

                                pdb::TypeData::Primitive(data) => {
                                    let mut type_name = primitive_name(data.kind).to_string();

                                    if data.indirection.is_some() {
                                        type_name.push_str(" *");
                                    } else {
                                        type_name.push(' ');
                                    }

                                    type_name.push_str(nested_data.name.to_string().to_string().as_str());

                                    members.push(ClassMember::TypeDefinition(TypeDefinition {
                                        type_name,
                                        underlying_type: nested_data.nested_type,
                                        field_attributes: nested_data.attributes,
                                        pointer_attributes: None,
                                        containing_class: None,
                                    }));
                                }

                                pdb::TypeData::Array(data) => {
                                    let mut type_name = type_name(machine_type, type_info, type_finder, data.element_type, Some(nested_data.name.to_string().to_string()), None, true)?;
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
                                        type_name = format!("{}[{}]", type_name, if element_size == 0 { size } else { size / element_size as u32 });
                                        element_size = size as usize;
                                    }

                                    members.push(ClassMember::TypeDefinition(TypeDefinition {
                                        type_name,
                                        underlying_type: nested_data.nested_type,
                                        field_attributes: nested_data.attributes,
                                        pointer_attributes: None,
                                        containing_class: None,
                                    }));
                                }

                                pdb::TypeData::Procedure(data) => println!(
                                    "WARNING: unhandled nested procedure at index {} in Class::add_member - {:?}",
                                    nested_type_item.index(),
                                    data
                                ),

                                _ => return Err(
                                    pdb::Error::IoError(
                                        std::io::Error::new(
                                            std::io::ErrorKind::InvalidInput,
                                            format!(
                                                "Unhandled nested type data at index {} in Class::add_member: {:#?}",
                                                nested_type_item.index(), nested_type_data
                                            )
                                        )
                                    )
                                )
                            }
                        }

                        pdb::TypeData::Method(ref data) => match data.name.to_string().to_string().as_str() {
                            // Ignore compiler-generated functions:
                            "__vecDelDtor" | "__local_vftable_ctor_closure" | "__autoclassinit" => (),
                            
                            _ => {
                                let method = match type_finder.find(data.method_type)?.parse() {
                                    Ok(pdb::TypeData::MemberFunction(function_data)) => Method {
                                        name: data.name.to_string().to_string(),
                                        return_type_name: type_name(machine_type, type_info, type_finder, function_data.return_type, None, None, true)?,
                                        arguments: argument_list(machine_type, type_info, type_finder, function_data.argument_list, None)?,
                                        field_attributes: Some(data.attributes),
                                        function_attributes: function_data.attributes
                                    },
                        
                                    Ok(data) => panic!("Unhandled member function type data in Class::add_member - {:#?}", data),
                                    Err(err) => panic!("Unhandled error in Class::add_member - {}", err)
                                };

                                members.push(ClassMember::Method(method));
                            }
                        }

                        pdb::TypeData::OverloadedMethod(ref data) => {
                            match type_finder.find(data.method_list)?.parse() {
                                Ok(pdb::TypeData::MethodList(method_list)) => {
                                    for pdb::MethodListEntry {
                                        attributes,
                                        method_type,
                                        ..
                                    } in method_list.methods {
                                        match data.name.to_string().to_string().as_str() {
                                            "__vecDelDtor" | "__local_vftable_ctor_closure" | "__autoclassinit" => (),

                                            _ => {
                                                let method = match type_finder.find(method_type)?.parse() {
                                                    Ok(pdb::TypeData::MemberFunction(function_data)) => Method {
                                                        name: data.name.to_string().to_string(),
                                                        return_type_name: type_name(machine_type, type_info, type_finder, function_data.return_type, None, None, true)?,
                                                        arguments: argument_list(machine_type, type_info, type_finder, function_data.argument_list, None)?,
                                                        field_attributes: Some(attributes),
                                                        function_attributes: function_data.attributes
                                                    },
                                        
                                                    Ok(data) => panic!("Unhandled member function type data in Class::add_member - {:#?}", data),
                                                    Err(err) => panic!("Unhandled error in Class::add_member - {}", err)
                                                };
                                
                                                members.push(ClassMember::Method(method));
                                            }
                                        }
                                    }
                                }

                                Ok(other) => panic!(
                                    "error: unexpected type in Class::add_member - {}, got {} -> {:?}",
                                    "processing OverloadedMethod, expected MethodList",
                                    data.method_list,
                                    other
                                ),

                                Err(err) => panic!("Unhandled error in Class::add_member - {}", err)
                            }
                        }

                        other => panic!("Unexpected type in Class::add_members, got {} -> {:?}", type_index, other),
                    }
                }

                if !members.is_empty() {
                    self.members.push(match members.len() {
                        1 => members[0].clone(),
                        _ => ClassMember::Class(Class {
                            kind: Some(pdb::ClassKind::Struct),
                            is_union: true,
                            is_declaration: false,
                            name: String::new(),
                            index: pdb::TypeIndex(0),
                            depth: 1,
                            line: self.line,
                            size: 0,
                            base_classes: vec![],
                            members,
                            field_attributes: None,
                        })
                    });
                }

                if let Some(continuation) = data.continuation {
                    // recurse
                    self.add_members(machine_type, type_info, type_finder, continuation)?;
                }
            }

            Ok(pdb::TypeData::FieldList(data)) => {
                for field in &data.fields {
                    self.add_member(machine_type, type_info, type_finder, field)?;
                }

                if let Some(continuation) = data.continuation {
                    // recurse
                    self.add_members(machine_type, type_info, type_finder, continuation)?;
                }
            }

            Ok(pdb::TypeData::Primitive(pdb::PrimitiveType { kind: pdb::PrimitiveKind::NoType, indirection: None })) => {
                self.is_declaration = true;
            }

            Ok(other) => panic!("Unexpected type in Class::add_members, got {} -> {:?}", type_index, other),

            Err(err) => println!("WARNING: failed to find type in Class::add_members, skipping: {err}")
        }

        Ok(())
    }

    fn add_member(
        &mut self,
        machine_type: pdb::MachineType,
        type_info: &pdb::TypeInformation,
        type_finder: &pdb::TypeFinder,
        field: &pdb::TypeData
    ) -> pdb::Result<()> {
        match *field {
            pdb::TypeData::Member(ref data) => {
                //
                // TODO: calculate union layout
                //

                self.members.push(ClassMember::Field(Field {
                    type_name: type_name(
                        machine_type,
                        type_info,
                        type_finder,
                        data.field_type,
                        Some(data.name.to_string().to_string()),
                        None,
                        true,
                    )?,
                    name: data.name.to_string().to_string(),
                    offset: Some(data.offset),
                    attributes: data.attributes
                }));
            }

            pdb::TypeData::StaticMember(ref data) => {
                //
                // TODO: determine if we need to calculate union layout?
                //

                self.members.push(ClassMember::Field(Field {
                    type_name: format!(
                        "static {}",
                        type_name(
                            machine_type,
                            type_info,
                            type_finder,
                            data.field_type,
                            Some(data.name.to_string().to_string()),
                            None,
                            true,
                        )?
                    ),
                    name: data.name.to_string().to_string(),
                    offset: None,
                    attributes: data.attributes
                }));
            }

            pdb::TypeData::BaseClass(ref data) => {
                self.base_classes.push(BaseClass {
                    type_name: format!(
                        "{}{}",
                        match data.attributes.access() {
                            0 => "private ",
                            1 => "protected ",
                            2 => "public ",
                            _ => ""
                        },
                        type_name(machine_type, type_info, type_finder, data.base_class, None, None, true)?
                    ),
                    offset: data.offset,
                    index: data.base_class
                });
            }

            pdb::TypeData::VirtualBaseClass(ref data) => {
                self.base_classes.push(BaseClass {
                    type_name: format!(
                        "{}virtual {}",
                        match data.attributes.access() {
                            0 => "private ",
                            1 => "protected ",
                            2 => "public ",
                            _ => ""
                        },
                        type_name(machine_type, type_info, type_finder, data.base_class, None, None, true)?
                    ),
                    offset: data.base_pointer_offset,
                    index: data.base_class
                });
            }

            pdb::TypeData::VirtualFunctionTablePointer(_) => (), // TODO: does this need handling?

            pdb::TypeData::Method(ref data) => match data.name.to_string().to_string().as_str() {
                // Ignore compiler-generated functions:
                "__vecDelDtor" | "__local_vftable_ctor_closure" | "__autoclassinit" => (),
                
                _ => {
                    let method = match type_finder.find(data.method_type)?.parse() {
                        Ok(pdb::TypeData::MemberFunction(function_data)) => Method {
                            name: data.name.to_string().to_string(),
                            return_type_name: type_name(machine_type, type_info, type_finder, function_data.return_type, None, None, true)?,
                            arguments: argument_list(machine_type, type_info, type_finder, function_data.argument_list, None)?,
                            field_attributes: Some(data.attributes),
                            function_attributes: function_data.attributes
                        },
            
                        Ok(data) => panic!("Unhandled member function type data in Class::add_member - {:#?}", data),
                        Err(err) => panic!("Unhandled error in Class::add_member - {}", err)
                    };

                    self.members.push(ClassMember::Method(method));
                }
            }

            pdb::TypeData::OverloadedMethod(ref data) => {
                match type_finder.find(data.method_list)?.parse() {
                    Ok(pdb::TypeData::MethodList(method_list)) => {
                        for pdb::MethodListEntry {
                            attributes,
                            method_type,
                            ..
                        } in method_list.methods {
                            match data.name.to_string().to_string().as_str() {
                                "__vecDelDtor" | "__local_vftable_ctor_closure" | "__autoclassinit" => (),

                                _ => {
                                    let method = match type_finder.find(method_type)?.parse() {
                                        Ok(pdb::TypeData::MemberFunction(function_data)) => Method {
                                            name: data.name.to_string().to_string(),
                                            return_type_name: type_name(machine_type, type_info, type_finder, function_data.return_type, None, None, true)?,
                                            arguments: argument_list(machine_type, type_info, type_finder, function_data.argument_list, None)?,
                                            field_attributes: Some(attributes),
                                            function_attributes: function_data.attributes
                                        },
                            
                                        Ok(data) => panic!("Unhandled member function type data in Class::add_member - {:#?}", data),
                                        Err(err) => panic!("Unhandled error in Class::add_member - {}", err)
                                    };
                    
                                    self.members.push(ClassMember::Method(method));
                                }
                            }
                        }
                    }

                    Ok(other) => panic!(
                        "error: unexpected type in Class::add_member - {}, got {} -> {:?}",
                        "processing OverloadedMethod, expected MethodList",
                        data.method_list,
                        other
                    ),

                    Err(err) => panic!("Unhandled error in Class::add_member - {}", err)
                }
            }

            pdb::TypeData::Nested(ref nested_data) => {
                let nested_type_item = type_finder.find(nested_data.nested_type)?;
                let nested_type_data = nested_type_item.parse()?;
                
                match &nested_type_data {
                    pdb::TypeData::Class(data) => {
                        let mut definition = Class {
                            kind: Some(data.kind),
                            is_union: false,
                            is_declaration: false,
                            name: nested_data.name.to_string().to_string(),
                            index: nested_data.nested_type,
                            depth: self.depth + 1,
                            line: 0,
                            size: data.size,
                            base_classes: vec![],
                            members: vec![],
                            field_attributes: Some(nested_data.attributes),
                        };
        
                        if let Some(derived_from) = data.derived_from {
                            definition.add_derived_from(type_finder, derived_from)?;
                        }
        
                        if data.properties.forward_reference() {
                            definition.is_declaration = true;
                        } else if let Some(fields) = data.fields {
                            definition.add_members(machine_type, type_info, type_finder, fields)?;
                        }
                        
                        let mut exists = false;
                        
                        for member in self.members.iter() {
                            if let ClassMember::Class(other_definition) = member {
                                if definition.kind == other_definition.kind
                                && definition.name == other_definition.name
                                && definition.size == other_definition.size
                                && definition.base_classes.eq(&other_definition.base_classes)
                                && definition.members.eq(&other_definition.members) {
                                    exists = true;
                                    break;
                                }
                            }
                        }
                        
                        if !exists {
                            self.members.push(ClassMember::Class(definition));
                        }
                    }
        
                    pdb::TypeData::Enumeration(data) => {
                        let mut definition = Enum {
                            name: nested_data.name.to_string().to_string(),
                            index: nested_data.nested_type,
                            depth: self.depth + 1,
                            line: 0,
                            underlying_type_name: type_name(machine_type, type_info, type_finder, data.underlying_type, None, None, true)?,
                            is_declaration: false,
                            values: vec![],
                            field_attributes: Some(nested_data.attributes),
                        };
        
                        if data.properties.forward_reference() {
                            definition.is_declaration = true;
                        } else {
                            definition.add_members(type_finder, data.fields)?;
                        }

                        let mut exists = false;
        
                        for member in self.members.iter() {
                            if let ClassMember::Enum(other_definition) = member {
                                if definition.name == other_definition.name
                                && definition.values.eq(&other_definition.values) {
                                    exists = true;
                                    break;
                                }
                            }
                        }
        
                        if !exists {
                            self.members.push(ClassMember::Enum(definition));
                        }
                    }
        
                    pdb::TypeData::Union(data) => {
                        let mut definition = Class {
                            kind: None,
                            is_union: true,
                            is_declaration: false,
                            name: nested_data.name.to_string().to_string(),
                            index: nested_data.nested_type,
                            depth: self.depth + 1,
                            line: 0,
                            size: data.size,
                            base_classes: vec![],
                            members: vec![],
                            field_attributes: Some(nested_data.attributes),
                        };
                        
                        if data.properties.forward_reference() {
                            definition.is_declaration = true;
                        } else {
                            definition.add_members(machine_type, type_info, type_finder, data.fields)?;
                        }
                        
                        let mut exists = false;
                        
                        for member in self.members.iter() {
                            if let ClassMember::Class(other_definition) = member {
                                if definition.kind == other_definition.kind
                                && definition.name == other_definition.name
                                && definition.size == other_definition.size
                                && definition.base_classes.eq(&other_definition.base_classes)
                                && definition.members.eq(&other_definition.members) {
                                    exists = true;
                                    break;
                                }
                            }
                        }
                        
                        if !exists {
                            self.members.push(ClassMember::Class(definition));
                        }
                    }
        
                    pdb::TypeData::Pointer(data) => {
                        let type_name = type_name(machine_type, type_info, type_finder, data.underlying_type, Some(nested_data.name.to_string().to_string()), None, true)?;

                        self.members.push(ClassMember::TypeDefinition(TypeDefinition {
                            type_name,
                            underlying_type: data.underlying_type,
                            field_attributes: nested_data.attributes,
                            pointer_attributes: Some(data.attributes),
                            containing_class: data.containing_class,
                        }));
                    }

                    pdb::TypeData::Modifier(data) => {
                        let mut type_name = String::new();

                        if data.constant {
                            type_name.push_str("const ");
                        }

                        if data.volatile {
                            type_name.push_str("volatile ");
                        }

                        type_name.push_str(self::type_name(machine_type, type_info, type_finder, data.underlying_type, Some(nested_data.name.to_string().to_string()), None, true)?.as_str());

                        self.members.push(ClassMember::TypeDefinition(TypeDefinition {
                            type_name,
                            underlying_type: data.underlying_type,
                            field_attributes: nested_data.attributes,
                            pointer_attributes: None,
                            containing_class: None,
                        }));
                    }

                    pdb::TypeData::Primitive(data) => {
                        let mut type_name = primitive_name(data.kind).to_string();

                        if data.indirection.is_some() {
                            type_name.push_str(" *");
                        } else {
                            type_name.push(' ');
                        }

                        type_name.push_str(nested_data.name.to_string().to_string().as_str());

                        self.members.push(ClassMember::TypeDefinition(TypeDefinition {
                            type_name,
                            underlying_type: nested_data.nested_type,
                            field_attributes: nested_data.attributes,
                            pointer_attributes: None,
                            containing_class: None,
                        }));
                    }

                    pdb::TypeData::Array(data) => {
                        let mut type_name = type_name(machine_type, type_info, type_finder, data.element_type, Some(nested_data.name.to_string().to_string()), None, true)?;
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
                            type_name = format!("{}[{}]", type_name, if element_size == 0 { size } else { size / element_size as u32 });
                            element_size = size as usize;
                        }

                        self.members.push(ClassMember::TypeDefinition(TypeDefinition {
                            type_name,
                            underlying_type: nested_data.nested_type,
                            field_attributes: nested_data.attributes,
                            pointer_attributes: None,
                            containing_class: None,
                        }));
                    }

                    pdb::TypeData::Procedure(data) => println!(
                        "WARNING: unhandled nested procedure at index {} in Class::add_member - {:?}",
                        nested_type_item.index(),
                        data
                    ),

                    _ => return Err(
                        pdb::Error::IoError(
                            std::io::Error::new(
                                std::io::ErrorKind::InvalidInput,
                                format!(
                                    "Unhandled nested type data at index {} in Class::add_member: {:#?}",
                                    nested_type_item.index(), nested_type_data
                                )
                            )
                        )
                    )
                }
            }

            ref data => panic!("Unhandled type data in Class::add_member - {} - {:#?}", self.name, data)
        }

        Ok(())
    }
}

impl fmt::Display for Class {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}{}",
            match self.kind {
                Some(pdb::ClassKind::Class) => "class",
                Some(pdb::ClassKind::Struct) => "struct",
                Some(pdb::ClassKind::Interface) => "interface",
                None => {
                    assert!(self.is_union);
                    "union"
                }
            },
            if self.name.is_empty() {
                format!("")
            } else {
                format!(" {}", self.name)
            }
        )?;

        if !self.base_classes.is_empty() {
            let last_base_class_index = self.base_classes.len() - 1;

            for (i, base) in self.base_classes.iter().enumerate() {
                if i == 0 {
                    writeln!(f, " :")?;
                } else {
                    writeln!(f)?;
                }
                        
                for _ in 0..self.depth {
                    write!(f, "\t")?;
                }
                
                write!(f, "\t{}{}", base.type_name, if i == last_base_class_index { "" } else { "," })?;
            }
        }

        if self.is_declaration {
            write!(f, ";")?;
            return Ok(())
        }
        
        writeln!(f)?;

        for _ in 0..self.depth {
            write!(f, "\t")?;
        }

        writeln!(f, "{{")?;

        let mut prev_access: Option<&str> = match self.kind.as_ref() {
            Some(pdb::ClassKind::Struct) => Some("public"),
            _ => None
        };

        for member in self.members.iter() {
            let field_attributes = match member {
                ClassMember::Class(data) => data.field_attributes,
                ClassMember::Enum(data) => data.field_attributes,
                ClassMember::Field(data) => Some(data.attributes),
                ClassMember::Method(data) => data.field_attributes,
                ClassMember::TypeDefinition(data) => Some(data.field_attributes),
            };

            let member_access = match field_attributes.map(|a| a.access()) {
                Some(1) => Some("private"),
                Some(2) => Some("protected"),
                Some(3) => Some("public"),
                _ => prev_access
            };

            if member_access != prev_access {
                if let Some(member_access) = member_access {
                    for _ in 0..self.depth {
                        write!(f, "\t")?;
                    }
            
                    writeln!(f, "{}:", member_access)?;
                }

                prev_access = member_access;
            }

            for _ in 0..self.depth {
                write!(f, "\t")?;
            }
    
            writeln!(f, "\t{}", member)?;
        }

        for _ in 0..self.depth {
            write!(f, "\t")?;
        }

        write!(f, "}};")
    }
}
