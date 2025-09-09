#![allow(dead_code)]

use crate::cpp::{Class, get_member_function_modifier, type_size};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeName {
    Primitive(PrimitiveType),
    Pointer(PointerType),
    Array(ArrayType),
    Function(FunctionType),
    Path(PathType),
    Enum(EnumType),
    Struct(StructType),
    Union(UnionType),
    Class(ClassType),
    Interface(InterfaceType)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PrimitiveType {
    pub is_const: bool,
    pub is_volatile: bool,
    pub kind: pdb2::PrimitiveKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PointerType {
    pub is_flat_32: bool,
    pub is_volatile: bool,
    pub is_const: bool,
    pub is_unaligned: bool,
    pub is_restrict: bool,
    pub is_mocom: bool,
    pub kind: PointerKind,
    pub mode: pdb2::PointerMode,
    pub underlying_type: Box<TypeName>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PointerKind {
    Near16,
    Far16,
    Huge16,
    BaseSeg,
    BaseVal,
    BaseSegVal,
    BaseAddr,
    BaseSegAddr,
    BaseType,
    BaseSelf,
    Near32,
    Far32,
    Ptr64,
    Near64,
    Near128,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArrayType {
    pub element_type: Box<TypeName>,
    pub dimensions: Vec<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionType {
    pub return_type: Option<Box<TypeName>>,
    pub this_type: Option<Box<TypeName>>,
    pub parameter_types: Vec<TypeName>,
    pub is_const: bool,
    pub is_volatile: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PathType {
    pub is_const: bool,
    pub is_volatile: bool,
    pub segments: Vec<PathTypeSegment>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PathTypeSegment {
    pub name: String,
    pub template: Option<PathTypeTemplate>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PathTypeTemplate {
    pub type_names: Vec<TypeName>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumType {
    pub path: PathType,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructType {
    pub path: PathType,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnionType {
    pub path: PathType,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassType {
    pub path: PathType,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InterfaceType {
    pub path: PathType,
}

impl PathType {
    pub fn parse(string: String) -> Self {
        todo!("parse path string: \"{string}\"")
    }
}

pub fn type_index_to_type_name<'p>(
    class_table: &mut Vec<Rc<RefCell<Class>>>,
    type_sizes: &mut HashMap<String, u64>,
    machine_type: pdb2::MachineType,
    type_info: &pdb2::TypeInformation,
    type_finder: &pdb2::TypeFinder<'p>,
    type_index: pdb2::TypeIndex,
    modifier: Option<&pdb2::ModifierType>,
    bitfield: Option<&pdb2::BitfieldType>,
) -> pdb2::Result<TypeName> {
    let type_item = type_finder.find(type_index)?;
    let type_data = type_item.parse()?;

    match &type_data {
        pdb2::TypeData::Modifier(data) => {
            type_index_to_type_name(
                class_table,
                type_sizes,
                machine_type,
                type_info,
                type_finder,
                data.underlying_type,
                Some(data),
                bitfield,
            )
        }

        pdb2::TypeData::Bitfield(data) => {
            type_index_to_type_name(
                class_table,
                type_sizes,
                machine_type,
                type_info,
                type_finder,
                data.underlying_type,
                modifier,
                Some(data),
            )
        }

        pdb2::TypeData::Primitive(data) => {
            let mut result = PrimitiveType {
                is_const: false,
                is_volatile: false,
                kind: data.kind,
            };

            if let Some(modifier) = modifier {
                result.is_const = modifier.constant;
                result.is_volatile = modifier.volatile;
            }

            if let Some(indirection) = data.indirection.as_ref() {
                return Ok(TypeName::Pointer(PointerType {
                    is_flat_32: false,
                    is_volatile: false,
                    is_const: false,
                    is_unaligned: false,
                    is_restrict: false,
                    is_mocom: false,
                    kind: match indirection {
                        pdb2::Indirection::Near16 => PointerKind::Near16,
                        pdb2::Indirection::Far16 => PointerKind::Far16,
                        pdb2::Indirection::Huge16 => PointerKind::Huge16,
                        pdb2::Indirection::Near32 => PointerKind::Near32,
                        pdb2::Indirection::Far32 => PointerKind::Far32,
                        pdb2::Indirection::Near64 => PointerKind::Near64,
                        pdb2::Indirection::Near128 => PointerKind::Near128,
                    },
                    mode: pdb2::PointerMode::Pointer,
                    underlying_type: Box::new(TypeName::Primitive(result)),
                }));
            }

            Ok(TypeName::Primitive(result))
        }

        pdb2::TypeData::Pointer(data) => Ok(TypeName::Pointer(PointerType {
            is_flat_32: data.attributes.is_flat_32(),
            is_volatile: data.attributes.is_volatile(),
            is_const: data.attributes.is_const(),
            is_unaligned: data.attributes.is_unaligned(),
            is_restrict: data.attributes.is_restrict(),
            is_mocom: data.attributes.is_mocom(),
            kind: match data.attributes.pointer_kind() {
                pdb2::PointerKind::Near16 => PointerKind::Near16,
                pdb2::PointerKind::Far16 => PointerKind::Far16,
                pdb2::PointerKind::Huge16 => PointerKind::Huge16,
                pdb2::PointerKind::BaseSeg => PointerKind::BaseSeg,
                pdb2::PointerKind::BaseVal => PointerKind::BaseVal,
                pdb2::PointerKind::BaseSegVal => PointerKind::BaseSegVal,
                pdb2::PointerKind::BaseAddr => PointerKind::BaseAddr,
                pdb2::PointerKind::BaseSegAddr => PointerKind::BaseSegAddr,
                pdb2::PointerKind::BaseType => PointerKind::BaseType,
                pdb2::PointerKind::BaseSelf => PointerKind::BaseSelf,
                pdb2::PointerKind::Near32 => PointerKind::Near32,
                pdb2::PointerKind::Far32 => PointerKind::Far32,
                pdb2::PointerKind::Ptr64 => PointerKind::Ptr64,
            },
            mode: data.attributes.pointer_mode(),
            underlying_type: Box::new(type_index_to_type_name(
                class_table,
                type_sizes,
                machine_type,
                type_info,
                type_finder,
                data.underlying_type,
                modifier,
                bitfield,
            )?),
        })),

        pdb2::TypeData::Array(data) => {
            let element_type = type_index_to_type_name(class_table, type_sizes, machine_type, type_info, type_finder, data.element_type, modifier, bitfield)?;
            let mut element_size = type_size(class_table, type_sizes, machine_type, type_info, type_finder, data.element_type)?;
            let mut dimensions = vec![];

            for &size in data.dimensions.iter() {
                dimensions.push(if element_size == 0 { size } else { size / element_size as u32 } as usize);
                element_size = size as usize;
            }
            
            Ok(TypeName::Array(ArrayType {
                element_type: Box::new(element_type),
                dimensions,
            }))
        }

        pdb2::TypeData::Procedure(data) => {
            Ok(TypeName::Function(FunctionType {
                return_type: match data.return_type {
                    Some(type_index) => Some(Box::new(type_index_to_type_name(
                        class_table,
                        type_sizes,
                        machine_type,
                        type_info,
                        type_finder,
                        type_index,
                        modifier,
                        bitfield,
                    )?)),

                    None => {
                        if data.attributes.is_constructor() || data.attributes.is_constructor_with_virtual_bases() {
                            None
                        } else {
                            Some(Box::new(TypeName::Primitive(PrimitiveType {
                                is_const: false,
                                is_volatile: false,
                                kind: pdb2::PrimitiveKind::Void,
                            })))
                        }
                    }
                },

                this_type: None,

                parameter_types: match type_finder.find(data.argument_list)?.parse()? {
                    pdb2::TypeData::ArgumentList(data) => {
                        let mut args = vec![];

                        for &arg_type in data.arguments.iter() {
                            args.push(type_index_to_type_name(
                                class_table,
                                type_sizes,
                                machine_type,
                                type_info,
                                type_finder,
                                arg_type,
                                None,
                                None,
                            )?);
                        }

                        args
                    }

                    _ => todo!(),
                },
            
                is_const: false,
                is_volatile: false,
            }))
        }

        pdb2::TypeData::MemberFunction(data) => {
            let mut result = FunctionType {
                return_type: Some(Box::new(type_index_to_type_name(
                    class_table,
                    type_sizes,
                    machine_type,
                    type_info,
                    type_finder,
                    data.return_type,
                    modifier,
                    bitfield,
                )?)),

                this_type: match data.this_pointer_type {
                    Some(type_index) =>  Some(Box::new(type_index_to_type_name(
                        class_table,
                        type_sizes,
                        machine_type,
                        type_info,
                        type_finder,
                        type_index,
                        modifier,
                        bitfield,
                    )?)),

                    None => None,
                },

                parameter_types: match type_finder.find(data.argument_list)?.parse()? {
                    pdb2::TypeData::ArgumentList(data) => {
                        let mut args = vec![];

                        for &arg_type in data.arguments.iter() {
                            args.push(type_index_to_type_name(
                                class_table,
                                type_sizes,
                                machine_type,
                                type_info,
                                type_finder,
                                arg_type,
                                None,
                                None,
                            )?);
                        }

                        args
                    }

                    _ => todo!(),
                },

                is_const: false,
                is_volatile: false,
            };

            if let Some(modifier) = get_member_function_modifier(data, type_finder) {
                result.is_const = modifier.constant;
                result.is_volatile = modifier.volatile;
            }

            Ok(TypeName::Function(result))
        }

        pdb2::TypeData::Enumeration(data) => {
            let mut result = PathType::parse(data.name.to_string().to_string());

            if let Some(modifier) = modifier {
                result.is_const = modifier.constant;
                result.is_volatile = modifier.volatile;
            }

            Ok(TypeName::Enum(EnumType { path: result }))
        }

        pdb2::TypeData::Class(data) => {
            let mut result = PathType::parse(data.name.to_string().to_string());

            if let Some(modifier) = modifier {
                result.is_const = modifier.constant;
                result.is_volatile = modifier.volatile;
            }

            match data.kind {
                pdb2::ClassKind::Class => Ok(TypeName::Class(ClassType { path: result })),
                pdb2::ClassKind::Struct => Ok(TypeName::Struct(StructType { path: result })),
                pdb2::ClassKind::Interface => Ok(TypeName::Interface(InterfaceType { path: result })),
            }
        }

        pdb2::TypeData::Union(data) => {
            let mut result = PathType::parse(data.name.to_string().to_string());

            if let Some(modifier) = modifier {
                result.is_const = modifier.constant;
                result.is_volatile = modifier.volatile;
            }

            Ok(TypeName::Union(UnionType { path: result }))
        }

        _ => panic!(
            "Unhandled type data for type_name at index {}: {:#?}",
            type_index, type_data
        )
    }
}
