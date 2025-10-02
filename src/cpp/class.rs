use super::*;
use std::{cell::RefCell, collections::BTreeMap, fmt, ops::Range, rc::Rc};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BaseClass {
    pub type_name: String,
    pub index: pdb2::TypeIndex,
    pub offset: u32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ClassMember {
    Class(Rc<RefCell<Class>>),
    Enum(Enum),
    Field(Field),
    Method(Method),
    TypeDefinition(TypeDefinition),
}

impl fmt::Display for ClassMember {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Class(data) => write!(f, "{}", data.borrow()),
            Self::Enum(data) => write!(f, "{}", data),
            Self::Field(data) => write!(f, "{}", data),
            Self::Method(data) => write!(f, "{}", data),
            Self::TypeDefinition(data) => write!(f, "{}", data),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Field {
    pub type_name: String,
    pub name: String,
    pub signature: String,
    pub offset: Option<u64>,
    pub size: usize,
    pub bitfield_info: Option<(u8, u8)>,
    pub attributes: pdb2::FieldAttributes
}

impl fmt::Display for Field {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.attributes.is_static() {
            write!(f, "static ")?;
        }

        if self.attributes.is_virtual() || self.attributes.is_intro() || self.attributes.is_pure_virtual() || self.attributes.is_pure_intro() {
            write!(f, "virtual ")?;
        }

        write!(f, "{}", self.signature)?;

        if let Some((_, bitfield_length)) = self.bitfield_info.as_ref() {
            write!(f, " : {}", bitfield_length)?;
        }

        if self.attributes.is_pure_virtual() || self.attributes.is_pure_intro() {
            write!(f, " = 0")?;
        }
        
        write!(f, ";")?;

        if let Some(offset) = self.offset {
            write!(f, " // 0x{:X}", offset)?;
        }

        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Method {
    pub is_inline: bool,
    pub declspecs: Vec<String>,
    pub signature: String,
    pub name: String,
    pub type_index: pdb2::TypeIndex,
    pub field_attributes: Option<pdb2::FieldAttributes>,
    pub function_attributes: pdb2::FunctionAttributes,
    pub modifier: Option<pdb2::ModifierType>,
}

impl fmt::Display for Method {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}{}{}{}{};{}",

            match self.field_attributes {
                Some(field_attributes) => {
                    if field_attributes.is_virtual() || field_attributes.is_intro() || field_attributes.is_pure_virtual() || field_attributes.is_pure_intro() {
                        "virtual "
                    } else if field_attributes.is_static() {
                        "static "
                    } else if field_attributes.is_friend() {
                        "friend "
                    } else {
                        ""
                    }
                },

                None => ""
            },

            if self.is_inline {
                "inline "
            } else {
                ""
            },

            if !self.declspecs.is_empty() {
                format!("__declspec({}) ", self.declspecs.join(", "))
            } else {
                String::new()
            },

            self.signature,

            match self.field_attributes {
                Some(field_attributes) => if field_attributes.is_pure_virtual() || field_attributes.is_pure_intro() {
                    if field_attributes.sealed() {
                        " final = 0"
                    } else {
                        " = 0"
                    }
                } else if field_attributes.is_virtual() {
                    if field_attributes.sealed() {
                        " override final"
                    } else {
                        " override"
                    }
                } else {
                    ""
                },

                None => ""
            },

            if self.field_attributes.as_ref().map(|a| a.is_pseudo() || a.is_compgenx()).unwrap_or(false) {
                " /* compiler_generated() */"
            } else {
                ""
            },
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Class {
    pub kind: Option<pdb2::ClassKind>,
    pub is_union: bool,
    pub is_declaration: bool,
    pub name: String,
    pub index: pdb2::TypeIndex,
    pub depth: u32,
    pub line: u32,
    pub size: u64,
    pub base_classes: Vec<BaseClass>,
    pub members: Vec<ClassMember>,
    pub properties: Option<pdb2::TypeProperties>,
    pub field_attributes: Option<pdb2::FieldAttributes>,
}

fn find_unnamed_unions_in_struct(fields: &[&Field]) -> Vec<Range<usize>> {
    let mut unions_found: Vec<Range<usize>> = vec![];
    // Temporary map of unions and fields that'll be used to compute the list
    // of unnamed unions which are in the struct.
    let mut unions_found_temp: BTreeMap<(u64, u8), (Range<usize>, u64)> = BTreeMap::new();

    // Discover unions
    let mut curr_union_offset_range: Range<u64> = Range::default();
    for (i, field) in fields.iter().enumerate() {
        let Some(field_offset) = field.offset else{
            continue;
        };
        
        // Check if the field is located inside of the union we're processing
        if curr_union_offset_range.contains(&field_offset) {
            // Third step of the "state machine", add new fields to the union.
            let union_info = unions_found_temp
                .get_mut(&(curr_union_offset_range.start, 0))
                .expect("key should exist in map");
            union_info.0.end = i + 1;
            // Update the union's size
            union_info.1 = std::cmp::max(
                union_info.1,
                field_offset - curr_union_offset_range.start + field.size as u64,
            );
            curr_union_offset_range.end = std::cmp::max(
                curr_union_offset_range.end,
                field_offset + field.size as u64,
            );
            // (Re)visit previous fields to compute the union's size
            // as well as the current union's end
            for previous_field in &fields[union_info.0.clone()] {
                let Some(previous_field_offset) = previous_field.offset else {
                    continue;
                };

                // Update the union's size
                if previous_field_offset > field_offset {
                    union_info.1 = std::cmp::max(
                        union_info.1,
                        previous_field_offset - field_offset + previous_field.size as u64,
                    );
                } else {
                    union_info.1 = std::cmp::max(union_info.1, previous_field.size as u64);
                }
                curr_union_offset_range.end = std::cmp::max(
                    curr_union_offset_range.end,
                    previous_field_offset + previous_field.size as u64,
                );
            }
        } else {
            match unions_found_temp
                .get_mut(&(field_offset, field.bitfield_info.unwrap_or_default().0))
            {
                Some(union_info) => {
                    // Second step of the "state machine", two fields share the
                    // same offset (taking bitfields into account). This becomes
                    // a union (the current one).
                    union_info.0.end = i + 1;
                    curr_union_offset_range.start = field_offset;
                    // (Re)visit previous fields to compute the union's size
                    // as well as the current union's end
                    for previous_field in &fields[union_info.0.clone()] {
                        let Some(previous_field_offset) = previous_field.offset else {
                            continue;
                        };

                        // Update the union's size
                        if previous_field_offset > field_offset {
                            union_info.1 = std::cmp::max(
                                union_info.1,
                                previous_field_offset - field_offset + previous_field.size as u64,
                            );
                        } else {
                            union_info.1 = std::cmp::max(union_info.1, previous_field.size as u64);
                        }
                        curr_union_offset_range.end = std::cmp::max(
                            curr_union_offset_range.end,
                            previous_field_offset + previous_field.size as u64,
                        );
                    }
                }
                None => {
                    // First step of the "state machine".
                    // Each field is a potential new union
                    unions_found_temp.insert(
                        (field_offset, field.bitfield_info.unwrap_or_default().0),
                        (Range { start: i, end: i }, field.size as u64),
                    );
                }
            }
        }
    }

    // Remove nested unions, they will be processed when we'll go deeper
    for (offset1, range1) in unions_found_temp.iter() {
        let mut is_top_level = true;
        for (offset2, range2) in unions_found_temp.iter() {
            if offset1 == offset2 {
                // Comparing a union with itself, ignore
                continue;
            }
            if range1.0.start >= range2.0.start && range1.0.end < range2.0.end {
                // Union #1 is contained by Union #2, no need to continue.
                // Union #1 isn't a "top-level" union.
                is_top_level = false;
                break;
            }
        }
        if is_top_level {
            // Only keep "top-level" union
            unions_found.push(range1.0.clone());
        }
    }
    
    unions_found
}

fn reconstruct_struct_fields(depth: u32, fields: &[&Field]) -> Vec<ClassMember> {
    let unions_found = find_unnamed_unions_in_struct(fields);
    
    let mut new_fields = vec![];

    for union_range in unions_found {
        if union_range.is_empty() {
            let field = &fields[union_range.start];
            new_fields.push(ClassMember::Field((*field).clone()));
        } else {
            let fields = reconstruct_union_fields(depth + 1, &fields[union_range]);

            let fields_size: usize = fields.iter().map(|m| {
                let ClassMember::Field(f) = m else {
                    return 0;
                };

                if f.offset.is_none() {
                    0
                } else {
                    f.size
                }
            }).sum();

            new_fields.push(ClassMember::Class(Rc::new(RefCell::new(Class {
                kind: None,
                is_union: true,
                is_declaration: false,
                name: String::new(),
                index: pdb2::TypeIndex(0),
                depth: depth + 1,
                line: 0,
                size: fields_size as u64,
                base_classes: vec![],
                members: fields,
                properties: None,
                field_attributes: None,
            }))));
        }
    }

    new_fields
}

fn find_unnamed_structs_in_unions(fields: &[&Field]) -> Vec<Range<usize>> {
    let mut structs_found: Vec<Range<usize>> = vec![];

    let field_count = fields.len();

    let Some(union_offset) = fields.iter().find(|f| f.offset.is_some()).map(|f| f.offset).flatten() else {
        return vec![];
    };

    let mut previous_field_offset = union_offset;
    let mut previous_field_bit_offset = fields[0].bitfield_info.unwrap_or_default().0;

    for (i, field) in fields.iter().enumerate() {
        let Some(field_offset) = field.offset else {
            continue;
        };

        // The field offset is lower than the offset of the previous field
        // -> "close" the struct
        if previous_field_offset > field_offset
            || (field_offset == previous_field_offset
                && previous_field_bit_offset > field.bitfield_info.unwrap_or_default().0)
        {
            if let Some(last_found_struct_range) = structs_found.pop() {
                // "Merge" previous field with the struct
                structs_found.push(Range {
                    start: last_found_struct_range.start,
                    end: i,
                });
            }
        }
        // Last element, check if we need to "close" a struct
        else if i == field_count - 1 {
            if let Some(last_found_struct_range) = structs_found.pop() {
                // Declare a new struct only if its length is greater than 1.
                if i > last_found_struct_range.start
                    && (field_offset != previous_field_offset
                        || field.bitfield_info.unwrap_or_default().0 != previous_field_bit_offset)
                {
                    // "Merge" previous field with the struct
                    structs_found.push(Range {
                        start: last_found_struct_range.start,
                        end: i + 1,
                    });
                } else {
                    structs_found.push(last_found_struct_range);
                }
            }
        }

        // Regular field, may be the beginning of a struct
        if field_offset == union_offset && field.bitfield_info.unwrap_or_default().0 == 0 {
            structs_found.push(Range { start: i, end: i });
        }

        previous_field_offset = field_offset;
        previous_field_bit_offset = field.bitfield_info.unwrap_or_default().0;
    }

    structs_found
}

fn reconstruct_union_fields(depth: u32, fields: &[&Field]) -> Vec<ClassMember> {
    let mut new_fields = vec![];

    let structs_found = find_unnamed_structs_in_unions(fields);

    for struct_range in structs_found {
        // Fields out of unnamed structs are represented by "empty" structs
        if struct_range.is_empty() {
            let field = fields[struct_range.start];
            new_fields.push(ClassMember::Field(field.clone()));
        } else {
            let fields = reconstruct_struct_fields(depth + 1, &fields[struct_range]);

            let fields_size: usize = fields.iter().map(|m| {
                let ClassMember::Field(f) = m else {
                    return 0;
                };

                if f.offset.is_none() {
                    0
                } else {
                    f.size
                }
            }).sum();

            new_fields.push(ClassMember::Class(Rc::new(RefCell::new(Class {
                kind: Some(pdb2::ClassKind::Struct),
                is_union: false,
                is_declaration: false,
                name: String::new(),
                index: pdb2::TypeIndex(0),
                depth: depth + 1,
                line: 0,
                size: fields_size as u64,
                base_classes: vec![],
                members: fields,
                properties: None,
                field_attributes: None,
            }))));
        }
    }

    new_fields
}

impl Class {
    pub fn add_derived_from(
        &mut self,
        _: &pdb2::TypeFinder,
        _: pdb2::TypeIndex
    ) -> pdb2::Result<()> {
        // TODO
        Ok(())
    }

    pub fn add_members(
        &mut self,
        class_table: &mut Vec<Rc<RefCell<Class>>>,
        type_sizes: &mut HashMap<String, u64>,
        machine_type: pdb2::MachineType,
        type_info: &pdb2::TypeInformation,
        type_finder: &pdb2::TypeFinder,
        type_index: pdb2::TypeIndex,
    ) -> pdb2::Result<()> {
        match type_finder.find(type_index)?.parse() {
            Ok(pdb2::TypeData::FieldList(data)) => {
                for field in &data.fields {
                    self.add_member(class_table, type_sizes, machine_type, type_info, type_finder, field)?;
                }

                if let Some(continuation) = data.continuation {
                    self.add_members(class_table, type_sizes, machine_type, type_info, type_finder, continuation)?;
                }

                let fields = self.members.iter()
                    .filter(|m| matches!(m, ClassMember::Field(_)))
                    .map(|m| {
                        let ClassMember::Field(f) = m else { unreachable!() };
                        f
                    }).collect::<Vec<_>>();

                let new_fields = if self.is_union {
                    reconstruct_union_fields(self.depth, fields.as_slice())
                } else {
                    reconstruct_struct_fields(self.depth, fields.as_slice())
                };

                let mut remove_indices = vec![];

                for (i, member) in self.members.iter().enumerate() {
                    if let ClassMember::Field(Field { offset: Some(_), .. }) = member {
                        remove_indices.push(i);
                    }
                }

                let insert_index = remove_indices.iter().min().cloned().unwrap_or(self.members.len() - new_fields.len());

                for i in remove_indices.into_iter().rev() {
                    self.members.remove(i);
                }

                for member in new_fields.into_iter().rev() {
                    self.members.insert(insert_index, member);
                }
            }

            Ok(pdb2::TypeData::Primitive(pdb2::PrimitiveType { kind: pdb2::PrimitiveKind::NoType, indirection: None })) => {
                self.is_declaration = true;
            }

            Ok(other) => panic!("Unexpected type in Class::add_members, got {} -> {:?}", type_index, other),

            Err(e) => {
                println!("WARNING: failed to find type in Class::add_members, skipping: {e}");
            }
        }

        Ok(())
    }

    fn add_member(
        &mut self,
        class_table: &mut Vec<Rc<RefCell<Class>>>,
        type_sizes: &mut HashMap<String, u64>,
        machine_type: pdb2::MachineType,
        type_info: &pdb2::TypeInformation,
        type_finder: &pdb2::TypeFinder,
        field: &pdb2::TypeData
    ) -> pdb2::Result<()> {
        match *field {
            pdb2::TypeData::Member(ref data) => {
                let bitfield_info = match type_finder.find(data.field_type)?.parse()? {
                    pdb2::TypeData::Bitfield(data) => Some((data.position, data.length)),
                    _ => None,
                };
                
                self.members.push(ClassMember::Field(Field {
                    type_name: type_name(
                        class_table,
                        type_sizes,
                        machine_type,
                        type_info,
                        type_finder,
                        data.field_type,
                        None,
                        None,
                        None,
                        None,
                        false
                    )?,
                    name: data.name.to_string().to_string(),
                    signature: type_name(
                        class_table,
                        type_sizes,
                        machine_type,
                        type_info,
                        type_finder,
                        data.field_type,
                        None,
                        Some(data.name.to_string().to_string()),
                        None,
                        None,
                        false
                    )?,
                    offset: Some(data.offset),
                    size: type_size(class_table, type_sizes, machine_type, type_info, type_finder, data.field_type)?,
                    bitfield_info,
                    attributes: data.attributes
                }));
            }

            pdb2::TypeData::StaticMember(ref data) => {
                let bitfield_info = match type_finder.find(data.field_type)?.parse()? {
                    pdb2::TypeData::Bitfield(data) => Some((data.position, data.length)),
                    _ => None,
                };

                self.members.push(ClassMember::Field(Field {
                    type_name: format!(
                        "static {}",
                        type_name(
                            class_table,
                            type_sizes,
                            machine_type,
                            type_info,
                            type_finder,
                            data.field_type,
                            None,
                            None,
                            None,
                            None,
                            false
                        )?
                    ),
                    name: data.name.to_string().to_string(),
                    signature: format!(
                        "static {}",
                        type_name(
                            class_table,
                            type_sizes,
                            machine_type,
                            type_info,
                            type_finder,
                            data.field_type,
                            None,
                            Some(data.name.to_string().to_string()),
                            None,
                            None,
                            false
                        )?
                    ),
                    offset: None,
                    size: type_size(class_table, type_sizes, machine_type, type_info, type_finder, data.field_type)?,
                    bitfield_info,
                    attributes: data.attributes
                }));
            }

            pdb2::TypeData::BaseClass(ref data) => {
                self.base_classes.push(BaseClass {
                    type_name: format!(
                        "{}{}",
                        match data.attributes.access() {
                            1 => "private ",
                            2 => "protected ",
                            3 => "public ",
                            _ => "",
                        },
                        type_name(class_table, type_sizes, machine_type, type_info, type_finder, data.base_class, None, None, None, None, false)?
                    ),
                    offset: data.offset,
                    index: data.base_class
                });
            }

            pdb2::TypeData::VirtualBaseClass(ref data) => {
                self.base_classes.push(BaseClass {
                    type_name: format!(
                        "{}virtual {}",
                        match data.attributes.access() {
                            1 => "private ",
                            2 => "protected ",
                            3 => "public ",
                            _ => ""
                        },
                        type_name(class_table, type_sizes, machine_type, type_info, type_finder, data.base_class, None, None, None, None, false)?
                    ),
                    offset: data.base_pointer_offset,
                    index: data.base_class
                });
            }

            pdb2::TypeData::VirtualFunctionTablePointer(_data) => {
                // panic!("{data:#?}")
                // TODO: does this need handling?
            }

            pdb2::TypeData::Method(ref data) => match data.name.to_string().to_string().as_str() {
                // Ignore compiler-generated functions:
                "__vecDelDtor" | "__local_vftable_ctor_closure" | "__autoclassinit" => (),
                
                _ => {
                    let method = match type_finder.find(data.method_type)?.parse() {
                        Ok(pdb2::TypeData::MemberFunction(function_data)) => {
                            let modifier = get_member_function_modifier(&function_data, type_finder);

                            let signature = type_name(
                                class_table,
                                type_sizes,
                                machine_type,
                                type_info,
                                type_finder,
                                data.method_type,
                                modifier.as_ref(),
                                Some(data.name.to_string().to_string()),
                                None,
                                None,
                                false
                            )?;

                            Method {
                                is_inline: false,
                                declspecs: vec![],
                                signature,
                                name: data.name.to_string().to_string(),
                                type_index: data.method_type,
                                field_attributes: Some(data.attributes),
                                function_attributes: function_data.attributes,
                                modifier,
                            }
                        }
            
                        Ok(data) => panic!("Unhandled member function type data in Class::add_member - {:#?}", data),
                        Err(err) => panic!("Unhandled error in Class::add_member - {}", err)
                    };

                    self.members.push(ClassMember::Method(method));
                }
            }

            pdb2::TypeData::OverloadedMethod(ref data) => {
                match type_finder.find(data.method_list)?.parse() {
                    Ok(pdb2::TypeData::MethodList(method_list)) => {
                        for pdb2::MethodListEntry {
                            attributes,
                            method_type,
                            ..
                        } in method_list.methods {
                            match data.name.to_string().to_string().as_str() {
                                "__vecDelDtor" | "__local_vftable_ctor_closure" | "__autoclassinit" => (),

                                _ => {
                                    let method = match type_finder.find(method_type)?.parse() {
                                        Ok(pdb2::TypeData::MemberFunction(function_data)) => {
                                            let modifier = get_member_function_modifier(&function_data, type_finder);

                                            let signature = type_name(
                                                class_table,
                                                type_sizes,
                                                machine_type,
                                                type_info,
                                                type_finder,
                                                method_type,
                                                modifier.as_ref(),
                                                Some(data.name.to_string().to_string()),
                                                None,
                                                None,
                                                false
                                            )?;
                                            
                                            Method {
                                                is_inline: false,
                                                declspecs: vec![],
                                                signature,
                                                name: data.name.to_string().to_string(),
                                                type_index: method_type,
                                                field_attributes: Some(attributes),
                                                function_attributes: function_data.attributes,
                                                modifier,
                                            }
                                        }
                            
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

            pdb2::TypeData::Nested(ref nested_data) => {
                let nested_type_item = type_finder.find(nested_data.nested_type)?;
                let nested_type_data = nested_type_item.parse()?;
                
                match &nested_type_data {
                    data if matches!(data, pdb2::TypeData::Class(_) | pdb2::TypeData::Union(_)) => {
                        let (name, size, properties, fields, derived_from) = match &data {
                            pdb2::TypeData::Class(class_data) => {
                                (class_data.name, class_data.size, class_data.properties, class_data.fields, class_data.derived_from)
                            }

                            pdb2::TypeData::Union(union_data) => {
                                (union_data.name, union_data.size, union_data.properties, Some(union_data.fields), None)
                            }

                            _ => unreachable!(),
                        };

                        // If a nested class is a forward reference that doesn't have the nested type property,
                        // and the name is different than the name in the nested type data, then it is a typedef.
                        if properties.forward_reference() && !properties.is_nested_type() && nested_data.name != name {

                            let type_name = self::type_name(
                                class_table,
                                type_sizes,
                                machine_type,
                                type_info,
                                type_finder,
                                nested_data.nested_type,
                                None,
                                Some(nested_data.name.to_string().to_string()),
                                None,
                                None,
                                false
                            )?;

                            self.members.push(ClassMember::TypeDefinition(TypeDefinition {
                                type_name,
                                underlying_type: nested_data.nested_type,
                                field_attributes: Some(nested_data.attributes),
                                pointer_attributes: None,
                                containing_class: None,
                            }));

                            return Ok(());
                        }

                        let definition = Rc::new(RefCell::new(Class {
                            kind: match &data {
                                pdb2::TypeData::Class(data) => Some(data.kind),
                                _ => None,
                            },
                            is_union: matches!(data, pdb2::TypeData::Union(_)),
                            is_declaration: false,
                            name: nested_data.name.to_string().to_string(),
                            index: nested_data.nested_type,
                            depth: self.depth + 1,
                            line: 0,
                            size: size,
                            base_classes: vec![],
                            members: vec![],
                            properties: Some(properties),
                            field_attributes: Some(nested_data.attributes),
                        }));

                        if !class_table.iter().any(|c| c.borrow().index == nested_data.nested_type) {
                            class_table.push(definition.clone());
                        }
        
                        if let Some(derived_from) = derived_from {
                            definition.borrow_mut().add_derived_from(type_finder, derived_from)?;
                        }
        
                        if properties.forward_reference() {
                            definition.borrow_mut().is_declaration = true;
                        } else if let Some(fields) = fields {
                            let mut temp = definition.borrow().clone();
                            temp.add_members(class_table, type_sizes, machine_type, type_info, type_finder, fields)?;
                            *definition.borrow_mut() = temp;
                        }
                        
                        let mut exists = false;
                        
                        for member in self.members.iter() {
                            if let ClassMember::Class(other_definition) = member
                                && definition.borrow().kind == other_definition.borrow().kind
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
                            self.members.push(ClassMember::Class(definition));
                        }
                    }
        
                    pdb2::TypeData::Enumeration(data) => {
                        let mut definition = Enum {
                            name: nested_data.name.to_string().to_string(),
                            index: nested_data.nested_type,
                            depth: self.depth + 1,
                            line: 0,
                            underlying_type_name: type_name(class_table, type_sizes, machine_type, type_info, type_finder, data.underlying_type, None, None, None, None, false)?,
                            size: type_size(class_table, type_sizes, machine_type, type_info, type_finder, data.underlying_type)?,
                            is_declaration: false,
                            values: vec![],
                            properties: data.properties,
                            field_attributes: Some(nested_data.attributes),
                        };
        
                        if data.properties.forward_reference() {
                            definition.is_declaration = true;
                        } else {
                            definition.add_members(type_finder, data.fields)?;
                        }

                        let mut exists = false;
        
                        for member in self.members.iter() {
                            if let ClassMember::Enum(other_definition) = member
                                && definition.name == other_definition.name
                                && definition.values.eq(&other_definition.values)
                            {
                                exists = true;
                                break;
                            }
                        }
        
                        if !exists {
                            self.members.push(ClassMember::Enum(definition));
                        }
                    }
        
                    pdb2::TypeData::Pointer(_)
                    | pdb2::TypeData::Modifier(_)
                    | pdb2::TypeData::Primitive(_)
                    | pdb2::TypeData::Array(_)
                    | pdb2::TypeData::Procedure(_) => {
                        let type_name = self::type_name(
                            class_table,
                            type_sizes,
                            machine_type,
                            type_info,
                            type_finder,
                            nested_data.nested_type,
                            None,
                            Some(nested_data.name.to_string().to_string()),
                            None,
                            None,
                            false
                        )?;

                        self.members.push(ClassMember::TypeDefinition(TypeDefinition {
                            type_name,
                            underlying_type: nested_data.nested_type,
                            field_attributes: Some(nested_data.attributes),
                            pointer_attributes: None,
                            containing_class: None,
                        }));
                    }

                    _ => return Err(
                        pdb2::Error::IoError(
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
            "{}{}{}{}",
            
            if self.field_attributes.as_ref().map(|a| a.is_friend()).unwrap_or(false) {
                "friend "
            } else {
                ""
            },

            match self.kind {
                Some(pdb2::ClassKind::Class) => "class",
                Some(pdb2::ClassKind::Struct) => "struct",
                Some(pdb2::ClassKind::Interface) => "interface",
                None => {
                    assert!(self.is_union);
                    "union"
                }
            },

            if self.name.is_empty() {
                String::new()
            } else {
                format!(" {}", self.name)
            },

            if self.field_attributes.as_ref().map(|a| a.sealed() || a.noinherit()).unwrap_or(false) {
                " final"
            } else {
                ""
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
                    write!(f, "    ")?;
                }
                
                write!(f, "    {}{}", base.type_name, if i == last_base_class_index { "" } else { "," })?;
            }
        }

        if self.is_declaration {
            write!(f, ";")?;
            return Ok(())
        }
        
        writeln!(f)?;

        for _ in 0..self.depth {
            write!(f, "    ")?;
        }

        writeln!(f, "{{")?;

        let mut prev_access: Option<&str> = match self.kind.as_ref() {
            Some(pdb2::ClassKind::Struct) => Some("public"),
            _ => {
                if self.is_union {
                    Some("public")
                } else {
                    None
                }
            }
        };

        for member in self.members.iter() {
            let field_attributes = match member {
                ClassMember::Class(data) => data.borrow().field_attributes,
                ClassMember::Enum(data) => data.field_attributes,
                ClassMember::Field(data) => Some(data.attributes),
                ClassMember::Method(data) => data.field_attributes,
                ClassMember::TypeDefinition(data) => data.field_attributes,
            };

            let member_access = match field_attributes.map(|a| a.access()) {
                Some(1) => Some("private"),
                Some(2) => Some("protected"),
                Some(3) => Some("public"),
                _ => prev_access
            };

            if member_access.is_none() && prev_access.is_none() {
                writeln!(f, "public:")?;
                prev_access = Some("public");
            }
            else if member_access != prev_access {
                if let Some(member_access) = member_access {
                    for _ in 0..self.depth {
                        write!(f, "    ")?;
                    }
            
                    writeln!(f, "{}:", member_access)?;
                }

                prev_access = member_access;
            }

            for _ in 0..self.depth {
                write!(f, "    ")?;
            }
    
            writeln!(f, "    {}", member)?;
        }

        for _ in 0..self.depth {
            write!(f, "    ")?;
        }

        if !self.name.is_empty() && self.size != 0 {
            writeln!(f, "}};")?;

            for _ in 0..self.depth {
                write!(f, "    ")?;
            }

            write!(f, "static_assert(sizeof({}) == {}, \"Invalid {} size\");", self.name, self.size, self.name)?;
        } else {
            write!(f, "}};")?;
        }

        Ok(())
    }
}
