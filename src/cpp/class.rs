use super::*;
use std::{cell::RefCell, collections::BTreeMap, fmt, ops::Range, rc::Rc};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BaseClass {
    pub type_name: String,
    pub index: pdb2::TypeIndex,
    pub offset: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Field {
    pub type_name: String,
    pub name: String,
    pub display: String,
    pub offset: u64,
    pub size: usize,
    pub bitfield_info: Option<(u8, u8)>,
    pub attributes: pdb2::FieldAttributes
}

impl fmt::Display for Field {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.attributes.is_static() {
            write!(f, "static ")?;
        }

        if self.attributes.is_virtual() {
            write!(f, "virtual ")?;
        }

        write!(f, "{}", self.display)?;

        if let Some((_, bitfield_length)) = self.bitfield_info.as_ref() {
            write!(f, " : {}", bitfield_length)?;
        }

        if self.attributes.is_pure_virtual() {
            write!(f, " = 0")?;
        }

        write!(f, ";")?;
        write!(f, " // 0x{:X}", self.offset)?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Method {
    pub name: String,
    pub type_index: pdb2::TypeIndex,
    pub return_type_name: String,
    pub arguments: Vec<String>,
    pub field_attributes: Option<pdb2::FieldAttributes>,
    pub function_attributes: pdb2::FunctionAttributes,
    pub modifier: Option<pdb2::ModifierType>,
}

impl fmt::Display for Method {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}{}{}({}){}{};",

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

            match self.modifier {
                Some(modifier) => {
                    let mut modifier_string = String::new();

                    if modifier.constant {
                        modifier_string.push_str(" const");
                    }

                    if modifier.volatile {
                        modifier_string.push_str(" volatile");
                    }

                    modifier_string
                }

                None => String::new()
            },

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
        if field.offset == std::u64::MAX {
            continue;
        }
        
        // Check if the field is located inside of the union we're processing
        if curr_union_offset_range.contains(&field.offset) {
            // Third step of the "state machine", add new fields to the union.
            let union_info = unions_found_temp
                .get_mut(&(curr_union_offset_range.start, 0))
                .expect("key should exist in map");
            union_info.0.end = i + 1;
            // Update the union's size
            union_info.1 = std::cmp::max(
                union_info.1,
                field.offset - curr_union_offset_range.start + field.size as u64,
            );
            curr_union_offset_range.end = std::cmp::max(
                curr_union_offset_range.end,
                field.offset + field.size as u64,
            );
            // (Re)visit previous fields to compute the union's size
            // as well as the current union's end
            for previous_field in &fields[union_info.0.clone()] {
                if previous_field.offset == std::u64::MAX {
                    continue;
                }

                // Update the union's size
                if previous_field.offset > field.offset {
                    union_info.1 = std::cmp::max(
                        union_info.1,
                        previous_field.offset - field.offset + previous_field.size as u64,
                    );
                } else {
                    union_info.1 = std::cmp::max(union_info.1, previous_field.size as u64);
                }
                curr_union_offset_range.end = std::cmp::max(
                    curr_union_offset_range.end,
                    previous_field.offset + previous_field.size as u64,
                );
            }
        } else {
            match unions_found_temp
                .get_mut(&(field.offset, field.bitfield_info.unwrap_or_default().0))
            {
                Some(union_info) => {
                    // Second step of the "state machine", two fields share the
                    // same offset (taking bitfields into account). This becomes
                    // a union (the current one).
                    union_info.0.end = i + 1;
                    curr_union_offset_range.start = field.offset;
                    // (Re)visit previous fields to compute the union's size
                    // as well as the current union's end
                    for previous_field in &fields[union_info.0.clone()] {
                        if previous_field.offset == std::u64::MAX {
                            continue;
                        }

                        // Update the union's size
                        if previous_field.offset > field.offset {
                            union_info.1 = std::cmp::max(
                                union_info.1,
                                previous_field.offset - field.offset + previous_field.size as u64,
                            );
                        } else {
                            union_info.1 = std::cmp::max(union_info.1, previous_field.size as u64);
                        }
                        curr_union_offset_range.end = std::cmp::max(
                            curr_union_offset_range.end,
                            previous_field.offset + previous_field.size as u64,
                        );
                    }
                }
                None => {
                    // First step of the "state machine".
                    // Each field is a potential new union
                    unions_found_temp.insert(
                        (field.offset, field.bitfield_info.unwrap_or_default().0),
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
                let ClassMember::Field(f) = m else { return 0 };
                if f.offset == std::u64::MAX {
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
                field_attributes: None,
            }))));
        }
    }

    new_fields
}

fn find_unnamed_structs_in_unions(fields: &[&Field]) -> Vec<Range<usize>> {
    let mut structs_found: Vec<Range<usize>> = vec![];

    let field_count = fields.len();
    let union_offset = fields[0].offset;
    let mut previous_field_offset = fields[0].offset;
    let mut previous_field_bit_offset = fields[0].bitfield_info.unwrap_or_default().0;

    for (i, field) in fields.iter().enumerate() {
        if field.offset == std::u64::MAX {
            continue;
        }

        // The field offset is lower than the offset of the previous field
        // -> "close" the struct
        if previous_field_offset > field.offset
            || (field.offset == previous_field_offset
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
                    && (field.offset != previous_field_offset
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
        if field.offset == union_offset && field.bitfield_info.unwrap_or_default().0 == 0 {
            structs_found.push(Range { start: i, end: i });
        }

        previous_field_offset = field.offset;
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
                let ClassMember::Field(f) = m else { return 0 };
                if f.offset == std::u64::MAX {
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
                    if let ClassMember::Field(Field { offset, .. }) = member
                        && *offset != std::u64::MAX
                    {
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

            Err(_) => {
                // println!("WARNING: failed to find type in Class::add_members, skipping: {err}");
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
                    type_name: type_name(class_table, type_sizes, 
                        machine_type,
                        type_info,
                        type_finder,
                        data.field_type,
                        None,
                        None,
                        None,
                        true,
                    )?,
                    name: data.name.to_string().to_string(),
                    display: type_name(class_table, type_sizes, 
                        machine_type,
                        type_info,
                        type_finder,
                        data.field_type,
                        None,
                        Some(data.name.to_string().to_string()),
                        None,
                        true,
                    )?,
                    offset: data.offset,
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
                        type_name(class_table, type_sizes, 
                            machine_type,
                            type_info,
                            type_finder,
                            data.field_type,
                            None,
                            None,
                            None,
                            true,
                        )?
                    ),
                    name: data.name.to_string().to_string(),
                    display: format!(
                        "static {}",
                        type_name(class_table, type_sizes, 
                            machine_type,
                            type_info,
                            type_finder,
                            data.field_type,
                            None,
                            Some(data.name.to_string().to_string()),
                            None,
                            true,
                        )?
                    ),
                    offset: std::u64::MAX,
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
                            0 => "private ",
                            1 => "protected ",
                            2 => "public ",
                            _ => ""
                        },
                        type_name(class_table, type_sizes, machine_type, type_info, type_finder, data.base_class, None, None, None, true)?
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
                            0 => "private ",
                            1 => "protected ",
                            2 => "public ",
                            _ => ""
                        },
                        type_name(class_table, type_sizes, machine_type, type_info, type_finder, data.base_class, None, None, None, true)?
                    ),
                    offset: data.base_pointer_offset,
                    index: data.base_class
                });
            }

            pdb2::TypeData::VirtualFunctionTablePointer(_) => (), // TODO: does this need handling?

            pdb2::TypeData::Method(ref data) => match data.name.to_string().to_string().as_str() {
                // Ignore compiler-generated functions:
                "__vecDelDtor" | "__local_vftable_ctor_closure" | "__autoclassinit" => (),
                
                _ => {
                    let method = match type_finder.find(data.method_type)?.parse() {
                        Ok(pdb2::TypeData::MemberFunction(function_data)) => Method {
                            name: data.name.to_string().to_string(),
                            type_index: data.method_type,
                            return_type_name: type_name(class_table, type_sizes, machine_type, type_info, type_finder, function_data.return_type, None, None, None, true)?,
                            arguments: argument_list(class_table, type_sizes, machine_type, type_info, type_finder, function_data.argument_list, None)?,
                            field_attributes: Some(data.attributes),
                            function_attributes: function_data.attributes,
                            modifier: None,
                        },
            
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
                                        Ok(pdb2::TypeData::MemberFunction(function_data)) => Method {
                                            name: data.name.to_string().to_string(),
                                            type_index: method_type,
                                            return_type_name: type_name(class_table, type_sizes, machine_type, type_info, type_finder, function_data.return_type, None, None, None, true)?,
                                            arguments: argument_list(class_table, type_sizes, machine_type, type_info, type_finder, function_data.argument_list, None)?,
                                            field_attributes: Some(attributes),
                                            function_attributes: function_data.attributes,
                                            modifier: None,
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

            pdb2::TypeData::Nested(ref nested_data) => {
                let nested_type_item = type_finder.find(nested_data.nested_type)?;
                let nested_type_data = nested_type_item.parse()?;
                
                match &nested_type_data {
                    data if matches!(data, pdb2::TypeData::Class(_) | pdb2::TypeData::Union(_)) => {
                        let (size, properties, fields, derived_from) = match &data {
                            pdb2::TypeData::Class(class_data) => {
                                (class_data.size, class_data.properties, class_data.fields, class_data.derived_from)
                            }

                            pdb2::TypeData::Union(union_data) => {
                                (union_data.size, union_data.properties, Some(union_data.fields), None)
                            }

                            _ => unreachable!(),
                        };

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
                            underlying_type_name: type_name(class_table, type_sizes, machine_type, type_info, type_finder, data.underlying_type, None, None, None, true)?,
                            size: type_size(class_table, type_sizes, machine_type, type_info, type_finder, data.underlying_type)?,
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
        
                    pdb2::TypeData::Pointer(data) => {
                        let type_name = type_name(class_table, type_sizes, machine_type, type_info, type_finder, data.underlying_type, None, Some(nested_data.name.to_string().to_string()), None, true)?;

                        self.members.push(ClassMember::TypeDefinition(TypeDefinition {
                            type_name,
                            underlying_type: data.underlying_type,
                            field_attributes: Some(nested_data.attributes),
                            pointer_attributes: Some(data.attributes),
                            containing_class: data.containing_class,
                        }));
                    }

                    pdb2::TypeData::Modifier(data) => {
                        let mut type_name = self::type_name(
                            class_table,
                            type_sizes,
                            machine_type,
                            type_info,
                            type_finder,
                            data.underlying_type,
                            Some(data),
                            Some(nested_data.name.to_string().to_string()),
                            None,
                            true,
                        )?;

                        if data.constant {
                            type_name.push_str("const");
                        }

                        if data.volatile {
                            type_name.push_str("volatile ");
                        }

                        self.members.push(ClassMember::TypeDefinition(TypeDefinition {
                            type_name,
                            underlying_type: data.underlying_type,
                            field_attributes: Some(nested_data.attributes),
                            pointer_attributes: None,
                            containing_class: None,
                        }));
                    }

                    pdb2::TypeData::Primitive(data) => {
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
                            field_attributes: Some(nested_data.attributes),
                            pointer_attributes: None,
                            containing_class: None,
                        }));
                    }

                    pdb2::TypeData::Array(data) => {
                        let mut type_name = type_name(class_table, type_sizes, machine_type, type_info, type_finder, data.element_type, None, Some(nested_data.name.to_string().to_string()), None, true)?;
                        let mut element_size = type_size(class_table, type_sizes, machine_type, type_info, type_finder, data.element_type)?;
            
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
                                    && let Ok(current_type_size) = type_size(class_table, type_sizes, machine_type, type_info, type_finder, current_type_item.index())
                                    && current_type_size != 0
                                {
                                    element_size = current_type_size;
                                    break;
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
                            field_attributes: Some(nested_data.attributes),
                            pointer_attributes: None,
                            containing_class: None,
                        }));
                    }

                    pdb2::TypeData::Procedure(_) => {
                        let type_name = type_name(class_table, type_sizes, 
                            machine_type,
                            type_info,
                            type_finder,
                            nested_data.nested_type,
                            None,
                            Some(nested_data.name.to_string().to_string()),
                            None,
                            true,
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
            "{}{}",
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

            if member_access != prev_access {
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
