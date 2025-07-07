use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumValue {
    pub name: String,
    pub value: pdb2::Variant,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Enum {
    pub name: String,
    pub index: pdb2::TypeIndex,
    pub depth: u32,
    pub line: u32,
    pub underlying_type_name: String,
    pub is_declaration: bool,
    pub values: Vec<EnumValue>,
    pub field_attributes: Option<pdb2::FieldAttributes>,
}

impl Enum {
    pub fn add_members(
        &mut self,
        type_finder: &pdb2::TypeFinder,
        type_index: pdb2::TypeIndex
    ) -> pdb2::Result<()> {
        match type_finder.find(type_index)?.parse() {
            Ok(pdb2::TypeData::FieldList(data)) => {
                for field in &data.fields {
                    match field {
                        pdb2::TypeData::Enumerate(data) => {
                            self.values.push(EnumValue {
                                name: data.name.to_string().to_string(),
                                value: data.value,
                            });
                        }
            
                        ref data => panic!("Unhandled enum type data in Enum::add_members - {:#?}", data)
                    }
                }

                if let Some(continuation) = data.continuation {
                    self.add_members(type_finder, continuation)?;
                }
            }

            Ok(pdb2::TypeData::Primitive(pdb2::PrimitiveType { kind: pdb2::PrimitiveKind::NoType, indirection: None })) => {
                self.is_declaration = true;
            }

            Ok(data) => panic!("Unexpected type data in Enum::add_members - {:#?}", data),

            Err(_) => {
                // println!("WARNING: unhandled error in Enum::add_members, skipping: {err}");
            }
        }

        Ok(())
    }
}

impl fmt::Display for Enum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "enum")?;

        let name = self.name.to_string();

        if name != "<unnamed-tag>" {
            write!(f, " {}", name)?;
        }

        if self.underlying_type_name != "long" {
            write!(f, " : {}", self.underlying_type_name)?;
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

        for value in &self.values {
            for _ in 0..self.depth {
                write!(f, "\t")?;
            }
    
            writeln!(
                f,
                "\t{} = {},",
                value.name,
                match value.value {
                    pdb2::Variant::U8(v) => format!("{}", v),
                    pdb2::Variant::U16(v) => format!("{}", v),
                    pdb2::Variant::U32(v) => format!("{}", v),
                    pdb2::Variant::U64(v) => format!("{}", v),
                    pdb2::Variant::I8(v) => format!("{}", v),
                    pdb2::Variant::I16(v) => format!("{}", v),
                    pdb2::Variant::I32(v) => format!("{}", v),
                    pdb2::Variant::I64(v) => format!("{}", v),
                }
            )?;
        }

        for _ in 0..self.depth {
            write!(f, "\t")?;
        }

        write!(f, "}};")?;

        Ok(())
    }
}
