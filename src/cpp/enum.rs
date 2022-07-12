use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumValue {
    pub name: String,
    pub value: pdb::Variant,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Enum {
    pub name: String,
    pub index: pdb::TypeIndex,
    pub depth: u32,
    pub line: u32,
    pub underlying_type_name: String,
    pub is_declaration: bool,
    pub values: Vec<EnumValue>,
    pub field_attributes: Option<pdb::FieldAttributes>,
}

impl Enum {
    pub fn add_members(
        &mut self,
        type_finder: &pdb::TypeFinder,
        type_index: pdb::TypeIndex
    ) -> pdb::Result<()> {
        match type_finder.find(type_index)?.parse() {
            Ok(pdb::TypeData::FieldList(data)) => {
                for field in &data.fields {
                    match field {
                        pdb::TypeData::Enumerate(ref data) => {
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

            Ok(pdb::TypeData::Primitive(pdb::PrimitiveType { kind: pdb::PrimitiveKind::NoType, indirection: None })) => {
                self.is_declaration = true;
            }

            Ok(data) => panic!("Unexpected type data in Enum::add_members - {:#?}", data),

            Err(err) => println!("WARNING: unhandled error in Enum::add_members, skipping: {err}")
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
                    pdb::Variant::U8(v) => format!("{}", v),
                    pdb::Variant::U16(v) => format!("{}", v),
                    pdb::Variant::U32(v) => format!("{}", v),
                    pdb::Variant::U64(v) => format!("{}", v),
                    pdb::Variant::I8(v) => format!("{}", v),
                    pdb::Variant::I16(v) => format!("{}", v),
                    pdb::Variant::I32(v) => format!("{}", v),
                    pdb::Variant::I64(v) => format!("{}", v),
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
