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
    pub size: usize,
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

        if !self.name.split("::").last().unwrap().contains("<unnamed") {
            write!(f, " {}", self.name)?;
        }

        if self.underlying_type_name != "int"
            && self.underlying_type_name != "long"
            && self.underlying_type_name != "__int32"
        {
            write!(f, " : {}", self.underlying_type_name)?;
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

        let is_signed = !self.underlying_type_name.starts_with("unsigned ");

        for value in &self.values {
            for _ in 0..self.depth {
                write!(f, "    ")?;
            }
    
            writeln!(
                f,
                "    {} = {},",
                value.name,
                match value.value {
                    pdb2::Variant::U8(value) => {
                        if is_signed {
                            format!("{}", unsafe { *((&raw const value) as *const i8) })
                        } else {
                            format!("{}", value)
                        }
                    }
                    
                    pdb2::Variant::U16(value) => {
                        if is_signed {
                            format!("{}", unsafe { *((&raw const value) as *const i16) })
                        } else {
                            format!("{}", value)
                        }
                    }

                    pdb2::Variant::U32(value) => {
                        if value > 0xFFFFFF {
                            let d = ((value >> 0) & 0xFF) as u8 as char;
                            let c = ((value >> 8) & 0xFF) as u8 as char;
                            let b = ((value >> 16) & 0xFF) as u8 as char;
                            let a = ((value >> 24) & 0xFF) as u8 as char;
                            if [a, b, c, d].iter().all(|c| *c >= 0x20 as char && c.is_ascii()) {
                                format!("'{a}{b}{c}{d}'")
                            } else {
                                if is_signed {
                                    format!("{}", unsafe { *((&raw const value) as *const i32) })
                                } else {
                                    format!("{}", value)
                                }
                            }
                        } else {
                            if is_signed {
                                format!("{}", unsafe { *((&raw const value) as *const i32) })
                            } else {
                                format!("{}", value)
                            }
                        }
                    }

                    pdb2::Variant::U64(value) => {
                        if value > 0xFFFFFF && value < 0x100000000 {
                            let d = ((value >> 0) & 0xFF) as u8 as char;
                            let c = ((value >> 8) & 0xFF) as u8 as char;
                            let b = ((value >> 16) & 0xFF) as u8 as char;
                            let a = ((value >> 24) & 0xFF) as u8 as char;
                            if [a, b, c, d].iter().all(|c| *c >= 0x20 as char && c.is_ascii()) {
                                format!("'{a}{b}{c}{d}'")
                            } else {
                                if is_signed {
                                    format!("{}", unsafe { *((&raw const value) as *const i64) })
                                } else {
                                    format!("{}", value)
                                }
                            }
                        } else {
                            if is_signed {
                                format!("{}", unsafe { *((&raw const value) as *const i64) })
                            } else {
                                format!("{}", value)
                            }
                        }
                    }

                    pdb2::Variant::I8(v) => {
                        if self.size > 1 {
                            if v == 0 {
                                format!("0")
                            } else if v > i8::MIN && v < i8::MAX {
                                format!("{}", v)
                            } else {
                                todo!("{value:#?}")
                            }
                        } else {
                            format!("{}", v)
                        }
                    }

                    pdb2::Variant::I16(v) => {
                        format!("{}", v)
                    }

                    pdb2::Variant::I32(v) => {
                        if self.size > 4 {
                            if v > 0xFFFFFF {
                                let d = ((v >> 0) & 0xFF) as u8 as char;
                                let c = ((v >> 8) & 0xFF) as u8 as char;
                                let b = ((v >> 16) & 0xFF) as u8 as char;
                                let a = ((v >> 24) & 0xFF) as u8 as char;
                                if [a, b, c, d].iter().all(|c| *c >= 0x20 as char && c.is_ascii()) {
                                    format!("'{a}{b}{c}{d}'")
                                } else {
                                    format!("{}", v)
                                }
                            } else {
                                todo!("{value:#?}")
                            }
                        } else if v > 0xFFFFFF {
                            let d = ((v >> 0) & 0xFF) as u8 as char;
                            let c = ((v >> 8) & 0xFF) as u8 as char;
                            let b = ((v >> 16) & 0xFF) as u8 as char;
                            let a = ((v >> 24) & 0xFF) as u8 as char;
                            if [a, b, c, d].iter().all(|c| *c >= 0x20 as char && c.is_ascii()) {
                                format!("'{a}{b}{c}{d}'")
                            } else {
                                format!("{}", v)
                            }
                        } else {
                            format!("{}", v)
                        }
                    }

                    pdb2::Variant::I64(v) => {
                        if self.size > 8 {
                            if v == 0 {
                                format!("0")
                            } else if v > i64::MIN && v < i64::MAX {
                                format!("{}", v)
                            } else {
                                todo!("{value:#?}")
                            }
                        } else {
                            format!("{}", v)
                        }
                    }
                }
            )?;
        }

        for _ in 0..self.depth {
            write!(f, "    ")?;
        }

        write!(f, "}};")?;

        Ok(())
    }
}
