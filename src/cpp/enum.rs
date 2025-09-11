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

        let all_match_index = self
            .values
            .iter()
            .enumerate()
            .all(|(i, v)| {
                let i = i as u64;
                match v.value {
                    pdb2::Variant::U8(n)  => n == (i as u8),
                    pdb2::Variant::U16(n) => n == (i as u16),
                    pdb2::Variant::U32(n) => n == (i as u32),
                    pdb2::Variant::U64(n) => n == (i as u64),
                    pdb2::Variant::I8(n)  => n >= 0 && n == (i as i8),
                    pdb2::Variant::I16(n) => n >= 0 && n == (i as i16),
                    pdb2::Variant::I32(n) => n >= 0 && n == (i as i32),
                    pdb2::Variant::I64(n) => n >= 0 && n == (i as i64),
                }
            });

        let fmt_value = |v: &pdb2::Variant| -> String {
            match *v {
                pdb2::Variant::U8(value) => {
                    let c = if (0x20..=0x7E).contains(&value) {
                        format!(" '{}'", value as char)
                    } else { String::new() };
                    format!(
                        "{}, // 0x{:02X}{}",
                        unsafe { *((&raw const value) as *const i8) },
                        value,
                        c
                    )
                }
                pdb2::Variant::U16(value) => {
                    let c = if (0x20..=0x7E).contains(&(value as u8)) {
                        format!(" '{}'", value as u8 as char)
                    } else { String::new() };
                    format!("{}, // 0x{:04X}{}", value, value, c)
                }
                pdb2::Variant::U32(value) => {
                    let c = if (0x20..=0x7E).contains(&(value as u8)) {
                        format!(" '{}'", value as u8 as char)
                    } else { String::new() };
                    format!("{}, // 0x{:08X}{}", value, value, c)
                }
                pdb2::Variant::U64(value) => {
                    let c = if (0x20..=0x7E).contains(&(value as u8)) {
                        format!(" '{}'", value as u8 as char)
                    } else { String::new() };
                    format!("{}, // 0x{:016X}{}", value, value, c)
                }
                pdb2::Variant::I8(value) => {
                    let c = if (0x20..=0x7E).contains(&(value as u8)) {
                        format!(" '{}'", value as u8 as char)
                    } else { String::new() };
                    format!("{}, // 0x{:02X}{}", value, value as u8, c)
                }
                pdb2::Variant::I16(value) => {
                    let c = if (0x20..=0x7E).contains(&(value as u8)) {
                        format!(" '{}'", value as u8 as char)
                    } else { String::new() };
                    format!("{}, // 0x{:04X}{}", value, value as u16, c)
                }
                pdb2::Variant::I32(value) => {
                    let c = if (0x20..=0x7E).contains(&(value as u8)) {
                        format!(" '{}'", value as u8 as char)
                    } else { String::new() };
                    format!("{}, // 0x{:08X}{}", value, value as u32, c)
                }
                pdb2::Variant::I64(value) => {
                    let c = if (0x20..=0x7E).contains(&(value as u8)) {
                        format!(" '{}'", value as u8 as char)
                    } else { String::new() };
                    format!("{}, // 0x{:016X}{}", value, value as u64, c)
                }
            }
        };

        for value in &self.values {
            for _ in 0..self.depth {
                write!(f, "    ")?;
            }

            if all_match_index {
                // Sequential: omit explicit assignment
                writeln!(f, "    {},", value.name)?;
            } else {
                // Non-sequential: keep explicit value with hex/ASCII comment
                writeln!(f, "    {} = {},", value.name, fmt_value(&value.value))?;
            }
        }

        for _ in 0..self.depth {
            write!(f, "    ")?;
        }

        write!(f, "}};")?;

        Ok(())
    }
}
