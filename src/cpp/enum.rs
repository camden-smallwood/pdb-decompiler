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

        let all_sequential = if let Some(first) = self.values.first() {
            let first_val =  
                match first.value {
                        pdb2::Variant::U8(n)  => (unsafe { *((&raw const n) as *const i8) }) as i64,
                        pdb2::Variant::U16(n) => n as i64,
                        pdb2::Variant::U32(n) => n as i64,
                        pdb2::Variant::U64(n) => n as i64,
                        pdb2::Variant::I8(n)  => n as i64,
                        pdb2::Variant::I16(n) => n as i64,
                        pdb2::Variant::I32(n) => n as i64,
                        pdb2::Variant::I64(n) => n as i64
                };

            let first_val_ok = first_val <= 0;

            first_val_ok && self
                .values
                .iter()
                .enumerate()
                .all(|(i, v)| {
                    let i = i as i64;
                    match v.value {
                        pdb2::Variant::U8(n)  => (unsafe { *((&raw const n) as *const i8) }) as i64 == (first_val + i),
                        pdb2::Variant::U16(n) => n as i64 == (first_val + i),
                        pdb2::Variant::U32(n) => n as i64 == (first_val + i),
                        pdb2::Variant::U64(n) => n as i64 == (first_val + i),
                        pdb2::Variant::I8(n)  => n as i64 == (first_val + i),
                        pdb2::Variant::I16(n) => n as i64 == (first_val + i),
                        pdb2::Variant::I32(n) => n as i64 == (first_val + i),
                        pdb2::Variant::I64(n) => n as i64 == (first_val + i),
                    }
                })
        } else {
            false
        };

        let all_match_bitfield = 
            self.values.len() > 2 &&
            self.values.iter().enumerate().all(|(i, v)| {
                let i = i as u32; // for bit-width comparisons & shifts
                match v.value {
                    // Unsigned: allow bit positions 0..BITS-1
                    pdb2::Variant::U8(n)   => i < u8::BITS  && n == (1u8  << i),
                    pdb2::Variant::U16(n)  => i < u16::BITS && n == (1u16 << i),
                    pdb2::Variant::U32(n)  => i < u32::BITS && n == (1u32 << i),
                    pdb2::Variant::U64(n)  => i < u64::BITS && n == (1u64 << i),

                    // Signed: highest usable bit is BITS-2 (to stay non-negative)
                    pdb2::Variant::I8(n)   => i < (i8::BITS  - 1) && n >= 0 && (n as i16)  == ((1i16  << i) as i16),
                    pdb2::Variant::I16(n)  => i < (i16::BITS - 1) && n >= 0 && (n as i32)  == ((1i32  << i) as i32),
                    pdb2::Variant::I32(n)  => i < (i32::BITS - 1) && n >= 0 && (n as i64)  == ((1i64  << i) as i64),
                    pdb2::Variant::I64(n)  => i < (i64::BITS - 1) && n >= 0 && (n as i128) == ((1i128 << i) as i128),
                }
            });

        let fmt_value_simple = |v: &pdb2::Variant| -> String {
            match *v {
                pdb2::Variant::U8(value) => { format!("{}", unsafe { *((&raw const value) as *const i8) }) } 
                pdb2::Variant::U16(value) => { format!("{}", value) } 
                pdb2::Variant::U32(value) => { format!("{}", value) } 
                pdb2::Variant::U64(value) => { format!("{}", value) } 
                pdb2::Variant::I8(value) => { format!("{}", value) } 
                pdb2::Variant::I16(value) => { format!("{}", value) } 
                pdb2::Variant::I32(value) => { format!("{}", value) } 
                pdb2::Variant::I64(value) => { format!("{}", value) }
            }
        };

        let fmt_value = |v: &pdb2::Variant| -> String {
            match *v {
                pdb2::Variant::U8(value) => {
                    let ch = if value >= 0x20 && value <= 0x7E && (0x20..=0x7E).contains(&(value as u8)) {
                        format!(" '{}'", value as char)
                    } else { String::new() };
                    format!(
                        "{}, // 0x{:02X}{}",
                        unsafe { *((&raw const value) as *const i8) },
                        value,
                        ch
                    )
                }
                pdb2::Variant::U16(value) => {
                    let ch = if value >= 0x20 && value <= 0x7E && (0x20..=0x7E).contains(&(value as u8)) {
                        format!(" '{}'", value as u8 as char)
                    } else { String::new() };
                    let _a = ((value >> 0) & 0xFF) as u8 as char;
                    let _b = ((value >> 8) & 0xFF) as u8 as char;
                    if value > 0xFF && [_b, _a].iter().all(|c| *c >= 0x20 as char && *c <= 0x7E as char) 
                    {
                        format!("{}, // 0x{:04X} {}", value, value, format!("'{_b}{_a}'"))
                    }
                    else
                    {
                        format!("{}, // 0x{:04X}{}", value, value, ch)
                    }
                }
                pdb2::Variant::U32(value) => {
                    let ch = if value >= 0x20 && value <= 0x7E && (0x20..=0x7E).contains(&(value as u8)) {
                        format!(" '{}'", value as u8 as char)
                    } else { String::new() };
                    let _a = ((value >> 0) & 0xFF) as u8 as char;
                    let _b = ((value >> 8) & 0xFF) as u8 as char;
                    let _c = ((value >> 16) & 0xFF) as u8 as char;
                    let _d = ((value >> 24) & 0xFF) as u8 as char;
                    if value > 0xFFFFFF && [_d, _c, _b, _a].iter().all(|c| *c >= 0x20 as char && *c <= 0x7E as char) 
                    {
                        format!("{}, // 0x{:08X}{}", format!("'{_d}{_c}{_b}{_a}'"), value, ch)
                    }
                    else if value > 0xFFFFFF && _a == 0 as char && [_d, _c, _b].iter().all(|c| *c >= 0x20 as char && *c <= 0x7E as char) 
                    {
                        format!("{}, // 0x{:08X} {}", value, value, format!("'{_d}{_c}{_b}\\0'"))
                    }
                    else if value > 0xFFFFFF && _a == 0 as char && _b == 0 as char && [_d, _c].iter().all(|c| *c >= 0x20 as char && *c <= 0x7E as char) 
                    {
                        format!("{}, // 0x{:08X} {}", value, value, format!("'{_c}{_b}\\0\\0'"))
                    }
                    else if value > 0xFFFF && [_c, _b, _a].iter().all(|c| *c >= 0x20 as char && *c <= 0x7E as char) 
                    {
                        format!("{}, // 0x{:08X} {}", value, value, format!("'{_c}{_b}{_a}'"))
                    }
                    else if value > 0xFF && [_b, _a].iter().all(|c| *c >= 0x20 as char && *c <= 0x7E as char) 
                    {
                        format!("{}, // 0x{:08X} {}", value, value, format!("'{_b}{_a}'"))
                    }
                    else
                    {
                        format!("{}, // 0x{:08X}{}", value, value, ch)
                    }
                }
                pdb2::Variant::U64(value) => {
                    let ch = if value >= 0x20 && value <= 0x7E && (0x20..=0x7E).contains(&(value as u8)) {
                        format!(" '{}'", value as u8 as char)
                    } else { String::new() };
                    format!("{}, // 0x{:016X}{}", value, value, ch)
                }
                pdb2::Variant::I8(value) => {
                    let ch = if value >= 0x20 && value <= 0x7E && (0x20..=0x7E).contains(&(value as u8)) {
                        format!(" '{}'", value as u8 as char)
                    } else { String::new() };
                    format!("{}, // 0x{:02X}{}", value, value, ch)
                }
                pdb2::Variant::I16(value) => {
                    let ch = if value >= 0x20 && value <= 0x7E && (0x20..=0x7E).contains(&(value as u8)) {
                        format!(" '{}'", value as u8 as char)
                    } else { String::new() };
                    let _a = ((value >> 0) & 0xFF) as u8 as char;
                    let _b = ((value >> 8) & 0xFF) as u8 as char;
                    if value > 0xFF && [_b, _a].iter().all(|c| *c >= 0x20 as char && *c <= 0x7E as char) 
                    {
                        format!("{}, // 0x{:04X} {}", value, value, format!("'{_b}{_a}'"))
                    }
                    else
                    {
                        format!("{}, // 0x{:04X}{}", value, value, ch)
                    }
                }
                pdb2::Variant::I32(value) => {
                    let ch = if value >= 0x20 && value <= 0x7E && (0x20..=0x7E).contains(&(value as u8)) {
                        format!(" '{}'", value as u8 as char)
                    } else { String::new() };
                    let _a = ((value >> 0) & 0xFF) as u8 as char;
                    let _b = ((value >> 8) & 0xFF) as u8 as char;
                    let _c = ((value >> 16) & 0xFF) as u8 as char;
                    let _d = ((value >> 24) & 0xFF) as u8 as char;
                    if value > 0xFFFFFF && [_d, _c, _b, _a].iter().all(|c| *c >= 0x20 as char && *c <= 0x7E as char) 
                    {
                        format!("{}, // 0x{:08X}{}", format!("'{_d}{_c}{_b}{_a}'"), value, ch)
                    }
                    else if value > 0xFFFFFF && _a == 0 as char && [_d, _c, _b].iter().all(|c| *c >= 0x20 as char && *c <= 0x7E as char) 
                    {
                        format!("{}, // 0x{:08X} {}", value, value, format!("'{_d}{_c}{_b}\\0'"))
                    }
                    else if value > 0xFFFFFF && _a == 0 as char && _b == 0 as char && [_d, _c].iter().all(|c| *c >= 0x20 as char && *c <= 0x7E as char) 
                    {
                        format!("{}, // 0x{:08X} {}", value, value, format!("'{_c}{_b}\\0\\0'"))
                    }
                    else if value > 0xFFFF && [_c, _b, _a].iter().all(|c| *c >= 0x20 as char && *c <= 0x7E as char) 
                    {
                        format!("{}, // 0x{:08X} {}", value, value, format!("'{_c}{_b}{_a}'"))
                    }
                    else if value > 0xFF && [_b, _a].iter().all(|c| *c >= 0x20 as char && *c <= 0x7E as char) 
                    {
                        format!("{}, // 0x{:08X} {}", value, value, format!("'{_b}{_a}'"))
                    }
                    else
                    {
                        format!("{}, // 0x{:08X}{}", value, value, ch)
                    }
                }
                pdb2::Variant::I64(value) => {
                    let ch = if value >= 0x20 && value <= 0x7E && (0x20..=0x7E).contains(&(value as u8)) {
                        format!(" '{}'", value as u8 as char)
                    } else { String::new() };
                    format!("{}, // 0x{:016X}{}", value, value, ch)
                }
            }
        };

        for (i, value) in self.values.iter().enumerate() {
            for _ in 0..self.depth {
                write!(f, "    ")?;
            }

            if all_match_index {
                if i == 0 {
                    writeln!(f, "    {} = 0,", value.name)?;
                } else {
                    writeln!(f, "    {},", value.name)?;
                }
            } else if all_match_bitfield {
                writeln!(f, "    {} = (1 << {}),", value.name, i)?;
            } else if all_sequential {
                if let Some(first) = self.values.first() {
                    let first_val =  
                        match first.value {
                                pdb2::Variant::U8(n)  => (unsafe { *((&raw const n) as *const i8) }) as i64,
                                pdb2::Variant::U16(n) => n as i64,
                                pdb2::Variant::U32(n) => n as i64,
                                pdb2::Variant::U64(n) => n as i64,
                                pdb2::Variant::I8(n)  => n as i64,
                                pdb2::Variant::I16(n) => n as i64,
                                pdb2::Variant::I32(n) => n as i64,
                                pdb2::Variant::I64(n) => n as i64
                        };

                    if (i as i64) <= -first_val {
                        writeln!(f, "    {} = {},", value.name, fmt_value_simple(&value.value))?;
                    } else {
                        writeln!(f, "    {},", value.name)?;
                    }
                } else {
                    unreachable!();
                };
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
