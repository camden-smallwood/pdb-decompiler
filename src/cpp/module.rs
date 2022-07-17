use std::{fmt, path::PathBuf};
use super::{Class, Enum, type_name};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModuleMember {
    Class(Class),
    Enum(Enum),
    UserDefinedType(String),
    UsingNamespace(String),
    Constant(String),
    Data(String, u64),
    ThreadStorage(String, u64),
    Procedure(String, u64),
}

impl fmt::Display for ModuleMember {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Class(c) => c.fmt(f),
            Self::Enum(e) => e.fmt(f),
            Self::UserDefinedType(u) => u.fmt(f),
            Self::UsingNamespace(n) => f.write_fmt(format_args!("using namespace {n};")),
            Self::Constant(c) => c.fmt(f),
            Self::Data(d, _) => d.fmt(f),
            Self::ThreadStorage(t, _) => t.fmt(f),
            Self::Procedure(p, _) => p.fmt(f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub path: Option<PathBuf>,
    pub headers: Vec<PathBuf>,
    pub members: Vec<ModuleMember>
}

impl Module {
    pub fn new() -> Self {
        Self {
            path: None,
            headers: vec![],
            members: vec![]
        }
    }

    pub fn with_path(mut self, path: PathBuf) -> Self {
        self.path = Some(path);
        self
    }

    pub fn add_type_definition(
        &mut self,
        type_info: &pdb::TypeInformation,
        type_finder: &pdb::TypeFinder,
        type_index: pdb::TypeIndex,
        line: u32
    ) -> pdb::Result<()> {
        if self.members.iter().position(|x| match x {
            ModuleMember::Class(c) => c.index == type_index,
            ModuleMember::Enum(e) => e.index == type_index,
            _ => false
        }).is_some() {
            return Ok(())
        }

        let type_item = match type_finder.find(type_index) {
            Ok(type_item) => type_item,
            Err(e) => {
                eprintln!("WARNING: failed to find type: {e}");
                return Ok(())
            }
        };

        match type_item.parse() {
            Ok(pdb::TypeData::Class(data)) => {
                let mut definition = Class {
                    kind: Some(data.kind),
                    is_union: false,
                    is_declaration: false,
                    name: data.name.to_string().to_string(),
                    index: type_index,
                    depth: 0,
                    line,
                    size: data.size,
                    base_classes: vec![],
                    members: vec![],
                    field_attributes: None,
                };

                if let Some(derived_from) = data.derived_from {
                    if let Err(e) = definition.add_derived_from(type_finder, derived_from) {
                        eprintln!("WARNING: failed to add class derived from: {e}");
                    }
                }

                if data.properties.forward_reference() {
                    definition.is_declaration = true;
                } else if let Some(fields) = data.fields {
                    if let Err(e) = definition.add_members(type_info, type_finder, fields) {
                        eprintln!("WARNING: failed to add class members: {e}");
                    }
                }

                let mut exists = false;

                for member in self.members.iter() {
                    if let ModuleMember::Class(other_definition) = member {
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
                    self.members.push(ModuleMember::Class(definition));
                }
            }

            Ok(pdb::TypeData::Union(data)) => {
                let mut definition = Class {
                    kind: None,
                    is_union: true,
                    is_declaration: false,
                    name: data.name.to_string().to_string(),
                    index: type_index,
                    depth: 0,
                    line,
                    size: data.size,
                    base_classes: vec![],
                    members: vec![],
                    field_attributes: None,
                };
                
                if data.properties.forward_reference() {
                    definition.is_declaration = true;
                } else if let Err(e) = definition.add_members(type_info, type_finder, data.fields) {
                    eprintln!("WARNING: failed to add union members: {e}");
                }

                let mut exists = false;

                for member in self.members.iter() {
                    if let ModuleMember::Class(other_definition) = member {
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
                    self.members.push(ModuleMember::Class(definition));
                }
            }

            Ok(pdb::TypeData::Enumeration(data)) => {
                let underlying_type_name = match type_name(type_info, type_finder, data.underlying_type, None, None, true) {
                    Ok(name) => name,
                    Err(e) => {
                        eprintln!("WARNING: failed to get enum type name: {e}");
                        return Ok(())
                    }
                };
                
                let mut definition = Enum {
                    name: data.name.to_string().to_string(),
                    index: type_index,
                    depth: 0,
                    line,
                    underlying_type_name,
                    is_declaration: false,
                    values: vec![],
                    field_attributes: None,
                };

                if data.properties.forward_reference() {
                    definition.is_declaration = true;
                } else if let Err(e) = definition.add_members(type_finder, data.fields) {
                    eprintln!("WARNING: failed to add enum members: {e}");
                }

                let mut exists = false;

                for member in self.members.iter() {
                    if let ModuleMember::Enum(other_definition) = member {
                        if definition.name == other_definition.name
                        && definition.values.eq(&other_definition.values) {
                            exists = true;
                            break;
                        }
                    }
                }

                if !exists {
                    self.members.push(ModuleMember::Enum(definition));
                }
            }

            Ok(other) => panic!("Unhandled type data in SourceData::add_type_definition - {:?}", other),

            Err(err) => panic!("Unhandled error in SourceData::add_type_definition - {}", err)
        }

        Ok(())
    }
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut storage: Vec<(u32, ModuleMember)> = vec![];
        let mut prev_line = 0;
 
        if let Some(path) = &self.path {
            let mut is_header = false;

            match path.extension().and_then(std::ffi::OsStr::to_str) {
                Some("c" | "cc" | "cpp" | "cxx" | "pch" | "asm" | "fasm" | "masm" | "res" | "exp") => (),
                _ => is_header = true,
            }

            if is_header {
                writeln!(f, "#pragma once")?;
            }
        }

        for header in self.headers.iter() {
            writeln!(f, "#include \"{}\"", header.to_string_lossy())?;
        }
        
        for u in &self.members {
            match u {
                ModuleMember::Class(x) => {
                    storage.push((x.line, u.clone()));
                    prev_line = x.line;
                }

                ModuleMember::Enum(x) => {
                    storage.push((x.line, u.clone()));
                    prev_line = x.line;
                }

                _ => {
                    prev_line += 1;
                    storage.push((prev_line, u.clone()));
                }
            }
        }

        storage.sort_by(|a, b| a.0.cmp(&b.0));

        for item in storage {
            writeln!(f)?;
            item.1.fmt(f)?;
        }

        Ok(())
    }
}
