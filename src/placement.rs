//! Declaration files for types in PDBs without IPI, which record no file for a type.
//!
//! Evidence, strongest first:
//! 1. the header holding line records of the type's member functions (an in-class body is inside the class);
//! 2. a header named after the source file defining its member functions, if that header is known;
//! 3. the file of the type it is nested in, or of the only types holding it by value;
//! 4. the only file whose code (locals, parameters, globals) uses it.
//!
//! Nested types always go with their outermost type, which their evidence counts for, and
//! function-local types with their function. Anonymous types and types without evidence stay
//! in `exported.h`.

use crate::{cpp, decompile::DecompileContext};
use pdb2::FallibleIterator;
use std::{
    collections::{BTreeMap, HashMap, HashSet},
    path::Path,
};

#[derive(Clone, Debug)]
pub struct Placement {
    /// The file as named by line records or the DBI file lists.
    pub file: String,
    /// Orders the type among the file's other members.
    pub line: u32,
    pub rule: &'static str,
    pub evidence: String,
    /// Placed types it holds by value or derives from, which must be defined before it
    pub requires: Vec<pdb2::TypeIndex>,
}

fn is_source(path: &str) -> bool {
    Path::new(path).extension().and_then(|e| e.to_str())
        .is_some_and(|e| cpp::SOURCE_FILE_EXTS.iter().any(|x| e.eq_ignore_ascii_case(x)))
}

fn is_header(path: &str) -> bool {
    Path::new(path).extension().and_then(|e| e.to_str())
        .is_some_and(|e| ["h", "hh", "hpp", "hxx"].iter().any(|x| e.eq_ignore_ascii_case(x)))
}

fn normalize(path: &str) -> String {
    crate::utils::sanitize_path(path).to_ascii_lowercase()
}

fn scope_of(name: &str) -> Option<String> {
    if let Some(i) = name.find("::operator") {
        return Some(name[..i].to_owned());
    }
    let parts = crate::namespaces::split_qualified(name);
    (parts.len() > 1).then(|| parts[..parts.len() - 1].join("::"))
}

struct Types<'a, 't> {
    finder: &'a pdb2::TypeFinder<'t>,
    names: HashMap<pdb2::TypeIndex, String>,
    by_name: HashMap<String, pdb2::TypeIndex>,
    /// Definitions of anonymous types, which references may reach through a forward reference
    anonymous: HashMap<String, pdb2::TypeIndex>,
}

impl Types<'_, '_> {
    /// The placed type a type reference names, through modifiers, arrays and optionally pointers.
    fn resolve(&self, mut index: pdb2::TypeIndex, through_pointers: bool) -> Option<pdb2::TypeIndex> {
        for _ in 0..16 {
            let name = match self.finder.find(index).ok()?.parse().ok()? {
                pdb2::TypeData::Modifier(m) => { index = m.underlying_type; continue; }
                pdb2::TypeData::Array(a) => { index = a.element_type; continue; }
                pdb2::TypeData::Pointer(p) if through_pointers => { index = p.underlying_type; continue; }
                pdb2::TypeData::Class(c) => c.name.to_string().into_owned(),
                pdb2::TypeData::Union(u) => u.name.to_string().into_owned(),
                pdb2::TypeData::Enumeration(e) => e.name.to_string().into_owned(),
                _ => return None,
            };
            if self.names.contains_key(&index) {
                return Some(index);
            }
            return (!cpp::is_anonymous_type_name(&name)).then(|| self.by_name.get(&name).copied()).flatten();
        }
        None
    }

    /// The anonymous class or union definition a type reference names, through modifiers and arrays.
    fn anonymous_definition(&self, mut index: pdb2::TypeIndex) -> Option<pdb2::TypeIndex> {
        for _ in 0..16 {
            let name = match self.finder.find(index).ok()?.parse().ok()? {
                pdb2::TypeData::Modifier(m) => { index = m.underlying_type; continue; }
                pdb2::TypeData::Array(a) => { index = a.element_type; continue; }
                pdb2::TypeData::Class(c) => c.name.to_string().into_owned(),
                pdb2::TypeData::Union(u) => u.name.to_string().into_owned(),
                _ => return None,
            };
            return cpp::is_anonymous_type_name(&name).then(|| self.anonymous.get(&name).copied().unwrap_or(index));
        }
        None
    }

    /// Placed types held by value or derived from. An anonymous member is defined inline, so
    /// what it holds counts as held here.
    fn by_value_dependencies(&self, index: pdb2::TypeIndex) -> Vec<pdb2::TypeIndex> {
        let mut result = vec![];
        let mut pending = vec![index];
        let mut seen = HashSet::new();
        while let Some(index) = pending.pop() {
            if !seen.insert(index) {
                continue;
            }
            let mut next = match self.finder.find(index).and_then(|t| t.parse()) {
                Ok(pdb2::TypeData::Class(c)) => c.fields,
                Ok(pdb2::TypeData::Union(u)) => Some(u.fields),
                _ => None,
            };
            while let Some(fields) = next {
                let Ok(pdb2::TypeData::FieldList(list)) = self.finder.find(fields).and_then(|t| t.parse()) else { break };
                for field in &list.fields {
                    let field_type = match field {
                        pdb2::TypeData::Member(m) => m.field_type,
                        pdb2::TypeData::BaseClass(b) => b.base_class,
                        pdb2::TypeData::VirtualBaseClass(v) => v.base_class,
                        _ => continue,
                    };
                    match self.resolve(field_type, false) {
                        Some(dependency) => result.push(dependency),
                        None => pending.extend(self.anonymous_definition(field_type)),
                    }
                }
                next = list.continuation;
            }
        }
        result
    }

    fn signature(&self, index: pdb2::TypeIndex) -> Vec<pdb2::TypeIndex> {
        let (return_type, arguments) = match self.finder.find(index).and_then(|t| t.parse()) {
            Ok(pdb2::TypeData::Procedure(p)) => (p.return_type, p.argument_list),
            Ok(pdb2::TypeData::MemberFunction(m)) => (Some(m.return_type), m.argument_list),
            _ => return vec![],
        };
        let mut result = return_type.and_then(|r| self.resolve(r, true)).into_iter().collect::<Vec<_>>();
        if let Ok(pdb2::TypeData::ArgumentList(list)) = self.finder.find(arguments).and_then(|t| t.parse()) {
            result.extend(list.arguments.iter().filter_map(|a| self.resolve(*a, true)));
        }
        result
    }
}

/// Places `types` (complete definitions, one per name) from the evidence in every module.
pub fn place_types(context: &mut DecompileContext, types: &[pdb2::TypeIndex]) -> pdb2::Result<HashMap<pdb2::TypeIndex, Placement>> {
    let mut names = HashMap::new();
    let mut by_name = HashMap::new();
    let mut anonymous = HashMap::new();
    for &index in types {
        let name = match context.type_finder.find(index).and_then(|t| t.parse()) {
            Ok(pdb2::TypeData::Class(c)) => c.name.to_string().into_owned(),
            Ok(pdb2::TypeData::Union(u)) => u.name.to_string().into_owned(),
            Ok(pdb2::TypeData::Enumeration(e)) => e.name.to_string().into_owned(),
            _ => continue,
        };
        // Anonymous types stay in exported.h: one holding only constants is defined nowhere else
        if cpp::is_anonymous_type_name(&name) {
            anonymous.entry(name).or_insert(index);
            continue;
        }
        by_name.entry(name.clone()).or_insert(index);
        names.insert(index, name);
    }
    let types = Types { finder: &context.type_finder, names, by_name, anonymous };

    // Nested types are embedded in their parent, so they go wherever the outermost type goes, and
    // their evidence counts for it. Function-local types go wherever their function goes.
    let parent = |index: pdb2::TypeIndex| scope_of(&types.names[&index]).and_then(|s| types.by_name.get(&s).copied());
    let outermost = |mut index| {
        while let Some(parent) = parent(index) {
            index = parent;
        }
        index
    };
    let function_local = types.names.iter()
        .filter_map(|(index, name)| Some((*index, crate::namespaces::split_local_scope(name)?)))
        .collect::<HashMap<_, _>>();

    // Known files by normalized path, keeping the spelling first seen.
    let mut known = BTreeMap::<String, String>::new();
    for files in context.source_files.iter() {
        for file in files {
            known.entry(normalize(file)).or_insert_with(|| file.clone());
        }
    }
    // Member function definitions: (file, line, procedure name)
    let mut methods = HashMap::<pdb2::TypeIndex, Vec<(String, u32, String)>>::new();
    // Files whose code uses a type, by normalized path: (spelling, earliest line)
    let mut uses = HashMap::<pdb2::TypeIndex, BTreeMap<String, (String, u32)>>::new();
    let note = |uses: &mut HashMap<pdb2::TypeIndex, BTreeMap<String, (String, u32)>>, index, file: &str, line: u32| {
        let first = uses.entry(outermost(index)).or_default().entry(normalize(file)).or_insert_with(|| (file.to_owned(), line));
        first.1 = first.1.min(line);
    };
    // The first definition of each procedure: (file, line)
    let mut procedures = HashMap::<String, (String, u32)>::new();

    let modules = context.debug_info.modules()?.collect::<Vec<_>>()?;
    for (module_index, module) in modules.iter().enumerate() {
        let Ok(Some(module_info)) = context.pdb.module_info(module) else { continue };
        let line_program = module_info.line_program();
        let Ok(lines) = crate::legacy::module_source_lines(context.pdb, module, line_program.as_ref().ok(), context.string_table) else { continue };
        let Ok(mut symbols) = module_info.symbols() else { continue };
        let mut first_lines = HashMap::new();
        for (file, offsets) in lines.iter().collect::<BTreeMap<_, _>>() {
            known.entry(normalize(file)).or_insert_with(|| file.clone());
            for offset in offsets.keys() {
                first_lines.entry(*offset).or_insert_with(|| {
                    let (file, line) = crate::legacy::line_at(&lines, *offset).unwrap();
                    (file.as_str(), line.line_start)
                });
            }
        }
        let module_source = context.source_files.get(module_index)
            .and_then(|files| files.iter().find(|f| is_source(f)));

        let mut procedure = None::<(&str, u32)>;
        let mut depth = 0usize;
        while let Ok(Some(symbol)) = symbols.next() {
            if symbol.ends_scope() {
                depth = depth.saturating_sub(1);
                if depth == 0 {
                    procedure = None;
                }
            }
            let starts_scope = symbol.starts_scope();
            match symbol.parse() {
                Ok(pdb2::SymbolData::Procedure(data)) => {
                    procedure = first_lines.get(&data.offset).copied();
                    if let Some((file, line)) = procedure {
                        for index in types.signature(data.type_index) {
                            note(&mut uses, index, file, line);
                        }
                        let name = data.name.to_string();
                        procedures.entry(name.to_string()).or_insert_with(|| (file.to_owned(), line));
                        if let Some(owner) = scope_of(&name).and_then(|s| types.by_name.get(&s).copied()) {
                            methods.entry(outermost(owner)).or_default().push((file.to_owned(), line, name.into_owned()));
                        }
                    }
                }
                Ok(pdb2::SymbolData::Data(data)) => {
                    let place = first_lines.get(&data.offset).copied().or(procedure)
                        .or_else(|| module_source.map(|f| (f.as_str(), 0)));
                    if let (Some((file, line)), Some(index)) = (place, types.resolve(data.type_index, true)) {
                        note(&mut uses, index, file, line);
                    }
                }
                Ok(pdb2::SymbolData::Local(pdb2::LocalSymbol { type_index, .. }))
                | Ok(pdb2::SymbolData::RegisterRelative(pdb2::RegisterRelativeSymbol { type_index, .. }))
                | Ok(pdb2::SymbolData::BasePointerRelative(pdb2::BasePointerRelativeSymbol { type_index, .. }))
                | Ok(pdb2::SymbolData::RegisterVariable(pdb2::RegisterVariableSymbol { type_index, .. })) => {
                    if let (Some((file, line)), Some(index)) = (procedure, types.resolve(type_index, true)) {
                        note(&mut uses, index, file, line);
                    }
                }
                _ => {}
            }
            if starts_scope {
                depth += 1;
            }
        }
    }

    let mut placed = HashMap::<pdb2::TypeIndex, Placement>::new();

    // 1. The header holding most of its member functions' line records
    let mut owners = methods.keys().copied().filter(|o| !function_local.contains_key(o)).collect::<Vec<_>>();
    owners.sort();
    for &owner in &owners {
        let mut headers = BTreeMap::<String, (usize, u32, &str, &str)>::new();
        for (file, line, name) in methods[&owner].iter().filter(|(f, ..)| !is_source(f)) {
            let entry = headers.entry(normalize(file)).or_insert((0, *line, file.as_str(), name.as_str()));
            entry.0 += 1;
            if *line < entry.1 {
                (entry.1, entry.3) = (*line, name.as_str());
            }
        }
        if let Some((_, (_, line, file, name))) = headers.into_iter().max_by(|a, b| a.1.0.cmp(&b.1.0).then(b.0.cmp(&a.0))) {
            placed.insert(owner, Placement {
                file: file.to_owned(),
                line: line.saturating_sub(1),
                rule: "member function lines",
                evidence: format!("{name} is defined at line {line}"),
                requires: vec![],
            });
        }
    }

    // 2. A known header named after the source file defining its member functions
    let mut headers_by_stem = HashMap::<String, Vec<&String>>::new();
    for (normalized, file) in &known {
        if is_header(normalized) && let Some(stem) = Path::new(normalized).file_stem() {
            headers_by_stem.entry(stem.to_string_lossy().into_owned()).or_default().push(file);
        }
    }
    for &owner in &owners {
        if placed.contains_key(&owner) {
            continue;
        }
        let mut sources = BTreeMap::<String, (usize, &str, &str)>::new();
        for (file, _, name) in &methods[&owner] {
            sources.entry(normalize(file)).or_insert((0, file.as_str(), name.as_str())).0 += 1;
        }
        let Some((source, (_, _, name))) = sources.into_iter().max_by(|a, b| a.1.0.cmp(&b.1.0).then(b.0.cmp(&a.0))) else { continue };
        let Some(stem) = Path::new(&source).file_stem().map(|s| s.to_string_lossy().into_owned()) else { continue };
        let Some(candidates) = headers_by_stem.get(&stem) else { continue };
        // Prefer the header sharing the longest directory prefix with the source; a tie is no evidence.
        let shared = |header: &str| normalize(header).split('/').zip(source.split('/')).take_while(|(a, b)| a == b).count();
        let best = candidates.iter().map(|h| shared(h)).max().unwrap_or(0);
        let mut best_headers = candidates.iter().filter(|h| shared(h) == best);
        if let (Some(header), None) = (best_headers.next(), best_headers.next()) {
            placed.insert(owner, Placement {
                file: (*header).clone(),
                line: 0,
                rule: "header named after source",
                evidence: format!("{name} is defined in {}", Path::new(&source).file_name().unwrap_or_default().to_string_lossy()),
                requires: vec![],
            });
        }
    }

    let mut dependencies = HashMap::<pdb2::TypeIndex, Vec<pdb2::TypeIndex>>::new();
    let mut users = HashMap::<pdb2::TypeIndex, HashSet<pdb2::TypeIndex>>::new();
    for &index in types.names.keys() {
        let mut requires = types.by_value_dependencies(index);
        requires.retain(|dependency| *dependency != index);
        requires.sort();
        requires.dedup();
        for &dependency in &requires {
            users.entry(dependency).or_default().insert(index);
        }
        dependencies.insert(index, requires);
    }
    let mut order = types.names.keys().copied().collect::<Vec<_>>();
    order.sort();

    // A function-local type is embedded in its function; an anonymous one has no spelling there.
    for (&index, (function, local)) in &function_local {
        if local.contains("<unnamed") {
            continue;
        }
        if let Some((file, line)) = procedures.get(function) {
            placed.insert(index, Placement {
                file: file.clone(),
                line: *line,
                rule: "function-local",
                evidence: format!("local to {function}"),
                requires: vec![],
            });
        }
    }

    propagate(&order, &types, &users, &parent, &function_local, &mut placed);

    // 4. The only file whose code uses it
    for &index in &order {
        if placed.contains_key(&index) || parent(index).is_some() || function_local.contains_key(&index) {
            continue;
        }
        let Some(files) = uses.get(&index).filter(|f| f.len() == 1) else { continue };
        let (file, line) = files.values().next().unwrap();
        placed.insert(index, Placement {
            file: file.clone(),
            line: line.saturating_sub(1),
            rule: "only use",
            evidence: "used only by code in this file".into(),
            requires: vec![],
        });
    }

    propagate(&order, &types, &users, &parent, &function_local, &mut placed);

    for (index, placement) in placed.iter_mut() {
        placement.requires = dependencies.remove(index).unwrap_or_default();
    }

    Ok(placed)
}

/// 3. Nested types follow their parent; a type held by value only from one file joins it.
fn propagate(
    order: &[pdb2::TypeIndex],
    types: &Types,
    users: &HashMap<pdb2::TypeIndex, HashSet<pdb2::TypeIndex>>,
    parent: &dyn Fn(pdb2::TypeIndex) -> Option<pdb2::TypeIndex>,
    function_local: &HashMap<pdb2::TypeIndex, (String, String)>,
    placed: &mut HashMap<pdb2::TypeIndex, Placement>,
) {
    loop {
        let mut changed = false;
        for &index in order {
            if placed.contains_key(&index) || function_local.contains_key(&index) {
                continue;
            }
            // A nested type is embedded in its parent, so it can only go where the parent goes.
            if let Some(parent) = parent(index) {
                if let Some(placement) = placed.get(&parent) {
                    let placement = Placement {
                        rule: "nested",
                        evidence: format!("nested in {}", types.names[&parent]),
                        ..placement.clone()
                    };
                    placed.insert(index, placement);
                    changed = true;
                }
                continue;
            }
            let Some(holders) = users.get(&index) else { continue };
            let Some(places) = holders.iter().map(|u| placed.get(u).map(|p| (u, p))).collect::<Option<Vec<_>>>() else { continue };
            let files = places.iter().map(|(_, p)| normalize(&p.file)).collect::<HashSet<_>>();
            if files.len() != 1 {
                continue;
            }
            let (holder, first) = places.iter().min_by_key(|(u, p)| (p.line, **u)).unwrap();
            let placement = Placement {
                file: first.file.clone(),
                line: first.line.saturating_sub(1),
                rule: "held by value",
                evidence: format!("held by value only by types placed here, such as {}", types.names[*holder]),
                requires: vec![],
            };
            placed.insert(index, placement);
            changed = true;
        }
        if !changed {
            break;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scope_keeps_template_arguments_and_operators() {
        assert_eq!(scope_of("A::B<C::D>::f").as_deref(), Some("A::B<C::D>"));
        assert_eq!(scope_of("A::B::operator<").as_deref(), Some("A::B"));
        assert_eq!(scope_of("A::B::operator()").as_deref(), Some("A::B"));
        assert_eq!(scope_of("f"), None);
    }

    #[test]
    fn extensionless_library_headers_are_not_headers_by_name() {
        assert!(is_source(r"D:\src\a.CPP") && !is_source(r"C:\vc\include\xtree"));
        assert!(is_header("x/foo.hpp") && !is_header("x/foo.inl") && !is_header(r"C:\vc\include\xtree"));
        assert_eq!(normalize(r"Z:\Dev\Foo.h"), normalize("z:/dev/foo.h"));
    }
}
