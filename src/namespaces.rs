//! Reconstruction of C++ namespace blocks from the fully-qualified names MSVC
//! records in the PDB.
//!
//! MSVC emits every symbol/type with its fully-qualified name, but namespaces have
//! no type records of their own — only classes/enums/unions do. So, given the set
//! of all known type full-names, the namespace path of any qualified name is the
//! run of leading scope components before the first prefix that names a known type;
//! the remainder is the class chain and the entity's own name.
//!
//! Anonymous namespaces appear as a synthetic `` `anonymous namespace' `` component
//! (a hyphenated `` `anonymous-namespace' `` in type records — normalized here).
//! Compiler-generated scopes (`` `dynamic initializer for' ``, EH funclets like
//! `dtor$0`/`catch$0`, local `` `1' `` scopes) also look qualified but are not real
//! namespaces; those are reported as `CompilerGenerated` so callers can drop them.

use crate::cpp;
use std::collections::HashSet;

/// The type-record form of the anonymous-namespace token (symbols use a space).
const ANON_TOKEN: &str = "`anonymous-namespace'";

/// Normalize the three textual forms of the anonymous-namespace token to a single
/// canonical form (`ANON_TOKEN`):
///   * `` `anonymous namespace' `` — used in symbol names (space)
///   * `` `anonymous-namespace' `` — used in some type records (hyphen)
///   * `?A0x<hex>`                 — MSVC's mangled/hashed form (in typedefs, etc.)
pub fn normalize_anon(name: &str) -> String {
    let name = name.replace("`anonymous namespace'", ANON_TOKEN);
    replace_anon_hash(&name)
}

/// Replace every `?A0x<hex>` run (MSVC's hashed anonymous-namespace name) with the
/// canonical anonymous-namespace token.
fn replace_anon_hash(name: &str) -> String {
    if !name.contains("?A0x") {
        return name.to_string();
    }

    let mut result = String::with_capacity(name.len());
    let mut rest = name;
    while let Some(pos) = rest.find("?A0x") {
        result.push_str(&rest[..pos]);
        let after = &rest[pos + 4..];
        let hex_len = after.chars().take_while(|c| c.is_ascii_hexdigit()).count();
        result.push_str(ANON_TOKEN);
        rest = &after[hex_len..];
    }
    result.push_str(rest);
    result
}

/// Render a type *reference* string for emission: the anonymous namespace has no
/// writable C++ spelling, so its qualifier is removed wherever it appears (an anon
/// type is referred to by its unqualified name within the translation unit). Named
/// namespace qualifiers are kept — they are valid to write. This is the single
/// place that concern is handled; it must never be applied to a mangled name
/// (e.g. the `mangled_x64` string), whose `?A0x…` token is the real linker symbol.
pub fn render_reference(name: &str) -> String {
    let name = strip_interior_anonymous(name);

    if !name.contains('`') && !name.contains("?A0x") {
        return name;
    }

    normalize_anon(&name)
        .replace("`anonymous-namespace'::", "")
        .replace("`anonymous-namespace'", "")
}

/// Remove *interior* anonymous-type placeholder components from a qualified name, i.e. an
/// `<unnamed-tag>` / `<unnamed-type-*>` immediately followed by `::`. An anonymous struct or
/// union injects its members (including nested types) into the enclosing scope, so a name like
/// `hkAgentEntry::<unnamed-type-m_extraData>::GskFlags` was written `hkAgentEntry::GskFlags`.
/// A *trailing* placeholder (the type itself is anonymous) is left intact — those are inlined
/// elsewhere, and here it is only a fallback spelling.
pub fn strip_interior_anonymous(name: &str) -> String {
    if !name.contains("<unnamed") {
        return name.to_string();
    }

    let mut result = String::with_capacity(name.len());
    let mut rest = name;

    while let Some(pos) = rest.find("<unnamed") {
        result.push_str(&rest[..pos]);
        let after = &rest[pos..];

        match after.find('>') {
            Some(gt) => {
                let end = gt + 1;
                if after[end..].starts_with("::") {
                    // Interior placeholder: drop it and the following `::`.
                    rest = &after[end + 2..];
                } else {
                    // Trailing placeholder: keep it verbatim.
                    result.push_str(&after[..end]);
                    rest = &after[end..];
                }
            }
            None => {
                // Malformed (no closing `>`): copy the rest unchanged.
                result.push_str(after);
                rest = "";
                break;
            }
        }
    }

    result.push_str(rest);
    result
}

/// One component of a namespace path.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum NsKey {
    /// An unnamed namespace (`namespace { ... }`).
    Anonymous,
    /// A named namespace (`namespace ident { ... }`).
    Named(String),
}

/// The classification of a fully-qualified name.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum NameClass {
    /// Lives at global scope — emit unchanged.
    Global,
    /// Lives in the given namespace path (outer → inner).
    Namespaced(Vec<NsKey>),
    /// A compiler-generated artifact (funclet, dynamic initializer, local scope) —
    /// callers should exclude it.
    CompilerGenerated,
}

/// Split a qualified name on top-level `::`, keeping template arguments (`<...>`)
/// intact as part of a single component.
pub fn split_qualified(name: &str) -> Vec<String> {
    let mut parts = vec![];
    let mut cur = String::new();
    let mut depth: i32 = 0;
    let mut chars = name.chars().peekable();

    while let Some(c) = chars.next() {
        match c {
            '<' => { depth += 1; cur.push(c); }
            '>' => { depth -= 1; cur.push(c); }
            ':' if depth == 0 && chars.peek() == Some(&':') => {
                chars.next();
                parts.push(std::mem::take(&mut cur));
            }
            _ => cur.push(c),
        }
    }

    parts.push(cur);
    parts
}

/// A single scope component is "clean" (a real namespace/type component, not a
/// compiler-generated scope) when it is the anonymous-namespace token or contains
/// none of the synthetic markers MSVC uses (`` ` ``, `$`, `[`).
fn component_is_clean(component: &str, allow_anon: bool) -> bool {
    if allow_anon && component == ANON_TOKEN {
        return true;
    }

    !(component.contains('`') || component.contains('$') || component.contains('['))
}

/// Classify a fully-qualified name into global / namespaced / compiler-generated,
/// using the set of all known type full-names to tell namespaces from class scopes.
pub fn classify(full_name: &str, type_names: &HashSet<String>) -> NameClass {
    let normalized = normalize_anon(full_name);
    let components = split_qualified(&normalized);

    if components.is_empty() {
        return NameClass::Global;
    }

    // The final component is the entity's own (simple) name; the rest is its scope.
    let scope = &components[..components.len() - 1];

    // The namespace path is the leading scope components up to (but not including)
    // the first prefix that names a known type.
    let mut namespace_len = scope.len();
    for k in 0..scope.len() {
        if type_names.contains(&scope[..=k].join("::")) {
            namespace_len = k;
            break;
        }
    }

    // Anything from the first type prefix onward (plus the entity name) must be
    // ordinary C++ (class chain + member/function/operator name). A synthetic marker
    // there means the whole symbol is compiler-generated.
    let entity = components[namespace_len..].join("::");
    if !component_is_clean(&entity, false) {
        return NameClass::CompilerGenerated;
    }

    if namespace_len == 0 {
        return NameClass::Global;
    }

    // Every namespace component must be a real namespace (or the anon token).
    let mut path = Vec::with_capacity(namespace_len);
    for component in &scope[..namespace_len] {
        if !component_is_clean(component, true) {
            return NameClass::CompilerGenerated;
        }

        path.push(if component == ANON_TOKEN {
            NsKey::Anonymous
        } else {
            NsKey::Named(component.clone())
        });
    }

    NameClass::Namespaced(path)
}

/// Remove the leading namespace-path components from a fully-qualified name,
/// keeping the class chain and the entity's own name. Used to turn an out-of-line
/// definition name like `blah::c_foo::bar` into `c_foo::bar` (or `blah::hello`
/// into `hello`) so it can be declared *inside* its reconstructed namespace block.
/// Anonymous and named namespaces alike. Returns the input unchanged when it is
/// not namespaced or is compiler-generated.
pub fn strip_namespace_prefix(full_name: &str, type_names: &HashSet<String>) -> String {
    match classify(full_name, type_names) {
        NameClass::Namespaced(path) => {
            let components = split_qualified(&normalize_anon(full_name));
            components[path.len()..].join("::")
        }
        _ => full_name.to_string(),
    }
}

/// The fully-qualified defining name of a module member, if it has one that can
/// carry a namespace qualifier.
fn defining_name(member: &cpp::ModuleMember) -> Option<String> {
    match member {
        cpp::ModuleMember::Class(class) => Some(class.borrow().name.clone()),
        cpp::ModuleMember::Enum(e) => Some(e.name.clone()),
        cpp::ModuleMember::Procedure(p) => Some(p.name.clone()),
        cpp::ModuleMember::Data { name, .. } => Some(name.clone()),
        cpp::ModuleMember::ThreadStorage { name, .. } => Some(name.clone()),
        _ => None,
    }
}

/// Remove the leading `namespace_len` scope components from a class's own stored
/// name so it can be declared inside its reconstructed namespace block. Idempotent:
/// a name with no remaining namespace prefix is left unchanged.
fn strip_class_namespace_prefix(class: &std::rc::Rc<std::cell::RefCell<cpp::Class>>, namespace_len: usize) {
    let mut class = class.borrow_mut();
    let components = split_qualified(&normalize_anon(&class.name));
    if components.len() > namespace_len {
        class.name = components[namespace_len..].join("::");
    }
}

/// Strip the leading `namespace_len` components from a member's own declared name
/// (class or enum), so it reads correctly inside its reconstructed namespace block.
fn strip_member_namespace_prefix(member: &mut cpp::ModuleMember, namespace_len: usize) {
    match member {
        cpp::ModuleMember::Class(class) => strip_class_namespace_prefix(class, namespace_len),
        cpp::ModuleMember::Enum(e) => {
            let components = split_qualified(&normalize_anon(&e.name));
            if components.len() > namespace_len {
                e.name = components[namespace_len..].join("::");
            }
        }
        _ => {}
    }
}

/// Ordered grouping node: members declared directly at this level, plus child
/// namespaces in first-seen order.
struct NsNode {
    members: Vec<cpp::ModuleMember>,
    children: Vec<(NsKey, NsNode)>,
}

impl NsNode {
    fn new() -> Self {
        Self { members: vec![], children: vec![] }
    }

    fn child_mut(&mut self, key: &NsKey) -> &mut NsNode {
        if let Some(index) = self.children.iter().position(|(k, _)| k == key) {
            return &mut self.children[index].1;
        }
        self.children.push((key.clone(), NsNode::new()));
        &mut self.children.last_mut().unwrap().1
    }

    fn insert(&mut self, path: &[NsKey], member: cpp::ModuleMember) {
        match path.split_first() {
            None => self.members.push(member),
            Some((head, rest)) => self.child_mut(head).insert(rest, member),
        }
    }

    fn into_members(self) -> Vec<cpp::ModuleMember> {
        let mut members = self.members;

        // A class/enum must be defined before its out-of-line member definitions,
        // but members arrive sorted by source line (which can place a definition
        // first). Stable-partition so type definitions lead, order preserved within.
        members.sort_by_key(|m| match m {
            cpp::ModuleMember::Class(_) | cpp::ModuleMember::Enum(_) => 0,
            _ => 1,
        });

        for (key, node) in self.children {
            members.push(cpp::ModuleMember::Namespace {
                name: match key {
                    NsKey::Anonymous => None,
                    NsKey::Named(name) => Some(name),
                },
                members: node.into_members(),
            });
        }
        members
    }
}

/// Rewrite a module's flat member list, gathering every namespaced member into
/// reconstructed (possibly nested) `namespace { ... }` blocks. Global members keep
/// their relative position; each top-level namespace block is emitted where its
/// first member appeared, so ordering stays close to the original.
pub fn group_module_members(
    members: Vec<cpp::ModuleMember>,
    type_names: &HashSet<String>,
) -> Vec<cpp::ModuleMember> {
    // Nothing to do if no member is namespaced.
    if !members.iter().any(|m| {
        defining_name(m)
            .map(|n| matches!(classify(&n, type_names), NameClass::Namespaced(_)))
            .unwrap_or(false)
    }) {
        return members;
    }

    enum Slot {
        Global(cpp::ModuleMember),
        Namespace(NsKey),
    }

    let mut slots: Vec<Slot> = vec![];
    let mut roots: Vec<(NsKey, NsNode)> = vec![];

    for mut member in members {
        let path = match defining_name(&member) {
            Some(name) => match classify(&name, type_names) {
                NameClass::Namespaced(path) => path,
                _ => {
                    slots.push(Slot::Global(member));
                    continue;
                }
            },
            None => {
                slots.push(Slot::Global(member));
                continue;
            }
        };

        // Strip the namespace prefix from the member's own declared name so it reads
        // correctly inside its reconstructed namespace block.
        strip_member_namespace_prefix(&mut member, path.len());

        let root_key = path[0].clone();
        if !roots.iter().any(|(k, _)| *k == root_key) {
            slots.push(Slot::Namespace(root_key.clone()));
            roots.push((root_key.clone(), NsNode::new()));
        }

        let node = &mut roots.iter_mut().find(|(k, _)| *k == root_key).unwrap().1;
        node.insert(&path[1..], member);
    }

    // Each root node's `into_members()` is exactly the content of that namespace.
    let mut built: Vec<(NsKey, Vec<cpp::ModuleMember>)> = roots
        .into_iter()
        .map(|(key, node)| (key, node.into_members()))
        .collect();

    let mut result = vec![];
    for slot in slots {
        match slot {
            Slot::Global(member) => result.push(member),
            Slot::Namespace(key) => {
                let index = built.iter().position(|(k, _)| *k == key).unwrap();
                let (_, members) = built.remove(index);
                result.push(cpp::ModuleMember::Namespace {
                    name: match key {
                        NsKey::Anonymous => None,
                        NsKey::Named(name) => Some(name),
                    },
                    members,
                });
            }
        }
    }

    result
}
