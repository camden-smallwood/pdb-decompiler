//! Names an SDK's headers define at namespace scope (classes, structs, unions, enums, typedefs),
//! so a dump can include the SDK's own headers instead of reconstructing those types.
//!
//! Headers are scanned, not compiled: both branches of a conditional count, and the only macros
//! understood are the C++ library's namespace macros and the COM interface macros.

use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

#[derive(Default)]
pub struct Sdk {
    /// Qualified name -> defining header, relative to its include directory
    names: HashMap<String, PathBuf>,
}

impl Sdk {
    pub fn scan(include_dirs: &[PathBuf]) -> std::io::Result<Self> {
        let mut sdk = Self::default();

        for include_dir in include_dirs {
            let mut files = vec![];
            collect_headers(include_dir, &mut files)?;
            files.sort();

            for file in files {
                let text = String::from_utf8_lossy(&std::fs::read(&file)?).into_owned();
                let header = file.strip_prefix(include_dir).unwrap_or(&file).to_path_buf();

                // The first header in path order wins, so output doesn't depend on the file system
                for name in definitions(&text) {
                    sdk.names.entry(name).or_insert_with(|| header.clone());
                }
            }
        }

        Ok(sdk)
    }

    /// Whether the SDK defines this exact name
    pub fn defines(&self, name: &str) -> bool {
        self.names.contains_key(name)
    }

    /// The header defining a type, found by its outermost type the SDK defines, so a nested
    /// type or a template instantiation is found through the type or template it belongs to
    pub fn header(&self, name: &str) -> Option<&Path> {
        let mut prefix = String::new();

        for component in crate::namespaces::split_qualified(name) {
            if !prefix.is_empty() {
                prefix.push_str("::");
            }

            prefix.push_str(component.split('<').next().unwrap_or(&component));

            if let Some(header) = self.names.get(&prefix) {
                return Some(header);
            }
        }

        None
    }
}

/// Headers have no extension (the C++ library's) or a header extension
fn collect_headers(dir: &Path, files: &mut Vec<PathBuf>) -> std::io::Result<()> {
    for entry in std::fs::read_dir(dir)? {
        let path = entry?.path();

        if path.is_dir() {
            collect_headers(&path, files)?;
        } else if path.extension().and_then(|e| e.to_str()).is_none_or(|e| ["h", "hh", "hpp", "hxx"].iter().any(|x| e.eq_ignore_ascii_case(x))) {
            files.push(path);
        }
    }

    Ok(())
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Token<'a> {
    Identifier(&'a str),
    Punctuation(char),
    Literal,
}

/// Identifiers, punctuation and literals, without comments or preprocessor directives
fn tokens(text: &str) -> Vec<Token<'_>> {
    let bytes = text.as_bytes();
    let mut tokens = vec![];
    let mut i = 0;
    let mut line_start = true;

    while i < bytes.len() {
        let c = bytes[i];

        if c == b'\n' {
            line_start = true;
            i += 1;
        } else if c.is_ascii_whitespace() {
            i += 1;
        } else if c == b'#' && line_start {
            // A directive runs to the end of the line, including escaped line breaks
            while i < bytes.len() && bytes[i] != b'\n' {
                if bytes[i] == b'/' && bytes.get(i + 1) == Some(&b'*') {
                    i = text[i + 2..].find("*/").map_or(bytes.len(), |end| i + 2 + end + 2);
                    continue;
                }
                i += if bytes[i] == b'\\' { 2 } else { 1 };
            }
        } else if c == b'/' && bytes.get(i + 1) == Some(&b'/') {
            i = text[i..].find('\n').map_or(bytes.len(), |end| i + end);
        } else if c == b'/' && bytes.get(i + 1) == Some(&b'*') {
            i = text[i + 2..].find("*/").map_or(bytes.len(), |end| i + 2 + end + 2);
        } else if c == b'"' || c == b'\'' {
            i += 1;
            while i < bytes.len() && bytes[i] != c && bytes[i] != b'\n' {
                i += if bytes[i] == b'\\' { 2 } else { 1 };
            }
            i += 1;
            tokens.push(Token::Literal);
            line_start = false;
        } else if c.is_ascii_alphabetic() || c == b'_' {
            let start = i;
            while i < bytes.len() && (bytes[i].is_ascii_alphanumeric() || bytes[i] == b'_') {
                i += 1;
            }
            tokens.push(Token::Identifier(&text[start..i]));
            line_start = false;
        } else if c.is_ascii_digit() {
            while i < bytes.len() && (bytes[i].is_ascii_alphanumeric() || bytes[i] == b'.') {
                i += 1;
            }
            tokens.push(Token::Literal);
            line_start = false;
        } else {
            // Multi-byte characters only occur in comments and literals in practice
            let width = text[i..].chars().next().map_or(1, char::len_utf8);
            tokens.push(Token::Punctuation(text[i..].chars().next().unwrap_or(' ')));
            i += width;
            line_start = false;
        }
    }

    tokens
}

enum Scope {
    Namespace(String),
    /// `extern "C" { }`, whose names are in the enclosing namespace
    Transparent,
    /// A class body; a typedef's declarators follow its closing brace
    Class { typedef: bool },
    Other,
}

fn is_class_key(token: Token) -> bool {
    matches!(token, Token::Identifier("class" | "struct" | "union" | "enum"))
}

/// The index after the group opened at `i`, matching `open` with `close`
fn skip_group(tokens: &[Token], mut i: usize, open: char, close: char) -> usize {
    let mut depth = 0;

    while i < tokens.len() {
        match tokens[i] {
            Token::Punctuation(c) if c == open => depth += 1,
            Token::Punctuation(c) if c == close => {
                depth -= 1;
                if depth == 0 {
                    return i + 1;
                }
            }
            _ => {}
        }
        i += 1;
    }

    i
}

/// Typedef names among declarators ending at the next `;`: a name directly before `,`, `;` or
/// `[`, or a function pointer's (`(*name)`)
fn declarators(tokens: &[Token], mut i: usize, names: &mut Vec<String>) -> usize {
    let mut depth = 0;
    let mut pointer_name = false;

    while i < tokens.len() {
        match tokens[i] {
            Token::Punctuation(';') if depth == 0 => return i + 1,
            Token::Punctuation('(') => depth += 1,
            Token::Punctuation(')') => depth -= 1,
            Token::Punctuation('{' | '}') => return i,
            Token::Identifier(name) => {
                let next = tokens.get(i + 1).copied();
                let previous = i.checked_sub(1).map(|p| tokens[p]);

                if depth == 0 && matches!(next, Some(Token::Punctuation(',' | ';' | '['))) {
                    names.push(name.to_string());
                } else if depth == 1 && !pointer_name && previous == Some(Token::Punctuation('*')) && next == Some(Token::Punctuation(')')) {
                    names.push(name.to_string());
                    pointer_name = true;
                }
            }
            _ => {}
        }
        i += 1;
    }

    i
}

/// Qualified names the text defines at namespace scope
fn definitions(text: &str) -> Vec<String> {
    let tokens = tokens(text);
    let mut names = vec![];
    let mut stack = vec![];
    let mut i = 0;

    let at_namespace_scope = |stack: &[Scope]| stack.iter().all(|s| matches!(s, Scope::Namespace(_) | Scope::Transparent));
    let qualify = |stack: &[Scope], name: &str| {
        let mut parts = stack.iter().filter_map(|s| match s {
            Scope::Namespace(namespace) => Some(namespace.as_str()),
            _ => None,
        }).collect::<Vec<_>>();
        parts.push(name);
        parts.join("::")
    };

    while i < tokens.len() {
        let next = |n: usize| tokens.get(i + n).copied();

        match tokens[i] {
            Token::Identifier("namespace") => match (next(1), next(2)) {
                (Some(Token::Identifier(name)), Some(Token::Punctuation('{'))) => {
                    stack.push(Scope::Namespace(name.to_string()));
                    i += 3;
                    continue;
                }
                _ => {}
            },

            Token::Identifier("_STD_BEGIN") => stack.push(Scope::Namespace("std".into())),
            Token::Identifier("_STDEXT_BEGIN") => stack.push(Scope::Namespace("stdext".into())),
            Token::Identifier("_STD_END" | "_STDEXT_END") => {
                stack.pop();
            }

            Token::Identifier("extern") if next(1) == Some(Token::Literal) && next(2) == Some(Token::Punctuation('{')) => {
                stack.push(Scope::Transparent);
                i += 3;
                continue;
            }

            // DECLARE_INTERFACE(name) and DECLARE_INTERFACE_(name, base) open an interface's body
            Token::Identifier("DECLARE_INTERFACE" | "DECLARE_INTERFACE_") if next(1) == Some(Token::Punctuation('(')) => {
                if let Some(Token::Identifier(name)) = next(2) && at_namespace_scope(&stack) {
                    names.push(qualify(&stack, name));
                }
                let end = skip_group(&tokens, i + 1, '(', ')');
                if tokens.get(end) == Some(&Token::Punctuation('{')) {
                    stack.push(Scope::Class { typedef: false });
                    i = end + 1;
                } else {
                    i = end;
                }
                continue;
            }

            Token::Identifier("typedef") if !next(1).is_some_and(is_class_key) => {
                let mut declared = vec![];
                i = declarators(&tokens, i + 1, &mut declared);
                if at_namespace_scope(&stack) {
                    names.extend(declared.iter().map(|name| qualify(&stack, name)));
                }
                continue;
            }

            token if is_class_key(token) || (token == Token::Identifier("typedef") && next(1).is_some_and(is_class_key)) => {
                let typedef = token == Token::Identifier("typedef");
                let mut j = i + if typedef { 2 } else { 1 };

                // `enum class`, and attribute macros, possibly with arguments, before the name
                let mut name = None;
                while let Some(Token::Identifier(identifier)) = tokens.get(j).copied() {
                    match tokens.get(j + 1) {
                        Some(Token::Punctuation('(')) => j = skip_group(&tokens, j + 1, '(', ')'),
                        Some(Token::Identifier(_)) => j += 1,
                        _ => {
                            name = Some(identifier);
                            j += 1;
                            break;
                        }
                    }
                }

                if tokens.get(j) == Some(&Token::Punctuation('<')) && name.is_some() {
                    j = skip_group(&tokens, j, '<', '>');
                }

                while matches!(tokens.get(j), Some(Token::Identifier("final" | "sealed"))) {
                    j += 1;
                }

                // A base clause runs to the body; anything else ends the declaration
                if tokens.get(j) == Some(&Token::Punctuation(':')) {
                    while j < tokens.len() && !matches!(tokens[j], Token::Punctuation('{' | ';' | ')')) {
                        j += 1;
                    }
                }

                if tokens.get(j) == Some(&Token::Punctuation('{')) {
                    if let Some(name) = name && at_namespace_scope(&stack) {
                        names.push(qualify(&stack, name));
                    }
                    stack.push(Scope::Class { typedef });
                    i = j + 1;
                    continue;
                }

                if typedef {
                    // `typedef struct tag *name;`
                    let mut declared = vec![];
                    i = declarators(&tokens, i + 1, &mut declared);
                    if at_namespace_scope(&stack) {
                        names.extend(declared.iter().map(|name| qualify(&stack, name)));
                    }
                    continue;
                }
            }

            Token::Punctuation('{') => stack.push(Scope::Other),

            Token::Punctuation('}') => {
                if let Some(Scope::Class { typedef: true }) = stack.pop() {
                    let mut declared = vec![];
                    i = declarators(&tokens, i + 1, &mut declared);
                    if at_namespace_scope(&stack) {
                        names.extend(declared.iter().map(|name| qualify(&stack, name)));
                    }
                    continue;
                }
            }

            _ => {}
        }

        i += 1;
    }

    names
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn finds_namespace_scope_definitions() {
        let text = r#"
            #define _STD_BEGIN namespace std {
            #pragma once // { not a scope
            extern "C" {
            typedef enum _D3DZBUFFERTYPE { D3DZB_FALSE = 0 } D3DZBUFFERTYPE, *PD3DZBUFFERTYPE;
            typedef struct _X *PX;
            typedef unsigned long DWORD;
            typedef void (__stdcall *PFN_CALLBACK)(void *pContext);
            struct DECLSPEC_NOVTABLE Plain : public Base { struct Nested { }; int f() { return 0; } };
            }
            _STD_BEGIN
            template<class _Ty, class _Alloc = allocator<_Ty> > class vector : public _Vector_val<_Ty, _Alloc> {
                friend class _Tree<_Ty>;
            };
            class ios_base;
            _STD_END
            namespace XGRAPHICS { struct VerOpList { }; }
            DECLARE_INTERFACE_(IDirect3DDevice9, IUnknown) { STDMETHOD(Reset)(THIS) PURE; };
            struct Name *function(void) { return 0; }
        "#;

        let mut names = definitions(text);
        names.sort();

        assert_eq!(names, [
            "D3DZBUFFERTYPE", "DWORD", "IDirect3DDevice9", "PD3DZBUFFERTYPE", "PFN_CALLBACK", "PX",
            "Plain", "XGRAPHICS::VerOpList", "_D3DZBUFFERTYPE", "std::vector",
        ]);
    }

    #[test]
    fn finds_types_through_their_outermost_type() {
        let mut sdk = Sdk::default();
        sdk.names.insert("std::vector".into(), PathBuf::from("vector"));
        sdk.names.insert("std::ios_base".into(), PathBuf::from("xiosbase"));

        assert_eq!(sdk.header("std::vector<int,std::allocator<int> >"), Some(Path::new("vector")));
        assert_eq!(sdk.header("std::ios_base::Init"), Some(Path::new("xiosbase")));
        assert_eq!(sdk.header("Game::vector"), None);
        assert!(sdk.defines("std::vector") && !sdk.defines("std::vector<int>"));
    }
}
