//! Writing standard- and rapidjson-library types the way a person would, not the way a PDB does.
//!
//! A PDB records a type fully instantiated. `std::vector<std::string>` comes
//! back as `std::vector<std::string,std::allocator<std::string> >`, an
//! `unordered_map` arrives carrying its hash, its comparator and an allocator
//! over a pair nobody wrote, and every stream is a `basic_` template with its
//! traits spelled out. None of that is information: each argument is the
//! default the header would have supplied, and naming it changes nothing about
//! the type -- same layout, same mangled name, same code.
//!
//! Two passes, in this order: drop trailing arguments that match the default,
//! then rename what is left to the alias everyone writes. Dropping cannot be a
//! text substitution, because the default depends on the arguments before it --
//! `std::allocator<T>` may only come off a `vector<T>`, and a `map<K,V>`'s
//! allocator is over `std::pair<K const ,V>`.

/// How a defaultable argument is built from the ones that carry information.
enum Default {
    /// `std::allocator<A0>`, `std::less<A0>` and friends: one wrapper, first argument.
    OverFirst(&'static str),
    /// `std::allocator<std::pair<A0 const ,A1> >`, which is what a map's is.
    OverPair,
    /// A literal, for `std::dynamic_extent` and `std::ratio`'s denominator.
    Literal(&'static str),
}

/// `(template, arguments that carry information, defaults in order)`.
///
/// The spellings are the PDB's: no space after a comma, and `const` written
/// after the type, as in `std::pair<int const ,V>`.
const DEFAULTS: &[(&str, usize, &[Default])] = &[
    ("vector", 1, &[Default::OverFirst("std::allocator")]),
    ("deque", 1, &[Default::OverFirst("std::allocator")]),
    ("list", 1, &[Default::OverFirst("std::allocator")]),
    ("forward_list", 1, &[Default::OverFirst("std::allocator")]),
    ("set", 1, &[Default::OverFirst("std::less"), Default::OverFirst("std::allocator")]),
    ("multiset", 1, &[Default::OverFirst("std::less"), Default::OverFirst("std::allocator")]),
    ("map", 2, &[Default::OverFirst("std::less"), Default::OverPair]),
    ("multimap", 2, &[Default::OverFirst("std::less"), Default::OverPair]),
    ("unordered_set", 1, &[
        Default::OverFirst("std::hash"),
        Default::OverFirst("std::equal_to"),
        Default::OverFirst("std::allocator"),
    ]),
    ("unordered_multiset", 1, &[
        Default::OverFirst("std::hash"),
        Default::OverFirst("std::equal_to"),
        Default::OverFirst("std::allocator"),
    ]),
    ("unordered_map", 2, &[
        Default::OverFirst("std::hash"),
        Default::OverFirst("std::equal_to"),
        Default::OverPair,
    ]),
    ("unordered_multimap", 2, &[
        Default::OverFirst("std::hash"),
        Default::OverFirst("std::equal_to"),
        Default::OverPair,
    ]),
    ("queue", 1, &[Default::OverFirst("std::deque")]),
    ("stack", 1, &[Default::OverFirst("std::deque")]),
    ("priority_queue", 1, &[
        Default::OverFirst("std::vector"),
        Default::OverFirst("std::less"),
    ]),
    ("unique_ptr", 1, &[Default::OverFirst("std::default_delete")]),
    // Character types. Dropping the traits and the allocator leaves a single
    // argument, which `alias_for` then names.
    ("basic_string", 1, &[
        Default::OverFirst("std::char_traits"),
        Default::OverFirst("std::allocator"),
    ]),
    ("basic_string_view", 1, &[Default::OverFirst("std::char_traits")]),
    ("basic_stringstream", 1, &[
        Default::OverFirst("std::char_traits"),
        Default::OverFirst("std::allocator"),
    ]),
    ("basic_istringstream", 1, &[
        Default::OverFirst("std::char_traits"),
        Default::OverFirst("std::allocator"),
    ]),
    ("basic_ostringstream", 1, &[
        Default::OverFirst("std::char_traits"),
        Default::OverFirst("std::allocator"),
    ]),
    ("basic_stringbuf", 1, &[
        Default::OverFirst("std::char_traits"),
        Default::OverFirst("std::allocator"),
    ]),
    ("basic_ifstream", 1, &[Default::OverFirst("std::char_traits")]),
    ("basic_ofstream", 1, &[Default::OverFirst("std::char_traits")]),
    ("basic_fstream", 1, &[Default::OverFirst("std::char_traits")]),
    ("basic_filebuf", 1, &[Default::OverFirst("std::char_traits")]),
    ("basic_istream", 1, &[Default::OverFirst("std::char_traits")]),
    ("basic_ostream", 1, &[Default::OverFirst("std::char_traits")]),
    ("basic_iostream", 1, &[Default::OverFirst("std::char_traits")]),
    ("basic_streambuf", 1, &[Default::OverFirst("std::char_traits")]),
    ("basic_regex", 1, &[Default::OverFirst("std::regex_traits")]),
    // `std::dynamic_extent` is `SIZE_MAX`, which the PDB renders as the number.
    // This one is 32-bit because the target is.
    ("span", 1, &[Default::Literal("4294967295")]),
    ("ratio", 1, &[Default::Literal("1")]),
    // rapidjson. `Generic*` templates spell out their encoding and allocator,
    // and the writers spell out the default encodings, allocator and write
    // flags; dropping those leaves `alias_for` a name to reduce.
    ("PrettyWriter", 1, &[
        Default::Literal("rapidjson::UTF8<char>"),
        Default::Literal("rapidjson::UTF8<char>"),
        Default::Literal("rapidjson::CrtAllocator"),
        Default::Literal("0"),
    ]),
    ("Writer", 1, &[
        Default::Literal("rapidjson::UTF8<char>"),
        Default::Literal("rapidjson::UTF8<char>"),
        Default::Literal("rapidjson::CrtAllocator"),
        Default::Literal("0"),
    ]),
    ("GenericStringBuffer", 1, &[Default::Literal("rapidjson::CrtAllocator")]),
    ("GenericDocument", 1, &[
        Default::Literal("rapidjson::MemoryPoolAllocator<rapidjson::CrtAllocator>"),
        Default::Literal("rapidjson::CrtAllocator"),
    ]),
    ("GenericValue", 1, &[
        Default::Literal("rapidjson::MemoryPoolAllocator<rapidjson::CrtAllocator>"),
    ]),
];

/// The `basic_` templates that have a name of their own once reduced to one
/// argument. `std::basic_string<char>` is spelled `std::string` by everyone who
/// has ever written it.
const ALIAS_STEMS: &[&str] = &[
    "string", "string_view", "stringstream", "istringstream", "ostringstream",
    "stringbuf", "ifstream", "ofstream", "fstream", "filebuf", "istream",
    "ostream", "iostream", "streambuf",
];

fn alias_for(name: &str, argument: &str) -> Option<String> {
    // `std::basic_string<char>` is spelled `std::string` by everyone.
    if let Some(stem) = name.strip_prefix("basic_") {
        if !(ALIAS_STEMS.contains(&stem) || stem == "regex") {
            return None;
        }
        return match argument {
            "char" => Some(format!("std::{stem}")),
            "wchar_t" => Some(format!("std::w{stem}")),
            _ => None,
        };
    }
    // `rapidjson::GenericStringBuffer<UTF8<char>>` is `rapidjson::StringBuffer`,
    // and the same applies to `GenericDocument` and `GenericValue`.
    if let Some(short) = name.strip_prefix("Generic") {
        if !matches!(short, "StringBuffer" | "Document" | "Value") {
            return None;
        }
        return (argument == "rapidjson::UTF8<char>")
            .then(|| format!("rapidjson::{short}"));
    }
    // `PrettyWriter` is just `Writer` with indentation; write it as `Writer`.
    if name == "PrettyWriter" {
        return Some(format!("rapidjson::Writer<{argument}>"));
    }
    None
}

fn defaults_for(name: &str) -> Option<(usize, &'static [Default])> {
    DEFAULTS.iter().find(|(n, _, _)| *n == name).map(|(_, keep, d)| (*keep, *d))
}

/// Index just past the `>` closing the `<` at `open_at`, or `None`.
fn matching_close(text: &[u8], open_at: usize) -> Option<usize> {
    let mut depth = 0i32;
    for (i, ch) in text.iter().enumerate().skip(open_at) {
        match ch {
            b'<' => depth += 1,
            b'>' => {
                depth -= 1;
                if depth == 0 {
                    return Some(i);
                }
            }
            _ => (),
        }
    }
    None
}

/// The top-level template arguments of `text`, which excludes the brackets.
///
/// A comma inside a nested `<>` belongs to that nesting, and one inside `()`
/// belongs to a function type: `std::function<bool __cdecl(int,char)>` has one
/// argument, not two.
fn split_arguments(text: &str) -> Vec<&str> {
    let bytes = text.as_bytes();
    let (mut depth, mut parens, mut start) = (0i32, 0i32, 0usize);
    let mut args = Vec::new();
    for i in 0..bytes.len() {
        match bytes[i] {
            b'<' => depth += 1,
            b'>' => depth -= 1,
            b'(' => parens += 1,
            b')' => parens -= 1,
            b',' if depth == 0 && parens == 0 => {
                args.push(text[start..i].trim());
                start = i + 1;
            }
            _ => (),
        }
    }
    args.push(text[start..].trim());
    args
}

fn without_spaces(text: &str) -> String {
    text.chars().filter(|c| !c.is_whitespace()).collect()
}

fn expected(default: &Default, leading: &[&str]) -> String {
    match default {
        Default::OverFirst(wrapper) => format!("{wrapper}<{}>", leading[0]),
        Default::OverPair => {
            format!("std::allocator<std::pair<{} const ,{}> >", leading[0], leading[1])
        }
        Default::Literal(text) => (*text).to_string(),
    }
}

/// Where a known standard- or rapidjson-library template starts at or after
/// `from`, and its name.
fn next_template(text: &str, from: usize) -> Option<(usize, usize, &'static str)> {
    const NAMESPACES: &[&str] = &["std::", "rapidjson::"];

    let bytes = text.as_bytes();
    // A rapidjson template can nest inside a std one (and vice versa), so take
    // the leftmost match across every namespace rather than stopping at the
    // first namespace that has any match at all.
    let mut earliest: Option<(usize, usize, &'static str)> = None;
    for namespace in NAMESPACES {
        let mut at = from;
        while let Some(found) = text[at..].find(namespace) {
            let start = at + found;
            // A qualified name reaching further left is not ours to touch.
            let preceded = start > 0 && {
                let b = bytes[start - 1];
                b.is_ascii_alphanumeric() || b == b'_' || b == b':'
            };
            let rest = &text[start + namespace.len()..];
            let end = rest
                .find(|c: char| !(c.is_ascii_alphanumeric() || c == '_'))
                .unwrap_or(rest.len());
            if !preceded && rest.as_bytes().get(end) == Some(&b'<') {
                let name = &rest[..end];
                if let Some((known, _, _)) = DEFAULTS.iter().find(|(n, _, _)| *n == name) {
                    let candidate = (start, start + namespace.len() + end, *known);
                    if earliest.is_none_or(|(e, _, _)| start < e) {
                        earliest = Some(candidate);
                    }
                    break;
                }
            }
            at = start + namespace.len();
        }
    }
    earliest
}

/// Rewrite every standard-library template in `text` without the arguments that
/// are already its default, naming what is left where it has a name.
pub fn compact_std_templates(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    let mut at = 0usize;
    while let Some((start, open_at, name)) = next_template(text, at) {
        let Some(close) = matching_close(text.as_bytes(), open_at) else {
            out.push_str(&text[at..open_at + 1]);
            at = open_at + 1;
            continue;
        };
        let inner = compact_std_templates(&text[open_at + 1..close]);
        let mut args: Vec<String> =
            split_arguments(&inner).into_iter().map(str::to_string).collect();

        if let Some((keep, defaults)) = defaults_for(name)
            && args.len() > keep
        {
            // Owned, because the peeling below shortens `args`.
            let leading: Vec<String> = args[..keep].to_vec();
            // Peel from the right: an argument only comes off when every
            // argument after it has come off too.
            while args.len() > keep && args.len() - keep <= defaults.len() {
                let index = args.len() - 1;
                let leading: Vec<&str> = leading.iter().map(String::as_str).collect();
                let want = without_spaces(&expected(&defaults[index - keep], &leading));
                if without_spaces(&args[index]) != want {
                    break;
                }
                args.pop();
            }
        }

        out.push_str(&text[at..start]);
        if args.len() == 1
            && let Some(alias) = alias_for(name, &args[0])
        {
            out.push_str(&alias);
            at = close + 1;
            continue;
        }
        out.push_str(&text[start..open_at + 1]);
        let rendered = args.join(",");
        out.push_str(&rendered);
        // A trailing `>` still needs separating from the one that closes this.
        if rendered.ends_with('>') {
            out.push(' ');
        }
        out.push('>');
        at = close + 1;
    }
    out.push_str(&text[at..]);
    out
}

#[cfg(test)]
mod tests {
    use super::compact_std_templates as compact;

    #[test]
    fn drops_the_defaults() {
        assert_eq!(
            compact("std::vector<std::string,std::allocator<std::string> >"),
            "std::vector<std::string>"
        );
        assert_eq!(
            compact(
                "std::unordered_map<int,ChildInfo,std::hash<int>,std::equal_to<int>,\
                 std::allocator<std::pair<int const ,ChildInfo> > >"
            ),
            "std::unordered_map<int,ChildInfo>"
        );
    }

    #[test]
    fn keeps_what_is_not_the_default() {
        assert_eq!(compact("std::vector<int,MyAllocator>"), "std::vector<int,MyAllocator>");
        assert_eq!(
            compact(
                "std::unordered_map<int,V,MyHash,std::equal_to<int>,\
                 std::allocator<std::pair<int const ,V> > >"
            ),
            "std::unordered_map<int,V,MyHash>"
        );
    }

    #[test]
    fn names_what_has_a_name() {
        assert_eq!(
            compact("std::basic_stringstream<char,std::char_traits<char>,std::allocator<char> >"),
            "std::stringstream"
        );
        assert_eq!(
            compact("std::basic_ifstream<char,std::char_traits<char> >"),
            "std::ifstream"
        );
    }

    #[test]
    fn compacts_rapidjson_types() {
        assert_eq!(
            compact(
                "rapidjson::PrettyWriter<rapidjson::GenericStringBuffer<rapidjson::UTF8<char>,\
                 rapidjson::CrtAllocator>,rapidjson::UTF8<char>,rapidjson::UTF8<char>,\
                 rapidjson::CrtAllocator,0>"
            ),
            "rapidjson::Writer<rapidjson::StringBuffer>"
        );
        assert_eq!(
            compact(
                "rapidjson::Writer<rapidjson::GenericStringBuffer<rapidjson::UTF8<char>,\
                 rapidjson::CrtAllocator>,rapidjson::UTF8<char>,rapidjson::UTF8<char>,\
                 rapidjson::CrtAllocator,0>"
            ),
            "rapidjson::Writer<rapidjson::StringBuffer>"
        );
        assert_eq!(
            compact("rapidjson::GenericStringBuffer<rapidjson::UTF8<char>,rapidjson::CrtAllocator>"),
            "rapidjson::StringBuffer"
        );
        assert_eq!(
            compact(
                "rapidjson::GenericDocument<rapidjson::UTF8<char>,\
                 rapidjson::MemoryPoolAllocator<rapidjson::CrtAllocator>,rapidjson::CrtAllocator>"
            ),
            "rapidjson::Document"
        );
        assert_eq!(
            compact(
                "rapidjson::GenericValue<rapidjson::UTF8<char>,\
                 rapidjson::MemoryPoolAllocator<rapidjson::CrtAllocator> >"
            ),
            "rapidjson::Value"
        );
    }

    #[test]
    fn a_comma_inside_a_function_type_is_not_a_separator() {
        assert_eq!(
            compact(
                "std::function<bool __cdecl(int,std::vector<std::string,\
                 std::allocator<std::string> > const &)>"
            ),
            "std::function<bool __cdecl(int,std::vector<std::string> const &)>"
        );
    }
}
