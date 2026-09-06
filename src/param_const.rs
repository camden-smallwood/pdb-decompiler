//! Recovery of top-level `const` on pointer parameters from the mangled name.
//!
//! MSVC stores a parameter's type by its type record, but a pointer-to-primitive
//! (e.g. `long *`) is a builtin "primitive with indirection" that has no room for a
//! cv-qualifier, so a source `long *const p` collapses to the same record as
//! `long *p` — the top-level `const` is lost from the type system. It survives only
//! in the mangled name (`Q`/`R`/`S` vs `P`). We demangle the public symbol and
//! transfer that top-level `const` back onto the exporter's rendered signature,
//! aligning parameters by position.
//!
//! (Top-level `const` on a *value* parameter — `int const` — is dropped by MSVC
//! everywhere, including the mangled name, and is genuinely unrecoverable.)

/// Split the balanced `(...)` parameter list out of a signature or demangled name,
/// returning the top-level, comma-separated parameters. `None` if no param list.
fn split_param_list(text: &str) -> Option<(usize, usize, Vec<String>)> {
    let chars: Vec<char> = text.chars().collect();

    // The parameter list is the last balanced top-level (...) group.
    let mut depth = 0i32;
    let mut open = None;
    let mut best: Option<(usize, usize)> = None;
    for (i, &c) in chars.iter().enumerate() {
        match c {
            '(' => { if depth == 0 { open = Some(i); } depth += 1; }
            ')' => { depth -= 1; if depth == 0 { if let Some(o) = open.take() { best = Some((o, i)); } } }
            '<' | '[' => depth += 1,
            '>' | ']' => depth -= 1,
            _ => {}
        }
    }

    let (open, close) = best?;
    let inner: String = chars[open + 1..close].iter().collect();

    let trimmed = inner.trim();
    if trimmed.is_empty() || trimmed == "void" {
        return Some((open, close, vec![]));
    }

    let mut params = vec![];
    let mut cur = String::new();
    let mut d = 0i32;
    for c in inner.chars() {
        match c {
            '<' | '(' | '[' => { d += 1; cur.push(c); }
            '>' | ')' | ']' => { d -= 1; cur.push(c); }
            ',' if d == 0 => params.push(std::mem::take(&mut cur)),
            _ => cur.push(c),
        }
    }
    params.push(cur);

    Some((open, close, params))
}

/// Byte index (into `param`) of the rightmost top-level `*` (the one that forms the
/// outermost pointer), or `None` if the parameter isn't a top-level pointer.
fn rightmost_top_level_star(param: &str) -> Option<usize> {
    let mut depth = 0i32;
    let mut last = None;
    for (i, c) in param.char_indices() {
        match c {
            '<' | '(' | '[' => depth += 1,
            '>' | ')' | ']' => depth -= 1,
            '*' if depth == 0 => last = Some(i),
            _ => {}
        }
    }
    last
}

/// Whether a parameter's outermost pointer is `const` (top-level const): the
/// rightmost top-level `*` is immediately followed (ignoring spaces) by `const`.
fn has_top_level_const(param: &str) -> bool {
    let Some(star) = rightmost_top_level_star(param) else { return false };
    param[star + 1..].trim_start().starts_with("const")
}

/// Insert `const` after the rightmost top-level `*` of a pointer parameter.
/// No-op if it isn't a top-level pointer or is already const.
fn insert_top_level_const(param: &str) -> String {
    if has_top_level_const(param) {
        return param.to_string();
    }
    let Some(star) = rightmost_top_level_star(param) else { return param.to_string() };

    let (head, tail) = param.split_at(star + 1); // head ends with '*'
    // `long *name` -> `long *const name`; `void **name` -> `void **const name`.
    format!("{head}const {}", tail.trim_start())
}

/// Given the demangled name, return whether each positional parameter has a
/// top-level-const outermost pointer. `None` if the name has no parameter list.
fn demangled_param_consts(demangled: &str) -> Option<Vec<bool>> {
    let (_, _, params) = split_param_list(demangled)?;
    Some(params.iter().map(|p| has_top_level_const(p)).collect())
}

/// Patch a rendered signature so pointer parameters that are top-level `const` in
/// the mangled name (but not in the exporter's type-record rendering) gain the
/// qualifier. Returns the signature unchanged when there is nothing to do, when the
/// name fails to demangle, or when parameter counts don't align.
pub fn patch_signature(signature: &str, mangled: &str) -> String {
    let flags = msvc_demangler::DemangleFlags::llvm();
    let Ok(demangled) = msvc_demangler::demangle(mangled, flags) else {
        return signature.to_string();
    };

    let Some(wanted) = demangled_param_consts(&demangled) else {
        return signature.to_string();
    };

    // Nothing to add.
    if !wanted.iter().any(|&c| c) {
        return signature.to_string();
    }

    let Some((open, close, params)) = split_param_list(signature) else {
        return signature.to_string();
    };

    // Only trust a positional match when the counts line up.
    if params.len() != wanted.len() {
        return signature.to_string();
    }

    let patched: Vec<String> = params
        .iter()
        .zip(wanted.iter())
        .map(|(p, &want_const)| {
            if want_const {
                // Preserve leading spacing from the original comma-split.
                let leading: String = p.chars().take_while(|c| c.is_whitespace()).collect();
                format!("{leading}{}", insert_top_level_const(p.trim_start()))
            } else {
                p.clone()
            }
        })
        .collect();

    let chars: Vec<char> = signature.chars().collect();
    let prefix: String = chars[..open + 1].iter().collect();
    let suffix: String = chars[close..].iter().collect();
    format!("{prefix}{}{suffix}", patched.join(","))
}
