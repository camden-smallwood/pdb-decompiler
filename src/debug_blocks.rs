//! Dumps each procedure's unrolled scope tree (variable declarations nested in
//! lexical blocks) to a JSON file keyed by mangled function name.
//!
//! The scope tree is the same data that gets spliced into decompiled output as
//! commented `// debug:` blocks. Populating it requires `unroll_functions`, so
//! that flag is forced on whenever `--export-debug-blocks-json` is set.

use crate::cpp;
use std::{cell::RefCell, collections::HashMap, error::Error, fs::File, path::Path, rc::Rc};

/// Walks every module's procedures and writes a `{ mangled_name: block }`
/// mapping to `path`, where each block is `{ "variables": [...], "blocks": [...] }`.
pub fn export_debug_blocks_json(
    modules: &HashMap<String, Rc<RefCell<cpp::Module>>>,
    path: &Path,
) -> Result<(), Box<dyn Error>> {
    let mut map = serde_json::Map::new();

    for module in modules.values() {
        let module = module.borrow();

        for member in module.members.iter() {
            let cpp::ModuleMember::Procedure(procedure) = member else {
                continue;
            };

            let Some(body) = procedure.body.as_ref() else {
                continue;
            };

            // The mangled name lives in the module's public symbols, keyed by address.
            let Some((mangled_name, _)) = module
                .mangled_symbols
                .iter()
                .find(|(_, address)| *address == procedure.address)
            else {
                continue;
            };

            map.insert(mangled_name.clone(), block_to_json(body));
        }
    }

    let file = File::create(path)?;
    serde_json::to_writer_pretty(file, &serde_json::Value::Object(map))?;

    Ok(())
}

/// Converts a `cpp::Block` into an ordered array of tagged items mirroring the
/// C++ source structure: `[{ "variable": signature } | { "block": [...] }, ...]`.
/// Only variable declarations and nested blocks are kept, in source order
/// (comments, labels, and pseudocode are dropped).
fn block_to_json(block: &cpp::Block) -> serde_json::Value {
    let mut items = vec![];

    for statement in block.statements.iter() {
        collect_statement(statement, &mut items);
    }

    serde_json::Value::Array(items)
}

fn collect_statement(statement: &cpp::Statement, items: &mut Vec<serde_json::Value>) {
    match statement {
        cpp::Statement::Variable(variable) => {
            items.push(serde_json::json!({ "variable": variable.signature }));
        }

        cpp::Statement::Block(block) => {
            items.push(serde_json::json!({ "block": block_to_json(block) }));
        }

        // Unwrap commented wrappers so debug blocks spliced elsewhere still serialize.
        cpp::Statement::Commented(inner) => collect_statement(inner, items),

        _ => {}
    }
}
