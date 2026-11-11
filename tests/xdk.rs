use std::{path::Path, process::Command};

fn run(command: &mut Command) -> String {
    let output = command.output().expect("could not launch tool");
    let text = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(output.status.success(), "{text}");
    assert!(!text.contains("failed to process module"), "{text}");
    text
}

/// Generate a real PowerPC PDB with the licensed SDK instead of redistributing it.
#[test]
#[ignore = "requires XDK_ROOT pointing to an installed Xbox 360 XDK"]
fn xdk_without_ipi() {
    let sdk = std::env::var_os("XDK_ROOT").expect("set XDK_ROOT");
    let bin = Path::new(&sdk).join("XDK/bin/win32");
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let work = root
        .join("target")
        .join(format!("xdk-test-{}", std::process::id()));
    std::fs::create_dir_all(&work).unwrap();
    std::fs::copy(
        root.join("tests/fixtures/xdk.cpp"),
        work.join("legacy.cpp"),
    )
    .unwrap();
    run(Command::new(bin.join("cl.exe")).current_dir(&work).args([
        "/nologo",
        "/c",
        "/Z7",
        "/Od",
        "/GS-",
        "/Forenamed.obj",
        "legacy.cpp",
    ]));
    run(Command::new(bin.join("link.exe")).current_dir(&work).args([
        "/nologo",
        "/debug",
        "/nodefaultlib",
        "/entry:entry",
        "/subsystem:xbox",
        "/out:legacy.exe",
        "/pdb:legacy.pdb",
        "renamed.obj",
    ]));
    let invoke = || {
        run(Command::new(env!("CARGO_BIN_EXE_pdb-decompiler"))
            .current_dir(&work)
            .args([
                "--pdb",
                "legacy.pdb",
                "--out",
                "output",
                "--export-cpp",
                "--source-path",
                work.to_str().unwrap(),
                "--unroll-functions",
                "--export-debug-blocks-json",
                "scopes.json",
            ]))
    };
    invoke();
    // Only legacy.cpp's code uses Sample, so it is placed there, as declared. Mode was emitted
    // only as a forward reference, which stays in the shared inventory.
    let types = std::fs::read_to_string(work.join("output/exported.h")).unwrap();
    assert!(!types.contains("struct Sample"), "{types}");
    assert!(types.contains("enum Mode;"), "{types}");
    assert!(
        types.contains("typedef unsigned long LegacyWord"),
        "{types}"
    );
    let placements = std::fs::read_to_string(work.join("output/placements.json")).unwrap();
    assert!(placements.contains("\"type\": \"Sample\""), "{placements}");
    let source = std::fs::read_to_string(
        work.join("output")
            .join(work.file_name().unwrap())
            .join("legacy.cpp"),
    )
    .unwrap();
    assert_eq!(source.matches("struct Sample").count(), 1, "{source}");
    for expected in [
        "Build cmd:",
        "Build src: legacy.cpp",
        "__int32 value",
        "Sample::add(__int32 amount)",
        "global_count",
        "__int32 local",
    ] {
        assert!(source.contains(expected), "missing {expected}: {source}");
    }
    let scopes = std::fs::read_to_string(work.join("scopes.json")).unwrap();
    assert!(scopes.contains("__int32 local"), "{scopes}");
    assert!(scopes.contains("__int32 nested"), "{scopes}");
    invoke();
    assert_eq!(
        types,
        std::fs::read_to_string(work.join("output/exported.h")).unwrap()
    );

    // Remove both preferred source-name routes in the generated fixture. Equal-length
    // substitutions preserve MSF stream layout; DBI's independent filename list remains.
    let mut bytes = std::fs::read(work.join("legacy.pdb")).unwrap();
    for (old, new) in [
        (b"/names\0".as_slice(), b"/xxxxx\0".as_slice()),
        (b"src\0", b"xxx\0"),
    ] {
        let offsets = bytes
            .windows(old.len())
            .enumerate()
            .filter_map(|(i, window)| (window == old).then_some(i))
            .collect::<Vec<_>>();
        assert!(!offsets.is_empty());
        for offset in offsets {
            bytes[offset..offset + old.len()].copy_from_slice(new);
        }
    }
    std::fs::write(work.join("dbi-only.pdb"), bytes).unwrap();
    run(Command::new(env!("CARGO_BIN_EXE_pdb-decompiler"))
        .current_dir(&work)
        .args([
            "--pdb",
            "dbi-only.pdb",
            "--out",
            "dbi-only",
            "--export-cpp",
            "--source-path",
            work.to_str().unwrap(),
        ]));
    let recovered = std::fs::read_to_string(
        work.join("dbi-only")
            .join(work.file_name().unwrap())
            .join("legacy.cpp"),
    )
    .unwrap();
    assert!(
        recovered.contains("Sample::add(__int32 amount)"),
        "{recovered}"
    );
}
