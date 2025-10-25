
#[inline(always)]
pub fn sanitize_path<S: AsRef<str>>(path: S) -> String {
    let mut result = path.as_ref().to_string().replace('\\', "/");

    if let Some((_, rest)) = result.split_once(':') {
        result = rest.to_string();
    }

    result
}

#[inline(always)]
pub fn canonicalize_path(out_path: &str, root_path: &str, path: &str, is_directory: bool) -> std::path::PathBuf {
    let path = std::path::PathBuf::from(format!(
        "{}/{}{}",
        out_path,
        if path.contains(':') {
            sanitize_path(path)
        } else {
            format!("{}/{}", sanitize_path(root_path), sanitize_path(path))
        },
        if is_directory { "/" } else { "" },
    ));

    if !path.exists() {
        if is_directory {
            if !path.exists() && let Err(e) = std::fs::create_dir_all(path.clone()) {
                panic!("Failed to create directory \"{}\": {e}", path.to_string_lossy());
            }
        } else {
            if let Some(parent_path) = path.parent()
                && let Err(e) = std::fs::create_dir_all(parent_path)
            {
                panic!(
                    "Failed to create parent directories for file \"{}\": {e}",
                    path.to_string_lossy(),
                );
            }
    
            if let Err(e) = std::fs::File::create(path.clone()) {
                panic!("Failed to create file \"{}\": {e}", path.to_string_lossy());
            }
        }
    }

    match path.canonicalize() {
        Ok(x) => x.to_string_lossy().replace(out_path, "").trim_start_matches("\\\\?\\").into(),
        Err(e) => panic!("Failed to canonicalize path \"{}\": {e}", path.to_string_lossy())
    }
}
