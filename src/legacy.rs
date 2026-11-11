//! Metadata shared by pre-IPI and modern PDBs. Stream 4 is not reserved in old PDBs.

use pdb2::FallibleIterator;
use std::collections::BTreeMap;
use std::collections::HashMap;

pub type LineOffsets =
    HashMap<pdb2::StringRef, HashMap<pdb2::PdbInternalSectionOffset, pdb2::LineInfo>>;
pub type SourceLines = HashMap<String, HashMap<pdb2::PdbInternalSectionOffset, pdb2::LineInfo>>;

/// The line record at `offset`. Identical-code folding can give several files a record at the
/// same offset; the smallest file name wins so that output does not vary between runs.
pub fn line_at(lines: &SourceLines, offset: pdb2::PdbInternalSectionOffset) -> Option<(&String, &pdb2::LineInfo)> {
    lines.iter()
        .filter_map(|(file, offsets)| Some((file, offsets.get(&offset)?)))
        .min_by_key(|(file, _)| *file)
}

/// C11 stores file offsets, per-file segment ranges, then offset/line arrays.
/// Unlike C13, filenames are local to this table, independent of `/names`.
pub fn c11_lines(bytes: &[u8], pascal_names: bool) -> pdb2::Result<SourceLines> {
    let word = |offset| -> pdb2::Result<u16> {
        let value = bytes
            .get(offset..offset + 2)
            .ok_or(pdb2::Error::UnexpectedEof)?;
        Ok(u16::from_le_bytes(value.try_into().unwrap()))
    };
    let files = word(0)? as usize;
    let segments = word(2)? as usize;
    bytes
        .get(..4 + files * 4 + segments * 10)
        .ok_or(pdb2::Error::UnexpectedEof)?;
    let mut result = SourceLines::new();
    for file in 0..files {
        let base = u32_at(bytes, 4 + file * 4)? as usize;
        let count = word(base)? as usize;
        let ranges = base + 4 + count * 4;
        let name_start = ranges + count * 8;
        let tail = bytes.get(name_start..).ok_or(pdb2::Error::UnexpectedEof)?;
        let name = if pascal_names {
            let len = *tail.first().ok_or(pdb2::Error::UnexpectedEof)? as usize;
            tail.get(1..1 + len).ok_or(pdb2::Error::UnexpectedEof)?
        } else {
            let len = tail
                .iter()
                .position(|&b| b == 0)
                .ok_or(pdb2::Error::UnexpectedEof)?;
            &tail[..len]
        };
        let offsets = result
            .entry(String::from_utf8_lossy(name).into_owned())
            .or_default();
        for segment in 0..count {
            let table = u32_at(bytes, base + 4 + segment * 4)? as usize;
            let section = word(table)?;
            let lines = word(table + 2)? as usize;
            let numbers = table + 4 + lines * 4;
            bytes
                .get(numbers..numbers + lines * 2)
                .ok_or(pdb2::Error::UnexpectedEof)?;
            let end = u32_at(bytes, ranges + segment * 8 + 4)?;
            for i in 0..lines {
                let offset = u32_at(bytes, table + 4 + i * 4)?;
                let next = if i + 1 < lines {
                    Some(u32_at(bytes, table + 8 + i * 4)?)
                } else {
                    end.checked_add(1)
                };
                let key = pdb2::PdbInternalSectionOffset { section, offset };
                let line = word(numbers + i * 2)? as u32;
                offsets.entry(key).or_insert(pdb2::LineInfo {
                    offset: key,
                    length: next.and_then(|next| next.checked_sub(offset)),
                    file_index: pdb2::FileIndex(file as u32),
                    line_start: line,
                    line_end: line,
                    column_start: None,
                    column_end: None,
                    kind: pdb2::LineInfoKind::Statement,
                });
            }
        }
    }
    Ok(result)
}

/// XDK MASM keeps a local string subsection whose size excludes alignment padding.
/// The current pdb2 subsection iterator does not skip that padding.
pub fn c13_lines(bytes: &[u8]) -> pdb2::Result<LineOffsets> {
    let mut sections = Vec::new();
    let mut checksums = &[][..];
    let mut pos = 0;
    while pos < bytes.len() {
        let kind = u32_at(bytes, pos)?;
        let size = u32_at(bytes, pos + 4)? as usize;
        let end = pos + 8 + size;
        let data = bytes.get(pos + 8..end).ok_or(pdb2::Error::UnexpectedEof)?;
        match kind {
            0xf2 => sections.push(data),
            0xf4 => checksums = data,
            _ => {}
        }
        pos = (end + 3) & !3;
        if pos > bytes.len() {
            return Err(pdb2::Error::UnexpectedEof);
        }
    }
    let mut files = HashMap::new();
    pos = 0;
    while pos < checksums.len() {
        let name = pdb2::StringRef(u32_at(checksums, pos)?);
        let len = *checksums.get(pos + 4).ok_or(pdb2::Error::UnexpectedEof)? as usize;
        let end = (pos + 6 + len + 3) & !3;
        if end > checksums.len() {
            return Err(pdb2::Error::UnexpectedEof);
        }
        files.insert(pos as u32, name);
        pos = end;
    }
    let mut result = LineOffsets::new();
    for data in sections {
        let base = u32_at(data, 0)?;
        let flags = u32_at(data, 4)?;
        let section = flags as u16;
        let columns = flags & 0x10000 != 0;
        let code_size = u32_at(data, 8)?;
        pos = 12;
        while pos < data.len() {
            let file = u32_at(data, pos)?;
            let count = u32_at(data, pos + 4)? as usize;
            let size = u32_at(data, pos + 8)? as usize;
            if size < 12 + count * if columns { 12 } else { 8 } {
                return Err(pdb2::Error::UnexpectedEof);
            }
            let block = data
                .get(pos..pos + size)
                .ok_or(pdb2::Error::UnexpectedEof)?;
            let name = *files.get(&file).ok_or(pdb2::Error::UnexpectedEof)?;
            for i in 0..count {
                let offset = u32_at(block, 12 + i * 8)?;
                let line = u32_at(block, 16 + i * 8)?;
                let next = if i + 1 < count {
                    u32_at(block, 20 + i * 8)?
                } else {
                    code_size
                };
                let column = if columns {
                    Some(u32_at(block, 12 + count * 8 + i * 4)?)
                } else {
                    None
                };
                let offset_key = pdb2::PdbInternalSectionOffset {
                    section,
                    offset: base.checked_add(offset).ok_or(pdb2::Error::UnexpectedEof)?,
                };
                result
                    .entry(name)
                    .or_default()
                    .entry(offset_key)
                    .or_insert(pdb2::LineInfo {
                        offset: offset_key,
                        length: next.checked_sub(offset),
                        file_index: pdb2::FileIndex(file),
                        line_start: line & 0xffffff,
                        line_end: (line & 0xffffff) + ((line >> 24) & 0x7f),
                        column_start: column.map(|c| c & 0xffff),
                        column_end: column.map(|c| c >> 16),
                        kind: if line & 0x80000000 != 0 {
                            pdb2::LineInfoKind::Statement
                        } else {
                            pdb2::LineInfoKind::Expression
                        },
                    });
            }
            pos += size;
        }
    }
    Ok(result)
}

/// A module's line records by file name: C11 tables when present, otherwise C13 subsections.
pub fn module_source_lines(
    pdb: &mut pdb2::PDB<'_, std::fs::File>,
    module: &pdb2::Module,
    line_program: Option<&pdb2::LineProgram>,
    strings: Option<&pdb2::StringTable>,
) -> pdb2::Result<SourceLines> {
    let info = module.info();
    let start = info.symbols_size as usize;
    if info.lines_size > 0 {
        let stream = pdb.raw_stream(info.stream)?.ok_or(pdb2::Error::StreamNotFound(info.stream.0 as u32))?;
        return c11_lines(stream.as_slice().get(start..start + info.lines_size as usize).ok_or(pdb2::Error::UnexpectedEof)?, false);
    }
    let Some(strings) = strings else { return Ok(SourceLines::new()) };
    if let Some(program) = line_program {
        let mut result = SourceLines::new();
        for line in program.lines().collect::<Vec<_>>()? {
            let file = program.get_file_info(line.file_index)?;
            result.entry(file.name.to_string_lossy(strings)?.into_owned()).or_default().entry(line.offset).or_insert(line);
        }
        return Ok(result);
    }
    let stream = pdb.raw_stream(info.stream)?.ok_or(pdb2::Error::StreamNotFound(info.stream.0 as u32))?;
    c13_lines(stream.as_slice().get(start..start + info.c13_lines_size as usize).ok_or(pdb2::Error::UnexpectedEof)?)?
        .into_iter()
        .map(|(name, lines)| Ok((name.to_string_lossy(strings)?.into_owned(), lines)))
        .collect()
}

/// `pdb2` parses the fixed COMPILE2 fields but omits its trailing command block.
pub fn build_environment(symbol: &pdb2::Symbol<'_>) -> pdb2::Result<BTreeMap<String, String>> {
    environment_bytes(symbol.raw_kind(), symbol.raw_bytes())
}

fn environment_bytes(kind: u16, bytes: &[u8]) -> pdb2::Result<BTreeMap<String, String>> {
    let start = match kind {
        pdb2::S_COMPILE2 => {
            let tail = bytes.get(20..).ok_or(pdb2::Error::UnexpectedEof)?;
            21 + tail
                .iter()
                .position(|&b| b == 0)
                .ok_or(pdb2::Error::UnexpectedEof)?
        }
        pdb2::S_COMPILE2_ST => 21 + *bytes.get(20).ok_or(pdb2::Error::UnexpectedEof)? as usize,
        pdb2::S_ENVBLOCK => 3,
        _ => return Ok(BTreeMap::new()),
    };
    let mut tail = bytes.get(start..).ok_or(pdb2::Error::UnexpectedEof)?;
    let mut result = BTreeMap::new();
    while !tail.is_empty() && tail[0] != 0 {
        let end = tail
            .iter()
            .position(|&b| b == 0)
            .ok_or(pdb2::Error::UnexpectedEof)?;
        let key = String::from_utf8_lossy(&tail[..end]).into_owned();
        tail = &tail[end + 1..];
        let end = tail
            .iter()
            .position(|&b| b == 0)
            .ok_or(pdb2::Error::UnexpectedEof)?;
        result.insert(key, String::from_utf8_lossy(&tail[..end]).into_owned());
        tail = &tail[end + 1..];
    }
    Ok(result)
}

fn u32_at(bytes: &[u8], offset: usize) -> pdb2::Result<u32> {
    let bytes = bytes
        .get(offset..offset + 4)
        .ok_or(pdb2::Error::UnexpectedEof)?;
    Ok(u32::from_le_bytes(bytes.try_into().unwrap()))
}

/// The feature signatures follow the serialized named-stream map, not the header.
pub fn has_ipi(bytes: &[u8]) -> pdb2::Result<bool> {
    let mut pos = 32 + u32_at(bytes, 28)? as usize;
    let count = u32_at(bytes, pos)? as usize;
    pos += 8;
    for _ in 0..2 {
        let words = u32_at(bytes, pos)? as usize;
        pos += 4 + words * 4;
    }
    pos += count * 8;
    let features = bytes.get(pos..).ok_or(pdb2::Error::UnexpectedEof)?;
    if features.len() % 4 != 0 {
        return Err(pdb2::Error::UnexpectedEof);
    }
    Ok(features.as_chunks::<4>().0.iter().any(|x| {
        matches!(u32::from_le_bytes(*x), 20091201 | 20140508)
    }))
}

/// DBI file info contains its own strings; it does not depend on `/names`.
pub fn source_files(bytes: &[u8]) -> pdb2::Result<Vec<Vec<String>>> {
    let start = 64
        + u32_at(bytes, 24)? as usize
        + u32_at(bytes, 28)? as usize
        + u32_at(bytes, 32)? as usize;
    let size = u32_at(bytes, 36)? as usize;
    if size == 0 {
        return Ok(Vec::new());
    }
    let data = bytes
        .get(start..start + size)
        .ok_or(pdb2::Error::UnexpectedEof)?;
    let word = |offset| -> pdb2::Result<usize> {
        let x = data
            .get(offset..offset + 2)
            .ok_or(pdb2::Error::UnexpectedEof)?;
        Ok(u16::from_le_bytes(x.try_into().unwrap()) as usize)
    };
    let modules = word(0)?;
    // The 16-bit total can overflow. Sum the per-module counts instead.
    let counts = (0..modules)
        .map(|i| word(4 + modules * 2 + i * 2))
        .collect::<pdb2::Result<Vec<_>>>()?;
    let offsets = 4 + modules * 4;
    let names = offsets + counts.iter().sum::<usize>() * 4;
    let mut cursor = offsets;
    let mut result = Vec::new();
    for count in counts {
        let mut files = Vec::new();
        for _ in 0..count {
            let offset = names + u32_at(data, cursor)? as usize;
            cursor += 4;
            let tail = data.get(offset..).ok_or(pdb2::Error::UnexpectedEof)?;
            let end = tail
                .iter()
                .position(|&x| x == 0)
                .ok_or(pdb2::Error::UnexpectedEof)?;
            files.push(String::from_utf8_lossy(&tail[..end]).into_owned());
        }
        result.push(files);
    }
    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn c11_local_filenames_offsets_and_line_numbers() {
        for pascal in [false, true] {
            let mut bytes = vec![0; 64];
            bytes[0..2].copy_from_slice(&1u16.to_le_bytes());
            bytes[2..4].copy_from_slice(&1u16.to_le_bytes());
            bytes[4..8].copy_from_slice(&20u32.to_le_bytes());
            bytes[8..12].copy_from_slice(&0x100u32.to_le_bytes());
            bytes[12..16].copy_from_slice(&0x10fu32.to_le_bytes());
            bytes[16..18].copy_from_slice(&3u16.to_le_bytes());
            bytes[20..22].copy_from_slice(&1u16.to_le_bytes());
            bytes[24..28].copy_from_slice(&48u32.to_le_bytes());
            bytes[28..32].copy_from_slice(&0x100u32.to_le_bytes());
            bytes[32..36].copy_from_slice(&0x10fu32.to_le_bytes());
            bytes[36..45].copy_from_slice(if pascal {
                b"\x08test.cpp"
            } else {
                b"test.cpp\0"
            });
            bytes[48..50].copy_from_slice(&3u16.to_le_bytes());
            bytes[50..52].copy_from_slice(&2u16.to_le_bytes());
            bytes[52..56].copy_from_slice(&0x100u32.to_le_bytes());
            bytes[56..60].copy_from_slice(&0x108u32.to_le_bytes());
            bytes[60..62].copy_from_slice(&10u16.to_le_bytes());
            bytes[62..64].copy_from_slice(&20u16.to_le_bytes());
            let lines = c11_lines(&bytes, pascal).unwrap();
            let line = &lines["test.cpp"][&pdb2::PdbInternalSectionOffset {
                section: 3,
                offset: 0x108,
            }];
            assert_eq!(line.line_start, 20);
            assert_eq!(line.length, Some(8));
            bytes.pop();
            assert!(c11_lines(&bytes, pascal).is_err());
        }
    }

    #[test]
    fn masm_c13_skips_string_subsection_padding() {
        let mut bytes = Vec::new();
        // One line at offset 4 in a section contribution beginning at 100.
        for word in [0xf2u32, 32, 100, 3, 16, 0, 1, 20, 4, 0x8000002a] {
            bytes.extend(word.to_le_bytes());
        }
        bytes.extend(0xf3u32.to_le_bytes());
        bytes.extend(2u32.to_le_bytes());
        bytes.extend(b"x\0\0\0"); // two bytes of alignment, excluded from length
        for word in [0xf4u32, 8, 123, 0] {
            bytes.extend(word.to_le_bytes());
        }
        let lines = c13_lines(&bytes).unwrap();
        let line = &lines[&pdb2::StringRef(123)][&pdb2::PdbInternalSectionOffset {
            section: 3,
            offset: 104,
        }];
        assert_eq!(line.line_start, 42);
        assert_eq!(line.length, Some(12));
        bytes.pop();
        assert!(c13_lines(&bytes).is_err());
    }

    #[test]
    fn compile2_and_envblock_preserve_xbox_flags() {
        let commands = b"cwd\0Z:\\src\0cmd\0-QVMX128 -callcap\0src\0foo.cpp\0\0";
        let mut compile = vec![0; 20];
        compile.extend(b"XDK compiler\0");
        compile.extend(commands);
        let parsed = environment_bytes(pdb2::S_COMPILE2, &compile).unwrap();
        assert_eq!(parsed["cmd"], "-QVMX128 -callcap");
        assert_eq!(parsed["src"], "foo.cpp");
        let mut env = vec![0; 3];
        env.extend(commands);
        assert_eq!(parsed, environment_bytes(pdb2::S_ENVBLOCK, &env).unwrap());
        let mut st = vec![0; 20];
        st.extend(b"\x03old");
        st.extend(commands);
        assert_eq!(parsed, environment_bytes(pdb2::S_COMPILE2_ST, &st).unwrap());
        env.truncate(env.len() - 2);
        assert!(environment_bytes(pdb2::S_ENVBLOCK, &env).is_err());
    }

    #[test]
    fn ipi_requires_a_feature_signature_not_stream_four() {
        let mut info = vec![0; 48]; // empty names map and bit vectors
        assert!(!has_ipi(&info).unwrap());
        info.extend(20091201u32.to_le_bytes());
        assert!(has_ipi(&info).unwrap());
        info.truncate(48);
        info.extend(20140508u32.to_le_bytes());
        assert!(has_ipi(&info).unwrap());
        info.pop();
        assert!(has_ipi(&info).is_err());
    }

    #[test]
    fn dbi_sources_use_local_strings_and_actual_counts() {
        let mut dbi = vec![0; 64];
        let mut files = vec![];
        for word in [2u16, 0, 0, 1, 1, 1] {
            files.extend(word.to_le_bytes());
        }
        files.extend(0u32.to_le_bytes());
        files.extend(6u32.to_le_bytes());
        files.extend(b"a.cpp\0b.cpp\0");
        dbi[36..40].copy_from_slice(&(files.len() as u32).to_le_bytes());
        dbi.extend(files);
        assert_eq!(
            source_files(&dbi).unwrap(),
            vec![vec!["a.cpp"], vec!["b.cpp"]]
        );
        dbi.pop();
        assert!(source_files(&dbi).is_err());
    }
}
