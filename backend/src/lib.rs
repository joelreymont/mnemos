use git::info_for_file;
use index as idx;
use notes::{self, NoteFilters};
use rpc::{Request, Response, INTERNAL_ERROR, INVALID_PARAMS, METHOD_NOT_FOUND, PARSE_ERROR};
use rusqlite::Connection;
use serde::{Deserialize, Serialize};
use serde_json::json;
use std::fs;
use std::path::Path;
use treesitter::{
    compute_display_position, compute_hash_at_position, compute_node_path, default_config,
    GrammarRegistry, ParserService,
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FileInfo {
    pub file: String,
    pub size: u64,
}

pub mod preload;
pub mod server;
pub mod snapshot;
pub mod version;

const IGNORE_DIRS: &[&str] = &[
    ".git",
    "target",
    "node_modules",
    ".hg",
    ".svn",
    ".idea",
    ".direnv",
    "venv",
    "env",
    "__pycache__",
    "build",
];

fn list_files(root: &Path) -> anyhow::Result<Vec<FileInfo>> {
    let mut stack = vec![root.to_path_buf()];
    let mut files = Vec::new();
    while let Some(dir) = stack.pop() {
        for entry in fs::read_dir(&dir)? {
            let entry = entry?;
            let path = entry.path();
            let name = entry.file_name().to_string_lossy().to_string();
            if entry.file_type()?.is_dir() {
                if IGNORE_DIRS.contains(&name.as_str()) {
                    continue;
                }
                stack.push(path);
            } else if entry.file_type()?.is_file() {
                let size = entry.metadata().map(|m| m.len()).unwrap_or(0);
                files.push(FileInfo {
                    file: path.to_string_lossy().to_string(),
                    size,
                });
            }
        }
    }
    files.sort_by(|a, b| a.file.cmp(&b.file));
    Ok(files)
}

pub fn handle(req: Request, db: &Connection, parser: &mut ParserService) -> Response {
    let id = req.id.clone();
    match req.method.as_str() {
        "hemis/list-files" => {
            if let Some(root) = req.params.get("projectRoot").and_then(|v| v.as_str()) {
                match list_files(Path::new(root)) {
                    Ok(files) => Response::result(id, serde_json::to_value(files).unwrap()),
                    Err(e) => Response::error(id, INTERNAL_ERROR, e.to_string()),
                }
            } else {
                Response::error(id, INVALID_PARAMS, "missing projectRoot")
            }
        }
        "hemis/get-file" => {
            if let Some(path) = req.params.get("file").and_then(|v| v.as_str()) {
                match fs::read_to_string(path) {
                    Ok(content) => {
                        let resp = json!({
                            "file": path,
                            "content": content,
                        });
                        Response::result(id, resp)
                    }
                    Err(e) => Response::error(id, INTERNAL_ERROR, e.to_string()),
                }
            } else {
                Response::error(id, INVALID_PARAMS, "missing file")
            }
        }
        "hemis/open-project" => {
            if req
                .params
                .get("projectRoot")
                .and_then(|v| v.as_str())
                .is_some()
            {
                Response::result(id, json!({"ok": true}))
            } else {
                Response::error(id, INVALID_PARAMS, "missing projectRoot")
            }
        }
        "hemis/explain-region" => {
            let file = req.params.get("file").and_then(|v| v.as_str());
            // Accept both nested format (start.line) and flat format (startLine)
            let start_line = req
                .params
                .get("start")
                .and_then(|s| s.get("line"))
                .and_then(|v| v.as_u64())
                .or_else(|| req.params.get("startLine").and_then(|v| v.as_u64()))
                .unwrap_or(1) as usize;
            let end_line = req
                .params
                .get("end")
                .and_then(|s| s.get("line"))
                .and_then(|v| v.as_u64())
                .or_else(|| req.params.get("endLine").and_then(|v| v.as_u64()))
                .unwrap_or(start_line as u64) as usize;
            if let Some(file) = file {
                match fs::read_to_string(file) {
                    Ok(content) => {
                        let snippet: String = content
                            .lines()
                            .enumerate()
                            .filter(|(idx, _)| {
                                let line_no = idx + 1;
                                line_no >= start_line && line_no <= end_line
                            })
                            .map(|(_, l)| l)
                            .collect::<Vec<_>>()
                            .join("\n");
                        let resp = if snippet.is_empty() {
                            json!({"content": "", "references": []})
                        } else {
                            json!({"content": snippet, "references": []})
                        };
                        Response::result(id, resp)
                    }
                    Err(e) => Response::error(id, INTERNAL_ERROR, e.to_string()),
                }
            } else {
                Response::error(id, INVALID_PARAMS, "missing file")
            }
        }
        "hemis/search" => {
            let query = req
                .params
                .get("query")
                .and_then(|v| v.as_str())
                .unwrap_or("");
            let proj = req.params.get("projectRoot").and_then(|v| v.as_str());
            let include_notes = req
                .params
                .get("includeNotes")
                .and_then(|v| v.as_bool())
                .unwrap_or(true);
            let query_vec: Option<Vec<f32>> = req
                .params
                .get("vector")
                .and_then(|v| v.as_array())
                .map(|arr| {
                    arr.iter()
                        .filter_map(|x| x.as_f64().map(|f| f as f32))
                        .collect::<Vec<f32>>()
                });
            // Reject empty queries to avoid returning all indexed content
            if query.trim().is_empty() && query_vec.is_none() {
                return Response::result(id, json!([]));
            }
            match idx::search(db, query, proj) {
                Ok(mut results) => {
                    // If a query vector is provided, blend semantic hits.
                    if let Some(vec) = query_vec {
                        if let Ok(mut semantic_hits) = idx::semantic_search(db, &vec, proj, 5) {
                            results.append(&mut semantic_hits);
                        }
                    }
                    if include_notes {
                        if let Ok(notes) = notes::search(db, query, proj, None, 0) {
                            for n in notes {
                                results.push(idx::SearchHit {
                                    file: n.file.clone(),
                                    line: n.line as usize,
                                    column: n.column as usize,
                                    text: n.summary.clone(),
                                    score: 0.5,
                                    kind: Some("note".into()),
                                    note_id: Some(n.id.clone()),
                                    note_summary: Some(n.summary.clone()),
                                });
                            }
                        }
                    }
                    Response::result(id, serde_json::to_value(results).unwrap())
                }
                Err(e) => Response::error(id, INTERNAL_ERROR, e.to_string()),
            }
        }
        "hemis/index-project" => {
            if let Some(root) = req.params.get("projectRoot").and_then(|v| v.as_str()) {
                let mut indexed = 0;
                let mut skipped = 0;
                match list_files(Path::new(root)) {
                    Ok(files) => {
                        for f in files {
                            if let Ok(content) = fs::read_to_string(&f.file) {
                                match idx::add_file(db, &f.file, root, &content) {
                                    Ok(Some(_)) => indexed += 1,
                                    Ok(None) => skipped += 1, // unchanged
                                    Err(e) => eprintln!("index failed for {}: {}", f.file, e),
                                }
                            }
                        }
                        Response::result(
                            id,
                            json!({"ok": true, "indexed": indexed, "skipped": skipped, "projectRoot": root}),
                        )
                    }
                    Err(e) => Response::error(id, INTERNAL_ERROR, e.to_string()),
                }
            } else {
                Response::error(id, INVALID_PARAMS, "missing projectRoot")
            }
        }
        "hemis/save-snapshot" => {
            if let Some(path) = req.params.get("path").and_then(|v| v.as_str()) {
                let project_root = req.params.get("projectRoot").and_then(|v| v.as_str());
                match snapshot::create(db, project_root).and_then(|payload| {
                    fs::write(path, serde_json::to_vec_pretty(&payload).unwrap())
                        .map(|_| payload.clone())
                        .map_err(|e| anyhow::anyhow!(e))
                }) {
                    Ok(payload) => Response::result(
                        id,
                        json!({"ok": true, "path": path, "counts": payload.get("counts")}),
                    ),
                    Err(e) => Response::error(id, INTERNAL_ERROR, e.to_string()),
                }
            } else {
                Response::error(id, INVALID_PARAMS, "missing path")
            }
        }
        "hemis/load-snapshot" => {
            if let Some(path) = req.params.get("path").and_then(|v| v.as_str()) {
                match fs::read_to_string(path) {
                    Ok(contents) => match serde_json::from_str::<serde_json::Value>(&contents) {
                        Ok(val) => match snapshot::restore(db, &val) {
                            Ok(status) => Response::result(id, status),
                            Err(e) => Response::error(id, INTERNAL_ERROR, e.to_string()),
                        },
                        Err(e) => Response::error(id, INTERNAL_ERROR, e.to_string()),
                    },
                    Err(e) => Response::error(id, INTERNAL_ERROR, e.to_string()),
                }
            } else {
                Response::error(id, INVALID_PARAMS, "missing path")
            }
        }
        "hemis/status" => {
            let proj = req.params.get("projectRoot").and_then(|v| v.as_str());
            match storage::counts(db, proj) {
                Ok(c) => Response::result(
                    id,
                    json!({
                        "ok": true,
                        "projectRoot": proj,
                        "counts": {
                            "notes": c.notes,
                            "files": c.files,
                            "embeddings": c.embeddings,
                            "edges": c.edges
                        }
                    }),
                ),
                Err(e) => Response::error(id, INTERNAL_ERROR, e.to_string()),
            }
        }
        "notes/list-for-file" => {
            let file = req.params.get("file").and_then(|v| v.as_str());
            let proj = req.params.get("projectRoot").and_then(|v| v.as_str());
            if let (Some(file), Some(proj)) = (file, proj) {
                let commit = req.params.get("commit").and_then(|v| v.as_str());
                let blob = req.params.get("blob").and_then(|v| v.as_str());
                let include_stale = req
                    .params
                    .get("includeStale")
                    .and_then(|v| v.as_bool())
                    .unwrap_or(false);
                let filters = NoteFilters {
                    file,
                    project_root: proj,
                    node_path: None,
                    commit,
                    blob,
                    include_stale,
                };
                match notes::list_for_file(db, filters) {
                    Ok(mut notes_list) => {
                        // If content is provided, compute display positions server-side
                        if let Some(content) = req.params.get("content").and_then(|v| v.as_str()) {
                            let file_path = Path::new(file);
                            for note in &mut notes_list {
                                let pos = compute_display_position(
                                    parser,
                                    file_path,
                                    content,
                                    note.line as u32,
                                    note.column as u32,
                                    note.node_text_hash.as_deref(),
                                    20, // search_radius
                                );
                                note.line = pos.line as i64;
                                note.stale = pos.stale;
                            }
                        }
                        Response::result(id, serde_json::to_value(notes_list).unwrap())
                    }
                    Err(e) => Response::error(id, INTERNAL_ERROR, e.to_string()),
                }
            } else {
                Response::error(id, INVALID_PARAMS, "missing file/projectRoot")
            }
        }
        "notes/list-project" => {
            if let Some(proj) = req.params.get("projectRoot").and_then(|v| v.as_str()) {
                let limit = req
                    .params
                    .get("limit")
                    .and_then(|v| v.as_u64())
                    .map(|v| v as usize);
                let offset = req
                    .params
                    .get("offset")
                    .and_then(|v| v.as_u64())
                    .unwrap_or(0) as usize;
                match notes::list_project(db, proj, limit, offset) {
                    Ok(n) => Response::result(id, serde_json::to_value(n).unwrap()),
                    Err(e) => Response::error(id, INTERNAL_ERROR, e.to_string()),
                }
            } else {
                Response::error(id, INVALID_PARAMS, "missing projectRoot")
            }
        }
        "notes/search" => {
            let query = req
                .params
                .get("query")
                .and_then(|v| v.as_str())
                .unwrap_or("");
            let proj = req.params.get("projectRoot").and_then(|v| v.as_str());
            let limit = req
                .params
                .get("limit")
                .and_then(|v| v.as_u64())
                .map(|v| v as usize);
            let offset = req
                .params
                .get("offset")
                .and_then(|v| v.as_u64())
                .unwrap_or(0) as usize;
            match notes::search(db, query, proj, limit, offset) {
                Ok(n) => Response::result(id, serde_json::to_value(n).unwrap()),
                Err(e) => Response::error(id, INTERNAL_ERROR, e.to_string()),
            }
        }
        "notes/list-by-node" => {
            let file = req.params.get("file").and_then(|v| v.as_str());
            let proj = req.params.get("projectRoot").and_then(|v| v.as_str());
            let node = req.params.get("nodePath").cloned();
            if let (Some(file), Some(proj)) = (file, proj) {
                let commit = req.params.get("commit").and_then(|v| v.as_str());
                let blob = req.params.get("blob").and_then(|v| v.as_str());
                let include_stale = req
                    .params
                    .get("includeStale")
                    .and_then(|v| v.as_bool())
                    .unwrap_or(false);
                let filters = NoteFilters {
                    file,
                    project_root: proj,
                    node_path: node,
                    commit,
                    blob,
                    include_stale,
                };
                match notes::list_by_node(db, filters) {
                    Ok(n) => Response::result(id, serde_json::to_value(n).unwrap()),
                    Err(e) => Response::error(id, INTERNAL_ERROR, e.to_string()),
                }
            } else {
                Response::error(id, INVALID_PARAMS, "missing file/projectRoot")
            }
        }
        "notes/create" => {
            let file = req.params.get("file").and_then(|v| v.as_str());
            let proj = req.params.get("projectRoot").and_then(|v| v.as_str());
            if let (Some(file), Some(proj)) = (file, proj) {
                let line = req.params.get("line").and_then(|v| v.as_i64()).unwrap_or(1);
                let column = req
                    .params
                    .get("column")
                    .and_then(|v| v.as_i64())
                    .unwrap_or(0);
                let tags = req.params.get("tags").cloned().unwrap_or_else(|| json!([]));
                let text = req
                    .params
                    .get("text")
                    .and_then(|v| v.as_str())
                    .unwrap_or_default();
                // If content is provided, compute node_path and node_text_hash server-side
                // Otherwise, use client-provided values (backwards compatibility)
                let content = req.params.get("content").and_then(|v| v.as_str());
                let (node_path, node_text_hash) = if let Some(content) = content {
                    let file_path = Path::new(file);
                    let computed_path = compute_node_path(parser, file_path, content, line as u32, column as u32);
                    let computed_hash = compute_hash_at_position(parser, file_path, content, line as u32, column as u32);
                    let node_path_value = if computed_path.is_empty() {
                        None
                    } else {
                        Some(serde_json::to_value(&computed_path).unwrap())
                    };
                    (node_path_value, computed_hash)
                } else {
                    // Backwards compatibility: use client-provided values
                    let node_path = req.params.get("nodePath").cloned();
                    let node_text_hash = req
                        .params
                        .get("nodeTextHash")
                        .and_then(|v| v.as_str())
                        .map(|s| s.to_string());
                    (node_path, node_text_hash)
                };
                let git = info_for_file(file);
                match notes::create(
                    db,
                    file,
                    proj,
                    line,
                    column,
                    node_path,
                    tags,
                    text,
                    git,
                    node_text_hash,
                ) {
                    Ok(n) => Response::result(id, serde_json::to_value(n).unwrap()),
                    Err(e) => Response::error(id, INTERNAL_ERROR, e.to_string()),
                }
            } else {
                Response::error(id, INVALID_PARAMS, "missing file/projectRoot")
            }
        }
        "notes/delete" => {
            if let Some(note_id) = req.params.get("id").and_then(|v| v.as_str()) {
                match notes::delete(db, note_id) {
                    Ok(ok) => Response::result(id, json!({"ok": ok})),
                    Err(e) => Response::error(id, INTERNAL_ERROR, e.to_string()),
                }
            } else {
                Response::error(id, INVALID_PARAMS, "missing id")
            }
        }
        "notes/update" => {
            if let Some(note_id) = req.params.get("id").and_then(|v| v.as_str()) {
                let text = req.params.get("text").and_then(|v| v.as_str());
                let tags = req.params.get("tags").cloned();
                match notes::update(db, note_id, text, tags) {
                    Ok(n) => Response::result(id, serde_json::to_value(n).unwrap()),
                    Err(e) => Response::error(id, INTERNAL_ERROR, e.to_string()),
                }
            } else {
                Response::error(id, INVALID_PARAMS, "missing id")
            }
        }
        "notes/get" => {
            if let Some(note_id) = req.params.get("id").and_then(|v| v.as_str()) {
                match notes::get(db, note_id) {
                    Ok(n) => Response::result(id, serde_json::to_value(n).unwrap()),
                    Err(e) => Response::error(id, INTERNAL_ERROR, e.to_string()),
                }
            } else {
                Response::error(id, INVALID_PARAMS, "missing id")
            }
        }
        "notes/backlinks" => {
            if let Some(note_id) = req.params.get("id").and_then(|v| v.as_str()) {
                match notes::backlinks(db, note_id) {
                    Ok(n) => Response::result(id, serde_json::to_value(n).unwrap()),
                    Err(e) => Response::error(id, INTERNAL_ERROR, e.to_string()),
                }
            } else {
                Response::error(id, INVALID_PARAMS, "missing id")
            }
        }
        "notes/reattach" => {
            let note_id = req.params.get("id").and_then(|v| v.as_str());
            let file = req.params.get("file").and_then(|v| v.as_str());
            if let Some(note_id) = note_id {
                let line = req.params.get("line").and_then(|v| v.as_i64()).unwrap_or(1);
                let column = req
                    .params
                    .get("column")
                    .and_then(|v| v.as_i64())
                    .unwrap_or(0);
                // If content is provided, compute node_path and hash server-side
                let content = req.params.get("content").and_then(|v| v.as_str());
                let (node_path, node_text_hash) = if let (Some(content), Some(file)) = (content, file) {
                    let file_path = Path::new(file);
                    let computed_path = compute_node_path(parser, file_path, content, line as u32, column as u32);
                    let computed_hash = compute_hash_at_position(parser, file_path, content, line as u32, column as u32);
                    let node_path_value = if computed_path.is_empty() {
                        None
                    } else {
                        Some(serde_json::to_value(&computed_path).unwrap())
                    };
                    (node_path_value, computed_hash)
                } else {
                    // Backwards compatibility
                    let node_path = req.params.get("nodePath").cloned();
                    let node_text_hash = req
                        .params
                        .get("nodeTextHash")
                        .and_then(|v| v.as_str())
                        .map(|s| s.to_string());
                    (node_path, node_text_hash)
                };
                let git = file.and_then(info_for_file);
                match notes::reattach(db, note_id, line, column, node_path, git, node_text_hash) {
                    Ok(n) => Response::result(id, serde_json::to_value(n).unwrap()),
                    Err(e) => Response::error(id, INTERNAL_ERROR, e.to_string()),
                }
            } else {
                Response::error(id, INVALID_PARAMS, "missing id")
            }
        }
        "notes/buffer-update" => {
            // Real-time position tracking: recompute positions for all notes in file
            let file = req.params.get("file").and_then(|v| v.as_str());
            let content = req.params.get("content").and_then(|v| v.as_str());
            if let (Some(file), Some(content)) = (file, content) {
                // Get all notes for this file
                let filters = NoteFilters {
                    file,
                    project_root: "",
                    node_path: None,
                    commit: None,
                    blob: None,
                    include_stale: true,
                };
                match notes::list_for_file(db, filters) {
                    Ok(mut notes_list) => {
                        let file_path = Path::new(file);
                        for note in &mut notes_list {
                            let pos = compute_display_position(
                                parser,
                                file_path,
                                content,
                                note.line as u32,
                                note.column as u32,
                                note.node_text_hash.as_deref(),
                                20,
                            );
                            note.line = pos.line as i64;
                            note.stale = pos.stale;
                        }
                        Response::result(id, serde_json::to_value(notes_list).unwrap())
                    }
                    Err(e) => Response::error(id, INTERNAL_ERROR, e.to_string()),
                }
            } else {
                Response::error(id, INVALID_PARAMS, "missing file/content")
            }
        }
        "index/add-file" => {
            let file = req.params.get("file").and_then(|v| v.as_str());
            let proj = req.params.get("projectRoot").and_then(|v| v.as_str());
            let content = req
                .params
                .get("content")
                .and_then(|v| v.as_str())
                .unwrap_or("");
            if let (Some(file), Some(proj)) = (file, proj) {
                match idx::add_file(db, file, proj, content) {
                    Ok(info) => Response::result(id, serde_json::to_value(info).unwrap()),
                    Err(e) => Response::error(id, INTERNAL_ERROR, e.to_string()),
                }
            } else {
                Response::error(id, INVALID_PARAMS, "missing file/projectRoot")
            }
        }
        "index/search" => {
            let query = req
                .params
                .get("query")
                .and_then(|v| v.as_str())
                .unwrap_or("");
            let proj = req.params.get("projectRoot").and_then(|v| v.as_str());
            match idx::search(db, query, proj) {
                Ok(results) => Response::result(id, serde_json::to_value(results).unwrap()),
                Err(e) => Response::error(id, INTERNAL_ERROR, e.to_string()),
            }
        }
        "shutdown" => Response::result(id, json!("shutting down")),
        _ => Response::error(
            id,
            METHOD_NOT_FOUND,
            format!("unknown method: {}", req.method),
        ),
    }
}

pub fn parse_and_handle(buf: &[u8], conn: &Connection, parser: &mut ParserService) -> Response {
    match rpc::parse_request(buf) {
        Ok(req) => handle(req, conn, parser),
        Err(_) => Response::error(None, PARSE_ERROR, "parse error"),
    }
}

/// Create a new ParserService with default configuration
pub fn create_parser_service() -> ParserService {
    let registry = GrammarRegistry::new(default_config());
    ParserService::new(registry)
}

#[cfg(test)]
mod tests {
    use super::*;
    use rusqlite::Connection;

    #[test]
    fn returns_parse_error_on_invalid_json() {
        let conn = Connection::open_in_memory().unwrap();
        let mut parser = create_parser_service();
        let resp = parse_and_handle(b"not json", &conn, &mut parser);
        assert!(resp.result.is_none());
        assert_eq!(resp.error.as_ref().unwrap().code, PARSE_ERROR);
    }

    #[test]
    fn shutdown_round_trip_succeeds() {
        let conn = Connection::open_in_memory().unwrap();
        let mut parser = create_parser_service();
        let resp = parse_and_handle(
            br#"{"jsonrpc":"2.0","id":1,"method":"shutdown","params":{}}"#,
            &conn,
            &mut parser,
        );
        assert_eq!(resp.result, Some(json!("shutting down")));
        assert!(resp.error.is_none());
    }
}
