use git::info_for_file;
use index as idx;
use notes::{self, NoteFilters};
use rpc::{Request, Response, INTERNAL_ERROR, METHOD_NOT_FOUND, PARSE_ERROR};
use rusqlite::Connection;
use serde_json::json;
use std::fs;
use std::path::Path;
use storage::now_unix;

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

fn list_files(root: &Path) -> anyhow::Result<Vec<String>> {
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
                files.push(path.to_string_lossy().to_string());
            }
        }
    }
    files.sort();
    Ok(files)
}

pub fn handle(req: Request, db: &Connection) -> Response {
    let id = req.id.clone();
    match req.method.as_str() {
        "hemis/list-files" => {
            if let Some(root) = req.params.get("projectRoot").and_then(|v| v.as_str()) {
                match list_files(Path::new(root)) {
                    Ok(files) => Response::result(id, serde_json::to_value(files).unwrap()),
                    Err(e) => Response::error(id, INTERNAL_ERROR, e.to_string()),
                }
            } else {
                Response::error(id, METHOD_NOT_FOUND, "missing projectRoot")
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
                Response::error(id, METHOD_NOT_FOUND, "missing file")
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
                Response::error(id, METHOD_NOT_FOUND, "missing projectRoot")
            }
        }
        "hemis/explain-region" => {
            let file = req.params.get("file").and_then(|v| v.as_str());
            let start_line = req
                .params
                .get("start")
                .and_then(|s| s.get("line"))
                .and_then(|v| v.as_u64())
                .unwrap_or(1) as usize;
            let end_line = req
                .params
                .get("end")
                .and_then(|s| s.get("line"))
                .and_then(|v| v.as_u64())
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
                            json!({"explanation": "No content in range", "references": []})
                        } else {
                            json!({"explanation": snippet, "references": []})
                        };
                        Response::result(id, resp)
                    }
                    Err(e) => Response::error(id, INTERNAL_ERROR, e.to_string()),
                }
            } else {
                Response::error(id, METHOD_NOT_FOUND, "missing file")
            }
        }
        "hemis/search" => {
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
        "hemis/save-snapshot" => {
            if let Some(path) = req.params.get("path").and_then(|v| v.as_str()) {
                let payload = json!({
                    "version": 1,
                    "projectRoot": req.params.get("projectRoot").and_then(|v| v.as_str()),
                    "createdAt": now_unix(),
                });
                match fs::write(path, serde_json::to_vec_pretty(&payload).unwrap()) {
                    Ok(_) => Response::result(id, json!({"ok": true, "path": path})),
                    Err(e) => Response::error(id, INTERNAL_ERROR, e.to_string()),
                }
            } else {
                Response::error(id, METHOD_NOT_FOUND, "missing path")
            }
        }
        "hemis/load-snapshot" => {
            if let Some(path) = req.params.get("path").and_then(|v| v.as_str()) {
                match fs::read_to_string(path) {
                    Ok(contents) => match serde_json::from_str::<serde_json::Value>(&contents) {
                        Ok(val) => Response::result(id, val),
                        Err(e) => Response::error(id, INTERNAL_ERROR, e.to_string()),
                    },
                    Err(e) => Response::error(id, INTERNAL_ERROR, e.to_string()),
                }
            } else {
                Response::error(id, METHOD_NOT_FOUND, "missing path")
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
                    Ok(n) => Response::result(id, serde_json::to_value(n).unwrap()),
                    Err(e) => Response::error(id, INTERNAL_ERROR, e.to_string()),
                }
            } else {
                Response::error(id, METHOD_NOT_FOUND, "missing file/projectRoot")
            }
        }
        "notes/list-project" => {
            if let Some(proj) = req.params.get("projectRoot").and_then(|v| v.as_str()) {
                match notes::list_project(db, proj) {
                    Ok(n) => Response::result(id, serde_json::to_value(n).unwrap()),
                    Err(e) => Response::error(id, INTERNAL_ERROR, e.to_string()),
                }
            } else {
                Response::error(id, METHOD_NOT_FOUND, "missing projectRoot")
            }
        }
        "notes/search" => {
            let query = req
                .params
                .get("query")
                .and_then(|v| v.as_str())
                .unwrap_or("");
            let proj = req.params.get("projectRoot").and_then(|v| v.as_str());
            match notes::search(db, query, proj) {
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
                Response::error(id, METHOD_NOT_FOUND, "missing file/projectRoot")
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
                let node_path = req.params.get("nodePath").cloned();
                let tags = req.params.get("tags").cloned().unwrap_or_else(|| json!([]));
                let text = req
                    .params
                    .get("text")
                    .and_then(|v| v.as_str())
                    .unwrap_or_default();
                let git = info_for_file(file);
                match notes::create(db, file, proj, line, column, node_path, tags, text, git) {
                    Ok(n) => Response::result(id, serde_json::to_value(n).unwrap()),
                    Err(e) => Response::error(id, INTERNAL_ERROR, e.to_string()),
                }
            } else {
                Response::error(id, METHOD_NOT_FOUND, "missing file/projectRoot")
            }
        }
        "notes/delete" => {
            if let Some(note_id) = req.params.get("id").and_then(|v| v.as_str()) {
                match notes::delete(db, note_id) {
                    Ok(ok) => Response::result(id, json!({"ok": ok})),
                    Err(e) => Response::error(id, INTERNAL_ERROR, e.to_string()),
                }
            } else {
                Response::error(id, METHOD_NOT_FOUND, "missing id")
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
                Response::error(id, METHOD_NOT_FOUND, "missing id")
            }
        }
        "notes/get" => {
            if let Some(note_id) = req.params.get("id").and_then(|v| v.as_str()) {
                match notes::get(db, note_id) {
                    Ok(n) => Response::result(id, serde_json::to_value(n).unwrap()),
                    Err(e) => Response::error(id, INTERNAL_ERROR, e.to_string()),
                }
            } else {
                Response::error(id, METHOD_NOT_FOUND, "missing id")
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
                Response::error(id, METHOD_NOT_FOUND, "missing file/projectRoot")
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

pub fn parse_and_handle(buf: &[u8], conn: &Connection) -> Response {
    match rpc::parse_request(buf) {
        Ok(req) => handle(req, conn),
        Err(_) => Response::error(None, PARSE_ERROR, "parse error"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rusqlite::Connection;

    #[test]
    fn returns_parse_error_on_invalid_json() {
        let conn = Connection::open_in_memory().unwrap();
        let resp = parse_and_handle(b"not json", &conn);
        assert!(resp.result.is_none());
        assert_eq!(resp.error.as_ref().unwrap().code, PARSE_ERROR);
    }

    #[test]
    fn shutdown_round_trip_succeeds() {
        let conn = Connection::open_in_memory().unwrap();
        let resp = parse_and_handle(
            br#"{"jsonrpc":"2.0","id":1,"method":"shutdown","params":{}}"#,
            &conn,
        );
        assert_eq!(resp.result, Some(json!("shutting down")));
        assert!(resp.error.is_none());
    }
}
