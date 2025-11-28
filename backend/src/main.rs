use std::io::{self, Read, Write};

use anyhow::Result;
use rpc::{Request, Response, METHOD_NOT_FOUND, PARSE_ERROR, INTERNAL_ERROR};
use serde_json::json;
use storage::connect;
use notes::{self, NoteFilters};
use index as idx;
use git::info_for_file;

fn handle(req: Request, db: &rusqlite::Connection) -> Response {
    let id = req.id.clone();
    match req.method.as_str() {
        "notes/list-for-file" => {
            let file = req.params.get("file").and_then(|v| v.as_str());
            let proj = req.params.get("projectRoot").and_then(|v| v.as_str());
            if let (Some(file), Some(proj)) = (file, proj) {
                let commit = req.params.get("commit").and_then(|v| v.as_str());
                let blob = req.params.get("blob").and_then(|v| v.as_str());
                let include_stale = req.params.get("includeStale").and_then(|v| v.as_bool()).unwrap_or(false);
                let filters = NoteFilters { file, project_root: proj, node_path: None, commit, blob, include_stale };
                match notes::list_for_file(db, filters) {
                    Ok(n) => Response::result(id, serde_json::to_value(n).unwrap()),
                    Err(e) => Response::error(id, INTERNAL_ERROR, e.to_string()),
                }
            } else {
                Response::error(id, METHOD_NOT_FOUND, "missing file/projectRoot")
            }
        }
        "notes/list-by-node" => {
            let file = req.params.get("file").and_then(|v| v.as_str());
            let proj = req.params.get("projectRoot").and_then(|v| v.as_str());
            let node = req.params.get("nodePath").cloned();
            if let (Some(file), Some(proj)) = (file, proj) {
                let commit = req.params.get("commit").and_then(|v| v.as_str());
                let blob = req.params.get("blob").and_then(|v| v.as_str());
                let include_stale = req.params.get("includeStale").and_then(|v| v.as_bool()).unwrap_or(false);
                let filters = NoteFilters { file, project_root: proj, node_path: node, commit, blob, include_stale };
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
                let column = req.params.get("column").and_then(|v| v.as_i64()).unwrap_or(0);
                let node_path = req.params.get("nodePath").cloned();
                let tags = req.params.get("tags").cloned().unwrap_or_else(|| json!([]));
                let text = req.params.get("text").and_then(|v| v.as_str()).unwrap_or_default();
                let git = info_for_file(file);
                match notes::create(db, file, proj, line, column, node_path, tags, text, git) {
                    Ok(n) => Response::result(id, serde_json::to_value(n).unwrap()),
                    Err(e) => Response::error(id, INTERNAL_ERROR, e.to_string()),
                }
            } else {
                Response::error(id, METHOD_NOT_FOUND, "missing file/projectRoot")
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
            let content = req.params.get("content").and_then(|v| v.as_str()).unwrap_or("");
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
            let query = req.params.get("query").and_then(|v| v.as_str()).unwrap_or("");
            let proj = req.params.get("projectRoot").and_then(|v| v.as_str());
            match idx::search(db, query, proj) {
                Ok(results) => Response::result(id, serde_json::to_value(results).unwrap()),
                Err(e) => Response::error(id, INTERNAL_ERROR, e.to_string()),
            }
        }
        "shutdown" => Response::result(id, json!("shutting down")),
        _ => Response::error(id, METHOD_NOT_FOUND, format!("unknown method: {}", req.method)),
    }
}

fn main() -> Result<()> {
    let db_path = std::env::var("HEMIS_DB_PATH").unwrap_or_else(|_| "hemis-notes.db".into());
    let conn = connect(&db_path)?;
    let mut stdin = Vec::new();
    io::stdin().read_to_end(&mut stdin)?;
    // Supports both framed and plain JSON per line for now.
    let mut out = Vec::new();
    let input = String::from_utf8_lossy(&stdin);
    for line in input.lines() {
        let bytes = line.as_bytes();
        let req: Request = match rpc::parse_request(bytes) {
            Ok(r) => r,
            Err(_) => { out.extend(rpc::encode_response(&Response::error(None, PARSE_ERROR, "parse error"))); continue; }
        };
        let resp = handle(req, &conn);
        let json = rpc::encode_response(&resp);
        out.extend(json);
        out.push(b'\n');
    }
    io::stdout().write_all(&out)?;
    Ok(())
}
