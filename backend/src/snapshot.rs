use anyhow::Result;
use rusqlite::Connection;
use serde_json::json;
use storage::{exec, now_unix, query_all, transaction};

pub fn create(conn: &Connection, project_root: Option<&str>) -> Result<serde_json::Value> {
    // Build queries with optional project_root filter
    let notes = if let Some(pr) = project_root {
        query_all(
            conn,
            "SELECT * FROM notes WHERE project_root = ?;",
            &[&pr as &dyn rusqlite::ToSql],
            |row| Ok(note_from_row(row)?),
        )?
    } else {
        query_all(conn, "SELECT * FROM notes;", &[], |row| {
            Ok(note_from_row(row)?)
        })?
    };

    let files = if let Some(pr) = project_root {
        query_all(
            conn,
            "SELECT * FROM files WHERE project_root = ?;",
            &[&pr as &dyn rusqlite::ToSql],
            |row| Ok(file_from_row(row)?),
        )?
    } else {
        query_all(conn, "SELECT * FROM files;", &[], |row| {
            Ok(file_from_row(row)?)
        })?
    };

    let embeddings = if let Some(pr) = project_root {
        query_all(
            conn,
            "SELECT * FROM embeddings WHERE project_root = ?;",
            &[&pr as &dyn rusqlite::ToSql],
            |row| Ok(embedding_from_row(row)?),
        )?
    } else {
        query_all(conn, "SELECT * FROM embeddings;", &[], |row| {
            Ok(embedding_from_row(row)?)
        })?
    };

    let edges = if let Some(pr) = project_root {
        query_all(
            conn,
            "SELECT * FROM edges WHERE project_root = ?;",
            &[&pr as &dyn rusqlite::ToSql],
            |row| Ok(edge_from_row(row)?),
        )?
    } else {
        query_all(conn, "SELECT * FROM edges;", &[], |row| {
            Ok(edge_from_row(row)?)
        })?
    };

    let counts = json!({
        "files": files.len(),
        "notes": notes.len(),
        "embeddings": embeddings.len(),
        "edges": edges.len()
    });
    Ok(json!({
        "version": 1,
        "projectRoot": project_root,
        "createdAt": now_unix(),
        "counts": counts,
        "notes": notes,
        "files": files,
        "embeddings": embeddings,
        "edges": edges
    }))
}

fn note_from_row(row: &rusqlite::Row) -> rusqlite::Result<serde_json::Value> {
    Ok(json!({
        "id": row.get::<_, String>("id")?,
        "file": row.get::<_, String>("file")?,
        "projectRoot": row.get::<_, String>("project_root")?,
        "line": row.get::<_, i64>("line")?,
        "column": row.get::<_, i64>("column")?,
        "nodePath": row.get::<_, Option<String>>("node_path")?,
        "tags": row.get::<_, String>("tags")?,
        "text": row.get::<_, String>("text")?,
        "summary": row.get::<_, String>("summary")?,
        "commitSha": row.get::<_, Option<String>>("commit_sha")?,
        "blobSha": row.get::<_, Option<String>>("blob_sha")?,
        "nodeTextHash": row.get::<_, Option<String>>("node_text_hash")?,
        "createdAt": row.get::<_, i64>("created_at")?,
        "updatedAt": row.get::<_, i64>("updated_at")?
    }))
}

fn file_from_row(row: &rusqlite::Row) -> rusqlite::Result<serde_json::Value> {
    Ok(json!({
        "file": row.get::<_, String>("file")?,
        "projectRoot": row.get::<_, String>("project_root")?,
        "content": row.get::<_, String>("content")?,
        "updatedAt": row.get::<_, i64>("updated_at")?,
    }))
}

fn embedding_from_row(row: &rusqlite::Row) -> rusqlite::Result<serde_json::Value> {
    Ok(json!({
        "file": row.get::<_, String>("file")?,
        "projectRoot": row.get::<_, String>("project_root")?,
        "vector": row.get::<_, String>("vector")?,
        "text": row.get::<_, String>("text")?,
        "updatedAt": row.get::<_, i64>("updated_at")?,
    }))
}

fn edge_from_row(row: &rusqlite::Row) -> rusqlite::Result<serde_json::Value> {
    Ok(json!({
        "id": row.get::<_, i64>("id")?,
        "src": row.get::<_, String>("src")?,
        "dst": row.get::<_, String>("dst")?,
        "kind": row.get::<_, String>("kind")?,
        "projectRoot": row.get::<_, String>("project_root")?,
        "updatedAt": row.get::<_, i64>("updated_at")?,
    }))
}

/// Maximum records per collection in a snapshot (prevents DoS via huge snapshots)
const MAX_SNAPSHOT_NOTES: usize = 100_000;
const MAX_SNAPSHOT_FILES: usize = 100_000;
const MAX_SNAPSHOT_EMBEDDINGS: usize = 100_000;
const MAX_SNAPSHOT_EDGES: usize = 500_000;

/// Maximum size for individual text fields (prevents memory exhaustion)
const MAX_NOTE_TEXT_LEN: usize = 1_000_000; // 1MB per note text
const MAX_FILE_CONTENT_LEN: usize = 1_000_000; // 1MB per file content
const MAX_EMBEDDING_VECTOR_LEN: usize = 100_000; // ~100KB per embedding vector string

pub fn restore(conn: &Connection, snapshot: &serde_json::Value) -> Result<serde_json::Value> {
    // Validate record counts before processing
    if let Some(notes) = snapshot.get("notes").and_then(|v| v.as_array()) {
        if notes.len() > MAX_SNAPSHOT_NOTES {
            return Err(anyhow::anyhow!("snapshot has too many notes: {} (max {})", notes.len(), MAX_SNAPSHOT_NOTES));
        }
    }
    if let Some(files) = snapshot.get("files").and_then(|v| v.as_array()) {
        if files.len() > MAX_SNAPSHOT_FILES {
            return Err(anyhow::anyhow!("snapshot has too many files: {} (max {})", files.len(), MAX_SNAPSHOT_FILES));
        }
    }
    if let Some(embeddings) = snapshot.get("embeddings").and_then(|v| v.as_array()) {
        if embeddings.len() > MAX_SNAPSHOT_EMBEDDINGS {
            return Err(anyhow::anyhow!("snapshot has too many embeddings: {} (max {})", embeddings.len(), MAX_SNAPSHOT_EMBEDDINGS));
        }
    }
    if let Some(edges) = snapshot.get("edges").and_then(|v| v.as_array()) {
        if edges.len() > MAX_SNAPSHOT_EDGES {
            return Err(anyhow::anyhow!("snapshot has too many edges: {} (max {})", edges.len(), MAX_SNAPSHOT_EDGES));
        }
    }

    // Validate project_root consistency - all records should belong to same project
    let expected_project = snapshot.get("projectRoot").and_then(|v| v.as_str());
    if let Some(expected) = expected_project {
        // Validate notes belong to expected project
        if let Some(notes) = snapshot.get("notes").and_then(|v| v.as_array()) {
            for (i, n) in notes.iter().enumerate() {
                if let Some(proj) = n.get("projectRoot").and_then(|v| v.as_str()) {
                    if proj != expected {
                        return Err(anyhow::anyhow!(
                            "snapshot note[{}] has mismatched projectRoot (expected: {}, got: {})",
                            i, expected, proj
                        ));
                    }
                }
            }
        }
        // Validate files belong to expected project
        if let Some(files) = snapshot.get("files").and_then(|v| v.as_array()) {
            for (i, f) in files.iter().enumerate() {
                if let Some(proj) = f.get("projectRoot").and_then(|v| v.as_str()) {
                    if proj != expected {
                        return Err(anyhow::anyhow!(
                            "snapshot file[{}] has mismatched projectRoot",
                            i
                        ));
                    }
                }
            }
        }
    }

    transaction(conn, || {
        exec(conn, "DELETE FROM notes;", &[])?;
        exec(conn, "DELETE FROM files;", &[])?;
        exec(conn, "DELETE FROM embeddings;", &[])?;
        exec(conn, "DELETE FROM edges;", &[])?;
        // Clear project_meta to avoid stale indexing/analysis timestamps
        exec(conn, "DELETE FROM project_meta;", &[])?;
        if let Some(notes) = snapshot.get("notes").and_then(|v| v.as_array()) {
            for (i, n) in notes.iter().enumerate() {
                // Validate required fields to prevent silent data corruption
                let id = n.get("id").and_then(|v| v.as_str())
                    .ok_or_else(|| anyhow::anyhow!("snapshot note[{}] missing required field: id", i))?;
                let file = n.get("file").and_then(|v| v.as_str())
                    .ok_or_else(|| anyhow::anyhow!("snapshot note[{}] missing required field: file", i))?;
                let proj = n.get("projectRoot").and_then(|v| v.as_str())
                    .ok_or_else(|| anyhow::anyhow!("snapshot note[{}] missing required field: projectRoot", i))?;
                // Validate id format (should be UUID-like)
                if id.is_empty() || id.len() > 64 {
                    return Err(anyhow::anyhow!("snapshot note[{}] has invalid id length", i));
                }
                if !id.chars().all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_') {
                    return Err(anyhow::anyhow!("snapshot note[{}] has invalid id format", i));
                }
                // Validate line/column ranges
                let line = n.get("line").and_then(|v| v.as_i64()).unwrap_or(1);
                if line < 1 || line > 10_000_000 {
                    return Err(anyhow::anyhow!("snapshot note[{}] has invalid line number: {}", i, line));
                }
                let column = n.get("column").and_then(|v| v.as_i64()).unwrap_or(0);
                if column < 0 || column > 100_000 {
                    return Err(anyhow::anyhow!("snapshot note[{}] has invalid column number: {}", i, column));
                }
                let node_path = n.get("nodePath").and_then(|v| v.as_str());
                let tags = n.get("tags").and_then(|v| v.as_str()).unwrap_or("[]");
                let text = n.get("text").and_then(|v| v.as_str()).unwrap_or("");
                // Validate text field size to prevent memory exhaustion
                if text.len() > MAX_NOTE_TEXT_LEN {
                    return Err(anyhow::anyhow!("snapshot note[{}] text too large: {} bytes (max {})", i, text.len(), MAX_NOTE_TEXT_LEN));
                }
                let summary = n.get("summary").and_then(|v| v.as_str()).unwrap_or("");
                if summary.len() > MAX_NOTE_TEXT_LEN {
                    return Err(anyhow::anyhow!("snapshot note[{}] summary too large: {} bytes (max {})", i, summary.len(), MAX_NOTE_TEXT_LEN));
                }
                let commit = n.get("commitSha").and_then(|v| v.as_str());
                let blob = n.get("blobSha").and_then(|v| v.as_str());
                let node_text_hash = n.get("nodeTextHash").and_then(|v| v.as_str());
                let created_at = n
                    .get("createdAt")
                    .and_then(|v| v.as_i64())
                    .unwrap_or_else(now_unix);
                let updated_at = n
                    .get("updatedAt")
                    .and_then(|v| v.as_i64())
                    .unwrap_or(created_at);
                exec(conn,
                     "INSERT INTO notes (id,file,project_root,line,column,node_path,tags,text,summary,commit_sha,blob_sha,node_text_hash,created_at,updated_at) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?);",
                     &[&id, &file, &proj, &line, &column, &node_path, &tags, &text, &summary, &commit, &blob, &node_text_hash, &created_at, &updated_at])?;
            }
        }
        if let Some(files) = snapshot.get("files").and_then(|v| v.as_array()) {
            for (i, f) in files.iter().enumerate() {
                let file = f.get("file").and_then(|v| v.as_str())
                    .ok_or_else(|| anyhow::anyhow!("snapshot file[{}] missing required field: file", i))?;
                let proj = f.get("projectRoot").and_then(|v| v.as_str())
                    .ok_or_else(|| anyhow::anyhow!("snapshot file[{}] missing required field: projectRoot", i))?;
                let content = f.get("content").and_then(|v| v.as_str()).unwrap_or("");
                // Validate content field size
                if content.len() > MAX_FILE_CONTENT_LEN {
                    return Err(anyhow::anyhow!("snapshot file[{}] content too large: {} bytes (max {})", i, content.len(), MAX_FILE_CONTENT_LEN));
                }
                let updated_at = f
                    .get("updatedAt")
                    .and_then(|v| v.as_i64())
                    .unwrap_or_else(now_unix);
                exec(
                    conn,
                    "INSERT INTO files (file, project_root, content, updated_at) VALUES (?,?,?,?);",
                    &[&file, &proj, &content, &updated_at],
                )?;
            }
        }
        if let Some(embs) = snapshot.get("embeddings").and_then(|v| v.as_array()) {
            for (i, e) in embs.iter().enumerate() {
                let file = e.get("file").and_then(|v| v.as_str())
                    .ok_or_else(|| anyhow::anyhow!("snapshot embedding[{}] missing required field: file", i))?;
                let proj = e.get("projectRoot").and_then(|v| v.as_str())
                    .ok_or_else(|| anyhow::anyhow!("snapshot embedding[{}] missing required field: projectRoot", i))?;
                let vector = e
                    .get("vector")
                    .map(|v| v.to_string())
                    .unwrap_or("[]".into());
                // Validate vector field size
                if vector.len() > MAX_EMBEDDING_VECTOR_LEN {
                    return Err(anyhow::anyhow!("snapshot embedding[{}] vector too large: {} bytes (max {})", i, vector.len(), MAX_EMBEDDING_VECTOR_LEN));
                }
                let text = e.get("text").and_then(|v| v.as_str()).unwrap_or("");
                if text.len() > MAX_FILE_CONTENT_LEN {
                    return Err(anyhow::anyhow!("snapshot embedding[{}] text too large: {} bytes (max {})", i, text.len(), MAX_FILE_CONTENT_LEN));
                }
                let updated_at = e
                    .get("updatedAt")
                    .and_then(|v| v.as_i64())
                    .unwrap_or_else(now_unix);
                exec(conn,
                     "INSERT INTO embeddings (file, project_root, vector, text, updated_at) VALUES (?,?,?,?,?);",
                     &[&file, &proj, &vector, &text, &updated_at])?;
            }
        }
        if let Some(edges) = snapshot.get("edges").and_then(|v| v.as_array()) {
            for (i, e) in edges.iter().enumerate() {
                let src = e.get("src").and_then(|v| v.as_str())
                    .ok_or_else(|| anyhow::anyhow!("snapshot edge[{}] missing required field: src", i))?;
                let dst = e.get("dst").and_then(|v| v.as_str())
                    .ok_or_else(|| anyhow::anyhow!("snapshot edge[{}] missing required field: dst", i))?;
                let kind = e.get("kind").and_then(|v| v.as_str()).unwrap_or("link");
                let proj = e.get("projectRoot").and_then(|v| v.as_str())
                    .ok_or_else(|| anyhow::anyhow!("snapshot edge[{}] missing required field: projectRoot", i))?;
                let updated_at = e
                    .get("updatedAt")
                    .and_then(|v| v.as_i64())
                    .unwrap_or_else(now_unix);
                exec(
                    conn,
                    "INSERT INTO edges (src, dst, kind, project_root, updated_at) VALUES (?,?,?,?,?);",
                    &[&src, &dst, &kind, &proj, &updated_at],
                )?;
            }
        }
        let counts = json!({
            "files": snapshot.get("files").and_then(|v| v.as_array()).map(|a| a.len()).unwrap_or(0),
            "notes": snapshot.get("notes").and_then(|v| v.as_array()).map(|a| a.len()).unwrap_or(0),
            "embeddings": snapshot.get("embeddings").and_then(|v| v.as_array()).map(|a| a.len()).unwrap_or(0),
            "edges": snapshot.get("edges").and_then(|v| v.as_array()).map(|a| a.len()).unwrap_or(0),
        });
        Ok(json!({"ok": true, "counts": counts}))
    })
}
