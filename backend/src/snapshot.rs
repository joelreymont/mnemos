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

pub fn restore(conn: &Connection, snapshot: &serde_json::Value) -> Result<serde_json::Value> {
    transaction(conn, || {
        exec(conn, "DELETE FROM notes;", &[])?;
        exec(conn, "DELETE FROM files;", &[])?;
        exec(conn, "DELETE FROM embeddings;", &[])?;
        exec(conn, "DELETE FROM edges;", &[])?;
        if let Some(notes) = snapshot.get("notes").and_then(|v| v.as_array()) {
            for n in notes {
                let id = n.get("id").and_then(|v| v.as_str()).unwrap_or("");
                let file = n.get("file").and_then(|v| v.as_str()).unwrap_or("");
                let proj = n.get("projectRoot").and_then(|v| v.as_str()).unwrap_or("");
                let line = n.get("line").and_then(|v| v.as_i64()).unwrap_or(1);
                let column = n.get("column").and_then(|v| v.as_i64()).unwrap_or(0);
                let node_path = n.get("nodePath").and_then(|v| v.as_str());
                let tags = n.get("tags").and_then(|v| v.as_str()).unwrap_or("[]");
                let text = n.get("text").and_then(|v| v.as_str()).unwrap_or("");
                let summary = n.get("summary").and_then(|v| v.as_str()).unwrap_or("");
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
            for f in files {
                let file = f.get("file").and_then(|v| v.as_str()).unwrap_or("");
                let proj = f.get("projectRoot").and_then(|v| v.as_str()).unwrap_or("");
                let content = f.get("content").and_then(|v| v.as_str()).unwrap_or("");
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
            for e in embs {
                let file = e.get("file").and_then(|v| v.as_str()).unwrap_or("");
                let proj = e.get("projectRoot").and_then(|v| v.as_str()).unwrap_or("");
                let vector = e
                    .get("vector")
                    .map(|v| v.to_string())
                    .unwrap_or("[]".into());
                let text = e.get("text").and_then(|v| v.as_str()).unwrap_or("");
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
            for e in edges {
                let src = e.get("src").and_then(|v| v.as_str()).unwrap_or("");
                let dst = e.get("dst").and_then(|v| v.as_str()).unwrap_or("");
                let kind = e.get("kind").and_then(|v| v.as_str()).unwrap_or("");
                let proj = e.get("projectRoot").and_then(|v| v.as_str()).unwrap_or("");
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
