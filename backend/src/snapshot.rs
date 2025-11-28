use anyhow::Result;
use rusqlite::Connection;
use serde_json::json;
use storage::{exec, now_unix, query_all};

pub fn create(conn: &Connection, project_root: Option<&str>) -> Result<serde_json::Value> {
    let notes = query_all(conn, "SELECT * FROM notes;", &[], |row| {
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
            "createdAt": row.get::<_, i64>("created_at")?,
            "updatedAt": row.get::<_, i64>("updated_at")?
        }))
    })?;
    let files = query_all(conn, "SELECT * FROM files;", &[], |row| {
        Ok(json!({
            "file": row.get::<_, String>("file")?,
            "projectRoot": row.get::<_, String>("project_root")?,
            "content": row.get::<_, String>("content")?,
            "updatedAt": row.get::<_, i64>("updated_at")?,
        }))
    })?;
    let embeddings = query_all(conn, "SELECT * FROM embeddings;", &[], |row| {
        Ok(json!({
            "file": row.get::<_, String>("file")?,
            "projectRoot": row.get::<_, String>("project_root")?,
            "vector": row.get::<_, String>("vector")?,
            "text": row.get::<_, String>("text")?,
            "updatedAt": row.get::<_, i64>("updated_at")?,
        }))
    })?;
    let edges = query_all(conn, "SELECT * FROM edges;", &[], |row| {
        Ok(json!({
            "id": row.get::<_, i64>("id")?,
            "src": row.get::<_, String>("src")?,
            "dst": row.get::<_, String>("dst")?,
            "kind": row.get::<_, String>("kind")?,
            "projectRoot": row.get::<_, String>("project_root")?,
            "updatedAt": row.get::<_, i64>("updated_at")?,
        }))
    })?;
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

pub fn restore(conn: &Connection, snapshot: &serde_json::Value) -> Result<serde_json::Value> {
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
            let created_at = n
                .get("createdAt")
                .and_then(|v| v.as_i64())
                .unwrap_or_else(now_unix);
            let updated_at = n
                .get("updatedAt")
                .and_then(|v| v.as_i64())
                .unwrap_or(created_at);
            exec(conn,
                 "INSERT INTO notes (id,file,project_root,line,column,node_path,tags,text,summary,commit_sha,blob_sha,created_at,updated_at) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?);",
                 &[&id, &file, &proj, &line, &column, &node_path, &tags, &text, &summary, &commit, &blob, &created_at, &updated_at])?;
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
}
