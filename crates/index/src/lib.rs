//! index: naive text indexing and search.

use anyhow::Result;
use rusqlite::Connection;
use serde::{Deserialize, Serialize};
use storage::{exec, now_unix, query_all};

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct IndexedFile {
    pub file: String,
    pub project_root: String,
    pub content: String,
    pub updated_at: i64,
    pub bytes: usize,
    pub lines: usize,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct SearchHit {
    pub file: String,
    pub line: usize,
    pub column: usize,
    pub text: String,
    pub score: f32,
}

pub fn add_file(
    conn: &Connection,
    file: &str,
    project_root: &str,
    content: &str,
) -> Result<IndexedFile> {
    let updated = now_unix();
    exec(
        conn,
        "INSERT OR REPLACE INTO files (file, project_root, content, updated_at) VALUES (?,?,?,?);",
        &[&file, &project_root, &content, &updated],
    )?;
    Ok(IndexedFile {
        file: file.to_string(),
        project_root: project_root.to_string(),
        content: content.to_string(),
        updated_at: updated,
        bytes: content.len(),
        lines: content.lines().count(),
    })
}

pub fn search(
    conn: &Connection,
    query: &str,
    project_root: Option<&str>,
) -> Result<Vec<SearchHit>> {
    let rows = if let Some(root) = project_root {
        query_all(
            conn,
            "SELECT file, content FROM files WHERE project_root = ?;",
            &[&root],
            |row| {
                let file: String = row.get(0)?;
                let content: String = row.get(1)?;
                Ok((file, content))
            },
        )?
    } else {
        query_all(conn, "SELECT file, content FROM files;", &[], |row| {
            let file: String = row.get(0)?;
            let content: String = row.get(1)?;
            Ok((file, content))
        })?
    };
    let mut hits = Vec::new();
    for (file, content) in rows {
        for (idx, line) in content.lines().enumerate() {
            if let Some(pos) = line.find(query) {
                hits.push(SearchHit {
                    file: file.clone(),
                    line: idx + 1,
                    column: pos,
                    text: line.to_string(),
                    score: 1.0,
                });
            }
        }
    }
    hits.sort_by(|a, b| b.score.total_cmp(&a.score));
    Ok(hits)
}
