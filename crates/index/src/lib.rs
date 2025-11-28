//! index: naive text indexing and search.

use anyhow::{anyhow, Result};
use rusqlite::Connection;
use serde::{Deserialize, Serialize};
use std::env;
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

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct Embedding {
    pub file: String,
    pub project_root: String,
    pub vector: Vec<f32>,
    pub text: String,
    pub updated_at: i64,
}

fn derive_vector(content: &str) -> Vec<f32> {
    vec![content.len() as f32, content.lines().count() as f32]
}

fn embedding_input(content: &str) -> String {
    content.lines().take(64).collect::<Vec<_>>().join("\n")
}

fn call_embedder(text: &str) -> Result<Vec<f32>> {
    if text.is_empty() {
        return Err(anyhow!("empty text"));
    }
    if let Ok(url) = env::var("HEMIS_EMBED_URL") {
        #[derive(Deserialize)]
        struct EmbedResp {
            vector: Vec<f32>,
        }
        let resp: EmbedResp = reqwest::blocking::Client::new()
            .post(url)
            .json(&serde_json::json!({ "text": text }))
            .send()
            .map_err(|e| anyhow!("embed request failed: {e}"))?
            .json()
            .map_err(|e| anyhow!("embed decode failed: {e}"))?;
        return Ok(resp.vector);
    }
    Ok(vec![text.len() as f32, 1.0])
}

fn upsert_embedding(
    conn: &Connection,
    file: &str,
    project_root: &str,
    vector: &[f32],
    text: &str,
) -> Result<()> {
    let updated = now_unix();
    exec(
        conn,
        "INSERT OR REPLACE INTO embeddings (id, file, project_root, vector, text, updated_at)
         VALUES (
            (SELECT id FROM embeddings WHERE file = ?1),
            ?1, ?2, ?3, ?4, ?5
         );",
        &[
            &file,
            &project_root,
            &serde_json::to_string(vector)?,
            &text,
            &updated,
        ],
    )?;
    Ok(())
}

fn embedding_from_row(row: &rusqlite::Row<'_>) -> Result<Embedding> {
    let vector_str: String = row.get("vector")?;
    let vector: Vec<f32> = serde_json::from_str(&vector_str).unwrap_or_default();
    Ok(Embedding {
        file: row.get("file")?,
        project_root: row.get("project_root")?,
        vector,
        text: row.get("text")?,
        updated_at: row.get("updated_at")?,
    })
}

fn dot(a: &[f32], b: &[f32]) -> f32 {
    let len = a.len().min(b.len());
    let mut sum = 0.0;
    for i in 0..len {
        sum += a[i] * b[i];
    }
    sum
}

pub fn upsert_embedding_for_file(
    conn: &Connection,
    file: &str,
    project_root: &str,
    content: &str,
) -> Result<()> {
    let vector =
        call_embedder(&embedding_input(content)).unwrap_or_else(|_| derive_vector(content));
    upsert_embedding(conn, file, project_root, &vector, content)
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
    upsert_embedding_for_file(conn, file, project_root, content)?;
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

pub fn semantic_search(
    conn: &Connection,
    query_vector: &[f32],
    project_root: Option<&str>,
    top_k: usize,
) -> Result<Vec<SearchHit>> {
    let rows = if let Some(root) = project_root {
        query_all(
            conn,
            "SELECT * FROM embeddings WHERE project_root = ?;",
            &[&root],
            embedding_from_row,
        )?
    } else {
        query_all(conn, "SELECT * FROM embeddings;", &[], embedding_from_row)?
    };
    let mut hits = Vec::new();
    for emb in rows {
        let score = dot(&emb.vector, query_vector);
        hits.push(SearchHit {
            file: emb.file.clone(),
            line: 1,
            column: 0,
            text: emb.text.clone(),
            score,
        });
    }
    hits.sort_by(|a, b| b.score.total_cmp(&a.score));
    if hits.len() > top_k {
        hits.truncate(top_k);
    }
    Ok(hits)
}
