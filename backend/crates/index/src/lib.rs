//! index: naive text indexing and search.

use anyhow::{anyhow, Result};
use rusqlite::Connection;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::cmp::Ordering;
use std::collections::BinaryHeap;
use std::env;
use storage::{exec, now_unix};

fn content_hash(content: &str) -> String {
    let mut hasher = Sha256::new();
    hasher.update(content.as_bytes());
    format!("{:x}", hasher.finalize())
}

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
    #[serde(skip_serializing_if = "Option::is_none")]
    pub kind: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub note_id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub note_summary: Option<String>,
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
        let timeout = env::var("HEMIS_EMBED_TIMEOUT_MS")
            .ok()
            .and_then(|s| s.parse::<u64>().ok())
            .unwrap_or(5_000);
        let client = reqwest::blocking::Client::builder()
            .timeout(std::time::Duration::from_millis(timeout))
            .build()
            .map_err(|e| anyhow!("embed client build failed: {e}"))?;
        let resp: EmbedResp = client
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
    let bin_hash = binary_hash(vector);
    let vec_norm = norm(vector);
    exec(
        conn,
        "INSERT OR REPLACE INTO embeddings (id, file, project_root, vector, text, binary_hash, norm, updated_at)
         VALUES (
            (SELECT id FROM embeddings WHERE file = ?1),
            ?1, ?2, ?3, ?4, ?5, ?6, ?7
         );",
        &[
            &file,
            &project_root,
            &serde_json::to_string(vector)?,
            &text,
            &bin_hash,
            &vec_norm,
            &updated,
        ],
    )?;
    Ok(())
}

// Note: embedding_from_row removed - now parse fields directly in semantic_search
// to avoid loading full vector when binary hash rejects the candidate

fn dot(a: &[f32], b: &[f32]) -> f32 {
    let len = a.len().min(b.len());
    let mut sum = 0.0;
    for i in 0..len {
        sum += a[i] * b[i];
    }
    sum
}

fn norm(v: &[f32]) -> f32 {
    dot(v, v).sqrt()
}

/// Compute binary hash of vector (sign bits packed into bytes)
fn binary_hash(v: &[f32]) -> Vec<u8> {
    let num_bytes = (v.len() + 7) / 8;
    let mut hash = vec![0u8; num_bytes];
    for (i, &val) in v.iter().enumerate() {
        if val >= 0.0 {
            hash[i / 8] |= 1 << (i % 8);
        }
    }
    hash
}

/// Count differing bits between two binary hashes (Hamming distance)
fn hamming_distance(a: &[u8], b: &[u8]) -> u32 {
    let len = a.len().min(b.len());
    let mut dist = 0u32;
    for i in 0..len {
        dist += (a[i] ^ b[i]).count_ones();
    }
    dist
}

pub fn upsert_embedding_for_file(
    conn: &Connection,
    file: &str,
    project_root: &str,
    content: &str,
) -> Result<()> {
    let input = embedding_input(content);
    // Use input (first 64 lines) for both embedder and fallback to keep vector/text consistent
    let vector = call_embedder(&input).unwrap_or_else(|_| derive_vector(&input));
    upsert_embedding(conn, file, project_root, &vector, &input)
}

/// Add or update a file in the index. Returns None if content unchanged.
pub fn add_file(
    conn: &Connection,
    file: &str,
    project_root: &str,
    content: &str,
) -> Result<Option<IndexedFile>> {
    let hash = content_hash(content);

    // Check if file exists with same content hash
    let existing_hash: Option<String> = conn
        .query_row(
            "SELECT content_hash FROM files WHERE file = ?;",
            [file],
            |row| row.get(0),
        )
        .ok();

    if existing_hash.as_ref() == Some(&hash) {
        return Ok(None); // Content unchanged, skip re-indexing
    }

    let updated = now_unix();
    exec(
        conn,
        "INSERT OR REPLACE INTO files (file, project_root, content, content_hash, updated_at) VALUES (?,?,?,?,?);",
        &[&file, &project_root, &content, &hash.as_str(), &updated],
    )?;
    upsert_embedding_for_file(conn, file, project_root, content)?;
    Ok(Some(IndexedFile {
        file: file.to_string(),
        project_root: project_root.to_string(),
        content: content.to_string(),
        updated_at: updated,
        bytes: content.len(),
        lines: content.lines().count(),
    }))
}

/// Escape special FTS5 characters and format query for matching
fn escape_fts_literal(value: &str) -> String {
    value.replace('"', "\"\"")
}

fn escape_fts_query(query: &str) -> Option<String> {
    let escaped: String = query
        .chars()
        .map(|c| match c {
            '"' | '*' | '^' | '-' | '+' | '(' | ')' | ':' => ' ',
            _ => c,
        })
        .collect();

    let terms: Vec<String> = escaped
        .split_whitespace()
        .filter(|s| !s.is_empty())
        .map(|s| format!("\"{}\"*", s))
        .collect();

    if terms.is_empty() {
        None
    } else {
        Some(terms.join(" OR "))
    }
}

pub fn search(
    conn: &Connection,
    query: &str,
    project_root: Option<&str>,
) -> Result<Vec<SearchHit>> {
    // Reject empty queries to avoid returning whole index
    if query.trim().is_empty() {
        return Ok(Vec::new());
    }

    // Use FTS5 for efficient full-text search - O(log N) instead of O(N)
    let base_query = match escape_fts_query(query) {
        Some(q) => q,
        None => return Ok(Vec::new()),
    };
    let fts_query = if let Some(root) = project_root {
        format!(
            "project_root:\"{}\" AND ({})",
            escape_fts_literal(root),
            base_query
        )
    } else {
        base_query
    };

    // FTS5 gives us matching files, then we scan those files for line positions
    let mut stmt;
    let mut rows;
    stmt = conn.prepare(
        r#"SELECT f.file, f.content FROM files f
           INNER JOIN files_fts fts ON f.rowid = fts.rowid
           WHERE files_fts MATCH ?;"#,
    )?;
    rows = stmt.query([fts_query.as_str()])?;

    let mut hits = Vec::new();
    let query_lower = query.to_lowercase();
    while let Some(row) = rows.next()? {
        let file: String = row.get(0)?;
        let content: String = row.get(1)?;
        // Scan matching files for exact line positions (case-insensitive to match FTS5)
        for (idx, line) in content.lines().enumerate() {
            let line_lower = line.to_lowercase();
            if let Some(pos) = line_lower.find(&query_lower) {
                hits.push(SearchHit {
                    file: file.clone(),
                    line: idx + 1,
                    column: pos,
                    text: line.to_string(),
                    score: 1.0,
                    kind: Some("file".into()),
                    note_id: None,
                    note_summary: None,
                });
            }
        }
    }
    Ok(hits)
}

/// Wrapper for min-heap ordering (we want top-k highest scores)
struct ScoredHit {
    score: f32,
    hit: SearchHit,
}

impl PartialEq for ScoredHit {
    fn eq(&self, other: &Self) -> bool {
        self.score == other.score
    }
}

impl Eq for ScoredHit {}

impl PartialOrd for ScoredHit {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ScoredHit {
    fn cmp(&self, other: &Self) -> Ordering {
        // Reverse order for min-heap (lowest score at top)
        other.score.total_cmp(&self.score)
    }
}

pub fn semantic_search(
    conn: &Connection,
    query_vector: &[f32],
    project_root: Option<&str>,
    top_k: usize,
) -> Result<Vec<SearchHit>> {
    // Pre-compute query binary hash for fast candidate filtering
    let query_hash = binary_hash(query_vector);
    let query_norm = norm(query_vector);

    // Two-phase filtering per row:
    // 1. Norm upper bound: skip if |q|*|v| can't beat current minimum (Cauchy-Schwarz)
    // 2. Hamming distance: skip if binary hash distance > threshold
    // 3. Exact dot product: only for candidates passing both filters
    //
    // This scans all rows but avoids expensive JSON parsing and dot products
    // for most candidates. No early termination - SQL order is arbitrary.

    let dim_bits = query_vector.len();
    // Allow up to 40% of bits different, but minimum of 1 for small vectors
    let hamming_threshold = ((dim_bits as f32 * 0.4) as u32).max(1);

    let mut heap: BinaryHeap<ScoredHit> = BinaryHeap::with_capacity(top_k + 1);

    let mut stmt;
    let mut rows;
    if let Some(root) = project_root {
        stmt = conn.prepare(
            "SELECT file, vector, text, binary_hash, norm FROM embeddings WHERE project_root = ?;",
        )?;
        rows = stmt.query([root])?;
    } else {
        stmt = conn.prepare("SELECT file, vector, text, binary_hash, norm FROM embeddings;")?;
        rows = stmt.query([])?;
    }

    while let Some(row) = rows.next()? {
        let bin_hash: Option<Vec<u8>> = row.get("binary_hash").ok();
        let emb_norm: Option<f64> = row.get("norm").ok();

        // Fast rejection 1: norm upper bound (Cauchy-Schwarz inequality)
        // dot(q,v) <= |q|*|v|, so skip if upper bound can't beat current min
        if let (Some(min_hit), Some(emb_n)) = (heap.peek(), emb_norm) {
            let upper_bound = query_norm * emb_n as f32;
            if upper_bound <= min_hit.score && heap.len() >= top_k {
                continue;
            }
        }

        // Fast rejection 2: Hamming distance on binary hash
        // High Hamming distance correlates with low cosine similarity
        if let Some(ref hash) = bin_hash {
            let dist = hamming_distance(&query_hash, hash);
            if dist > hamming_threshold && heap.len() >= top_k {
                continue;
            }
        }

        // Passed filters - compute exact dot product
        let vector_str: String = row.get("vector")?;
        let vector: Vec<f32> = serde_json::from_str(&vector_str).unwrap_or_default();
        let score = dot(&vector, query_vector);

        let hit = SearchHit {
            file: row.get("file")?,
            line: 1,
            column: 0,
            text: row.get("text")?,
            score,
            kind: Some("semantic".into()),
            note_id: None,
            note_summary: None,
        };

        if heap.len() < top_k {
            heap.push(ScoredHit { score, hit });
        } else if let Some(min) = heap.peek() {
            if score > min.score {
                heap.pop();
                heap.push(ScoredHit { score, hit });
            }
        }
    }

    // Extract results in descending score order
    let mut hits: Vec<_> = heap.into_iter().map(|sh| sh.hit).collect();
    hits.sort_by(|a, b| b.score.total_cmp(&a.score));
    Ok(hits)
}
