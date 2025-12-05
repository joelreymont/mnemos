//! index: naive text indexing and search.

use anyhow::{anyhow, Result};
use rusqlite::Connection;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::cmp::Ordering;
use std::collections::BinaryHeap;
use std::env;
use storage::{exec, now_unix};

/// Maximum vector dimension (covers most embedding models: OpenAI 3072, Cohere 4096)
const MAX_VECTOR_DIM: usize = 4096;

/// Validate that a vector contains only finite values and reasonable dimensions.
fn validate_vector(v: &[f32]) -> Result<()> {
    if v.len() > MAX_VECTOR_DIM {
        return Err(anyhow!(
            "vector dimension {} exceeds maximum {}",
            v.len(),
            MAX_VECTOR_DIM
        ));
    }
    for (i, &val) in v.iter().enumerate() {
        if !val.is_finite() {
            return Err(anyhow!("vector contains non-finite value at index {}: {}", i, val));
        }
    }
    Ok(())
}

/// Convert vector to binary blob (little-endian f32 bytes)
fn vector_to_blob(v: &[f32]) -> Vec<u8> {
    let mut blob = Vec::with_capacity(v.len() * 4);
    for &val in v {
        blob.extend_from_slice(&val.to_le_bytes());
    }
    blob
}

/// Convert binary blob back to vector
fn blob_to_vector(blob: &[u8]) -> Option<Vec<f32>> {
    if blob.len() % 4 != 0 {
        return None;
    }
    let mut vec = Vec::with_capacity(blob.len() / 4);
    for chunk in blob.chunks_exact(4) {
        let bytes: [u8; 4] = chunk.try_into().ok()?;
        vec.push(f32::from_le_bytes(bytes));
    }
    Some(vec)
}

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

/// Check if an IP address is in a private/internal range
fn is_private_ip(ip: &std::net::IpAddr) -> bool {
    match ip {
        std::net::IpAddr::V4(ipv4) => {
            ipv4.is_loopback()           // 127.0.0.0/8
                || ipv4.is_private()      // 10.0.0.0/8, 172.16.0.0/12, 192.168.0.0/16
                || ipv4.is_link_local()   // 169.254.0.0/16
                || ipv4.is_broadcast()
                || ipv4.is_unspecified()
                || ipv4.octets()[0] == 100 && (ipv4.octets()[1] & 0xC0) == 64 // 100.64.0.0/10 (CGNAT)
        }
        std::net::IpAddr::V6(ipv6) => {
            ipv6.is_loopback() || ipv6.is_unspecified()
                // IPv4-mapped addresses
                || ipv6.to_ipv4_mapped().map(|v4| {
                    v4.is_loopback() || v4.is_private() || v4.is_link_local()
                }).unwrap_or(false)
        }
    }
}

/// Validate embedder URL to prevent SSRF attacks
fn validate_embedder_url(url: &str) -> Result<()> {
    // Must be HTTPS for security (unless localhost for development)
    let parsed = url::Url::parse(url).map_err(|e| anyhow!("invalid embedder URL: {e}"))?;

    let scheme = parsed.scheme();
    let host = parsed.host_str().unwrap_or("");

    // Allow HTTP only for localhost/127.0.0.1 (development)
    let is_localhost = host == "localhost" || host == "127.0.0.1" || host == "::1";

    if scheme != "https" && !(scheme == "http" && is_localhost) {
        return Err(anyhow!("embedder URL must use HTTPS (or HTTP for localhost)"));
    }

    // Block internal/private IP ranges to prevent SSRF
    if !is_localhost {
        // Block common internal hostnames
        let blocked_hosts = ["metadata", "metadata.google.internal", "169.254.169.254"];
        if blocked_hosts.contains(&host) {
            return Err(anyhow!("embedder URL points to blocked internal host"));
        }

        // Resolve DNS and check if IP is private (prevents DNS rebinding)
        let port = parsed.port().unwrap_or(if scheme == "https" { 443 } else { 80 });
        let socket_addr = format!("{}:{}", host, port);
        if let Ok(addrs) = std::net::ToSocketAddrs::to_socket_addrs(&socket_addr.as_str()) {
            for addr in addrs {
                if is_private_ip(&addr.ip()) {
                    return Err(anyhow!("embedder URL resolves to private IP address"));
                }
            }
        }
        // Note: If DNS resolution fails, we let the request proceed and fail naturally
        // This avoids blocking legitimate URLs due to temporary DNS issues
    }

    Ok(())
}

fn call_embedder(text: &str) -> Result<Vec<f32>> {
    if text.is_empty() {
        return Err(anyhow!("empty text"));
    }
    if let Ok(url) = env::var("HEMIS_EMBED_URL") {
        // Validate URL before making request (SSRF protection)
        validate_embedder_url(&url)?;

        #[derive(Deserialize)]
        struct EmbedResp {
            vector: Vec<f32>,
        }
        // Cap timeout to prevent resource exhaustion (max 30 seconds)
        const MAX_TIMEOUT_MS: u64 = 30_000;
        let timeout = env::var("HEMIS_EMBED_TIMEOUT_MS")
            .ok()
            .and_then(|s| s.parse::<u64>().ok())
            .unwrap_or(5_000)
            .min(MAX_TIMEOUT_MS);
        let client = reqwest::blocking::Client::builder()
            .timeout(std::time::Duration::from_millis(timeout))
            .build()
            .map_err(|e| anyhow!("embed client build failed: {e}"))?;
        let resp: EmbedResp = client
            .post(&url)
            .json(&serde_json::json!({ "text": text }))
            .send()
            .map_err(|e| anyhow!("embed request failed: {e}"))?
            .json()
            .map_err(|e| anyhow!("embed decode failed: {e}"))?;
        // Validate vector from external embedder
        validate_vector(&resp.vector)?;
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
    let vec_blob = vector_to_blob(vector);
    exec(
        conn,
        "INSERT OR REPLACE INTO embeddings (id, file, project_root, vector, text, binary_hash, norm, vector_blob, updated_at)
         VALUES (
            (SELECT id FROM embeddings WHERE file = ?1),
            ?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8
         );",
        &[
            &file,
            &project_root,
            &serde_json::to_string(vector)?,
            &text,
            &bin_hash,
            &vec_norm,
            &vec_blob,
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

/// Add or update a file in the index. Returns None if content unchanged and embeddings exist.
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
        // Content unchanged - but check if embeddings are missing or incomplete
        // This can happen after snapshot restore or previous embedder failure
        let has_embedding: bool = conn
            .query_row(
                "SELECT 1 FROM embeddings WHERE file = ? AND binary_hash IS NOT NULL AND norm IS NOT NULL;",
                [file],
                |_| Ok(true),
            )
            .unwrap_or(false);

        if has_embedding {
            return Ok(None); // Content unchanged and embeddings exist
        }
        // Embeddings missing - recompute them below
        upsert_embedding_for_file(conn, file, project_root, content)?;
        return Ok(None); // Content unchanged, just fixed embeddings
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
/// Escape a literal string for use in FTS5 column filter (e.g., project_root:"value")
/// FTS5 requires escaping double quotes within quoted strings
fn escape_fts_literal(value: &str) -> String {
    // Truncate to prevent query explosion (each quote doubles)
    const MAX_LITERAL_LEN: usize = 1000;
    let truncated = if value.len() > MAX_LITERAL_LEN {
        &value[..MAX_LITERAL_LEN]
    } else {
        value
    };
    // In FTS5 double-quoted strings, double quotes are escaped by doubling them
    // Other special chars (* ^ etc.) are treated literally inside quotes
    truncated.replace('"', "\"\"")
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

    // Limits to prevent DoS via expensive searches
    const MAX_FILE_SIZE: usize = 1 * 1024 * 1024; // 1MB (reduced from 10MB)
    const MAX_LINES_PER_FILE: usize = 10_000; // Reduced from 100K
    const MAX_TOTAL_HITS: usize = 500; // Reduced from 1K
    const MAX_FILES_TO_SCAN: usize = 500; // Reduced from 1K

    // FTS5 gives us matching files, then we scan those files for line positions
    // Limit at SQL level to avoid fetching more files than we'll scan
    let mut stmt;
    let mut rows;
    stmt = conn.prepare(
        r#"SELECT f.file, f.content FROM files f
           INNER JOIN files_fts fts ON f.rowid = fts.rowid
           WHERE files_fts MATCH ?
           LIMIT ?;"#,
    )?;
    rows = stmt.query(rusqlite::params![fts_query.as_str(), MAX_FILES_TO_SCAN])?;

    let mut hits = Vec::new();
    let query_lower = query.to_lowercase();
    // Tokenize query for multi-term matching: a line matches if it contains ALL terms
    let query_terms: Vec<&str> = query_lower.split_whitespace().collect();
    let mut files_scanned = 0;

    while let Some(row) = rows.next()? {
        // Limit total files scanned
        if files_scanned >= MAX_FILES_TO_SCAN {
            break;
        }
        files_scanned += 1;

        let file: String = row.get(0)?;
        let content: String = row.get(1)?;

        // Skip oversized files to prevent memory/CPU exhaustion
        if content.len() > MAX_FILE_SIZE {
            continue;
        }

        // Scan matching files for line positions (case-insensitive to match FTS5)
        for (idx, line) in content.lines().take(MAX_LINES_PER_FILE).enumerate() {
            // Stop if we have enough hits
            if hits.len() >= MAX_TOTAL_HITS {
                break;
            }

            let line_lower = line.to_lowercase();
            // Check if all query terms appear in the line
            let all_terms_match = query_terms.iter().all(|term| line_lower.contains(term));
            if all_terms_match {
                // Find position of first term for column
                let pos = query_terms.first()
                    .and_then(|term| line_lower.find(term))
                    .unwrap_or(0);
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

        // Early exit if we have enough hits
        if hits.len() >= MAX_TOTAL_HITS {
            break;
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
    // Validate query vector
    validate_vector(query_vector)?;

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
            "SELECT file, vector, text, binary_hash, norm, vector_blob FROM embeddings WHERE project_root = ?;",
        )?;
        rows = stmt.query([root])?;
    } else {
        stmt = conn.prepare("SELECT file, vector, text, binary_hash, norm, vector_blob FROM embeddings;")?;
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

        // Passed filters - load vector (prefer binary blob over JSON for speed)
        let vector_blob: Option<Vec<u8>> = row.get("vector_blob").ok();
        let vector: Vec<f32> = if let Some(blob) = vector_blob {
            // Fast path: read from binary blob
            match blob_to_vector(&blob) {
                Some(v) => v,
                None => continue, // Malformed blob
            }
        } else {
            // Fallback: parse JSON (for old data before migration)
            let vector_str: String = row.get("vector")?;
            const MAX_VECTOR_STR_LEN: usize = 1024 * 1024;
            if vector_str.len() > MAX_VECTOR_STR_LEN {
                continue;
            }
            match serde_json::from_str(&vector_str) {
                Ok(v) => v,
                Err(_) => continue,
            }
        };
        // Skip vectors with non-finite values (NaN/Infinity from corrupted data)
        if validate_vector(&vector).is_err() {
            continue;
        }
        // Skip vectors with mismatched dimensions (comparing different embedding models)
        if vector.len() != query_vector.len() {
            let file: String = row.get("file").unwrap_or_default();
            eprintln!(
                "[index] Warning: embedding dimension mismatch for {}: {} vs {} (expected)",
                file,
                vector.len(),
                query_vector.len()
            );
            continue;
        }
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

#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck_macros::quickcheck;

    // Property: escape_fts_query always produces valid FTS5 syntax (no unbalanced quotes)
    #[quickcheck]
    fn prop_escape_fts_query_balanced_quotes(input: String) -> bool {
        match escape_fts_query(&input) {
            None => true, // Empty result is valid
            Some(result) => {
                // Count quotes - should be even (balanced)
                let quote_count = result.chars().filter(|&c| c == '"').count();
                quote_count % 2 == 0
            }
        }
    }

    // Property: escape_fts_query never panics on any input
    #[quickcheck]
    fn prop_escape_fts_query_never_panics(input: String) -> bool {
        let _ = escape_fts_query(&input);
        true
    }

    // Property: escape_fts_literal never panics and produces valid output
    #[quickcheck]
    fn prop_escape_fts_literal_never_panics(input: String) -> bool {
        let result = escape_fts_literal(&input);
        // Verify doubling worked: count quotes in output should be 2x quotes in input
        // (capped by truncation at 1000 chars)
        let input_quotes = input.chars().take(1000).filter(|&c| c == '"').count();
        let output_quotes = result.chars().filter(|&c| c == '"').count();
        output_quotes == input_quotes * 2
    }

    // Property: escape_fts_literal truncates long inputs
    #[quickcheck]
    fn prop_escape_fts_literal_bounded_length(input: String) -> bool {
        let result = escape_fts_literal(&input);
        // Output can be at most 2x input length (each char could become 2 chars if all quotes)
        // But also bounded by MAX_LITERAL_LEN * 2
        result.len() <= 2000 // MAX_LITERAL_LEN is 1000, worst case doubles
    }

    // Property: binary_hash is deterministic
    #[quickcheck]
    fn prop_binary_hash_deterministic(v: Vec<i8>) -> bool {
        let floats: Vec<f32> = v.iter().map(|&x| x as f32).collect();
        binary_hash(&floats) == binary_hash(&floats)
    }

    // Property: hamming_distance is symmetric
    #[quickcheck]
    fn prop_hamming_symmetric(a: Vec<u8>, b: Vec<u8>) -> bool {
        hamming_distance(&a, &b) == hamming_distance(&b, &a)
    }

    // Property: hamming_distance(x, x) == 0
    #[quickcheck]
    fn prop_hamming_self_zero(v: Vec<u8>) -> bool {
        hamming_distance(&v, &v) == 0
    }

    // Property: hamming_distance is bounded by min length * 8
    #[quickcheck]
    fn prop_hamming_bounded(a: Vec<u8>, b: Vec<u8>) -> bool {
        let min_len = a.len().min(b.len());
        let dist = hamming_distance(&a, &b);
        dist <= (min_len * 8) as u32
    }

    // Property: validate_vector accepts finite vectors within dimension limit
    #[quickcheck]
    fn prop_validate_vector_finite_ok(v: Vec<i16>) -> bool {
        // Convert to f32, ensuring finite values
        let floats: Vec<f32> = v.iter().take(100).map(|&x| x as f32).collect();
        validate_vector(&floats).is_ok()
    }

    // Property: validate_vector rejects NaN
    #[test]
    fn test_validate_vector_rejects_nan() {
        let v = vec![1.0, f32::NAN, 3.0];
        assert!(validate_vector(&v).is_err());
    }

    // Property: validate_vector rejects Infinity
    #[test]
    fn test_validate_vector_rejects_infinity() {
        let v = vec![1.0, f32::INFINITY, 3.0];
        assert!(validate_vector(&v).is_err());
        let v = vec![1.0, f32::NEG_INFINITY, 3.0];
        assert!(validate_vector(&v).is_err());
    }

    // Property: validate_vector rejects oversized vectors
    #[test]
    fn test_validate_vector_rejects_oversized() {
        let v = vec![1.0; MAX_VECTOR_DIM + 1];
        assert!(validate_vector(&v).is_err());
    }

    // Property: dot product is commutative
    #[quickcheck]
    fn prop_dot_commutative(a: Vec<i8>, b: Vec<i8>) -> bool {
        let af: Vec<f32> = a.iter().map(|&x| x as f32).collect();
        let bf: Vec<f32> = b.iter().map(|&x| x as f32).collect();
        (dot(&af, &bf) - dot(&bf, &af)).abs() < 1e-6
    }

    // Property: norm is non-negative
    #[quickcheck]
    fn prop_norm_non_negative(v: Vec<i8>) -> bool {
        let floats: Vec<f32> = v.iter().map(|&x| x as f32).collect();
        norm(&floats) >= 0.0
    }

    // Property: norm of zero vector is zero
    #[test]
    fn test_norm_zero_vector() {
        let v: Vec<f32> = vec![0.0; 10];
        assert!((norm(&v) - 0.0).abs() < 1e-10);
    }

    // Property: binary_hash length matches expected
    #[quickcheck]
    fn prop_binary_hash_length(v: Vec<i8>) -> bool {
        let floats: Vec<f32> = v.iter().map(|&x| x as f32).collect();
        let hash = binary_hash(&floats);
        let expected_len = (floats.len() + 7) / 8;
        hash.len() == expected_len
    }
}
