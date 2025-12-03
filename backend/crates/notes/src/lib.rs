//! notes: note models and operations.

use anyhow::Result;
use git::GitInfo;
use once_cell::sync::Lazy;
use regex::Regex;
use rusqlite;
use rusqlite::Connection;
use serde::{Deserialize, Serialize};
use storage::{exec, now_unix, query_all};
use uuid::Uuid;

/// Regex for extracting note links in [[desc][id]] format.
static LINK_RE: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"\[\[[^\]]*\]\[([a-f0-9-]{36})\]\]").unwrap());

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct Note {
    pub id: String,
    pub file: String,
    pub project_root: String,
    pub line: i64,
    pub column: i64,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub node_path: Option<serde_json::Value>,
    #[serde(default)]
    pub tags: serde_json::Value,
    pub text: String,
    pub summary: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub commit_sha: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub blob_sha: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub node_text_hash: Option<String>,
    #[serde(default)]
    pub stale: bool,
    pub created_at: i64,
    pub updated_at: i64,
}

pub struct NoteFilters<'a> {
    pub file: &'a str,
    pub project_root: &'a str,
    pub node_path: Option<serde_json::Value>,
    pub commit: Option<&'a str>,
    pub blob: Option<&'a str>,
    pub include_stale: bool,
}
impl<'a> Clone for NoteFilters<'a> {
    fn clone(&self) -> Self {
        Self {
            file: self.file,
            project_root: self.project_root,
            node_path: self.node_path.clone(),
            commit: self.commit,
            blob: self.blob,
            include_stale: self.include_stale,
        }
    }
}

fn summarize(text: &str) -> String {
    // Get first line only for summary
    let first_line = text.lines().next().unwrap_or(text);
    if first_line.chars().count() <= 60 {
        first_line.to_string()
    } else {
        // Find char boundary at or before position 57
        let boundary = first_line
            .char_indices()
            .take(57)
            .last()
            .map(|(i, c)| i + c.len_utf8())
            .unwrap_or(0);
        format!("{}...", &first_line[..boundary])
    }
}

/// Extract note IDs from links in the format [[desc][id]].
fn extract_links(text: &str) -> Vec<String> {
    LINK_RE
        .captures_iter(text)
        .filter_map(|cap| cap.get(1).map(|m| m.as_str().to_string()))
        .collect()
}

/// Update edges table: remove old edges from src, insert new edges.
fn update_edges(conn: &Connection, src: &str, project_root: &str, text: &str) -> Result<()> {
    // Remove existing edges from this note
    exec(conn, "DELETE FROM edges WHERE src = ?;", &[&src])?;
    // Insert new edges
    let links = extract_links(text);
    let ts = now_unix();
    for dst in links {
        exec(
            conn,
            "INSERT INTO edges (src, dst, kind, project_root, updated_at) VALUES (?, ?, ?, ?, ?);",
            &[&src, &dst, &"link", &project_root, &ts],
        )?;
    }
    Ok(())
}

pub fn create(
    conn: &Connection,
    file: &str,
    project_root: &str,
    line: i64,
    column: i64,
    node_path: Option<serde_json::Value>,
    tags: serde_json::Value,
    text: &str,
    git: Option<GitInfo>,
    node_text_hash: Option<String>,
) -> Result<Note> {
    let id = Uuid::new_v4().to_string();
    let ts = now_unix();
    let summary = summarize(text);
    let (commit, blob) = git
        .map(|g| (Some(g.commit), g.blob))
        .unwrap_or((None, None));
    let tags_str = serde_json::to_string(&tags).unwrap_or_else(|_| "[]".to_string());
    let node_path_str = node_path
        .as_ref()
        .and_then(|v| serde_json::to_string(v).ok());
    exec(conn, "INSERT INTO notes (id,file,project_root,line,column,node_path,tags,text,summary,commit_sha,blob_sha,node_text_hash,created_at,updated_at) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?)",
         &[&id, &file, &project_root, &line, &column, &node_path_str, &tags_str, &text, &summary, &commit, &blob, &node_text_hash, &ts, &ts])?;
    // Parse and store links to other notes
    update_edges(conn, &id, project_root, text)?;
    Ok(Note {
        id,
        file: file.to_string(),
        project_root: project_root.to_string(),
        line,
        column,
        node_path,
        tags,
        text: text.to_string(),
        summary,
        commit_sha: commit,
        blob_sha: blob,
        node_text_hash,
        stale: false,
        created_at: ts,
        updated_at: ts,
    })
}

fn map_note(row: &rusqlite::Row<'_>, stale: bool) -> Result<Note> {
    let node_path: Option<String> = row.get("node_path").ok();
    let tags: Option<String> = row.get("tags").ok();
    Ok(Note {
        id: row.get("id")?,
        file: row.get("file")?,
        project_root: row.get("project_root")?,
        line: row.get("line")?,
        column: row.get("column")?,
        node_path: node_path.and_then(|s| serde_json::from_str(&s).ok()),
        tags: tags
            .and_then(|s| serde_json::from_str(&s).ok())
            .unwrap_or_else(|| serde_json::Value::Array(vec![])),
        text: row.get("text")?,
        summary: row.get("summary")?,
        commit_sha: row.get("commit_sha").ok(),
        blob_sha: row.get("blob_sha").ok(),
        node_text_hash: row.get("node_text_hash").ok(),
        stale,
        created_at: row.get("created_at")?,
        updated_at: row.get("updated_at")?,
    })
}

fn stale(
    note_commit: &Option<String>,
    note_blob: &Option<String>,
    commit: Option<&str>,
    blob: Option<&str>,
) -> bool {
    // Blob SHA is the most reliable indicator - if file content matches, not stale
    // Only fall back to commit comparison if we don't have blob info
    match (note_blob, blob, note_commit, commit) {
        // Both have blob SHA - compare blobs (content-based)
        (Some(nb), Some(b), _, _) => nb != b,
        // No blob info, fall back to commit comparison
        (_, _, Some(nc), Some(c)) => nc != c,
        // Not enough info to determine staleness
        _ => false,
    }
}

pub fn list_for_file(conn: &Connection, filters: NoteFilters<'_>) -> Result<Vec<Note>> {
    let rows = query_all(
        conn,
        "SELECT * FROM notes WHERE file = ? ORDER BY updated_at DESC;",
        &[&filters.file],
        |row| {
            let note_commit: Option<String> = row.get("commit_sha").ok();
            let note_blob: Option<String> = row.get("blob_sha").ok();
            let is_stale = stale(&note_commit, &note_blob, filters.commit, filters.blob);
            map_note(row, is_stale)
        },
    )?;
    Ok(rows)
}

pub fn list_by_node(conn: &Connection, filters: NoteFilters<'_>) -> Result<Vec<Note>> {
    let np = filters
        .node_path
        .as_ref()
        .and_then(|v| serde_json::to_string(v).ok());
    let rows = query_all(conn,
        "SELECT * FROM notes WHERE file = ? AND project_root = ? AND node_path = ? ORDER BY updated_at DESC;",
        &[&filters.file, &filters.project_root, &np],
        |row| {
            let note_commit: Option<String> = row.get("commit_sha").ok();
            let note_blob: Option<String> = row.get("blob_sha").ok();
            let is_stale = stale(&note_commit, &note_blob, filters.commit, filters.blob);
            map_note(row, is_stale)
        })?;
    Ok(rows)
}

pub fn get(conn: &Connection, id: &str) -> Result<Note> {
    let rows = query_all(conn, "SELECT * FROM notes WHERE id = ?;", &[&id], |row| {
        map_note(row, false)
    })?;
    rows.into_iter()
        .next()
        .ok_or_else(|| anyhow::anyhow!("note not found"))
}

pub fn list_project(
    conn: &Connection,
    project_root: &str,
    limit: Option<usize>,
    offset: usize,
) -> Result<Vec<Note>> {
    // Cap limit to prevent DoS via excessive memory allocation
    const MAX_LIMIT: i64 = 10000;
    const DEFAULT_LIMIT: i64 = 100; // Default limit when none specified
    let limit_val: i64 = limit.map(|l| (l as i64).min(MAX_LIMIT)).unwrap_or(DEFAULT_LIMIT);
    let offset_val = offset as i64;

    // Use parameterized query to prevent SQL injection
    let mut stmt = conn.prepare(
        "SELECT * FROM notes WHERE project_root = ? ORDER BY updated_at DESC LIMIT ? OFFSET ?;",
    )?;
    let mut rows = stmt.query(rusqlite::params![project_root, limit_val, offset_val])?;
    let mut notes = Vec::new();
    while let Some(row) = rows.next()? {
        notes.push(map_note(row, false)?);
    }
    Ok(notes)
}

pub fn delete(conn: &Connection, id: &str) -> Result<bool> {
    // Remove edges where this note is source or target
    exec(
        conn,
        "DELETE FROM edges WHERE src = ? OR dst = ?;",
        &[&id, &id],
    )?;
    let count = exec(conn, "DELETE FROM notes WHERE id = ?;", &[&id])?;
    Ok(count > 0)
}

pub fn update(
    conn: &Connection,
    id: &str,
    text: Option<&str>,
    tags: Option<serde_json::Value>,
) -> Result<Note> {
    let mut note = get(conn, id)?;
    let new_text = text.unwrap_or(&note.text).to_string();
    let new_tags = tags.unwrap_or(note.tags.clone());
    let summary = summarize(&new_text);
    let now = now_unix();
    exec(
        conn,
        "UPDATE notes SET text = ?, summary = ?, tags = ?, updated_at = ? WHERE id = ?;",
        &[
            &new_text,
            &summary,
            &serde_json::to_string(&new_tags)?,
            &now,
            &id,
        ],
    )?;
    // Parse and store links to other notes
    update_edges(conn, id, &note.project_root, &new_text)?;
    note.text = new_text;
    note.tags = new_tags;
    note.summary = summary;
    note.updated_at = now;
    Ok(note)
}

pub fn search(
    conn: &Connection,
    query: &str,
    project_root: Option<&str>,
    limit: Option<usize>,
    offset: usize,
) -> Result<Vec<Note>> {
    if query.trim().is_empty() {
        return Ok(Vec::new());
    }

    // Use FTS5 for efficient full-text search
    // Escape special FTS5 characters and wrap terms for prefix matching
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

    let pagination = match limit {
        Some(lim) => format!("LIMIT {} OFFSET {}", lim, offset),
        None => format!("LIMIT -1 OFFSET {}", offset),
    };

    let sql = format!(
        r#"SELECT n.* FROM notes n
           INNER JOIN notes_fts fts ON n.rowid = fts.rowid
           WHERE notes_fts MATCH ?
           ORDER BY n.updated_at DESC {};"#,
        pagination
    );

    let rows = query_all(conn, &sql, &[&fts_query.as_str()], |row| {
        map_note(row, false)
    })?;
    Ok(rows)
}

/// Escape special FTS5 characters and format query for substring matching
fn escape_fts_query(query: &str) -> Option<String> {
    // FTS5 special characters that need escaping: " * ^ - + ( ) : OR AND NOT
    // For substring matching, we wrap each word with * for prefix matching
    let escaped: String = query
        .chars()
        .map(|c| match c {
            '"' | '*' | '^' | '-' | '+' | '(' | ')' | ':' => ' ',
            _ => c,
        })
        .collect();

    // Split into words and add prefix matching
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

/// Escape a literal string for use in FTS5 column filter (e.g., project_root:"value")
/// FTS5 requires escaping double quotes within quoted strings
fn escape_fts_literal(value: &str) -> String {
    // In FTS5 double-quoted strings, double quotes are escaped by doubling them
    // Other special chars (* ^ etc.) are treated literally inside quotes
    value.replace('"', "\"\"")
}

/// Find all notes that link TO the given note (backlinks).
pub fn backlinks(conn: &Connection, note_id: &str) -> Result<Vec<Note>> {
    // Find notes where src links to note_id (dst)
    let sql = r#"
        SELECT n.* FROM notes n
        INNER JOIN edges e ON e.src = n.id
        WHERE e.dst = ?
        ORDER BY n.updated_at DESC;
    "#;
    query_all(conn, sql, &[&note_id], |row| map_note(row, false))
}

/// Reattach a note to a new position, updating commit/blob SHAs and node_text_hash.
pub fn reattach(
    conn: &Connection,
    id: &str,
    line: i64,
    column: i64,
    node_path: Option<serde_json::Value>,
    git: Option<GitInfo>,
    node_text_hash: Option<String>,
) -> Result<Note> {
    let mut note = get(conn, id)?;
    let now = now_unix();
    let (commit, blob) = git
        .map(|g| (Some(g.commit), g.blob))
        .unwrap_or((None, None));
    let node_path_str = node_path
        .as_ref()
        .and_then(|v| serde_json::to_string(v).ok());
    exec(
        conn,
        "UPDATE notes SET line = ?, column = ?, node_path = ?, commit_sha = ?, blob_sha = ?, node_text_hash = ?, updated_at = ? WHERE id = ?;",
        &[&line, &column, &node_path_str, &commit, &blob, &node_text_hash, &now, &id],
    )?;
    note.line = line;
    note.column = column;
    note.node_path = node_path;
    note.commit_sha = commit;
    note.blob_sha = blob;
    note.node_text_hash = node_text_hash;
    note.stale = false;
    note.updated_at = now;
    Ok(note)
}
