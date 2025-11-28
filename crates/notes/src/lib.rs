//! notes: note models and operations.

use anyhow::Result;
use git::GitInfo;
use rusqlite;
use rusqlite::Connection;
use serde::{Deserialize, Serialize};
use storage::{exec, now_unix, query_all};
use uuid::Uuid;

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
    if text.len() <= 60 {
        text.to_string()
    } else {
        format!("{}...", &text[..57])
    }
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
        .map(|v| serde_json::to_string(v).unwrap());
    exec(conn, "INSERT INTO notes (id,file,project_root,line,column,node_path,tags,text,summary,commit_sha,blob_sha,created_at,updated_at) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)",
         &[&id, &file, &project_root, &line, &column, &node_path_str, &tags_str, &text, &summary, &commit, &blob, &ts, &ts])?;
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
    match (note_commit, note_blob, commit, blob) {
        (Some(nc), _, Some(c), _) if nc != c => true,
        (_, Some(nb), _, Some(b)) if nb != b => true,
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
    Ok(rows
        .into_iter()
        .filter(|n| filters.include_stale || !n.stale)
        .collect())
}

pub fn list_by_node(conn: &Connection, filters: NoteFilters<'_>) -> Result<Vec<Note>> {
    let np = filters
        .node_path
        .as_ref()
        .map(|v| serde_json::to_string(v).unwrap());
    let rows = query_all(conn,
        "SELECT * FROM notes WHERE file = ? AND project_root = ? AND node_path = ? ORDER BY updated_at DESC;",
        &[&filters.file, &filters.project_root, &np],
        |row| {
            let note_commit: Option<String> = row.get("commit_sha").ok();
            let note_blob: Option<String> = row.get("blob_sha").ok();
            let is_stale = stale(&note_commit, &note_blob, filters.commit, filters.blob);
            map_note(row, is_stale)
        })?;
    Ok(rows
        .into_iter()
        .filter(|n| filters.include_stale || !n.stale)
        .collect())
}

pub fn get(conn: &Connection, id: &str) -> Result<Note> {
    let rows = query_all(conn, "SELECT * FROM notes WHERE id = ?;", &[&id], |row| {
        map_note(row, false)
    })?;
    rows.into_iter()
        .next()
        .ok_or_else(|| anyhow::anyhow!("note not found"))
}

pub fn list_project(conn: &Connection, project_root: &str) -> Result<Vec<Note>> {
    query_all(
        conn,
        "SELECT * FROM notes WHERE project_root = ? ORDER BY updated_at DESC;",
        &[&project_root],
        |row| map_note(row, false),
    )
}

pub fn delete(conn: &Connection, id: &str) -> Result<bool> {
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
    note.text = new_text;
    note.tags = new_tags;
    note.summary = summary;
    note.updated_at = now;
    Ok(note)
}

pub fn search(conn: &Connection, query: &str, project_root: Option<&str>) -> Result<Vec<Note>> {
    let pattern = format!("%{}%", query);
    let sql = if project_root.is_some() {
        "SELECT * FROM notes WHERE project_root = ? AND (text LIKE ? OR summary LIKE ? OR file LIKE ?) ORDER BY updated_at DESC;"
    } else {
        "SELECT * FROM notes WHERE (text LIKE ? OR summary LIKE ? OR file LIKE ?) ORDER BY updated_at DESC;"
    };
    let pattern_b = pattern.clone();
    let rows = if let Some(proj) = project_root {
        query_all(conn, sql, &[&proj, &pattern, &pattern, &pattern_b], |row| {
            map_note(row, false)
        })?
    } else {
        query_all(conn, sql, &[&pattern, &pattern, &pattern_b], |row| {
            map_note(row, false)
        })?
    };
    Ok(rows)
}
