//! storage: SQLite connection and migrations.

use anyhow::{bail, Result};
use log::warn;
use rusqlite::Connection;

/// Current schema version. Increment when making schema changes.
/// - v1: Initial schema
/// - v2: Added content_hash, node_text_hash, binary embeddings
pub const SCHEMA_VERSION: i64 = 2;

const SCHEMA: &str = r#"
CREATE TABLE IF NOT EXISTS schema_meta (
  key TEXT PRIMARY KEY,
  value TEXT
);
CREATE TABLE IF NOT EXISTS notes (
  id TEXT PRIMARY KEY,
  file TEXT,
  project_root TEXT,
  line INTEGER,
  column INTEGER,
  node_path TEXT,
  tags TEXT,
  text TEXT,
  summary TEXT,
  commit_sha TEXT,
  blob_sha TEXT,
  created_at INTEGER,
  updated_at INTEGER
);

CREATE TABLE IF NOT EXISTS files (
  file TEXT PRIMARY KEY,
  project_root TEXT,
  content TEXT,
  updated_at INTEGER
);

CREATE TABLE IF NOT EXISTS embeddings (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  file TEXT,
  project_root TEXT,
  vector TEXT,
  text TEXT,
  updated_at INTEGER
);

CREATE TABLE IF NOT EXISTS edges (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  src TEXT,
  dst TEXT,
  kind TEXT,
  project_root TEXT,
  updated_at INTEGER
);

CREATE INDEX IF NOT EXISTS idx_notes_file ON notes(file);
CREATE INDEX IF NOT EXISTS idx_notes_project_root ON notes(project_root);
CREATE INDEX IF NOT EXISTS idx_notes_updated ON notes(project_root, updated_at DESC);
CREATE INDEX IF NOT EXISTS idx_files_project ON files(project_root);
CREATE INDEX IF NOT EXISTS idx_embeddings_file ON embeddings(file);
CREATE INDEX IF NOT EXISTS idx_embeddings_project ON embeddings(project_root);
CREATE INDEX IF NOT EXISTS idx_edges_src ON edges(src);
CREATE INDEX IF NOT EXISTS idx_edges_dst ON edges(dst);
CREATE INDEX IF NOT EXISTS idx_edges_project ON edges(project_root);

CREATE TABLE IF NOT EXISTS note_versions (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  note_id TEXT NOT NULL,
  version INTEGER NOT NULL,
  text TEXT,
  line INTEGER,
  column INTEGER,
  node_path TEXT,
  commit_sha TEXT,
  blob_sha TEXT,
  reason TEXT,
  created_at INTEGER,
  UNIQUE(note_id, version)
);

CREATE INDEX IF NOT EXISTS idx_note_versions_note ON note_versions(note_id);
CREATE INDEX IF NOT EXISTS idx_note_versions_created ON note_versions(created_at DESC);

CREATE TABLE IF NOT EXISTS project_meta (
  project_root TEXT PRIMARY KEY,
  indexed_at INTEGER,
  indexed_commit_sha TEXT,
  analyzed_at INTEGER,
  analysis_commit_sha TEXT,
  analysis_provider TEXT
);
"#;

pub fn connect(path: &str) -> Result<Connection> {
    let conn = Connection::open(path)?;
    configure_pragmas(&conn)?;
    check_and_migrate(&conn)?;
    Ok(conn)
}

/// Get the schema version from the database, or None if not set.
fn get_schema_version(conn: &Connection) -> Result<Option<i64>> {
    // First check if schema_meta table exists
    let table_exists: bool = conn
        .query_row(
            "SELECT 1 FROM sqlite_master WHERE type='table' AND name='schema_meta'",
            [],
            |_| Ok(true),
        )
        .unwrap_or(false);

    if !table_exists {
        return Ok(None);
    }

    let version: Option<i64> = conn
        .query_row(
            "SELECT CAST(value AS INTEGER) FROM schema_meta WHERE key = 'version'",
            [],
            |row| row.get(0),
        )
        .ok();

    Ok(version)
}

/// Set the schema version in the database.
fn set_schema_version(conn: &Connection, version: i64) -> Result<()> {
    conn.execute(
        "INSERT OR REPLACE INTO schema_meta (key, value) VALUES ('version', ?1)",
        [version.to_string()],
    )?;
    Ok(())
}

/// Check schema version and migrate if needed.
fn check_and_migrate(conn: &Connection) -> Result<()> {
    // Check version BEFORE running schema to catch incompatible databases early
    let db_version = get_schema_version(conn)?;

    if let Some(v) = db_version {
        if v > SCHEMA_VERSION {
            // Database is from a newer version of mnemos - fail before modifying anything
            bail!(
                "Database schema version {} is newer than this version of mnemos (schema {}). \
                Please upgrade mnemos or use a different database file.",
                v,
                SCHEMA_VERSION
            );
        }
    }

    // Now safe to run schema (creates tables if not exist)
    conn.execute_batch(SCHEMA)?;

    match db_version {
        None => {
            // Fresh database or pre-versioning database
            // Run all migrations and set version
            migrate(conn)?;
            set_schema_version(conn, SCHEMA_VERSION)?;
        }
        Some(v) if v < SCHEMA_VERSION => {
            // Database needs migration
            migrate(conn)?;
            set_schema_version(conn, SCHEMA_VERSION)?;
        }
        Some(_) => {
            // Version matches, still run migrations to ensure columns exist
            // (idempotent operations)
            migrate(conn)?;
        }
    }

    Ok(())
}

fn configure_pragmas(conn: &Connection) -> Result<()> {
    // WAL mode for better concurrency (readers don't block writers)
    conn.execute_batch(
        "
        PRAGMA journal_mode = WAL;
        PRAGMA synchronous = NORMAL;
        PRAGMA busy_timeout = 5000;
        PRAGMA cache_size = -64000;
        PRAGMA temp_store = MEMORY;
        ",
    )?;
    Ok(())
}

fn migrate(conn: &Connection) -> Result<()> {
    conn.execute_batch(SCHEMA)?;
    // Ensure columns exist (for forward/backward compatibility).
    for (table, col, ty) in [
        ("notes", "commit_sha", "TEXT"),
        ("notes", "blob_sha", "TEXT"),
        ("notes", "node_text_hash", "TEXT"),
        ("files", "content_hash", "TEXT"),
        ("embeddings", "binary_hash", "BLOB"),
        ("embeddings", "norm", "REAL"),
        ("embeddings", "vector_blob", "BLOB"),
    ] {
        ensure_column(conn, table, col, ty)?;
    }
    // Create FTS5 tables for full-text search
    create_fts_tables(conn)?;
    Ok(())
}

fn table_sql(conn: &Connection, name: &str) -> Result<Option<String>> {
    let mut stmt =
        conn.prepare("SELECT sql FROM sqlite_master WHERE type='table' AND name=?1 LIMIT 1;")?;
    let mut rows = stmt.query([name])?;
    if let Some(row) = rows.next()? {
        let sql: String = row.get(0)?;
        Ok(Some(sql))
    } else {
        Ok(None)
    }
}

fn drop_notes_fts(conn: &Connection) -> Result<()> {
    conn.execute_batch(
        r#"
        DROP TRIGGER IF EXISTS notes_fts_insert;
        DROP TRIGGER IF EXISTS notes_fts_delete;
        DROP TRIGGER IF EXISTS notes_fts_update;
        DROP TABLE IF EXISTS notes_fts;
        "#,
    )?;
    Ok(())
}

fn drop_files_fts(conn: &Connection) -> Result<()> {
    conn.execute_batch(
        r#"
        DROP TRIGGER IF EXISTS files_fts_insert;
        DROP TRIGGER IF EXISTS files_fts_delete;
        DROP TRIGGER IF EXISTS files_fts_update;
        DROP TABLE IF EXISTS files_fts;
        "#,
    )?;
    Ok(())
}

fn create_fts_tables(conn: &Connection) -> Result<()> {
    // Rebuild FTS tables if missing or if project_root was UNINDEXED (prevents indexed filtering).
    let notes_fts_sql = table_sql(conn, "notes_fts")?;
    let notes_needs_rebuild = notes_fts_sql
        .as_ref()
        .map(|sql| sql.contains("project_root UNINDEXED"))
        .unwrap_or(true);
    if notes_needs_rebuild {
        drop_notes_fts(conn)?;
        conn.execute_batch(
            r#"
            CREATE VIRTUAL TABLE notes_fts USING fts5(
                id UNINDEXED,
                project_root,
                text,
                summary,
                file,
                content='notes',
                content_rowid='rowid'
            );

            CREATE TRIGGER notes_fts_insert AFTER INSERT ON notes BEGIN
                INSERT INTO notes_fts(rowid, id, project_root, text, summary, file)
                VALUES (NEW.rowid, NEW.id, NEW.project_root, NEW.text, NEW.summary, NEW.file);
            END;

            CREATE TRIGGER notes_fts_delete AFTER DELETE ON notes BEGIN
                INSERT INTO notes_fts(notes_fts, rowid, id, project_root, text, summary, file)
                VALUES ('delete', OLD.rowid, OLD.id, OLD.project_root, OLD.text, OLD.summary, OLD.file);
            END;

            CREATE TRIGGER notes_fts_update AFTER UPDATE ON notes BEGIN
                INSERT INTO notes_fts(notes_fts, rowid, id, project_root, text, summary, file)
                VALUES ('delete', OLD.rowid, OLD.id, OLD.project_root, OLD.text, OLD.summary, OLD.file);
                INSERT INTO notes_fts(rowid, id, project_root, text, summary, file)
                VALUES (NEW.rowid, NEW.id, NEW.project_root, NEW.text, NEW.summary, NEW.file);
            END;

            INSERT INTO notes_fts(rowid, id, project_root, text, summary, file)
            SELECT rowid, id, project_root, text, summary, file FROM notes;
            "#,
        )?;
    }

    let files_fts_sql = table_sql(conn, "files_fts")?;
    let files_needs_rebuild = files_fts_sql
        .as_ref()
        .map(|sql| sql.contains("project_root UNINDEXED"))
        .unwrap_or(true);
    if files_needs_rebuild {
        drop_files_fts(conn)?;
        conn.execute_batch(
            r#"
            CREATE VIRTUAL TABLE files_fts USING fts5(
                file UNINDEXED,
                project_root,
                content,
                content='files',
                content_rowid='rowid'
            );

            CREATE TRIGGER files_fts_insert AFTER INSERT ON files BEGIN
                INSERT INTO files_fts(rowid, file, project_root, content)
                VALUES (NEW.rowid, NEW.file, NEW.project_root, NEW.content);
            END;

            CREATE TRIGGER files_fts_delete AFTER DELETE ON files BEGIN
                INSERT INTO files_fts(files_fts, rowid, file, project_root, content)
                VALUES ('delete', OLD.rowid, OLD.file, OLD.project_root, OLD.content);
            END;

            CREATE TRIGGER files_fts_update AFTER UPDATE ON files BEGIN
                INSERT INTO files_fts(files_fts, rowid, file, project_root, content)
                VALUES ('delete', OLD.rowid, OLD.file, OLD.project_root, OLD.content);
                INSERT INTO files_fts(rowid, file, project_root, content)
                VALUES (NEW.rowid, NEW.file, NEW.project_root, NEW.content);
            END;

            INSERT INTO files_fts(rowid, file, project_root, content)
            SELECT rowid, file, project_root, content FROM files;
            "#,
        )?;
    }

    Ok(())
}

/// Allowed table names for schema migrations (whitelist to prevent SQL injection)
const ALLOWED_TABLES: &[&str] = &["notes", "files", "embeddings", "edges", "project_meta"];

/// Allowed column types for schema migrations
const ALLOWED_TYPES: &[&str] = &["TEXT", "INTEGER", "REAL", "BLOB"];

fn ensure_column(conn: &Connection, table: &str, column: &str, ty: &str) -> Result<()> {
    // Validate table name against whitelist to prevent SQL injection
    if !ALLOWED_TABLES.contains(&table) {
        return Err(anyhow::anyhow!("invalid table name: {}", table));
    }
    // Validate column name: alphanumeric and underscores only
    if !column.chars().all(|c| c.is_ascii_alphanumeric() || c == '_') {
        return Err(anyhow::anyhow!("invalid column name: {}", column));
    }
    // Validate type against whitelist
    let ty_upper = ty.to_uppercase();
    if !ALLOWED_TYPES.contains(&ty_upper.as_str()) {
        return Err(anyhow::anyhow!("invalid column type: {}", ty));
    }

    let mut stmt = conn.prepare(&format!("PRAGMA table_info({});", table))?;
    let cols = stmt.query_map([], |row| row.get::<_, String>(1))?;
    let mut present = false;
    for col in cols {
        if col? == column {
            present = true;
            break;
        }
    }
    if !present {
        conn.execute(
            &format!("ALTER TABLE {} ADD COLUMN {} {};", table, column, ty_upper),
            [],
        )?;
    }
    Ok(())
}

pub fn now_unix() -> i64 {
    chrono::Utc::now().timestamp()
}

pub fn exec(conn: &Connection, sql: &str, params: &[&dyn rusqlite::ToSql]) -> Result<usize> {
    Ok(conn.execute(sql, params)?)
}

pub fn query_all<T, F>(
    conn: &Connection,
    sql: &str,
    params: &[&dyn rusqlite::ToSql],
    map: F,
) -> Result<Vec<T>>
where
    F: Fn(&rusqlite::Row<'_>) -> Result<T>,
{
    let mut stmt = conn.prepare(sql)?;
    let mut rows = stmt.query(params)?;
    let mut out = Vec::new();
    while let Some(row) = rows.next()? {
        out.push(map(row)?);
    }
    Ok(out)
}

/// Execute a function within a database transaction.
/// Commits on success, rolls back on error.
pub fn transaction<T, F>(conn: &Connection, f: F) -> Result<T>
where
    F: FnOnce() -> Result<T>,
{
    conn.execute("BEGIN IMMEDIATE", [])?;
    match f() {
        Ok(result) => {
            conn.execute("COMMIT", [])?;
            Ok(result)
        }
        Err(e) => {
            // Log rollback failure but still return the original error
            if let Err(rollback_err) = conn.execute("ROLLBACK", []) {
                warn!("ROLLBACK failed after error: {}", rollback_err);
            }
            Err(e)
        }
    }
}

/// Get counts of notes, files, embeddings, and edges efficiently using COUNT(*).
/// If project_root is provided, counts are filtered to that project.
#[derive(Debug, Clone)]
pub struct Counts {
    pub notes: i64,
    pub files: i64,
    pub embeddings: i64,
    pub edges: i64,
}

pub fn counts(conn: &Connection, project_root: Option<&str>) -> Result<Counts> {
    let (notes, files, embeddings, edges) = if let Some(proj) = project_root {
        let notes: i64 = conn.query_row(
            "SELECT COUNT(*) FROM notes WHERE project_root = ?",
            [proj],
            |row| row.get(0),
        )?;
        let files: i64 = conn.query_row(
            "SELECT COUNT(*) FROM files WHERE project_root = ?",
            [proj],
            |row| row.get(0),
        )?;
        let embeddings: i64 = conn.query_row(
            "SELECT COUNT(*) FROM embeddings WHERE project_root = ?",
            [proj],
            |row| row.get(0),
        )?;
        let edges: i64 = conn.query_row(
            "SELECT COUNT(*) FROM edges WHERE project_root = ?",
            [proj],
            |row| row.get(0),
        )?;
        (notes, files, embeddings, edges)
    } else {
        let notes: i64 = conn.query_row("SELECT COUNT(*) FROM notes", [], |row| row.get(0))?;
        let files: i64 = conn.query_row("SELECT COUNT(*) FROM files", [], |row| row.get(0))?;
        let embeddings: i64 =
            conn.query_row("SELECT COUNT(*) FROM embeddings", [], |row| row.get(0))?;
        let edges: i64 = conn.query_row("SELECT COUNT(*) FROM edges", [], |row| row.get(0))?;
        (notes, files, embeddings, edges)
    };
    Ok(Counts {
        notes,
        files,
        embeddings,
        edges,
    })
}

/// Metadata about a project's indexing and analysis state.
#[derive(Debug, Clone, Default)]
pub struct ProjectMeta {
    pub project_root: String,
    pub indexed_at: Option<i64>,
    pub indexed_commit_sha: Option<String>,
    pub analyzed_at: Option<i64>,
    pub analysis_commit_sha: Option<String>,
    pub analysis_provider: Option<String>,
}

/// Get project metadata. Returns None if project not found.
pub fn get_project_meta(conn: &Connection, project_root: &str) -> Result<Option<ProjectMeta>> {
    let mut stmt = conn.prepare(
        "SELECT project_root, indexed_at, indexed_commit_sha, analyzed_at, analysis_commit_sha, analysis_provider
         FROM project_meta WHERE project_root = ?",
    )?;
    let mut rows = stmt.query([project_root])?;
    if let Some(row) = rows.next()? {
        Ok(Some(ProjectMeta {
            project_root: row.get(0)?,
            indexed_at: row.get(1)?,
            indexed_commit_sha: row.get(2)?,
            analyzed_at: row.get(3)?,
            analysis_commit_sha: row.get(4)?,
            analysis_provider: row.get(5)?,
        }))
    } else {
        Ok(None)
    }
}

/// Update project indexing metadata.
pub fn set_project_indexed(
    conn: &Connection,
    project_root: &str,
    commit_sha: Option<&str>,
) -> Result<()> {
    let now = now_unix();
    conn.execute(
        "INSERT INTO project_meta (project_root, indexed_at, indexed_commit_sha)
         VALUES (?1, ?2, ?3)
         ON CONFLICT(project_root) DO UPDATE SET
           indexed_at = ?2,
           indexed_commit_sha = ?3",
        rusqlite::params![project_root, now, commit_sha],
    )?;
    Ok(())
}

/// Update project analysis metadata.
pub fn set_project_analyzed(
    conn: &Connection,
    project_root: &str,
    commit_sha: Option<&str>,
    provider: &str,
) -> Result<()> {
    let now = now_unix();
    conn.execute(
        "INSERT INTO project_meta (project_root, analyzed_at, analysis_commit_sha, analysis_provider)
         VALUES (?1, ?2, ?3, ?4)
         ON CONFLICT(project_root) DO UPDATE SET
           analyzed_at = ?2,
           analysis_commit_sha = ?3,
           analysis_provider = ?4",
        rusqlite::params![project_root, now, commit_sha, provider],
    )?;
    Ok(())
}

/// Check if project needs re-analysis (commit SHA changed since last analysis).
pub fn project_analysis_stale(conn: &Connection, project_root: &str, current_commit: &str) -> Result<bool> {
    if let Some(meta) = get_project_meta(conn, project_root)? {
        if let Some(analysis_sha) = meta.analysis_commit_sha {
            return Ok(analysis_sha != current_commit);
        }
    }
    // No analysis or no commit recorded means stale
    Ok(true)
}

/// A version of a note's state at a point in time
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct NoteVersion {
    pub note_id: String,
    pub version: i64,
    pub text: Option<String>,
    pub line: i64,
    pub column: i64,
    pub node_path: Option<String>,
    pub commit_sha: Option<String>,
    pub blob_sha: Option<String>,
    pub reason: Option<String>,
    pub created_at: i64,
}

/// Parameters for saving a note version
pub struct SaveNoteVersionParams<'a> {
    pub note_id: &'a str,
    pub text: Option<&'a str>,
    pub line: i64,
    pub column: i64,
    pub node_path: Option<&'a str>,
    pub commit_sha: Option<&'a str>,
    pub blob_sha: Option<&'a str>,
    pub reason: Option<&'a str>,
}

/// Save a version of a note before update
pub fn save_note_version(conn: &Connection, params: SaveNoteVersionParams<'_>) -> Result<i64> {
    let SaveNoteVersionParams {
        note_id,
        text,
        line,
        column,
        node_path,
        commit_sha,
        blob_sha,
        reason,
    } = params;
    let now = now_unix();

    // Get next version number
    let next_version: i64 = conn
        .query_row(
            "SELECT COALESCE(MAX(version), 0) + 1 FROM note_versions WHERE note_id = ?1",
            [note_id],
            |row| row.get(0),
        )
        .unwrap_or(1);

    conn.execute(
        "INSERT INTO note_versions (note_id, version, text, line, column, node_path, commit_sha, blob_sha, reason, created_at)
         VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10)",
        rusqlite::params![note_id, next_version, text, line, column, node_path, commit_sha, blob_sha, reason, now],
    )?;

    Ok(next_version)
}

/// List all versions of a note (most recent first)
pub fn list_note_versions(conn: &Connection, note_id: &str, limit: usize) -> Result<Vec<NoteVersion>> {
    let mut stmt = conn.prepare(
        "SELECT note_id, version, text, line, column, node_path, commit_sha, blob_sha, reason, created_at
         FROM note_versions
         WHERE note_id = ?1
         ORDER BY version DESC
         LIMIT ?2",
    )?;

    let rows = stmt.query_map(rusqlite::params![note_id, limit], |row| {
        Ok(NoteVersion {
            note_id: row.get(0)?,
            version: row.get(1)?,
            text: row.get(2)?,
            line: row.get(3)?,
            column: row.get(4)?,
            node_path: row.get(5)?,
            commit_sha: row.get(6)?,
            blob_sha: row.get(7)?,
            reason: row.get(8)?,
            created_at: row.get(9)?,
        })
    })?;

    let mut versions = Vec::new();
    for row in rows {
        versions.push(row?);
    }
    Ok(versions)
}

/// Get a specific version of a note
pub fn get_note_version(conn: &Connection, note_id: &str, version: i64) -> Result<Option<NoteVersion>> {
    let mut stmt = conn.prepare(
        "SELECT note_id, version, text, line, column, node_path, commit_sha, blob_sha, reason, created_at
         FROM note_versions
         WHERE note_id = ?1 AND version = ?2",
    )?;

    let mut rows = stmt.query(rusqlite::params![note_id, version])?;
    if let Some(row) = rows.next()? {
        Ok(Some(NoteVersion {
            note_id: row.get(0)?,
            version: row.get(1)?,
            text: row.get(2)?,
            line: row.get(3)?,
            column: row.get(4)?,
            node_path: row.get(5)?,
            commit_sha: row.get(6)?,
            blob_sha: row.get(7)?,
            reason: row.get(8)?,
            created_at: row.get(9)?,
        }))
    } else {
        Ok(None)
    }
}

/// Count versions for a note
pub fn count_note_versions(conn: &Connection, note_id: &str) -> Result<i64> {
    conn.query_row(
        "SELECT COUNT(*) FROM note_versions WHERE note_id = ?1",
        [note_id],
        |row| row.get(0),
    )
    .map_err(|e| e.into())
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::NamedTempFile;

    #[test]
    fn test_fresh_database_sets_version() {
        let tmp = NamedTempFile::new().unwrap();
        let conn = connect(tmp.path().to_str().unwrap()).unwrap();

        let version = get_schema_version(&conn).unwrap();
        assert_eq!(version, Some(SCHEMA_VERSION));
    }

    #[test]
    fn test_existing_database_migrates() {
        let tmp = NamedTempFile::new().unwrap();
        let path = tmp.path().to_str().unwrap();

        // Create a database with old schema (no schema_meta, no content_hash)
        {
            let conn = Connection::open(path).unwrap();
            conn.execute_batch(
                r#"
                CREATE TABLE files (
                    file TEXT PRIMARY KEY,
                    project_root TEXT,
                    content TEXT,
                    updated_at INTEGER
                );
                INSERT INTO files (file, project_root, content, updated_at)
                VALUES ('/test/file.rs', '/test', 'fn main() {}', 123);
                "#,
            )
            .unwrap();
        }

        // Now connect with new code - should migrate
        let conn = connect(path).unwrap();

        // Version should be set
        let version = get_schema_version(&conn).unwrap();
        assert_eq!(version, Some(SCHEMA_VERSION));

        // content_hash column should exist (migration ran)
        let has_content_hash: bool = conn
            .query_row(
                "SELECT 1 FROM pragma_table_info('files') WHERE name = 'content_hash'",
                [],
                |_| Ok(true),
            )
            .unwrap_or(false);
        assert!(has_content_hash, "content_hash column should exist after migration");
    }

    #[test]
    fn test_newer_database_fails() {
        let tmp = NamedTempFile::new().unwrap();
        let path = tmp.path().to_str().unwrap();

        // Create a database with a future schema version
        {
            let conn = Connection::open(path).unwrap();
            conn.execute_batch(
                r#"
                CREATE TABLE schema_meta (key TEXT PRIMARY KEY, value TEXT);
                INSERT INTO schema_meta (key, value) VALUES ('version', '999');
                "#,
            )
            .unwrap();
        }

        // Connecting should fail with clear error
        let result = connect(path);
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(
            err.contains("999") && err.contains("newer"),
            "Error should mention version mismatch: {}",
            err
        );
    }

    #[test]
    fn test_schema_version_constant_exported() {
        // Ensure SCHEMA_VERSION is accessible for version reporting
        assert!(SCHEMA_VERSION > 0);
    }
}
