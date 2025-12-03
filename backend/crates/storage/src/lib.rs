//! storage: SQLite connection and migrations.

use anyhow::Result;
use rusqlite::Connection;

const SCHEMA: &str = r#"
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
"#;

pub fn connect(path: &str) -> Result<Connection> {
    let conn = Connection::open(path)?;
    configure_pragmas(&conn)?;
    migrate(&conn)?;
    Ok(conn)
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
    ] {
        ensure_column(conn, table, col, ty)?;
    }
    Ok(())
}

fn ensure_column(conn: &Connection, table: &str, column: &str, ty: &str) -> Result<()> {
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
            &format!("ALTER TABLE {} ADD COLUMN {} {};", table, column, ty),
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
    let rows = stmt.query_map(params, |row| {
        map(row).map_err(|_e| rusqlite::Error::ExecuteReturnedResults)
    })?;
    let mut out = Vec::new();
    for r in rows {
        out.push(r?);
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
            let _ = conn.execute("ROLLBACK", []);
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
        let notes: i64 =
            conn.query_row("SELECT COUNT(*) FROM notes", [], |row| row.get(0))?;
        let files: i64 =
            conn.query_row("SELECT COUNT(*) FROM files", [], |row| row.get(0))?;
        let embeddings: i64 =
            conn.query_row("SELECT COUNT(*) FROM embeddings", [], |row| row.get(0))?;
        let edges: i64 =
            conn.query_row("SELECT COUNT(*) FROM edges", [], |row| row.get(0))?;
        (notes, files, embeddings, edges)
    };
    Ok(Counts {
        notes,
        files,
        embeddings,
        edges,
    })
}
