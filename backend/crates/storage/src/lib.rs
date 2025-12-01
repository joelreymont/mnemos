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
CREATE INDEX IF NOT EXISTS idx_edges_src ON edges(src);
CREATE INDEX IF NOT EXISTS idx_edges_dst ON edges(dst);
"#;

pub fn connect(path: &str) -> Result<Connection> {
    let conn = Connection::open(path)?;
    migrate(&conn)?;
    Ok(conn)
}

fn migrate(conn: &Connection) -> Result<()> {
    conn.execute_batch(SCHEMA)?;
    // Ensure columns exist (for forward/backward compatibility).
    for (table, col, ty) in [
        ("notes", "commit_sha", "TEXT"),
        ("notes", "blob_sha", "TEXT"),
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
