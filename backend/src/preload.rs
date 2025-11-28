use anyhow::Result;
use rusqlite::Connection;

#[derive(Debug, Default)]
pub struct PreloadStats {
    pub notes: usize,
    pub files: usize,
    pub embeddings: usize,
}

fn preload_table(conn: &Connection, sql: &str) -> Result<usize> {
    let mut stmt = conn.prepare(sql)?;
    let mut rows = stmt.query([])?;
    let mut count = 0;
    while rows.next()?.is_some() {
        count += 1;
    }
    Ok(count)
}

pub fn preload(conn: &Connection) -> Result<PreloadStats> {
    // Touch core tables to warm SQLite page cache and catch schema issues early.
    let notes = preload_table(conn, "SELECT id, text FROM notes;")?;
    let files = preload_table(conn, "SELECT file, content FROM files;")?;
    let embeddings = preload_table(conn, "SELECT id, vector FROM embeddings;")?;
    Ok(PreloadStats {
        notes,
        files,
        embeddings,
    })
}
