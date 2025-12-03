use anyhow::Result;
use rusqlite::Connection;

/// Verify database schema is accessible. Runs cheap queries to catch
/// schema issues early without loading data into memory.
pub fn sanity_check(conn: &Connection) -> Result<()> {
    // Quick existence check on each table - no data loaded
    conn.query_row("SELECT 1 FROM notes LIMIT 1", [], |_| Ok(()))
        .or_else(|e| match e {
            rusqlite::Error::QueryReturnedNoRows => Ok(()),
            _ => Err(e),
        })?;
    conn.query_row("SELECT 1 FROM files LIMIT 1", [], |_| Ok(()))
        .or_else(|e| match e {
            rusqlite::Error::QueryReturnedNoRows => Ok(()),
            _ => Err(e),
        })?;
    conn.query_row("SELECT 1 FROM embeddings LIMIT 1", [], |_| Ok(()))
        .or_else(|e| match e {
            rusqlite::Error::QueryReturnedNoRows => Ok(()),
            _ => Err(e),
        })?;
    Ok(())
}
