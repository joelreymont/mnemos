use anyhow::Result;
use rusqlite::Connection;
use serde_json::json;

pub fn restore(_conn: &Connection, snapshot: &serde_json::Value) -> Result<serde_json::Value> {
    // For now, just acknowledge and return counts; future: re-load embeddings/files if bundled.
    let counts = snapshot.get("counts").cloned().unwrap_or_else(|| json!({}));
    Ok(json!({
        "ok": true,
        "counts": counts
    }))
}
