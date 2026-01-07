use index::upsert_embedding_for_file;
use storage::connect;

#[test]
fn embedder_falls_back_on_error() {
    let db = tempfile::NamedTempFile::new().unwrap();
    std::env::set_var("MNEMOS_EMBED_URL", "http://127.0.0.1:9"); // unroutable -> fails fast
    let conn = connect(db.path().to_str().unwrap()).unwrap();
    let content = "fn main() {}\n";
    // Should not panic; should store derived vector.
    upsert_embedding_for_file(&conn, "/tmp/fallback.rs", "/tmp", content).unwrap();
    let mut stmt = conn
        .prepare("SELECT vector FROM embeddings WHERE file = ?1")
        .unwrap();
    let vec_str: String = stmt
        .query_row(["/tmp/fallback.rs"], |row| row.get(0))
        .unwrap();
    let vec: Vec<f32> = serde_json::from_str(&vec_str).unwrap();
    assert_eq!(vec.len(), 2, "fallback vector should have two dimensions");
    std::env::remove_var("MNEMOS_EMBED_URL");
}
