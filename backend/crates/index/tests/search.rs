use index::{add_file, search};
use storage::connect;
use tempfile::NamedTempFile;

#[test]
fn search_finds_hits() {
    let db = NamedTempFile::new().unwrap();
    let conn = connect(db.path().to_str().unwrap()).unwrap();
    let content = "fn add() {}\n// hello\nfn foo() { add(); }";
    add_file(&conn, "/tmp/test.rs", "/tmp", content).unwrap();
    let hits = search(&conn, "fn", Some("/tmp")).unwrap();
    assert!(hits.len() >= 2);
}
