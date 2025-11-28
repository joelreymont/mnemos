use backend::snapshot;
use serde_json::json;
use storage::connect;

#[test]
fn restore_returns_counts() {
    let conn = connect(":memory:").unwrap();
    let snap = json!({
        "version": 1,
        "counts": { "files": 2, "notes": 1, "embeddings": 1 }
    });
    let out = snapshot::restore(&conn, &snap).unwrap();
    assert_eq!(out.get("counts").unwrap(), snap.get("counts").unwrap());
    assert_eq!(out.get("ok").unwrap(), &json!(true));
}
