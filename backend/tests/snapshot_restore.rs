use backend::snapshot;
use serde_json::json;
use storage::connect;

#[test]
fn restore_returns_counts() {
    let conn = connect(":memory:").unwrap();
    let snap = json!({
        "version": 1,
        "files": [],
        "notes": [],
        "embeddings": [],
    });
    let out = snapshot::restore(&conn, &snap).unwrap();
    assert_eq!(
        out.get("counts").unwrap(),
        &json!({"files":0,"notes":0,"embeddings":0})
    );
    assert_eq!(out.get("ok").unwrap(), &json!(true));
}

#[test]
fn restore_inserts_notes_and_files() {
    let conn = connect(":memory:").unwrap();
    let snap = json!({
        "version": 1,
        "files": [
            { "file": "/tmp/a.rs", "projectRoot": "/tmp", "content": "fn a(){}", "updatedAt": 1 }
        ],
        "notes": [
            { "id": "n1", "file": "/tmp/a.rs", "projectRoot": "/tmp", "line": 1, "column": 0,
              "tags": "[]", "text": "t", "summary": "t", "createdAt": 1, "updatedAt": 1 }
        ],
        "embeddings": [
            { "file": "/tmp/a.rs", "projectRoot": "/tmp", "vector": "[1.0]", "text": "fn a", "updatedAt": 1 }
        ]
    });
    snapshot::restore(&conn, &snap).unwrap();
    let counts = snapshot::create(&conn, None)
        .unwrap()
        .get("counts")
        .cloned()
        .unwrap();
    assert_eq!(counts.get("files").unwrap(), 1);
    assert_eq!(counts.get("notes").unwrap(), 1);
    assert_eq!(counts.get("embeddings").unwrap(), 1);
}
