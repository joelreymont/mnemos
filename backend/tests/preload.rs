use backend::preload::preload;
use rusqlite::params;
use storage::connect;

#[test]
fn preload_scans_existing_rows() {
    let conn = connect(":memory:").unwrap();
    conn.execute(
        "INSERT INTO notes (id,file,project_root,line,column,node_path,tags,text,summary,commit_sha,blob_sha,created_at,updated_at)
         VALUES (?1,?2,?3,1,0,NULL,'[]','body','body',NULL,NULL,1,1);",
        params!["n1", "/tmp/a.rs", "/tmp"],
    )
    .unwrap();
    conn.execute(
        "INSERT INTO files (file, project_root, content, updated_at) VALUES (?1,?2,'fn a(){}',1);",
        params!["/tmp/a.rs", "/tmp"],
    )
    .unwrap();
    conn.execute(
        "INSERT INTO embeddings (file, project_root, vector, text, updated_at) VALUES (?1,?2,'[1.0]','fn a',1);",
        params!["/tmp/a.rs", "/tmp"],
    )
    .unwrap();
    let stats = preload(&conn).unwrap();
    assert_eq!(stats.notes, 1);
    assert_eq!(stats.files, 1);
    assert_eq!(stats.embeddings, 1);
}
