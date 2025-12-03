use backend::preload::sanity_check;
use rusqlite::params;
use storage::connect;

#[test]
fn sanity_check_passes_with_data() {
    // Verify sanity_check works when tables have rows
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
    sanity_check(&conn).unwrap();
}

#[test]
fn sanity_check_passes_with_empty_tables() {
    // Verify sanity_check works when tables are empty
    let conn = connect(":memory:").unwrap();
    sanity_check(&conn).unwrap();
}
