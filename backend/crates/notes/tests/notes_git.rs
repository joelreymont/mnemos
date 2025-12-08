use git::GitInfo;
use notes::{create, get, list_by_node, list_for_file, CreateNoteParams, NoteFilters};
use storage::connect;
use tempfile::NamedTempFile;

#[test]
fn create_and_filter_stale() {
    let db = NamedTempFile::new().unwrap();
    let conn = connect(db.path().to_str().unwrap()).unwrap();
    let git = GitInfo {
        root: "/tmp".into(),
        commit: "abc".into(),
        blob: Some("blob1".into()),
    };
    let note = create(
        &conn,
        CreateNoteParams {
            file: "/tmp/test.rs",
            project_root: "/tmp",
            line: 1,
            column: 0,
            node_path: Some(serde_json::json!(["fn"])),
            tags: serde_json::json!([]),
            text: "hello",
            git: Some(git),
            node_text_hash: None,
        },
    )
    .unwrap();
    let filters = NoteFilters {
        file: &note.file,
        project_root: &note.project_root,
        node_path: None,
        commit: Some("abc"),
        blob: Some("blob1"),
        include_stale: false,
    };
    let notes = list_for_file(&conn, filters).unwrap();
    assert_eq!(notes.len(), 1);
    let filters_stale = NoteFilters {
        file: &note.file,
        project_root: &note.project_root,
        node_path: None,
        commit: Some("abc"),
        blob: Some("other"),
        include_stale: true, // Request stale notes to verify staleness flag
    };
    let stale = list_for_file(&conn, filters_stale.clone()).unwrap();
    assert_eq!(stale.len(), 1);
    assert!(stale[0].stale);
    let fetched = get(&conn, &note.id).unwrap();
    assert_eq!(fetched.id, note.id);
    // list by node with a path
    let filters_node = NoteFilters {
        node_path: Some(serde_json::json!(["fn"])),
        ..filters_stale
    };
    let node_notes = list_by_node(&conn, filters_node).unwrap();
    assert_eq!(node_notes.len(), 1);
    assert!(node_notes[0].stale);
}
