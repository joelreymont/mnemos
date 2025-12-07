//! Comprehensive snapshot round-trip tests

use backend::snapshot;
use serde_json::json;
use storage::connect;

/// Helper to create a note via RPC simulation
fn insert_test_note(
    conn: &rusqlite::Connection,
    id: &str,
    file: &str,
    project_root: &str,
    line: i64,
    column: i64,
    text: &str,
) {
    storage::exec(
        conn,
        "INSERT INTO notes (id, file, project_root, line, column, node_path, tags, text, summary, created_at, updated_at)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
        &[
            &id as &dyn rusqlite::ToSql,
            &file,
            &project_root,
            &line,
            &column,
            &"[]",
            &"[]",
            &text,
            &text.lines().next().unwrap_or(""),
            &1234567890_i64,
            &1234567890_i64,
        ],
    )
    .unwrap();
}

/// Helper to create a file via RPC simulation
fn insert_test_file(conn: &rusqlite::Connection, file: &str, project_root: &str, content: &str) {
    storage::exec(
        conn,
        "INSERT INTO files (file, project_root, content, updated_at) VALUES (?, ?, ?, ?)",
        &[
            &file as &dyn rusqlite::ToSql,
            &project_root,
            &content,
            &1234567890_i64,
        ],
    )
    .unwrap();
}

/// Helper to create an edge via RPC simulation
fn insert_test_edge(
    conn: &rusqlite::Connection,
    src: &str,
    dst: &str,
    kind: &str,
    project_root: &str,
) {
    storage::exec(
        conn,
        "INSERT INTO edges (src, dst, kind, project_root, updated_at) VALUES (?, ?, ?, ?, ?)",
        &[
            &src as &dyn rusqlite::ToSql,
            &dst,
            &kind,
            &project_root,
            &1234567890_i64,
        ],
    )
    .unwrap();
}

/// Helper to verify note exists and has correct data
fn verify_note(conn: &rusqlite::Connection, id: &str, expected_file: &str, expected_text: &str) {
    let (file, text): (String, String) = conn
        .query_row("SELECT file, text FROM notes WHERE id = ?", [id], |row| {
            Ok((row.get(0)?, row.get(1)?))
        })
        .expect("note should exist");
    assert_eq!(file, expected_file, "note file should match");
    assert_eq!(text, expected_text, "note text should match");
}

/// Helper to verify file exists and has correct content
fn verify_file(conn: &rusqlite::Connection, file: &str, expected_content: &str) {
    let content: String = conn
        .query_row("SELECT content FROM files WHERE file = ?", [file], |row| {
            row.get(0)
        })
        .expect("file should exist");
    assert_eq!(content, expected_content, "file content should match");
}

/// Helper to verify edge exists
fn verify_edge(conn: &rusqlite::Connection, src: &str, dst: &str, expected_kind: &str) {
    let kind: String = conn
        .query_row(
            "SELECT kind FROM edges WHERE src = ? AND dst = ?",
            [src, dst],
            |row| row.get(0),
        )
        .expect("edge should exist");
    assert_eq!(kind, expected_kind, "edge kind should match");
}

#[test]
fn full_roundtrip_via_rpc() {
    // Create first database with notes, files, and edges
    let conn1 = connect(":memory:").unwrap();
    let project_root = "/test/project";

    // Create test data
    insert_test_note(
        &conn1,
        "note-1",
        "/test/project/src/main.rs",
        project_root,
        10,
        5,
        "This is a test note",
    );
    insert_test_note(
        &conn1,
        "note-2",
        "/test/project/src/lib.rs",
        project_root,
        20,
        0,
        "Another note",
    );
    insert_test_file(
        &conn1,
        "/test/project/src/main.rs",
        project_root,
        "fn main() {}\n",
    );
    insert_test_file(
        &conn1,
        "/test/project/src/lib.rs",
        project_root,
        "pub fn hello() {}\n",
    );
    insert_test_edge(&conn1, "note-1", "note-2", "link", project_root);
    insert_test_edge(&conn1, "note-2", "note-1", "link", project_root);

    // Save snapshot
    let snapshot = snapshot::create(&conn1, None).unwrap();

    // Verify snapshot structure
    assert_eq!(snapshot.get("version").unwrap(), 1);
    assert_eq!(snapshot.get("counts").unwrap().get("notes").unwrap(), 2);
    assert_eq!(snapshot.get("counts").unwrap().get("files").unwrap(), 2);
    assert_eq!(snapshot.get("counts").unwrap().get("edges").unwrap(), 2);

    // Create second database and load snapshot
    let conn2 = connect(":memory:").unwrap();
    let restore_result = snapshot::restore(&conn2, &snapshot).unwrap();

    // Verify restore result
    assert_eq!(restore_result.get("ok").unwrap(), &json!(true));
    assert_eq!(
        restore_result.get("counts").unwrap().get("notes").unwrap(),
        2
    );
    assert_eq!(
        restore_result.get("counts").unwrap().get("files").unwrap(),
        2
    );
    assert_eq!(
        restore_result.get("counts").unwrap().get("edges").unwrap(),
        2
    );

    // Verify notes exist with correct data
    verify_note(
        &conn2,
        "note-1",
        "/test/project/src/main.rs",
        "This is a test note",
    );
    verify_note(&conn2, "note-2", "/test/project/src/lib.rs", "Another note");

    // Verify files exist with correct content
    verify_file(&conn2, "/test/project/src/main.rs", "fn main() {}\n");
    verify_file(&conn2, "/test/project/src/lib.rs", "pub fn hello() {}\n");

    // Verify edges exist
    verify_edge(&conn2, "note-1", "note-2", "link");
    verify_edge(&conn2, "note-2", "note-1", "link");

    // Verify note positions are correct
    let (line, column): (i64, i64) = conn2
        .query_row(
            "SELECT line, column FROM notes WHERE id = 'note-1'",
            [],
            |row| Ok((row.get(0)?, row.get(1)?)),
        )
        .unwrap();
    assert_eq!(line, 10);
    assert_eq!(column, 5);
}

#[test]
fn roundtrip_with_complex_note_text() {
    let conn1 = connect(":memory:").unwrap();
    let project_root = "/test/project";

    // Create note with complex text including newlines, unicode, special chars
    let complex_text = "Line 1: Test note\nLine 2: With unicode 🦀\nLine 3: Special chars: <>&\"'";
    insert_test_note(
        &conn1,
        "note-complex",
        "/test/file.rs",
        project_root,
        5,
        0,
        complex_text,
    );

    // Save and restore
    let snapshot = snapshot::create(&conn1, None).unwrap();
    let conn2 = connect(":memory:").unwrap();
    snapshot::restore(&conn2, &snapshot).unwrap();

    // Verify complex text preserved exactly
    verify_note(&conn2, "note-complex", "/test/file.rs", complex_text);
}

#[test]
fn roundtrip_preserves_relationships() {
    let conn1 = connect(":memory:").unwrap();
    let project_root = "/test/project";

    // Create notes with backlink relationships
    insert_test_note(&conn1, "note-a", "/test/a.rs", project_root, 1, 0, "Note A");
    insert_test_note(
        &conn1,
        "note-b",
        "/test/b.rs",
        project_root,
        2,
        0,
        "Note B links to [[note-a]]",
    );
    insert_test_note(
        &conn1,
        "note-c",
        "/test/c.rs",
        project_root,
        3,
        0,
        "Note C links to [[note-a]]",
    );
    insert_test_edge(&conn1, "note-b", "note-a", "link", project_root);
    insert_test_edge(&conn1, "note-c", "note-a", "link", project_root);

    // Save and restore
    let snapshot = snapshot::create(&conn1, None).unwrap();
    let conn2 = connect(":memory:").unwrap();
    snapshot::restore(&conn2, &snapshot).unwrap();

    // Verify all edges exist
    verify_edge(&conn2, "note-b", "note-a", "link");
    verify_edge(&conn2, "note-c", "note-a", "link");

    // Verify backlinks work
    let backlink_count: i64 = conn2
        .query_row(
            "SELECT COUNT(*) FROM edges WHERE dst = 'note-a'",
            [],
            |row| row.get(0),
        )
        .unwrap();
    assert_eq!(backlink_count, 2, "note-a should have 2 backlinks");
}

#[test]
fn bad_version_handling() {
    let conn = connect(":memory:").unwrap();

    // Create snapshot with unsupported version
    let snapshot = json!({
        "version": 999,
        "projectRoot": null,
        "createdAt": 1234567890,
        "counts": {
            "notes": 0,
            "files": 0,
            "embeddings": 0,
            "edges": 0
        },
        "notes": [],
        "files": [],
        "embeddings": [],
        "edges": []
    });

    // Attempt to load - should succeed (no version check in current implementation)
    // but verify database wasn't corrupted
    let result = snapshot::restore(&conn, &snapshot);
    assert!(
        result.is_ok(),
        "restore should handle different versions gracefully"
    );

    // Verify database is still accessible
    let count: i64 = conn
        .query_row("SELECT COUNT(*) FROM notes", [], |row| row.get(0))
        .unwrap();
    assert_eq!(count, 0, "database should not be corrupted");
}

#[test]
fn malformed_json_truncated() {
    let conn = connect(":memory:").unwrap();

    // Create snapshot with missing edges field (simulates truncated JSON)
    // The restore should succeed gracefully since edges is optional if not present
    let truncated_json = json!({
        "version": 1,
        "notes": [],
        "files": [],
        "embeddings": []
        // Missing "edges" field - should be treated as empty array
    });

    // Should succeed gracefully when optional field is missing
    let result = snapshot::restore(&conn, &truncated_json);
    assert!(
        result.is_ok(),
        "should handle missing optional edges field gracefully"
    );

    // Verify database is still accessible
    let count: i64 = conn
        .query_row("SELECT COUNT(*) FROM notes", [], |row| row.get(0))
        .unwrap();
    assert_eq!(count, 0, "database should not be corrupted");
}

#[test]
fn malformed_json_invalid_structure() {
    let conn = connect(":memory:").unwrap();

    // Snapshot with wrong types
    let invalid_snapshot = json!({
        "version": 1,
        "notes": "not an array", // Should be an array
        "files": [],
        "embeddings": [],
        "edges": []
    });

    // Should handle gracefully (notes field won't be processed since it's not an array)
    let result = snapshot::restore(&conn, &invalid_snapshot);
    assert!(result.is_ok(), "should handle type mismatches gracefully");

    // Verify no notes were inserted
    let count: i64 = conn
        .query_row("SELECT COUNT(*) FROM notes", [], |row| row.get(0))
        .unwrap();
    assert_eq!(count, 0);
}

#[test]
fn partial_snapshot_missing_required_note_fields() {
    let conn = connect(":memory:").unwrap();

    // Snapshot with note missing required field 'id'
    let snapshot = json!({
        "version": 1,
        "notes": [
            {
                // Missing "id" field
                "file": "/test.rs",
                "projectRoot": "/test",
                "line": 1,
                "column": 0,
                "tags": "[]",
                "text": "test",
                "summary": "test",
                "createdAt": 1234567890,
                "updatedAt": 1234567890
            }
        ],
        "files": [],
        "embeddings": [],
        "edges": []
    });

    // Should fail with clear error message
    let result = snapshot::restore(&conn, &snapshot);
    assert!(
        result.is_err(),
        "should fail on missing required field 'id'"
    );
    assert!(
        result
            .unwrap_err()
            .to_string()
            .contains("missing required field: id"),
        "error should mention missing 'id' field"
    );

    // Verify transaction was rolled back
    let count: i64 = conn
        .query_row("SELECT COUNT(*) FROM notes", [], |row| row.get(0))
        .unwrap();
    assert_eq!(
        count, 0,
        "no partial data should be inserted after rollback"
    );
}

#[test]
fn partial_snapshot_missing_required_file_fields() {
    let conn = connect(":memory:").unwrap();

    // Snapshot with file missing required field 'projectRoot'
    let snapshot = json!({
        "version": 1,
        "notes": [],
        "files": [
            {
                "file": "/test.rs",
                // Missing "projectRoot" field
                "content": "test content",
                "updatedAt": 1234567890
            }
        ],
        "embeddings": [],
        "edges": []
    });

    // Should fail with clear error message
    let result = snapshot::restore(&conn, &snapshot);
    assert!(
        result.is_err(),
        "should fail on missing required field 'projectRoot'"
    );
    assert!(
        result
            .unwrap_err()
            .to_string()
            .contains("missing required field: projectRoot"),
        "error should mention missing 'projectRoot' field"
    );

    // Verify transaction was rolled back
    let count: i64 = conn
        .query_row("SELECT COUNT(*) FROM files", [], |row| row.get(0))
        .unwrap();
    assert_eq!(
        count, 0,
        "no partial data should be inserted after rollback"
    );
}

#[test]
fn partial_snapshot_missing_required_edge_fields() {
    let conn = connect(":memory:").unwrap();

    // Snapshot with edge missing required field 'dst'
    let snapshot = json!({
        "version": 1,
        "notes": [],
        "files": [],
        "embeddings": [],
        "edges": [
            {
                "src": "note-1",
                // Missing "dst" field
                "kind": "link",
                "projectRoot": "/test",
                "updatedAt": 1234567890
            }
        ]
    });

    // Should fail with clear error message
    let result = snapshot::restore(&conn, &snapshot);
    assert!(
        result.is_err(),
        "should fail on missing required field 'dst'"
    );
    assert!(
        result
            .unwrap_err()
            .to_string()
            .contains("missing required field: dst"),
        "error should mention missing 'dst' field"
    );

    // Verify transaction was rolled back
    let count: i64 = conn
        .query_row("SELECT COUNT(*) FROM edges", [], |row| row.get(0))
        .unwrap();
    assert_eq!(
        count, 0,
        "no partial data should be inserted after rollback"
    );
}

#[test]
fn snapshot_with_invalid_note_id() {
    let conn = connect(":memory:").unwrap();

    // Snapshot with invalid note ID (contains invalid characters)
    let snapshot = json!({
        "version": 1,
        "notes": [
            {
                "id": "note with spaces!", // Invalid ID format
                "file": "/test.rs",
                "projectRoot": "/test",
                "line": 1,
                "column": 0,
                "tags": "[]",
                "text": "test",
                "summary": "test",
                "createdAt": 1234567890,
                "updatedAt": 1234567890
            }
        ],
        "files": [],
        "embeddings": [],
        "edges": []
    });

    // Should fail due to invalid ID format
    let result = snapshot::restore(&conn, &snapshot);
    assert!(result.is_err(), "should fail on invalid note ID format");
    assert!(
        result
            .unwrap_err()
            .to_string()
            .contains("invalid id format"),
        "error should mention invalid ID format"
    );
}

#[test]
fn snapshot_with_invalid_line_numbers() {
    let conn = connect(":memory:").unwrap();

    // Snapshot with invalid line number (negative)
    let snapshot = json!({
        "version": 1,
        "notes": [
            {
                "id": "note-1",
                "file": "/test.rs",
                "projectRoot": "/test",
                "line": -5, // Invalid negative line
                "column": 0,
                "tags": "[]",
                "text": "test",
                "summary": "test",
                "createdAt": 1234567890,
                "updatedAt": 1234567890
            }
        ],
        "files": [],
        "embeddings": [],
        "edges": []
    });

    // Should fail due to invalid line number
    let result = snapshot::restore(&conn, &snapshot);
    assert!(result.is_err(), "should fail on invalid line number");
    assert!(
        result
            .unwrap_err()
            .to_string()
            .contains("invalid line number"),
        "error should mention invalid line number"
    );
}

#[test]
fn snapshot_restores_to_empty_database() {
    let conn1 = connect(":memory:").unwrap();
    let conn2 = connect(":memory:").unwrap();

    // Add data to conn1
    insert_test_note(&conn1, "note-1", "/test.rs", "/test", 10, 0, "Test note");

    // Add different data to conn2 first
    insert_test_note(&conn2, "note-old", "/old.rs", "/test", 5, 0, "Old note");

    // Verify old data exists
    let count_before: i64 = conn2
        .query_row("SELECT COUNT(*) FROM notes", [], |row| row.get(0))
        .unwrap();
    assert_eq!(count_before, 1);

    // Create snapshot from conn1 and restore to conn2
    let snapshot = snapshot::create(&conn1, None).unwrap();
    snapshot::restore(&conn2, &snapshot).unwrap();

    // Verify old data was replaced
    let count_after: i64 = conn2
        .query_row("SELECT COUNT(*) FROM notes", [], |row| row.get(0))
        .unwrap();
    assert_eq!(count_after, 1, "should have exactly 1 note after restore");

    // Verify it's the new note, not the old one
    verify_note(&conn2, "note-1", "/test.rs", "Test note");

    // Verify old note is gone
    let old_exists: bool = conn2
        .query_row(
            "SELECT COUNT(*) FROM notes WHERE id = 'note-old'",
            [],
            |row| Ok(row.get::<_, i64>(0)? > 0),
        )
        .unwrap();
    assert!(!old_exists, "old note should be deleted");
}

#[test]
fn empty_snapshot_clears_database() {
    let conn = connect(":memory:").unwrap();

    // Add some data
    insert_test_note(&conn, "note-1", "/test.rs", "/test", 10, 0, "Test note");
    insert_test_file(&conn, "/test.rs", "/test", "content");

    // Verify data exists
    let note_count: i64 = conn
        .query_row("SELECT COUNT(*) FROM notes", [], |row| row.get(0))
        .unwrap();
    assert_eq!(note_count, 1);

    // Restore empty snapshot
    let empty_snapshot = json!({
        "version": 1,
        "notes": [],
        "files": [],
        "embeddings": [],
        "edges": []
    });
    snapshot::restore(&conn, &empty_snapshot).unwrap();

    // Verify database is empty
    let note_count: i64 = conn
        .query_row("SELECT COUNT(*) FROM notes", [], |row| row.get(0))
        .unwrap();
    assert_eq!(note_count, 0, "notes should be cleared");

    let file_count: i64 = conn
        .query_row("SELECT COUNT(*) FROM files", [], |row| row.get(0))
        .unwrap();
    assert_eq!(file_count, 0, "files should be cleared");
}

#[test]
fn roundtrip_with_git_metadata() {
    let conn1 = connect(":memory:").unwrap();
    let project_root = "/test/project";

    // Insert note with git metadata
    storage::exec(
        &conn1,
        "INSERT INTO notes (id, file, project_root, line, column, node_path, tags, text, summary, commit_sha, blob_sha, node_text_hash, created_at, updated_at)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
        &[
            &"note-git" as &dyn rusqlite::ToSql,
            &"/test/file.rs",
            &project_root,
            &10_i64,
            &0_i64,
            &"[]",
            &"[]",
            &"Note with git metadata",
            &"Note with git metadata",
            &"abc123commit",
            &"def456blob",
            &"hash789",
            &1234567890_i64,
            &1234567890_i64,
        ],
    )
    .unwrap();

    // Save and restore
    let snapshot = snapshot::create(&conn1, None).unwrap();
    let conn2 = connect(":memory:").unwrap();
    snapshot::restore(&conn2, &snapshot).unwrap();

    // Verify git metadata preserved
    let (commit_sha, blob_sha, node_text_hash): (Option<String>, Option<String>, Option<String>) =
        conn2
            .query_row(
                "SELECT commit_sha, blob_sha, node_text_hash FROM notes WHERE id = 'note-git'",
                [],
                |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
            )
            .unwrap();
    assert_eq!(commit_sha.as_deref(), Some("abc123commit"));
    assert_eq!(blob_sha.as_deref(), Some("def456blob"));
    assert_eq!(node_text_hash.as_deref(), Some("hash789"));
}

#[test]
fn snapshot_with_large_text_within_limits() {
    let conn1 = connect(":memory:").unwrap();

    // Create note with large text (but within limits)
    let large_text = "a".repeat(500_000); // 500KB, below 1MB limit
    insert_test_note(&conn1, "note-large", "/test.rs", "/test", 1, 0, &large_text);

    // Save and restore
    let snapshot = snapshot::create(&conn1, None).unwrap();
    let conn2 = connect(":memory:").unwrap();
    let result = snapshot::restore(&conn2, &snapshot);

    assert!(result.is_ok(), "should handle large text within limits");
    verify_note(&conn2, "note-large", "/test.rs", &large_text);
}

// ============================================================================
// QuickCheck property-based tests
// ============================================================================

/// Newtype for generating valid note IDs (alphanumeric + dashes)
#[derive(Debug, Clone)]
struct ValidNoteId(String);

impl quickcheck::Arbitrary for ValidNoteId {
    fn arbitrary(g: &mut quickcheck::Gen) -> Self {
        let len = usize::arbitrary(g) % 32 + 8; // 8-40 chars
        let chars: String = (0..len)
            .map(|_| {
                let choices = b"abcdefghijklmnopqrstuvwxyz0123456789-";
                let idx = usize::arbitrary(g) % choices.len();
                choices[idx] as char
            })
            .collect();
        ValidNoteId(chars)
    }
}

/// Newtype for generating valid file paths
#[derive(Debug, Clone)]
struct ValidPath(String);

impl quickcheck::Arbitrary for ValidPath {
    fn arbitrary(g: &mut quickcheck::Gen) -> Self {
        let segments = usize::arbitrary(g) % 4 + 1; // 1-5 path segments
        let path: String = (0..segments)
            .map(|_| {
                let len = usize::arbitrary(g) % 10 + 1;
                let seg: String = (0..len)
                    .map(|_| {
                        let choices = b"abcdefghijklmnopqrstuvwxyz0123456789_";
                        let idx = usize::arbitrary(g) % choices.len();
                        choices[idx] as char
                    })
                    .collect();
                seg
            })
            .collect::<Vec<_>>()
            .join("/");
        ValidPath(format!("/test/{}.rs", path))
    }
}

/// Newtype for generating safe note text (no control chars)
#[derive(Debug, Clone)]
struct SafeText(String);

impl quickcheck::Arbitrary for SafeText {
    fn arbitrary(g: &mut quickcheck::Gen) -> Self {
        let len = usize::arbitrary(g) % 200;
        let text: String = (0..len)
            .map(|_| {
                let choices = b"abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.,!?\n";
                let idx = usize::arbitrary(g) % choices.len();
                choices[idx] as char
            })
            .collect();
        SafeText(text)
    }
}

#[quickcheck_macros::quickcheck]
fn prop_snapshot_roundtrip_preserves_notes(
    id: ValidNoteId,
    file: ValidPath,
    text: SafeText,
    line: u16,
    column: u16,
) -> bool {
    let conn1 = connect(":memory:").unwrap();
    let project_root = "/test";
    let line = (line as i64).max(1); // Ensure positive line
    let column = column as i64;

    // Insert note
    insert_test_note(&conn1, &id.0, &file.0, project_root, line, column, &text.0);

    // Create snapshot
    let snapshot = match snapshot::create(&conn1, None) {
        Ok(s) => s,
        Err(_) => return false,
    };

    // Restore to new database
    let conn2 = connect(":memory:").unwrap();
    if snapshot::restore(&conn2, &snapshot).is_err() {
        return false;
    }

    // Verify note preserved
    let result: Result<(String, String, i64, i64), _> = conn2.query_row(
        "SELECT file, text, line, column FROM notes WHERE id = ?",
        [&id.0],
        |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?, row.get(3)?)),
    );

    match result {
        Ok((restored_file, restored_text, restored_line, restored_column)) => {
            restored_file == file.0
                && restored_text == text.0
                && restored_line == line
                && restored_column == column
        }
        Err(_) => false,
    }
}

#[quickcheck_macros::quickcheck]
fn prop_snapshot_roundtrip_preserves_edges(
    src_id: ValidNoteId,
    dst_id: ValidNoteId,
) -> bool {
    // Skip if src and dst are the same
    if src_id.0 == dst_id.0 {
        return true;
    }

    let conn1 = connect(":memory:").unwrap();
    let project_root = "/test";

    // Insert notes first (edges need valid notes)
    insert_test_note(&conn1, &src_id.0, "/test/src.rs", project_root, 1, 0, "source");
    insert_test_note(&conn1, &dst_id.0, "/test/dst.rs", project_root, 2, 0, "dest");
    insert_test_edge(&conn1, &src_id.0, &dst_id.0, "link", project_root);

    // Create snapshot
    let snapshot = match snapshot::create(&conn1, None) {
        Ok(s) => s,
        Err(_) => return false,
    };

    // Restore to new database
    let conn2 = connect(":memory:").unwrap();
    if snapshot::restore(&conn2, &snapshot).is_err() {
        return false;
    }

    // Verify edge preserved
    let result: Result<String, _> = conn2.query_row(
        "SELECT kind FROM edges WHERE src = ? AND dst = ?",
        [&src_id.0, &dst_id.0],
        |row| row.get(0),
    );

    result.map(|kind| kind == "link").unwrap_or(false)
}

#[quickcheck_macros::quickcheck]
fn prop_malformed_json_never_panics(random_bytes: Vec<u8>) -> bool {
    let conn = connect(":memory:").unwrap();

    // Try to parse random bytes as JSON
    let maybe_json: Result<serde_json::Value, _> =
        serde_json::from_slice(&random_bytes);

    // If it parses as JSON, try to restore it
    if let Ok(json_value) = maybe_json {
        // This should never panic, even with garbage JSON
        let _ = snapshot::restore(&conn, &json_value);
    }

    // If we got here without panicking, the property holds
    true
}

#[quickcheck_macros::quickcheck]
fn prop_arbitrary_json_object_never_panics(
    version: Option<i64>,
    has_notes: bool,
    has_files: bool,
) -> bool {
    let conn = connect(":memory:").unwrap();

    // Build a somewhat valid-looking JSON object with random structure
    let mut obj = serde_json::Map::new();

    if let Some(v) = version {
        obj.insert("version".to_string(), json!(v));
    }

    if has_notes {
        obj.insert("notes".to_string(), json!([]));
    }

    if has_files {
        obj.insert("files".to_string(), json!([]));
    }

    let json_value = serde_json::Value::Object(obj);

    // This should never panic
    let _ = snapshot::restore(&conn, &json_value);

    true
}

#[quickcheck_macros::quickcheck]
fn prop_version_field_variations(version: i64) -> bool {
    let conn = connect(":memory:").unwrap();

    let snapshot = json!({
        "version": version,
        "notes": [],
        "files": [],
        "embeddings": [],
        "edges": []
    });

    // Restore should handle any version gracefully (no panic)
    let _ = snapshot::restore(&conn, &snapshot);

    // Database should still be accessible
    let count: Result<i64, _> = conn.query_row("SELECT COUNT(*) FROM notes", [], |row| row.get(0));
    count.is_ok()
}
