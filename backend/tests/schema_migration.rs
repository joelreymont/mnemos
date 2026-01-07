//! Integration tests for schema migration and version checking.
//!
//! These tests simulate real-world scenarios where users have old databases
//! and verify the UX is clear and helpful.

use assert_cmd::cargo::cargo_bin_cmd;
use rpc::decode_framed;
use rusqlite::Connection;
use std::process::Command;
use tempfile::NamedTempFile;

/// Create a database with the old schema (missing content_hash, no schema_meta)
fn create_old_schema_db(path: &str) {
    let conn = Connection::open(path).unwrap();
    conn.execute_batch(
        r#"
        -- Old schema without content_hash column and no schema_meta table
        CREATE TABLE files (
            file TEXT PRIMARY KEY,
            project_root TEXT,
            content TEXT,
            updated_at INTEGER
        );
        CREATE TABLE notes (
            id TEXT PRIMARY KEY,
            file TEXT,
            project_root TEXT,
            line INTEGER,
            column INTEGER,
            node_path TEXT,
            tags TEXT,
            text TEXT,
            summary TEXT,
            created_at INTEGER,
            updated_at INTEGER
        );
        -- Insert some test data
        INSERT INTO files (file, project_root, content, updated_at)
        VALUES ('/test/main.rs', '/test', 'fn main() {}', 1234567890);
        INSERT INTO notes (id, file, project_root, line, column, text, summary, created_at, updated_at)
        VALUES ('note-123', '/test/main.rs', '/test', 1, 0, 'Old note', 'Old note summary', 1234567890, 1234567890);
        "#,
    )
    .unwrap();
}

/// Create a database with a future schema version (simulates downgrade scenario)
fn create_future_schema_db(path: &str) {
    let conn = Connection::open(path).unwrap();
    conn.execute_batch(
        r#"
        CREATE TABLE schema_meta (key TEXT PRIMARY KEY, value TEXT);
        INSERT INTO schema_meta (key, value) VALUES ('version', '999');
        "#,
    )
    .unwrap();
}

/// Format a JSON-RPC request with Content-Length header
fn format_request(json: &str) -> String {
    format!("Content-Length: {}\r\n\r\n{}", json.len(), json)
}

#[test]
fn old_schema_migrates_and_works() {
    let db_file = NamedTempFile::new().unwrap();
    let db_path = db_file.path().to_str().unwrap();

    // Create old schema database with data
    create_old_schema_db(db_path);

    // Send status request
    let req = format_request(r#"{"jsonrpc":"2.0","id":1,"method":"mnemos/status","params":{}}"#);

    let assert = cargo_bin_cmd!("mnemos")
        .env("MNEMOS_DB_PATH", db_path)
        .write_stdin(req)
        .assert()
        .success();

    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("Should decode response");
    let response = String::from_utf8_lossy(&body);

    // Should get a successful response with counts
    assert!(
        response.contains("\"result\""),
        "Expected result, got: {}",
        response
    );
    assert!(
        response.contains("\"notes\""),
        "Expected notes count in response"
    );

    // Verify schema was updated
    let conn = Connection::open(db_path).unwrap();
    let version: i64 = conn
        .query_row(
            "SELECT CAST(value AS INTEGER) FROM schema_meta WHERE key = 'version'",
            [],
            |row| row.get(0),
        )
        .expect("Schema version should be set after migration");
    assert_eq!(version, storage::SCHEMA_VERSION);

    // Verify content_hash column was added
    let has_content_hash: bool = conn
        .query_row(
            "SELECT 1 FROM pragma_table_info('files') WHERE name = 'content_hash'",
            [],
            |_| Ok(true),
        )
        .unwrap_or(false);
    assert!(
        has_content_hash,
        "content_hash column should exist after migration"
    );
}

#[test]
fn old_data_preserved_after_migration() {
    let db_file = NamedTempFile::new().unwrap();
    let db_path = db_file.path().to_str().unwrap();

    // Create old schema database with data
    create_old_schema_db(db_path);

    // Get the old note
    let req = format_request(r#"{"jsonrpc":"2.0","id":1,"method":"notes/get","params":{"id":"note-123"}}"#);

    let assert = cargo_bin_cmd!("mnemos")
        .env("MNEMOS_DB_PATH", db_path)
        .write_stdin(req)
        .assert()
        .success();

    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("Should decode response");
    let response = String::from_utf8_lossy(&body);

    assert!(
        response.contains("Old note"),
        "Old note should be preserved after migration: {}",
        response
    );
}

#[test]
fn future_schema_fails_with_clear_error() {
    let db_file = NamedTempFile::new().unwrap();
    let db_path = db_file.path().to_str().unwrap();

    // Create database with future schema version
    create_future_schema_db(db_path);

    // Try to use mnemos - should fail immediately
    let req = format_request(r#"{"jsonrpc":"2.0","id":1,"method":"mnemos/status","params":{}}"#);

    let assert = cargo_bin_cmd!("mnemos")
        .env("MNEMOS_DB_PATH", db_path)
        .write_stdin(req)
        .assert()
        .failure();

    // Error message should be helpful
    let stderr = String::from_utf8_lossy(&assert.get_output().stderr);
    assert!(
        stderr.contains("999") && stderr.contains("newer"),
        "Error should mention version 999 and 'newer'. Got: {}",
        stderr
    );
    assert!(
        stderr.contains("upgrade") || stderr.contains("different database"),
        "Error should suggest upgrading or using different db. Got: {}",
        stderr
    );
}

#[test]
fn index_project_works_after_migration() {
    let db_file = NamedTempFile::new().unwrap();
    let db_path = db_file.path().to_str().unwrap();

    // Create old schema database
    create_old_schema_db(db_path);

    // Create a test project directory
    let project_dir = tempfile::tempdir().unwrap();
    let test_file = project_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn hello() { println!(\"world\"); }").unwrap();

    // Initialize git repo (required for index-project)
    Command::new("git")
        .args(["init"])
        .current_dir(project_dir.path())
        .output()
        .unwrap();
    Command::new("git")
        .args(["add", "."])
        .current_dir(project_dir.path())
        .output()
        .unwrap();
    Command::new("git")
        .args(["commit", "-m", "init"])
        .current_dir(project_dir.path())
        .env("GIT_AUTHOR_NAME", "Test")
        .env("GIT_AUTHOR_EMAIL", "test@test.com")
        .env("GIT_COMMITTER_NAME", "Test")
        .env("GIT_COMMITTER_EMAIL", "test@test.com")
        .output()
        .unwrap();

    // Index the project - this uses content_hash internally
    let request = format!(
        r#"{{"jsonrpc":"2.0","id":1,"method":"mnemos/index-project","params":{{"file":"{}"}}}}"#,
        test_file.to_str().unwrap()
    );
    let req = format_request(&request);

    let assert = cargo_bin_cmd!("mnemos")
        .env("MNEMOS_DB_PATH", db_path)
        .write_stdin(req)
        .assert()
        .success();

    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("Should decode response");
    let response = String::from_utf8_lossy(&body);

    // Should succeed
    assert!(
        response.contains("\"result\"") && response.contains("indexed"),
        "Index should succeed after migration: {}",
        response
    );
}
