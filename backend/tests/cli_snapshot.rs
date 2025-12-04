//! Tests for CLI --save-snapshot and --load-snapshot commands.

use std::process::Command;
use tempfile::TempDir;

fn hemis_binary() -> String {
    // Use debug binary from target directory
    std::env::var("CARGO_BIN_EXE_hemis").unwrap_or_else(|_| {
        // Fall back to target/debug/hemis
        let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".to_string());
        format!("{}/target/debug/hemis", manifest_dir.trim_end_matches("/backend"))
    })
}

#[test]
fn cli_save_snapshot_creates_file() {
    // Create temp directory for snapshot and db
    let tmp = TempDir::new().unwrap();
    let snapshot_path = tmp.path().join("snapshot.json");
    let db_path = tmp.path().join("test.db");

    // Run save-snapshot command
    let output = Command::new(hemis_binary())
        .env("HEMIS_DB_PATH", db_path.to_str().unwrap())
        .arg("--save-snapshot")
        .arg(snapshot_path.to_str().unwrap())
        .output()
        .expect("failed to run hemis");

    // Check command succeeded
    assert!(output.status.success(), "save-snapshot failed: {}", String::from_utf8_lossy(&output.stderr));

    // Check output mentions snapshot saved
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("Snapshot saved"), "Expected 'Snapshot saved' in output: {}", stdout);

    // Verify file was created
    assert!(snapshot_path.exists(), "Snapshot file should exist");

    // Verify file is valid JSON with expected structure
    let content = std::fs::read_to_string(&snapshot_path).unwrap();
    let json: serde_json::Value = serde_json::from_str(&content).expect("Invalid JSON in snapshot");
    assert!(json.get("version").is_some(), "Snapshot should have version");
    assert!(json.get("counts").is_some(), "Snapshot should have counts");
    assert!(json.get("notes").is_some(), "Snapshot should have notes array");
    assert!(json.get("files").is_some(), "Snapshot should have files array");
}

#[test]
fn cli_load_snapshot_restores_data() {
    // Create temp directory
    let tmp = TempDir::new().unwrap();
    let snapshot_path = tmp.path().join("snapshot.json");
    let db_path = tmp.path().join("test.db");

    // Create a snapshot file with test data
    let snapshot_content = serde_json::json!({
        "version": 1,
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
    std::fs::write(&snapshot_path, serde_json::to_string_pretty(&snapshot_content).unwrap()).unwrap();

    // Run load-snapshot command
    let output = Command::new(hemis_binary())
        .env("HEMIS_DB_PATH", db_path.to_str().unwrap())
        .arg("--load-snapshot")
        .arg(snapshot_path.to_str().unwrap())
        .output()
        .expect("failed to run hemis");

    // Check command succeeded
    assert!(output.status.success(), "load-snapshot failed: {}", String::from_utf8_lossy(&output.stderr));

    // Check output mentions snapshot loaded
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("Snapshot loaded"), "Expected 'Snapshot loaded' in output: {}", stdout);
}

#[test]
fn cli_save_and_load_roundtrip() {
    // Create temp directory
    let tmp = TempDir::new().unwrap();
    let snapshot_path = tmp.path().join("snapshot.json");
    let db1_path = tmp.path().join("db1.db");
    let db2_path = tmp.path().join("db2.db");

    // Create first db and add a note via RPC
    {
        let conn = storage::connect(db1_path.to_str().unwrap()).unwrap();
        storage::exec(
            &conn,
            "INSERT INTO notes (id, file, project_root, line, column, node_path, tags, text, summary, created_at, updated_at)
             VALUES ('test-id-123', '/test/file.rs', '/test', 10, 5, '[]', '[]', 'Test note', 'Test', 1234567890, 1234567890)",
            &[],
        ).unwrap();
    }

    // Save snapshot from db1
    let output = Command::new(hemis_binary())
        .env("HEMIS_DB_PATH", db1_path.to_str().unwrap())
        .arg("--save-snapshot")
        .arg(snapshot_path.to_str().unwrap())
        .output()
        .expect("failed to run hemis save");

    assert!(output.status.success(), "save failed: {}", String::from_utf8_lossy(&output.stderr));

    // Verify snapshot has note
    let content = std::fs::read_to_string(&snapshot_path).unwrap();
    let json: serde_json::Value = serde_json::from_str(&content).unwrap();
    assert_eq!(json["counts"]["notes"], 1, "Should have 1 note in snapshot");

    // Load snapshot into db2
    let output = Command::new(hemis_binary())
        .env("HEMIS_DB_PATH", db2_path.to_str().unwrap())
        .arg("--load-snapshot")
        .arg(snapshot_path.to_str().unwrap())
        .output()
        .expect("failed to run hemis load");

    assert!(output.status.success(), "load failed: {}", String::from_utf8_lossy(&output.stderr));

    // Verify db2 has the note
    let conn2 = storage::connect(db2_path.to_str().unwrap()).unwrap();
    let count: i64 = conn2.query_row("SELECT COUNT(*) FROM notes", [], |r| r.get(0)).unwrap();
    assert_eq!(count, 1, "db2 should have 1 note after restore");
}

#[test]
fn cli_load_snapshot_nonexistent_file_fails() {
    let tmp = TempDir::new().unwrap();
    let db_path = tmp.path().join("test.db");

    // Run load-snapshot with nonexistent file
    let output = Command::new(hemis_binary())
        .env("HEMIS_DB_PATH", db_path.to_str().unwrap())
        .arg("--load-snapshot")
        .arg("/nonexistent/path/snapshot.json")
        .output()
        .expect("failed to run hemis");

    // Should fail
    assert!(!output.status.success(), "load-snapshot should fail for nonexistent file");
}

#[test]
fn cli_load_snapshot_invalid_json_fails() {
    let tmp = TempDir::new().unwrap();
    let snapshot_path = tmp.path().join("invalid.json");
    let db_path = tmp.path().join("test.db");

    // Create invalid JSON file
    std::fs::write(&snapshot_path, "not valid json {{{").unwrap();

    // Run load-snapshot
    let output = Command::new(hemis_binary())
        .env("HEMIS_DB_PATH", db_path.to_str().unwrap())
        .arg("--load-snapshot")
        .arg(snapshot_path.to_str().unwrap())
        .output()
        .expect("failed to run hemis");

    // Should fail
    assert!(!output.status.success(), "load-snapshot should fail for invalid JSON");
}
