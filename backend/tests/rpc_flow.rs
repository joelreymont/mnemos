use assert_cmd::cargo::cargo_bin_cmd;
use rpc::{decode_framed, Response};
use serde_json;
use std::fs;
use std::process::Command;
use tempfile::NamedTempFile;

fn git(dir: &std::path::Path, args: &[&str]) -> anyhow::Result<String> {
    let output = Command::new("git")
        .current_dir(dir)
        .env("GIT_AUTHOR_NAME", "test")
        .env("GIT_AUTHOR_EMAIL", "test@example.com")
        .env("GIT_COMMITTER_NAME", "test")
        .env("GIT_COMMITTER_EMAIL", "test@example.com")
        .args(args)
        .output()?;
    if !output.status.success() {
        anyhow::bail!(
            "git {:?} failed: {}",
            args,
            String::from_utf8_lossy(&output.stderr)
        );
    }
    Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
}

#[test]
fn handles_multiple_framed_requests() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let req_create = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "notes/create",
        "params": {
            "file": "/tmp/test.rs",
            "projectRoot": "/tmp",
            "line": 1,
            "column": 0,
            "nodePath": ["fn"],
            "tags": ["a"],
            "text": "hello"
        }
    })
    .to_string();
    let req_list = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "notes/list-by-node",
        "params": {
            "file": "/tmp/test.rs",
            "projectRoot": "/tmp",
            "nodePath": ["fn"]
        }
    })
    .to_string();
    let input = format!(
        "Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}",
        req_create.len(),
        req_create,
        req_list.len(),
        req_list
    );
    let assert = cargo_bin_cmd!("backend")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let mut stdout = assert.get_output().stdout.clone();
    let mut bodies = Vec::new();
    while let Some((body, used)) = decode_framed(&stdout) {
        bodies.push(body);
        stdout.drain(..used);
    }
    assert_eq!(bodies.len(), 2, "expected two framed responses");
    let first: Response = serde_json::from_slice(&bodies[0])?;
    let second: Response = serde_json::from_slice(&bodies[1])?;

    let note_val = first.result.expect("create response should have result");
    let note_id = note_val
        .get("id")
        .and_then(|v| v.as_str())
        .expect("note id");
    let list = second
        .result
        .expect("list response should have result")
        .as_array()
        .cloned()
        .expect("list result should be array");
    assert_eq!(list.len(), 1);
    let returned = &list[0];
    assert_eq!(
        returned.get("id").and_then(|v| v.as_str()).expect("id"),
        note_id
    );
    assert_eq!(
        returned
            .get("stale")
            .and_then(|v| v.as_bool())
            .expect("stale flag"),
        false
    );
    Ok(())
}

#[test]
fn handles_line_delimited_shutdown() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let req = r#"{"jsonrpc":"2.0","id":7,"method":"shutdown","params":{}}"#;
    let assert = cargo_bin_cmd!("backend")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(format!("{req}\n"))
        .assert()
        .success();
    let stdout = assert.get_output().stdout.clone();
    let stdout_str = String::from_utf8(stdout)?;
    let resp: Response = serde_json::from_str(stdout_str.trim())?;
    assert_eq!(resp.id, Some(serde_json::json!(7)));
    assert_eq!(resp.result, Some(serde_json::json!("shutting down")));
    Ok(())
}

#[test]
fn filters_stale_notes_by_commit() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let repo = tempfile::tempdir()?;
    let file = repo.path().join("main.rs");
    fs::write(&file, "fn main() {}\n")?;
    git(repo.path(), &["init"])?;
    git(repo.path(), &["config", "user.email", "test@example.com"])?;
    git(repo.path(), &["config", "user.name", "test"])?;
    git(repo.path(), &["add", "main.rs"])?;
    git(repo.path(), &["commit", "-m", "init"])?;
    let file_str = file.to_string_lossy().to_string();
    let root_str = repo.path().to_string_lossy().to_string();
    let req_create = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "notes/create",
        "params": {
            "file": file_str,
            "projectRoot": root_str,
            "line": 1,
            "column": 0,
            "text": "stale note",
            "tags": []
        }
    })
    .to_string();
    let req_stale_filtered = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "notes/list-for-file",
        "params": {
            "file": file_str,
            "projectRoot": root_str,
            "commit": "deadbeef",
            "includeStale": false
        }
    })
    .to_string();
    let req_stale_included = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 3,
        "method": "notes/list-for-file",
        "params": {
            "file": file_str,
            "projectRoot": root_str,
            "commit": "deadbeef",
            "includeStale": true
        }
    })
    .to_string();
    let input = format!(
        "Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}",
        req_create.len(),
        req_create,
        req_stale_filtered.len(),
        req_stale_filtered,
        req_stale_included.len(),
        req_stale_included
    );
    let assert = cargo_bin_cmd!("backend")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let mut stdout = assert.get_output().stdout.clone();
    let mut bodies = Vec::new();
    while let Some((body, used)) = decode_framed(&stdout) {
        bodies.push(body);
        stdout.drain(..used);
    }
    assert_eq!(bodies.len(), 3);
    let created: Response = serde_json::from_slice(&bodies[0])?;
    assert!(created.result.is_some());
    let filtered: Response = serde_json::from_slice(&bodies[1])?;
    let filtered_list = filtered
        .result
        .and_then(|v| v.as_array().cloned())
        .unwrap_or_default();
    assert!(
        filtered_list.is_empty(),
        "stale notes should be filtered out"
    );
    let included: Response = serde_json::from_slice(&bodies[2])?;
    let included_list = included
        .result
        .and_then(|v| v.as_array().cloned())
        .unwrap_or_default();
    assert_eq!(included_list.len(), 1);
    let stale_flag = included_list[0]
        .get("stale")
        .and_then(|v| v.as_bool())
        .unwrap_or(false);
    assert!(
        stale_flag,
        "stale note should be marked stale when included"
    );
    Ok(())
}
