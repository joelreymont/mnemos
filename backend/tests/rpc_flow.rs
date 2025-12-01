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
    let assert = cargo_bin_cmd!("hemis")
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
fn handles_large_framed_body() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let big_content = "fn main() {}\n".repeat(2_000); // ~20k bytes to exceed the read buffer.
    let req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 99,
        "method": "index/add-file",
        "params": {
            "file": "/tmp/big.rs",
            "projectRoot": "/tmp",
            "content": big_content
        }
    })
    .to_string();
    let input = format!("Content-Length: {}\r\n\r\n{}", req.len(), req);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("framed response for large body");
    let resp: Response = serde_json::from_slice(&body)?;
    assert_eq!(resp.id, Some(serde_json::json!(99)));
    assert!(
        resp.error.is_none(),
        "expected success, got {:?}",
        resp.error
    );
    Ok(())
}

#[test]
fn handles_line_delimited_shutdown() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let req = r#"{"jsonrpc":"2.0","id":7,"method":"shutdown","params":{}}"#;
    let assert = cargo_bin_cmd!("hemis")
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
    let assert = cargo_bin_cmd!("hemis")
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
    assert_eq!(
        filtered_list.len(),
        1,
        "stale notes should still be returned for re-anchoring"
    );
    assert_eq!(
        filtered_list[0]
            .get("stale")
            .and_then(|v| v.as_bool())
            .unwrap_or(false),
        true
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

#[test]
fn filters_stale_notes_by_blob() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let repo = tempfile::tempdir()?;
    let file = repo.path().join("main.rs");
    fs::write(&file, "fn main() {}\n")?;
    git(repo.path(), &["init"])?;
    git(repo.path(), &["config", "user.email", "test@example.com"])?;
    git(repo.path(), &["config", "user.name", "test"])?;
    git(repo.path(), &["add", "main.rs"])?;
    git(repo.path(), &["commit", "-m", "init"])?;
    let old_commit = git(repo.path(), &["rev-parse", "HEAD"])?;
    let old_blob = git(repo.path(), &["hash-object", "main.rs"])?;

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
            "text": "blob note",
            "tags": []
        }
    })
    .to_string();

    // Use a mismatching blob to force stale without changing commit.
    let new_blob = format!("{}-mismatch", old_blob);
    let new_commit = old_commit.clone();

    let req_stale_filtered = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "notes/list-for-file",
        "params": {
            "file": file_str,
            "projectRoot": root_str,
            "commit": new_commit,
            "blob": new_blob,
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
            "commit": new_commit,
            "blob": format!("{}-mismatch", old_blob),
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
    let assert = cargo_bin_cmd!("hemis")
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
    let note_obj = created
        .result
        .as_ref()
        .and_then(|v| v.as_object())
        .cloned()
        .expect("create result object");
    let note_commit = note_obj
        .get("commitSha")
        .and_then(|v| v.as_str())
        .expect("note should capture commitSha")
        .to_string();
    let note_blob = note_obj
        .get("blobSha")
        .and_then(|v| v.as_str())
        .expect("note should capture blobSha")
        .to_string();
    assert_eq!(note_commit, old_commit);
    assert_eq!(note_blob, old_blob);
    let filtered: Response = serde_json::from_slice(&bodies[1])?;
    let filtered_list = filtered
        .result
        .and_then(|v| v.as_array().cloned())
        .unwrap_or_default();
    assert_eq!(
        filtered_list.len(),
        1,
        "stale notes should still return for blob/commit mismatch"
    );
    assert!(
        filtered_list[0]
            .get("stale")
            .and_then(|v| v.as_bool())
            .unwrap_or(false)
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
        "stale note should be marked stale when included for blob mismatch"
    );
    Ok(())
}

#[test]
fn indexes_project_and_searches() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let root = tempfile::tempdir()?;
    let file_a = root.path().join("a.rs");
    let file_b = root.path().join("b.rs");
    fs::write(&file_a, "fn alpha() {}")?;
    fs::write(&file_b, "fn beta() {}")?;

    let req_index = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "hemis/index-project",
        "params": { "projectRoot": root.path().to_string_lossy() }
    })
    .to_string();
    let req_search = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "hemis/search",
        "params": {
            "query": "alpha",
            "projectRoot": root.path().to_string_lossy()
        }
    })
    .to_string();
    let input = format!(
        "Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}",
        req_index.len(),
        req_index,
        req_search.len(),
        req_search
    );
    let assert = cargo_bin_cmd!("hemis")
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
    assert_eq!(bodies.len(), 2);
    let search_resp: Response = serde_json::from_slice(&bodies[1])?;
    let hits = search_resp
        .result
        .as_ref()
        .and_then(|v| v.as_array())
        .cloned()
        .unwrap_or_default();
    assert_eq!(hits.len(), 1);
    assert!(hits[0]
        .get("file")
        .and_then(|v| v.as_str())
        .unwrap()
        .ends_with("a.rs"));
    Ok(())
}

#[test]
fn lists_and_reads_files() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let root = tempfile::tempdir()?;
    let file_path = root.path().join("a.rs");
    fs::write(&file_path, "fn main() {}\n")?;
    let root_str = root.path().to_string_lossy().to_string();
    let file_str = file_path.to_string_lossy().to_string();

    let req_list = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "hemis/list-files",
        "params": { "projectRoot": root_str }
    })
    .to_string();
    let req_get = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "hemis/get-file",
        "params": { "file": file_str }
    })
    .to_string();
    let input = format!(
        "Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}",
        req_list.len(),
        req_list,
        req_get.len(),
        req_get
    );
    let assert = cargo_bin_cmd!("hemis")
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
    assert_eq!(bodies.len(), 2);
    let list_resp: Response = serde_json::from_slice(&bodies[0])?;
    let files = list_resp
        .result
        .as_ref()
        .and_then(|v| v.as_array())
        .cloned()
        .unwrap_or_default();
    assert_eq!(files.len(), 1);
    assert_eq!(
        files[0].as_str().unwrap(),
        file_path.to_string_lossy().to_string()
    );
    let get_resp: Response = serde_json::from_slice(&bodies[1])?;
    let obj = get_resp
        .result
        .as_ref()
        .and_then(|v| v.as_object())
        .unwrap();
    assert_eq!(obj.get("file").and_then(|v| v.as_str()).unwrap(), file_str);
    assert_eq!(
        obj.get("content").and_then(|v| v.as_str()).unwrap(),
        "fn main() {}\n"
    );
    Ok(())
}

#[test]
fn backlinks_returns_linking_notes() -> anyhow::Result<()> {
    // Test: create note A, then create note B that links to A, then query backlinks for A.
    let db = NamedTempFile::new()?;

    // Create note A
    let req_create_a = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "notes/create",
        "params": {
            "file": "/tmp/test.rs",
            "projectRoot": "/tmp",
            "line": 1,
            "column": 0,
            "text": "Note A - the target",
            "tags": []
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req_create_a.len(), req_create_a);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("create A response");
    let resp_a: Response = serde_json::from_slice(&body)?;
    let note_a_id = resp_a
        .result
        .as_ref()
        .and_then(|v| v.get("id"))
        .and_then(|v| v.as_str())
        .expect("note A id")
        .to_string();

    // Create note B that links to A using [[desc][id]] format
    let link_text = format!("Note B links to [[target][{}]]", note_a_id);
    let req_create_b = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "notes/create",
        "params": {
            "file": "/tmp/test.rs",
            "projectRoot": "/tmp",
            "line": 10,
            "column": 0,
            "text": link_text,
            "tags": []
        }
    })
    .to_string();

    // Query backlinks for note A
    let req_backlinks = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 3,
        "method": "notes/backlinks",
        "params": { "id": note_a_id }
    })
    .to_string();

    let input = format!(
        "Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}",
        req_create_b.len(),
        req_create_b,
        req_backlinks.len(),
        req_backlinks
    );
    let assert = cargo_bin_cmd!("hemis")
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
    assert_eq!(bodies.len(), 2, "expected create B and backlinks responses");

    let resp_b: Response = serde_json::from_slice(&bodies[0])?;
    let note_b_id = resp_b
        .result
        .as_ref()
        .and_then(|v| v.get("id"))
        .and_then(|v| v.as_str())
        .expect("note B id");

    let backlinks_resp: Response = serde_json::from_slice(&bodies[1])?;
    let backlinks = backlinks_resp
        .result
        .as_ref()
        .and_then(|v| v.as_array())
        .cloned()
        .unwrap_or_default();

    assert_eq!(backlinks.len(), 1, "note A should have exactly one backlink");
    let linking_note = &backlinks[0];
    assert_eq!(
        linking_note.get("id").and_then(|v| v.as_str()).unwrap(),
        note_b_id,
        "backlink should be note B"
    );
    assert!(
        linking_note
            .get("text")
            .and_then(|v| v.as_str())
            .unwrap()
            .contains("Note B links to"),
        "backlink text should match note B"
    );

    Ok(())
}

/// Verify that deleting a note removes its edges (both outgoing and incoming).
#[test]
fn delete_note_removes_edges() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;

    // Create note A (the target)
    let req_create_a = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "notes/create",
        "params": {
            "file": "/tmp/test.rs",
            "projectRoot": "/tmp",
            "line": 1,
            "column": 0,
            "text": "Note A - target",
            "tags": []
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req_create_a.len(), req_create_a);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("create A response");
    let resp_a: Response = serde_json::from_slice(&body)?;
    let note_a_id = resp_a
        .result
        .as_ref()
        .and_then(|v| v.get("id"))
        .and_then(|v| v.as_str())
        .expect("note A id")
        .to_string();

    // Create note B that links to A
    let link_text = format!("Note B links to [[target][{}]]", note_a_id);
    let req_create_b = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "notes/create",
        "params": {
            "file": "/tmp/test.rs",
            "projectRoot": "/tmp",
            "line": 10,
            "column": 0,
            "text": link_text,
            "tags": []
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req_create_b.len(), req_create_b);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("create B response");
    let resp_b: Response = serde_json::from_slice(&body)?;
    let note_b_id = resp_b
        .result
        .as_ref()
        .and_then(|v| v.get("id"))
        .and_then(|v| v.as_str())
        .expect("note B id")
        .to_string();

    // Verify backlinks exist before delete
    let req_backlinks = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 3,
        "method": "notes/backlinks",
        "params": { "id": note_a_id }
    })
    .to_string();
    let input = format!("Content-Length: {}\r\n\r\n{}", req_backlinks.len(), req_backlinks);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("backlinks before delete");
    let resp: Response = serde_json::from_slice(&body)?;
    let backlinks = resp
        .result
        .as_ref()
        .and_then(|v| v.as_array())
        .cloned()
        .unwrap_or_default();
    assert_eq!(backlinks.len(), 1, "should have one backlink before delete");

    // Delete note B (the one with the outgoing link)
    let req_delete = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 4,
        "method": "notes/delete",
        "params": { "id": note_b_id }
    })
    .to_string();
    let input = format!("Content-Length: {}\r\n\r\n{}", req_delete.len(), req_delete);
    cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();

    // Verify backlinks are gone after delete
    let req_backlinks = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 5,
        "method": "notes/backlinks",
        "params": { "id": note_a_id }
    })
    .to_string();
    let input = format!("Content-Length: {}\r\n\r\n{}", req_backlinks.len(), req_backlinks);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("backlinks after delete");
    let resp: Response = serde_json::from_slice(&body)?;
    let backlinks = resp
        .result
        .as_ref()
        .and_then(|v| v.as_array())
        .cloned()
        .unwrap_or_default();
    assert_eq!(
        backlinks.len(),
        0,
        "backlinks should be empty after deleting linking note"
    );

    Ok(())
}
