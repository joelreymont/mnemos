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
    let file_obj = files[0].as_object().unwrap();
    assert_eq!(
        file_obj.get("file").and_then(|v| v.as_str()).unwrap(),
        file_path.to_string_lossy().to_string()
    );
    assert!(file_obj.get("size").and_then(|v| v.as_u64()).is_some());
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

/// Verify UTF-8 handling in note summary - should not panic on multi-byte characters.
#[test]
fn handles_utf8_in_summary() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    // Text with multi-byte UTF-8 characters that would cause panic if slicing at byte 57
    // Use Japanese text mixed with ASCII to exceed the 60 char limit
    let unicode_text = "Hello \u{3053}\u{3093}\u{306B}\u{3061}\u{306F} ".repeat(15); // ~90 chars
    let req_create = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "notes/create",
        "params": {
            "file": "/tmp/test.rs",
            "projectRoot": "/tmp",
            "line": 1,
            "column": 0,
            "text": unicode_text,
            "tags": []
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req_create.len(), req_create);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("create response");
    let resp: Response = serde_json::from_slice(&body)?;
    assert!(resp.error.is_none(), "should not error on UTF-8 text");
    let note = resp.result.expect("should have result");
    let text = note.get("text").and_then(|v| v.as_str()).expect("text");
    assert!(text.chars().count() > 60, "text should be longer than 60 chars");
    let summary = note.get("summary").and_then(|v| v.as_str()).expect("summary");
    // Summary should be truncated with "..." and not panic
    assert!(summary.ends_with("..."), "long text should be truncated");
    // Should contain Japanese characters
    assert!(summary.contains("\u{3053}"), "summary should preserve Japanese chars");
    // Summary char count should be exactly 60 (57 + "...")
    assert_eq!(summary.chars().count(), 60, "summary should be 57 chars + ...");
    Ok(())
}

/// Verify that updating a note's text updates its edges.
#[test]
fn update_note_updates_edges() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;

    // Create note A (first target)
    let req_create_a = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "notes/create",
        "params": {
            "file": "/tmp/test.rs",
            "projectRoot": "/tmp",
            "line": 1,
            "column": 0,
            "text": "Note A",
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
    let (body, _) = decode_framed(&stdout).unwrap();
    let resp_a: Response = serde_json::from_slice(&body)?;
    let note_a_id = resp_a
        .result
        .as_ref()
        .and_then(|v| v.get("id"))
        .and_then(|v| v.as_str())
        .expect("note A id")
        .to_string();

    // Create note B (second target)
    let req_create_b = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "notes/create",
        "params": {
            "file": "/tmp/test.rs",
            "projectRoot": "/tmp",
            "line": 5,
            "column": 0,
            "text": "Note B",
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
    let (body, _) = decode_framed(&stdout).unwrap();
    let resp_b: Response = serde_json::from_slice(&body)?;
    let note_b_id = resp_b
        .result
        .as_ref()
        .and_then(|v| v.get("id"))
        .and_then(|v| v.as_str())
        .expect("note B id")
        .to_string();

    // Create note C that links to A
    let link_text_a = format!("Note C links to [[A][{}]]", note_a_id);
    let req_create_c = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 3,
        "method": "notes/create",
        "params": {
            "file": "/tmp/test.rs",
            "projectRoot": "/tmp",
            "line": 10,
            "column": 0,
            "text": link_text_a,
            "tags": []
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req_create_c.len(), req_create_c);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).unwrap();
    let resp_c: Response = serde_json::from_slice(&body)?;
    let note_c_id = resp_c
        .result
        .as_ref()
        .and_then(|v| v.get("id"))
        .and_then(|v| v.as_str())
        .expect("note C id")
        .to_string();

    // Verify A has a backlink from C
    let req_backlinks_a = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 4,
        "method": "notes/backlinks",
        "params": { "id": note_a_id }
    })
    .to_string();
    let input = format!(
        "Content-Length: {}\r\n\r\n{}",
        req_backlinks_a.len(),
        req_backlinks_a
    );
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).unwrap();
    let resp: Response = serde_json::from_slice(&body)?;
    let backlinks = resp
        .result
        .as_ref()
        .and_then(|v| v.as_array())
        .cloned()
        .unwrap_or_default();
    assert_eq!(backlinks.len(), 1, "A should have one backlink from C");

    // Verify B has no backlinks
    let req_backlinks_b = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 5,
        "method": "notes/backlinks",
        "params": { "id": note_b_id }
    })
    .to_string();
    let input = format!(
        "Content-Length: {}\r\n\r\n{}",
        req_backlinks_b.len(),
        req_backlinks_b
    );
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).unwrap();
    let resp: Response = serde_json::from_slice(&body)?;
    let backlinks = resp
        .result
        .as_ref()
        .and_then(|v| v.as_array())
        .cloned()
        .unwrap_or_default();
    assert_eq!(backlinks.len(), 0, "B should have no backlinks initially");

    // Update note C to link to B instead of A
    let link_text_b = format!("Note C now links to [[B][{}]]", note_b_id);
    let req_update = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 6,
        "method": "notes/update",
        "params": {
            "id": note_c_id,
            "text": link_text_b
        }
    })
    .to_string();
    let input = format!("Content-Length: {}\r\n\r\n{}", req_update.len(), req_update);
    cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();

    // Verify A no longer has backlinks
    let req_backlinks_a = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 7,
        "method": "notes/backlinks",
        "params": { "id": note_a_id }
    })
    .to_string();
    let input = format!(
        "Content-Length: {}\r\n\r\n{}",
        req_backlinks_a.len(),
        req_backlinks_a
    );
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).unwrap();
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
        "A should have no backlinks after C's update"
    );

    // Verify B now has a backlink from C
    let req_backlinks_b = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 8,
        "method": "notes/backlinks",
        "params": { "id": note_b_id }
    })
    .to_string();
    let input = format!(
        "Content-Length: {}\r\n\r\n{}",
        req_backlinks_b.len(),
        req_backlinks_b
    );
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).unwrap();
    let resp: Response = serde_json::from_slice(&body)?;
    let backlinks = resp
        .result
        .as_ref()
        .and_then(|v| v.as_array())
        .cloned()
        .unwrap_or_default();
    assert_eq!(backlinks.len(), 1, "B should have one backlink from C");
    assert_eq!(
        backlinks[0].get("id").and_then(|v| v.as_str()).unwrap(),
        note_c_id,
        "backlink should be note C"
    );

    Ok(())
}

/// Verify reattach updates note position and refreshes git SHAs.
#[test]
fn reattach_updates_note_position() -> anyhow::Result<()> {
    // Test: create a note, then reattach it to a new position.
    let db = NamedTempFile::new()?;
    let repo = tempfile::tempdir()?;
    let file = repo.path().join("main.rs");
    fs::write(&file, "fn main() {}\nfn other() {}\n")?;
    git(repo.path(), &["init"])?;
    git(repo.path(), &["config", "user.email", "test@example.com"])?;
    git(repo.path(), &["config", "user.name", "test"])?;
    git(repo.path(), &["add", "main.rs"])?;
    git(repo.path(), &["commit", "-m", "init"])?;

    let file_str = file.to_string_lossy().to_string();
    let root_str = repo.path().to_string_lossy().to_string();

    // Create note at line 1
    let req_create = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "notes/create",
        "params": {
            "file": file_str,
            "projectRoot": root_str,
            "line": 1,
            "column": 0,
            "text": "Note on main",
            "tags": []
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req_create.len(), req_create);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("create response");
    let resp: Response = serde_json::from_slice(&body)?;
    let note_id = resp
        .result
        .as_ref()
        .and_then(|v| v.get("id"))
        .and_then(|v| v.as_str())
        .expect("note id")
        .to_string();
    let orig_line = resp
        .result
        .as_ref()
        .and_then(|v| v.get("line"))
        .and_then(|v| v.as_i64())
        .expect("orig line");
    assert_eq!(orig_line, 1, "note should start at line 1");

    // Reattach note to line 2, column 5
    let req_reattach = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "notes/reattach",
        "params": {
            "id": note_id,
            "file": file_str,
            "line": 2,
            "column": 5,
            "nodePath": ["fn", "other"]
        }
    })
    .to_string();

    let input = format!(
        "Content-Length: {}\r\n\r\n{}",
        req_reattach.len(),
        req_reattach
    );
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("reattach response");
    let resp: Response = serde_json::from_slice(&body)?;
    assert!(resp.error.is_none(), "reattach should succeed");

    let updated_note = resp.result.expect("reattach result");
    assert_eq!(
        updated_note.get("line").and_then(|v| v.as_i64()).unwrap(),
        2,
        "line should be updated to 2"
    );
    assert_eq!(
        updated_note.get("column").and_then(|v| v.as_i64()).unwrap(),
        5,
        "column should be updated to 5"
    );
    let node_path = updated_note
        .get("nodePath")
        .and_then(|v| v.as_array())
        .cloned()
        .unwrap_or_default();
    assert_eq!(node_path.len(), 2, "nodePath should have 2 elements");
    assert_eq!(
        updated_note.get("stale").and_then(|v| v.as_bool()).unwrap(),
        false,
        "reattached note should not be stale"
    );
    // Verify commit/blob SHAs are set (from git info)
    assert!(
        updated_note.get("commitSha").and_then(|v| v.as_str()).is_some(),
        "reattached note should have commitSha"
    );
    assert!(
        updated_note.get("blobSha").and_then(|v| v.as_str()).is_some(),
        "reattached note should have blobSha"
    );

    Ok(())
}

/// Verify reattach returns error for non-existent note.
#[test]
fn reattach_fails_for_missing_note() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "notes/reattach",
        "params": {
            "id": "00000000-0000-0000-0000-000000000000",
            "line": 1,
            "column": 0
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
    let (body, _) = decode_framed(&stdout).expect("reattach response");
    let resp: Response = serde_json::from_slice(&body)?;
    assert!(
        resp.error.is_some(),
        "reattach should fail for non-existent note"
    );
    assert!(
        resp.error
            .as_ref()
            .unwrap()
            .message
            .contains("not found"),
        "error should mention note not found"
    );

    Ok(())
}

/// Verify pagination works for notes/list-project.
#[test]
fn pagination_works_for_list_project() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;

    // Create 5 notes
    for i in 1..=5 {
        let req = serde_json::json!({
            "jsonrpc": "2.0",
            "id": i,
            "method": "notes/create",
            "params": {
                "file": "/tmp/test.rs",
                "projectRoot": "/tmp",
                "line": i,
                "column": 0,
                "text": format!("Note {}", i),
                "tags": []
            }
        })
        .to_string();
        let input = format!("Content-Length: {}\r\n\r\n{}", req.len(), req);
        cargo_bin_cmd!("hemis")
            .env("HEMIS_DB_PATH", db.path())
            .write_stdin(input)
            .assert()
            .success();
    }

    // List with limit=2, offset=0
    let req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 10,
        "method": "notes/list-project",
        "params": {
            "projectRoot": "/tmp",
            "limit": 2,
            "offset": 0
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
    let (body, _) = decode_framed(&stdout).unwrap();
    let resp: Response = serde_json::from_slice(&body)?;
    let notes = resp
        .result
        .as_ref()
        .and_then(|v| v.as_array())
        .cloned()
        .unwrap_or_default();
    assert_eq!(notes.len(), 2, "should return 2 notes with limit=2");

    // List with limit=2, offset=2
    let req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 11,
        "method": "notes/list-project",
        "params": {
            "projectRoot": "/tmp",
            "limit": 2,
            "offset": 2
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
    let (body, _) = decode_framed(&stdout).unwrap();
    let resp: Response = serde_json::from_slice(&body)?;
    let notes = resp
        .result
        .as_ref()
        .and_then(|v| v.as_array())
        .cloned()
        .unwrap_or_default();
    assert_eq!(notes.len(), 2, "should return 2 notes with offset=2");

    // List with offset=4 (only 1 remaining)
    let req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 12,
        "method": "notes/list-project",
        "params": {
            "projectRoot": "/tmp",
            "limit": 10,
            "offset": 4
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
    let (body, _) = decode_framed(&stdout).unwrap();
    let resp: Response = serde_json::from_slice(&body)?;
    let notes = resp
        .result
        .as_ref()
        .and_then(|v| v.as_array())
        .cloned()
        .unwrap_or_default();
    assert_eq!(notes.len(), 1, "should return 1 note with offset=4");

    Ok(())
}
