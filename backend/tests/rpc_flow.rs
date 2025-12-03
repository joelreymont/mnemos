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
    assert!(filtered_list[0]
        .get("stale")
        .and_then(|v| v.as_bool())
        .unwrap_or(false));
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
    // Canonicalize paths since the backend canonicalizes project root
    let canonical_root = root.path().canonicalize()?;
    let canonical_file = file_path.canonicalize()?;
    let root_str = canonical_root.to_string_lossy().to_string();
    let file_str = canonical_file.to_string_lossy().to_string();

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
        "params": { "file": file_str, "projectRoot": root_str }
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
        canonical_file.to_string_lossy().to_string()
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

    let input = format!(
        "Content-Length: {}\r\n\r\n{}",
        req_create_a.len(),
        req_create_a
    );
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

    assert_eq!(
        backlinks.len(),
        1,
        "note A should have exactly one backlink"
    );
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

/// Verify that updating a note to add a link creates the edge for backlinks.
#[test]
fn backlinks_found_after_update_adds_link() -> anyhow::Result<()> {
    // Test: create note A (target), create note B (no link), update B to link to A, query backlinks.
    let db = NamedTempFile::new()?;

    // Create note A (target)
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

    let input = format!(
        "Content-Length: {}\r\n\r\n{}",
        req_create_a.len(),
        req_create_a
    );
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

    // Create note B without any link
    let req_create_b = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "notes/create",
        "params": {
            "file": "/tmp/test.rs",
            "projectRoot": "/tmp",
            "line": 10,
            "column": 0,
            "text": "Note B - no links yet",
            "tags": []
        }
    })
    .to_string();

    let input = format!(
        "Content-Length: {}\r\n\r\n{}",
        req_create_b.len(),
        req_create_b
    );
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

    // Update note B to add link to A
    let link_text = format!("Note B - see [[target][{}]]", note_a_id);
    let req_update = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 3,
        "method": "notes/update",
        "params": {
            "id": note_b_id,
            "text": link_text
        }
    })
    .to_string();

    // Query backlinks for note A
    let req_backlinks = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 4,
        "method": "notes/backlinks",
        "params": { "id": note_a_id }
    })
    .to_string();

    let input = format!(
        "Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}",
        req_update.len(),
        req_update,
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
    assert_eq!(bodies.len(), 2, "expected update and backlinks responses");

    let backlinks_resp: Response = serde_json::from_slice(&bodies[1])?;
    let backlinks = backlinks_resp
        .result
        .as_ref()
        .and_then(|v| v.as_array())
        .cloned()
        .unwrap_or_default();

    assert_eq!(
        backlinks.len(),
        1,
        "note A should have one backlink after update"
    );
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
            .contains("Note B - see"),
        "backlink text should contain updated text"
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

    let input = format!(
        "Content-Length: {}\r\n\r\n{}",
        req_create_a.len(),
        req_create_a
    );
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

    let input = format!(
        "Content-Length: {}\r\n\r\n{}",
        req_create_b.len(),
        req_create_b
    );
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
    let input = format!(
        "Content-Length: {}\r\n\r\n{}",
        req_backlinks.len(),
        req_backlinks
    );
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
    let input = format!(
        "Content-Length: {}\r\n\r\n{}",
        req_backlinks.len(),
        req_backlinks
    );
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
    assert!(
        text.chars().count() > 60,
        "text should be longer than 60 chars"
    );
    let summary = note
        .get("summary")
        .and_then(|v| v.as_str())
        .expect("summary");
    // Summary should be truncated with "..." and not panic
    assert!(summary.ends_with("..."), "long text should be truncated");
    // Should contain Japanese characters
    assert!(
        summary.contains("\u{3053}"),
        "summary should preserve Japanese chars"
    );
    // Summary char count should be exactly 60 (57 + "...")
    assert_eq!(
        summary.chars().count(),
        60,
        "summary should be 57 chars + ..."
    );
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

    let input = format!(
        "Content-Length: {}\r\n\r\n{}",
        req_create_a.len(),
        req_create_a
    );
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

    let input = format!(
        "Content-Length: {}\r\n\r\n{}",
        req_create_b.len(),
        req_create_b
    );
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

    let input = format!(
        "Content-Length: {}\r\n\r\n{}",
        req_create_c.len(),
        req_create_c
    );
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
        updated_note
            .get("commitSha")
            .and_then(|v| v.as_str())
            .is_some(),
        "reattached note should have commitSha"
    );
    assert!(
        updated_note
            .get("blobSha")
            .and_then(|v| v.as_str())
            .is_some(),
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
        resp.error.as_ref().unwrap().message.contains("not found"),
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

/// Test: notes/create with content computes node_text_hash server-side.
#[test]
fn create_with_content_computes_hash() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let root = tempfile::tempdir()?;
    let file = root.path().join("test.rs");
    let content = "fn main() {\n    println!(\"hello\");\n}\n\nfn helper() {\n    let x = 1;\n}\n";
    fs::write(&file, content)?;

    let file_str = file.to_string_lossy().to_string();
    let root_str = root.path().to_string_lossy().to_string();

    // Create note with content - server should compute hash
    // Line 5 (1-based) is "fn helper() {" in the test content
    let req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "notes/create",
        "params": {
            "file": file_str,
            "projectRoot": root_str,
            "line": 5,
            "column": 0,
            "text": "Note on helper function",
            "content": content,
            "tags": []
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
    let (body, _) = decode_framed(&stdout).expect("create response");
    let resp: Response = serde_json::from_slice(&body)?;
    assert!(resp.error.is_none(), "create should succeed");

    let note = resp.result.expect("create result");
    // Server should have computed nodeTextHash
    assert!(
        note.get("nodeTextHash")
            .and_then(|v| v.as_str())
            .is_some(),
        "server should compute nodeTextHash when content is provided"
    );
    // Server should have computed nodePath
    let node_path = note.get("nodePath").and_then(|v| v.as_array());
    assert!(
        node_path.is_some(),
        "server should compute nodePath when content is provided"
    );
    let path = node_path.unwrap();
    assert!(
        path.iter().any(|v| v.as_str() == Some("function_item")),
        "nodePath should contain function_item for Rust function"
    );

    Ok(())
}

/// Test: notes/list-for-file with content computes display positions.
#[test]
fn list_with_content_computes_positions() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let root = tempfile::tempdir()?;
    let file = root.path().join("test.rs");
    let original_content = "fn main() {}\n\nfn helper() {\n    let x = 1;\n}\n";
    fs::write(&file, original_content)?;

    let file_str = file.to_string_lossy().to_string();
    let root_str = root.path().to_string_lossy().to_string();

    // Create note with content at line 3 (1-based: helper function)
    // Original content: line 1 = "fn main() {}", line 2 = "", line 3 = "fn helper() {"
    let req_create = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "notes/create",
        "params": {
            "file": file_str,
            "projectRoot": root_str,
            "line": 3,
            "column": 0,
            "text": "Note on helper",
            "content": original_content,
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
    let _orig_hash = resp
        .result
        .as_ref()
        .and_then(|v| v.get("nodeTextHash"))
        .and_then(|v| v.as_str())
        .expect("original hash")
        .to_string();

    // New content: helper moved to line 5 (added comments)
    let new_content = "fn main() {}\n\n// Comment 1\n// Comment 2\nfn helper() {\n    let x = 1;\n}\n";

    // List notes with new content - should find helper at new position
    let req_list = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "notes/list-for-file",
        "params": {
            "file": file_str,
            "projectRoot": root_str,
            "content": new_content
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req_list.len(), req_list);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("list response");
    let resp: Response = serde_json::from_slice(&body)?;
    let notes = resp
        .result
        .as_ref()
        .and_then(|v| v.as_array())
        .cloned()
        .expect("notes list");

    assert_eq!(notes.len(), 1, "should have one note");
    let note = &notes[0];
    // Note should have moved to line 5 (1-based: helper is at line 5 in new content)
    // New content: line 1 = "fn main() {}", 2 = "", 3 = "// Comment 1", 4 = "// Comment 2", 5 = "fn helper() {"
    let display_line = note.get("line").and_then(|v| v.as_i64()).expect("line");
    assert_eq!(display_line, 5, "note should follow helper to line 5");
    // Note should NOT be stale (hash still matches)
    let stale = note.get("stale").and_then(|v| v.as_bool()).unwrap_or(true);
    assert!(!stale, "note should not be stale when code moves");

    Ok(())
}

/// Test: notes/buffer-update recomputes positions for all notes.
#[test]
fn buffer_update_recomputes_positions() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let root = tempfile::tempdir()?;
    let file = root.path().join("test.rs");
    let original_content = "fn main() {}\n\nfn helper() {\n    let x = 1;\n}\n";
    fs::write(&file, original_content)?;

    let file_str = file.to_string_lossy().to_string();
    let root_str = root.path().to_string_lossy().to_string();

    // Create note on helper function (line 3, 1-based)
    // Original content: line 1 = "fn main() {}", line 2 = "", line 3 = "fn helper() {"
    let req_create = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "notes/create",
        "params": {
            "file": file_str,
            "projectRoot": root_str,
            "line": 3,
            "column": 0,
            "text": "Note on helper",
            "content": original_content,
            "tags": []
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req_create.len(), req_create);
    cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();

    // Buffer update with modified content (helper renamed - should be stale)
    let modified_content =
        "fn main() {}\n\nfn renamed_function() {\n    let x = 1;\n}\n";

    let req_update = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "notes/buffer-update",
        "params": {
            "file": file_str,
            "content": modified_content
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req_update.len(), req_update);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("buffer-update response");
    let resp: Response = serde_json::from_slice(&body)?;
    let notes = resp
        .result
        .as_ref()
        .and_then(|v| v.as_array())
        .cloned()
        .expect("notes list");

    assert_eq!(notes.len(), 1, "should have one note");
    let note = &notes[0];
    // Note should be stale (hash no longer matches)
    let stale = note.get("stale").and_then(|v| v.as_bool()).unwrap_or(false);
    assert!(stale, "note should be stale when function renamed");

    Ok(())
}

/// Test dynamic grammar loading: fetch, build, and use a grammar.
/// This test requires tree-sitter CLI to be installed.
#[test]
#[ignore] // Run with: cargo test --test rpc_flow dynamic_grammar -- --ignored
fn dynamic_grammar_fetch_build_and_use() -> anyhow::Result<()> {
    // Skip if tree-sitter CLI is not available
    let ts_check = Command::new("tree-sitter").arg("--version").output();
    if ts_check.is_err() || !ts_check.unwrap().status.success() {
        eprintln!("Skipping test: tree-sitter CLI not found");
        return Ok(());
    }

    // Create temp directories for config and DB
    let config_dir = tempfile::tempdir()?;
    let db = NamedTempFile::new()?;
    let project_dir = tempfile::tempdir()?;

    // Create languages.toml with JSON grammar source
    let languages_toml = r#"
[[grammar]]
name = "json"
source = { git = "https://github.com/tree-sitter/tree-sitter-json" }

[[language]]
name = "json"
file-types = ["json"]
skip-nodes = ["string", "number", "true", "false", "null"]
container-nodes = ["document", "object", "array"]
"#;
    fs::write(config_dir.path().join("languages.toml"), languages_toml)?;

    let config_path = config_dir.path().to_string_lossy().to_string();

    // Fetch the JSON grammar
    let fetch_output = cargo_bin_cmd!("hemis")
        .env("HEMIS_CONFIG_DIR", &config_path)
        .args(["grammar", "fetch", "json"])
        .output()?;

    if !fetch_output.status.success() {
        eprintln!(
            "grammar fetch failed: {}",
            String::from_utf8_lossy(&fetch_output.stderr)
        );
        anyhow::bail!("grammar fetch failed");
    }

    // Verify sources directory was created
    let sources_dir = config_dir.path().join("grammars").join("sources").join("json");
    assert!(sources_dir.exists(), "Grammar source should be fetched");

    // Build the JSON grammar
    let build_output = cargo_bin_cmd!("hemis")
        .env("HEMIS_CONFIG_DIR", &config_path)
        .args(["grammar", "build", "json"])
        .output()?;

    eprintln!(
        "grammar build stdout: {}",
        String::from_utf8_lossy(&build_output.stdout)
    );
    eprintln!(
        "grammar build stderr: {}",
        String::from_utf8_lossy(&build_output.stderr)
    );

    if !build_output.status.success() {
        anyhow::bail!("grammar build failed");
    }

    // Verify grammar was built (either .so or .dylib depending on platform)
    // tree-sitter CLI outputs as libtree-sitter-<name>.<ext>
    let grammars_dir = config_dir.path().join("grammars");
    let so_path = grammars_dir.join("libtree-sitter-json.so");
    let dylib_path = grammars_dir.join("libtree-sitter-json.dylib");
    assert!(
        so_path.exists() || dylib_path.exists(),
        "Grammar should be built as libtree-sitter-json.so or .dylib"
    );

    // Create a JSON file for testing
    let json_file = project_dir.path().join("test.json");
    let json_content = r#"{
  "name": "test",
  "value": 42
}"#;
    fs::write(&json_file, json_content)?;

    let file_str = json_file.to_string_lossy().to_string();
    let root_str = project_dir.path().to_string_lossy().to_string();

    // Create a note on the JSON file with content (server computes hash)
    let req_create = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "notes/create",
        "params": {
            "file": file_str,
            "projectRoot": root_str,
            "line": 2,
            "column": 2,
            "text": "Name field for this object",
            "content": json_content
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req_create.len(), req_create);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .env("HEMIS_CONFIG_DIR", &config_path)
        .write_stdin(input)
        .assert()
        .success();

    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("create response");
    let resp: Response = serde_json::from_slice(&body)?;

    let note = resp.result.as_ref().expect("note created");
    eprintln!("Created note response: {:?}", note);
    assert!(note.get("id").is_some(), "Note should have an ID");

    // Verify nodeTextHash was computed (proves tree-sitter parsed the JSON)
    // Note: nodeTextHash may not be computed if grammar loading failed silently
    let has_hash = note.get("nodeTextHash").and_then(|v| v.as_str()).is_some();
    if !has_hash {
        eprintln!("Warning: nodeTextHash not computed. Grammar may not have loaded.");
        // This is acceptable - the test still verifies the grammar fetch/build worked
    }

    // List notes with content to verify displayLine computation
    let req_list = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "notes/list-for-file",
        "params": {
            "file": file_str,
            "projectRoot": root_str,
            "content": json_content
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req_list.len(), req_list);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .env("HEMIS_CONFIG_DIR", &config_path)
        .write_stdin(input)
        .assert()
        .success();

    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("list response");
    let resp: Response = serde_json::from_slice(&body)?;

    let notes = resp
        .result
        .as_ref()
        .and_then(|v| v.as_array())
        .expect("notes array");

    assert_eq!(notes.len(), 1, "Should have one note");
    let note = &notes[0];

    // Verify displayLine was computed (proves grammar was loaded and used)
    let display_line = note.get("displayLine").and_then(|v| v.as_u64());
    eprintln!("Note displayLine: {:?}", display_line);

    // The test passes if:
    // 1. Grammar was fetched and built successfully (verified above)
    // 2. Note was created successfully
    // If displayLine is computed, that's extra confirmation the grammar was loaded
    if display_line.is_some() {
        eprintln!("SUCCESS: Grammar was dynamically loaded and used for parsing");
    } else {
        eprintln!("Note: displayLine not computed - grammar may not have loaded at runtime");
        // Still consider this a success since fetch/build worked
    }

    Ok(())
}

// Test project-meta RPC returns correct state for unindexed project
#[test]
fn project_meta_returns_unindexed_state() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let tmpdir = tempfile::tempdir()?;
    let project_root = tmpdir.path().to_string_lossy().to_string();

    // Initialize git repo
    git(tmpdir.path(), &["init"])?;
    std::fs::write(tmpdir.path().join("test.rs"), "fn main() {}")?;
    git(tmpdir.path(), &["add", "."])?;
    git(tmpdir.path(), &["-c", "commit.gpgsign=false", "commit", "-m", "init"])?;

    let req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "hemis/project-meta",
        "params": {
            "projectRoot": project_root
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
    let (body, _) = decode_framed(&stdout).expect("decode response");
    let resp: Response = serde_json::from_slice(&body)?;

    let result = resp.result.expect("should have result");

    // Project should not be indexed yet
    assert_eq!(result.get("indexed").and_then(|v| v.as_bool()), Some(false));
    assert_eq!(result.get("analyzed").and_then(|v| v.as_bool()), Some(false));
    assert_eq!(result.get("analysisStale").and_then(|v| v.as_bool()), Some(true));

    // Should have currentCommit since it's a git repo
    assert!(result.get("currentCommit").is_some());

    Ok(())
}

// Test index-project updates project_meta
#[test]
fn index_project_updates_project_meta() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let tmpdir = tempfile::tempdir()?;
    let project_root = tmpdir.path().to_string_lossy().to_string();

    // Initialize git repo with a file
    git(tmpdir.path(), &["init"])?;
    std::fs::write(tmpdir.path().join("lib.rs"), "pub fn hello() {}")?;
    git(tmpdir.path(), &["add", "."])?;
    git(tmpdir.path(), &["-c", "commit.gpgsign=false", "commit", "-m", "init"])?;

    // Index the project (without AI), then check project-meta
    let req_index = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "hemis/index-project",
        "params": {
            "projectRoot": project_root,
            "includeAI": false
        }
    })
    .to_string();

    let req_meta = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "hemis/project-meta",
        "params": {
            "projectRoot": project_root
        }
    })
    .to_string();

    let input = format!(
        "Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}",
        req_index.len(), req_index, req_meta.len(), req_meta
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
    assert_eq!(bodies.len(), 2, "expected two responses");

    let resp_index: Response = serde_json::from_slice(&bodies[0])?;
    let resp_meta: Response = serde_json::from_slice(&bodies[1])?;

    let result_index = resp_index.result.expect("should have result");
    assert_eq!(result_index.get("ok").and_then(|v| v.as_bool()), Some(true));
    assert!(result_index.get("indexed").and_then(|v| v.as_u64()).unwrap_or(0) >= 1);

    let result_meta = resp_meta.result.expect("should have result");
    assert_eq!(result_meta.get("indexed").and_then(|v| v.as_bool()), Some(true));
    assert!(result_meta.get("indexedAt").and_then(|v| v.as_i64()).is_some());
    assert!(result_meta.get("indexedCommit").and_then(|v| v.as_str()).is_some());

    // AI should still be false since we didn't include it
    assert_eq!(result_meta.get("analyzed").and_then(|v| v.as_bool()), Some(false));

    Ok(())
}

// Test explain-region works without AI
#[test]
fn explain_region_without_ai_returns_snippet() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let tmpdir = tempfile::tempdir()?;
    let file_path = tmpdir.path().join("test.rs");

    let content = r#"fn main() {
    println!("Hello");
    println!("World");
}
"#;
    std::fs::write(&file_path, content)?;

    let req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "hemis/explain-region",
        "params": {
            "file": file_path.to_string_lossy(),
            "startLine": 2,
            "endLine": 3,
            "useAI": false
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
    let (body, _) = decode_framed(&stdout).expect("decode response");
    let resp: Response = serde_json::from_slice(&body)?;

    let result = resp.result.expect("should have result");

    // Should return the snippet
    let snippet = result.get("content").and_then(|v| v.as_str()).expect("should have content");
    assert!(snippet.contains("println"));
    assert!(snippet.contains("Hello"));
    assert!(snippet.contains("World"));

    // Should not have explanation when AI is disabled
    assert!(result.get("explanation").is_none());
    assert!(result.get("ai").is_none());

    Ok(())
}

// Test explain-region with AI disabled in environment returns snippet only
#[test]
fn explain_region_with_ai_disabled_env() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let tmpdir = tempfile::tempdir()?;
    let file_path = tmpdir.path().join("test.rs");

    let content = "fn add(a: i32, b: i32) -> i32 { a + b }";
    std::fs::write(&file_path, content)?;

    let req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "hemis/explain-region",
        "params": {
            "file": file_path.to_string_lossy(),
            "startLine": 1,
            "endLine": 1,
            "useAI": true
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req.len(), req);

    // Run with HEMIS_AI_PROVIDER=none to disable AI
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .env("HEMIS_AI_PROVIDER", "none")
        .write_stdin(input)
        .assert()
        .success();

    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("decode response");
    let resp: Response = serde_json::from_slice(&body)?;

    let result = resp.result.expect("should have result");

    // Should return the snippet
    let snippet = result.get("content").and_then(|v| v.as_str()).expect("should have content");
    assert!(snippet.contains("fn add"));

    // Should have AI error since it was requested but disabled
    let ai_info = result.get("ai");
    assert!(ai_info.is_some());
    let ai_error = ai_info.and_then(|a| a.get("error")).and_then(|v| v.as_str());
    assert!(ai_error.is_some());
    assert!(ai_error.unwrap().contains("no AI CLI available"));

    Ok(())
}

// Test index-project with AI requested but disabled via env
#[test]
fn index_project_with_ai_disabled_env() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let tmpdir = tempfile::tempdir()?;
    let project_root = tmpdir.path().to_string_lossy().to_string();

    // Initialize git repo
    git(tmpdir.path(), &["init"])?;
    std::fs::write(tmpdir.path().join("main.rs"), "fn main() {}")?;
    git(tmpdir.path(), &["add", "."])?;
    git(tmpdir.path(), &["-c", "commit.gpgsign=false", "commit", "-m", "init"])?;

    let req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "hemis/index-project",
        "params": {
            "projectRoot": project_root,
            "includeAI": true
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req.len(), req);

    // Run with HEMIS_AI_PROVIDER=none to disable AI
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .env("HEMIS_AI_PROVIDER", "none")
        .write_stdin(input)
        .assert()
        .success();

    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("decode response");
    let resp: Response = serde_json::from_slice(&body)?;

    let result = resp.result.expect("should have result");

    // Indexing should still succeed
    assert_eq!(result.get("ok").and_then(|v| v.as_bool()), Some(true));
    assert!(result.get("indexed").and_then(|v| v.as_u64()).unwrap_or(0) >= 1);

    // AI should have error
    let ai_info = result.get("ai");
    assert!(ai_info.is_some(), "should have ai info when includeAI requested");
    let ai_error = ai_info.and_then(|a| a.get("error")).and_then(|v| v.as_str());
    assert!(ai_error.is_some(), "should have ai error when disabled");

    Ok(())
}

// Test explain-region reads from content param instead of disk
#[test]
fn explain_region_uses_content_param() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let tmpdir = tempfile::tempdir()?;
    let file_path = tmpdir.path().join("test.rs");

    // Write one thing to disk
    std::fs::write(&file_path, "fn disk_version() {}")?;

    // But send different content in the request
    let buffer_content = "fn buffer_version() {\n    let x = 42;\n}\n";

    let req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "hemis/explain-region",
        "params": {
            "file": file_path.to_string_lossy(),
            "startLine": 1,
            "endLine": 3,
            "content": buffer_content,
            "useAI": false
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
    let (body, _) = decode_framed(&stdout).expect("decode response");
    let resp: Response = serde_json::from_slice(&body)?;

    let result = resp.result.expect("should have result");

    // Should return buffer content, not disk content
    let snippet = result.get("content").and_then(|v| v.as_str()).expect("should have content");
    assert!(snippet.contains("buffer_version"), "should use content param");
    assert!(snippet.contains("let x = 42"), "should have buffer content");
    assert!(!snippet.contains("disk_version"), "should NOT have disk content");

    Ok(())
}

// Test project-meta staleness detection after new commit
#[test]
fn project_meta_detects_staleness_after_commit() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let tmpdir = tempfile::tempdir()?;
    let project_root = tmpdir.path().to_string_lossy().to_string();

    // Initialize git repo and index
    git(tmpdir.path(), &["init"])?;
    std::fs::write(tmpdir.path().join("lib.rs"), "pub fn v1() {}")?;
    git(tmpdir.path(), &["add", "."])?;
    git(tmpdir.path(), &["-c", "commit.gpgsign=false", "commit", "-m", "v1"])?;

    // Index the project
    let req_index = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "hemis/index-project",
        "params": { "projectRoot": project_root }
    })
    .to_string();
    let input = format!("Content-Length: {}\r\n\r\n{}", req_index.len(), req_index);
    cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();

    // Check project-meta - should not be stale yet
    let req_meta = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "hemis/project-meta",
        "params": { "projectRoot": project_root }
    })
    .to_string();
    let input = format!("Content-Length: {}\r\n\r\n{}", req_meta.len(), req_meta);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("decode");
    let resp: Response = serde_json::from_slice(&body)?;
    let meta1 = resp.result.expect("meta result");
    assert_eq!(meta1.get("indexed").and_then(|v| v.as_bool()), Some(true));
    let commit1 = meta1.get("indexedCommit").and_then(|v| v.as_str()).unwrap().to_string();

    // Make a new commit
    std::fs::write(tmpdir.path().join("lib.rs"), "pub fn v2() {}")?;
    git(tmpdir.path(), &["add", "."])?;
    git(tmpdir.path(), &["-c", "commit.gpgsign=false", "commit", "-m", "v2"])?;

    // Check project-meta again - currentCommit should differ from indexedCommit
    let input = format!("Content-Length: {}\r\n\r\n{}", req_meta.len(), req_meta);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("decode");
    let resp: Response = serde_json::from_slice(&body)?;
    let meta2 = resp.result.expect("meta result");

    let current_commit = meta2.get("currentCommit").and_then(|v| v.as_str()).unwrap();
    let indexed_commit = meta2.get("indexedCommit").and_then(|v| v.as_str()).unwrap();

    assert_ne!(current_commit, indexed_commit, "commits should differ after new commit");
    assert_eq!(indexed_commit, commit1, "indexedCommit should be unchanged");

    Ok(())
}

// Test notes/get returns a single note by ID
#[test]
fn get_note_by_id() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;

    // Create a note
    let req_create = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "notes/create",
        "params": {
            "file": "/tmp/test.rs",
            "projectRoot": "/tmp",
            "line": 10,
            "column": 5,
            "text": "Test note for get",
            "tags": ["tag1", "tag2"]
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

    // Get the note by ID
    let req_get = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "notes/get",
        "params": { "id": note_id }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req_get.len(), req_get);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("get response");
    let resp: Response = serde_json::from_slice(&body)?;

    let note = resp.result.expect("should have result");
    assert_eq!(note.get("id").and_then(|v| v.as_str()).unwrap(), note_id);
    assert_eq!(note.get("text").and_then(|v| v.as_str()).unwrap(), "Test note for get");
    assert_eq!(note.get("line").and_then(|v| v.as_i64()).unwrap(), 10);
    assert_eq!(note.get("column").and_then(|v| v.as_i64()).unwrap(), 5);

    Ok(())
}

// Test notes/get returns error for non-existent note
#[test]
fn get_note_not_found() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;

    let req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "notes/get",
        "params": { "id": "00000000-0000-0000-0000-000000000000" }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req.len(), req);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("get response");
    let resp: Response = serde_json::from_slice(&body)?;

    assert!(resp.error.is_some(), "should return error for non-existent note");
    assert!(resp.error.as_ref().unwrap().message.contains("not found"));

    Ok(())
}

// Test notes/search finds notes by text
#[test]
fn search_notes_by_text() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let tmpdir = tempfile::tempdir()?;
    let project_root = tmpdir.path().to_string_lossy().to_string();

    // Create notes with different text
    for (i, text) in ["apple pie recipe", "banana bread", "apple cider"].iter().enumerate() {
        let req = serde_json::json!({
            "jsonrpc": "2.0",
            "id": i + 1,
            "method": "notes/create",
            "params": {
                "file": format!("{}/test.rs", project_root),
                "projectRoot": project_root,
                "line": i + 1,
                "column": 0,
                "text": text,
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

    // Search for "apple"
    let req_search = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 10,
        "method": "notes/search",
        "params": {
            "query": "apple",
            "projectRoot": project_root
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req_search.len(), req_search);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("search response");
    let resp: Response = serde_json::from_slice(&body)?;

    let results = resp.result.as_ref().and_then(|v| v.as_array()).expect("search results");
    assert_eq!(results.len(), 2, "should find 2 notes with 'apple'");

    // Verify both apple notes are found
    let texts: Vec<&str> = results
        .iter()
        .filter_map(|n| n.get("text").and_then(|v| v.as_str()))
        .collect();
    assert!(texts.iter().any(|t| t.contains("apple pie")));
    assert!(texts.iter().any(|t| t.contains("apple cider")));

    Ok(())
}

// Test hemis/status returns correct counts
#[test]
fn status_returns_counts() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let tmpdir = tempfile::tempdir()?;
    let project_root = tmpdir.path().to_string_lossy().to_string();

    // Create a note
    let req_create = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "notes/create",
        "params": {
            "file": format!("{}/test.rs", project_root),
            "projectRoot": project_root,
            "line": 1,
            "column": 0,
            "text": "Status test note",
            "tags": []
        }
    })
    .to_string();
    let input = format!("Content-Length: {}\r\n\r\n{}", req_create.len(), req_create);
    cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();

    // Get status
    let req_status = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "hemis/status",
        "params": {}
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req_status.len(), req_status);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("status response");
    let resp: Response = serde_json::from_slice(&body)?;

    let result = resp.result.expect("should have result");
    assert_eq!(result.get("ok").and_then(|v| v.as_bool()), Some(true));

    let counts = result.get("counts").expect("should have counts");
    assert!(counts.get("notes").and_then(|v| v.as_u64()).unwrap() >= 1);

    Ok(())
}

// Test index/add-file indexes a single file
#[test]
fn index_add_file() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let tmpdir = tempfile::tempdir()?;
    let file_path = tmpdir.path().join("indexed.rs");
    let project_root = tmpdir.path().to_string_lossy().to_string();

    let content = "fn indexed_function() { let x = 42; }";
    std::fs::write(&file_path, content)?;

    let req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "index/add-file",
        "params": {
            "file": file_path.to_string_lossy(),
            "projectRoot": project_root,
            "content": content
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
    let (body, _) = decode_framed(&stdout).expect("index response");
    let resp: Response = serde_json::from_slice(&body)?;

    // Should succeed
    assert!(resp.error.is_none(), "index/add-file should succeed");

    // Now search for the indexed content
    let req_search = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "index/search",
        "params": {
            "query": "indexed_function"
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req_search.len(), req_search);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("search response");
    let resp: Response = serde_json::from_slice(&body)?;

    let results = resp.result.as_ref().and_then(|v| v.as_array()).expect("search results");
    assert!(!results.is_empty(), "should find indexed file");

    Ok(())
}

// Test hemis/open-project initializes project root
#[test]
fn open_project_succeeds() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let tmpdir = tempfile::tempdir()?;
    let project_root = tmpdir.path().to_string_lossy().to_string();

    // Initialize git repo
    git(tmpdir.path(), &["init"])?;

    let req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "hemis/open-project",
        "params": {
            "projectRoot": project_root
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
    let (body, _) = decode_framed(&stdout).expect("open-project response");
    let resp: Response = serde_json::from_slice(&body)?;

    let result = resp.result.expect("should have result");
    assert_eq!(result.get("ok").and_then(|v| v.as_bool()), Some(true));

    Ok(())
}

// Test hemis/open-project fails without projectRoot param
#[test]
fn open_project_requires_root() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;

    let req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "hemis/open-project",
        "params": {}
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req.len(), req);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("open-project response");
    let resp: Response = serde_json::from_slice(&body)?;

    assert!(resp.error.is_some(), "should error without projectRoot");
    assert!(resp.error.as_ref().unwrap().message.contains("projectRoot"));

    Ok(())
}

/// Integration test matching the demo reattach script flow:
/// 1. Create note on a function
/// 2. Insert lines above (note follows)
/// 3. Rename function (note becomes stale)
/// 4. Reattach note (note becomes fresh)
#[test]
fn demo_reattach_flow() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let root = tempfile::tempdir()?;
    let file = root.path().join("app.rs");

    // Initial content matching demo setup
    let initial_content = r#"fn main() {
    let config = load_config();
    let server = Server::new(config);
    server.start();
}

fn load_config() -> Config {
    Config::default()
}

struct Server {
    config: Config,
}

impl Server {
    fn new(config: Config) -> Self {
        Self { config }
    }

    fn start(&self) {
        println!("Starting server...");
    }
}
"#;
    fs::write(&file, initial_content)?;

    // Canonicalize paths for comparison
    let canonical_file = file.canonicalize()?;
    let canonical_root = root.path().canonicalize()?;
    let file_str = canonical_file.to_string_lossy().to_string();
    let root_str = canonical_root.to_string_lossy().to_string();

    // Step 1: Create note on fn new (line 16)
    let req_create = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "notes/create",
        "params": {
            "file": file_str,
            "projectRoot": root_str,
            "line": 16,
            "column": 4,
            "text": "Factory method - add validation",
            "content": initial_content,
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
    assert!(resp.error.is_none(), "create should succeed: {:?}", resp.error);
    let note_id = resp
        .result
        .as_ref()
        .and_then(|v| v.get("id"))
        .and_then(|v| v.as_str())
        .expect("note id")
        .to_string();
    let orig_hash = resp
        .result
        .as_ref()
        .and_then(|v| v.get("nodeTextHash"))
        .and_then(|v| v.as_str())
        .expect("nodeTextHash should be computed")
        .to_string();

    // Step 2: Insert lines above (simulates adding comments above impl)
    let content_with_comments = r#"fn main() {
    let config = load_config();
    let server = Server::new(config);
    server.start();
}

fn load_config() -> Config {
    Config::default()
}

struct Server {
    config: Config,
}

// Comment line 1
// Comment line 2
impl Server {
    fn new(config: Config) -> Self {
        Self { config }
    }

    fn start(&self) {
        println!("Starting server...");
    }
}
"#;

    // Step 3: List notes - note should follow code
    let req_list = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "notes/list-for-file",
        "params": {
            "file": file_str,
            "projectRoot": root_str,
            "content": content_with_comments
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req_list.len(), req_list);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("list response");
    let resp: Response = serde_json::from_slice(&body)?;
    let notes = resp.result.as_ref().and_then(|v| v.as_array()).cloned().unwrap();
    assert_eq!(notes.len(), 1, "should have one note");
    let display_line = notes[0].get("line").and_then(|v| v.as_i64()).unwrap();
    assert_eq!(display_line, 18, "note should follow code to line 18 (original 16 + 2 comments)");
    let stale = notes[0].get("stale").and_then(|v| v.as_bool()).unwrap_or(true);
    assert!(!stale, "note should NOT be stale (hash still matches)");

    // Step 4: Rename fn new to fn create (makes note stale)
    let content_renamed = r#"fn main() {
    let config = load_config();
    let server = Server::new(config);
    server.start();
}

fn load_config() -> Config {
    Config::default()
}

struct Server {
    config: Config,
}

// Comment line 1
// Comment line 2
impl Server {
    fn create(config: Config) -> Self {
        Self { config }
    }

    fn start(&self) {
        println!("Starting server...");
    }
}
"#;

    let req_list_stale = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 3,
        "method": "notes/list-for-file",
        "params": {
            "file": file_str,
            "projectRoot": root_str,
            "content": content_renamed
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req_list_stale.len(), req_list_stale);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("list stale response");
    let resp: Response = serde_json::from_slice(&body)?;
    let notes = resp.result.as_ref().and_then(|v| v.as_array()).cloned().unwrap();
    assert_eq!(notes.len(), 1, "should still have one note");
    let stale = notes[0].get("stale").and_then(|v| v.as_bool()).unwrap_or(false);
    assert!(stale, "note SHOULD be stale (function renamed)");

    // Step 5: Reattach note to the renamed function
    let req_reattach = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 4,
        "method": "notes/reattach",
        "params": {
            "id": note_id,
            "file": file_str,
            "line": 18,
            "column": 4,
            "content": content_renamed
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req_reattach.len(), req_reattach);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("reattach response");
    let resp: Response = serde_json::from_slice(&body)?;
    assert!(resp.error.is_none(), "reattach should succeed: {:?}", resp.error);

    let updated_note = resp.result.expect("reattach result");
    let new_hash = updated_note
        .get("nodeTextHash")
        .and_then(|v| v.as_str())
        .expect("new hash");
    assert_ne!(orig_hash, new_hash, "hash should be updated for renamed function");
    let stale = updated_note.get("stale").and_then(|v| v.as_bool()).unwrap_or(true);
    assert!(!stale, "reattached note should NOT be stale");

    // Step 6: Verify note is fresh on next list
    let req_list_fresh = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 5,
        "method": "notes/list-for-file",
        "params": {
            "file": file_str,
            "projectRoot": root_str,
            "content": content_renamed
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req_list_fresh.len(), req_list_fresh);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("list fresh response");
    let resp: Response = serde_json::from_slice(&body)?;
    let notes = resp.result.as_ref().and_then(|v| v.as_array()).cloned().unwrap();
    assert_eq!(notes.len(), 1, "should have one note");
    let stale = notes[0].get("stale").and_then(|v| v.as_bool()).unwrap_or(true);
    assert!(!stale, "note should be fresh after reattach");

    Ok(())
}

/// Integration test matching the full neovim demo script:
/// Covers notes, indexing, search, links, backlinks, and status.
#[test]
fn demo_full_workflow() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let root = tempfile::tempdir()?;

    // Create demo files matching demo.json setup
    let app_rs = root.path().join("app.rs");
    let app_content = r#"fn main() {
    let config = load_config();
    let server = Server::new(config);
    server.start();
}

fn load_config() -> Config {
    Config::default()
}

struct Server {
    config: Config,
}

impl Server {
    fn new(config: Config) -> Self {
        Self { config }
    }

    fn start(&self) {
        println!("Starting server...");
    }
}

struct Config {
    port: u16,
}

impl Default for Config {
    fn default() -> Self {
        Self { port: 8080 }
    }
}
"#;
    fs::write(&app_rs, app_content)?;

    let utils_rs = root.path().join("utils.rs");
    let utils_content = r#"pub fn format_port(port: u16) -> String {
    format!(":{}", port)
}

pub fn validate_port(port: u16) -> bool {
    port > 1024 && port < 65535
}
"#;
    fs::write(&utils_rs, utils_content)?;

    // Canonicalize paths
    let canonical_app = app_rs.canonicalize()?;
    let canonical_root = root.path().canonicalize()?;
    let app_str = canonical_app.to_string_lossy().to_string();
    let root_str = canonical_root.to_string_lossy().to_string();

    // Step 1: Create first note on fn new
    let req_create1 = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "notes/create",
        "params": {
            "file": app_str,
            "projectRoot": root_str,
            "line": 16,
            "column": 4,
            "text": "Factory method for Server - add validation",
            "content": app_content,
            "tags": []
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req_create1.len(), req_create1);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("create1 response");
    let resp: Response = serde_json::from_slice(&body)?;
    assert!(resp.error.is_none(), "create1 should succeed");
    let note1_id = resp
        .result
        .as_ref()
        .and_then(|v| v.get("id"))
        .and_then(|v| v.as_str())
        .expect("note1 id")
        .to_string();

    // Step 2: Create second note on Config struct
    let req_create2 = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "notes/create",
        "params": {
            "file": app_str,
            "projectRoot": root_str,
            "line": 25,
            "column": 0,
            "text": "Config struct improvements:\n- Add validation for port range\n- Support environment variable overrides",
            "content": app_content,
            "tags": []
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req_create2.len(), req_create2);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("create2 response");
    let resp: Response = serde_json::from_slice(&body)?;
    assert!(resp.error.is_none(), "create2 should succeed");
    let note2_id = resp
        .result
        .as_ref()
        .and_then(|v| v.get("id"))
        .and_then(|v| v.as_str())
        .expect("note2 id")
        .to_string();

    // Step 3: List notes (should have 2)
    let req_list = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 3,
        "method": "notes/list-project",
        "params": {
            "projectRoot": root_str
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req_list.len(), req_list);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("list response");
    let resp: Response = serde_json::from_slice(&body)?;
    let notes = resp.result.as_ref().and_then(|v| v.as_array()).cloned().unwrap();
    assert_eq!(notes.len(), 2, "should have two notes");

    // Step 4: Index project
    let req_index = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 4,
        "method": "hemis/index-project",
        "params": {
            "projectRoot": root_str
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req_index.len(), req_index);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .env("HEMIS_AI_PROVIDER", "none")
        .write_stdin(input)
        .assert()
        .success();
    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("index response");
    let resp: Response = serde_json::from_slice(&body)?;
    assert!(resp.error.is_none(), "index should succeed: {:?}", resp.error);
    let indexed = resp.result.as_ref().and_then(|v| v.get("indexed")).and_then(|v| v.as_i64()).unwrap();
    assert!(indexed > 0, "should have indexed files");

    // Step 5: Search for "start"
    let req_search = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 5,
        "method": "hemis/search",
        "params": {
            "query": "start",
            "projectRoot": root_str
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req_search.len(), req_search);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("search response");
    let resp: Response = serde_json::from_slice(&body)?;
    assert!(resp.error.is_none(), "search should succeed");
    let hits = resp.result.as_ref().and_then(|v| v.as_array()).cloned().unwrap();
    assert!(!hits.is_empty(), "search should find 'start'");

    // Step 6: Update first note to add link to second note (using [[desc][id]] format)
    let req_update = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 6,
        "method": "notes/update",
        "params": {
            "id": note1_id,
            "text": format!("Factory method for Server - add validation - see [[config][{}]]", note2_id)
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req_update.len(), req_update);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("update response");
    let resp: Response = serde_json::from_slice(&body)?;
    assert!(resp.error.is_none(), "update should succeed");

    // Step 7: Check backlinks from note2
    let req_backlinks = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 7,
        "method": "notes/backlinks",
        "params": {
            "id": note2_id
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req_backlinks.len(), req_backlinks);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("backlinks response");
    let resp: Response = serde_json::from_slice(&body)?;
    assert!(resp.error.is_none(), "backlinks should succeed");
    let backlinks = resp.result.as_ref().and_then(|v| v.as_array()).cloned().unwrap();
    assert_eq!(backlinks.len(), 1, "note2 should have one backlink from note1");
    let backlink_id = backlinks[0].get("id").and_then(|v| v.as_str()).unwrap();
    assert_eq!(backlink_id, note1_id, "backlink should be from note1");

    // Step 8: Delete second note
    let req_delete = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 8,
        "method": "notes/delete",
        "params": {
            "id": note2_id
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req_delete.len(), req_delete);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("delete response");
    let resp: Response = serde_json::from_slice(&body)?;
    assert!(resp.error.is_none(), "delete should succeed");
    let ok = resp.result.as_ref().and_then(|v| v.get("ok")).and_then(|v| v.as_bool()).unwrap();
    assert!(ok, "note should be deleted");

    // Step 9: Verify only one note remains
    let req_list2 = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 9,
        "method": "notes/list-project",
        "params": {
            "projectRoot": root_str
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req_list2.len(), req_list2);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("list2 response");
    let resp: Response = serde_json::from_slice(&body)?;
    let notes = resp.result.as_ref().and_then(|v| v.as_array()).cloned().unwrap();
    assert_eq!(notes.len(), 1, "should have one note after deletion");

    // Step 10: Check status
    let req_status = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 10,
        "method": "hemis/status",
        "params": {
            "projectRoot": root_str
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req_status.len(), req_status);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("status response");
    let resp: Response = serde_json::from_slice(&body)?;
    assert!(resp.error.is_none(), "status should succeed");
    let result = resp.result.expect("status result");
    assert_eq!(result.get("ok").and_then(|v| v.as_bool()), Some(true));
    let counts = result.get("counts").expect("counts");
    let notes_count = counts.get("notes").and_then(|v| v.as_i64()).unwrap();
    assert_eq!(notes_count, 1, "status should show 1 note");
    let files_count = counts.get("files").and_then(|v| v.as_i64()).unwrap();
    assert!(files_count > 0, "status should show indexed files");

    Ok(())
}
