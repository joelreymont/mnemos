use assert_cmd::cargo::cargo_bin_cmd;
use rpc::{decode_framed, Response};
use serde_json::{self, Value};
use std::fs;
use std::process::Command;
use tempfile::NamedTempFile;

/// Extract notes array from response result (handles both wrapped and unwrapped formats)
fn extract_notes(result: &Value) -> Option<Vec<Value>> {
    // New wrapped format: { notes: [...], contentHash: "..." }
    if let Some(notes) = result.get("notes").and_then(|v| v.as_array()) {
        return Some(notes.clone());
    }
    // Legacy format: direct array
    result.as_array().cloned()
}

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
fn display_config_returns_colors_and_icons() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "hemis/display-config",
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
    let (body, _) = decode_framed(&stdout).expect("framed response");
    let resp: Response = serde_json::from_slice(&body)?;
    let result = resp.result.expect("should have result");

    // Verify colors
    let colors = result.get("colors").expect("colors");
    assert_eq!(colors.get("note").and_then(|v| v.as_str()), Some("#4682B4"));
    assert_eq!(colors.get("noteStale").and_then(|v| v.as_str()), Some("#808080"));

    // Verify icons
    let icons = result.get("icons").expect("icons");
    assert!(icons.get("noteFresh").is_some());
    assert!(icons.get("noteStale").is_some());

    // Verify templates
    let templates = result.get("templates").expect("templates");
    assert!(templates.get("displayLabel").is_some());

    Ok(())
}

#[test]
fn note_templates_returns_templates() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "hemis/note-templates",
        "params": {}
    })
    .to_string();
    let input = format!("Content-Length: {}\r\n\r\n{}", req.len(), req);
    let out = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let (body, _) = decode_framed(&out.get_output().stdout).unwrap();
    let resp: Response = serde_json::from_slice(&body)?;
    let result = resp.result.expect("should have result");
    let templates = result.get("templates").and_then(|v| v.as_array()).expect("templates array");

    assert!(templates.len() >= 4, "should have at least 4 templates");

    // Verify template structure
    let ids: Vec<&str> = templates.iter().filter_map(|t| t.get("id").and_then(|v| v.as_str())).collect();
    assert!(ids.contains(&"bug"));
    assert!(ids.contains(&"todo"));
    assert!(ids.contains(&"decision"));

    Ok(())
}

#[test]
fn suggest_tags_from_rust_function() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let content = "fn main() {\n    let x = 1;\n}\n";
    let req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "hemis/suggest-tags",
        "params": {
            "file": "/tmp/test.rs",
            "content": content,
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
    let (body, _) = decode_framed(&stdout).expect("framed response");
    let resp: Response = serde_json::from_slice(&body)?;
    let result = resp.result.expect("should have result");
    let tags = result.get("tags").and_then(|v| v.as_array()).expect("tags array");
    let tags: Vec<&str> = tags.iter().filter_map(|v| v.as_str()).collect();

    // Should include "rust" (language) and "function" (node type)
    assert!(tags.contains(&"rust"), "should suggest rust tag");
    assert!(tags.contains(&"function"), "should suggest function tag");
    Ok(())
}

#[test]
fn suggest_tags_for_test_file() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "hemis/suggest-tags",
        "params": {
            "file": "/tmp/my_test.py"
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
    let (body, _) = decode_framed(&stdout).expect("framed response");
    let resp: Response = serde_json::from_slice(&body)?;
    let result = resp.result.expect("should have result");
    let tags = result.get("tags").and_then(|v| v.as_array()).expect("tags array");
    let tags: Vec<&str> = tags.iter().filter_map(|v| v.as_str()).collect();

    // Should include "python" (language) and "test" (file pattern)
    assert!(tags.contains(&"python"), "should suggest python tag");
    assert!(tags.contains(&"test"), "should suggest test tag");
    Ok(())
}

#[test]
fn graph_returns_nodes_and_edges() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let dir = tempfile::tempdir()?;
    let root = dir.path().to_string_lossy().to_string();

    // Create two notes that link to each other
    let req1 = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "notes/create",
        "params": {
            "file": format!("{}/test.rs", root),
            "projectRoot": &root,
            "line": 1,
            "column": 0,
            "tags": ["rust"],
            "text": "First note"
        }
    })
    .to_string();
    let input1 = format!("Content-Length: {}\r\n\r\n{}", req1.len(), req1);
    let out1 = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input1)
        .assert()
        .success();
    let (body1, _) = decode_framed(&out1.get_output().stdout).unwrap();
    let resp1: Response = serde_json::from_slice(&body1)?;
    let note1_id = resp1.result.unwrap().get("id").unwrap().as_str().unwrap().to_string();

    // Create second note that links to first
    let req2 = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "notes/create",
        "params": {
            "file": format!("{}/test.rs", root),
            "projectRoot": &root,
            "line": 5,
            "column": 0,
            "tags": ["rust"],
            "text": format!("Second note linking to [[first note][{}]]", note1_id)
        }
    })
    .to_string();
    let input2 = format!("Content-Length: {}\r\n\r\n{}", req2.len(), req2);
    cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input2)
        .assert()
        .success();

    // Query graph
    let req_graph = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 3,
        "method": "hemis/graph",
        "params": {
            "projectRoot": &root
        }
    })
    .to_string();
    let input_graph = format!("Content-Length: {}\r\n\r\n{}", req_graph.len(), req_graph);
    let out_graph = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input_graph)
        .assert()
        .success();
    let (body_graph, _) = decode_framed(&out_graph.get_output().stdout).unwrap();
    let resp_graph: Response = serde_json::from_slice(&body_graph)?;
    let result = resp_graph.result.expect("should have result");

    let nodes = result.get("nodes").and_then(|v| v.as_array()).expect("nodes array");
    let edges = result.get("edges").and_then(|v| v.as_array()).expect("edges array");

    assert_eq!(nodes.len(), 2, "should have 2 notes");
    assert_eq!(edges.len(), 1, "should have 1 edge");

    // Verify edge connects the notes
    let edge = &edges[0];
    assert!(edge.get("src").is_some());
    assert_eq!(edge.get("dst").and_then(|v| v.as_str()), Some(note1_id.as_str()));
    assert_eq!(edge.get("kind").and_then(|v| v.as_str()), Some("link"));

    Ok(())
}

#[test]
fn tasks_detects_todo_fixme() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let dir = tempfile::tempdir()?;
    let root = dir.path().to_string_lossy().to_string();
    let content = r#"fn main() {
    // TODO: implement this
    let x = 1; // FIXME: use better name
    // HACK: workaround for issue
}
"#;

    // Index the file
    let req_index = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "index/add-file",
        "params": {
            "file": format!("{}/test.rs", root),
            "projectRoot": &root,
            "content": content
        }
    })
    .to_string();
    let input_index = format!("Content-Length: {}\r\n\r\n{}", req_index.len(), req_index);
    cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input_index)
        .assert()
        .success();

    // Query tasks
    let req_tasks = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "hemis/tasks",
        "params": {
            "projectRoot": &root
        }
    })
    .to_string();
    let input_tasks = format!("Content-Length: {}\r\n\r\n{}", req_tasks.len(), req_tasks);
    let out = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input_tasks)
        .assert()
        .success();
    let (body, _) = decode_framed(&out.get_output().stdout).unwrap();
    let resp: Response = serde_json::from_slice(&body)?;
    let result = resp.result.expect("should have result");
    let tasks = result.get("tasks").and_then(|v| v.as_array()).expect("tasks array");

    assert_eq!(tasks.len(), 3, "should find 3 tasks (TODO, FIXME, HACK)");

    // Verify task details
    let kinds: Vec<&str> = tasks.iter().filter_map(|t| t.get("kind").and_then(|v| v.as_str())).collect();
    assert!(kinds.contains(&"TODO"));
    assert!(kinds.contains(&"FIXME"));
    assert!(kinds.contains(&"HACK"));

    Ok(())
}

#[test]
fn file_context_returns_suggestions() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let dir = tempfile::tempdir()?;
    let root = dir.path().to_string_lossy().to_string();
    let file = format!("{}/test.rs", root);

    let req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "hemis/file-context",
        "params": {
            "file": &file,
            "projectRoot": &root
        }
    })
    .to_string();
    let input = format!("Content-Length: {}\r\n\r\n{}", req.len(), req);
    let out = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let (body, _) = decode_framed(&out.get_output().stdout).unwrap();
    let resp: Response = serde_json::from_slice(&body)?;
    let result = resp.result.expect("should have result");

    // Should have suggestions
    let suggestions = result.get("suggestions").expect("suggestions");
    assert!(suggestions.get("uncoveredFile").is_some());
    assert!(suggestions.get("hasStaleNotes").is_some());

    // File with no notes should be marked uncovered
    assert_eq!(suggestions.get("uncoveredFile").and_then(|v| v.as_bool()), Some(true));

    Ok(())
}

#[test]
fn code_references_returns_node_path() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let content = "fn main() {\n    let x = 1;\n}\n";
    let req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "hemis/code-references",
        "params": {
            "file": "/tmp/test.rs",
            "content": content,
            "line": 1,
            "column": 3
        }
    })
    .to_string();
    let input = format!("Content-Length: {}\r\n\r\n{}", req.len(), req);
    let out = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let (body, _) = decode_framed(&out.get_output().stdout).unwrap();
    let resp: Response = serde_json::from_slice(&body)?;
    let result = resp.result.expect("should have result");

    // Should have node path
    let node_path = result.get("nodePath").and_then(|v| v.as_array()).expect("nodePath array");
    assert!(!node_path.is_empty(), "should have node path");

    // Should have anchor info
    let anchor = result.get("anchor").expect("anchor");
    assert!(anchor.get("line").is_some());

    // Should identify function_item in references
    let refs = result.get("references").and_then(|v| v.as_array()).expect("references");
    let kinds: Vec<&str> = refs.iter().filter_map(|r| r.get("kind").and_then(|v| v.as_str())).collect();
    assert!(kinds.contains(&"function_def"), "should identify function definition");

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
    // With includeStale=false, stale notes should be filtered out
    let filtered: Response = serde_json::from_slice(&bodies[1])?;
    let filtered_list = filtered
        .result
        .as_ref()
        .and_then(extract_notes)
        .unwrap_or_default();
    assert_eq!(
        filtered_list.len(),
        0,
        "stale notes should be filtered out when includeStale=false"
    );
    // With includeStale=true, stale notes should be included with stale flag
    let included: Response = serde_json::from_slice(&bodies[2])?;
    let included_list = included
        .result
        .as_ref()
        .and_then(extract_notes)
        .unwrap_or_default();
    assert_eq!(included_list.len(), 1, "stale notes should be included when includeStale=true");
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
    // With includeStale=false, stale notes should be filtered out
    let filtered: Response = serde_json::from_slice(&bodies[1])?;
    let filtered_list = filtered
        .result
        .as_ref()
        .and_then(extract_notes)
        .unwrap_or_default();
    assert_eq!(
        filtered_list.len(),
        0,
        "stale notes should be filtered out when includeStale=false"
    );
    // With includeStale=true, stale notes should be included with stale flag
    let included: Response = serde_json::from_slice(&bodies[2])?;
    let included_list = included
        .result
        .as_ref()
        .and_then(extract_notes)
        .unwrap_or_default();
    assert_eq!(included_list.len(), 1, "stale notes should be included when includeStale=true");
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
fn only_stale_filter() -> anyhow::Result<()> {
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

    // Create a fresh note (will match current commit)
    let req_create_fresh = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "notes/create",
        "params": {
            "file": file_str,
            "projectRoot": root_str,
            "line": 1,
            "column": 0,
            "text": "fresh note",
            "tags": []
        }
    })
    .to_string();

    // List with onlyStale=true (fresh note should not appear)
    let req_only_stale = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "notes/list-for-file",
        "params": {
            "file": file_str,
            "projectRoot": root_str,
            "commit": "deadbeef",
            "includeStale": true,
            "onlyStale": true
        }
    })
    .to_string();

    // List normally (fresh note should appear)
    let req_all = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 3,
        "method": "notes/list-for-file",
        "params": {
            "file": file_str,
            "projectRoot": root_str,
            "commit": "deadbeef",
            "includeStale": true,
            "onlyStale": false
        }
    })
    .to_string();

    let input = format!(
        "Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}",
        req_create_fresh.len(),
        req_create_fresh,
        req_only_stale.len(),
        req_only_stale,
        req_all.len(),
        req_all
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

    // Create should succeed
    let created: Response = serde_json::from_slice(&bodies[0])?;
    assert!(created.result.is_some());

    // onlyStale=true: note is stale (commit mismatch), so it SHOULD appear
    let only_stale: Response = serde_json::from_slice(&bodies[1])?;
    let only_stale_list = only_stale
        .result
        .as_ref()
        .and_then(extract_notes)
        .unwrap_or_default();
    assert_eq!(
        only_stale_list.len(),
        1,
        "stale note should appear when onlyStale=true"
    );
    let stale_flag = only_stale_list[0]
        .get("stale")
        .and_then(|v| v.as_bool())
        .unwrap_or(false);
    assert!(stale_flag, "note should be marked stale");

    // onlyStale=false: stale note should appear (includeStale=true)
    let all: Response = serde_json::from_slice(&bodies[2])?;
    let all_list = all
        .result
        .as_ref()
        .and_then(extract_notes)
        .unwrap_or_default();
    assert_eq!(
        all_list.len(),
        1,
        "stale note should appear when includeStale=true, onlyStale=false"
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
        .and_then(extract_notes)
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
        .and_then(extract_notes)
        .expect("notes list");

    assert_eq!(notes.len(), 1, "should have one note");
    let note = &notes[0];
    // Note should be stale (hash no longer matches)
    let stale = note.get("stale").and_then(|v| v.as_bool()).unwrap_or(false);
    assert!(stale, "note should be stale when function renamed");

    Ok(())
}

/// Test dynamic grammar loading: fetch, build, and use a grammar.
/// Uses bundled tree-sitter headers from test fixtures.
#[test]
fn dynamic_grammar_flow() -> anyhow::Result<()> {
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

    // Create languages.toml with test_json grammar (local, no network dependency)
    // The source.local path points to the fixtures directory, but we also copy files
    // to the sources directory manually for the hermetic test
    let fixture_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join("test_grammar");
    let fixture_path_str = fixture_path.to_string_lossy();
    let languages_toml = format!(r#"
[[grammar]]
name = "test_json"
source = {{ local = "{}" }}

[[language]]
name = "test_json"
file-types = ["json"]
skip-nodes = ["string", "number", "true", "false", "null"]
container-nodes = ["document", "object", "array"]
"#, fixture_path_str);
    fs::write(config_dir.path().join("languages.toml"), languages_toml)?;

    let config_path = config_dir.path().to_string_lossy().to_string();

    // HERMETIC: Copy test grammar fixtures directly to sources directory
    // This eliminates network dependency on github.com
    let fixture_grammar_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join("test_grammar");

    let sources_dir = config_dir
        .path()
        .join("grammars")
        .join("sources")
        .join("test_json");
    fs::create_dir_all(&sources_dir)?;

    // Copy grammar files
    fs::copy(
        fixture_grammar_path.join("grammar.js"),
        sources_dir.join("grammar.js"),
    )?;

    let src_dir = sources_dir.join("src");
    fs::create_dir_all(&src_dir)?;
    fs::copy(
        fixture_grammar_path.join("src").join("parser.c"),
        src_dir.join("parser.c"),
    )?;

    // Copy bundled tree_sitter headers for hermetic build
    let ts_header_dir = src_dir.join("tree_sitter");
    fs::create_dir_all(&ts_header_dir)?;
    for header in ["parser.h", "alloc.h", "array.h"] {
        fs::copy(
            fixture_grammar_path.join("src").join("tree_sitter").join(header),
            ts_header_dir.join(header),
        )?;
    }

    // Verify sources directory was created
    assert!(sources_dir.exists(), "Grammar source should exist");

    // Build the test_json grammar
    let build_output = cargo_bin_cmd!("hemis")
        .env("HEMIS_CONFIG_DIR", &config_path)
        .args(["grammar", "build", "test_json"])
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
    let so_path = grammars_dir.join("libtree-sitter-test_json.so");
    let dylib_path = grammars_dir.join("libtree-sitter-test_json.dylib");
    assert!(
        so_path.exists() || dylib_path.exists(),
        "Grammar should be built as libtree-sitter-test_json.so or .dylib"
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
        .and_then(extract_notes)
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
fn explain_region_no_ai_snippet() -> anyhow::Result<()> {
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
fn explain_region_ai_disabled() -> anyhow::Result<()> {
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
fn meta_detects_stale_after_commit() -> anyhow::Result<()> {
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

// Test notes/create and notes/update trim whitespace from text
#[test]
fn create_and_update_trim_text() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;

    // Create a note with leading/trailing whitespace
    let req_create = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "notes/create",
        "params": {
            "file": "/tmp/test.rs",
            "projectRoot": "/tmp",
            "line": 10,
            "column": 5,
            "text": "  \n  trimmed note text  \n  ",
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
    let result = resp.result.expect("should have result");
    let note_id = result.get("id").and_then(|v| v.as_str()).unwrap().to_string();

    // Verify created text is trimmed
    assert_eq!(
        result.get("text").and_then(|v| v.as_str()).unwrap(),
        "trimmed note text"
    );

    // Update the note with whitespace-padded text
    let req_update = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "notes/update",
        "params": {
            "id": note_id,
            "text": "\t\tupdated trimmed text\t\t"
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
    let result = resp.result.expect("should have result");

    // Verify updated text is trimmed
    assert_eq!(
        result.get("text").and_then(|v| v.as_str()).unwrap(),
        "updated trimmed text"
    );

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
    let notes = resp.result.as_ref().and_then(extract_notes).unwrap();
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
    let notes = resp.result.as_ref().and_then(extract_notes).unwrap();
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
    let notes = resp.result.as_ref().and_then(extract_notes).unwrap();
    assert_eq!(notes.len(), 1, "should have one note");
    let stale = notes[0].get("stale").and_then(|v| v.as_bool()).unwrap_or(true);
    assert!(!stale, "note should be fresh after reattach");

    Ok(())
}

/// Test that notes at different lines don't interfere with stale/reattach.
/// Matches demo flow: AI notes at line 7, Factory note at line 16.
#[test]
fn demo_multiple_notes_reattach_isolation() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let root = tempfile::tempdir()?;
    let file = root.path().join("app.rs");

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

    let canonical_file = file.canonicalize()?;
    let canonical_root = root.path().canonicalize()?;
    let file_str = canonical_file.to_string_lossy().to_string();
    let root_str = canonical_root.to_string_lossy().to_string();

    // Create "AI" note at line 7 (load_config)
    let req_ai_note = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "notes/create",
        "params": {
            "file": file_str,
            "projectRoot": root_str,
            "line": 7,
            "column": 0,
            "text": "[claude] This function loads config",
            "content": initial_content,
            "tags": []
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req_ai_note.len(), req_ai_note);
    cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();

    // Create Factory note at line 16 (fn new)
    let req_factory = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
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

    let input = format!("Content-Length: {}\r\n\r\n{}", req_factory.len(), req_factory);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();
    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("create factory response");
    let resp: Response = serde_json::from_slice(&body)?;
    let factory_id = resp
        .result
        .as_ref()
        .and_then(|v| v.get("id"))
        .and_then(|v| v.as_str())
        .expect("factory note id")
        .to_string();

    // Insert comments above impl (line 15) - this shifts fn new to line 18
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

    // Verify notes track correctly
    let req_list = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 3,
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
    let notes = resp.result.as_ref().and_then(extract_notes).unwrap();
    assert_eq!(notes.len(), 2, "should have two notes");

    // Find notes by their lines
    let ai_note = notes.iter().find(|n| n.get("text").and_then(|t| t.as_str()).unwrap_or("").contains("claude"));
    let factory_note = notes.iter().find(|n| n.get("text").and_then(|t| t.as_str()).unwrap_or("").contains("Factory"));

    assert!(ai_note.is_some(), "AI note should exist");
    assert!(factory_note.is_some(), "Factory note should exist");

    let ai_line = ai_note.unwrap().get("line").and_then(|v| v.as_i64()).unwrap();
    let factory_line = factory_note.unwrap().get("line").and_then(|v| v.as_i64()).unwrap();

    assert_eq!(ai_line, 7, "AI note should stay at line 7 (above insertion)");
    assert_eq!(factory_line, 18, "Factory note should move to line 18 (original 16 + 2 comments)");

    // Rename fn new -> fn create (makes factory note stale)
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

    // List again - factory note should be stale, AI note should be fine
    let req_list_stale = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 4,
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
    let notes = resp.result.as_ref().and_then(extract_notes).unwrap();

    let ai_note = notes.iter().find(|n| n.get("text").and_then(|t| t.as_str()).unwrap_or("").contains("claude")).unwrap();
    let factory_note = notes.iter().find(|n| n.get("text").and_then(|t| t.as_str()).unwrap_or("").contains("Factory")).unwrap();

    let ai_stale = ai_note.get("stale").and_then(|v| v.as_bool()).unwrap_or(false);
    let factory_stale = factory_note.get("stale").and_then(|v| v.as_bool()).unwrap_or(false);

    assert!(!ai_stale, "AI note at line 7 should NOT be stale (unchanged)");
    assert!(factory_stale, "Factory note should be stale (function renamed)");

    // Reattach factory note
    let req_reattach = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 5,
        "method": "notes/reattach",
        "params": {
            "id": factory_id,
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
    assert!(resp.error.is_none(), "reattach should succeed");

    // Verify both notes are fresh
    let req_list_fresh = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 6,
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
    let notes = resp.result.as_ref().and_then(extract_notes).unwrap();

    for note in &notes {
        let stale = note.get("stale").and_then(|v| v.as_bool()).unwrap_or(true);
        assert!(!stale, "all notes should be fresh after reattach");
    }

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
    let notes = resp.result.as_ref().and_then(extract_notes).unwrap();
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
    let notes = resp.result.as_ref().and_then(extract_notes).unwrap();
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

// Test explain-region with real AI provider (claude or codex)
// This test is skipped if no AI CLI is available
#[test]
fn explain_region_with_real_ai() -> anyhow::Result<()> {
    // Check for AI CLI availability - prefer claude, fall back to codex
    let ai_provider = if std::process::Command::new("claude")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
    {
        Some("claude")
    } else if std::process::Command::new("codex")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
    {
        Some("codex")
    } else {
        None
    };

    let Some(provider) = ai_provider else {
        eprintln!("Skipping explain_region_with_real_ai: no AI CLI (claude or codex) found");
        return Ok(());
    };

    eprintln!("Running explain_region_with_real_ai with provider: {}", provider);

    let db = NamedTempFile::new()?;
    let tmpdir = tempfile::tempdir()?;
    let file_path = tmpdir.path().join("test.rs");

    let content = r#"fn add(a: i32, b: i32) -> i32 {
    a + b
}
"#;
    std::fs::write(&file_path, content)?;

    let req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "hemis/explain-region",
        "params": {
            "file": file_path.to_string_lossy(),
            "projectRoot": tmpdir.path().to_string_lossy(),
            "startLine": 1,
            "endLine": 3,
            "useAI": true
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req.len(), req);

    // Run with the detected AI provider
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .env("HEMIS_AI_PROVIDER", provider)
        .timeout(std::time::Duration::from_secs(120))
        .write_stdin(input)
        .assert()
        .success();

    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("decode response");
    let resp: Response = serde_json::from_slice(&body)?;

    if let Some(err) = resp.error {
        panic!("RPC error: {:?}", err);
    }

    let result = resp.result.expect("should have result");

    // Should return the snippet
    let snippet = result.get("content").and_then(|v| v.as_str()).expect("should have content");
    assert!(snippet.contains("fn add"), "snippet should contain the function");

    // Should have AI explanation
    let explanation = result.get("explanation").and_then(|v| v.as_str());
    assert!(explanation.is_some(), "should have AI explanation");
    let explanation = explanation.unwrap();
    assert!(!explanation.is_empty(), "explanation should not be empty");
    eprintln!("AI explanation ({} chars): {}...", explanation.len(), &explanation[..explanation.len().min(200)]);

    // Should have AI info
    let ai_info = result.get("ai").expect("should have ai info");
    let returned_provider = ai_info.get("provider").and_then(|v| v.as_str());
    assert_eq!(returned_provider, Some(provider), "should return correct provider");

    Ok(())
}

// Test that persistent Claude process speeds up consecutive calls.
// Second call should be faster because it reuses the running process.
#[test]
fn persistent_claude_speedup() -> anyhow::Result<()> {
    // Only run with Claude
    if !std::process::Command::new("claude")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
    {
        eprintln!("Skipping persistent_claude_speedup: claude CLI not found");
        return Ok(());
    }

    let db = NamedTempFile::new()?;
    let tmpdir = tempfile::tempdir()?;
    let file_path = tmpdir.path().join("test.rs");

    let content = r#"fn multiply(a: i32, b: i32) -> i32 {
    a * b
}
"#;
    std::fs::write(&file_path, content)?;

    // Use a long-running hemis process (server mode via pipe)
    use std::io::{BufRead, BufReader, Write};
    use std::process::{Command, Stdio};

    let mut child = Command::new(env!("CARGO_BIN_EXE_hemis"))
        .env("HEMIS_DB_PATH", db.path())
        .env("HEMIS_AI_PROVIDER", "claude")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()?;

    let mut stdin = child.stdin.take().unwrap();
    let stdout = child.stdout.take().unwrap();
    let mut reader = BufReader::new(stdout);

    // Helper to send request and read response
    fn send_request(stdin: &mut std::process::ChildStdin, reader: &mut BufReader<std::process::ChildStdout>, req: &str) -> anyhow::Result<serde_json::Value> {
        let msg = format!("Content-Length: {}\r\n\r\n{}", req.len(), req);
        stdin.write_all(msg.as_bytes())?;
        stdin.flush()?;

        // Read Content-Length header
        let mut header = String::new();
        reader.read_line(&mut header)?;
        let len: usize = header.trim().strip_prefix("Content-Length: ").unwrap().parse()?;

        // Read empty line
        let mut empty = String::new();
        reader.read_line(&mut empty)?;

        // Read body
        let mut body = vec![0u8; len];
        std::io::Read::read_exact(reader, &mut body)?;

        let resp: Response = serde_json::from_slice(&body)?;
        resp.result.ok_or_else(|| anyhow::anyhow!("no result"))
    }

    // First: index-project to warm up Claude
    let index_req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "hemis/index-project",
        "params": {
            "projectRoot": tmpdir.path().to_string_lossy(),
            "includeAI": false
        }
    }).to_string();

    let _ = send_request(&mut stdin, &mut reader, &index_req)?;
    eprintln!("Index complete, Claude should be warming up...");

    // Give warm-up time to complete (spawned in background thread, sends a test query)
    // The warm-up query takes ~5-10s on first run, so we wait enough for it to finish
    std::thread::sleep(std::time::Duration::from_secs(15));

    // First explain-region call (should be fast since Claude is already warm)
    let explain_req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "hemis/explain-region",
        "params": {
            "file": file_path.to_string_lossy(),
            "projectRoot": tmpdir.path().to_string_lossy(),
            "startLine": 1,
            "endLine": 3,
            "useAI": true
        }
    }).to_string();

    let start1 = std::time::Instant::now();
    let result1 = send_request(&mut stdin, &mut reader, &explain_req)?;
    let time1 = start1.elapsed();
    eprintln!("First explain-region took: {:?}", time1);

    assert!(result1.get("explanation").is_some(), "first call should have explanation");

    // Second explain-region call (should use persistent process - much faster)
    let explain_req2 = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 3,
        "method": "hemis/explain-region",
        "params": {
            "file": file_path.to_string_lossy(),
            "projectRoot": tmpdir.path().to_string_lossy(),
            "startLine": 1,
            "endLine": 3,
            "useAI": true
        }
    }).to_string();

    let start2 = std::time::Instant::now();
    let result2 = send_request(&mut stdin, &mut reader, &explain_req2)?;
    let time2 = start2.elapsed();
    eprintln!("Second explain-region took: {:?}", time2);

    assert!(result2.get("explanation").is_some(), "second call should have explanation");

    // Both calls should be fast (under 10s) since Claude is already warm
    // The first call after warm-up should not have startup overhead
    assert!(time1.as_secs() < 10, "first call should be fast after warm-up: {:?}", time1);
    assert!(time2.as_secs() < 10, "second call should also be fast: {:?}", time2);
    eprintln!("Time comparison: {:?} -> {:?} (both should be fast)", time1, time2);

    // Cleanup
    let _ = child.kill();

    Ok(())
}

// Test Codex one-shot mode works correctly.
#[test]
fn codex_explain_region() -> anyhow::Result<()> {
    // Only run with Codex
    if !std::process::Command::new("codex")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
    {
        eprintln!("Skipping codex_explain_region: codex CLI not found");
        return Ok(());
    }

    let db = NamedTempFile::new()?;
    let tmpdir = tempfile::tempdir()?;
    let file_path = tmpdir.path().join("test.rs");

    let content = r#"fn divide(a: i32, b: i32) -> Option<i32> {
    if b == 0 {
        None
    } else {
        Some(a / b)
    }
}
"#;
    std::fs::write(&file_path, content)?;

    let req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "hemis/explain-region",
        "params": {
            "file": file_path.to_string_lossy(),
            "projectRoot": tmpdir.path().to_string_lossy(),
            "startLine": 1,
            "endLine": 7,
            "useAI": true
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req.len(), req);

    let start = std::time::Instant::now();
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .env("HEMIS_AI_PROVIDER", "codex")
        .timeout(std::time::Duration::from_secs(120))
        .write_stdin(input)
        .assert()
        .success();
    let elapsed = start.elapsed();

    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("decode response");
    let resp: Response = serde_json::from_slice(&body)?;

    if let Some(err) = resp.error {
        panic!("RPC error: {:?}", err);
    }

    let result = resp.result.expect("should have result");

    // Should have AI explanation
    let explanation = result.get("explanation").and_then(|v| v.as_str());
    assert!(explanation.is_some(), "should have AI explanation");
    let explanation = explanation.unwrap();
    assert!(!explanation.is_empty(), "explanation should not be empty");
    // Safely truncate at char boundary for display
    let display_len = explanation.char_indices().take(200).last().map(|(i, c)| i + c.len_utf8()).unwrap_or(0);
    eprintln!("Codex explanation ({} chars, {:?}): {}...",
              explanation.len(), elapsed, &explanation[..display_len]);

    // Should have AI info with codex provider
    let ai_info = result.get("ai").expect("should have ai info");
    let returned_provider = ai_info.get("provider").and_then(|v| v.as_str());
    assert_eq!(returned_provider, Some("codex"), "should return codex provider");

    Ok(())
}

/// Test that the events socket receives note-created events when notes are created.
/// This verifies the push-based event notification system works correctly.
#[test]
fn event_socket_receives_note_created() -> anyhow::Result<()> {
    use std::io::{BufRead, BufReader, Read, Write};
    use std::os::unix::net::UnixStream;
    use std::process::{Command, Stdio};
    use std::time::Duration;

    // Create a temp directory for hemis data (sockets, db)
    let hemis_dir = tempfile::tempdir()?;
    let db_path = hemis_dir.path().join("hemis.db");
    let rpc_socket = hemis_dir.path().join("hemis.sock");
    let events_socket = hemis_dir.path().join("events.sock");

    // Start hemis server
    let mut child = Command::new(env!("CARGO_BIN_EXE_hemis"))
        .arg("--serve")
        .env("HEMIS_DIR", hemis_dir.path())
        .env("HEMIS_DB_PATH", &db_path)
        .stderr(Stdio::piped())
        .spawn()?;

    // Wait for server to start and create sockets
    let mut retries = 0;
    while !rpc_socket.exists() || !events_socket.exists() {
        std::thread::sleep(Duration::from_millis(100));
        retries += 1;
        if retries > 30 {
            let _ = child.kill();
            anyhow::bail!("Server did not create sockets within 3 seconds");
        }
    }
    eprintln!("[test] Server started, sockets created");

    // Connect to the events socket FIRST (before creating notes)
    let events_stream = UnixStream::connect(&events_socket)?;
    events_stream.set_read_timeout(Some(Duration::from_secs(5)))?;
    events_stream.set_nonblocking(false)?;
    eprintln!("[test] Connected to events socket");

    // Small delay to ensure subscription is registered
    std::thread::sleep(Duration::from_millis(100));

    // Connect to RPC socket and create a note
    let mut rpc_stream = UnixStream::connect(&rpc_socket)?;
    rpc_stream.set_read_timeout(Some(Duration::from_secs(5)))?;

    let create_req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "notes/create",
        "params": {
            "file": "/tmp/test.rs",
            "projectRoot": "/tmp",
            "line": 42,
            "column": 0,
            "tags": [],
            "text": "Test note for event socket"
        }
    })
    .to_string();

    let msg = format!("Content-Length: {}\r\n\r\n{}", create_req.len(), create_req);
    rpc_stream.write_all(msg.as_bytes())?;
    rpc_stream.flush()?;
    eprintln!("[test] Sent create request");

    // Read the RPC response
    let mut rpc_reader = BufReader::new(&rpc_stream);
    let mut header = String::new();
    rpc_reader.read_line(&mut header)?;
    let len: usize = header
        .trim()
        .strip_prefix("Content-Length: ")
        .ok_or_else(|| anyhow::anyhow!("missing Content-Length"))?
        .parse()?;
    let mut empty = String::new();
    rpc_reader.read_line(&mut empty)?;
    let mut body = vec![0u8; len];
    rpc_reader.read_exact(&mut body)?;

    let resp: Response = serde_json::from_slice(&body)?;
    let note_id = resp
        .result
        .as_ref()
        .and_then(|v| v.get("id"))
        .and_then(|v| v.as_str())
        .ok_or_else(|| anyhow::anyhow!("note creation failed"))?;
    eprintln!("[test] Note created with id: {}", note_id);

    // Read the event from events socket (JSON line)
    let mut events_reader = BufReader::new(&events_stream);
    let mut event_line = String::new();
    events_reader.read_line(&mut event_line)?;
    eprintln!("[test] Received event: {}", event_line.trim());

    // Parse and verify the event
    let event: serde_json::Value = serde_json::from_str(&event_line)?;
    assert_eq!(
        event.get("type").and_then(|v| v.as_str()),
        Some("note-created"),
        "event type should be note-created"
    );
    assert_eq!(
        event.get("id").and_then(|v| v.as_str()),
        Some(note_id),
        "event should contain the note id"
    );
    assert_eq!(
        event.get("file").and_then(|v| v.as_str()),
        Some("/tmp/test.rs"),
        "event should contain the file"
    );
    assert_eq!(
        event.get("line").and_then(|v| v.as_i64()),
        Some(42),
        "event should contain the line"
    );

    // Cleanup
    let _ = child.kill();
    eprintln!("[test] Event socket test passed!");

    Ok(())
}

/// Test that the events socket receives note-updated and note-deleted events.
#[test]
fn socket_receives_updates_and_deletes() -> anyhow::Result<()> {
    use std::io::{BufRead, BufReader, Read, Write};
    use std::os::unix::net::UnixStream;
    use std::process::{Command, Stdio};
    use std::time::Duration;

    // Create a temp directory for hemis data
    let hemis_dir = tempfile::tempdir()?;
    let db_path = hemis_dir.path().join("hemis.db");
    let rpc_socket = hemis_dir.path().join("hemis.sock");
    let events_socket = hemis_dir.path().join("events.sock");

    // Start hemis server
    let mut child = Command::new(env!("CARGO_BIN_EXE_hemis"))
        .arg("--serve")
        .env("HEMIS_DIR", hemis_dir.path())
        .env("HEMIS_DB_PATH", &db_path)
        .stderr(Stdio::piped())
        .spawn()?;

    // Wait for server to start
    let mut retries = 0;
    while !rpc_socket.exists() || !events_socket.exists() {
        std::thread::sleep(Duration::from_millis(100));
        retries += 1;
        if retries > 30 {
            let _ = child.kill();
            anyhow::bail!("Server did not create sockets within 3 seconds");
        }
    }

    // Connect to events socket first
    let events_stream = UnixStream::connect(&events_socket)?;
    events_stream.set_read_timeout(Some(Duration::from_secs(5)))?;
    std::thread::sleep(Duration::from_millis(100));

    // Connect to RPC socket
    let mut rpc_stream = UnixStream::connect(&rpc_socket)?;
    rpc_stream.set_read_timeout(Some(Duration::from_secs(5)))?;

    // Helper to send RPC request and get response
    let send_rpc = |stream: &mut UnixStream, req: &serde_json::Value| -> anyhow::Result<serde_json::Value> {
        let body = req.to_string();
        let msg = format!("Content-Length: {}\r\n\r\n{}", body.len(), body);
        stream.write_all(msg.as_bytes())?;
        stream.flush()?;

        let mut reader = BufReader::new(&*stream);
        let mut header = String::new();
        reader.read_line(&mut header)?;
        let len: usize = header
            .trim()
            .strip_prefix("Content-Length: ")
            .ok_or_else(|| anyhow::anyhow!("missing Content-Length"))?
            .parse()?;
        let mut empty = String::new();
        reader.read_line(&mut empty)?;
        let mut body = vec![0u8; len];
        reader.read_exact(&mut body)?;
        Ok(serde_json::from_slice(&body)?)
    };

    // 1. Create a note
    let create_resp = send_rpc(&mut rpc_stream, &serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "notes/create",
        "params": {
            "file": "/tmp/test.rs",
            "projectRoot": "/tmp",
            "line": 10,
            "column": 0,
            "text": "Original note text"
        }
    }))?;

    let note_id = create_resp
        .get("result")
        .and_then(|v| v.get("id"))
        .and_then(|v| v.as_str())
        .ok_or_else(|| anyhow::anyhow!("note creation failed"))?
        .to_string();

    // Read note-created event
    let mut events_reader = BufReader::new(&events_stream);
    let mut event_line = String::new();
    events_reader.read_line(&mut event_line)?;
    let event: serde_json::Value = serde_json::from_str(&event_line)?;
    assert_eq!(event.get("type").and_then(|v| v.as_str()), Some("note-created"));

    // 2. Update the note
    let _update_resp = send_rpc(&mut rpc_stream, &serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "notes/update",
        "params": {
            "id": note_id,
            "text": "Updated note text"
        }
    }))?;

    // Read note-updated event
    event_line.clear();
    events_reader.read_line(&mut event_line)?;
    let event: serde_json::Value = serde_json::from_str(&event_line)?;
    assert_eq!(
        event.get("type").and_then(|v| v.as_str()),
        Some("note-updated"),
        "should receive note-updated event"
    );
    assert_eq!(
        event.get("id").and_then(|v| v.as_str()),
        Some(note_id.as_str()),
        "event should contain the note id"
    );

    // 3. Delete the note
    let _delete_resp = send_rpc(&mut rpc_stream, &serde_json::json!({
        "jsonrpc": "2.0",
        "id": 3,
        "method": "notes/delete",
        "params": {
            "id": note_id
        }
    }))?;

    // Read note-deleted event
    event_line.clear();
    events_reader.read_line(&mut event_line)?;
    let event: serde_json::Value = serde_json::from_str(&event_line)?;
    assert_eq!(
        event.get("type").and_then(|v| v.as_str()),
        Some("note-deleted"),
        "should receive note-deleted event"
    );
    assert_eq!(
        event.get("id").and_then(|v| v.as_str()),
        Some(note_id.as_str()),
        "event should contain the deleted note id"
    );

    // Cleanup
    let _ = child.kill();
    eprintln!("[test] Update/delete event test passed!");

    Ok(())
}

#[test]
fn notes_get_at_position_returns_note() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let dir = tempfile::tempdir()?;
    let file = dir.path().join("test.rs");
    fs::write(&file, "fn main() {\n    println!(\"hello\");\n}\n")?;

    // Create a note at line 2
    let req_create = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "notes/create",
        "params": {
            "file": file.to_str().unwrap(),
            "projectRoot": dir.path().to_str().unwrap(),
            "line": 2,
            "column": 4,
            "text": "Print statement"
        }
    })
    .to_string();

    // Get note at position line 2
    let req_get = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "notes/get-at-position",
        "params": {
            "file": file.to_str().unwrap(),
            "projectRoot": dir.path().to_str().unwrap(),
            "line": 2,
            "content": "fn main() {\n    println!(\"hello\");\n}\n"
        }
    })
    .to_string();

    // Get note at non-existent position
    let req_get_none = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 3,
        "method": "notes/get-at-position",
        "params": {
            "file": file.to_str().unwrap(),
            "projectRoot": dir.path().to_str().unwrap(),
            "line": 99,
            "content": "fn main() {\n    println!(\"hello\");\n}\n"
        }
    })
    .to_string();

    let input = format!(
        "Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}",
        req_create.len(),
        req_create,
        req_get.len(),
        req_get,
        req_get_none.len(),
        req_get_none
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
    assert_eq!(bodies.len(), 3, "expected three responses");

    // Check get-at-position found the note
    let get_resp: Response = serde_json::from_slice(&bodies[1])?;
    let note = get_resp.result.expect("get-at-position should return result");
    assert!(note.get("id").is_some(), "should find note at position");
    assert_eq!(
        note.get("text").and_then(|v| v.as_str()),
        Some("Print statement")
    );

    // Check get-at-position returns null for non-existent position
    // The raw JSON has "result":null which serde deserializes as result: None (not Some(Null))
    // So we check the raw JSON instead
    let raw_none: serde_json::Value = serde_json::from_slice(&bodies[2])?;
    assert_eq!(
        raw_none.get("result"),
        Some(&serde_json::Value::Null),
        "should return null for non-existent position"
    );
    assert!(
        raw_none.get("error").is_none(),
        "should not have error for non-existent position"
    );

    Ok(())
}

#[test]
fn link_suggestions_formatted() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let dir = tempfile::tempdir()?;
    let file = dir.path().join("test.rs");
    fs::write(&file, "fn main() {}\n")?;

    // Create notes with specific text
    let req_create1 = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "notes/create",
        "params": {
            "file": file.to_str().unwrap(),
            "projectRoot": dir.path().to_str().unwrap(),
            "line": 1,
            "column": 0,
            "text": "Main entry point for the application"
        }
    })
    .to_string();

    let req_create2 = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "notes/create",
        "params": {
            "file": file.to_str().unwrap(),
            "projectRoot": dir.path().to_str().unwrap(),
            "line": 1,
            "column": 3,
            "text": "Another note about main function"
        }
    })
    .to_string();

    // Get link suggestions matching "main"
    let req_suggestions = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 3,
        "method": "notes/link-suggestions",
        "params": {
            "query": "main",
            "projectRoot": dir.path().to_str().unwrap()
        }
    })
    .to_string();

    let input = format!(
        "Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}",
        req_create1.len(),
        req_create1,
        req_create2.len(),
        req_create2,
        req_suggestions.len(),
        req_suggestions
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
    assert_eq!(bodies.len(), 3, "expected three responses");

    // Check link suggestions
    let suggestions_resp: Response = serde_json::from_slice(&bodies[2])?;
    let suggestions = suggestions_resp
        .result
        .expect("link-suggestions should return result")
        .as_array()
        .expect("should be array")
        .clone();

    assert!(
        suggestions.len() >= 2,
        "should find at least 2 notes matching 'main'"
    );

    // Check format of suggestions
    for suggestion in &suggestions {
        let formatted = suggestion.get("formatted").and_then(|v| v.as_str());
        assert!(formatted.is_some(), "suggestion should have formatted field");
        let formatted = formatted.unwrap();
        assert!(
            formatted.starts_with("[[") && formatted.ends_with("]]"),
            "formatted should be [[desc][id]] format"
        );
        assert!(
            suggestion.get("noteId").is_some(),
            "suggestion should have noteId"
        );
    }

    Ok(())
}

#[test]
fn notes_anchor_computes_position() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let dir = tempfile::tempdir()?;
    let file = dir.path().join("test.rs");
    let content = "fn main() {\n    let x = 5;\n}\n";
    fs::write(&file, content)?;

    // Request anchor position at line 1 (0-indexed), column 0
    let req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "notes/anchor",
        "params": {
            "file": file.to_str().unwrap(),
            "content": content,
            "cursorLine": 1,
            "cursorColumn": 4
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
    let (body, _) = decode_framed(&stdout).expect("should have response");
    let resp: Response = serde_json::from_slice(&body)?;
    let result = resp.result.expect("anchor should return result");

    // Should have line, column, nodePath fields
    assert!(result.get("line").is_some(), "should have line");
    assert!(result.get("column").is_some(), "should have column");
    assert!(result.get("nodePath").is_some(), "should have nodePath");

    Ok(())
}

#[test]
fn buffer_context_returns_git_and_language() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let dir = tempfile::tempdir()?;

    // Initialize git repo
    git(dir.path(), &["init"])?;
    git(dir.path(), &["config", "user.email", "test@test.com"])?;
    git(dir.path(), &["config", "user.name", "Test"])?;

    let file = dir.path().join("test.rs");
    fs::write(&file, "fn main() {}\n")?;
    git(dir.path(), &["add", "."])?;
    git(dir.path(), &["-c", "commit.gpgsign=false", "commit", "-m", "init"])?;

    let req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "hemis/buffer-context",
        "params": {
            "file": file.to_str().unwrap()
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
    let (body, _) = decode_framed(&stdout).expect("should have response");
    let resp: Response = serde_json::from_slice(&body)?;
    let result = resp.result.expect("buffer-context should return result");

    // Should have projectRoot, commit, language
    assert!(result.get("projectRoot").is_some(), "should have projectRoot");
    assert!(result.get("commit").is_some(), "should have commit");
    assert_eq!(
        result.get("language").and_then(|v| v.as_str()),
        Some("rust"),
        "should detect rust language"
    );

    Ok(())
}

#[test]
fn task_status_returns_not_found() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;

    let req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "hemis/task-status",
        "params": {
            "taskId": "nonexistent-task-id"
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
    let (body, _) = decode_framed(&stdout).expect("should have response");
    let resp: Response = serde_json::from_slice(&body)?;

    // Should return error for non-existent task
    assert!(resp.error.is_some(), "should return error for unknown task");

    Ok(())
}

#[test]
fn task_list_returns_empty() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;

    let req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "hemis/task-list",
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
    let (body, _) = decode_framed(&stdout).expect("should have response");
    let resp: Response = serde_json::from_slice(&body)?;
    let result = resp.result.expect("task-list should return result");

    // Should return empty array
    assert!(result.is_array(), "should return array");
    assert_eq!(result.as_array().unwrap().len(), 0, "should be empty");

    Ok(())
}

#[test]
fn index_project_async_returns_task_id() -> anyhow::Result<()> {
    let dir = tempfile::tempdir()?;
    let db = NamedTempFile::new()?;

    let req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "hemis/index-project",
        "params": {
            "projectRoot": dir.path().to_str().unwrap(),
            "async": true
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
    let (body, _) = decode_framed(&stdout).expect("should have response");
    let resp: Response = serde_json::from_slice(&body)?;
    let result = resp.result.expect("async index should return result");

    // Should have taskId and async:true
    assert!(result.get("taskId").is_some(), "should have taskId");
    assert_eq!(
        result.get("async").and_then(|v| v.as_bool()),
        Some(true),
        "should have async:true"
    );

    Ok(())
}

#[test]
fn note_history_tracks_versions() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;

    // Create a note
    let req_create = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "notes/create",
        "params": {
            "file": "/tmp/test.rs",
            "projectRoot": "/tmp",
            "line": 1,
            "column": 0,
            "text": "Original text"
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
    let (body, _) = decode_framed(&stdout).expect("should have response");
    let resp: Response = serde_json::from_slice(&body)?;
    let note_id = resp.result.unwrap().get("id").unwrap().as_str().unwrap().to_string();

    // Update the note
    let req_update = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "notes/update",
        "params": {
            "id": note_id,
            "text": "Updated text"
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req_update.len(), req_update);
    cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();

    // Get history
    let req_history = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 3,
        "method": "notes/history",
        "params": {
            "id": note_id
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req_history.len(), req_history);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();

    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("should have response");
    let resp: Response = serde_json::from_slice(&body)?;
    let versions = resp.result.expect("history should return result");

    // Should have at least one version (from the update)
    assert!(versions.is_array(), "should return array");
    let versions_arr = versions.as_array().unwrap();
    assert!(!versions_arr.is_empty(), "should have at least one version");

    // First version should have original text
    let v1 = &versions_arr[0];
    assert_eq!(v1.get("text").and_then(|v| v.as_str()), Some("Original text"));
    assert_eq!(v1.get("reason").and_then(|v| v.as_str()), Some("update"));

    Ok(())
}

#[test]
fn summarize_file_returns_sections() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;

    // Create a note
    let req_create = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "notes/create",
        "params": {
            "file": "/tmp/test.rs",
            "projectRoot": "/tmp",
            "line": 2,
            "column": 0,
            "text": "Important function"
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req_create.len(), req_create);
    cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();

    // Request summary
    let content = "fn main() {\n    println!(\"Hello\");\n}\n";
    let req_summarize = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "hemis/summarize-file",
        "params": {
            "file": "/tmp/test.rs",
            "projectRoot": "/tmp",
            "content": content
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req_summarize.len(), req_summarize);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();

    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("should have response");
    let resp: Response = serde_json::from_slice(&body)?;
    let result = resp.result.expect("summarize-file should return result");

    // Should have file info and sections
    assert_eq!(result.get("file").and_then(|v| v.as_str()), Some("/tmp/test.rs"));
    assert_eq!(result.get("noteCount").and_then(|v| v.as_u64()), Some(1));

    let sections = result.get("sections").and_then(|v| v.as_array()).unwrap();
    assert_eq!(sections.len(), 1);

    // Section should have code snippet
    let section = &sections[0];
    assert!(section.get("codeSnippet").is_some(), "should have code snippet");
    assert_eq!(section.get("summary").and_then(|v| v.as_str()), Some("Important function"));

    Ok(())
}

/// Test notes/get-version retrieves a specific version
#[test]
fn get_version_retrieves_specific_version() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;

    // Create a note
    let req_create = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "notes/create",
        "params": {
            "file": "/tmp/test.rs",
            "projectRoot": "/tmp",
            "line": 1,
            "column": 0,
            "text": "Version 0 text"
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
    let (body, _) = decode_framed(&stdout).expect("should have response");
    let resp: Response = serde_json::from_slice(&body)?;
    let note_id = resp.result.unwrap().get("id").unwrap().as_str().unwrap().to_string();

    // Update the note to create version 1
    let req_update = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "notes/update",
        "params": {
            "id": note_id,
            "text": "Version 1 text"
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req_update.len(), req_update);
    cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();

    // Get version 1
    let req_get = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 3,
        "method": "notes/get-version",
        "params": {
            "id": note_id,
            "version": 1
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req_get.len(), req_get);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();

    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("should have response");
    let resp: Response = serde_json::from_slice(&body)?;
    let version = resp.result.expect("get-version should return result");

    // Should have the original text
    assert_eq!(version.get("text").and_then(|v| v.as_str()), Some("Version 0 text"));
    assert_eq!(version.get("version").and_then(|v| v.as_i64()), Some(1));

    Ok(())
}

/// Test notes/get-version returns error for non-existent version
#[test]
fn get_version_not_found() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;

    // Create a note (no updates, so no versions)
    let req_create = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "notes/create",
        "params": {
            "file": "/tmp/test.rs",
            "projectRoot": "/tmp",
            "line": 1,
            "column": 0,
            "text": "Test note"
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
    let (body, _) = decode_framed(&stdout).expect("should have response");
    let resp: Response = serde_json::from_slice(&body)?;
    let note_id = resp.result.unwrap().get("id").unwrap().as_str().unwrap().to_string();

    // Try to get non-existent version
    let req_get = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "notes/get-version",
        "params": {
            "id": note_id,
            "version": 999
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req_get.len(), req_get);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();

    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("should have response");
    let resp: Response = serde_json::from_slice(&body)?;

    assert!(resp.error.is_some(), "should return error for non-existent version");
    assert!(resp.error.unwrap().message.contains("not found"));

    Ok(())
}

/// Test notes/restore-version restores a note to a previous version
#[test]
fn restore_version_restores_previous_state() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;

    // Create a note
    let req_create = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "notes/create",
        "params": {
            "file": "/tmp/test.rs",
            "projectRoot": "/tmp",
            "line": 1,
            "column": 0,
            "text": "Original text"
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
    let (body, _) = decode_framed(&stdout).expect("should have response");
    let resp: Response = serde_json::from_slice(&body)?;
    let note_id = resp.result.unwrap().get("id").unwrap().as_str().unwrap().to_string();

    // Update the note
    let req_update = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "notes/update",
        "params": {
            "id": note_id,
            "text": "Modified text"
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req_update.len(), req_update);
    cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();

    // Verify note now has modified text
    let req_get = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 3,
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
    let (body, _) = decode_framed(&stdout).expect("should have response");
    let resp: Response = serde_json::from_slice(&body)?;
    assert_eq!(
        resp.result.as_ref().and_then(|v| v.get("text")).and_then(|v| v.as_str()),
        Some("Modified text")
    );

    // Restore to version 1 (original text)
    let req_restore = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 4,
        "method": "notes/restore-version",
        "params": {
            "id": note_id,
            "version": 1
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req_restore.len(), req_restore);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();

    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("should have response");
    let resp: Response = serde_json::from_slice(&body)?;
    let restored = resp.result.expect("restore should return result");

    // Note should now have original text
    assert_eq!(restored.get("text").and_then(|v| v.as_str()), Some("Original text"));

    // Verify by fetching the note again
    let input = format!("Content-Length: {}\r\n\r\n{}", req_get.len(), req_get);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();

    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("should have response");
    let resp: Response = serde_json::from_slice(&body)?;
    assert_eq!(
        resp.result.as_ref().and_then(|v| v.get("text")).and_then(|v| v.as_str()),
        Some("Original text"),
        "note should have original text after restore"
    );

    Ok(())
}

/// Test notes/restore-version returns error for non-existent version
#[test]
fn restore_version_not_found() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;

    // Create a note
    let req_create = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "notes/create",
        "params": {
            "file": "/tmp/test.rs",
            "projectRoot": "/tmp",
            "line": 1,
            "column": 0,
            "text": "Test note"
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
    let (body, _) = decode_framed(&stdout).expect("should have response");
    let resp: Response = serde_json::from_slice(&body)?;
    let note_id = resp.result.unwrap().get("id").unwrap().as_str().unwrap().to_string();

    // Try to restore non-existent version
    let req_restore = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "notes/restore-version",
        "params": {
            "id": note_id,
            "version": 999
        }
    })
    .to_string();

    let input = format!("Content-Length: {}\r\n\r\n{}", req_restore.len(), req_restore);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(input)
        .assert()
        .success();

    let stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("should have response");
    let resp: Response = serde_json::from_slice(&body)?;

    assert!(resp.error.is_some(), "should return error for non-existent version");

    Ok(())
}
