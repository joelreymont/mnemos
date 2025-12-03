use assert_cmd::cargo::cargo_bin_cmd;
use insta::assert_json_snapshot;
use rpc::decode_framed;
use serde_json::{self, json, Value};
use tempfile::NamedTempFile;

/// Regex for matching UUIDs (full or truncated) in text fields.
fn scrub_uuids_in_string(s: &str) -> String {
    // Match full UUIDs or truncated UUIDs (at least 8 chars of the first segment)
    let re = regex::Regex::new(r"[a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{1,12}\.{0,3}|[a-f0-9]{8}-[a-f0-9]{1,4}\.{0,3}")
        .unwrap();
    re.replace_all(s, "<uuid>").to_string()
}

fn scrub_obj(obj: &mut serde_json::Map<String, Value>) {
    if obj.contains_key("id") {
        obj.insert("id".into(), json!("<id>"));
    }
    if obj.contains_key("createdAt") {
        obj.insert("createdAt".into(), json!("<ts>"));
    }
    if obj.contains_key("updatedAt") {
        obj.insert("updatedAt".into(), json!("<ts>"));
    }
    // Scrub UUIDs from text and summary fields
    for key in ["text", "summary"] {
        if let Some(Value::String(s)) = obj.get(key) {
            let scrubbed = scrub_uuids_in_string(s);
            obj.insert(key.to_string(), json!(scrubbed));
        }
    }
}

fn scrub_response(mut resp: Value) -> Value {
    if let Some(result) = resp.get_mut("result") {
        match result {
            Value::Object(obj) => scrub_obj(obj),
            Value::Array(arr) => {
                for val in arr {
                    if let Some(obj) = val.as_object_mut() {
                        scrub_obj(obj);
                    }
                }
            }
            _ => {}
        }
    }
    resp
}

#[test]
fn snapshot_create_and_list() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let req_create = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "notes/create",
        "params": {
            "file": "/tmp/test.rs",
            "projectRoot": "/tmp",
            "line": 3,
            "column": 2,
            "text": "snapshot note",
            "tags": ["x"]
        }
    })
    .to_string();
    let req_list = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "notes/list-for-file",
        "params": {
            "file": "/tmp/test.rs",
            "projectRoot": "/tmp"
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
    let responses: Vec<Value> = bodies
        .into_iter()
        .map(|b| serde_json::from_slice(&b).unwrap())
        .map(scrub_response)
        .collect();
    assert_json_snapshot!("create_and_list", responses);
    Ok(())
}

#[test]
fn snapshot_update_and_delete() -> anyhow::Result<()> {
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
            "text": "initial",
            "tags": []
        }
    })
    .to_string();
    let req_update_tpl = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "notes/update",
        "params": {
            "id": "<id>",
            "text": "updated"
        }
    });
    let req_delete_tpl = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 3,
        "method": "notes/delete",
        "params": {
            "id": "<id>"
        }
    });
    // First create, then patch in the returned id for update/delete.
    let create_input = format!("Content-Length: {}\r\n\r\n{}", req_create.len(), req_create);
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(create_input)
        .assert()
        .success();
    let mut stdout = assert.get_output().stdout.clone();
    let (body, used) = decode_framed(&stdout).expect("create response");
    stdout.drain(..used);
    let created: Value = serde_json::from_slice(&body)?;
    let note_id = created
        .get("result")
        .and_then(|v| v.get("id"))
        .and_then(|v| v.as_str())
        .unwrap();
    let mut req_update = req_update_tpl.clone();
    let mut req_delete = req_delete_tpl.clone();
    if let Some(obj) = req_update.get_mut("params") {
        if let Some(id) = obj.get_mut("id") {
            *id = Value::String(note_id.to_string());
        }
    }
    if let Some(obj) = req_delete.get_mut("params") {
        if let Some(id) = obj.get_mut("id") {
            *id = Value::String(note_id.to_string());
        }
    }
    let upd = serde_json::to_string(&req_update)?;
    let del = serde_json::to_string(&req_delete)?;
    let input = format!(
        "Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}",
        upd.len(),
        upd,
        del.len(),
        del
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
    let mut responses: Vec<Value> = bodies
        .into_iter()
        .map(|b| serde_json::from_slice(&b).unwrap())
        .map(scrub_response)
        .collect();
    // Prepend the create response for context.
    responses.insert(0, scrub_response(created));
    assert_json_snapshot!("update_and_delete", responses);
    Ok(())
}

#[test]
fn snapshot_list_by_node() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let req_create = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "notes/create",
        "params": {
            "file": "/tmp/test.rs",
            "projectRoot": "/tmp",
            "line": 5,
            "column": 1,
            "nodePath": ["fn", "body"],
            "text": "by-node",
            "tags": []
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
            "nodePath": ["fn", "body"]
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
    let responses: Vec<Value> = bodies
        .into_iter()
        .map(|b| serde_json::from_slice(&b).unwrap())
        .map(scrub_response)
        .collect();
    assert_json_snapshot!("list_by_node", responses);
    Ok(())
}

#[test]
fn snapshot_index_search() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let add_req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "index/add-file",
        "params": {
            "file": "/tmp/foo.rs",
            "projectRoot": "/tmp",
            "content": "fn add(x: i32) -> i32 { x + 1 }\n"
        }
    })
    .to_string();
    let search_req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "index/search",
        "params": {
            "query": "add",
            "projectRoot": "/tmp"
        }
    })
    .to_string();
    let input = format!(
        "Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}",
        add_req.len(),
        add_req,
        search_req.len(),
        search_req
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
    let responses: Vec<Value> = bodies
        .into_iter()
        .map(|b| serde_json::from_slice(&b).unwrap())
        .map(scrub_response)
        .collect();
    assert_json_snapshot!("index_search", responses);
    Ok(())
}

#[test]
fn snapshot_invalid_params() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "notes/create",
        "params": {
            "projectRoot": "/tmp",
            "text": "missing file"
        }
    })
    .to_string();
    let input = format!("Content-Length: {}\r\n\r\n{}", req.len(), req);
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
    let responses: Vec<Value> = bodies
        .into_iter()
        .map(|b| serde_json::from_slice(&b).unwrap())
        .collect();
    assert_json_snapshot!("invalid_params", responses);
    Ok(())
}

#[test]
fn snapshot_missing_id_update_delete() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let req_update = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "notes/update",
        "params": {
            "text": "oops"
        }
    })
    .to_string();
    let req_delete = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "notes/delete",
        "params": {}
    })
    .to_string();
    let input = format!(
        "Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}",
        req_update.len(),
        req_update,
        req_delete.len(),
        req_delete
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
    let responses: Vec<Value> = bodies
        .into_iter()
        .map(|b| serde_json::from_slice(&b).unwrap())
        .collect();
    assert_json_snapshot!("missing_id_update_delete", responses);
    Ok(())
}

#[test]
fn snapshot_unknown_method() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 99,
        "method": "unknown/method",
        "params": {}
    })
    .to_string();
    let input = format!("Content-Length: {}\r\n\r\n{}", req.len(), req);
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
    let responses: Vec<Value> = bodies
        .into_iter()
        .map(|b| serde_json::from_slice(&b).unwrap())
        .collect();
    assert_json_snapshot!("unknown_method", responses);
    Ok(())
}

#[test]
fn snapshot_explain_region() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let root = tempfile::tempdir()?;
    let file_path = root.path().join("code.rs");
    std::fs::write(&file_path, "fn add(x: i32) -> i32 {\n    x + 1\n}\n")?;
    let req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "hemis/explain-region",
        "params": {
            "file": file_path.to_string_lossy(),
            "start": {"line": 1, "column": 0},
            "end": {"line": 2, "column": 0}
        }
    })
    .to_string();
    let input = format!("Content-Length: {}\r\n\r\n{}", req.len(), req);
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
    let responses: Vec<Value> = bodies
        .into_iter()
        .map(|b| serde_json::from_slice(&b).unwrap())
        .collect();
    assert_json_snapshot!("explain_region", responses);
    Ok(())
}

#[test]
fn snapshot_save_and_load() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let root = tempfile::tempdir()?;
    let file_path = root.path().join("code.rs");
    std::fs::write(&file_path, "fn main() {}\n")?;
    // Index a file so the snapshot has counts.
    let add_req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "index/add-file",
        "params": {
            "file": file_path.to_string_lossy(),
            "projectRoot": root.path().to_string_lossy(),
            "content": "fn main() {}\n"
        }
    })
    .to_string();
    let snap_path = tempfile::NamedTempFile::new()?;
    let save_req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "hemis/save-snapshot",
        "params": {
            "path": snap_path.path(),
            "projectRoot": root.path().to_string_lossy()
        }
    })
    .to_string();
    let load_req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 3,
        "method": "hemis/load-snapshot",
        "params": {
            "path": snap_path.path()
        }
    })
    .to_string();
    let input = format!(
        "Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}",
        add_req.len(),
        add_req,
        save_req.len(),
        save_req,
        load_req.len(),
        load_req
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
    let responses: Vec<Value> = bodies
        .into_iter()
        .map(|b| serde_json::from_slice(&b).unwrap())
        .map(|mut v: Value| {
            if let Some(result) = v.get_mut("result") {
                if let Some(map) = result.as_object_mut() {
                    for key in ["file", "projectRoot", "path"] {
                        if let Some(val) = map.get_mut(key) {
                            if let Some(s) = val.as_str() {
                                *val = Value::String(
                                    s.replace(root.path().to_string_lossy().as_ref(), "/tmp"),
                                );
                            }
                        }
                    }
                    for key in ["updatedAt", "createdAt"] {
                        if map.contains_key(key) {
                            map.insert(key.to_string(), Value::String("<ts>".into()));
                        }
                    }
                    if map.get("path").is_some() {
                        map.insert(
                            "path".to_string(),
                            Value::String("/tmp/snapshot.json".into()),
                        );
                    }
                }
            }
            v
        })
        .collect();
    assert_json_snapshot!("snapshot_save_load", responses);
    Ok(())
}

#[test]
fn snapshot_status_returns_counts() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let root = tempfile::tempdir()?;
    let file_path = root.path().join("code.rs");
    std::fs::write(&file_path, "fn main() {}\n")?;
    let add_req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "index/add-file",
        "params": {
            "file": file_path.to_string_lossy(),
            "projectRoot": root.path().to_string_lossy(),
            "content": "fn main() {}\n"
        }
    })
    .to_string();
    let status_req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "hemis/status",
        "params": {
            "projectRoot": root.path().to_string_lossy()
        }
    })
    .to_string();
    let input = format!(
        "Content-Length: {}\r\n\r\n{}Content-Length: {}\r\n\r\n{}",
        add_req.len(),
        add_req,
        status_req.len(),
        status_req
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
    let root_prefix = root.path().to_string_lossy().to_string();
    let responses: Vec<Value> = bodies
        .into_iter()
        .map(|b| serde_json::from_slice(&b).unwrap())
        .map(|mut v: Value| {
            if let Some(result) = v.get_mut("result") {
                if let Some(map) = result.as_object_mut() {
                    for key in ["projectRoot", "file"] {
                        if let Some(Value::String(val)) = map.get_mut(key) {
                            *val = val.replace(&root_prefix, "/tmp");
                        }
                    }
                    if map.contains_key("updatedAt") {
                        map.insert("updatedAt".into(), Value::String("<ts>".into()));
                    }
                }
            }
            v
        })
        .collect();
    assert_json_snapshot!("snapshot_status", responses);
    Ok(())
}

#[test]
fn snapshot_index_project() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let root = tempfile::tempdir()?;
    let file_path = root.path().join("foo.rs");
    std::fs::write(&file_path, "fn foo() {}\n")?;
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
            "query": "foo",
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
    let root_prefix = root.path().to_string_lossy().to_string();
    let responses: Vec<Value> = bodies
        .into_iter()
        .map(|b| serde_json::from_slice(&b).unwrap())
        .map(|mut v: Value| {
            if let Some(result) = v.get_mut("result") {
                match result {
                    Value::Array(arr) => {
                        for val in arr.iter_mut() {
                            if let Some(file) = val.get_mut("file") {
                                if let Some(s) = file.as_str() {
                                    *file = Value::String(s.replace(root_prefix.as_str(), "/tmp"));
                                }
                            }
                        }
                    }
                    Value::Object(map) => {
                        if let Some(val) = map.get("projectRoot").and_then(|v| v.as_str()) {
                            let replaced = Value::String(val.replace(root_prefix.as_str(), "/tmp"));
                            map.insert("projectRoot".to_string(), replaced);
                        }
                    }
                    _ => {}
                }
            }
            v
        })
        .collect();
    assert_json_snapshot!("index_project", responses);
    Ok(())
}

#[test]
fn snapshot_backlinks() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    // Create target note A
    let req_create_a = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "notes/create",
        "params": {
            "file": "/tmp/test.rs",
            "projectRoot": "/tmp",
            "line": 1,
            "column": 0,
            "text": "Note A - the target"
        }
    })
    .to_string();
    let create_a_input = format!(
        "Content-Length: {}\r\n\r\n{}",
        req_create_a.len(),
        req_create_a
    );
    let assert = cargo_bin_cmd!("hemis")
        .env("HEMIS_DB_PATH", db.path())
        .write_stdin(create_a_input)
        .assert()
        .success();
    let mut stdout = assert.get_output().stdout.clone();
    let (body, _) = decode_framed(&stdout).expect("create A response");
    let created_a: Value = serde_json::from_slice(&body)?;
    let note_a_id = created_a
        .get("result")
        .and_then(|v| v.get("id"))
        .and_then(|v| v.as_str())
        .unwrap();

    // Create note B that links to A
    let req_create_b = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "notes/create",
        "params": {
            "file": "/tmp/test.rs",
            "projectRoot": "/tmp",
            "line": 10,
            "column": 0,
            "text": format!("Note B links to [[target][{}]]", note_a_id)
        }
    })
    .to_string();
    // Query backlinks for A
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
    stdout = assert.get_output().stdout.clone();
    let mut bodies = Vec::new();
    while let Some((body, used)) = decode_framed(&stdout) {
        bodies.push(body);
        stdout.drain(..used);
    }
    let mut responses: Vec<Value> = bodies
        .into_iter()
        .map(|b| serde_json::from_slice(&b).unwrap())
        .map(scrub_response)
        .collect();
    // Prepend the create A response for context
    responses.insert(0, scrub_response(created_a));
    assert_json_snapshot!("backlinks", responses);
    Ok(())
}
