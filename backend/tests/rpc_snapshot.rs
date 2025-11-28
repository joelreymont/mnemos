use assert_cmd::cargo::cargo_bin_cmd;
use insta::assert_json_snapshot;
use rpc::decode_framed;
use serde_json::{self, json, Value};
use tempfile::NamedTempFile;

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
    let assert = cargo_bin_cmd!("backend")
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
    let responses: Vec<Value> = bodies
        .into_iter()
        .map(|b| serde_json::from_slice(&b).unwrap())
        .collect();
    assert_json_snapshot!("unknown_method", responses);
    Ok(())
}

#[test]
fn snapshot_list_files() -> anyhow::Result<()> {
    let db = NamedTempFile::new()?;
    let root = tempfile::tempdir()?;
    let file_path = root.path().join("foo.txt");
    std::fs::write(&file_path, "hello")?;
    let req = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "hemis/list-files",
        "params": { "projectRoot": root.path().to_string_lossy() }
    })
    .to_string();
    let input = format!("Content-Length: {}\r\n\r\n{}", req.len(), req);
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
    let responses: Vec<Value> = bodies
        .into_iter()
        .map(|b| serde_json::from_slice(&b).unwrap())
        .collect();
    let redacted = responses
        .into_iter()
        .map(|mut v| {
            if let Some(arr) = v.get_mut("result").and_then(|r| r.as_array_mut()) {
                for item in arr.iter_mut() {
                    if let Some(path) = item.as_str() {
                        *item = serde_json::Value::String(
                            path.replace(root.path().to_string_lossy().as_ref(), "<root>"),
                        );
                    }
                }
            }
            v
        })
        .collect::<Vec<_>>();
    assert_json_snapshot!("list_files", redacted);
    Ok(())
}
