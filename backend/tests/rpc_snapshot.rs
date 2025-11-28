use assert_cmd::Command;
use insta::assert_json_snapshot;
use rpc::decode_framed;
use serde_json::{self, json, Value};
use tempfile::NamedTempFile;

fn scrub_note(note: &mut serde_json::Map<String, Value>) {
    note.insert("id".into(), json!("<id>"));
    note.insert("createdAt".into(), json!("<ts>"));
    note.insert("updatedAt".into(), json!("<ts>"));
}

fn scrub_response(mut resp: Value) -> Value {
    if let Some(result) = resp.get_mut("result") {
        match result {
            Value::Object(obj) => scrub_note(obj),
            Value::Array(arr) => {
                for val in arr {
                    if let Some(obj) = val.as_object_mut() {
                        scrub_note(obj);
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
    let assert = Command::cargo_bin("backend")?
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
