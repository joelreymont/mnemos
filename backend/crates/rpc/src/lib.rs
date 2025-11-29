//! rpc: JSON-RPC 2.0 framing and types.

use serde::{Deserialize, Serialize};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum RpcError {
    #[error("parse error: {0}")]
    Parse(String),
    #[error("method not found: {0}")]
    MethodNotFound(String),
    #[error("invalid params: {0}")]
    InvalidParams(String),
    #[error("internal error: {0}")]
    Internal(String),
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct Request {
    pub jsonrpc: String,
    pub id: Option<serde_json::Value>,
    pub method: String,
    #[serde(default)]
    pub params: serde_json::Value,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct Response {
    pub jsonrpc: String,
    pub id: Option<serde_json::Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result: Option<serde_json::Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<RpcErrorObj>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct RpcErrorObj {
    pub code: i64,
    pub message: String,
}

impl Response {
    pub fn result(id: Option<serde_json::Value>, result: serde_json::Value) -> Self {
        Self {
            jsonrpc: "2.0".into(),
            id,
            result: Some(result),
            error: None,
        }
    }

    pub fn error(id: Option<serde_json::Value>, code: i64, message: impl Into<String>) -> Self {
        Self {
            jsonrpc: "2.0".into(),
            id,
            result: None,
            error: Some(RpcErrorObj {
                code,
                message: message.into(),
            }),
        }
    }
}

pub const PARSE_ERROR: i64 = -32700;
pub const METHOD_NOT_FOUND: i64 = -32601;
pub const INVALID_PARAMS: i64 = -32602;
pub const INTERNAL_ERROR: i64 = -32000;

/// Decode a single JSON-RPC message from a buffer.
pub fn parse_request(bytes: &[u8]) -> Result<Request, RpcError> {
    serde_json::from_slice(bytes).map_err(|e| RpcError::Parse(e.to_string()))
}

/// Encode a response as JSON bytes.
pub fn encode_response(resp: &Response) -> Vec<u8> {
    serde_json::to_vec(resp).unwrap_or_default()
}

/// Simple Content-Length framing for stdin/stdout.
/// Decode a Content-Length framed message from BUF.
/// Returns (body, total_consumed_bytes).
pub fn decode_framed(buf: &[u8]) -> Option<(Vec<u8>, usize)> {
    let s = std::str::from_utf8(buf).ok()?;
    let mut parts = s.split("\r\n\r\n");
    let header = parts.next()?;
    let body = parts.next()?;
    let len = header
        .lines()
        .find_map(|l| l.strip_prefix("Content-Length:"))
        .and_then(|v| v.trim().parse::<usize>().ok())?;
    let header_len = header.len() + 4; // include separating CRLFCRLF
    let body_bytes = body.as_bytes();
    if body_bytes.len() >= len {
        Some((body_bytes[..len].to_vec(), header_len + len))
    } else {
        None
    }
}
