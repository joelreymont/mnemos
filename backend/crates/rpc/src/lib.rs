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

    /// Create a result response by serializing a value. Returns an error response on serialization failure.
    pub fn result_from<T: serde::Serialize>(id: Option<serde_json::Value>, value: T) -> Self {
        match serde_json::to_value(value) {
            Ok(v) => Self::result(id, v),
            Err(e) => Self::error(id, -32603, format!("failed to serialize response: {}", e)),
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

/// Maximum message size (100MB - prevents memory exhaustion attacks)
pub const MAX_MESSAGE_SIZE: usize = 100 * 1024 * 1024;

/// Maximum header size (8KB - prevents header parsing attacks)
const MAX_HEADER_SIZE: usize = 8 * 1024;

/// Find the position of `\r\n\r\n` in the buffer (returns position of first \r).
#[inline]
fn find_header_end(buf: &[u8]) -> Option<usize> {
    // Search pattern: \r\n\r\n (bytes: 13, 10, 13, 10)
    let needle = b"\r\n\r\n";
    buf.windows(4)
        .position(|w| w == needle)
}

/// Parse Content-Length from header bytes without full UTF-8 conversion.
/// Only converts the Content-Length line to string for parsing.
fn parse_content_length(header: &[u8]) -> Option<usize> {
    // Cap header size to prevent scanning huge buffers
    if header.len() > MAX_HEADER_SIZE {
        return None;
    }

    // Search for "Content-Length:" (case-sensitive per HTTP spec)
    const PREFIX: &[u8] = b"Content-Length:";

    // Find the line containing Content-Length
    let mut pos = 0;
    while pos < header.len() {
        // Check if this line starts with Content-Length:
        if header[pos..].starts_with(PREFIX) {
            // Find end of line
            let value_start = pos + PREFIX.len();
            let line_end = header[value_start..]
                .iter()
                .position(|&b| b == b'\r' || b == b'\n')
                .map(|p| value_start + p)
                .unwrap_or(header.len());

            // Parse the value (trim whitespace, convert to string for parsing)
            let value_bytes = &header[value_start..line_end];
            let value_str = std::str::from_utf8(value_bytes).ok()?;
            return value_str.trim().parse().ok();
        }

        // Move to next line
        match header[pos..].iter().position(|&b| b == b'\n') {
            Some(nl) => pos += nl + 1,
            None => break,
        }
    }
    None
}

/// Simple Content-Length framing for stdin/stdout.
/// Decode a Content-Length framed message from BUF.
/// Returns (body, total_consumed_bytes) or None if incomplete/invalid.
///
/// Optimized to avoid full UTF-8 conversion of buffer - only parses header
/// bytes directly and returns body slice as Vec.
pub fn decode_framed(buf: &[u8]) -> Option<(Vec<u8>, usize)> {
    // Find header/body separator without UTF-8 conversion
    let header_end = find_header_end(buf)?;

    // Cap header size early
    if header_end > MAX_HEADER_SIZE {
        return None;
    }

    let header = &buf[..header_end];
    let body_start = header_end + 4; // skip \r\n\r\n

    // Parse Content-Length from header bytes
    let content_length = parse_content_length(header)?;

    // Reject oversized messages to prevent DoS
    if content_length > MAX_MESSAGE_SIZE {
        return None;
    }

    // Check if we have the full body
    let body_end = body_start + content_length;
    if buf.len() < body_end {
        return None; // Incomplete message
    }

    // Return body as Vec (required for ownership)
    Some((buf[body_start..body_end].to_vec(), body_end))
}
