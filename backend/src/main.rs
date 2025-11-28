use std::collections::VecDeque;
use std::io::{self, Read, Write};

use anyhow::Result;
use backend::parse_and_handle;
use rpc::{decode_framed, encode_response};
use storage::connect;

fn main() -> Result<()> {
    let db_path = std::env::var("HEMIS_DB_PATH").unwrap_or_else(|_| "hemis-notes.db".into());
    let conn = connect(&db_path)?;
    let mut stdin = Vec::new();
    io::stdin().read_to_end(&mut stdin)?;
    let mut out = Vec::new();
    let mut buffer: VecDeque<u8> = stdin.into();
    loop {
        // Try framed message first.
        if let Some((body, consumed)) = decode_framed(buffer.make_contiguous()) {
            buffer.drain(..consumed);
            let resp = parse_and_handle(&body, &conn);
            out.extend(encode_response(&resp));
            continue;
        }
        // If no framed message, fall back to a single line if present.
        if let Some(pos) = buffer.iter().position(|b| *b == b'\n') {
            let line: Vec<u8> = buffer.drain(..=pos).collect();
            let trimmed = line
                .iter()
                .filter(|b| **b != b'\n' && **b != b'\r')
                .cloned()
                .collect::<Vec<u8>>();
            if trimmed.is_empty() {
                continue;
            }
            let resp = parse_and_handle(&trimmed, &conn);
            out.extend(encode_response(&resp));
            out.push(b'\n');
            continue;
        }
        break;
    }
    if !out.is_empty() {
        io::stdout().write_all(&out)?;
    }
    Ok(())
}
