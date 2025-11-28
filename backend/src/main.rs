use std::collections::VecDeque;
use std::io::{self, Read, Write};

use anyhow::Result;
use backend::parse_and_handle;
use rpc::{decode_framed, encode_response, Response};
use storage::connect;

fn write_response<W: Write>(out: &mut W, resp: Response, framed: bool) -> io::Result<()> {
    let bytes = encode_response(&resp);
    if framed {
        write!(out, "Content-Length: {}\r\n\r\n", bytes.len())?;
        out.write_all(&bytes)?;
    } else {
        out.write_all(&bytes)?;
        out.write_all(b"\n")?;
    }
    out.flush()
}

fn main() -> Result<()> {
    let db_path = std::env::var("HEMIS_DB_PATH").unwrap_or_else(|_| "hemis-notes.db".into());
    let conn = connect(&db_path)?;
    // Preload tables to warm cache and ensure schema is ready.
    backend::preload::preload(&conn)?;
    let mut stdin = io::stdin().lock();
    let mut stdout = io::stdout().lock();
    let mut buffer: VecDeque<u8> = VecDeque::new();
    let mut read_buf = [0u8; 8192];
    loop {
        if buffer.is_empty() {
            let n = stdin.read(&mut read_buf)?;
            if n == 0 {
                break;
            }
            buffer.extend(&read_buf[..n]);
        }
        let mut handled = false;
        // Try framed message first.
        while let Some((body, consumed)) = decode_framed(buffer.make_contiguous()) {
            buffer.drain(..consumed);
            let resp = parse_and_handle(&body, &conn);
            write_response(&mut stdout, resp, true)?;
            handled = true;
        }
        // If no framed message, fall back to a single line if present.
        if let Some(pos) = buffer.iter().position(|b| *b == b'\n') {
            let line: Vec<u8> = buffer.drain(..=pos).collect();
            let trimmed = line
                .iter()
                .filter(|b| **b != b'\n' && **b != b'\r')
                .cloned()
                .collect::<Vec<u8>>();
            if !trimmed.is_empty() {
                let resp = parse_and_handle(&trimmed, &conn);
                write_response(&mut stdout, resp, false)?;
            }
            handled = true;
        }
        if handled {
            continue;
        }
        // Need more data; read again.
        let n = stdin.read(&mut read_buf)?;
        if n == 0 {
            break;
        }
        buffer.extend(&read_buf[..n]);
    }
    Ok(())
}
