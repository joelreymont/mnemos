use std::collections::VecDeque;
use std::io::{self, Read, Write};
use std::str;

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

fn framed_needs_more(buf: &[u8]) -> bool {
    // If we see a Content-Length header but don't yet have the full body,
    // avoid falling back to newline parsing.
    const PREFIX: &[u8] = b"Content-Length:";
    if !buf.starts_with(PREFIX) {
        return false;
    }
    if let Some(split) = buf.windows(4).position(|w| w == b"\r\n\r\n") {
        if let Ok(header) = str::from_utf8(&buf[..split]) {
            if let Some(len) = header
                .lines()
                .find_map(|l| l.strip_prefix("Content-Length:"))
                .and_then(|v| v.trim().parse::<usize>().ok())
            {
                return buf.len() < split + 4 + len;
            }
        }
    }
    true
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
        let mut need_more = false;
        // Try framed message first.
        while let Some((body, consumed)) = decode_framed(buffer.make_contiguous()) {
            buffer.drain(..consumed);
            let resp = parse_and_handle(&body, &conn);
            write_response(&mut stdout, resp, true)?;
            handled = true;
        }
        if !handled && framed_needs_more(buffer.make_contiguous()) {
            need_more = true;
        }
        // If no framed message, fall back to a single line if present.
        if !need_more {
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
        }
        if handled {
            continue;
        }
        if need_more {
            let n = stdin.read(&mut read_buf)?;
            if n == 0 {
                break;
            }
            buffer.extend(&read_buf[..n]);
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
