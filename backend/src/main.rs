//! Hemis backend - JSON-RPC server for code notes.
//!
//! Modes:
//! - Server mode (default): Listens on Unix domain socket ~/.hemis/hemis.sock
//! - Stdio mode: Reads from stdin, writes to stdout (for testing/debugging)
//!
//! Auto-detection:
//! - `--serve`: Force server mode
//! - `--stdio`: Force stdio mode
//! - No flags + TTY stdin: Server mode (user ran `hemis` in terminal)
//! - No flags + pipe stdin: Stdio mode (backward compat for existing integrations)

use std::collections::VecDeque;
use std::io::{self, Read, Write};
use std::path::PathBuf;
use std::str;

use anyhow::Result;
use backend::parse_and_handle;
use backend::server::Server;
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

/// Get the default database path (~/.hemis/hemis.db).
fn default_db_path() -> String {
    std::env::var("HEMIS_DB_PATH").unwrap_or_else(|_| {
        hemis_dir()
            .join("hemis.db")
            .to_string_lossy()
            .into_owned()
    })
}

/// Get the hemis directory (~/.hemis or HEMIS_DIR), creating it if needed.
fn hemis_dir() -> PathBuf {
    let dir = std::env::var("HEMIS_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|_| {
            dirs::home_dir()
                .map(|h| h.join(".hemis"))
                .unwrap_or_else(|| PathBuf::from(".hemis"))
        });
    let _ = std::fs::create_dir_all(&dir);
    dir
}

/// Run in stdio mode (reads from stdin, writes to stdout).
fn run_stdio_mode() -> Result<()> {
    let db_path = std::env::var("HEMIS_DB_PATH").unwrap_or_else(|_| default_db_path());
    let conn = connect(&db_path)?;
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

        // Try framed message first
        while let Some((body, consumed)) = decode_framed(buffer.make_contiguous()) {
            buffer.drain(..consumed);
            let resp = parse_and_handle(&body, &conn);
            write_response(&mut stdout, resp, true)?;
            handled = true;
        }

        if !handled && framed_needs_more(buffer.make_contiguous()) {
            need_more = true;
        }

        // Fall back to newline-delimited if no framed message
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

        // Need more data
        let n = stdin.read(&mut read_buf)?;
        if n == 0 {
            break;
        }
        buffer.extend(&read_buf[..n]);
    }

    Ok(())
}

/// Run in server mode (Unix domain socket).
fn run_server_mode() -> Result<()> {
    let db_path = default_db_path();
    let hdir = hemis_dir();

    let server = Server::new(hdir, db_path);
    server.run()
}

/// Determine which mode to run in.
#[derive(Debug, Clone, Copy, PartialEq)]
enum Mode {
    Server,
    Stdio,
}

fn detect_mode() -> Mode {
    let args: Vec<String> = std::env::args().collect();

    // Explicit flags take priority
    if args.contains(&"--serve".to_string()) || args.contains(&"-s".to_string()) {
        return Mode::Server;
    }
    if args.contains(&"--stdio".to_string()) {
        return Mode::Stdio;
    }

    // Auto-detect based on TTY
    // TTY = user ran `hemis` in terminal → server mode
    // Pipe = spawned by editor → stdio mode (backward compat)
    if atty::is(atty::Stream::Stdin) {
        Mode::Server
    } else {
        Mode::Stdio
    }
}

fn print_version() {
    println!(
        "hemis {} ({})",
        backend::version::PROTOCOL_VERSION,
        backend::version::GIT_HASH
    );
}

fn print_help() {
    println!("hemis - A second brain for your code");
    println!();
    println!("USAGE:");
    println!("    hemis [OPTIONS]");
    println!();
    println!("OPTIONS:");
    println!("    --serve, -s    Run as server (Unix socket at ~/.hemis/hemis.sock)");
    println!("    --stdio        Run in stdio mode (for testing/debugging)");
    println!("    --version, -v  Print version information");
    println!("    --help, -h     Print this help message");
    println!();
    println!("ENVIRONMENT:");
    println!("    HEMIS_DB_PATH  Override database path (default: ~/.hemis/hemis.db)");
    println!();
    println!("Without flags, auto-detects mode:");
    println!("    - TTY stdin → server mode");
    println!("    - Pipe stdin → stdio mode");
}

fn main() -> Result<()> {
    let args: Vec<String> = std::env::args().collect();

    if args.contains(&"--version".to_string()) || args.contains(&"-v".to_string()) {
        print_version();
        return Ok(());
    }

    if args.contains(&"--help".to_string()) || args.contains(&"-h".to_string()) {
        print_help();
        return Ok(());
    }

    match detect_mode() {
        Mode::Server => run_server_mode(),
        Mode::Stdio => run_stdio_mode(),
    }
}
