//! Unix domain socket server mode for the Hemis backend.
//!
//! The server:
//! - Listens on ~/.hemis/hemis.sock
//! - Tracks connected clients with reference counting
//! - Shuts down after 30 seconds with no connections
//! - Supports the hemis/version endpoint for version checking

use std::fs;
use std::io::{Read, Write};
use std::os::unix::net::{UnixListener, UnixStream};
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};

use anyhow::Result;
use rpc::{decode_framed, encode_response, Response};
use serde_json::json;
use storage::connect;

use crate::events;
use crate::version::{GIT_HASH, PROTOCOL_VERSION, VersionInfo};
use crate::{create_parser_service, parse_and_handle, preload};

/// Grace period before shutdown when no clients are connected.
const SHUTDOWN_GRACE_SECS: u64 = 30;

/// Shared server state.
pub struct Server {
    socket_path: PathBuf,
    events_socket_path: PathBuf,
    lock_path: PathBuf,
    db_path: String,
    connections: Arc<AtomicUsize>,
    start_time: Instant,
    shutdown_scheduled: Arc<AtomicBool>,
}

impl Server {
    /// Create a new server instance.
    pub fn new(hemis_dir: PathBuf, db_path: String) -> Self {
        Self {
            socket_path: hemis_dir.join("hemis.sock"),
            events_socket_path: hemis_dir.join("events.sock"),
            lock_path: hemis_dir.join("hemis.lock"),
            db_path,
            connections: Arc::new(AtomicUsize::new(0)),
            start_time: Instant::now(),
            shutdown_scheduled: Arc::new(AtomicBool::new(false)),
        }
    }

    /// Run the server, listening for connections.
    pub fn run(&self) -> Result<()> {
        // Clean up any stale socket
        if self.socket_path.exists() {
            fs::remove_file(&self.socket_path)?;
        }

        // Start the events socket server
        events::start_event_server(self.events_socket_path.clone());

        // Create the socket
        let listener = UnixListener::bind(&self.socket_path)?;
        eprintln!("Hemis server listening on {}", self.socket_path.display());

        // Write PID to lock file
        fs::write(
            &self.lock_path,
            format!(
                "{}\n{}",
                std::process::id(),
                std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap()
                    .as_secs()
            ),
        )?;

        // Accept connections
        for stream in listener.incoming() {
            match stream {
                Ok(stream) => self.handle_client(stream),
                Err(e) => eprintln!("Connection failed: {}", e),
            }
        }

        Ok(())
    }

    /// Handle a client connection in a new thread.
    fn handle_client(&self, stream: UnixStream) {
        let count = self.connections.fetch_add(1, Ordering::SeqCst) + 1;
        eprintln!("Client connected ({} total)", count);

        // Cancel any pending shutdown
        self.shutdown_scheduled.store(false, Ordering::SeqCst);

        let db_path = self.db_path.clone();
        let connections = self.connections.clone();
        let shutdown_scheduled = self.shutdown_scheduled.clone();
        let start_time = self.start_time;
        let socket_path = self.socket_path.clone();
        let lock_path = self.lock_path.clone();

        std::thread::spawn(move || {
            // Each thread gets its own DB connection
            let conn = match connect(&db_path) {
                Ok(c) => c,
                Err(e) => {
                    eprintln!("Failed to connect to DB: {}", e);
                    connections.fetch_sub(1, Ordering::SeqCst);
                    return;
                }
            };

            // Each thread gets its own ParserService (for tree caching)
            let mut parser = create_parser_service();

            // Verify schema is accessible
            if let Err(e) = preload::sanity_check(&conn) {
                eprintln!("Schema sanity check failed: {}", e);
            }

            if let Err(e) = handle_connection(stream, &conn, &mut parser, start_time, &connections) {
                // Don't log "broken pipe" errors - that's just client disconnect
                let err_str = e.to_string();
                if !err_str.contains("Broken pipe") && !err_str.contains("Connection reset") {
                    eprintln!("Connection error: {}", e);
                }
            }

            let remaining = connections.fetch_sub(1, Ordering::SeqCst) - 1;
            eprintln!("Client disconnected ({} remaining)", remaining);

            if remaining == 0 {
                schedule_shutdown_check(connections, shutdown_scheduled, socket_path, lock_path);
            }
        });
    }
}

/// Handle a single client connection.
fn handle_connection(
    mut stream: UnixStream,
    conn: &rusqlite::Connection,
    parser: &mut treesitter::ParserService,
    start_time: Instant,
    connections: &Arc<AtomicUsize>,
) -> Result<()> {
    let mut buffer = Vec::new();
    let mut read_buf = [0u8; 8192];
    // Max buffer size: MAX_MESSAGE_SIZE + 1KB for headers
    let max_buffer_size = rpc::MAX_MESSAGE_SIZE + 1024;

    loop {
        match stream.read(&mut read_buf) {
            Ok(0) => break, // Client disconnected
            Ok(n) => {
                buffer.extend_from_slice(&read_buf[..n]);

                // Prevent unbounded buffer growth (DoS protection)
                if buffer.len() > max_buffer_size {
                    return Err(anyhow::anyhow!("client buffer too large"));
                }

                // Process complete messages
                while let Some((body, consumed)) = decode_framed(&buffer) {
                    buffer.drain(..consumed);
                    let response = handle_request(&body, conn, parser, start_time, connections);
                    write_response(&mut stream, &response)?;
                }
            }
            Err(e) if e.kind() == std::io::ErrorKind::WouldBlock => continue,
            Err(e) => return Err(e.into()),
        }
    }

    Ok(())
}

/// Handle a single request, including server-specific methods.
fn handle_request(
    body: &[u8],
    conn: &rusqlite::Connection,
    parser: &mut treesitter::ParserService,
    start_time: Instant,
    connections: &Arc<AtomicUsize>,
) -> Response {
    // Try to parse to check for server-specific methods
    if let Ok(req) = rpc::parse_request(body) {
        match req.method.as_str() {
            "hemis/version" => {
                let info = VersionInfo {
                    protocol_version: PROTOCOL_VERSION,
                    git_hash: GIT_HASH.to_string(),
                    uptime_secs: start_time.elapsed().as_secs(),
                    connections: connections.load(Ordering::SeqCst),
                };
                return Response::result_from(req.id, info);
            }
            "shutdown" => {
                // Schedule shutdown after response is sent
                std::thread::spawn(|| {
                    std::thread::sleep(Duration::from_millis(100));
                    eprintln!("Shutdown requested, exiting...");
                    std::process::exit(0);
                });
                return Response::result(req.id, json!({"status": "shutting_down"}));
            }
            _ => {}
        }
    }

    // Delegate to standard handler
    parse_and_handle(body, conn, parser)
}

/// Write a framed response to the stream.
fn write_response<W: Write>(out: &mut W, resp: &Response) -> std::io::Result<()> {
    let bytes = encode_response(resp);
    write!(out, "Content-Length: {}\r\n\r\n", bytes.len())?;
    out.write_all(&bytes)?;
    out.flush()
}

/// Schedule a shutdown check after the grace period.
fn schedule_shutdown_check(
    connections: Arc<AtomicUsize>,
    shutdown_scheduled: Arc<AtomicBool>,
    socket_path: PathBuf,
    lock_path: PathBuf,
) {
    if shutdown_scheduled.swap(true, Ordering::SeqCst) {
        // Already scheduled
        return;
    }

    std::thread::spawn(move || {
        eprintln!(
            "No clients connected, scheduling shutdown in {}s",
            SHUTDOWN_GRACE_SECS
        );
        std::thread::sleep(Duration::from_secs(SHUTDOWN_GRACE_SECS));

        if connections.load(Ordering::SeqCst) == 0 {
            eprintln!("No clients for {}s, shutting down", SHUTDOWN_GRACE_SECS);
            cleanup_and_exit(socket_path, lock_path);
        } else {
            eprintln!("Clients reconnected, cancelling shutdown");
            shutdown_scheduled.store(false, Ordering::SeqCst);
        }
    });
}

/// Clean up socket and lock files, then exit.
fn cleanup_and_exit(socket_path: PathBuf, lock_path: PathBuf) {
    let _ = fs::remove_file(&socket_path);
    let _ = fs::remove_file(&lock_path);
    // Also clean up events socket (in same directory)
    if let Some(parent) = socket_path.parent() {
        let events_path = parent.join("events.sock");
        events::cleanup_event_socket(&events_path);
    }
    std::process::exit(0);
}
