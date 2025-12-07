//! Hemis backend - JSON-RPC server for code notes.
//!
//! Modes:
//! - Server mode (default): Listens on Unix domain socket ~/.hemis/hemis.sock
//! - Stdio mode: Reads from stdin, writes to stdout (for testing/debugging)
//!
//! Subcommands:
//! - `grammar list`: List available grammars (bundled and user-installed)
//! - `grammar fetch <name>`: Fetch a grammar source from git
//! - `grammar build <name>`: Build a grammar from source
//!
//! Auto-detection:
//! - `--serve`: Force server mode
//! - `--stdio`: Force stdio mode
//! - No flags + TTY stdin: Server mode (user ran `hemis` in terminal)
//! - No flags + pipe stdin: Stdio mode (backward compat for existing integrations)

use std::collections::VecDeque;
use std::io::{self, Read, Write};
use std::path::PathBuf;
use std::process::Command;
use std::str;

use anyhow::{anyhow, Result};
use backend::config::{CliOverrides, ResolvedConfig};
use backend::{create_parser_service, parse_and_handle, snapshot};
use backend::server::Server;
use rpc::{decode_framed, encode_response, Response};
use storage::connect;
use treesitter::{load_config, GrammarSource, GrammarSourceLocation};

/// Validate a git URL to prevent command injection.
/// Only allows https:// and git:// protocols.
fn validate_git_url(url: &str) -> Result<()> {
    // Must start with safe protocol
    if !url.starts_with("https://") && !url.starts_with("git://") {
        return Err(anyhow!("Only https:// and git:// URLs are allowed"));
    }
    // Basic validation: no shell metacharacters
    if url.contains(|c: char| c == ';' || c == '|' || c == '&' || c == '$' || c == '`' || c == '\n' || c == '\r') {
        return Err(anyhow!("Git URL contains invalid characters"));
    }
    Ok(())
}

/// Validate a git branch/tag name to prevent command injection.
fn validate_git_ref(ref_name: &str) -> Result<()> {
    // Git ref names: alphanumeric, dash, underscore, slash, dot
    // Also reject refs starting with - to prevent option injection
    if ref_name.starts_with('-') {
        return Err(anyhow!("Git ref cannot start with '-'"));
    }
    if !ref_name.chars().all(|c| c.is_alphanumeric() || c == '-' || c == '_' || c == '/' || c == '.') {
        return Err(anyhow!("Git ref contains invalid characters"));
    }
    if ref_name.is_empty() || ref_name.len() > 256 {
        return Err(anyhow!("Git ref must be 1-256 characters"));
    }
    // Prevent path traversal sequences
    if ref_name.contains("..") || ref_name.contains("//") {
        return Err(anyhow!("Git ref cannot contain '..' or '//'"));
    }
    Ok(())
}

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

/// Get the hemis data directory (~/.hemis or HEMIS_DIR), creating it if needed.
fn hemis_dir() -> PathBuf {
    let dir = backend::config::data_dir();
    let _ = std::fs::create_dir_all(&dir);
    dir
}

/// Run in stdio mode (reads from stdin, writes to stdout).
fn run_stdio_mode(config: &ResolvedConfig) -> Result<()> {
    let conn = connect(&config.db_path)?;
    backend::preload::sanity_check(&conn)?;

    // Initialize job queue for async index-project (sync=false)
    backend::jobs::init_job_queue(config.db_path.clone());

    let mut parser = create_parser_service();
    let mut stdin = io::stdin().lock();
    let mut stdout = io::stdout().lock();
    let mut buffer: VecDeque<u8> = VecDeque::new();
    let mut read_buf = [0u8; 8192];

    loop {
        // Prevent unbounded buffer growth (limit to MAX_MESSAGE_SIZE + 1KB for headers)
        if buffer.len() > rpc::MAX_MESSAGE_SIZE + 1024 {
            return Err(anyhow::anyhow!("input buffer too large, possible DoS attempt"));
        }

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
            let resp = parse_and_handle(&body, &conn, &mut parser);
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
                    let resp = parse_and_handle(&trimmed, &conn, &mut parser);
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
fn run_server_mode(config: &ResolvedConfig) -> Result<()> {
    let hdir = hemis_dir();
    let server = Server::new(hdir, config.db_path.clone());
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

/// List of bundled grammars that are compiled into the binary
const BUNDLED_GRAMMARS: &[&str] = &[
    "rust", "python", "javascript", "typescript", "go", "lua", "c", "cpp", "java",
];

/// Run the grammar subcommand
fn run_grammar_command(args: &[String]) -> Result<()> {
    if args.is_empty() {
        print_grammar_help();
        return Ok(());
    }

    match args[0].as_str() {
        "list" => grammar_list(),
        "fetch" => {
            if args.len() < 2 {
                eprintln!("Usage: hemis grammar fetch <name>");
                eprintln!("       hemis grammar fetch --all");
                std::process::exit(1);
            }
            if args[1] == "--all" {
                grammar_fetch_all()
            } else {
                grammar_fetch(&args[1])
            }
        }
        "build" => {
            if args.len() < 2 {
                eprintln!("Usage: hemis grammar build <name>");
                eprintln!("       hemis grammar build --all");
                std::process::exit(1);
            }
            if args[1] == "--all" {
                grammar_build_all()
            } else {
                grammar_build(&args[1])
            }
        }
        "help" | "--help" | "-h" => {
            print_grammar_help();
            Ok(())
        }
        _ => {
            eprintln!("Unknown grammar subcommand: {}", args[0]);
            print_grammar_help();
            std::process::exit(1);
        }
    }
}

fn print_grammar_help() {
    println!("hemis grammar - Manage tree-sitter grammars");
    println!();
    println!("USAGE:");
    println!("    hemis grammar <COMMAND>");
    println!();
    println!("COMMANDS:");
    println!("    list              List available grammars (bundled and user-installed)");
    println!("    fetch <name>      Fetch grammar source from git");
    println!("    fetch --all       Fetch all grammars defined in languages.toml");
    println!("    build <name>      Build a grammar from source");
    println!("    build --all       Build all fetched grammars");
    println!("    help              Show this help message");
    println!();
    println!("CONFIGURATION:");
    println!("    Grammars are configured in ~/.config/hemis/languages.toml");
    println!("    Fetched sources are stored in ~/.config/hemis/grammars/sources/");
    println!("    Built libraries are stored in ~/.config/hemis/grammars/");
}

/// List available grammars
fn grammar_list() -> Result<()> {
    let config = load_config()?;
    let grammars_dir = config.grammars_dir();

    println!("Bundled grammars (always available):");
    for name in BUNDLED_GRAMMARS {
        println!("  {} (built-in)", name);
    }
    println!();

    // Check for user-installed grammars
    let mut user_grammars = Vec::new();
    if grammars_dir.exists() {
        for entry in std::fs::read_dir(&grammars_dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_file() {
                if let Some(ext) = path.extension() {
                    if ext == "so" || ext == "dylib" {
                        if let Some(name) = path.file_stem() {
                            let name = name.to_string_lossy();
                            // Strip "libtree-sitter-" prefix if present
                            let name = name
                                .strip_prefix("libtree-sitter-")
                                .or_else(|| name.strip_prefix("tree-sitter-"))
                                .unwrap_or(&name);
                            user_grammars.push(name.to_string());
                        }
                    }
                }
            }
        }
    }

    if !user_grammars.is_empty() {
        println!("User-installed grammars:");
        for name in &user_grammars {
            println!("  {} ({})", name, grammars_dir.display());
        }
        println!();
    }

    // Show configured grammar sources
    if !config.grammars.is_empty() {
        println!("Configured grammar sources (from languages.toml):");
        for grammar in &config.grammars {
            let status = if user_grammars.contains(&grammar.name) {
                "installed"
            } else {
                "not built"
            };
            println!("  {} [{}]", grammar.name, status);
            if let Some(ref git) = grammar.source.git {
                println!("    git: {}", git);
            }
        }
    }

    Ok(())
}

/// Fetch a grammar source from git
fn grammar_fetch(name: &str) -> Result<()> {
    let config = load_config()?;

    // Find grammar in config
    let grammar = config.grammars.iter().find(|g| g.name == name);
    let grammar = match grammar {
        Some(g) => g,
        None => {
            // Check if it's a bundled grammar
            if BUNDLED_GRAMMARS.contains(&name) {
                println!("'{}' is a bundled grammar (no fetch needed)", name);
                return Ok(());
            }

            // Try to use default tree-sitter grammar URL
            println!("Grammar '{}' not in config, trying default source...", name);
            &GrammarSource {
                name: name.to_string(),
                source: GrammarSourceLocation {
                    git: Some(format!(
                        "https://github.com/tree-sitter/tree-sitter-{}",
                        name
                    )),
                    rev: None,
                    path: None,
                },
            }
        }
    };

    let git_url = match &grammar.source.git {
        Some(url) => url,
        None => {
            eprintln!("Grammar '{}' has no git source configured", name);
            return Ok(());
        }
    };

    // Validate git URL and ref to prevent command injection
    validate_git_url(git_url)?;
    if let Some(ref rev) = grammar.source.rev {
        validate_git_ref(rev)?;
    }

    let sources_dir = config.grammars_dir().join("sources");
    std::fs::create_dir_all(&sources_dir)?;

    let target_dir = sources_dir.join(name);

    if target_dir.exists() {
        println!("Updating {}...", name);
        let status = Command::new("git")
            .args(["pull", "--ff-only"])
            .current_dir(&target_dir)
            .status()?;
        if !status.success() {
            eprintln!("Warning: git pull failed, trying fresh clone...");
            std::fs::remove_dir_all(&target_dir)?;
        } else {
            println!("Updated {}", name);
            return Ok(());
        }
    }

    println!("Fetching {} from {}...", name, git_url);
    let mut cmd = Command::new("git");
    cmd.args(["clone", "--depth", "1"]);
    if let Some(ref rev) = grammar.source.rev {
        cmd.args(["--branch", rev]);
    }
    cmd.arg(git_url).arg(&target_dir);

    let status = cmd.status()?;
    if !status.success() {
        anyhow::bail!("git clone failed for {}", name);
    }

    println!("Fetched {} to {}", name, target_dir.display());
    Ok(())
}

/// Fetch all configured grammars
fn grammar_fetch_all() -> Result<()> {
    let config = load_config()?;

    if config.grammars.is_empty() {
        println!("No grammars configured in languages.toml");
        println!();
        println!("Add grammar sources like:");
        println!("  [[grammar]]");
        println!("  name = \"zig\"");
        println!("  source = {{ git = \"https://github.com/tree-sitter-grammars/tree-sitter-zig\" }}");
        return Ok(());
    }

    for grammar in &config.grammars {
        if let Err(e) = grammar_fetch(&grammar.name) {
            eprintln!("Error fetching {}: {}", grammar.name, e);
        }
    }

    Ok(())
}

/// Build a grammar from source
fn grammar_build(name: &str) -> Result<()> {
    let config = load_config()?;

    // Check if it's bundled
    if BUNDLED_GRAMMARS.contains(&name) {
        println!("'{}' is a bundled grammar (no build needed)", name);
        return Ok(());
    }

    let sources_dir = config.grammars_dir().join("sources");
    let source_dir = sources_dir.join(name);

    if !source_dir.exists() {
        anyhow::bail!(
            "Source not found for '{}'. Run 'hemis grammar fetch {}' first.",
            name,
            name
        );
    }

    // Find the src directory (may be at root or in a subdirectory)
    let src_dir = if source_dir.join("src").exists() {
        source_dir.join("src")
    } else if source_dir.join("grammar.js").exists() {
        // Need to generate parser first
        println!("Generating parser for {}...", name);
        let status = Command::new("npx")
            .args(["tree-sitter", "generate"])
            .current_dir(&source_dir)
            .status();

        match status {
            Ok(s) if s.success() => source_dir.join("src"),
            _ => {
                anyhow::bail!(
                    "Failed to generate parser. Make sure tree-sitter-cli is installed: npm install -g tree-sitter-cli"
                );
            }
        }
    } else {
        anyhow::bail!("Cannot find src directory or grammar.js in {}", source_dir.display());
    };

    let parser_c = src_dir.join("parser.c");
    if !parser_c.exists() {
        anyhow::bail!("parser.c not found in {}", src_dir.display());
    }

    // Determine output filename based on platform
    let lib_ext = if cfg!(target_os = "macos") {
        "dylib"
    } else {
        "so"
    };
    let output_name = format!("libtree-sitter-{}.{}", name, lib_ext);
    let output_path = config.grammars_dir().join(&output_name);

    std::fs::create_dir_all(config.grammars_dir())?;

    println!("Building {}...", name);

    // Compile parser.c (and scanner.c if present)
    let scanner_c = src_dir.join("scanner.c");
    let scanner_cc = src_dir.join("scanner.cc");

    let mut cc_args = vec![
        "-shared".to_string(),
        "-fPIC".to_string(),
        "-O2".to_string(),
        "-I".to_string(),
        src_dir.to_string_lossy().to_string(),
        parser_c.to_string_lossy().to_string(),
        "-o".to_string(),
        output_path.to_string_lossy().to_string(),
    ];

    // Add scanner if present
    if scanner_c.exists() {
        cc_args.insert(4, scanner_c.to_string_lossy().to_string());
    }

    // Use cc for C files
    let status = Command::new("cc").args(&cc_args).status()?;

    if !status.success() {
        // If C++ scanner, try g++
        if scanner_cc.exists() {
            let mut cxx_args = vec![
                "-shared".to_string(),
                "-fPIC".to_string(),
                "-O2".to_string(),
                "-I".to_string(),
                src_dir.to_string_lossy().to_string(),
                parser_c.to_string_lossy().to_string(),
                scanner_cc.to_string_lossy().to_string(),
                "-o".to_string(),
                output_path.to_string_lossy().to_string(),
            ];

            // Add C++ stdlib on macOS
            if cfg!(target_os = "macos") {
                cxx_args.push("-lc++".to_string());
            } else {
                cxx_args.push("-lstdc++".to_string());
            }

            let status = Command::new("c++").args(&cxx_args).status()?;
            if !status.success() {
                anyhow::bail!("Compilation failed for {}", name);
            }
        } else {
            anyhow::bail!("Compilation failed for {}", name);
        }
    }

    println!("Built {} -> {}", name, output_path.display());
    Ok(())
}

/// Build all fetched grammars
fn grammar_build_all() -> Result<()> {
    let config = load_config()?;
    let sources_dir = config.grammars_dir().join("sources");

    if !sources_dir.exists() {
        println!("No grammar sources found. Run 'hemis grammar fetch --all' first.");
        return Ok(());
    }

    let mut built = 0;
    for entry in std::fs::read_dir(&sources_dir)? {
        let entry = entry?;
        if entry.path().is_dir() {
            let name = entry.file_name().to_string_lossy().to_string();
            match grammar_build(&name) {
                Ok(()) => built += 1,
                Err(e) => eprintln!("Error building {}: {}", name, e),
            }
        }
    }

    println!();
    println!("Built {} grammars", built);
    Ok(())
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
    println!("    hemis ensure-daemon");
    println!("    hemis grammar <COMMAND>");
    println!("    hemis oracle <QUESTION>");
    println!();
    println!("SUBCOMMANDS:");
    println!("    ensure-daemon  Ensure daemon is running, output JSON with socket path");
    println!("    grammar        Manage tree-sitter grammars (list, fetch, build)");
    println!("    oracle         Ask codex -m o3 for help with difficult questions");
    println!();
    println!("OPTIONS:");
    println!("    --serve, -s            Run as server (Unix socket at ~/.hemis/hemis.sock)");
    println!("    --stdio                Run in stdio mode (for testing/debugging)");
    println!("    --config <PATH>        Path to config file (default: ~/.config/hemis/config.toml)");
    println!("    --db-path <PATH>       Path to SQLite database (overrides config/env)");
    println!("    --ai-provider <NAME>   AI provider: codex, claude, none (overrides config/env)");
    println!("    --save-snapshot <PATH> Save database snapshot to file and exit");
    println!("    --load-snapshot <PATH> Load database snapshot from file and exit");
    println!("    --version, -v          Print version information");
    println!("    --help, -h             Print this help message");
    println!();
    println!("CONFIG FILE:");
    println!("    db-path = \"/path/to/hemis.db\"");
    println!("    ai-provider = \"claude\"  # or \"codex\", \"none\"");
    println!();
    println!("PRECEDENCE: CLI flags > environment variables > config file > defaults");
    println!();
    println!("Without flags, auto-detects mode:");
    println!("    - TTY stdin → server mode");
    println!("    - Pipe stdin → stdio mode");
}

/// Parse a value argument (--flag value or --flag=value)
fn parse_arg_value<'a>(args: &'a [String], flag: &str) -> Option<&'a str> {
    for (i, arg) in args.iter().enumerate() {
        // Handle --flag=value format
        if let Some(value) = arg.strip_prefix(&format!("{}=", flag)) {
            return Some(value);
        }
        // Handle --flag value format
        if arg == flag {
            if let Some(value) = args.get(i + 1) {
                if !value.starts_with('-') {
                    return Some(value);
                }
            }
        }
    }
    None
}

/// Save snapshot to file
fn run_save_snapshot(config: &ResolvedConfig, path: &str) -> Result<()> {
    let conn = connect(&config.db_path)?;

    // Create snapshot (no project root filter - save all data)
    let payload = snapshot::create(&conn, None)?;

    let json = serde_json::to_string_pretty(&payload)
        .map_err(|e| anyhow!("failed to serialize snapshot: {}", e))?;

    std::fs::write(path, json)
        .map_err(|e| anyhow!("failed to write snapshot file: {}", e))?;

    if let Some(counts) = payload.get("counts") {
        println!("Snapshot saved to {}", path);
        println!("  Notes: {}", counts.get("notes").and_then(|v| v.as_u64()).unwrap_or(0));
        println!("  Files: {}", counts.get("files").and_then(|v| v.as_u64()).unwrap_or(0));
        println!("  Embeddings: {}", counts.get("embeddings").and_then(|v| v.as_u64()).unwrap_or(0));
        println!("  Edges: {}", counts.get("edges").and_then(|v| v.as_u64()).unwrap_or(0));
    } else {
        println!("Snapshot saved to {}", path);
    }

    Ok(())
}

/// Load snapshot from file
fn run_load_snapshot(config: &ResolvedConfig, path: &str) -> Result<()> {
    // Size limit for snapshot files (10MB)
    const MAX_SNAPSHOT_SIZE: u64 = 10 * 1024 * 1024;

    let meta = std::fs::metadata(path)
        .map_err(|e| anyhow!("failed to access snapshot file: {}", e))?;

    if meta.len() > MAX_SNAPSHOT_SIZE {
        return Err(anyhow!("snapshot file too large: {} bytes (max {})", meta.len(), MAX_SNAPSHOT_SIZE));
    }

    let content = std::fs::read_to_string(path)
        .map_err(|e| anyhow!("failed to read snapshot file: {}", e))?;

    let val: serde_json::Value = serde_json::from_str(&content)
        .map_err(|e| anyhow!("invalid snapshot JSON: {}", e))?;

    let conn = connect(&config.db_path)?;

    let result = snapshot::restore(&conn, &val)
        .map_err(|e| anyhow!("snapshot restore failed: {}", e))?;

    println!("Snapshot loaded from {}", path);
    if let Some(counts) = result.get("counts") {
        println!("  Notes: {}", counts.get("notes").and_then(|v| v.as_u64()).unwrap_or(0));
        println!("  Files: {}", counts.get("files").and_then(|v| v.as_u64()).unwrap_or(0));
        println!("  Embeddings: {}", counts.get("embeddings").and_then(|v| v.as_u64()).unwrap_or(0));
        println!("  Edges: {}", counts.get("edges").and_then(|v| v.as_u64()).unwrap_or(0));
    }

    Ok(())
}

/// Run the oracle command (ask codex -m o3)
fn run_oracle_command(args: &[String]) -> Result<()> {
    if args.is_empty() {
        println!("Usage: hemis oracle <QUESTION>");
        println!();
        println!("Ask codex -m o3 (the oracle) for help with difficult questions.");
        println!();
        println!("Examples:");
        println!("    hemis oracle \"How do I implement a trie in Rust?\"");
        println!("    hemis oracle \"What's the best way to handle async errors?\"");
        return Ok(());
    }

    // Join all arguments as the question
    let question = args.join(" ");
    println!("Asking the oracle...");
    println!();

    let output = Command::new("codex")
        .args(["-m", "o3", "-p", &question])
        .output()?;

    if output.status.success() {
        let response = String::from_utf8_lossy(&output.stdout);
        println!("{}", response.trim());
    } else {
        let stderr = String::from_utf8_lossy(&output.stderr);
        eprintln!("Oracle error: {}", stderr.trim());
        std::process::exit(1);
    }

    Ok(())
}

/// Ensure daemon is running and return connection info.
/// This is the entry point for clients to discover/start the daemon.
fn run_ensure_daemon() -> Result<()> {
    use std::os::unix::net::UnixStream;
    use std::time::Duration;

    let hemis_dir = dirs::home_dir()
        .map(|h| h.join(".hemis"))
        .unwrap_or_else(|| PathBuf::from(".hemis"));
    let socket_path = hemis_dir.join("hemis.sock");
    let events_socket_path = hemis_dir.join("events.sock");
    let lock_path = hemis_dir.join("hemis.lock");

    // Try to connect to existing daemon
    if socket_path.exists() {
        match UnixStream::connect(&socket_path) {
            Ok(_stream) => {
                // Daemon is running
                println!(
                    "{}",
                    serde_json::json!({
                        "status": "running",
                        "socketPath": socket_path.to_string_lossy(),
                        "eventsSocketPath": events_socket_path.to_string_lossy(),
                        "started": false
                    })
                );
                return Ok(());
            }
            Err(_) => {
                // Socket exists but not connectable - stale
                eprintln!("Cleaning up stale socket...");
                let _ = std::fs::remove_file(&socket_path);
                let _ = std::fs::remove_file(&events_socket_path);
                let _ = std::fs::remove_file(&lock_path);
            }
        }
    }

    // Create hemis directory if needed
    std::fs::create_dir_all(&hemis_dir)?;

    // Start daemon in background
    let exe = std::env::current_exe()?;
    let child = Command::new(&exe)
        .arg("--serve")
        .stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .spawn()?;

    eprintln!("Started daemon (pid {})", child.id());

    // Wait for socket to appear (up to 5 seconds)
    for _ in 0..50 {
        std::thread::sleep(Duration::from_millis(100));
        if socket_path.exists() {
            if UnixStream::connect(&socket_path).is_ok() {
                println!(
                    "{}",
                    serde_json::json!({
                        "status": "running",
                        "socketPath": socket_path.to_string_lossy(),
                        "eventsSocketPath": events_socket_path.to_string_lossy(),
                        "started": true,
                        "pid": child.id()
                    })
                );
                return Ok(());
            }
        }
    }

    anyhow::bail!("Daemon failed to start within 5 seconds");
}

fn main() -> Result<()> {
    // Initialize logging: HEMIS_LOG=debug for verbose, default is info
    env_logger::Builder::from_env(env_logger::Env::default().filter_or("HEMIS_LOG", "info"))
        .format_timestamp_millis()
        .init();

    let args: Vec<String> = std::env::args().collect();

    // Check for ensure-daemon subcommand first
    if args.len() > 1 && args[1] == "ensure-daemon" {
        return run_ensure_daemon();
    }

    // Check for grammar subcommand
    if args.len() > 1 && args[1] == "grammar" {
        return run_grammar_command(&args[2..]);
    }

    // Check for oracle subcommand
    if args.len() > 1 && args[1] == "oracle" {
        return run_oracle_command(&args[2..]);
    }

    if args.contains(&"--version".to_string()) || args.contains(&"-v".to_string()) {
        print_version();
        return Ok(());
    }

    if args.contains(&"--help".to_string()) || args.contains(&"-h".to_string()) {
        print_help();
        return Ok(());
    }

    // Parse CLI arguments
    let config_path = parse_arg_value(&args, "--config");
    let cli_db_path = parse_arg_value(&args, "--db-path").map(String::from);
    let cli_ai_provider = parse_arg_value(&args, "--ai-provider").map(String::from);

    // Build resolved config (CLI flags > config file > env vars > defaults)
    let overrides = CliOverrides {
        db_path: cli_db_path,
        ai_provider: cli_ai_provider,
    };
    let config = ResolvedConfig::new(config_path, overrides);

    // Check for snapshot commands
    if let Some(path) = parse_arg_value(&args, "--save-snapshot") {
        return run_save_snapshot(&config, path);
    }

    if let Some(path) = parse_arg_value(&args, "--load-snapshot") {
        return run_load_snapshot(&config, path);
    }

    match detect_mode() {
        Mode::Server => run_server_mode(&config),
        Mode::Stdio => run_stdio_mode(&config),
    }
}
