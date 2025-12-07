//! ai_cli: integration with Codex CLI and Claude Code CLI for analysis/explanation.
//!
//! This module shells out to local CLI tools instead of using hosted APIs,
//! so Hemis never needs API keys.
//!
//! Provider selection:
//! - default: Codex CLI (`codex` on PATH)
//! - set HEMIS_AI_PROVIDER=claude to use Claude CLI (`claude`)
//!
//! All functions are best-effort: on failure, the caller can fall back to
//! simpler behaviour (e.g. returning the raw snippet).

use anyhow::{anyhow, Context, Result};
use log::debug;
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::{Duration, Instant};

/// Maximum concurrent AI CLI processes (prevents resource exhaustion)
const MAX_CONCURRENT_AI_CALLS: usize = 2;

/// Rate limit: maximum AI calls per minute
const MAX_CALLS_PER_MINUTE: usize = 10;

/// Global counter for active AI CLI processes
static ACTIVE_AI_CALLS: AtomicUsize = AtomicUsize::new(0);

/// Rate limiting state
use std::sync::Mutex;
static RATE_LIMIT_STATE: Mutex<Option<RateLimitState>> = Mutex::new(None);

struct RateLimitState {
    window_start: Instant,
    call_count: usize,
}

/// Check and update rate limit. Returns error if rate exceeded.
fn check_rate_limit() -> Result<()> {
    let mut state = RATE_LIMIT_STATE.lock().map_err(|_| anyhow!("rate limit lock poisoned"))?;

    let now = Instant::now();
    let window = Duration::from_secs(60);

    match state.as_mut() {
        Some(s) => {
            // Use checked_duration_since to handle clock skew gracefully
            let elapsed = now.checked_duration_since(s.window_start).unwrap_or(window);
            if elapsed < window {
                if s.call_count >= MAX_CALLS_PER_MINUTE {
                    return Err(anyhow!("rate limit exceeded (max {} calls per minute)", MAX_CALLS_PER_MINUTE));
                }
                s.call_count += 1;
            } else {
                // Window expired, reset
                s.window_start = now;
                s.call_count = 1;
            }
        }
        None => {
            *state = Some(RateLimitState {
                window_start: now,
                call_count: 1,
            });
        }
    }
    Ok(())
}

/// RAII guard to track active AI calls
struct AiCallGuard;

impl AiCallGuard {
    fn try_acquire() -> Result<Self> {
        // Check rate limit first
        check_rate_limit()?;

        let current = ACTIVE_AI_CALLS.fetch_add(1, Ordering::SeqCst);
        if current >= MAX_CONCURRENT_AI_CALLS {
            ACTIVE_AI_CALLS.fetch_sub(1, Ordering::SeqCst);
            return Err(anyhow!(
                "too many concurrent AI requests (max {})",
                MAX_CONCURRENT_AI_CALLS
            ));
        }
        Ok(AiCallGuard)
    }
}

impl Drop for AiCallGuard {
    fn drop(&mut self) {
        ACTIVE_AI_CALLS.fetch_sub(1, Ordering::SeqCst);
    }
}

/// Validate project_root is a safe directory for AI CLI execution.
/// Returns canonicalized path or error.
fn validate_project_root(project_root: &Path) -> Result<PathBuf> {
    // Canonicalize to resolve symlinks and relative paths
    let canonical = project_root.canonicalize()
        .with_context(|| "invalid project root path")?;

    // Must be a directory
    if !canonical.is_dir() {
        return Err(anyhow!("project root is not a directory"));
    }

    // Reject system paths
    let path_str = canonical.to_string_lossy();
    if path_str == "/" || path_str.starts_with("/etc") || path_str.starts_with("/var")
        || path_str.starts_with("/usr") || path_str.starts_with("/bin")
        || path_str.starts_with("/sbin") || path_str.starts_with("/System")
        || path_str.starts_with("/Library") || path_str.starts_with("C:\\Windows")
    {
        return Err(anyhow!("refusing to run AI CLI in system directory"));
    }

    Ok(canonical)
}

/// Build minimal, safe environment for AI CLI subprocess.
fn build_safe_env() -> HashMap<String, String> {
    let mut env = HashMap::new();

    // Only pass through essential environment variables
    let allowed_vars = [
        "PATH", "HOME", "USER", "LANG", "LC_ALL", "TERM",
        "SHELL", "TMPDIR", "XDG_CONFIG_HOME", "XDG_DATA_HOME",
    ];

    for var in &allowed_vars {
        if let Ok(val) = std::env::var(var) {
            env.insert(var.to_string(), val);
        }
    }

    // Set safe defaults
    env.insert("LC_ALL".to_string(), "C.UTF-8".to_string());

    env
}

/// Persistent Claude CLI process using streaming JSON mode.
/// This avoids the ~10s startup cost on each call after the first.
struct PersistentClaude {
    child: std::process::Child,
    stdin: std::process::ChildStdin,
    stdout: std::io::BufReader<std::process::ChildStdout>,
    /// Project root this process was spawned for (for isolation check)
    project_root: PathBuf,
}

impl PersistentClaude {
    /// Spawn a new persistent Claude process.
    fn spawn(project_root: &Path) -> Result<Self> {
        use std::io::BufReader;

        let safe_env = build_safe_env();

        let mut child = Command::new("claude")
            .arg("-p")
            .arg("--input-format")
            .arg("stream-json")
            .arg("--output-format")
            .arg("stream-json")
            .arg("--verbose")
            .current_dir(project_root)
            .env_clear()
            .envs(&safe_env)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .spawn()
            .with_context(|| "failed to spawn persistent claude process")?;

        let stdin = child.stdin.take().ok_or_else(|| anyhow!("no stdin"))?;
        let stdout = child.stdout.take().ok_or_else(|| anyhow!("no stdout"))?;

        Ok(Self {
            child,
            stdin,
            stdout: BufReader::new(stdout),
            project_root: project_root.to_path_buf(),
        })
    }

    /// Send a prompt and get the response.
    fn query(&mut self, prompt: &str) -> Result<String> {
        use std::io::{BufRead, Write};

        debug!("[hemis] Sending query to persistent Claude...");

        // Send user message in stream-json format
        let msg = serde_json::json!({
            "type": "user",
            "message": {
                "role": "user",
                "content": prompt
            }
        });
        writeln!(self.stdin, "{}", msg)?;
        self.stdin.flush()?;

        debug!("[hemis] Query sent, waiting for response...");

        // Read responses until we get the result message
        let start = Instant::now();
        let mut line_count = 0;

        loop {
            if start.elapsed().as_secs() > AI_CLI_TIMEOUT_SECS {
                return Err(anyhow!("persistent claude query timed out after {} lines", line_count));
            }

            let mut line = String::new();
            match self.stdout.read_line(&mut line) {
                Ok(0) => return Err(anyhow!("claude process closed stdout after {} lines", line_count)),
                Ok(_) => {
                    line_count += 1;
                    let line = line.trim();
                    if line.is_empty() {
                        continue;
                    }

                    // Parse JSON response
                    if let Ok(json) = serde_json::from_str::<serde_json::Value>(line) {
                        let msg_type = json.get("type").and_then(|t| t.as_str()).unwrap_or("unknown");
                        debug!("[hemis] Claude message type: {}", msg_type);

                        // Check for result message (final response)
                        if msg_type == "result" {
                            if let Some(text) = json.get("result").and_then(|r| r.as_str()) {
                                debug!("[hemis] Got result after {} lines, {} chars", line_count, text.len());
                                return Ok(text.to_string());
                            }
                            // Check for error
                            if json.get("is_error").and_then(|e| e.as_bool()) == Some(true) {
                                let err_msg = json.get("error").and_then(|e| e.as_str()).unwrap_or("unknown error");
                                return Err(anyhow!("claude returned error: {}", err_msg));
                            }
                        }
                    }
                }
                Err(e) => return Err(anyhow!("error reading from claude after {} lines: {}", line_count, e)),
            }
        }
    }

    /// Check if the process is still alive.
    fn is_alive(&mut self) -> bool {
        matches!(self.child.try_wait(), Ok(None))
    }
}

impl Drop for PersistentClaude {
    fn drop(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

/// Global persistent Claude instance
static PERSISTENT_CLAUDE: Mutex<Option<PersistentClaude>> = Mutex::new(None);

/// Get or create persistent Claude process
fn get_persistent_claude(project_root: &Path) -> Result<std::sync::MutexGuard<'static, Option<PersistentClaude>>> {
    let mut guard = PERSISTENT_CLAUDE.lock().map_err(|_| anyhow!("claude lock poisoned"))?;

    // Check if we need to spawn or respawn
    let needs_spawn = match guard.as_mut() {
        None => true,
        Some(claude) => {
            if !claude.is_alive() {
                debug!("[hemis] Claude process died, respawning...");
                true
            } else if claude.project_root != project_root {
                // Project changed - kill old process and spawn new one for isolation
                debug!(
                    "[hemis] Project changed ({} -> {}), respawning Claude...",
                    claude.project_root.display(),
                    project_root.display()
                );
                true
            } else {
                false
            }
        }
    };

    if needs_spawn {
        debug!("[hemis] Spawning persistent Claude process for {}...", project_root.display());
        *guard = Some(PersistentClaude::spawn(project_root)?);
    }

    Ok(guard)
}

/// Pre-warm the persistent Claude process for faster first query.
/// Call this during index-project to avoid delay on first explain-region.
/// This sends a simple query to ensure Claude is fully initialized.
pub fn warm_up_claude(project_root: &Path) -> Result<()> {
    let safe_project_root = validate_project_root(project_root)?;
    let mut guard = get_persistent_claude(&safe_project_root)?;

    // Send a simple query to ensure Claude is fully initialized
    if let Some(claude) = guard.as_mut() {
        let _ = claude.query("Say OK")?;
        debug!("[hemis] Claude process warmed up and ready");
    }

    Ok(())
}

/// Which CLI to use for AI calls.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CliProvider {
    Codex,
    Claude,
}

impl CliProvider {
    /// Detect provider from environment or availability.
    pub fn from_env() -> Option<CliProvider> {
        // Check env var first
        if let Ok(v) = std::env::var("HEMIS_AI_PROVIDER") {
            let v = v.to_lowercase();
            if v == "claude" || v == "claude-cli" {
                if is_available("claude") {
                    return Some(CliProvider::Claude);
                }
            } else if v == "codex" {
                if is_available("codex") {
                    return Some(CliProvider::Codex);
                }
            } else if v == "none" || v == "disabled" {
                return None;
            }
        }

        // Auto-detect: prefer codex, fall back to claude
        if is_available("codex") {
            Some(CliProvider::Codex)
        } else if is_available("claude") {
            Some(CliProvider::Claude)
        } else {
            None
        }
    }

    pub fn as_str(self) -> &'static str {
        match self {
            CliProvider::Codex => "codex",
            CliProvider::Claude => "claude",
        }
    }
}

/// Check if a CLI tool is available on PATH.
fn is_available(cmd: &str) -> bool {
    Command::new("which")
        .arg(cmd)
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Ensure `<project_root>/.hemis` exists with secure permissions and return its path.
fn ensure_hemis_dir(project_root: &Path) -> Result<PathBuf> {
    let hemis = project_root.join(".hemis");
    fs::create_dir_all(&hemis).with_context(|| {
        format!(
            "failed to create .hemis directory at {}",
            hemis.display()
        )
    })?;
    // Set secure permissions (owner-only) on Unix
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let perms = std::fs::Permissions::from_mode(0o700);
        fs::set_permissions(&hemis, perms).ok(); // Best-effort, don't fail if already set
    }
    Ok(hemis)
}

/// Return the contents of `<project_root>/.hemis/analysis.md` if present.
pub fn load_analysis(project_root: &Path) -> Option<String> {
    let path = project_root.join(".hemis").join("analysis.md");
    fs::read_to_string(&path).ok()
}

/// Check if analysis file exists.
pub fn has_analysis(project_root: &Path) -> bool {
    project_root.join(".hemis").join("analysis.md").exists()
}

/// Run repository analysis via the configured CLI provider and persist
/// `.hemis/analysis.md`. Returns the provider used.
pub fn analyze_repo(project_root: &Path) -> Result<CliProvider> {
    let provider = CliProvider::from_env()
        .ok_or_else(|| anyhow!("no AI CLI available (codex or claude)"))?;

    let hemis_dir = ensure_hemis_dir(project_root)?;

    let prompt = r#"You are Hemis, an assistant analyzing this codebase.

Goal: produce a concise analysis document in Markdown format that will help
explain code regions to developers. Be information-dense but readable.

Instructions:
1. Scan important source files in this repository.
2. Identify modules, cross-cutting concerns, and architectural layers.
3. Document in this structure:

# Project Analysis

## Overview
- Project name and purpose (1-2 sentences)
- Primary languages used
- Key technologies/frameworks

## Architecture
- High-level layers (UI, domain, infrastructure, etc.)
- Module structure and responsibilities
- Data flow between components

## Key Patterns
- Design patterns used
- Conventions (naming, file organization)
- Important invariants

## Cross-Cutting Concerns
- Error handling approach
- Logging/telemetry
- Configuration management
- Testing strategy

## Module Summary
For each significant module:
- Path and purpose
- Key exports/interfaces
- Dependencies

Output ONLY the Markdown document. Do not wrap in code blocks.
"#;

    let output = match provider {
        CliProvider::Codex => run_codex(project_root, prompt)?,
        CliProvider::Claude => run_claude(project_root, prompt)?,
    };

    // Clean up the output (remove any markdown code block wrappers)
    let cleaned = clean_markdown_output(&output);

    let path = hemis_dir.join("analysis.md");
    fs::write(&path, cleaned.as_bytes())
        .with_context(|| format!("failed to write {}", path.display()))?;

    Ok(provider)
}

/// Clean markdown output by removing code block wrappers if present.
fn clean_markdown_output(s: &str) -> String {
    let trimmed = s.trim();

    // Remove ```markdown ... ``` wrapper (using safe string methods)
    if let Some(inner) = trimmed.strip_prefix("```markdown") {
        if let Some(end) = inner.rfind("```") {
            // end is a byte offset into inner, which is safe since we're finding "```"
            return inner.get(..end).unwrap_or(inner).trim().to_string();
        }
    }

    // Remove ``` ... ``` wrapper
    if let Some(inner) = trimmed.strip_prefix("```") {
        if let Some(stripped) = inner.strip_suffix("```") {
            // Skip language identifier on first line if present
            let inner = if let Some(idx) = stripped.find('\n') {
                // idx is a byte offset to ASCII newline, safe to slice at idx+1
                stripped.get(idx + 1..).unwrap_or(stripped)
            } else {
                stripped
            };
            return inner.trim().to_string();
        }
    }

    trimmed.to_string()
}

/// Explain a region of code using the configured CLI provider.
///
/// If `detailed` is true, provides a comprehensive explanation.
/// Otherwise provides a brief 2-4 sentence summary.
///
/// Returns (provider, explanation, had_analysis_context).
/// The caller is responsible for creating any notes based on the explanation.
pub fn explain_region(
    project_root: &Path,
    file: &str,
    start_line: usize,
    end_line: usize,
    snippet: &str,
    detailed: bool,
) -> Result<(CliProvider, String, bool)> {
    let provider = CliProvider::from_env()
        .ok_or_else(|| anyhow!("no AI CLI available (codex or claude)"))?;

    let analysis = load_analysis(project_root);
    let has_analysis = analysis.is_some();

    let analysis_section = if let Some(ref a) = analysis {
        format!("## Project Context\n\n{}\n\n", a)
    } else {
        String::new()
    };

    let prompt = if detailed {
        format!(
            r#"You are Hemis, an assistant embedded in a codebase-aware tool.

{analysis_section}Explain the following code region in detail. Your response will be stored as a code note.

**File:** {file}
**Lines:** {start}-{end}

```
{snippet}
```

Provide a comprehensive explanation covering:
1. What this code does and its purpose
2. How it fits into the project architecture
3. Key assumptions, invariants, or constraints
4. Potential pitfalls or edge cases a maintainer should know
5. Any important implementation details

Be thorough but focused. Output plain text only, no markdown.
"#,
            analysis_section = analysis_section,
            file = file,
            start = start_line,
            end = end_line,
            snippet = snippet
        )
    } else {
        format!(
            r#"You are Hemis, an assistant embedded in a codebase-aware tool.

{analysis_section}Explain the following code region. Your response will be stored as a code note.

**File:** {file}
**Lines:** {start}-{end}

```
{snippet}
```

Provide a brief explanation (2-4 sentences) covering:
- What this code does
- Key assumptions or pitfalls

Keep it short and practical - this will be displayed as an inline note. Output plain text only, no markdown.
"#,
            analysis_section = analysis_section,
            file = file,
            start = start_line,
            end = end_line,
            snippet = snippet
        )
    };

    let explanation = match provider {
        CliProvider::Codex => run_codex(project_root, &prompt)?,
        CliProvider::Claude => run_claude(project_root, &prompt)?,
    };

    Ok((provider, explanation.trim().to_string(), has_analysis))
}

/// Timeout for AI CLI subprocess execution (5 minutes)
const AI_CLI_TIMEOUT_SECS: u64 = 300;

/// RAII guard to ensure temp file cleanup on all exit paths.
struct TempFileGuard(std::path::PathBuf);

impl Drop for TempFileGuard {
    fn drop(&mut self) {
        let _ = std::fs::remove_file(&self.0);
    }
}

/// Helper: run Codex CLI with a prompt in the given project root.
fn run_codex(project_root: &Path, prompt: &str) -> Result<String> {
    // Acquire concurrency guard (limits parallel AI calls and rate limiting)
    let _concurrency_guard = AiCallGuard::try_acquire()?;

    // Validate project root
    let safe_project_root = validate_project_root(project_root)?;

    // Create unique temp file for output since codex prints progress/diagnostics to stdout
    let tmp_dir = std::env::temp_dir();
    let output_path = tmp_dir.join(format!("hemis-codex-{}.txt", uuid::Uuid::new_v4()));
    let _temp_guard = TempFileGuard(output_path.clone());

    // Build safe environment (minimal variables)
    let safe_env = build_safe_env();

    // codex exec <prompt> --output-last-message <file>
    let mut child = Command::new("codex")
        .arg("exec")
        .arg("--skip-git-repo-check")
        .arg("--sandbox")
        .arg("read-only")
        .arg("--output-last-message")
        .arg(&output_path)
        .arg(prompt)
        .current_dir(&safe_project_root)
        .env_clear()
        .envs(&safe_env)
        // Inherit stdout/stderr to avoid pipe buffer deadlock
        // Codex writes its actual output to the temp file
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .spawn()
        .with_context(|| "failed to spawn `codex` CLI")?;

    // Wait with timeout, properly reaping the process
    let start = std::time::Instant::now();
    loop {
        match child.try_wait() {
            Ok(Some(status)) => {
                // Process exited, read output file
                let result = std::fs::read_to_string(&output_path)
                    .with_context(|| format!("failed to read codex output from {}", output_path.display()))?;
                if !status.success() {
                    return Err(anyhow!("`codex exec` failed with status {:?}", status.code()));
                }
                return Ok(result);
            }
            Ok(None) => {
                if start.elapsed().as_secs() > AI_CLI_TIMEOUT_SECS {
                    // Kill and wait to reap (prevent zombie)
                    let _ = child.kill();
                    let _ = child.wait(); // Reap the zombie
                    return Err(anyhow!("codex CLI timed out after {} seconds", AI_CLI_TIMEOUT_SECS));
                }
                std::thread::sleep(std::time::Duration::from_millis(100));
            }
            Err(e) => {
                // Try to kill and reap on error
                let _ = child.kill();
                let _ = child.wait();
                return Err(anyhow!("failed to wait for codex CLI: {}", e));
            }
        }
    }
}

/// Helper: run Claude Code CLI with a prompt in the given project root.
/// Uses persistent process for fast response times after first call.
fn run_claude(project_root: &Path, prompt: &str) -> Result<String> {
    debug!("[hemis] run_claude: starting...");

    // Acquire concurrency guard (limits parallel AI calls and rate limiting)
    let _concurrency_guard = AiCallGuard::try_acquire()?;
    debug!("[hemis] run_claude: acquired concurrency guard");

    // Validate project root
    let safe_project_root = validate_project_root(project_root)?;
    debug!("[hemis] run_claude: validated project root");

    // Try persistent process first
    debug!("[hemis] run_claude: trying to get persistent claude...");
    if let Ok(mut guard) = get_persistent_claude(&safe_project_root) {
        debug!("[hemis] run_claude: got persistent claude mutex");
        if let Some(claude) = guard.as_mut() {
            debug!("[hemis] run_claude: sending query to persistent claude...");
            match claude.query(prompt) {
                Ok(result) => {
                    debug!("[hemis] run_claude: persistent query succeeded ({} chars)", result.len());
                    return Ok(result);
                }
                Err(e) => {
                    // Process died or failed, will respawn on next call
                    debug!("[hemis] Persistent claude failed: {}, falling back to one-shot", e);
                    *guard = None;
                }
            }
        } else {
            debug!("[hemis] run_claude: no persistent claude in mutex");
        }
    } else {
        debug!("[hemis] run_claude: failed to get persistent claude mutex");
    }

    // Fallback: one-shot process (slower but more reliable)
    debug!("[hemis] run_claude: falling back to one-shot mode");
    run_claude_oneshot(&safe_project_root, prompt)
}

/// One-shot Claude CLI call (fallback when persistent fails).
fn run_claude_oneshot(project_root: &Path, prompt: &str) -> Result<String> {
    let safe_env = build_safe_env();

    use std::io::Read;
    use std::sync::mpsc;

    let mut child = Command::new("claude")
        .arg("-p")
        .arg(prompt)
        .current_dir(project_root)
        .env_clear()
        .envs(&safe_env)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .with_context(|| "failed to spawn `claude` CLI")?;

    // Take ownership of stdout/stderr to drain in background threads
    let stdout = child.stdout.take();
    let stderr = child.stderr.take();

    // Spawn threads to drain pipes (prevents deadlock if buffer fills)
    let (stdout_tx, stdout_rx) = mpsc::channel();
    let (stderr_tx, stderr_rx) = mpsc::channel();

    let stdout_thread = std::thread::spawn(move || {
        let mut output = String::new();
        if let Some(mut out) = stdout {
            let _ = out.read_to_string(&mut output);
        }
        let _ = stdout_tx.send(output);
    });

    let stderr_thread = std::thread::spawn(move || {
        let mut output = String::new();
        if let Some(mut err) = stderr {
            let _ = err.read_to_string(&mut output);
        }
        let _ = stderr_tx.send(output);
    });

    // Wait with timeout
    let start = std::time::Instant::now();
    let status = loop {
        match child.try_wait() {
            Ok(Some(status)) => break status,
            Ok(None) => {
                if start.elapsed().as_secs() > AI_CLI_TIMEOUT_SECS {
                    // Kill and wait to reap (prevent zombie)
                    let _ = child.kill();
                    let _ = child.wait(); // Reap the zombie
                    // Wait for drain threads to finish
                    let _ = stdout_thread.join();
                    let _ = stderr_thread.join();
                    return Err(anyhow!("claude CLI timed out after {} seconds", AI_CLI_TIMEOUT_SECS));
                }
                std::thread::sleep(std::time::Duration::from_millis(100));
            }
            Err(e) => {
                // Try to kill and reap on error
                let _ = child.kill();
                let _ = child.wait();
                let _ = stdout_thread.join();
                let _ = stderr_thread.join();
                return Err(anyhow!("failed to wait for claude CLI: {}", e));
            }
        }
    };

    // Wait for drain threads and get output
    let _ = stdout_thread.join();
    let _ = stderr_thread.join();

    let stdout_output = stdout_rx.recv().unwrap_or_default();
    let stderr_output = stderr_rx.recv().unwrap_or_default();

    if !status.success() {
        return Err(anyhow!(
            "`claude -p` failed with status {:?}: {}",
            status.code(),
            stderr_output
        ));
    }

    Ok(stdout_output)
}

#[cfg(test)]
mod tests {
    use super::*;
    use serial_test::serial;

    #[test]
    fn clean_markdown_removes_code_block_wrapper() {
        let input = "```markdown\n# Title\n\nContent\n```";
        assert_eq!(clean_markdown_output(input), "# Title\n\nContent");
    }

    #[test]
    fn clean_markdown_preserves_plain_content() {
        let input = "# Title\n\nContent";
        assert_eq!(clean_markdown_output(input), "# Title\n\nContent");
    }

    #[test]
    fn is_available_checks_path() {
        // 'ls' should be available on any Unix system
        assert!(is_available("ls"));
        // Random non-existent command
        assert!(!is_available("definitely_not_a_real_command_xyz123"));
    }

    #[test]
    #[serial(env)]
    fn provider_from_env_respects_disabled() {
        std::env::set_var("HEMIS_AI_PROVIDER", "none");
        assert!(CliProvider::from_env().is_none());
        std::env::remove_var("HEMIS_AI_PROVIDER");
    }
}
