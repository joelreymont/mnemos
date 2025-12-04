// Hemis MCP Server - Tools to reduce token usage during development.
//
// Tools:
// - cargo_test_summary: Run cargo test and return compact pass/fail summary
// - cargo_clippy: Run clippy and return compact warning/error summary
// - cargo_build: Run cargo build and return compact result
// - swift_test: Run swift test and return compact summary
// - bd_context: Return beads in compact form
// - git_context: Return git status/diff compactly

use std::borrow::Cow;
use std::process::{Command, Stdio};
use std::thread;
use std::time::{Duration, Instant};

use rmcp::{
    ErrorData as McpError, ServerHandler, ServiceExt,
    handler::server::{
        router::tool::ToolRouter,
        wrapper::Parameters,
    },
    model::*,
    schemars, tool, tool_handler, tool_router,
    transport::stdio,
};

const SERVER_VERSION: &str = "v1";
const COMMAND_TIMEOUT: Duration = Duration::from_secs(120);

#[allow(dead_code)]
fn mcp_error(msg: &str) -> McpError {
    McpError {
        code: ErrorCode::INTERNAL_ERROR,
        message: Cow::from(msg.to_string()),
        data: None,
    }
}

/// Run a command with timeout, returns (stdout, stderr) or error message
fn run_command_with_timeout(mut cmd: Command, timeout: Duration) -> Result<(String, String), String> {
    cmd.stdout(Stdio::piped()).stderr(Stdio::piped());

    let mut child = cmd.spawn().map_err(|e| format!("spawn failed: {}", e))?;
    let start = Instant::now();

    loop {
        match child.try_wait() {
            Ok(Some(status)) => {
                let output = child.wait_with_output().map_err(|e| format!("wait failed: {}", e))?;
                let stdout = String::from_utf8_lossy(&output.stdout).to_string();
                let stderr = String::from_utf8_lossy(&output.stderr).to_string();
                if status.success() {
                    return Ok((stdout, stderr));
                } else {
                    let error_output = if stderr.trim().is_empty() { &stdout } else { &stderr };
                    return Err(format!("exit {}: {}", status.code().unwrap_or(-1), error_output));
                }
            }
            Ok(None) => {
                if start.elapsed() > timeout {
                    let _ = child.kill();
                    return Err(format!("TIMEOUT after {}s", timeout.as_secs()));
                }
                thread::sleep(Duration::from_millis(100));
            }
            Err(e) => return Err(format!("wait error: {}", e)),
        }
    }
}

// ============================================================================
// Request types
// ============================================================================

#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct CargoTestRequest {
    /// Working directory for cargo test (defaults to current dir)
    #[serde(default)]
    pub cwd: Option<String>,
    /// Optional test filter (e.g., "unit" to run only matching tests)
    #[serde(default)]
    pub filter: Option<String>,
    /// Package to test (for workspace)
    #[serde(default)]
    pub package: Option<String>,
}

#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct BdContextRequest {
    /// Working directory for bd command (defaults to current dir)
    #[serde(default)]
    pub cwd: Option<String>,
    /// Max beads to return (default 50)
    #[serde(default = "default_beads_limit")]
    pub limit: usize,
}

fn default_beads_limit() -> usize { 50 }

#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct GitContextRequest {
    /// Working directory for git commands (defaults to current dir)
    #[serde(default)]
    pub cwd: Option<String>,
    /// Include diff output (default false for compact output)
    #[serde(default)]
    pub include_diff: bool,
    /// Max diff lines to show (default 100)
    #[serde(default = "default_diff_limit")]
    pub diff_limit: usize,
}

fn default_diff_limit() -> usize { 100 }

#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct VersionRequest {}

// === BD (Beads) Issue Tracker Request Types ===

#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct BdListRequest {
    /// Filter by status: "open", "closed", "in_progress", "ready" (default: ready)
    #[serde(default = "default_bd_status")]
    pub status: String,
    /// Filter by label (optional)
    #[serde(default)]
    pub label: Option<String>,
    /// Max results (default 20)
    #[serde(default = "default_bd_limit")]
    pub limit: usize,
}

fn default_bd_status() -> String { "ready".to_string() }
fn default_bd_limit() -> usize { 20 }

#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct BdCreateRequest {
    /// Issue title (required)
    pub title: String,
    /// Issue description (recommended)
    #[serde(default)]
    pub description: Option<String>,
    /// Labels (e.g., "backend", "bug", "perf")
    #[serde(default)]
    pub labels: Vec<String>,
    /// Priority 0-4 (0=highest, default=2)
    #[serde(default = "default_priority")]
    pub priority: u8,
}

fn default_priority() -> u8 { 2 }

#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct BdCloseRequest {
    /// Issue ID to close (e.g., "hemis-abc")
    pub id: String,
}

#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct BdShowRequest {
    /// Issue ID to show (e.g., "hemis-abc")
    pub id: String,
}

#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct BdUpdateRequest {
    /// Issue ID to update (e.g., "hemis-abc")
    pub id: String,
    /// New status: "open", "in_progress", "closed"
    #[serde(default)]
    pub status: Option<String>,
    /// Add labels
    #[serde(default)]
    pub add_labels: Vec<String>,
}

// === Cargo Tools Request Types ===

#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct CargoClippyRequest {
    /// Working directory (defaults to current dir)
    #[serde(default)]
    pub cwd: Option<String>,
    /// Package to check (for workspace)
    #[serde(default)]
    pub package: Option<String>,
    /// Auto-fix warnings where possible
    #[serde(default)]
    pub fix: bool,
}

#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct CargoBuildRequest {
    /// Working directory (defaults to current dir)
    #[serde(default)]
    pub cwd: Option<String>,
    /// Package to build (for workspace)
    #[serde(default)]
    pub package: Option<String>,
    /// Build in release mode
    #[serde(default)]
    pub release: bool,
}

#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct SwiftTestRequest {
    /// Working directory (defaults to current dir)
    #[serde(default)]
    pub cwd: Option<String>,
    /// Filter tests by name
    #[serde(default)]
    pub filter: Option<String>,
}

// ============================================================================
// Server implementation
// ============================================================================

#[derive(Clone)]
struct HemisServer {
    tool_router: ToolRouter<HemisServer>,
}

impl HemisServer {
    fn new() -> Self {
        Self {
            tool_router: Self::tool_router(),
        }
    }
}

#[tool_router]
impl HemisServer {
    #[tool(description = "Get hemis-mcp server version")]
    fn version(
        &self,
        Parameters(_req): Parameters<VersionRequest>,
    ) -> Result<CallToolResult, McpError> {
        Ok(CallToolResult::success(vec![Content::text(format!(
            "hemis-mcp {}", SERVER_VERSION
        ))]))
    }

    #[tool(description = "Run cargo test and return compact summary with pass/fail counts. Much more compact than raw test output.")]
    fn cargo_test_summary(
        &self,
        Parameters(req): Parameters<CargoTestRequest>,
    ) -> Result<CallToolResult, McpError> {
        let mut cmd = Command::new("cargo");
        cmd.args(["test", "--no-fail-fast", "--color=never"]);

        if let Some(pkg) = &req.package {
            cmd.args(["-p", pkg]);
        }
        if let Some(filter) = &req.filter {
            cmd.arg(filter);
        }
        if let Some(cwd) = &req.cwd {
            cmd.current_dir(cwd);
        }

        match run_command_with_timeout(cmd, COMMAND_TIMEOUT) {
            Ok((stdout, stderr)) => {
                let output = format!("{}\n{}", stdout, stderr);
                let summary = parse_cargo_test_output(&output);
                Ok(CallToolResult::success(vec![Content::text(summary)]))
            }
            Err(e) => Ok(CallToolResult::success(vec![Content::text(format!("ERROR: {}", e))]))
        }
    }

    #[tool(description = "Return beads (work items) in compact form. Shows id, status, and title for open beads.")]
    fn bd_context(
        &self,
        Parameters(req): Parameters<BdContextRequest>,
    ) -> Result<CallToolResult, McpError> {
        let mut cmd = Command::new("bd");
        cmd.args(["list", "--json", "--status=open"]);

        if let Some(cwd) = &req.cwd {
            cmd.current_dir(cwd);
        }

        match run_command_with_timeout(cmd, Duration::from_secs(10)) {
            Ok((stdout, _)) => {
                let compact = parse_beads_json(&stdout, req.limit);
                Ok(CallToolResult::success(vec![Content::text(compact)]))
            }
            Err(e) => {
                Ok(CallToolResult::success(vec![Content::text(format!("bd error: {}", e))]))
            }
        }
    }

    #[tool(description = "Return git status and optionally diff in compact form. Shows modified/added/deleted files.")]
    fn git_context(
        &self,
        Parameters(req): Parameters<GitContextRequest>,
    ) -> Result<CallToolResult, McpError> {
        let cwd = req.cwd.as_deref();
        let mut result = String::new();

        // Get branch info
        let mut branch_cmd = Command::new("git");
        branch_cmd.args(["branch", "--show-current"]);
        if let Some(dir) = cwd {
            branch_cmd.current_dir(dir);
        }
        if let Ok((branch, _)) = run_command_with_timeout(branch_cmd, Duration::from_secs(5)) {
            result.push_str(&format!("branch: {}", branch.trim()));
        }

        // Get status
        let mut status_cmd = Command::new("git");
        status_cmd.args(["status", "--porcelain"]);
        if let Some(dir) = cwd {
            status_cmd.current_dir(dir);
        }

        match run_command_with_timeout(status_cmd, Duration::from_secs(10)) {
            Ok((stdout, _)) => {
                if stdout.trim().is_empty() {
                    result.push_str("\nstatus: clean");
                } else {
                    let lines: Vec<&str> = stdout.lines().collect();
                    result.push_str(&format!("\nchanges: {} files", lines.len()));
                    for line in &lines {
                        result.push_str(&format!("\n  {}", line));
                    }
                }
            }
            Err(e) => {
                result.push_str(&format!("\nstatus error: {}", e));
            }
        }

        // Optionally include diff
        if req.include_diff {
            let mut diff_cmd = Command::new("git");
            diff_cmd.args(["diff", "--stat"]);
            if let Some(dir) = cwd {
                diff_cmd.current_dir(dir);
            }

            if let Ok((diff_out, _)) = run_command_with_timeout(diff_cmd, Duration::from_secs(10)) {
                let diff_lines: Vec<&str> = diff_out.lines().take(req.diff_limit).collect();
                if !diff_lines.is_empty() {
                    result.push_str("\n\ndiff --stat:");
                    for line in diff_lines {
                        result.push_str(&format!("\n  {}", line));
                    }
                }
            }
        }

        Ok(CallToolResult::success(vec![Content::text(result)]))
    }

    // === BD (Beads) Issue Tracker Tools ===

    #[tool(description = "List issues from bd tracker. Default shows ready work (open, no blockers).")]
    fn bd_list(
        &self,
        Parameters(req): Parameters<BdListRequest>,
    ) -> Result<CallToolResult, McpError> {
        let mut cmd = Command::new("bd");

        // Map status to bd command
        match req.status.as_str() {
            "ready" => { cmd.arg("ready"); }
            "blocked" => { cmd.arg("blocked"); }
            "closed" => { cmd.args(["list", "--status", "closed"]); }
            "in_progress" => { cmd.args(["list", "--status", "in_progress"]); }
            "open" => { cmd.args(["list", "--status", "open"]); }
            _ => { cmd.arg("ready"); }
        }

        if let Some(ref label) = req.label {
            cmd.args(["--label", label]);
        }

        match run_command_with_timeout(cmd, Duration::from_secs(10)) {
            Ok((stdout, _)) => {
                let lines: Vec<&str> = stdout.lines().take(req.limit + 5).collect();
                Ok(CallToolResult::success(vec![Content::text(lines.join("\n"))]))
            }
            Err(e) => Ok(CallToolResult::success(vec![Content::text(format!("BD ERROR: {}", e))]))
        }
    }

    #[tool(description = "Create a new issue in bd tracker. Returns issue ID.")]
    fn bd_create(
        &self,
        Parameters(req): Parameters<BdCreateRequest>,
    ) -> Result<CallToolResult, McpError> {
        let mut cmd = Command::new("bd");
        cmd.arg("create");
        cmd.arg(&req.title);

        if let Some(ref desc) = req.description {
            cmd.args(["--description", desc]);
        }

        if !req.labels.is_empty() {
            cmd.args(["--labels", &req.labels.join(",")]);
        }

        cmd.args(["--priority", &req.priority.to_string()]);

        match run_command_with_timeout(cmd, Duration::from_secs(10)) {
            Ok((stdout, _)) => {
                Ok(CallToolResult::success(vec![Content::text(format!("CREATED: {}", stdout.trim()))]))
            }
            Err(e) => Ok(CallToolResult::success(vec![Content::text(format!("BD CREATE ERROR: {}", e))]))
        }
    }

    #[tool(description = "Close an issue in bd tracker.")]
    fn bd_close(
        &self,
        Parameters(req): Parameters<BdCloseRequest>,
    ) -> Result<CallToolResult, McpError> {
        let mut cmd = Command::new("bd");
        cmd.args(["close", &req.id]);

        match run_command_with_timeout(cmd, Duration::from_secs(10)) {
            Ok((stdout, _)) => {
                Ok(CallToolResult::success(vec![Content::text(format!("CLOSED: {}", stdout.trim()))]))
            }
            Err(e) => Ok(CallToolResult::success(vec![Content::text(format!("BD CLOSE ERROR: {}", e))]))
        }
    }

    #[tool(description = "Show details of a specific issue.")]
    fn bd_show(
        &self,
        Parameters(req): Parameters<BdShowRequest>,
    ) -> Result<CallToolResult, McpError> {
        let mut cmd = Command::new("bd");
        cmd.args(["show", &req.id]);

        match run_command_with_timeout(cmd, Duration::from_secs(10)) {
            Ok((stdout, _)) => {
                Ok(CallToolResult::success(vec![Content::text(stdout)]))
            }
            Err(e) => Ok(CallToolResult::success(vec![Content::text(format!("BD SHOW ERROR: {}", e))]))
        }
    }

    #[tool(description = "Update an issue status or labels.")]
    fn bd_update(
        &self,
        Parameters(req): Parameters<BdUpdateRequest>,
    ) -> Result<CallToolResult, McpError> {
        let mut cmd = Command::new("bd");
        cmd.args(["update", &req.id]);

        if let Some(ref status) = req.status {
            cmd.args(["--status", status]);
        }

        if !req.add_labels.is_empty() {
            cmd.args(["--add-labels", &req.add_labels.join(",")]);
        }

        match run_command_with_timeout(cmd, Duration::from_secs(10)) {
            Ok((stdout, _)) => {
                Ok(CallToolResult::success(vec![Content::text(format!("UPDATED: {}", stdout.trim()))]))
            }
            Err(e) => Ok(CallToolResult::success(vec![Content::text(format!("BD UPDATE ERROR: {}", e))]))
        }
    }

    // === Cargo Tools ===

    #[tool(description = "Run cargo clippy and return compact warning/error summary. Much more compact than raw clippy output.")]
    fn cargo_clippy(
        &self,
        Parameters(req): Parameters<CargoClippyRequest>,
    ) -> Result<CallToolResult, McpError> {
        let mut cmd = Command::new("cargo");

        if req.fix {
            cmd.args(["clippy", "--fix", "--allow-dirty", "--allow-staged"]);
        } else {
            cmd.arg("clippy");
        }

        cmd.args(["--message-format=json", "--color=never"]);

        if let Some(pkg) = &req.package {
            cmd.args(["-p", pkg]);
        }
        if let Some(cwd) = &req.cwd {
            cmd.current_dir(cwd);
        }

        cmd.stdout(Stdio::piped()).stderr(Stdio::piped());

        match cmd.output() {
            Ok(output) => {
                let stdout = String::from_utf8_lossy(&output.stdout);
                let stderr = String::from_utf8_lossy(&output.stderr);
                let summary = parse_clippy_json(&stdout, &stderr);
                Ok(CallToolResult::success(vec![Content::text(summary)]))
            }
            Err(e) => Ok(CallToolResult::success(vec![Content::text(format!("ERROR: {}", e))]))
        }
    }

    #[tool(description = "Run cargo build and return compact result. Shows success/fail and first errors only.")]
    fn cargo_build(
        &self,
        Parameters(req): Parameters<CargoBuildRequest>,
    ) -> Result<CallToolResult, McpError> {
        let mut cmd = Command::new("cargo");
        cmd.args(["build", "--message-format=json", "--color=never"]);

        if req.release {
            cmd.arg("--release");
        }
        if let Some(pkg) = &req.package {
            cmd.args(["-p", pkg]);
        }
        if let Some(cwd) = &req.cwd {
            cmd.current_dir(cwd);
        }

        cmd.stdout(Stdio::piped()).stderr(Stdio::piped());

        match cmd.output() {
            Ok(output) => {
                let stdout = String::from_utf8_lossy(&output.stdout);
                let stderr = String::from_utf8_lossy(&output.stderr);
                let summary = parse_cargo_build_json(&stdout, &stderr, output.status.success());
                Ok(CallToolResult::success(vec![Content::text(summary)]))
            }
            Err(e) => Ok(CallToolResult::success(vec![Content::text(format!("ERROR: {}", e))]))
        }
    }

    #[tool(description = "Run swift test and return compact summary with pass/fail counts.")]
    fn swift_test(
        &self,
        Parameters(req): Parameters<SwiftTestRequest>,
    ) -> Result<CallToolResult, McpError> {
        let mut cmd = Command::new("swift");
        cmd.arg("test");

        if let Some(filter) = &req.filter {
            cmd.args(["--filter", filter]);
        }
        if let Some(cwd) = &req.cwd {
            cmd.current_dir(cwd);
        }

        cmd.stdout(Stdio::piped()).stderr(Stdio::piped());

        match cmd.output() {
            Ok(output) => {
                let stdout = String::from_utf8_lossy(&output.stdout);
                let stderr = String::from_utf8_lossy(&output.stderr);
                let combined = format!("{}\n{}", stdout, stderr);
                let summary = parse_swift_test_output(&combined, output.status.success());
                Ok(CallToolResult::success(vec![Content::text(summary)]))
            }
            Err(e) => Ok(CallToolResult::success(vec![Content::text(format!("ERROR: {}", e))]))
        }
    }
}

/// Parse cargo test output into compact summary
fn parse_cargo_test_output(output: &str) -> String {
    use regex::Regex;

    let mut total_passed = 0;
    let mut total_failed = 0;
    let mut total_ignored = 0;
    let mut failed_tests: Vec<String> = Vec::new();
    let mut packages_tested: Vec<String> = Vec::new();

    // Match "test result: ok. X passed; Y failed; Z ignored"
    let result_re = Regex::new(r"test result: (ok|FAILED)\. (\d+) passed; (\d+) failed; (\d+) ignored").unwrap();
    // Match "Running unittests" or package name
    let pkg_re = Regex::new(r"Running (?:unittests |tests )?.*\(target/.*?/deps/(\w+)-").unwrap();
    // Match failed test names
    let failed_re = Regex::new(r"---- (\S+) stdout ----").unwrap();
    // Also catch "FAILED" lines
    let failed_line_re = Regex::new(r"^test (\S+) \.\.\. FAILED").unwrap();

    for line in output.lines() {
        if let Some(caps) = result_re.captures(line) {
            let passed: usize = caps.get(2).unwrap().as_str().parse().unwrap_or(0);
            let failed: usize = caps.get(3).unwrap().as_str().parse().unwrap_or(0);
            let ignored: usize = caps.get(4).unwrap().as_str().parse().unwrap_or(0);
            total_passed += passed;
            total_failed += failed;
            total_ignored += ignored;
        }

        if let Some(caps) = pkg_re.captures(line) {
            let pkg = caps.get(1).unwrap().as_str().to_string();
            if !packages_tested.contains(&pkg) {
                packages_tested.push(pkg);
            }
        }

        if let Some(caps) = failed_re.captures(line) {
            failed_tests.push(caps.get(1).unwrap().as_str().to_string());
        }

        if let Some(caps) = failed_line_re.captures(line) {
            let name = caps.get(1).unwrap().as_str().to_string();
            if !failed_tests.contains(&name) {
                failed_tests.push(name);
            }
        }
    }

    let mut result = String::new();

    // Status line
    if total_failed == 0 {
        result.push_str(&format!("OK: {} passed", total_passed));
    } else {
        result.push_str(&format!("FAILED: {} passed, {} failed", total_passed, total_failed));
    }

    if total_ignored > 0 {
        result.push_str(&format!(", {} ignored", total_ignored));
    }

    // Packages
    if !packages_tested.is_empty() {
        result.push_str(&format!("\npackages: {}", packages_tested.join(", ")));
    }

    // Failed tests (most important info)
    if !failed_tests.is_empty() {
        result.push_str("\n\nfailed tests:");
        for test in &failed_tests {
            result.push_str(&format!("\n  - {}", test));
        }
    }

    result
}

/// Parse beads JSON into compact format
fn parse_beads_json(json_str: &str, limit: usize) -> String {
    let beads: Vec<serde_json::Value> = match serde_json::from_str(json_str) {
        Ok(v) => v,
        Err(_) => return "No beads found".to_string(),
    };

    if beads.is_empty() {
        return "No open beads".to_string();
    }

    let mut result = String::new();
    result.push_str(&format!("{} open beads:\n", beads.len().min(limit)));

    for bead in beads.iter().take(limit) {
        let id = bead.get("id").and_then(|v| v.as_str()).unwrap_or("?");
        let title = bead.get("title").and_then(|v| v.as_str()).unwrap_or("?");
        let _status = bead.get("status").and_then(|v| v.as_str()).unwrap_or("?");
        let deps = bead.get("dependency_count").and_then(|v| v.as_u64()).unwrap_or(0);

        if deps > 0 {
            result.push_str(&format!("[{}] {} (blocked by {})\n", id, title, deps));
        } else {
            result.push_str(&format!("[{}] {}\n", id, title));
        }
    }

    result.trim().to_string()
}

/// Parse clippy JSON output into compact summary
fn parse_clippy_json(stdout: &str, stderr: &str) -> String {
    let mut warnings = 0;
    let mut errors = 0;
    let mut issues: Vec<String> = Vec::new();
    const MAX_ISSUES: usize = 10;

    // Parse JSON lines from stdout
    for line in stdout.lines() {
        if line.trim().is_empty() {
            continue;
        }
        if let Ok(msg) = serde_json::from_str::<serde_json::Value>(line) {
            if let Some(reason) = msg.get("reason").and_then(|r| r.as_str()) {
                if reason == "compiler-message" {
                    if let Some(message) = msg.get("message") {
                        let level = message.get("level").and_then(|l| l.as_str()).unwrap_or("");
                        let text = message.get("message").and_then(|m| m.as_str()).unwrap_or("");

                        match level {
                            "warning" => {
                                warnings += 1;
                                if issues.len() < MAX_ISSUES {
                                    // Get file location
                                    let loc = message.get("spans")
                                        .and_then(|s| s.as_array())
                                        .and_then(|arr| arr.first())
                                        .map(|span| {
                                            let file = span.get("file_name")
                                                .and_then(|f| f.as_str())
                                                .unwrap_or("?");
                                            let line = span.get("line_start")
                                                .and_then(|l| l.as_u64())
                                                .unwrap_or(0);
                                            format!("{}:{}", file, line)
                                        })
                                        .unwrap_or_default();

                                    if !text.starts_with("unused") || issues.len() < 5 {
                                        issues.push(format!("  warn: {} [{}]", text, loc));
                                    }
                                }
                            }
                            "error" => {
                                errors += 1;
                                if issues.len() < MAX_ISSUES {
                                    let loc = message.get("spans")
                                        .and_then(|s| s.as_array())
                                        .and_then(|arr| arr.first())
                                        .map(|span| {
                                            let file = span.get("file_name")
                                                .and_then(|f| f.as_str())
                                                .unwrap_or("?");
                                            let line = span.get("line_start")
                                                .and_then(|l| l.as_u64())
                                                .unwrap_or(0);
                                            format!("{}:{}", file, line)
                                        })
                                        .unwrap_or_default();

                                    issues.push(format!("  error: {} [{}]", text, loc));
                                }
                            }
                            _ => {}
                        }
                    }
                }
            }
        }
    }

    // Check for build errors in stderr
    if errors == 0 && warnings == 0 && stderr.contains("error") {
        return format!("BUILD ERROR:\n{}", stderr.lines().take(20).collect::<Vec<_>>().join("\n"));
    }

    let mut result = String::new();

    if errors == 0 && warnings == 0 {
        result.push_str("OK: no warnings or errors");
    } else if errors == 0 {
        result.push_str(&format!("OK: {} warnings", warnings));
    } else {
        result.push_str(&format!("FAILED: {} errors, {} warnings", errors, warnings));
    }

    if !issues.is_empty() {
        result.push('\n');
        result.push_str(&issues.join("\n"));
        if warnings + errors > MAX_ISSUES {
            result.push_str(&format!("\n  ... and {} more", warnings + errors - MAX_ISSUES));
        }
    }

    result
}

/// Parse cargo build JSON output into compact summary
fn parse_cargo_build_json(stdout: &str, stderr: &str, success: bool) -> String {
    let mut errors: Vec<String> = Vec::new();
    let mut warnings = 0;
    const MAX_ERRORS: usize = 5;

    // Parse JSON lines from stdout
    for line in stdout.lines() {
        if line.trim().is_empty() {
            continue;
        }
        if let Ok(msg) = serde_json::from_str::<serde_json::Value>(line) {
            if let Some(reason) = msg.get("reason").and_then(|r| r.as_str()) {
                if reason == "compiler-message" {
                    if let Some(message) = msg.get("message") {
                        let level = message.get("level").and_then(|l| l.as_str()).unwrap_or("");
                        let text = message.get("message").and_then(|m| m.as_str()).unwrap_or("");

                        match level {
                            "error" => {
                                if errors.len() < MAX_ERRORS {
                                    let loc = message.get("spans")
                                        .and_then(|s| s.as_array())
                                        .and_then(|arr| arr.first())
                                        .map(|span| {
                                            let file = span.get("file_name")
                                                .and_then(|f| f.as_str())
                                                .unwrap_or("?");
                                            let line = span.get("line_start")
                                                .and_then(|l| l.as_u64())
                                                .unwrap_or(0);
                                            format!("{}:{}", file, line)
                                        })
                                        .unwrap_or_default();

                                    errors.push(format!("  {} [{}]", text, loc));
                                }
                            }
                            "warning" => warnings += 1,
                            _ => {}
                        }
                    }
                }
            }
        }
    }

    let mut result = String::new();

    if success {
        result.push_str("OK: build succeeded");
        if warnings > 0 {
            result.push_str(&format!(" ({} warnings)", warnings));
        }
    } else {
        result.push_str(&format!("FAILED: {} errors", errors.len()));
        if warnings > 0 {
            result.push_str(&format!(", {} warnings", warnings));
        }
        if !errors.is_empty() {
            result.push('\n');
            result.push_str(&errors.join("\n"));
        } else if !stderr.is_empty() {
            // Fallback to stderr if no JSON errors found
            result.push_str("\n");
            result.push_str(&stderr.lines().take(10).collect::<Vec<_>>().join("\n"));
        }
    }

    result
}

/// Parse swift test output into compact summary
fn parse_swift_test_output(output: &str, success: bool) -> String {
    use regex::Regex;

    let mut passed = 0;
    let mut failed = 0;
    let mut failed_tests: Vec<String> = Vec::new();

    // Match XCTest output: "Executed N tests, with M failures"
    let xctest_re = Regex::new(r"Executed (\d+) tests?, with (\d+) failures?").unwrap();
    // Match swift-testing output: "N tests passed" or "Test run with N tests"
    let swift_testing_re = Regex::new(r"(\d+) tests? passed").unwrap();
    // Match failed test case names
    let failed_re = Regex::new(r"Test Case.*\[(\S+)\].*failed").unwrap();
    // Also match swift-testing failures
    let swift_failed_re = Regex::new(r"✘ Test .* failed").unwrap();

    for line in output.lines() {
        if let Some(caps) = xctest_re.captures(line) {
            let total: usize = caps.get(1).unwrap().as_str().parse().unwrap_or(0);
            let failures: usize = caps.get(2).unwrap().as_str().parse().unwrap_or(0);
            passed += total - failures;
            failed += failures;
        }

        if let Some(caps) = swift_testing_re.captures(line) {
            let count: usize = caps.get(1).unwrap().as_str().parse().unwrap_or(0);
            passed += count;
        }

        if let Some(caps) = failed_re.captures(line) {
            let name = caps.get(1).unwrap().as_str().to_string();
            if !failed_tests.contains(&name) && failed_tests.len() < 10 {
                failed_tests.push(name);
            }
        }

        if swift_failed_re.is_match(line) && failed_tests.len() < 10 {
            // Extract test name from swift-testing failure line
            let test_name = line.split("Test ").nth(1)
                .and_then(|s| s.split(" failed").next())
                .unwrap_or("unknown")
                .to_string();
            if !failed_tests.contains(&test_name) {
                failed_tests.push(test_name);
            }
        }
    }

    let mut result = String::new();

    if success && failed == 0 {
        result.push_str(&format!("OK: {} passed", passed));
    } else if failed > 0 {
        result.push_str(&format!("FAILED: {} passed, {} failed", passed, failed));
        if !failed_tests.is_empty() {
            result.push_str("\nfailed:");
            for test in &failed_tests {
                result.push_str(&format!("\n  - {}", test));
            }
        }
    } else if !success {
        // Build failed or other error
        let error_lines: Vec<&str> = output.lines()
            .filter(|l| l.contains("error:") || l.contains("Error:"))
            .take(5)
            .collect();
        if error_lines.is_empty() {
            result.push_str("FAILED: build or test error (check output)");
        } else {
            result.push_str("FAILED:\n");
            result.push_str(&error_lines.join("\n"));
        }
    } else {
        result.push_str(&format!("OK: {} passed", passed));
    }

    result
}

#[tool_handler]
impl ServerHandler for HemisServer {
    fn get_info(&self) -> ServerInfo {
        ServerInfo {
            protocol_version: ProtocolVersion::V_2024_11_05,
            capabilities: ServerCapabilities {
                tools: Some(ToolsCapability {
                    list_changed: None,
                }),
                ..Default::default()
            },
            server_info: Implementation {
                name: "hemis-mcp".to_string(),
                version: SERVER_VERSION.to_string(),
                title: None,
                icons: None,
                website_url: None,
            },
            instructions: Some("Hemis development tools for compact output".to_string()),
        }
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let server = HemisServer::new();
    let service = server.serve(stdio()).await?;
    service.waiting().await?;
    Ok(())
}
