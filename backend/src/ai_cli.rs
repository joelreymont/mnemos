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
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

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

/// Ensure `<project_root>/.hemis` exists and return its path.
fn ensure_hemis_dir(project_root: &Path) -> Result<PathBuf> {
    let hemis = project_root.join(".hemis");
    fs::create_dir_all(&hemis).with_context(|| {
        format!(
            "failed to create .hemis directory at {}",
            hemis.display()
        )
    })?;
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

    // Remove ```markdown ... ``` wrapper
    if trimmed.starts_with("```markdown") {
        if let Some(end) = trimmed.rfind("```") {
            let inner = &trimmed[11..end]; // skip "```markdown"
            return inner.trim().to_string();
        }
    }

    // Remove ``` ... ``` wrapper
    if trimmed.starts_with("```") && trimmed.ends_with("```") && trimmed.len() > 6 {
        let inner = &trimmed[3..trimmed.len()-3];
        // Skip language identifier on first line if present
        let inner = if let Some(idx) = inner.find('\n') {
            &inner[idx+1..]
        } else {
            inner
        };
        return inner.trim().to_string();
    }

    trimmed.to_string()
}

/// Explain a region of code using the configured CLI provider.
///
/// Returns (provider, explanation, had_analysis_context).
/// The caller is responsible for creating any notes based on the explanation.
pub fn explain_region(
    project_root: &Path,
    file: &str,
    start_line: usize,
    end_line: usize,
    snippet: &str,
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

    let prompt = format!(
        r#"You are Hemis, an assistant embedded in a codebase-aware tool.

{analysis_section}Explain the following code region.

**File:** {file}
**Lines:** {start}-{end}

```
{snippet}
```

Provide a clear, concise explanation covering:
1. What this code does
2. How it fits into the project architecture
3. Important invariants or assumptions
4. Potential pitfalls a maintainer should know

Keep the explanation focused and practical. Output plain text only.
"#,
        analysis_section = analysis_section,
        file = file,
        start = start_line,
        end = end_line,
        snippet = snippet
    );

    let explanation = match provider {
        CliProvider::Codex => run_codex(project_root, &prompt)?,
        CliProvider::Claude => run_claude(project_root, &prompt)?,
    };

    Ok((provider, explanation.trim().to_string(), has_analysis))
}

/// Helper: run Codex CLI with a prompt in the given project root.
fn run_codex(project_root: &Path, prompt: &str) -> Result<String> {
    // Create temp file for output since codex prints progress/diagnostics to stdout
    let tmp_dir = std::env::temp_dir();
    let output_path = tmp_dir.join(format!("hemis-codex-{}.txt", std::process::id()));

    // codex exec <prompt> --output-last-message <file>
    // The prompt is passed as a positional argument, not with -p (that's for profile)
    let output = Command::new("codex")
        .arg("exec")
        .arg("--skip-git-repo-check")
        .arg("--sandbox")
        .arg("read-only")
        .arg("--output-last-message")
        .arg(&output_path)
        .arg(prompt)
        .current_dir(project_root)
        .output()
        .with_context(|| "failed to spawn `codex` CLI")?;

    // Read output before checking status (file may exist even on failure)
    let result = std::fs::read_to_string(&output_path);
    // Clean up temp file
    let _ = std::fs::remove_file(&output_path);

    if !output.status.success() {
        return Err(anyhow!(
            "`codex exec` failed with status {:?}: {}",
            output.status.code(),
            String::from_utf8_lossy(&output.stderr)
        ));
    }

    result.with_context(|| format!("failed to read codex output from {}", output_path.display()))
}

/// Helper: run Claude Code CLI with a prompt in the given project root.
fn run_claude(project_root: &Path, prompt: &str) -> Result<String> {
    let output = Command::new("claude")
        .arg("-p")
        .arg(prompt)
        .current_dir(project_root)
        .output()
        .with_context(|| "failed to spawn `claude` CLI")?;

    if !output.status.success() {
        return Err(anyhow!(
            "`claude -p` failed with status {:?}: {}",
            output.status.code(),
            String::from_utf8_lossy(&output.stderr)
        ));
    }

    Ok(String::from_utf8_lossy(&output.stdout).to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

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
    fn provider_from_env_respects_disabled() {
        std::env::set_var("HEMIS_AI_PROVIDER", "none");
        assert!(CliProvider::from_env().is_none());
        std::env::remove_var("HEMIS_AI_PROVIDER");
    }
}
