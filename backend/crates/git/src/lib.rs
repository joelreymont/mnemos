//! git: commit/blob helpers using git2 with CLI fallback.

use anyhow::Result;
use std::path::Path;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GitInfo {
    pub root: String,
    pub commit: String,
    pub blob: Option<String>,
}

pub fn info_for_file(path: &str) -> Option<GitInfo> {
    info_git2(path).or_else(|| info_cli(path).ok()).flatten()
}

/// Get the HEAD commit SHA for a directory (typically project root).
pub fn head_commit(dir: &Path) -> Option<String> {
    // Try git2 first
    if let Ok(repo) = git2::Repository::discover(dir) {
        if let Ok(head) = repo.head() {
            if let Some(oid) = head.target() {
                return Some(oid.to_string());
            }
        }
    }
    // Fall back to CLI
    cmd(dir, &["rev-parse", "HEAD"]).ok()
}

/// Get the git root directory for a path.
pub fn find_root(path: &Path) -> Option<String> {
    // Try git2 first
    if let Ok(repo) = git2::Repository::discover(path) {
        if let Some(workdir) = repo.workdir() {
            return Some(workdir.to_string_lossy().into_owned());
        }
    }
    // Fall back to CLI
    cmd(path, &["rev-parse", "--show-toplevel"]).ok()
}

fn info_git2(path: &str) -> Option<Option<GitInfo>> {
    let p = Path::new(path);
    let repo = git2::Repository::discover(p).ok()?;
    let root = repo.workdir()?.to_path_buf();
    let head = repo.head().ok()?.target()?;
    let commit = head.to_string();
    let blob = repo.blob_path(p).ok().map(|id| id.to_string());
    Some(Some(GitInfo {
        root: root.to_string_lossy().into_owned(),
        commit,
        blob,
    }))
}

fn info_cli(path: &str) -> Result<Option<GitInfo>> {
    if std::process::Command::new("git")
        .arg("--version")
        .output()
        .is_err()
    {
        return Ok(None);
    }
    let p = Path::new(path);
    let dir = p.parent().unwrap_or_else(|| Path::new("."));
    let root = cmd(dir, &["rev-parse", "--show-toplevel"])?;
    let commit = cmd(dir, &["rev-parse", "HEAD"])?;
    let blob = cmd(dir, &["hash-object", path]).ok();
    Ok(Some(GitInfo { root, commit, blob }))
}

fn cmd(dir: &Path, args: &[&str]) -> Result<String> {
    let out = std::process::Command::new("git")
        .args(args)
        .current_dir(dir)
        .output()?;
    if out.status.success() {
        // Validate UTF-8 explicitly instead of using lossy conversion
        let output = String::from_utf8(out.stdout)
            .map_err(|_| anyhow::anyhow!("git output is not valid UTF-8"))?
            .trim()
            .to_string();
        // Basic validation: output should be non-empty and reasonable length
        // Git SHAs are 40 hex chars (SHA1) or 64 hex chars (SHA256)
        // Paths can be longer, so just cap at reasonable limit
        const MAX_OUTPUT_LEN: usize = 4096;
        if output.len() > MAX_OUTPUT_LEN {
            anyhow::bail!("git output too long");
        }
        Ok(output)
    } else {
        anyhow::bail!("git failed")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::tempdir;

    #[test]
    fn cli_info_returns_error_for_non_repo() {
        let dir = tempdir().unwrap();
        let file = dir.path().join("f.txt");
        fs::write(&file, "hi").unwrap();
        // info_cli calls rev-parse which fails outside a repo
        let result = info_cli(file.to_str().unwrap());
        assert!(result.is_err(), "info_cli should error on non-repo path");
    }

    #[test]
    fn info_for_file_returns_none_for_non_repo() {
        let dir = tempdir().unwrap();
        let file = dir.path().join("f.txt");
        fs::write(&file, "hi").unwrap();
        // info_for_file should gracefully return None for non-repo paths
        let result = info_for_file(file.to_str().unwrap());
        assert!(result.is_none(), "info_for_file should return None for non-repo path");
    }
}
