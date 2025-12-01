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
        Ok(String::from_utf8_lossy(&out.stdout).trim().to_string())
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
    fn cli_info_handles_non_repo() {
        let dir = tempdir().unwrap();
        let file = dir.path().join("f.txt");
        fs::write(&file, "hi").unwrap();
        // On systems with global git config, `hash-object` works even outside a repo,
        // so just ensure we don't error.
        let _ = info_cli(file.to_str().unwrap()).unwrap();
    }
}
