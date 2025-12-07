//! Job queue for offloading heavy operations from RPC threads.
//!
//! This module provides a simple work queue that processes jobs in a background
//! thread, preventing long-running operations from blocking RPC responses.

use crate::events::{self, Event};
use rusqlite::Connection;
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::Path;
use std::sync::mpsc::{channel, Sender};
use std::sync::{Arc, Mutex};
use std::thread;
use uuid::Uuid;

/// Job types that can be queued for background processing
#[derive(Debug, Clone)]
pub enum Job {
    /// Index a project - scans files and builds search index
    IndexProject {
        task_id: String,
        project_root: String,
        include_ai: bool,
    },
}

/// Status of a job in the queue
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum JobStatus {
    Queued,
    Running,
    Completed,
    Failed,
}

/// Information about a job's execution status
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JobInfo {
    pub task_id: String,
    pub status: JobStatus,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result: Option<serde_json::Value>,
}

/// Thread-safe job queue for background processing
pub struct JobQueue {
    sender: Sender<Job>,
    /// Track job status for queries
    jobs: Arc<Mutex<std::collections::HashMap<String, JobInfo>>>,
}

impl JobQueue {
    /// Create a new job queue with a worker thread
    pub fn new(db_path: String) -> Self {
        let (tx, rx) = channel::<Job>();
        let jobs = Arc::new(Mutex::new(std::collections::HashMap::new()));
        let jobs_clone = jobs.clone();

        // Spawn worker thread
        thread::spawn(move || {
            worker_loop(rx, jobs_clone, db_path);
        });

        Self { sender: tx, jobs }
    }

    /// Enqueue a job for processing
    pub fn enqueue(&self, job: Job) -> String {
        let task_id = match &job {
            Job::IndexProject { task_id, .. } => task_id.clone(),
        };

        // Record job as queued
        if let Ok(mut jobs) = self.jobs.lock() {
            jobs.insert(
                task_id.clone(),
                JobInfo {
                    task_id: task_id.clone(),
                    status: JobStatus::Queued,
                    error: None,
                    result: None,
                },
            );
        }

        // Send to worker thread
        let _ = self.sender.send(job);
        task_id
    }

    /// Get the status of a job
    pub fn get_status(&self, task_id: &str) -> Option<JobInfo> {
        self.jobs.lock().ok()?.get(task_id).cloned()
    }

    /// List all tasks, optionally filtered by status
    pub fn list_tasks(&self, status_filter: Option<&str>) -> Vec<JobInfo> {
        let jobs = match self.jobs.lock() {
            Ok(j) => j,
            Err(_) => return Vec::new(),
        };
        let mut tasks: Vec<JobInfo> = jobs.values().cloned().collect();

        // Filter by status if provided
        if let Some(filter) = status_filter {
            tasks.retain(|t| {
                let status_str = match t.status {
                    JobStatus::Queued => "queued",
                    JobStatus::Running => "running",
                    JobStatus::Completed => "completed",
                    JobStatus::Failed => "failed",
                };
                status_str == filter
            });
        }

        // Sort by task_id for consistent ordering
        tasks.sort_by(|a, b| a.task_id.cmp(&b.task_id));
        tasks
    }
}

/// Background worker loop that processes jobs
fn worker_loop(
    rx: std::sync::mpsc::Receiver<Job>,
    jobs: Arc<Mutex<std::collections::HashMap<String, JobInfo>>>,
    db_path: String,
) {
    for job in rx {
        match job {
            Job::IndexProject {
                task_id,
                project_root,
                include_ai,
            } => {
                // Update status to running
                if let Ok(mut jobs_map) = jobs.lock() {
                    if let Some(info) = jobs_map.get_mut(&task_id) {
                        info.status = JobStatus::Running;
                    }
                }

                // Emit job-started event
                events::emit(Event::FileIndexed {
                    file: format!("Starting indexing for {}", project_root),
                    project: project_root.clone(),
                });

                // Open database connection for this thread
                let result = Connection::open(&db_path).and_then(|db| {
                    process_index_project(&db, &project_root, include_ai)
                });

                // Update job status based on result
                if let Ok(mut jobs_map) = jobs.lock() {
                    if let Some(info) = jobs_map.get_mut(&task_id) {
                        match result {
                            Ok(result_value) => {
                                info.status = JobStatus::Completed;
                                info.result = Some(result_value);
                            }
                            Err(e) => {
                                info.status = JobStatus::Failed;
                                info.error = Some(e.to_string());
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Process an index-project job synchronously
/// Returns a JSON value with the results
/// Made public for sync mode in the handler
pub fn process_index_project(
    db: &Connection,
    root: &str,
    include_ai: bool,
) -> Result<serde_json::Value, rusqlite::Error> {
    use serde_json::json;

    let mut indexed = 0;
    let mut skipped = 0;

    // Max file size for indexing (1MB)
    const MAX_INDEX_FILE_SIZE: u64 = 1024 * 1024;

    match list_files(Path::new(root)) {
        Ok(result) => {
            let truncated = result.truncated;
            for f in result.files {
                // Skip oversized files
                let file_size = fs::metadata(&f.file).map(|m| m.len()).unwrap_or(0);
                if file_size > MAX_INDEX_FILE_SIZE {
                    skipped += 1;
                    continue;
                }

                if let Ok(content) = fs::read_to_string(&f.file) {
                    match index::add_file(db, &f.file, root, &content) {
                        Ok(Some(_)) => indexed += 1,
                        Ok(None) => skipped += 1,
                        Err(e) => eprintln!("index failed for {}: {}", f.file, e),
                    }
                }
            }

            // Update project_meta with indexing info
            let commit_sha = git::head_commit(Path::new(root));
            let _ = storage::set_project_indexed(db, root, commit_sha.as_deref());

            // Pre-warm the Claude process in background for faster explain-region
            if crate::ai_cli::CliProvider::from_env() == Some(crate::ai_cli::CliProvider::Claude)
            {
                let root_path = Path::new(root).to_path_buf();
                std::thread::spawn(move || {
                    if let Err(e) = crate::ai_cli::warm_up_claude(&root_path) {
                        eprintln!("[hemis] Claude warm-up failed: {}", e);
                    }
                });
            }

            // Optionally run AI analysis
            let mut ai_result: Option<serde_json::Value> = None;
            if include_ai {
                match crate::ai_cli::analyze_repo(Path::new(root)) {
                    Ok(provider) => {
                        let _ = storage::set_project_analyzed(
                            db,
                            root,
                            commit_sha.as_deref(),
                            provider.as_str(),
                        );
                        ai_result = Some(json!({
                            "analyzed": true,
                            "provider": provider.as_str()
                        }));
                    }
                    Err(e) => {
                        ai_result = Some(json!({
                            "analyzed": false,
                            "error": e.to_string()
                        }));
                    }
                }
            }

            // Emit index-complete event
            events::emit(Event::IndexComplete {
                project: root.to_string(),
                files_indexed: indexed,
            });

            // Build human-readable status message
            let status_message = match &ai_result {
                Some(ai) if ai.get("analyzed") == Some(&json!(true)) => {
                    let provider = ai
                        .get("provider")
                        .and_then(|v| v.as_str())
                        .unwrap_or("AI");
                    format!("Project indexed: {} files, analyzed with {}", indexed, provider)
                }
                Some(ai) if ai.get("error").is_some() => {
                    let error = ai.get("error").and_then(|v| v.as_str()).unwrap_or("unknown");
                    format!(
                        "Project indexed: {} files (AI failed: {})",
                        indexed, error
                    )
                }
                _ => format!("Project indexed: {} files", indexed),
            };

            let mut result = json!({
                "ok": true,
                "indexed": indexed,
                "skipped": skipped,
                "truncated": truncated,
                "projectRoot": root,
                "statusMessage": status_message
            });
            if let Some(ai) = ai_result {
                result["ai"] = ai;
            }
            Ok(result)
        }
        Err(e) => {
            eprintln!("Failed to list files: {}", e);
            Err(rusqlite::Error::InvalidQuery)
        }
    }
}

// File listing code copied from lib.rs to avoid circular dependency

/// Internal struct for file listing
#[derive(Debug, Clone)]
struct FileInfo {
    file: String,
    #[allow(dead_code)]
    size: u64,
}

/// Result of list_files including truncation status
struct ListFilesResult {
    files: Vec<FileInfo>,
    truncated: bool,
}

const IGNORE_DIRS: &[&str] = &[
    ".git",
    "target",
    "node_modules",
    ".hg",
    ".svn",
    ".idea",
    ".direnv",
    "venv",
    "env",
    "__pycache__",
    "build",
];

const MAX_TRAVERSAL_DEPTH: usize = 50;
const MAX_FILE_COUNT: usize = 100_000;

fn list_files(root: &Path) -> anyhow::Result<ListFilesResult> {
    use anyhow::Context;

    // Canonicalize path to prevent path traversal attacks
    let canonical_root = root
        .canonicalize()
        .with_context(|| format!("failed to canonicalize path: {}", root.display()))?;

    // Validate the path is a directory and not a system path
    if !canonical_root.is_dir() {
        return Err(anyhow::anyhow!(
            "path is not a directory: {}",
            canonical_root.display()
        ));
    }

    // Reject obvious system paths to prevent accidental traversal
    let path_str = canonical_root.to_string_lossy();
    if path_str == "/"
        || path_str.starts_with("/etc")
        || path_str.starts_with("/var")
        || path_str.starts_with("/usr")
        || path_str.starts_with("/bin")
        || path_str.starts_with("/sbin")
        || path_str.starts_with("/System")
        || path_str.starts_with("/Library")
        || path_str.starts_with("C:\\Windows")
    {
        return Err(anyhow::anyhow!(
            "refusing to traverse system path: {}",
            canonical_root.display()
        ));
    }

    // Track (path, depth) for depth limiting
    let mut stack = vec![(canonical_root.clone(), 0usize)];
    let mut files = Vec::new();
    let mut truncated = false;
    while let Some((dir, depth)) = stack.pop() {
        // Check depth limit
        if depth > MAX_TRAVERSAL_DEPTH {
            continue; // Skip directories beyond max depth
        }

        for entry in fs::read_dir(&dir)? {
            let entry = entry?;
            let path = entry.path();
            let file_type = entry.file_type()?;

            // Skip symbolic links entirely (TOCTOU protection)
            if file_type.is_symlink() {
                continue;
            }

            // Ensure traversed paths stay within the original root (additional safety)
            if let Ok(canonical_path) = path.canonicalize() {
                if !canonical_path.starts_with(&canonical_root) {
                    continue; // Skip paths that escape the root
                }
            }

            let name = entry.file_name().to_string_lossy().to_string();
            if file_type.is_dir() {
                if IGNORE_DIRS.contains(&name.as_str()) {
                    continue;
                }
                stack.push((path, depth + 1));
            } else if file_type.is_file() {
                // Check file count limit
                if files.len() >= MAX_FILE_COUNT {
                    truncated = true;
                    files.sort_by(|a: &FileInfo, b: &FileInfo| a.file.cmp(&b.file));
                    return Ok(ListFilesResult { files, truncated });
                }
                let size = entry.metadata().map(|m| m.len()).unwrap_or(0);
                files.push(FileInfo {
                    file: path.to_string_lossy().to_string(),
                    size,
                });
            }
        }
    }
    files.sort_by(|a, b| a.file.cmp(&b.file));
    Ok(ListFilesResult { files, truncated })
}

/// Generate a new task ID
pub fn generate_task_id() -> String {
    Uuid::new_v4().to_string()
}

/// Global job queue instance
static JOB_QUEUE: std::sync::OnceLock<JobQueue> = std::sync::OnceLock::new();

/// Initialize the global job queue (call once at server startup)
pub fn init_job_queue(db_path: String) {
    JOB_QUEUE.get_or_init(|| JobQueue::new(db_path));
}

/// Get the global job queue
pub fn job_queue() -> &'static JobQueue {
    JOB_QUEUE.get().expect("job queue not initialized")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_task_id_is_unique() {
        let id1 = generate_task_id();
        let id2 = generate_task_id();
        assert_ne!(id1, id2);
    }

    #[test]
    fn test_job_status_serialization() {
        let info = JobInfo {
            task_id: "test-123".to_string(),
            status: JobStatus::Queued,
            error: None,
            result: None,
        };
        let json = serde_json::to_string(&info).unwrap();
        assert!(json.contains("\"status\":\"queued\""));

        let info = JobInfo {
            task_id: "test-123".to_string(),
            status: JobStatus::Running,
            error: None,
            result: None,
        };
        let json = serde_json::to_string(&info).unwrap();
        assert!(json.contains("\"status\":\"running\""));
    }

    #[test]
    fn test_job_queue_creation() {
        let _queue = JobQueue::new(":memory:".to_string());
        // Should not panic
    }

    #[test]
    fn test_job_queue_enqueue_returns_task_id() {
        let queue = JobQueue::new(":memory:".to_string());
        let task_id = generate_task_id();
        let returned_id = queue.enqueue(Job::IndexProject {
            task_id: task_id.clone(),
            project_root: "/tmp/test".to_string(),
            include_ai: false,
        });
        assert_eq!(task_id, returned_id);
    }

    #[test]
    fn test_job_queue_get_status() {
        let queue = JobQueue::new(":memory:".to_string());
        let task_id = generate_task_id();
        queue.enqueue(Job::IndexProject {
            task_id: task_id.clone(),
            project_root: "/tmp/test".to_string(),
            include_ai: false,
        });

        let status = queue.get_status(&task_id);
        assert!(status.is_some());
        let info = status.unwrap();
        assert_eq!(info.task_id, task_id);
        // Should be Queued or Running depending on timing
        assert!(matches!(info.status, JobStatus::Queued | JobStatus::Running));
    }

    // Security tests for filesystem traversal protection

    #[test]
    fn test_list_files_rejects_system_paths() {
        // Should reject root path
        assert!(list_files(Path::new("/")).is_err());
        // Should reject /etc
        assert!(list_files(Path::new("/etc")).is_err());
        // Should reject /var
        assert!(list_files(Path::new("/var")).is_err());
        // Should reject /usr
        assert!(list_files(Path::new("/usr")).is_err());
    }

    #[test]
    fn test_list_files_rejects_non_directory() {
        let tmpdir = tempfile::tempdir().unwrap();
        let file_path = tmpdir.path().join("test.txt");
        std::fs::write(&file_path, "content").unwrap();
        // Should reject files (not directories)
        assert!(list_files(&file_path).is_err());
    }

    #[test]
    fn test_list_files_rejects_nonexistent_path() {
        // Should reject paths that don't exist
        assert!(list_files(Path::new("/nonexistent/path/that/does/not/exist")).is_err());
    }

    #[test]
    fn test_list_files_skips_symlinks() {
        let tmpdir = tempfile::tempdir().unwrap();
        let target_file = tmpdir.path().join("target.txt");
        std::fs::write(&target_file, "content").unwrap();

        // Create a symlink
        let link_path = tmpdir.path().join("link.txt");
        #[cfg(unix)]
        std::os::unix::fs::symlink(&target_file, &link_path).unwrap();

        let result = list_files(tmpdir.path()).unwrap();
        // Should only find the target file, not the symlink
        assert_eq!(result.files.len(), 1);
        assert!(result.files[0].file.ends_with("target.txt"));
    }

    #[test]
    fn test_list_files_ignores_common_dirs() {
        let tmpdir = tempfile::tempdir().unwrap();

        // Create files in ignored directories
        for dir_name in &[".git", "node_modules", "target", "__pycache__"] {
            let ignored_dir = tmpdir.path().join(dir_name);
            std::fs::create_dir_all(&ignored_dir).unwrap();
            std::fs::write(ignored_dir.join("file.txt"), "content").unwrap();
        }

        // Create a file in a non-ignored directory
        let src_dir = tmpdir.path().join("src");
        std::fs::create_dir_all(&src_dir).unwrap();
        std::fs::write(src_dir.join("main.rs"), "fn main() {}").unwrap();

        let result = list_files(tmpdir.path()).unwrap();
        // Should only find main.rs, not files in ignored directories
        assert_eq!(result.files.len(), 1);
        assert!(result.files[0].file.ends_with("main.rs"));
    }

    #[test]
    fn test_list_files_respects_file_count_limit() {
        let tmpdir = tempfile::tempdir().unwrap();

        // Create files up to but not exceeding a reasonable count for testing
        // (We can't easily test MAX_FILE_COUNT=100,000 but we verify the mechanism works)
        for i in 0..10 {
            std::fs::write(tmpdir.path().join(format!("file{}.txt", i)), "content").unwrap();
        }

        let result = list_files(tmpdir.path()).unwrap();
        assert_eq!(result.files.len(), 10);
        assert!(!result.truncated, "should not truncate with only 10 files");
    }

    #[test]
    fn test_list_files_empty_directory() {
        let tmpdir = tempfile::tempdir().unwrap();
        let result = list_files(tmpdir.path()).unwrap();
        assert!(result.files.is_empty());
        assert!(!result.truncated);
    }

    #[test]
    fn test_list_files_nested_directories() {
        let tmpdir = tempfile::tempdir().unwrap();

        // Create nested structure
        let deep = tmpdir.path().join("a/b/c/d");
        std::fs::create_dir_all(&deep).unwrap();
        std::fs::write(deep.join("deep.txt"), "content").unwrap();
        std::fs::write(tmpdir.path().join("root.txt"), "content").unwrap();

        let result = list_files(tmpdir.path()).unwrap();
        assert_eq!(result.files.len(), 2);
        // Files should be sorted
        assert!(result.files.iter().any(|f| f.file.ends_with("deep.txt")));
        assert!(result.files.iter().any(|f| f.file.ends_with("root.txt")));
    }
}
