//! File watcher for detecting external changes.
//!
//! Watches project directories for file changes made outside the editor
//! (e.g., git checkout, external editor). When files with notes change,
//! notifies connected clients to refresh their note positions.

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use notify::{Config, Event, EventKind, RecommendedWatcher, RecursiveMode, Watcher};

/// Debounce duration for file changes (coalesce rapid changes)
const DEBOUNCE_MS: u64 = 100;

/// File change event sent to listeners
#[derive(Debug, Clone)]
pub struct FileChange {
    pub path: PathBuf,
    pub kind: FileChangeKind,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FileChangeKind {
    Modified,
    Created,
    Deleted,
}

/// Callback type for file change notifications (Arc for cloning across threads)
pub type ChangeCallback = Arc<dyn Fn(FileChange) + Send + Sync>;

/// File watcher that monitors directories for external changes.
pub struct FileWatcher {
    watcher: RecommendedWatcher,
    watched_dirs: Arc<Mutex<HashSet<PathBuf>>>,
    #[allow(dead_code)]
    pending_changes: Arc<Mutex<HashMap<PathBuf, (FileChangeKind, Instant)>>>,
    callbacks: Arc<Mutex<Vec<ChangeCallback>>>,
    _processor_handle: std::thread::JoinHandle<()>,
}

impl FileWatcher {
    /// Create a new file watcher.
    pub fn new() -> notify::Result<Self> {
        let pending_changes: Arc<Mutex<HashMap<PathBuf, (FileChangeKind, Instant)>>> =
            Arc::new(Mutex::new(HashMap::new()));
        let callbacks: Arc<Mutex<Vec<ChangeCallback>>> = Arc::new(Mutex::new(Vec::new()));

        let pending_for_handler = pending_changes.clone();

        // Create the notify watcher
        let watcher = RecommendedWatcher::new(
            move |res: notify::Result<Event>| {
                if let Ok(event) = res {
                    let kind = match event.kind {
                        EventKind::Create(_) => Some(FileChangeKind::Created),
                        EventKind::Modify(_) => Some(FileChangeKind::Modified),
                        EventKind::Remove(_) => Some(FileChangeKind::Deleted),
                        _ => None,
                    };

                    if let Some(kind) = kind {
                        let mut pending = pending_for_handler.lock().unwrap();
                        for path in event.paths {
                            // Only track files, not directories
                            if path.is_file() || kind == FileChangeKind::Deleted {
                                pending.insert(path, (kind, Instant::now()));
                            }
                        }
                    }
                }
            },
            Config::default(),
        )?;

        // Spawn processor thread that handles debouncing
        let pending_for_processor = pending_changes.clone();
        let callbacks_for_processor = callbacks.clone();

        let processor_handle = std::thread::spawn(move || {
            loop {
                std::thread::sleep(Duration::from_millis(50));

                let now = Instant::now();
                let debounce_threshold = Duration::from_millis(DEBOUNCE_MS);

                // Collect changes that have been stable long enough
                let ready_changes: Vec<FileChange> = {
                    let mut pending = pending_for_processor.lock().unwrap();
                    let ready: Vec<_> = pending
                        .iter()
                        .filter(|(_, (_, timestamp))| now.duration_since(*timestamp) >= debounce_threshold)
                        .map(|(path, (kind, _))| FileChange {
                            path: path.clone(),
                            kind: *kind,
                        })
                        .collect();

                    // Remove processed changes
                    for change in &ready {
                        pending.remove(&change.path);
                    }

                    ready
                };

                // Notify callbacks
                if !ready_changes.is_empty() {
                    // Clone callbacks to release lock before invoking (prevents blocking registration)
                    let callbacks: Vec<_> = {
                        let guard = callbacks_for_processor.lock().unwrap();
                        guard.clone()
                    };
                    for change in ready_changes {
                        for callback in &callbacks {
                            // Use catch_unwind to prevent panics from poisoning the mutex
                            let change_clone = change.clone();
                            let _ = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                                callback(change_clone);
                            }));
                        }
                    }
                }
            }
        });

        Ok(Self {
            watcher,
            watched_dirs: Arc::new(Mutex::new(HashSet::new())),
            pending_changes,
            callbacks,
            _processor_handle: processor_handle,
        })
    }

    /// Watch a directory for changes.
    pub fn watch(&mut self, path: &Path) -> notify::Result<()> {
        let canonical = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());

        let mut watched = self.watched_dirs.lock().unwrap();
        if watched.contains(&canonical) {
            return Ok(()); // Already watching
        }

        self.watcher.watch(&canonical, RecursiveMode::Recursive)?;
        watched.insert(canonical);
        Ok(())
    }

    /// Stop watching a directory.
    pub fn unwatch(&mut self, path: &Path) -> notify::Result<()> {
        let canonical = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());

        let mut watched = self.watched_dirs.lock().unwrap();
        if watched.remove(&canonical) {
            self.watcher.unwatch(&canonical)?;
        }
        Ok(())
    }

    /// Register a callback for file changes.
    pub fn on_change(&self, callback: ChangeCallback) {
        let mut callbacks = self.callbacks.lock().unwrap();
        callbacks.push(callback);
    }

    /// Get list of watched directories.
    pub fn watched_dirs(&self) -> Vec<PathBuf> {
        let watched = self.watched_dirs.lock().unwrap();
        watched.iter().cloned().collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::sync::mpsc::channel;
    use tempfile::tempdir;

    #[test]
    fn test_watcher_creation() {
        let watcher = FileWatcher::new();
        assert!(watcher.is_ok());
    }

    #[test]
    fn test_watch_directory() {
        let dir = tempdir().unwrap();
        let mut watcher = FileWatcher::new().unwrap();

        let result = watcher.watch(dir.path());
        assert!(result.is_ok());
        assert_eq!(watcher.watched_dirs().len(), 1);
    }

    #[test]
    fn test_detects_file_change() {
        let dir = tempdir().unwrap();
        let file_path = dir.path().join("test.txt");
        fs::write(&file_path, "initial").unwrap();

        let mut watcher = FileWatcher::new().unwrap();
        watcher.watch(dir.path()).unwrap();

        let (tx, rx) = channel();
        watcher.on_change(Arc::new(move |change| {
            let _ = tx.send(change);
        }));

        // Modify the file
        std::thread::sleep(Duration::from_millis(50));
        fs::write(&file_path, "modified").unwrap();

        // Wait for debounced notification
        let change = rx.recv_timeout(Duration::from_secs(2));
        assert!(change.is_ok());
        let change = change.unwrap();
        assert_eq!(change.kind, FileChangeKind::Modified);
    }
}
