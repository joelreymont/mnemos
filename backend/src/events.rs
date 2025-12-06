//! Event broadcasting system for hemis.
//!
//! Provides a push-based notification mechanism via a Unix socket.
//! Clients connect to ~/.hemis/events.sock and receive JSON-lines events.

use serde::Serialize;
use std::collections::HashMap;
use std::fs;
use std::io::Write;
use std::os::unix::net::{UnixListener, UnixStream};
use std::path::PathBuf;
use std::sync::mpsc::{self, Receiver, SyncSender};
use std::sync::{Arc, Mutex, RwLock};
use std::thread;

/// Event types that can be broadcast to listeners.
#[derive(Debug, Clone, Serialize)]
#[serde(tag = "type", rename_all = "kebab-case")]
pub enum Event {
    /// A note was created
    NoteCreated {
        id: String,
        file: String,
        line: i64,
        #[serde(skip_serializing_if = "Option::is_none")]
        project_root: Option<String>,
    },
    /// A note was updated
    NoteUpdated {
        id: String,
    },
    /// A note was deleted
    NoteDeleted {
        id: String,
    },
    /// A note's display position changed (due to buffer edits)
    NotePositionChanged {
        id: String,
        file: String,
        old_line: i64,
        new_line: i64,
        stale: bool,
    },
    /// AI explanation completed for a note
    AiComplete {
        note_id: String,
        success: bool,
        #[serde(skip_serializing_if = "Option::is_none")]
        provider: Option<String>,
    },
    /// Project indexing completed
    IndexComplete {
        project: String,
        files_indexed: usize,
    },
    /// Single file was indexed
    FileIndexed {
        file: String,
        project: String,
    },
}

impl Event {
    /// Serialize to JSON line (with newline)
    pub fn to_json_line(&self) -> String {
        let mut json = serde_json::to_string(self).unwrap_or_else(|_| "{}".to_string());
        json.push('\n');
        json
    }
}

/// Subscriber ID for tracking connections
type SubscriberId = u64;

/// Thread-safe event broadcaster
pub struct EventBroadcaster {
    /// Connected clients with their write streams
    subscribers: Arc<RwLock<HashMap<SubscriberId, Arc<Mutex<UnixStream>>>>>,
    /// Next subscriber ID
    next_id: Arc<Mutex<SubscriberId>>,
    /// Bounded channel for sending events to broadcaster thread
    tx: SyncSender<Event>,
}

impl EventBroadcaster {
    /// Create a new event broadcaster
    pub fn new() -> Self {
        let subscribers: Arc<RwLock<HashMap<SubscriberId, Arc<Mutex<UnixStream>>>>> =
            Arc::new(RwLock::new(HashMap::new()));
        let next_id = Arc::new(Mutex::new(0u64));

        // Bounded channel - drops events if buffer is full (backpressure)
        const EVENT_BUFFER_SIZE: usize = 1024;
        let (tx, rx): (SyncSender<Event>, Receiver<Event>) = mpsc::sync_channel(EVENT_BUFFER_SIZE);

        // Spawn broadcaster thread
        let subs = subscribers.clone();
        thread::spawn(move || {
            broadcaster_loop(rx, subs);
        });

        Self {
            subscribers,
            next_id,
            tx,
        }
    }

    /// Broadcast an event to all connected clients
    pub fn emit(&self, event: Event) {
        // Non-blocking send - if channel is full, event is dropped
        let _ = self.tx.try_send(event);
    }

}

/// Background loop that receives events and broadcasts to all subscribers
fn broadcaster_loop(
    rx: Receiver<Event>,
    subscribers: Arc<RwLock<HashMap<SubscriberId, Arc<Mutex<UnixStream>>>>>,
) {
    for event in rx {
        let json = event.to_json_line();
        let bytes = json.as_bytes();

        // Snapshot subscriber IDs and streams to minimize lock hold time
        let snapshot: Vec<(SubscriberId, Arc<Mutex<UnixStream>>)> = {
            let subs = match subscribers.read() {
                Ok(guard) => guard,
                Err(poisoned) => {
                    eprintln!("[events] Subscriber lock poisoned, recovering");
                    poisoned.into_inner()
                }
            };
            subs.iter().map(|(&id, s)| (id, s.clone())).collect()
        };

        let mut failed: Vec<SubscriberId> = Vec::new();

        for (id, stream_arc) in snapshot {
            // Use try_lock to avoid blocking on slow clients
            let write_result = match stream_arc.try_lock() {
                Ok(mut stream) => {
                    // Socket has write timeout set, but try_lock ensures we don't
                    // block waiting for lock if another thread is using this stream
                    stream.write_all(bytes).and_then(|_| stream.flush())
                }
                Err(_) => {
                    // Stream is locked by another operation, skip this event for this client
                    // (not a failure, just contention)
                    continue;
                }
            };

            if write_result.is_err() {
                failed.push(id);
            }
        }

        // Remove failed subscribers immediately
        if !failed.is_empty() {
            let mut subs = match subscribers.write() {
                Ok(guard) => guard,
                Err(poisoned) => {
                    eprintln!("[events] Subscriber lock poisoned, recovering");
                    poisoned.into_inner()
                }
            };
            for id in failed {
                subs.remove(&id);
                eprintln!("[events] Client {} write failed, removed", id);
            }
        }
    }
}

/// Global event broadcaster instance
static BROADCASTER: std::sync::OnceLock<EventBroadcaster> = std::sync::OnceLock::new();

/// Get or initialize the global broadcaster
pub fn broadcaster() -> &'static EventBroadcaster {
    BROADCASTER.get_or_init(EventBroadcaster::new)
}

/// Emit an event to all connected clients
pub fn emit(event: Event) {
    broadcaster().emit(event);
}

/// Start the event socket server
pub fn start_event_server(socket_path: PathBuf) {
    // Clean up stale socket
    if socket_path.exists() {
        let _ = fs::remove_file(&socket_path);
    }

    let broadcaster = broadcaster();

    thread::spawn(move || {
        match UnixListener::bind(&socket_path) {
            Ok(listener) => {
                eprintln!("[events] Event server listening on {}", socket_path.display());

                for stream in listener.incoming() {
                    match stream {
                        Ok(stream) => {
                            // Set write timeout to prevent slow clients from blocking the broadcaster
                            // 100ms is enough for any reasonable client to accept the write
                            let _ = stream.set_write_timeout(Some(std::time::Duration::from_millis(100)));

                            // Clone broadcaster reference for the connection handler
                            let subs = broadcaster.subscribers.clone();
                            let next_id = broadcaster.next_id.clone();

                            // Add subscriber (recover from poisoned locks)
                            let id = {
                                let mut id_guard = match next_id.lock() {
                                    Ok(guard) => guard,
                                    Err(poisoned) => poisoned.into_inner(),
                                };
                                let id = *id_guard;
                                *id_guard += 1;
                                drop(id_guard);

                                let mut s = match subs.write() {
                                    Ok(guard) => guard,
                                    Err(poisoned) => poisoned.into_inner(),
                                };
                                s.insert(id, Arc::new(Mutex::new(stream)));
                                eprintln!("[events] Client {} connected ({} total)", id, s.len());
                                id
                            };

                            // Note: Client cleanup happens when write fails in broadcaster_loop.
                            // We don't spawn a reader thread because the event socket is
                            // write-only (server pushes events, clients don't send anything).
                            let _ = id;
                        }
                        Err(e) => {
                            eprintln!("[events] Connection error: {}", e);
                        }
                    }
                }
            }
            Err(e) => {
                eprintln!("[events] Failed to bind socket {}: {}", socket_path.display(), e);
            }
        }
    });
}

/// Clean up the event socket
pub fn cleanup_event_socket(socket_path: &PathBuf) {
    let _ = fs::remove_file(socket_path);
}

#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck_macros::quickcheck;

    // Property: to_json_line always produces valid JSON
    #[quickcheck]
    fn prop_note_created_valid_json(id: String, file: String, line: i64, project: Option<String>) -> bool {
        let event = Event::NoteCreated {
            id,
            file,
            line,
            project_root: project,
        };
        let json = event.to_json_line();
        // Should parse as valid JSON (minus the trailing newline)
        serde_json::from_str::<serde_json::Value>(json.trim()).is_ok()
    }

    // Property: to_json_line always ends with exactly one newline
    #[quickcheck]
    fn prop_note_created_ends_with_single_newline(id: String, file: String, line: i64) -> bool {
        let event = Event::NoteCreated {
            id,
            file,
            line,
            project_root: None,
        };
        let json = event.to_json_line();
        json.ends_with('\n') && !json.ends_with("\n\n")
    }

    // Property: to_json_line always contains the type field
    #[quickcheck]
    fn prop_note_updated_has_type(id: String) -> bool {
        let event = Event::NoteUpdated { id };
        let json = event.to_json_line();
        json.contains("\"type\":")
    }

    // Property: to_json_line never panics for NoteDeleted
    #[quickcheck]
    fn prop_note_deleted_never_panics(id: String) -> bool {
        let event = Event::NoteDeleted { id };
        let _ = event.to_json_line();
        true
    }

    // Property: to_json_line for AiComplete produces valid JSON
    #[quickcheck]
    fn prop_ai_complete_valid_json(note_id: String, success: bool, provider: Option<String>) -> bool {
        let event = Event::AiComplete {
            note_id,
            success,
            provider,
        };
        let json = event.to_json_line();
        serde_json::from_str::<serde_json::Value>(json.trim()).is_ok()
    }

    // Property: to_json_line for IndexComplete produces valid JSON
    #[quickcheck]
    fn prop_index_complete_valid_json(project: String, files_indexed: usize) -> bool {
        let event = Event::IndexComplete {
            project,
            files_indexed,
        };
        let json = event.to_json_line();
        serde_json::from_str::<serde_json::Value>(json.trim()).is_ok()
    }

    // Property: to_json_line for FileIndexed produces valid JSON
    #[quickcheck]
    fn prop_file_indexed_valid_json(file: String, project: String) -> bool {
        let event = Event::FileIndexed { file, project };
        let json = event.to_json_line();
        serde_json::from_str::<serde_json::Value>(json.trim()).is_ok()
    }

    // Property: to_json_line for NotePositionChanged produces valid JSON
    #[quickcheck]
    fn prop_note_position_changed_valid_json(id: String, file: String, old_line: i64, new_line: i64, stale: bool) -> bool {
        let event = Event::NotePositionChanged { id, file, old_line, new_line, stale };
        let json = event.to_json_line();
        serde_json::from_str::<serde_json::Value>(json.trim()).is_ok()
    }

    // Property: skip_serializing_if works - None fields are omitted
    #[test]
    fn test_none_fields_omitted() {
        let event = Event::NoteCreated {
            id: "id".to_string(),
            file: "f".to_string(),
            line: 1,
            project_root: None,
        };
        let json = event.to_json_line();
        assert!(!json.contains("project_root"));

        let event = Event::AiComplete {
            note_id: "id".to_string(),
            success: true,
            provider: None,
        };
        let json = event.to_json_line();
        assert!(!json.contains("provider"));
    }

    #[test]
    fn test_event_note_created_json() {
        let event = Event::NoteCreated {
            id: "abc123".to_string(),
            file: "/tmp/test.rs".to_string(),
            line: 42,
            project_root: Some("/tmp/project".to_string()),
        };

        let json = event.to_json_line();
        assert!(json.ends_with('\n'));
        assert!(json.contains("\"type\":\"note-created\""));
        assert!(json.contains("\"id\":\"abc123\""));
        assert!(json.contains("\"file\":\"/tmp/test.rs\""));
        assert!(json.contains("\"line\":42"));
        assert!(json.contains("\"project_root\":\"/tmp/project\""));
    }

    #[test]
    fn test_event_note_created_without_project() {
        let event = Event::NoteCreated {
            id: "abc123".to_string(),
            file: "/tmp/test.rs".to_string(),
            line: 42,
            project_root: None,
        };

        let json = event.to_json_line();
        // project_root should be omitted when None
        assert!(!json.contains("project_root"));
    }

    #[test]
    fn test_event_note_updated_json() {
        let event = Event::NoteUpdated {
            id: "xyz789".to_string(),
        };

        let json = event.to_json_line();
        assert!(json.contains("\"type\":\"note-updated\""));
        assert!(json.contains("\"id\":\"xyz789\""));
    }

    #[test]
    fn test_event_note_deleted_json() {
        let event = Event::NoteDeleted {
            id: "del456".to_string(),
        };

        let json = event.to_json_line();
        assert!(json.contains("\"type\":\"note-deleted\""));
        assert!(json.contains("\"id\":\"del456\""));
    }

    #[test]
    fn test_event_ai_complete_json() {
        let event = Event::AiComplete {
            note_id: "note123".to_string(),
            success: true,
            provider: Some("claude".to_string()),
        };

        let json = event.to_json_line();
        assert!(json.contains("\"type\":\"ai-complete\""));
        assert!(json.contains("\"note_id\":\"note123\""));
        assert!(json.contains("\"success\":true"));
        assert!(json.contains("\"provider\":\"claude\""));
    }

    #[test]
    fn test_event_ai_complete_without_provider() {
        let event = Event::AiComplete {
            note_id: "note123".to_string(),
            success: false,
            provider: None,
        };

        let json = event.to_json_line();
        assert!(json.contains("\"success\":false"));
        // provider should be omitted when None
        assert!(!json.contains("provider"));
    }

    #[test]
    fn test_event_index_complete_json() {
        let event = Event::IndexComplete {
            project: "/tmp/myproject".to_string(),
            files_indexed: 100,
        };

        let json = event.to_json_line();
        assert!(json.contains("\"type\":\"index-complete\""));
        assert!(json.contains("\"project\":\"/tmp/myproject\""));
        assert!(json.contains("\"files_indexed\":100"));
    }

    #[test]
    fn test_event_file_indexed_json() {
        let event = Event::FileIndexed {
            file: "/tmp/test.rs".to_string(),
            project: "/tmp/project".to_string(),
        };

        let json = event.to_json_line();
        assert!(json.contains("\"type\":\"file-indexed\""));
        assert!(json.contains("\"file\":\"/tmp/test.rs\""));
        assert!(json.contains("\"project\":\"/tmp/project\""));
    }

    #[test]
    fn test_event_note_position_changed_json() {
        let event = Event::NotePositionChanged {
            id: "abc123".to_string(),
            file: "/tmp/test.rs".to_string(),
            old_line: 10,
            new_line: 15,
            stale: false,
        };

        let json = event.to_json_line();
        assert!(json.ends_with('\n'));
        assert!(json.contains("\"type\":\"note-position-changed\""));
        assert!(json.contains("\"id\":\"abc123\""));
        assert!(json.contains("\"file\":\"/tmp/test.rs\""));
        assert!(json.contains("\"old_line\":10"));
        assert!(json.contains("\"new_line\":15"));
        assert!(json.contains("\"stale\":false"));
    }

    #[test]
    fn test_event_note_position_changed_stale() {
        let event = Event::NotePositionChanged {
            id: "xyz".to_string(),
            file: "/test.rs".to_string(),
            old_line: 5,
            new_line: 5,
            stale: true,
        };

        let json = event.to_json_line();
        assert!(json.contains("\"stale\":true"));
    }

    #[test]
    fn test_event_json_line_ends_with_newline() {
        let events = vec![
            Event::NoteCreated {
                id: "1".to_string(),
                file: "f".to_string(),
                line: 1,
                project_root: None,
            },
            Event::NoteUpdated { id: "2".to_string() },
            Event::NoteDeleted { id: "3".to_string() },
            Event::AiComplete {
                note_id: "4".to_string(),
                success: true,
                provider: None,
            },
            Event::IndexComplete {
                project: "p".to_string(),
                files_indexed: 0,
            },
            Event::FileIndexed {
                file: "f".to_string(),
                project: "p".to_string(),
            },
            Event::NotePositionChanged {
                id: "1".to_string(),
                file: "f".to_string(),
                old_line: 10,
                new_line: 12,
                stale: false,
            },
        ];

        for event in events {
            let json = event.to_json_line();
            assert!(json.ends_with('\n'), "Event JSON should end with newline");
            assert!(!json.ends_with("\n\n"), "Event JSON should have exactly one newline");
        }
    }

    #[test]
    fn test_event_kebab_case_types() {
        // Verify that all event types use kebab-case
        let note_created = Event::NoteCreated {
            id: "1".to_string(),
            file: "f".to_string(),
            line: 1,
            project_root: None,
        };
        assert!(note_created.to_json_line().contains("note-created"));

        let note_updated = Event::NoteUpdated { id: "1".to_string() };
        assert!(note_updated.to_json_line().contains("note-updated"));

        let note_deleted = Event::NoteDeleted { id: "1".to_string() };
        assert!(note_deleted.to_json_line().contains("note-deleted"));

        let ai_complete = Event::AiComplete {
            note_id: "1".to_string(),
            success: true,
            provider: None,
        };
        assert!(ai_complete.to_json_line().contains("ai-complete"));

        let index_complete = Event::IndexComplete {
            project: "p".to_string(),
            files_indexed: 0,
        };
        assert!(index_complete.to_json_line().contains("index-complete"));

        let file_indexed = Event::FileIndexed {
            file: "f".to_string(),
            project: "p".to_string(),
        };
        assert!(file_indexed.to_json_line().contains("file-indexed"));

        let position_changed = Event::NotePositionChanged {
            id: "1".to_string(),
            file: "f".to_string(),
            old_line: 5,
            new_line: 7,
            stale: false,
        };
        assert!(position_changed.to_json_line().contains("note-position-changed"));
    }

    #[test]
    fn test_broadcaster_can_emit_without_subscribers() {
        // Test that emitting events without subscribers doesn't panic
        let broadcaster = EventBroadcaster::new();
        broadcaster.emit(Event::NoteCreated {
            id: "test".to_string(),
            file: "/tmp/test.rs".to_string(),
            line: 1,
            project_root: None,
        });
        broadcaster.emit(Event::NoteUpdated {
            id: "test".to_string(),
        });
        broadcaster.emit(Event::NoteDeleted {
            id: "test".to_string(),
        });
        // Give the broadcaster thread time to process
        std::thread::sleep(std::time::Duration::from_millis(50));
    }

    #[test]
    fn test_event_clone() {
        // Test that Event implements Clone correctly
        let original = Event::NoteCreated {
            id: "abc".to_string(),
            file: "/test.rs".to_string(),
            line: 42,
            project_root: Some("/project".to_string()),
        };

        let cloned = original.clone();

        // Verify cloned event serializes identically
        assert_eq!(original.to_json_line(), cloned.to_json_line());
    }

    #[test]
    fn test_event_debug() {
        // Test that Event implements Debug
        let event = Event::NoteCreated {
            id: "test".to_string(),
            file: "/test.rs".to_string(),
            line: 1,
            project_root: None,
        };

        let debug_str = format!("{:?}", event);
        assert!(debug_str.contains("NoteCreated"));
        assert!(debug_str.contains("test"));
    }
}
