# Hemis Demo Script

A walkthrough for screen recording the Hemis note-taking system across Emacs, Neovim, and VS Code.

## Setup (Before Recording)

```bash
# Build the backend
cd ~/Work/hemis
cargo build --release

# Clean slate - remove old database
rm -f ~/.hemis/hemis-notes.db

# Prepare a demo project
mkdir -p /tmp/hemis-demo
cat > /tmp/hemis-demo/app.rs << 'EOF'
fn main() {
    let config = load_config();
    let server = Server::new(config);
    server.start();
}

fn load_config() -> Config {
    Config::default()
}

struct Server {
    config: Config,
}

impl Server {
    fn new(config: Config) -> Self {
        Self { config }
    }

    fn start(&self) {
        println!("Starting server...");
    }
}

struct Config {
    port: u16,
}

impl Default for Config {
    fn default() -> Self {
        Self { port: 8080 }
    }
}
EOF

cat > /tmp/hemis-demo/utils.rs << 'EOF'
pub fn format_port(port: u16) -> String {
    format!(":{}", port)
}

pub fn validate_port(port: u16) -> bool {
    port > 1024 && port < 65535
}
EOF

cd /tmp/hemis-demo
git init && git add . && git commit -m "Initial"
```

---

## Part 1: Emacs Demo (~5 min)

### 1.1 Start Hemis & Help
```
Open Emacs
M-x hemis-start
→ Shows "Hemis: connected to backend"

M-x hemis-help
→ Shows available keybindings and commands
```

### 1.2 Open Demo File
```
C-x C-f /tmp/hemis-demo/app.rs
```

### 1.3 Index the Project
```
M-x hemis-index-project
→ Shows "Project indexed: 2 files"
```

### 1.4 Create First Note
```
Move cursor to line 4 (server.start())
C-c h n (hemis-add-note)
→ Type: "TODO: Add graceful shutdown handling"
→ C-c C-c to save
→ Shows "Note created"
→ Note overlay appears below the line
```

### 1.5 Create Multiline Note
```
Move cursor to line 8 (load_config function)
C-c h n
→ Type multiple lines:
  "This function needs:
  - Error handling
  - Config file support
  - Environment variable override"
→ C-c C-c to save
```

### 1.6 Create Note with Link
```
Move cursor to line 18 (Server::new)
C-c h n
→ Type: "Related to startup: [["
→ [[ triggers link search
→ Select "TODO: Add graceful shutdown"
→ Completes link as [[TODO: Add graceful...][id]]
→ C-c C-c to save
```

### 1.7 View Notes List
```
C-c h l (hemis-list-notes)
→ Shows buffer with all notes
→ j/k or n/p to navigate
→ Press RET to jump to note location
→ Press v to view full note text
```

### 1.8 View Note Details
```
Move to a note in the list
Press v (hemis-view-note)
→ Opens note in markdown mode with full text
```

### 1.9 Show Backlinks
```
In notes list, cursor on "shutdown" note
Press b (hemis-show-backlinks)
→ Shows the Server::new note that links to it
```

### 1.10 Search Notes & Files
```
M-x hemis-search-project
→ Type: "config"
→ Shows matching notes AND indexed file content
→ Press RET to visit result
```

### 1.11 Explain Region (LLM Hook)
```
Select lines 12-16 (Server struct)
M-x hemis-explain-region
→ Returns code snippet ready for LLM context
```

### 1.12 Browse Project Files
```
M-x hemis-list-files
→ Shows all files in project
→ Select to view content
```

### 1.13 Save/Load Snapshot
```
M-x hemis-save-snapshot
→ Enter path: /tmp/hemis-backup.json
→ Saves all notes, files, embeddings

M-x hemis-load-snapshot
→ Select file
→ Restores database state
```

### 1.14 Edit a Note
```
C-c h e (hemis-edit-note-at-point)
→ Modify text in buffer
→ C-c C-c to save
```

### 1.15 Delete a Note
```
C-c h d (hemis-delete-note-at-point)
→ Confirms deletion
→ Note removed
```

### 1.16 Check Status
```
C-c h S (hemis-status)
→ Shows: "3 notes, 2 files, 0 embeddings"
```

---

## Part 2: Neovim Demo (~4 min)

### 2.1 Open File & Help
```
nvim /tmp/hemis-demo/app.rs

:Hemis help
→ Shows all available commands
```

### 2.2 Verify Connection & Status
```
:Hemis status
→ Shows note/file counts from shared database
```

### 2.3 Refresh to See Existing Notes
```
:Hemis refresh
→ Overlays appear for notes created in Emacs
→ (Demonstrates shared database)
```

### 2.4 Create a Simple Note
```
Move to line 28 (Config struct)
:Hemis add
→ Type: "Add validation constraints"
→ Press Enter
```

### 2.5 Create Multiline Note
```
Move to line 32 (port field)
:Hemis add_multiline
→ Opens buffer for longer note
→ Type multiple lines:
  "Port configuration:
  - Default: 8080
  - Range: 1024-65535
  - Can override via env"
→ Save and close buffer
```

### 2.6 List Notes with Navigation
```
:Hemis list
→ Opens floating window with notes
→ j/k to navigate
→ Press Enter to jump to note
→ Press q to close
```

### 2.7 Insert Link to Another Note
```
Move to a new line
:Hemis insert_link
→ Search for "shutdown"
→ Select note
→ Inserts [[description][id]] format
```

### 2.8 Show Backlinks
```
Move to line with linked note
:Hemis backlinks
→ Shows notes that reference this one
```

### 2.9 Index Current File
```
:Hemis index_file
→ Indexes current buffer for search
```

### 2.10 Index Entire Project
```
:Hemis index_project
→ Indexes all files in project root
```

### 2.11 Search
```
:Hemis search config
→ Shows matching notes and file content
→ Select to jump to location
```

### 2.12 Edit Note
```
Move cursor to note line
:Hemis edit
→ Modify text
→ Save
```

### 2.13 Delete Note
```
:Hemis delete
→ Confirms deletion
→ Note removed
```

### 2.14 Shutdown Backend
```
:Hemis shutdown
→ Cleanly stops the backend process
```

---

## Part 3: VS Code Demo (~3 min)

### 3.1 Open Folder
```
File → Open Folder → /tmp/hemis-demo
```

### 3.2 Check Status
```
Cmd+Shift+P → "Hemis: Status"
→ Shows note/file/embedding counts
```

### 3.3 View Existing Notes
```
Open app.rs
→ Notes from Emacs/Neovim appear as line decorations
```

### 3.4 Refresh Notes
```
Cmd+Shift+P → "Hemis: Refresh Notes"
→ Updates decorations from database
```

### 3.5 Add Note
```
Click on line 35 (Default impl)
Cmd+Shift+P → "Hemis: Add Note"
→ Type: "Consider using builder pattern"
→ Press Enter
```

### 3.6 List Notes in File
```
Cmd+Shift+P → "Hemis: List Notes"
→ QuickPick shows all notes with line numbers
→ Select to jump to location
```

### 3.7 Edit Note
```
Place cursor on note line
Cmd+Shift+P → "Hemis: Edit Note"
→ Shows input with current text
→ Modify and press Enter
```

### 3.8 Delete Note
```
Place cursor on note line
Cmd+Shift+P → "Hemis: Delete Note"
→ Confirmation dialog
→ Confirm to remove
```

### 3.9 Index Current File
```
Cmd+Shift+P → "Hemis: Index File"
→ Indexes for search
```

### 3.10 Index Project
```
Cmd+Shift+P → "Hemis: Index Project"
→ Shows progress
→ "Indexed X files"
```

### 3.11 Search Notes & Files
```
Cmd+Shift+P → "Hemis: Search"
→ Type: "port"
→ Shows [Note] and [File] results with scores
→ Select to open location
```

### 3.12 Insert Link to Note
```
Cmd+Shift+P → "Hemis: Insert Link"
→ Search for existing note
→ Inserts [[description][id]] at cursor
```

### 3.13 View Note Details
```
Place cursor on note line
Cmd+Shift+P → "Hemis: View Note" (or Cmd+Shift+H V)
→ Opens note in markdown preview
→ Shows file, timestamps, stale status, full text
```

### 3.14 Show Backlinks
```
Place cursor on note line
Cmd+Shift+P → "Hemis: Show Backlinks" (or Cmd+Shift+H B)
→ Lists notes that link to this note
→ Select to jump to linking note
```

---

## Part 4: Cross-Editor Sync (~1 min)

### 4.1 Show Database Location
```bash
ls -la ~/.hemis/hemis-notes.db
sqlite3 ~/.hemis/hemis-notes.db "SELECT COUNT(*) FROM notes;"
```

### 4.2 Demonstrate Real-Time Sync
```
1. In Emacs: Create note "Cross-editor sync test"
2. In Neovim: :Hemis refresh → Note appears instantly
3. In VS Code: Refresh → Note appears
→ All editors share the same database!
```

### 4.3 Show Stale Note Detection
```
1. Create a note on line 10
2. In terminal: Edit app.rs, insert lines above
3. git add . && git commit -m "Changed"
4. Refresh notes in editor
→ Note shows [STALE] indicator (git SHA changed)
```

---

## Closing Remarks

**Key Features Demonstrated:**
- Notes attach to code locations (file, line, column)
- Multiline notes with full markdown support
- Cross-note linking with [[description][id]] syntax
- Backlinks for bidirectional navigation
- Full-text search across notes and indexed files
- Git-aware stale detection (commit/blob SHA tracking)
- Tree-sitter integration anchors notes to AST nodes
- Project indexing for semantic code search
- Snapshot save/load for backup/restore
- Single shared SQLite database across all editors
- Works offline, local-first, no cloud dependency
