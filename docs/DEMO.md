# Mnemos Demo Script

A walkthrough for screen recording the Mnemos note-taking system across Emacs, Neovim, and VS Code.

## Setup (Before Recording)

```bash
# Build the backend
cd ~/Work/mnemos
zig build -Doptimize=ReleaseFast

# Clean slate - remove old notes
rm -rf /tmp/mnemos-demo/.mnemos

# Prepare a demo project
mkdir -p /tmp/mnemos-demo
cat > /tmp/mnemos-demo/app.rs << 'EOF'
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

cat > /tmp/mnemos-demo/utils.rs << 'EOF'
pub fn format_port(port: u16) -> String {
    format!(":{}", port)
}

pub fn validate_port(port: u16) -> bool {
    port > 1024 && port < 65535
}
EOF

cd /tmp/mnemos-demo
git init && git add . && git commit -m "Initial"
```

---

## Part 1: Emacs Demo (~5 min)

### 1.1 Start Mnemos & Help
```
Open Emacs
M-x mnemos-start
→ Shows "Mnemos: connected to backend"

M-x mnemos-help
→ Shows available keybindings and commands
```

### 1.2 Open Demo File
```
C-x C-f /tmp/mnemos-demo/app.rs
```

### 1.3 Search Uses Ripgrep
```
M-x mnemos-search-project
→ Uses ripgrep for project-wide search
```

### 1.4 Create First Note
```
Move cursor to line 4 (server.start())
C-c m n (mnemos-add-note)
→ Type: "TODO: Add graceful shutdown handling"
→ C-c C-c to save
→ Shows "Note created"
→ Note overlay appears below the line
```

### 1.5 Create Multiline Note
```
Move cursor to line 8 (load_config function)
C-c m n
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
C-c m n
→ Type: "Related to startup: [["
→ [[ triggers link search
→ Select "TODO: Add graceful shutdown"
→ Completes link as [[TODO: Add graceful...][id]]
→ C-c C-c to save
```

### 1.7 View Notes List
```
C-c m l (mnemos-list-notes)
→ Shows buffer with all notes
→ j/k or n/p to navigate
→ Press RET to jump to note location
→ Press v to view full note text
```

### 1.8 View Note Details
```
Move to a note in the list
Press v (mnemos-view-note)
→ Opens note in markdown mode with full text
```

### 1.9 Show Backlinks
```
In notes list, cursor on "shutdown" note
Press b (mnemos-show-backlinks)
→ Shows the Server::new note that links to it
```

### 1.10 Search Notes & Files
```
M-x mnemos-search-project
→ Type: "config"
→ Shows matching notes and file content via ripgrep
→ Press RET to visit result
```

### 1.11 Explain Region (LLM Hook)
```
Select lines 12-16 (Server struct)
M-x mnemos-explain-region
→ Returns code snippet ready for LLM context
```

### 1.12 Save/Load Snapshot
```
M-x mnemos-save-snapshot
→ Enter path: /tmp/mnemos-backup.json
→ Saves all notes

M-x mnemos-load-snapshot
→ Select file
→ Restores notes state
```

### 1.14 Edit a Note
```
C-c m e (mnemos-edit-note-at-point)
→ Modify text in buffer
→ C-c C-c to save
```

### 1.15 Delete a Note
```
C-c m d (mnemos-delete-note-at-point)
→ Confirms deletion
→ Note removed
```

### 1.16 Check Status
```
C-c m S (mnemos-status)
→ Shows: "3 notes"
```

---

## Part 2: Neovim Demo (~4 min)

### 2.1 Open File & Help
```
nvim /tmp/mnemos-demo/app.rs

:Mnemos help
→ Shows all available commands
```

### 2.2 Verify Connection & Status
```
:Mnemos status
→ Shows note counts from shared notes directory
```

### 2.3 Refresh to See Existing Notes
```
:Mnemos refresh
→ Overlays appear for notes created in Emacs
→ (Demonstrates shared notes directory)
```

### 2.4 Create a Simple Note
```
Move to line 28 (Config struct)
:Mnemos add
→ Type: "Add validation constraints"
→ Press Enter
```

### 2.5 Create Multiline Note
```
Move to line 32 (port field)
:Mnemos add_multiline
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
:Mnemos list
→ Opens floating window with notes
→ j/k to navigate
→ Press Enter to jump to note
→ Press q to close
```

### 2.7 Insert Link to Another Note
```
Move to a new line
:Mnemos insert_link
→ Search for "shutdown"
→ Select note
→ Inserts [[description][id]] format
```

### 2.8 Show Backlinks
```
Move to line with linked note
:Mnemos backlinks
→ Shows notes that reference this one
```

### 2.9 Search
```
:Mnemos search config
→ Shows matching notes and file content via ripgrep
→ Select to jump to location
```

### 2.10 Edit Note
```
Move cursor to note line
:Mnemos edit
→ Modify text
→ Save
```

### 2.11 Delete Note
```
:Mnemos delete
→ Confirms deletion
→ Note removed
```

### 2.12 Explain Region (LLM Hook)
```
Select lines 12-16 (Server struct) in visual mode
:MnemosExplainRegion
→ Copies snippet to clipboard (LLM-ready)
```

### 2.14 Save/Load Snapshot
```
:MnemosSaveSnapshot
→ Enter path: /tmp/mnemos-backup.json
→ Saves all notes

:MnemosLoadSnapshot
→ Enter path
→ Confirms replacement
→ Restores notes state
```

### 2.15 Shutdown Backend
```
:MnemosShutdown
→ Cleanly stops the backend process
```

---

## Part 3: VS Code Demo (~3 min)

### 3.1 Open Folder
```
File → Open Folder → /tmp/mnemos-demo
```

### 3.2 Check Status
```
Cmd+Shift+P → "Mnemos: Status"
→ Shows note counts
```

### 3.3 View Existing Notes
```
Open app.rs
→ Notes from Emacs/Neovim appear as line decorations
```

### 3.4 Refresh Notes
```
Cmd+Shift+P → "Mnemos: Refresh Notes"
→ Updates decorations from notes
```

### 3.5 Add Note
```
Click on line 35 (Default impl)
Cmd+Shift+P → "Mnemos: Add Note"
→ Type: "Consider using builder pattern"
→ Press Enter
```

### 3.6 List Notes in File
```
Cmd+Shift+P → "Mnemos: List Notes"
→ QuickPick shows all notes with line numbers
→ Select to jump to location
```

### 3.7 Edit Note
```
Place cursor on note line
Cmd+Shift+P → "Mnemos: Edit Note"
→ Shows input with current text
→ Modify and press Enter
```

### 3.8 Delete Note
```
Place cursor on note line
Cmd+Shift+P → "Mnemos: Delete Note"
→ Confirmation dialog
→ Confirm to remove
```

### 3.9 Index Current File
```
Cmd+Shift+P → "Mnemos: Index File"
→ Indexes for search
```

### 3.10 Index Project
```
Cmd+Shift+P → "Mnemos: Index Project"
→ Shows progress
→ "Indexed X files"
```

### 3.11 Search Notes & Files
```
Cmd+Shift+P → "Mnemos: Search"
→ Type: "port"
→ Shows [Note] and [File] results with scores
→ Select to open location
```

### 3.12 Insert Link to Note
```
Cmd+Shift+P → "Mnemos: Insert Link"
→ Search for existing note
→ Inserts [[description][id]] at cursor
```

### 3.13 View Note Details
```
Place cursor on note line
Cmd+Shift+P → "Mnemos: View Note" (or Cmd+Shift+H V)
→ Opens note in markdown preview
→ Shows file, timestamps, stale status, full text
```

### 3.14 Show Backlinks
```
Place cursor on note line
Cmd+Shift+P → "Mnemos: Show Backlinks" (or Cmd+Shift+H B)
→ Lists notes that link to this note
→ Select to jump to linking note
```

### 3.15 Browse Project Files
```
Cmd+Shift+P → "Mnemos: List Project Files"
→ Shows all files in project with sizes
→ Select to open file
```

### 3.16 Explain Region (LLM Hook)
```
Select lines 12-16 (Server struct)
Cmd+Shift+P → "Mnemos: Explain Region" (or Cmd+Shift+H X)
→ Copies code snippet to clipboard (LLM-ready format)
```

### 3.17 Save/Load Snapshot
```
Cmd+Shift+P → "Mnemos: Save Snapshot"
→ Choose save location
→ "Snapshot saved: X notes, Y files"

Cmd+Shift+P → "Mnemos: Load Snapshot"
→ Select snapshot file
→ Confirms replacement
→ Restores notes state
```

### 3.18 Show Help
```
Cmd+Shift+P → "Mnemos: Help"
→ Opens markdown with all commands and keybindings
```

### 3.19 Shutdown Backend
```
Cmd+Shift+P → "Mnemos: Shutdown Backend"
→ Cleanly stops the backend process
```

---

## Part 4: Cross-Editor Sync (~1 min)

### 4.1 Show Notes Location
```bash
ls -la /tmp/mnemos-demo/.mnemos/notes
```

### 4.2 Demonstrate Real-Time Sync
```
1. In Emacs: Create note "Cross-editor sync test"
2. In Neovim: :Mnemos refresh → Note appears instantly
3. In VS Code: Refresh → Note appears
→ All editors share the same notes directory!
```

### 4.3 Show Stale Note Detection
```
1. Create a note on line 10
2. In terminal: Edit app.rs, insert lines above
3. Refresh notes in editor
→ Note shows [STALE] indicator (node hash changed)
```

---

## Closing Remarks

**Key Features Demonstrated:**
- Notes attach to code locations (file, line, column)
- Multiline notes with full markdown support
- Cross-note linking with [[description][id]] syntax
- Backlinks for bidirectional navigation
- Full-text search across notes and files via ripgrep
- Stale detection via node hash tracking
- Tree-sitter integration anchors notes to AST nodes
- Snapshot save/load for backup/restore
- Single shared notes directory across all editors
- Works offline, local-first, no cloud dependency
