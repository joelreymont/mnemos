# Mnemos: A Second Brain for Your Code

**Mnemos = "a second brain for your code."**

It is:

- **Local-first** — models and indexes live on your machine.
- **Lisp-orchestrated** — Common Lisp is the always-on brain that coordinates everything.
- **LLM-powered** — Qwen / Llama / other local models provide deep reasoning.
- **UI-rich** — Lauri + React present code, notes, backlinks, and conversations.

Mnemos is not "just another code assistant". It is:

- A code browser with memory.
- A semantic layer over your entire project.
- A way to attach and traverse notes, explanations, and design decisions.
- A REPL-friendly environment where behaviour can be patched live.

This spec describes:

- Overall architecture.
- Processes and responsibilities.
- LLM interaction flow.
- Indexing pipeline.
- Tools and JSON-RPC contracts.
- Startup flow and configuration.
- Skeleton code for Lisp and React parts.


---

# 1. Architecture

Mnemos is a three-process architecture designed for interactive, local code intelligence.

```text
+---------------------+        JSON-RPC        +--------------------+       HTTP/JSON        +------------------+
|   UI (Lauri +       | <--------------------> |   Lisp Backend     | <--------------------> |   LLM Runner     |
|   React front-end)  |                        |  (Mnemos Brain)     |                       | (Qwen / Llama)   |
+---------------------+                        +--------------------+                       +------------------+
       ^                                                ^
       |                                                |
       |                                  +-------------+------------+
       |                                  |  Indexes / Storage       |
       |                                  +--------------------------+
```

## 1.1 Responsibilities

### UI (Lauri + React)
- Renders:
  - Code editor view with syntax highlighting.
  - Notes and backlinks.
  - Conversation panel.
  - Search and navigation panels.
- Sends user actions to Lisp as JSON-RPC calls:
  - `explain-region`
  - `find-usages`
  - `open-symbol`
  - `compare-versions`
- Streams partial responses from Lisp/LLM to the user.

### Lisp Backend (Mnemos Brain)
- Maintains all long-lived state:
  - Project registry and configuration.
  - Symbol graph and AST cache.
  - Embeddings and semantic index.
  - Notes and backlinks.
- Talks to:
  - UI via JSON-RPC.
  - LLM runner via HTTP/JSON or a simple TCP protocol.
- Manages:
  - Tool definitions and dispatch.
  - LLM prompts and conversation scaffolding.
  - LLM runner supervision (start/stop/health-check).

### LLM Runner
- Loads a local model (e.g. Qwen 2.5 Coder).
- Exposes a simple API:
  - `/v1/chat/completions` or equivalent.
  - Streaming responses.
- Does no application-specific logic.
- Can be swapped without changing Lisp code (only config).

## 1.2 Isolation

- UI can reload without killing Lisp.
- LLM runner can crash and be restarted without losing backend state.
- Lisp can be restarted; UI reconnects and reloads project state from disk.


---

# 2. Processes and Responsibilities

## 2.1 UI Process

- Runs inside Lauri (your Lisp-hosted Tauri-equivalent) using a React front-end.
- Talks to the Lisp backend using:
  - WebSocket or HTTP-based JSON-RPC.
- Responsibilities:
  - Rendering code and semantic overlays.
  - Providing navigation (search, go-to-def, backlinks).
  - Exposing conversational interface to Mnemos.
  - Displaying streaming tokens from the LLM.

## 2.2 Lisp Backend Process

Runs in a Common Lisp image (e.g., SBCL) and loads Mnemos packages:

- `mnemos.core`   — shared utilities and types.
- `mnemos.config` — configuration loading and validation.
- `mnemos.index`  — indexing, symbol graph, embeddings, notes.
- `mnemos.tools`  — tool implementations used by the LLM.
- `mnemos.llm`    — client for the LLM runner.
- `mnemos.server` — JSON-RPC server for the UI.

It also:
- Connects to SLY/Slynk for live coding.
- Maintains in-memory caches for hot project data.
- Supervises the external LLM runner process.

## 2.3 LLM Runner Process

Can be:
- `ollama serve`
- `llama-server --model qwen-32b-q4.gguf --port 4010`
- A minimal custom wrapper around llama.cpp or MLX.

Requirements:
- Accepts JSON payload with `messages`, `tools`, and other parameters.
- Supports streaming.
- Returns structured tool call requests when appropriate.


---

# 3. LLM Flow

## 3.1 Input Construction

For each user action, Lisp:

1. Collects project context:
   - Current buffer and selection.
   - Surrounding lines for context.
   - Symbol definition and references from the index.
   - Related notes and backlinks.
2. Chooses a prompt template:
   - `explain-function`
   - `compare-two-implementations`
   - `trace-call-chain`
   - `summarise-subsystem`
3. Renders a `messages` array:
   - System: stable instruction for Mnemos' persona and boundaries.
   - Context: code snippets and summaries.
   - User: the user’s question or command.

## 3.2 Tool Schemas

Lisp provides the LLM with a list of tools, e.g.:

- `find-usages`
- `find-symbol-definition`
- `search-embedding`
- `open-file`
- `highlight-region`
- `get-ast-fragment`

Each tool includes a JSON schema so the LLM can format arguments.

## 3.3 Streaming and Tool Calls

The LLM responds with a stream of events:

- `token` — partial text to display.
- `tool_call` — request for a tool to be executed.
- `done` — completion of the response.

Lisp:
- For tokens: forwards them to the UI.
- For tool calls:
  - Executes the corresponding `mnemos.tools` function.
  - Sends a follow-up “tool result” message back to the model.
- For completion:
  - Finalises the response and returns structured metadata (e.g., suggested notes, file locations).

## 3.4 Example Tool Call Exchange (Conceptual)

1. LLM:  
   `tool_call: { "name": "find-usages", "arguments": {"symbol": "parse-expression"} }`
2. Lisp:
   - Runs `mnemos.index:find-usages`.
   - Returns results as `tool_result` message to LLM:
     - file paths
     - line/column ranges
3. LLM:
   - Incorporates the fetched usages into its explanation.


---

# 4. Indexing and Project State

Mnemos builds a multi-layer index over your project.

## 4.1 Components

- **File scanner** — discovers files to index based on config.
- **AST engine** — Tree-sitter based parsers per language.
- **Symbol graph** — global representation of definitions and references.
- **Embedding index** — vector representations for search and clustering.
- **Notes and backlinks** — user-authored annotations and cross-links.

## 4.2 Pipeline

1. Discover files under the project root.
2. For each file:
   - Parse into AST.
   - Extract declarations, definitions, and references.
3. Update symbol graph:
   - Nodes: symbols, files, notes.
   - Edges: “defines”, “refers-to”, “explains”, “duplicates”, etc.
4. Generate embeddings:
   - Functions and methods.
   - Important comments/docstrings.
   - Notes and higher-level summaries.
5. Persist index state:
   - Lightweight per-file caches.
   - Global symbol/embedding databases.
   - Note graph and backlinks.

## 4.3 Project Snapshots

Mnemos may periodically write snapshots:

- Project metadata.
- Index version and layout.
- Summaries of key areas.
- Checkpoints for faster resume.

Snapshots allow:
- Quickly reopening large projects.
- Migrating between machines or model versions with less work.


---

# 5. Tools

Tools are the bridge between the LLM and your actual capabilities.

## 5.1 Design Goals

- Keep tools **small and composable**.
- Expose **semantic operations**:
  - “Find usages of symbol X”.
  - “Get AST fragment for this region”.
  - “Search for code related to Y”.
- Keep them **stateless** where possible; state lives in the index.

## 5.2 Example Tool Set

### Code Understanding

- `find-symbol-definition`
- `find-usages`
- `list-children` (methods in a class, functions in a module)
- `get-ast-fragment`
- `resolve-import`

### Semantic Navigation

- `search-embedding`
- `find-related-notes`
- `cluster-functions`
- `suggest-refactor-targets`

### UI Integration

- `open-file`
- `highlight-region`
- `show-note-panel`
- `create-note`

## 5.3 Tool Implementation Shape (Lisp)

A tool is a function from JSON arguments to JSON results.

```lisp
(defun tool-find-usages (args)
  (let* ((symbol (gethash "symbol" args))
         (results (mnemos.index:find-usages symbol)))
    `(("status" . "ok")
      ("results" . ,results))))
```

A dispatcher maps tool names to these functions:

```lisp
(defun dispatch-tool (name args)
  (ecase name
    ("find-usages" (tool-find-usages args))
    ("find-symbol-definition" (tool-find-symbol-definition args))
    ;; ...
    ))
```


---

# 6. Startup Flow

## 6.1 Phases

1. UI boot.
2. Lisp backend initialisation.
3. Model presence check.
4. LLM runner startup.
5. Project load and indexing.
6. Ready for interaction.

## 6.2 Model Presence Check

Steps:

- Check configured model directory:
  - Are weights present?
  - Is checksum valid?
- If missing or invalid:
  - Ask the UI to present a “download model” dialog.
  - Offer:
    - Small model — faster, smaller, less smart.
    - Full model — slower, larger, more capable.
  - Download and verify.
  - Store under:
    - `~/Library/Application Support/Mnemos/models/<model-name>/`

## 6.3 LLM Runner Startup

Lisp calls something like:

```text
llama-server --model /path/to/qwen-32b-q4.gguf --port 4010
```

or uses an Ollama model.

- Lisp waits for an HTTP health check to succeed.
- Stores the runner PID.
- Logs startup events.

## 6.4 Project Load

- Determine the current project root (from UI request or last session).
- Load:
  - AST caches (if any).
  - Symbol graph.
  - Embedding index.
  - Notes and backlinks.

- If not indexed yet:
  - Kick off a background indexing job.
  - UI shows progress.

## 6.5 Ready

Once minimal indexes exist, the system transitions to “ready”:

- UI enables queries.
- LLM-backed features become active.


---

 # 7. Configuration

 ## 7.1 Global Configuration

 Example keys:

 - Engine type: `"ollama"`, `"llama-cpp"`, `"mlx"`.
 - Host/port for LLM runner.
 - Default model name.
 - Paths for logs and models.

 Stored in a file like:

 ```toml
 [engine]
 type = "llama-cpp"
 host = "127.0.0.1"
 port = 4010
 model_path = "/Users/you/Library/Application Support/Mnemos/models/qwen-32b-q4.gguf"

[paths]
 models_dir = "/Users/you/Library/Application Support/Mnemos/models"
 logs_dir = "/Users/you/Library/Application Support/Mnemos/logs"
 ```

 ## 7.2 Project Configuration

 Project-specific config:

 ```toml
 [project]
 root = "/path/to/project"
 name = "my-cool-project"

[index]
 include = ["src", "lib"]
 exclude = ["dist", "node_modules", ".git"]
 ```

 Used to tweak indexing and ignore noise.


---

# 8. Storage Layout

## 8.1 Global

```text
~/Library/Application Support/Mnemos/
  models/
    qwen-32b-q4.gguf
    qwen-8b-q4.gguf
  logs/
    mnemos-backend.log
    llm-runner.log
  cache/
    embeddings-cache/
```

## 8.2 Per-Project

```text
~/Library/Application Support/Mnemos/projects/<project-id>/
  config.toml
  ast/
    src__main.lisp.json
    src__foo.lisp.json
  symbols/
    graph.sqlite
  embeddings/
    vectors.sqlite
  notes/
    notes.json
  snapshots/
    snapshot-0001.json
```


---

# 9. UI Bridge

The UI and Lisp backend talk over JSON-RPC.

## 9.1 Example Methods

- `mnemos.open-project`
- `mnemos.list-files`
- `mnemos.get-file`
- `mnemos.explain-region`
- `mnemos.find-usages`
- `mnemos.search`

## 9.2 Example Request

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "mnemos.explain-region",
  "params": {
    "file": "src/main.lisp",
    "start": {"line": 10, "column": 0},
    "end": {"line": 40, "column": 0}
  }
}
```

## 9.3 Example Response

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "explanation": "This function initialises the server and sets up routes...",
    "references": [
      {"file": "src/routes.lisp", "line": 12},
      {"file": "src/config.lisp", "line": 5}
    ]
  }
}
```


---

# 10. Embedding System

## 10.1 Purpose

Embeddings let Mnemos:

- Find similar code snippets.
- Connect notes and code semantically.
- Provide “related to this” suggestions.
- Cluster functions or modules by behaviour.

## 10.2 What Gets Embedded

- Functions and methods.
- Important comments/docs.
- Notes and summaries.
- Occasionally, entire files or modules.

## 10.3 Storage

A simple pattern:

- `vectors.sqlite` with tables:
  - `items(id, kind, file, offset, length, metadata-json)`
  - `vectors(id, vector-blob)`

The index may be kept in memory for speed and flushed periodically.


---

# 11. Snapshots

## 11.1 Why Snapshots

Large projects can take time to index. Snapshots allow:

- Fast reopen.
- Rollbacks for experiments.
- Stable context for reproducible sessions.

## 11.2 Contents

A snapshot might include:

- Project config.
- Index version.
- Summary of major subsystems.
- Pointers to embeddings and notes.
- Checksums for model + source tree to detect drift.

## 11.3 Format

Typically JSON or a compact binary format:

```json
{
  "version": 1,
  "project_id": "my-cool-project",
  "created_at": "2025-11-16T12:34:56Z",
  "subsystems": [
    {"name": "parser", "summary": "..."},
    {"name": "backend", "summary": "..."}
  ]
}
```


---

