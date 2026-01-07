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
