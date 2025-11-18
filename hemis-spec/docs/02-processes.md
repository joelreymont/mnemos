# 2. Processes and Responsibilities

## 2.1 UI Process

- Runs inside Lauri (your Lisp-hosted Tauri-equivalent) using a React front-end.
- Talks to the Lisp backend using:
  - WebSocket or HTTP-based JSON-RPC.
- Responsibilities:
  - Rendering code and semantic overlays.
  - Providing navigation (search, go-to-def, backlinks).
  - Exposing conversational interface to Hemis.
  - Displaying streaming tokens from the LLM.

## 2.2 Lisp Backend Process

Runs in a Common Lisp image (e.g., SBCL) and loads Hemis packages:

- `hemis.core`   — shared utilities and types.
- `hemis.config` — configuration loading and validation.
- `hemis.index`  — indexing, symbol graph, embeddings, notes.
- `hemis.tools`  — tool implementations used by the LLM.
- `hemis.llm`    — client for the LLM runner.
- `hemis.server` — JSON-RPC server for the UI.

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
