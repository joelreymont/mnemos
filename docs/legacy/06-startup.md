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
    - `~/Library/Application Support/Hemis/models/<model-name>/`

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
