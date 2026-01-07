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
