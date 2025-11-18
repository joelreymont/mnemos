# 8. Storage Layout

## 8.1 Global

```text
~/Library/Application Support/Hemis/
  models/
    qwen-32b-q4.gguf
    qwen-8b-q4.gguf
  logs/
    hemis-backend.log
    llm-runner.log
  cache/
    embeddings-cache/
```

## 8.2 Per-Project

```text
~/Library/Application Support/Hemis/projects/<project-id>/
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
