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
