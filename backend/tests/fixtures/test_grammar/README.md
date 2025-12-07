# Test Grammar Fixture

This directory contains a minimal tree-sitter grammar for testing purposes.

## Purpose

This test grammar is used by the `dynamic_grammar_fetch_build_and_use` test in `tests/rpc_flow.rs` to provide a hermetic test environment that doesn't depend on external network resources.

## Contents

- `grammar.js`: A minimal JSON-like grammar definition
- `src/parser.c`: Pre-generated parser code from tree-sitter

## Why This Exists

Previously, the test fetched a grammar from `https://github.com/tree-sitter/tree-sitter-json` via git clone, which:
- Required network access (non-hermetic)
- Could fail due to network issues or rate limiting
- Made the test slower and less reliable

By including a local copy of a minimal test grammar, the test is now:
- **Hermetic**: No external dependencies
- **Fast**: No network I/O
- **Reliable**: Cannot fail due to external factors
- **Isolated**: Each test run is independent

## Grammar Details

The grammar is a simplified version of JSON that supports:
- Objects: `{ "key": "value" }`
- Arrays: `[1, 2, 3]`
- Strings: `"text"`
- Numbers: `42` or `3.14`
- Booleans: `true`, `false`
- Null: `null`

It includes the essential tree-sitter node types used by the test:
- `document` (root)
- `object`, `array` (containers)
- `string`, `number`, `true`, `false`, `null` (skip nodes)

This is sufficient for testing the dynamic grammar loading, building, and usage flow.
