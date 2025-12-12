# Hemis

Docs: `docs/`

## Rules

Behavior rules are in:
- **Global**: `~/.claude/rules/` (zero-tolerance, workflow, oracle, file-org, cost-control)
- **Local**: `.claude/rules/` (demo-permission)

This file contains project-specific technical info only.

## MCP Tools
| Task | Tool | Params |
|------|------|--------|
| Git | `git_context` | `include_diff`, `diff_limit` |
| Issues | `bd_*` | `bd_create/close/show/list` |
| Rust | `cargo_test/clippy/build` | `cwd`, `package`, `filter`, `fix` |
| Swift | `swift_test` | **ASK FIRST** (launches GUI) |
| AI | `ask_oracle` | `question` |

Bash OK for: git commit/push/pull, hemis CLI, deps

## Test Suites
| Suite | Command |
|-------|---------|
| Rust | `cargo test` |
| Clippy | `cargo clippy` |
| Neovim | `cd ui/neovim && nvim --headless -u tests/minimal_init.lua -c "PlenaryBustedDirectory tests/ {minimal_init = 'tests/minimal_init.lua'}"` |
| Emacs | `emacs -Q --batch -L ui/emacs -l hemis.el -L ui/emacs/tests -l hemis-test.el -f ert-run-tests-batch-and-exit` |
| VSCode | `cd ui/vscode && npm test` |

## Key Files
`backend/src/lib.rs` (RPC) | `ai_cli.rs` (AI) | `crates/storage` (SQLite) | `crates/treesitter` (parser)

## RPC
`notes/` create,get,update,delete,list-for-file,list-by-node,list-project,search,backlinks,reattach,buffer-update
`index/` add-file,search
`hemis/` search,status,index-project,list-files,get-file,explain-region,project-meta,open-project,save-snapshot,load-snapshot,shutdown

## Conventions
- Staleness: `nodeTextHash`=SHA256, search +/-20 lines
- AI: `HEMIS_AI_PROVIDER` env (codex/claude/none)
- Tree-sitter: bundled; dynamic via `~/.config/hemis/grammars/`

## Demo Driver
`cd ../hemis-demo && swift run hemis-demo <script> --show-labels`
Scripts: full, neovim, reattach | Options: --editor neovim|emacs|vscode, --prepare-only, --skip-setup, --countdown N, --record
See `.claude/rules/demo-permission.md` for permission requirements.

## MCP Development
Add tools to `backend/tools/hemis_mcp/src/main.rs`. Update this file when adding.
