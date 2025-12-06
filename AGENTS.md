The docs describing this app are in docs/

## Demo Automation

The demo driver (`hemis-demo`) takes control of keyboard/mouse. **Always ask for confirmation before running demos.** Wait for explicit "go", "run it", "start".

## MCP Tools (MANDATORY)

**ALWAYS use MCP tools. NEVER fall back to Bash when an MCP tool exists.**

**If an MCP tool is broken, FIX IT IMMEDIATELY.** Do not work around broken tools with Bash. The MCP server is in `backend/tools/hemis_mcp/src/main.rs`. Rebuild with `cargo_build` (cwd: `backend/tools/hemis_mcp`, release: true).

| Task | MCP Tool |
|------|----------|
| Git status | `git_context` |
| Work items | `bd_context`, `bd_list`, `bd_create`, `bd_close`, `bd_show`, `bd_update` |
| Rust tests | `cargo_test` |
| Rust clippy | `cargo_clippy` |
| Rust build | `cargo_build` |
| Swift tests | `swift_test` |
| Rebuild MCP | `cargo_build` with `cwd` and `release: true` |
| Ask o3 | `ask_oracle` |

**Parameters** (all optional unless noted):
- `cargo_test/clippy/build`: `cwd`, `package`, `filter` (test only), `fix` (clippy), `release` (build)
- `swift_test`: `cwd`, `filter`
- `git_context`: `include_diff`, `diff_limit`
- `bd_list`: `status` (ready/open/closed/in_progress), `label`, `limit`
- `bd_create`: `title` (required), `description`, `labels`, `priority`
- `bd_close/show`: `id` (required)
- `bd_update`: `id` (required), `status`, `add_labels`
- `ask_oracle`: `question` (required)

**When Bash IS appropriate:** git commit/push/pull, hemis CLI, installing deps, file operations not covered by MCP.

## Development Guidelines

### Commits
- One commit per logical feature with tests
- Short summaries, no emojis

### Testing (MANDATORY)
Every bug fix and feature MUST include tests. Write tests FIRST or alongside implementation.

### Workflow
- Run tests yourself, don't ask user
- Work continuously without pausing unless blocked
- Break work into small steps, execute end-to-end

**MANDATORY: Create beads for ALL new tasks** via `bd_create` BEFORE starting work. This includes tasks you discover while working. Close beads when done.

**MANDATORY: Commit after closing beads.** Each bead closure = one commit.

### Key Files
- `backend/src/lib.rs` - RPC dispatch
- `backend/src/ai_cli.rs` - AI provider integration
- `backend/crates/storage/src/lib.rs` - SQLite
- `backend/crates/treesitter/src/lib.rs` - Parser

### RPC Methods
- `notes/` - create, get, update, delete, list-for-file, list-by-node, list-project, search, backlinks, reattach, buffer-update
- `index/` - add-file, search
- `hemis/` - search, status, index-project, list-files, get-file, explain-region, project-meta, open-project, save-snapshot, load-snapshot, shutdown

### Conventions
- **Staleness**: `nodeTextHash` = SHA256 of node text; search +/-20 lines for matching hash
- **AI**: `HEMIS_AI_PROVIDER` env (codex/claude/none), Codex preferred
- **Tree-sitter**: Bundled (rust, python, js, ts, go, lua, c, cpp, java); dynamic via `~/.config/hemis/grammars/`

### Demo Driver
- Swift-based in `../hemis-demo/` (sibling directory)
- Run: `cd ../hemis-demo && swift run hemis-demo <script> --show-labels`
- Scripts: `neovim`, `reattach`
- Options: `--prepare-only`, `--skip-setup`, `--countdown N`, `--record`

## Creating New MCP Tools

Add to `backend/tools/hemis_mcp/src/main.rs`. Each tool should return minimal structured output with appropriate timeout.

**MANDATORY: Update this file when creating new tools.**
