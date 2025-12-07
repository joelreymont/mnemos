Docs: `docs/`

## MCP Tools (MANDATORY)

**ALWAYS use MCP tools. NEVER use Bash when an MCP tool exists.** If broken, FIX IT - don't work around. MCP server: `backend/tools/hemis_mcp/src/main.rs`.

| Task | Tool | Key Params |
|------|------|------------|
| Git | `git_context` | `include_diff`, `diff_limit` |
| Work items | `bd_*` | `bd_create(title)`, `bd_close/show(id)`, `bd_list(status)` |
| Rust | `cargo_test/clippy/build` | `cwd`, `package`, `filter`, `fix`, `release` |
| Swift | `swift_test` | `cwd`, `filter` |
| Ask o3 | `ask_oracle` | `question` |

**Bash OK for:** git commit/push/pull, hemis CLI, installing deps.

## Demo Automation

Demo driver (`hemis-demo`) controls keyboard/mouse. **Ask for explicit confirmation ("go", "run it") before running.**

## Development Guidelines

### Commits
- One commit per logical feature with tests
- Short summaries, no emojis

### Testing (MANDATORY - ZERO TOLERANCE)
**ALL tests MUST pass. There are NO "pre-existing" failing tests.** If a test fails, FIX IT IMMEDIATELY before continuing. No exceptions.

- Every bug fix and feature MUST include tests
- Write tests FIRST or alongside implementation
- Run `cargo test` before committing - if ANY test fails, the commit is blocked
- Flaky tests must be either fixed or marked `#[ignore]` with explanation

### Workflow
- Run tests yourself, don't ask user
- Work continuously without pausing unless blocked
- **Beads: Create via `bd_create` BEFORE starting ANY task. Close when done. Each closure = one commit.**

### Key Files
| File | Purpose |
|------|---------|
| `backend/src/lib.rs` | RPC dispatch |
| `backend/src/ai_cli.rs` | AI providers |
| `backend/crates/storage/src/lib.rs` | SQLite |
| `backend/crates/treesitter/src/lib.rs` | Parser |

### RPC Methods
`notes/` create, get, update, delete, list-for-file, list-by-node, list-project, search, backlinks, reattach, buffer-update
`index/` add-file, search
`hemis/` search, status, index-project, list-files, get-file, explain-region, project-meta, open-project, save-snapshot, load-snapshot, shutdown

### Conventions
- **Staleness**: `nodeTextHash` = SHA256; search +/-20 lines for match
- **AI**: `HEMIS_AI_PROVIDER` env (codex/claude/none)
- **Tree-sitter**: Bundled; dynamic via `~/.config/hemis/grammars/`

### Demo Driver
`cd ../hemis-demo && swift run hemis-demo <script> --show-labels`
Scripts: `neovim`, `reattach` | Options: `--prepare-only`, `--skip-setup`, `--countdown N`, `--record`

## Creating MCP Tools

Add to `backend/tools/hemis_mcp/src/main.rs`. **Update this file when adding tools.**
