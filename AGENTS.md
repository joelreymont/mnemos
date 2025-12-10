Docs: `docs/`

## MCP Tools (MANDATORY)

**ALWAYS use MCP tools. NEVER use Bash when an MCP tool exists.** If broken, FIX IT - don't work around. MCP server: `backend/tools/hemis_mcp/src/main.rs`.

| Task | Tool | Key Params |
|------|------|------------|
| Git | `git_context` | `include_diff`, `diff_limit` |
| Work items | `bd_*` | `bd_create(title)`, `bd_close/show(id)`, `bd_list(status)` |
| Rust | `cargo_test/clippy/build` | `cwd`, `package`, `filter`, `fix`, `release` |
| Swift | `swift_test` | `cwd`, `filter` ⚠️ **ASK FIRST for hemis-demo** |
| Ask o3 | `ask_oracle` | `question` |

**Bash OK for:** git commit/push/pull, hemis CLI, installing deps.

**⚠️ swift_test in hemis-demo REQUIRES PERMISSION** - E2E tests launch GUI and control keyboard/mouse.

## Demo Automation

Demo driver (`hemis-demo`) controls keyboard/mouse. **Ask for explicit confirmation ("go", "run it") before running.**

**Permission is ONE-TIME ONLY** - each demo run requires fresh permission. Do not re-run demos without asking again.

## Development Guidelines

### Commits
- One commit per logical feature with tests
- Short summaries, no emojis

### Testing (MANDATORY - ZERO TOLERANCE)
**ALL tests MUST pass. There are NO "pre-existing" failing tests.** If a test fails, FIX IT IMMEDIATELY before continuing. No exceptions.

**DO NOT claim tests are "pre-existing failures" - they are YOUR failures to fix.** The codebase owner maintains zero tolerance for broken tests. If tests fail, the code that broke them must be fixed, not the other way around.

- Every bug fix and feature MUST include tests
- Write tests FIRST or alongside implementation
- Flaky tests must be either fixed or marked `#[ignore]` with explanation
- **Types used in tests MUST have proper Codable/Decodable conformance**

#### Test Suites (run before committing)
| Suite | Command | Notes |
|-------|---------|-------|
| Rust unit/integration | `cargo test` | Runs all backend tests |
| Rust clippy | `cargo clippy` | Warnings = errors |
| Neovim UI (headless) | `cd ui/neovim && nvim --headless -u tests/minimal_init.lua -c "PlenaryBustedDirectory tests/ {minimal_init = 'tests/minimal_init.lua'}"` | Plenary-based, runs headless |
| Emacs UI (headless) | `emacs -Q --batch -L ui/emacs -l hemis.el -L ui/emacs/tests -l hemis-test.el -f ert-run-tests-batch-and-exit` | ERT-based, runs headless (from repo root) |
| VSCode E2E | `cd ui/vscode && npm test` | Playwright-based, mostly headless |

#### hemis-demo (REQUIRES PERMISSION)
**DO NOT run hemis-demo tests or demos without explicit user permission.** The demo driver controls keyboard/mouse and launches GUI applications.
- **`swift_test` MCP tool with cwd=hemis-demo** - launches GUI, ASK FIRST
- `swift test` in hemis-demo - launches GUI, ASK FIRST
- `swift run hemis-demo` - definitely launches GUI, ASK FIRST
- Any command that runs E2E tests in hemis-demo - ASK FIRST

### Warnings = Errors (MANDATORY - ZERO TOLERANCE)
**ALL warnings MUST be fixed. Warnings are errors.** Run `cargo clippy` and fix ALL warnings before committing.

- **NEVER use `#[allow(...)]` to suppress warnings** - fix the root cause
- `too_many_arguments` → refactor into a params struct
- `dead_code` → remove unused code
- Any other warning → fix it properly, don't suppress it
- If clippy suggests a fix, apply it. If it's wrong, understand why before ignoring.

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
