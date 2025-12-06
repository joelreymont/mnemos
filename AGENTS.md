The docs describing this app are in docs/

## Demo Automation

The demo driver (`hemis-demo`) takes control of the keyboard and mouse. Before running any demo:
1. **Always ask for confirmation first** - never start demos automatically
2. Wait for the user to explicitly say "go", "run it", "start", etc.

When asked to work on demo-related code:
- Read and modify demo scripts
- Build the demo driver (`swift build` in hemis-demo)
- Run tests (`swift test` in hemis-demo)
- Ask before running demos, then run when confirmed

## MCP Tools (MANDATORY)

**ALWAYS use MCP tools first. NEVER fall back to Bash commands when an MCP tool exists.**

The hemis-mcp server provides low-token, purpose-built tools. Using MCP tools instead of raw commands reduces token usage by 10-50x and provides structured output.

### Tool Priority

| Task | USE THIS (MCP) | NOT THIS (Bash) |
|------|----------------|-----------------|
| Check git status | `mcp__hemis-mcp__git_context` | `git status && git diff` |
| View work items | `mcp__hemis-mcp__bd_context` | `bd ready` |
| Run tests | `mcp__hemis-mcp__cargo_test` | `cargo test` |
| Run clippy | `mcp__hemis-mcp__cargo_clippy` | `cargo clippy` |
| Build project | `mcp__hemis-mcp__cargo_build` | `cargo build` |
| Swift tests | `mcp__hemis-mcp__swift_test` | `swift test` |
| List issues | `mcp__hemis-mcp__bd_list` | `bd list` |
| Create issue | `mcp__hemis-mcp__bd_create` | `bd create` |
| Close issue | `mcp__hemis-mcp__bd_close` | `bd close` |
| Show issue | `mcp__hemis-mcp__bd_show` | `bd show` |
| Update issue | `mcp__hemis-mcp__bd_update` | `bd update` |
| Rebuild MCP | `mcp__hemis-mcp__rebuild_mcp` | `cargo build -p hemis-mcp --release` |

### Tool Reference

| Tool | Purpose | Output |
|------|---------|--------|
| `version` | Server version | `hemis-mcp v1` |
| `git_context` | Compact git status | Branch, changed files, optional diff |
| `bd_context` | List open beads | Work items with status and blockers |
| `cargo_test` | Run tests with summary | Pass/fail counts, first failures |
| `cargo_clippy` | Run clippy with summary | Warning/error counts, first issues |
| `cargo_build` | Build with summary | Success/fail, first errors |
| `swift_test` | Run swift test with summary | Pass/fail counts, failed test names |
| `bd_list` | List issues by status | Issues with id, title, blockers |
| `bd_create` | Create new issue | Issue ID |
| `bd_close` | Close an issue | Confirmation |
| `bd_show` | Show issue details | Full issue info |
| `bd_update` | Update issue status/labels | Confirmation |
| `rebuild_mcp` | Rebuild MCP server in release | Success/fail, errors |

### Parameters

**git_context:**
- `include_diff` (bool, default false): Include diff output
- `diff_limit` (int, default 100): Max diff lines

**bd_context:**
- `limit` (int, default 50): Max beads to return

**cargo_test:**
- `package` (string): Package to test (for workspace)
- `filter` (string): Test filter pattern

**cargo_clippy:**
- `cwd` (string): Working directory
- `package` (string): Package to check (for workspace)
- `fix` (bool, default false): Auto-fix warnings where possible

**cargo_build:**
- `cwd` (string): Working directory
- `package` (string): Package to build (for workspace)
- `release` (bool, default false): Build in release mode

**swift_test:**
- `cwd` (string): Working directory
- `filter` (string): Filter tests by name

**bd_list:**
- `status` (string, default "ready"): Filter by status (ready/blocked/open/closed/in_progress)
- `label` (string): Filter by label
- `limit` (int, default 20): Max results

**bd_create:**
- `title` (string, required): Issue title
- `description` (string): Issue description
- `labels` (array): Labels like "backend", "bug"
- `priority` (int, default 2): Priority 0-4 (0=highest)

**bd_close:**
- `id` (string, required): Issue ID to close

**bd_show:**
- `id` (string, required): Issue ID to show

**bd_update:**
- `id` (string, required): Issue ID to update
- `status` (string): New status (open/in_progress/closed)
- `add_labels` (array): Labels to add

**rebuild_mcp:**
- No parameters

### Anti-Patterns (DO NOT DO)

```bash
# WRONG - Don't run cargo commands directly
cargo test
cargo test -p hemis-storage
cargo clippy
cargo build
cargo build --release -p hemis-mcp

# WRONG - Don't run swift test directly
swift test

# WRONG - Don't run git status directly
git status
git diff

# WRONG - Don't run bd directly
bd ready
bd list
bd create "title"
bd close hemis-abc
bd show hemis-abc
bd update hemis-abc --status in_progress
```

```
# CORRECT - Use MCP tools
mcp__hemis-mcp__cargo_test
mcp__hemis-mcp__cargo_test with package: "hemis-storage"
mcp__hemis-mcp__cargo_clippy
mcp__hemis-mcp__cargo_clippy with package: "hemis-storage", fix: true
mcp__hemis-mcp__cargo_build
mcp__hemis-mcp__cargo_build with release: true
mcp__hemis-mcp__swift_test
mcp__hemis-mcp__swift_test with filter: "KeyNotation"
mcp__hemis-mcp__git_context
mcp__hemis-mcp__git_context with include_diff: true
mcp__hemis-mcp__bd_context
mcp__hemis-mcp__bd_list with status: "open"
mcp__hemis-mcp__bd_create with title: "Fix bug", labels: ["backend"]
mcp__hemis-mcp__bd_close with id: "hemis-abc"
mcp__hemis-mcp__bd_show with id: "hemis-abc"
mcp__hemis-mcp__bd_update with id: "hemis-abc", status: "in_progress"
mcp__hemis-mcp__rebuild_mcp
```

### When Bash IS Appropriate

Only use Bash for operations MCP doesn't cover:
- Git commits, push, pull, rebase
- Running the `hemis` CLI directly
- Installing dependencies
- File operations outside the test/status workflow
- Complex bd queries not covered by MCP tools

### Server Location

- Configuration: `.mcp.json` (project root)
- Codex config: `$CODEX_HOME/config.toml` (now `~/.config/codex/config.toml`; symlink kept at `~/.codex/config.toml`) must include `[mcp_servers.hemis-mcp]` pointing to `/Users/joel/Work/hemis/backend/tools/hemis_mcp/target/release/hemis-mcp` with `cwd = "/Users/joel/Work/hemis"` (Codex 0.64.0 does not auto-load `.mcp.json`)
- Server source: `backend/tools/hemis_mcp/`

### Creating New MCP Tools

**If you find yourself repeatedly running the same Bash commands, create a new MCP tool.**

Add new tools to `backend/tools/hemis_mcp/src/main.rs`. Each tool should:
1. Return structured, minimal output (not raw command output)
2. Have a clear single purpose
3. Include timeout appropriate to the operation

Good candidates for new MCP tools:
- Any operation you run more than 3 times in a session
- Commands with verbose output that needs filtering
- Multi-step operations that should be atomic

**MANDATORY: When you create a new MCP tool, you MUST update this AGENTS.md file:**
1. Add the tool to the Tool Priority table (if it replaces a Bash command)
2. Add the tool to the Tool Reference table
3. Document parameters in the Parameters section
4. Add anti-patterns showing the wrong Bash way vs correct MCP way

**Failure to update documentation means the tool won't be used. Do not skip this step.**

## Development Guidelines

### Git Configuration

Author: `Joel Reymont <18791+joelreymont@users.noreply.github.com>`

### Commits

- One commit per logical feature with its tests
- Short, succinct summaries
- Combine "fixed this" commits with their implementation commits
- No emojis in commits, code, or documentation

### Testing

**MANDATORY: Every bug fix and feature MUST include tests.** No exceptions. If you fix a bug, write a test that would have caught it. If you add a feature, write tests that verify it works. Untested code is broken code waiting to happen.

Before submitting any fix or feature:
1. Ask yourself: "What test would have caught this bug?" or "What tests verify this feature?"
2. Write those tests FIRST or alongside the implementation
3. Run tests to verify they pass
4. If existing test infrastructure doesn't cover your change, add new test files

Test requirements:
- Follow existing test infrastructure patterns
- Keep tests short and focused
- Add purpose comment at start, minimize other comments
- No colors or emojis in test output
- Test edge cases, not just happy paths
- For timing/race conditions: test retry behavior, timeouts, delayed resource availability

### Code Style

- No marketing language; use technical facts
- Avoid words like "comprehensive", "complete", "critical"
- If uncertain, ask for help

### Workflow

- Never ask the user to run tests; run them yourself
- Work continuously without pausing for confirmation unless blocked
- Prioritize optimal/performant solutions even if harder to implement
- Break work into small steps; execute end-to-end
- State assumptions briefly and continue
- Update Project Reference section after major changes (stable knowledge only)
- Commit after completing major changes (don't batch unrelated work)

**MANDATORY: ALWAYS create beads for new tasks.** Every bug, feature, or improvement MUST have a bead created via `mcp__hemis-mcp__bd_create` BEFORE starting work. No exceptions. This includes tasks the user gives you AND tasks you discover while working. Close beads when done. Never let tasks exist only in your head or in TodoWrite - beads are the source of truth for work items.

**MANDATORY: Commit after closing beads.** When you complete work and close a bead, immediately commit those changes. Do not batch multiple unrelated bead completions into one commit. Each bead closure should result in a commit.

### Docs
- See `docs/ARCHITECTURE.md` for backend/frontend architecture and stale-note behavior.

### Demo Driver
- Swift-based demo automation in sibling directory `../hemis-demo/` (NOT in this repo)
- MUST run from hemis-demo directory: `cd ../hemis-demo && swift run hemis-demo <script> --show-labels`
- Scripts: `neovim` (full demo), `reattach` (stale note detection and reattach flow)
- Options: `--prepare-only`, `--skip-setup`, `--countdown N`, `--record`, `--list-scripts`
- Aborts if target app loses focus or window occluded (>30% overlap)
- Script config in `scripts/demo.json`

## Slash Commands

Custom commands in `.claude/commands/` for common workflows.

### Testing Commands
| Command | When to Use |
|---------|-------------|
| `/test-all` | After any code change; runs Emacs + Neovim in parallel |
| `/debug-test <name>` | When a test fails; provides systematic investigation |
| `/fix-test <name>` | After identifying failure; enforces think-first approach |
| `/update-snapshots` | When display output intentionally changes |

### Development Commands
| Command | When to Use |
|---------|-------------|
| `/build` | Before testing; verifies backend compiles |
| `/add-rpc-method <name>` | When adding new backend functionality |
| `/ui-parity <feature>` | When implementing features; ensures cross-editor consistency |

### Debugging Commands
| Command | When to Use |
|---------|-------------|
| `/rpc-debug` | Connection refused, socket closed, env var issues |
| `/backend-status` | Quick health check before debugging |

## Debugging Patterns

**Emacs Process Environment:**
- Use `make-process` with `process-environment`, NOT `call-process-shell-command` with shell exports
- Expand paths before changing `default-directory`

**Neovim Async Issues:**
- Wrap `vim.notify` in `vim.schedule()` for libuv callbacks
- Use plenary.busted for async test handling
- Reset module cache: `package.loaded["hemis.rpc"] = nil`
- Always call `rpc.stop()` in test cleanup

## Project Reference

### Status
- Rust: 147 tests passing | All RPC methods have integration coverage
- Work items: `bd ready`

### Architecture
See `docs/ARCHITECTURE.md` for system design and diagrams.

### Key Files
- `backend/src/lib.rs` - RPC method dispatch
- `backend/src/ai_cli.rs` - AI provider integration (Codex/Claude CLI)
- `backend/crates/storage/src/lib.rs` - SQLite schema and queries
- `backend/crates/treesitter/src/lib.rs` - Parser service

### RPC Methods
`notes/` - create, get, update, delete, list-for-file, list-by-node, list-project, search, backlinks, reattach, buffer-update
`index/` - add-file, search
`hemis/` - search, status, index-project, list-files, get-file, explain-region, project-meta, open-project, save-snapshot, load-snapshot, shutdown

### Conventions

**Note Staleness:**
- `nodeTextHash`: SHA256 of tree-sitter node text at creation
- Position tracking: search +/-20 lines for matching hash
- Fallback: git-based `stale` flag if no hash/tree-sitter

**AI Integration:**
- `HEMIS_AI_PROVIDER` env: codex, claude, none/disabled
- Auto-detection: Codex preferred, Claude fallback
- `hemis/index-project` + `includeAI` -> `.hemis/analysis.md`

**Tree-sitter:**
- Bundled: rust, python, javascript, typescript, go, lua, c, cpp, java
- Dynamic: `~/.config/hemis/grammars/`, config `~/.config/hemis/languages.toml`
- CLI: `hemis grammar list/fetch/build`

### Quick Commands
```sh
# RPC connection
ls -la ~/.hemis/hemis.sock
pgrep -fl 'hemis.*--serve'
pkill -f 'hemis --serve' && rm -f ~/.hemis/hemis.{sock,lock}

# Tests
cargo test                      # All tests
cargo test --test rpc_flow      # RPC flow tests
cargo test -p hemis-treesitter  # Treesitter tests
```
