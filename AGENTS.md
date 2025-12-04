The docs describing this app are in docs/

## MCP Tools (MANDATORY)

**ALWAYS use MCP tools first. NEVER fall back to Bash commands when an MCP tool exists.**

The hemis-mcp server provides low-token, purpose-built tools. Using MCP tools instead of raw commands reduces token usage by 10-50x and provides structured output.

### Tool Priority

| Task | USE THIS (MCP) | NOT THIS (Bash) |
|------|----------------|-----------------|
| Check git status | `mcp__hemis-mcp__git_context` | `git status && git diff` |
| View work items | `mcp__hemis-mcp__bd_context` | `bd ready` |
| Run tests | `mcp__hemis-mcp__cargo_test_summary` | `cargo test` |

### Tool Reference

| Tool | Purpose | Output |
|------|---------|--------|
| `version` | Server version | `hemis-mcp v1` |
| `git_context` | Compact git status | Branch, changed files, optional diff |
| `bd_context` | List open beads | Work items with status and blockers |
| `cargo_test_summary` | Run tests with summary | Pass/fail counts, first failures |

### Parameters

**git_context:**
- `include_diff` (bool, default false): Include diff output
- `diff_limit` (int, default 100): Max diff lines

**bd_context:**
- `limit` (int, default 50): Max beads to return

**cargo_test_summary:**
- `package` (string): Package to test (for workspace)
- `filter` (string): Test filter pattern

### Anti-Patterns (DO NOT DO)

```bash
# WRONG - Don't run cargo test directly
cargo test
cargo test -p hemis-storage

# WRONG - Don't run git status directly
git status
git diff

# WRONG - Don't run bd directly
bd ready
bd list
```

```
# CORRECT - Use MCP tools
mcp__hemis-mcp__cargo_test_summary
mcp__hemis-mcp__cargo_test_summary with package: "hemis-storage"
mcp__hemis-mcp__git_context
mcp__hemis-mcp__git_context with include_diff: true
mcp__hemis-mcp__bd_context
```

### When Bash IS Appropriate

Only use Bash for operations MCP doesn't cover:
- Git commits, push, pull, rebase
- Running the `hemis` CLI directly
- Installing dependencies
- File operations outside the test/status workflow

### Server Location

- Configuration: `.mcp.json` (project root)
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

## Development Guidelines

### Git Configuration

Author: `Joel Reymont <18791+joelreymont@users.noreply.github.com>`

### Commits

- One commit per logical feature with its tests
- Short, succinct summaries
- Combine "fixed this" commits with their implementation commits
- No emojis in commits, code, or documentation

### Testing

- Follow existing test infrastructure patterns
- Keep tests short and focused
- Add purpose comment at start, minimize other comments
- No colors or emojis in test output
- Ensure test coverage for new features

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
- Update CONTEXT.md after major steps (stable knowledge only, not session history)
- Use `bd` (beads) for work item tracking
- Commit after completing major changes (don't batch unrelated work)

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

## Context Files

- **CONTEXT.md** - Quick reference (test counts, RPC methods, conventions, debugging)
- **docs/ARCHITECTURE.md** - System design, diagrams, architectural decisions
- **beads (`bd`)** - Work item tracking; run `bd ready` to see available work
