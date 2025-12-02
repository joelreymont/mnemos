The docs describing this app are in docs/

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
- Update CONTEXT.md after each major step (not checked into git)
- Do NOT read or update SESSION.md (deprecated, use CONTEXT.md instead)

### Docs
- See `docs/ARCHITECTURE.md` for the current backend/frontend architecture and stale-note behavior.

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

## Test Counts (keep updated)
- Rust backend: 42 tests (including 14 RPC flow + 13 snapshot tests)
- Emacs UI: 47 tests (37 unit + 10 demo flow)
- Neovim UI: 39 tests (21 display + 18 integration)
- VS Code UI: 22 tests (10 decoration + 12 integration)

### Common Debugging Patterns

**RPC Connection Issues:**
1. Check socket: `ls -la ~/.hemis/hemis.sock`
2. Check process: `pgrep -fl 'hemis.*--serve'`
3. Clean restart: `pkill -f 'hemis --serve' && rm -f ~/.hemis/hemis.{sock,lock}`

**Emacs Process Environment:**
- Use `make-process` with `process-environment`, NOT `call-process-shell-command` with shell exports
- Expand paths before changing `default-directory`

**Neovim Async Issues:**
- Wrap `vim.notify` in `vim.schedule()` for libuv callbacks
- Use plenary.busted for async test handling
- Reset module cache: `package.loaded["hemis.rpc"] = nil`
- Always call `rpc.stop()` in test cleanup

### Session Management

1. **CONTEXT.md** - The primary context file for session progress
   - This is THE ONLY file to update with session progress and current state
   - Current development phase and active tasks
   - Recent changes and bug fixes (this session)
   - Known issues and workarounds
   - Test status and results
   - Update after each major step or milestone
   - DO NOT use SESSION.md (deprecated)

2. **SESSION.md** - Deprecated
   - DO NOT read or update this file
   - Use CONTEXT.md instead

3. **After compaction** - Always re-read these files:
   - Read AGENTS.md for development guidelines and instructions
   - Read CONTEXT.md for current state and progress
   - This ensures continuity across context window resets

4. **Hooks**:
   - `hooks/pre-compaction.sh` - Updates CONTEXT.md before compaction
   - `hooks/post-compaction.sh` - Reloads AGENTS.md and CONTEXT.md after compaction
