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

### Context and Compaction

- Before compaction, append the current session history in markdown to HISTORY.md and snapshot current context to CONTEXT.md
- After compaction, re-read AGENTS.md and CONTEXT.md to restore instructions and context
- Use `hooks/pre-compaction.sh` (stdin or file args) to perform the pre-compaction writes and `hooks/post-compaction.sh` to reload instructions after compaction
