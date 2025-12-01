# Fix Failing Test

Systematically fix a failing test using structured reasoning.

## Arguments
- `$ARGUMENTS` - The failing test name or error output

## Instructions

### Phase 1: Understand
1. Read the failing test code
2. Read the source code it exercises
3. Identify what assertion fails and why

### Phase 2: Think (use extended thinking)
Before making changes, reason through:
- What is the test actually verifying?
- What behavior changed to break it?
- Is the test wrong or the implementation?
- What's the minimal fix?

### Phase 3: Fix
Apply ONE of these strategies:
- **If implementation is wrong**: Fix the source code
- **If test expectation is outdated**: Update the test
- **If test is flaky**: Add proper waits/retries or mock dependencies

### Phase 4: Verify
1. Run the specific test to confirm fix
2. Run related tests to ensure no regressions
3. Run full test suite with `/test-all`

## Common Test Issues

### Emacs
- **Path issues**: Use `expand-file-name` for absolute paths
- **Async timing**: Add `(sit-for 0.1)` or `(sleep-for 0.1)`
- **Process env**: Use `process-environment` not shell exports

### Neovim
- **Fast event context**: Wrap vim.notify in `vim.schedule()`
- **Async callbacks**: Use plenary.busted, not custom runners
- **State leaks**: Call `rpc.stop()` in cleanup
- **Module cache**: Reset with `package.loaded["module"] = nil`

## Anti-Patterns to Avoid
- Don't skip tests to make CI pass
- Don't add arbitrary sleeps without understanding the race
- Don't change test assertions to match broken behavior
