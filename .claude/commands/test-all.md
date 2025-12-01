# Run All UI Tests

Run all UI tests across Emacs and Neovim in parallel. Requires the Rust backend to be built.

## Instructions

1. First verify the backend is built:
   ```bash
   cargo build 2>&1 | tail -5
   ```

2. Run tests in parallel using the Bash tool with these commands:
   - **Emacs** (from project root):
     ```bash
     cd /Users/joel/Work/hemis && HEMIS_BACKEND=./target/debug/hemis emacs -batch -L ui/emacs -L ui/emacs/tests -l ert -l hemis.el -l hemis-test.el -f ert-run-tests-batch-and-exit 2>&1
     ```
   - **Neovim** (from ui/neovim directory):
     ```bash
     cd /Users/joel/Work/hemis/ui/neovim && HEMIS_TEST_BACKEND=/Users/joel/Work/hemis/target/debug/hemis nvim --headless -u tests/minimal_init.lua -c "luafile tests/run.lua" 2>&1
     ```

3. Report results in this format:
   | Editor | Tests | Status |
   |--------|-------|--------|
   | Emacs | X/Y | passed/failed |
   | Neovim | X/Y | passed/failed |

4. If any tests fail:
   - Extract the specific test name and error message
   - Read the failing test file to understand what it tests
   - Suggest investigation steps

## Expected Counts
- Emacs: 37 tests
- Neovim: 32 tests (21 display + 11 integration)
