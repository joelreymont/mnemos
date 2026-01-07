# Debug Failing Test

Debug a specific failing test with full context analysis.

## Arguments
- `$ARGUMENTS` - The test name or pattern (e.g., "mnemos-index-rust" or "notes/create")

## Instructions

1. **Identify the test file** based on the test name:
   - Emacs tests: `ui/emacs/tests/mnemos-test.el`
   - Neovim display tests: `ui/neovim/tests/display_spec.lua`
   - Neovim integration tests: `ui/neovim/tests/integration_spec.lua`

2. **Read the test** to understand what it does:
   - Search for the test name pattern in the appropriate file
   - Read the test function and any helper functions it uses

3. **Identify dependencies**:
   - What source files does the test exercise?
   - Does it require the backend? (integration tests do)
   - What mock/helper functions does it use?

4. **Run the specific test**:
   - Emacs: `emacs -batch ... -f ert --eval "(ert-run-tests-batch-and-exit \"TEST_NAME\")"`
   - Neovim: Look at `it("test description"...` and run with verbose output

5. **Analyze the error**:
   - Read the relevant source code the test exercises
   - Check for recent changes that might have broken it
   - Look at similar passing tests for comparison

6. **Report findings**:
   - What the test expects vs what actually happens
   - Root cause hypothesis
   - Suggested fix

## Test File Locations
```
ui/emacs/tests/mnemos-test.el     # All Emacs tests
ui/emacs/mnemos.el                # Emacs plugin source

ui/neovim/tests/display_spec.lua     # Display tests
ui/neovim/tests/integration_spec.lua # Backend integration tests
ui/neovim/tests/helpers.lua          # Test utilities
ui/neovim/lua/mnemos/display.lua      # Display source
ui/neovim/lua/mnemos/rpc.lua          # RPC client source
```
