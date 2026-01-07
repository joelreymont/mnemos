# Check UI Parity Across Editors

Analyze feature implementation across Emacs and Neovim to ensure consistent behavior.

## Arguments
- `$ARGUMENTS` - The feature to check (e.g., "note overlay", "backlinks", "add note")

## Instructions

1. **Search for the feature** in both editors:
   - Emacs: `ui/emacs/mnemos.el`
   - Neovim: `ui/neovim/lua/mnemos/` directory

2. **Compare implementations**:
   - How does each editor render the feature?
   - What RPC methods does each use?
   - Are there behavioral differences?

3. **Check test coverage**:
   - Does Emacs have tests for this feature?
   - Does Neovim have tests for this feature?
   - Are the test assertions equivalent?

4. **Report findings**:

   ## Feature: [name]

   | Aspect | Emacs | Neovim |
   |--------|-------|--------|
   | Implementation | file:line | file:line |
   | RPC methods | list | list |
   | Test coverage | yes/no | yes/no |

   ### Differences
   - List any behavioral differences

   ### Recommendations
   - Suggest fixes for parity issues

## Key Files
```
# Emacs
ui/emacs/mnemos.el               # Main plugin
ui/emacs/tests/mnemos-test.el    # Tests

# Neovim
ui/neovim/lua/mnemos/init.lua    # Main plugin
ui/neovim/lua/mnemos/display.lua # Note display
ui/neovim/lua/mnemos/rpc.lua     # Backend communication
ui/neovim/lua/mnemos/commands.lua # User commands
ui/neovim/tests/display_spec.lua
ui/neovim/tests/integration_spec.lua
```
