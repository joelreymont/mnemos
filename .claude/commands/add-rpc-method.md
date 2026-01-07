# Add New RPC Method

Guide for adding a new JSON-RPC method across backend and all editor UIs.

## Arguments
- `$ARGUMENTS` - The method name (e.g., "notes/archive" or "mnemos/export")

## Instructions

### Step 1: Define in Backend

1. **Add handler** in `backend/src/lib.rs`:
   - Find the `match method` block
   - Add new arm with params parsing and handler call

2. **Add types** in `backend/crates/rpc/src/lib.rs` if needed:
   - Request params struct
   - Response struct

3. **Add tests** in `backend/tests/rpc_flow.rs` or `rpc_snapshot.rs`

### Step 2: Implement in Emacs

1. **Add function** in `ui/emacs/mnemos.el`:
   ```elisp
   (defun mnemos-METHOD-NAME ()
     "Description."
     (interactive)
     (mnemos--request "METHOD/NAME"
       `(:param1 ,value1)
       (lambda (result)
         ;; Handle result
         )))
   ```

2. **Add keybinding** if user-facing:
   ```elisp
   (define-key mnemos-mode-map (kbd "C-c h X") #'mnemos-METHOD-NAME)
   ```

3. **Add test** in `ui/emacs/tests/mnemos-test.el`

### Step 3: Implement in Neovim

1. **Add function** in `ui/neovim/lua/mnemos/commands.lua`:
   ```lua
   function M.method_name()
     rpc.request("METHOD/NAME", { param1 = value1 }, function(err, result)
       -- Handle result
     end)
   end
   ```

2. **Add command** in `ui/neovim/lua/mnemos/init.lua`:
   ```lua
   vim.api.nvim_create_user_command("MnemosMethodName", commands.method_name, {})
   ```

3. **Add keybinding** in setup if user-facing

4. **Add test** in `ui/neovim/tests/integration_spec.lua`

### Step 4: Verify Parity

Run `/ui-parity METHOD_NAME` to ensure both editors:
- Use same RPC method name
- Pass same parameters
- Handle response consistently
- Have equivalent test coverage

### Step 5: Update Documentation

Add method to `docs/ARCHITECTURE.md` RPC methods section if it's a core feature.

## RPC Method Naming Convention
- `notes/*` - Note CRUD operations
- `mnemos/*` - System/project operations
- Use lowercase with hyphens: `notes/list-for-file`
