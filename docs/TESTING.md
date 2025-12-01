# Hemis UI Testing Strategy

## Overview

All three UIs (Emacs, Neovim, VS Code) can be tested headlessly by capturing and verifying "display state" - the structured representation of what the user would see.

## Display State Model

Each UI renders notes as:

```
┌──────────────────────────────────────────────────────────────┐
│  Component        │ Emacs          │ Neovim       │ VS Code  │
├───────────────────┼────────────────┼──────────────┼──────────┤
│  Note indicator   │ Overlay        │ Extmark      │ Decoration│
│  Notes list       │ Buffer         │ Buffer       │ QuickPick │
│  Backlinks        │ Buffer         │ Buffer       │ QuickPick │
│  Search results   │ Buffer         │ Quickfix     │ QuickPick │
│  Help             │ Message        │ Notify       │ Document  │
└──────────────────────────────────────────────────────────────┘
```

## Testing Layers

### Layer 1: Unit Tests (Mock Backend)

Test UI logic without real backend. Mock RPC responses.

**What to verify:**
- Given notes data, correct overlays/decorations created
- List buffer shows expected content
- Navigation works (n/p/Enter)
- User input flows to correct RPC calls

### Layer 2: Integration Tests (Real Backend)

Test end-to-end with real backend and isolated database.

**What to verify:**
- Create note → overlay appears
- Delete note → overlay removed
- Edit note → overlay content updates
- Search → results match created notes
- Backlinks → shows linking notes

### Layer 3: Display State Snapshot (Optional)

Capture full display state, compare against expected snapshots.

```
DisplayState = {
  buffer_content: string,
  overlays: [{line, column, text, stale}],
  active_buffer: string,
  message: string
}
```

## Emacs Testing

Uses ERT (Emacs Lisp Regression Testing). Run headless:

```bash
cd ui/emacs && emacs --batch -L . -L tests -l ert -l hemis.el -l hemis-test.el \
  -f ert-run-tests-batch-and-exit
```

### Display State Capture

```elisp
(defun hemis-test--capture-overlay-state ()
  "Capture the display state of all hemis overlays in current buffer.
Returns list of plists with :line :before-string :face :count :texts."
  (let (result)
    (dolist (ov hemis--overlays)
      (when (overlay-get ov 'hemis-note-marker)
        (push (list :line (line-number-at-pos (overlay-start ov))
                    :before-string (overlay-get ov 'before-string)
                    :face (get-text-property 0 'face (or (overlay-get ov 'before-string) ""))
                    :count (overlay-get ov 'hemis-note-count)
                    :texts (overlay-get ov 'hemis-note-texts))
              result)))
    (nreverse result)))
```

### Key Test Patterns

```elisp
;; Verify overlay content (uses before-string, not after-string)
(ert-deftest hemis-note-overlay-shows-text ()
  (with-temp-buffer
    (insert "fn main() {}\n")
    (set-visited-file-name "/tmp/test.rs" t t)
    (setq comment-start "// ")
    (hemis--apply-notes '(((id . "1") (file . "/tmp/test.rs") (line . 1) (column . 0)
                           (text . "Test note"))))
    (let* ((state (hemis-test--capture-overlay-state))
           (ov-state (car state))
           (before-str (plist-get ov-state :before-string)))
      (should (string-match-p "Test note" before-str)))))

;; Verify face color
(ert-deftest hemis-overlay-has-steel-blue-face ()
  (with-temp-buffer
    (insert "fn main() {}\n")
    (set-visited-file-name "/tmp/face.rs" t t)
    (hemis--apply-notes '(((id . "1") (file . "/tmp/face.rs") (line . 1) (column . 0)
                           (text . "Blue note"))))
    (let* ((state (hemis-test--capture-overlay-state))
           (face (plist-get (car state) :face)))
      (should (equal (plist-get face :foreground) "SteelBlue")))))
```

## Neovim Testing

Uses headless mode with custom test runner.

```bash
cd ui/neovim && nvim --headless -u NONE -c "set rtp+=." -c "lua require('tests.run')"
```

### Test Infrastructure

```lua
-- tests/display_spec.lua
local helpers = require("tests.helpers")

describe("hemis display", function()
  before_each(function()
    helpers.setup_test_buffer()
  end)

  after_each(function()
    helpers.cleanup()
  end)

  it("shows note as virtual text", function()
    local display = require("hemis.display")
    local notes = {{id = "1", line = 1, text = "Test note"}}

    display.render_notes(0, notes)

    local state = helpers.capture_display_state()
    assert.equals(1, #state.extmarks)
    assert.truthy(string.find(state.extmarks[1].text, "Test note"))
  end)
end)
```

### Display State Capture

```lua
-- tests/helpers.lua
local M = {}

function M.capture_display_state()
  local buf = vim.api.nvim_get_current_buf()
  local ns = vim.api.nvim_create_namespace("hemis")
  local marks = vim.api.nvim_buf_get_extmarks(buf, ns, 0, -1, {details = true})

  local extmarks = {}
  for _, mark in ipairs(marks) do
    table.insert(extmarks, {
      line = mark[2] + 1,  -- 1-indexed
      text = mark[4].virt_text and mark[4].virt_text[1][1] or "",
    })
  end

  return {
    buffer = table.concat(vim.api.nvim_buf_get_lines(buf, 0, -1, false), "\n"),
    extmarks = extmarks,
    cursor = vim.api.nvim_win_get_cursor(0),
  }
end

function M.setup_test_buffer()
  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_set_current_buf(buf)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, {"fn main() {", "}"})
end

return M
```

### Key Test Patterns

```lua
-- Verify extmark content
it("renders note with correct text", function()
  local display = require("hemis.display")
  display.render_notes(0, {{id = "1", line = 1, text = "Important"}})

  local state = helpers.capture_display_state()
  assert.truthy(string.find(state.extmarks[1].text, "Important"))
end)

-- Verify list buffer
it("list_notes shows notes in buffer", function()
  local commands = require("hemis.commands")
  commands.buffer_notes = {{id = "1", line = 1, text = "Listed note"}}
  commands.list_notes()

  local buf_name = vim.api.nvim_buf_get_name(0)
  assert.truthy(string.find(buf_name, "Hemis Notes"))

  local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
  local found = false
  for _, line in ipairs(lines) do
    if string.find(line, "Listed note") then found = true end
  end
  assert.truthy(found)
end)
```

## VS Code Testing

Uses @vscode/test-electron with Mocha.

```bash
npm test  # Runs in headless VS Code instance
```

### Test Infrastructure

```typescript
// src/test/suite/display.test.ts
import * as assert from 'assert';
import * as vscode from 'vscode';
import { refreshNotes } from '../../decorations';
import { Note } from '../../notes';

suite('Hemis Display', () => {
  let editor: vscode.TextEditor;

  setup(async () => {
    const doc = await vscode.workspace.openTextDocument({
      content: 'fn main() {}\n',
      language: 'rust'
    });
    editor = await vscode.window.showTextDocument(doc);
  });

  test('renders note decoration', async () => {
    // Note: VS Code doesn't expose decoration content directly
    // We test that the decoration provider is called correctly
    const state = captureDisplayState(editor);
    // Verify decoration ranges are set
  });
});
```

### Display State Capture

```typescript
// src/test/helpers.ts
export interface DisplayState {
  content: string;
  decorationRanges: { line: number; character: number }[];
  selection: { line: number; character: number };
}

export function captureDisplayState(editor: vscode.TextEditor): DisplayState {
  return {
    content: editor.document.getText(),
    decorationRanges: [], // Would need decoration provider access
    selection: {
      line: editor.selection.active.line,
      character: editor.selection.active.character,
    },
  };
}
```

### VS Code Limitations

VS Code doesn't expose decoration content in tests. Workarounds:

1. **Test decoration provider logic** - Verify `DecorationProvider.provideDecorations()` returns correct ranges
2. **Test command effects** - Verify commands modify state correctly
3. **Integration via TreeView** - Test `TreeDataProvider.getChildren()` returns expected items

```typescript
test('notes tree shows created notes', async () => {
  const provider = new NotesTreeDataProvider();
  // Mock backend to return test note
  const items = await provider.getChildren();
  assert.strictEqual(items.length, 1);
  assert.strictEqual(items[0].label, 'Test note');
});
```

## Running Tests

### All UIs

```bash
# From project root
make test-ui

# Or individually:
make test-emacs
make test-neovim
make test-vscode
```

### Makefile Targets

```makefile
test-emacs:
	cd ui/emacs && emacs --batch -L . -L tests -l ert -l hemis.el -l hemis-test.el \
		-f ert-run-tests-batch-and-exit

test-neovim:
	cd ui/neovim && nvim --headless -u NONE -c "set rtp+=." -c "lua require('tests.run')"

test-vscode:
	cd ui/vscode && npm test
```

## Test Coverage Goals

| Feature | Emacs | Neovim | VS Code |
|---------|-------|--------|---------|
| Note overlay at correct line | [x] | [x] | [x] |
| Note overlay shows text | [x] | [x] | [x] |
| Comment prefix per language | [x] | [x] | [x] |
| Stale indicator displayed | [x] | [x] | [x] |
| Multiline note formatting | [x] | [x] | [x] |
| Multiple notes same line | [x] | [x] | [ ] |
| List buffer shows notes | [x] | [x] | [ ] |
| List navigation (n/p) | [x] | [ ] | [ ] |
| List visit (RET) | [x] | [ ] | [ ] |
| Status command | [ ] | [x] | [ ] |
| Help content | [ ] | [x] | [ ] |

## Best Practices

1. **Isolate tests** - Each test uses isolated temp files and database
2. **Mock sparingly** - Prefer real backend for integration tests
3. **Verify user-visible state** - Test what user sees, not internals
4. **Fast feedback** - Unit tests with mocks run in <1s
5. **Clear failures** - Test names describe expected behavior
