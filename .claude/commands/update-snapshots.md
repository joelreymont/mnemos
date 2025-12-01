# Update Test Snapshots

Update Neovim snapshot tests when display output intentionally changes.

## Instructions

### Step 1: Understand What Changed
Before updating snapshots, verify the change is intentional:
- Read the test that uses the snapshot
- Understand what display output changed and why
- Confirm this is desired behavior, not a regression

### Step 2: Update Neovim Snapshots
```bash
cd /Users/joel/Work/hemis/ui/neovim
HEMIS_UPDATE_SNAPSHOTS=1 HEMIS_TEST_BACKEND=/Users/joel/Work/hemis/target/debug/hemis \
  nvim --headless -u tests/minimal_init.lua -c "luafile tests/run.lua" 2>&1
```

### Step 3: Review Changes
```bash
git diff ui/neovim/tests/snapshots/
```

Verify each changed snapshot:
- Does the new output match expected behavior?
- Are highlight groups correct?
- Is formatting/indentation preserved?

### Step 4: Run Tests Without Update Flag
```bash
cd /Users/joel/Work/hemis/ui/neovim
HEMIS_TEST_BACKEND=/Users/joel/Work/hemis/target/debug/hemis \
  nvim --headless -u tests/minimal_init.lua -c "luafile tests/run.lua" 2>&1
```

All tests should pass with the new snapshots.

## Snapshot Locations
```
ui/neovim/tests/snapshots/
├── single_note_display.snap
├── multiple_notes_display.snap
├── stale_note_indicator.snap
└── multiline_note_text.snap
```

## When to Update vs Fix

**Update snapshots when:**
- Intentionally changing display format
- Adding new information to display
- Changing highlight groups by design

**Fix code instead when:**
- Test failure reveals a bug
- Output changed unintentionally
- Regression from refactoring
