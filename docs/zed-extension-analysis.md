# Zed Editor Extension Analysis

**Date:** 2025-12-15
**Status:** Blocked - waiting for Zed extension API improvements

## Summary

Mnemos UI cannot currently be implemented for the Zed editor due to fundamental limitations in Zed's extension API.

## Zed Extension Capabilities (Current)

**What extensions CAN do:**
- Language support (tree-sitter grammars, LSP)
- Themes and icon themes
- Slash commands
- MCP servers
- Execute processes (`process:exec`)
- Download files

**What extensions CANNOT do:**
- Custom UI panels
- Overlays / inline decorations
- Virtual text (extmarks)
- Gutter markers
- Custom keybindings from extensions

## Technical Blocker

Zed's UI framework (GPUI) is Rust-based with heavy trait/closure usage, making it extremely hard to expose to WASM extensions. VSCode reuses Electron's webview for extension UI; Zed has no equivalent.

From [GPUI2 discussion](https://github.com/zed-industries/zed/discussions/6679):
> "If GPUI uses too many Rust features like traits/closures, it will be hard to bind to for extensions... unless scriptability is considered in the GPUI design, extensions will need a new custom UI framework."

## Mnemos Requirements vs Zed Support

| Feature | Mnemos Needs | Zed Support |
|---------|-------------|-------------|
| Inline note markers (virtual text) | ✓ | ✗ |
| Picker/menu UI | ✓ | ✗ |
| Gutter decorations | ✓ | ✗ |
| Custom keybindings | ✓ | Limited |
| Backend RPC | ✓ | Process exec only |

## Virtual Text Comparison

| Editor | Virtual Text API |
|--------|------------------|
| Neovim | `nvim_buf_set_extmark()` with `virt_text` |
| Emacs | `overlay-put` with `before-string`/`after-string` |
| VSCode | `TextEditorDecorationType` |
| **Zed** | ❌ None exposed to extensions |

## Possible Workaround: LSP-Based Implementation

An LSP-based mnemos could provide limited functionality:

| LSP Feature | Mnemos Use |
|-------------|-----------|
| `textDocument/inlayHint` | Show note markers at positions |
| `textDocument/hover` | Display note content on hover |
| `textDocument/codeAction` | "Add Note", "Edit Note", "Delete Note" |
| Diagnostics (hint severity) | Highlight lines with notes |

**Limitations of LSP approach:**
- No rich picker UI (just code action menu)
- Awkward editing (can't capture text input)
- No backlinks navigation
- No custom commands

## Relevant Issues to Watch

- [Allow extensions to show custom UI](https://github.com/zed-industries/extensions/issues/1288) - labeled "needs infrastructure"
- [Allow Extensions to Create and Manage Customizable Side Panels](https://github.com/zed-industries/zed/issues/21400)
- [GPUI2 and extensions discussion](https://github.com/zed-industries/zed/discussions/6679)

## Conclusion

**Not implementable** with current Zed extension API. Monitor the above issues for when Zed adds:
1. Decoration/virtual-text API for extensions
2. Custom UI panel support
3. Richer keybinding support

An LSP-based approach could provide read-mostly functionality but would be a degraded experience compared to Neovim/Emacs/VSCode implementations.
