# Mnemos

Zig rewrite of mnemos backend. Rust version on `rust` branch.

## Build

```bash
zig build        # compile
zig build run    # run
zig build test   # test
```

## Key References

- **[Zig 0.15 I/O API](docs/zig-0.15-io-api.md)** - stdout, writers, ArrayList changes
- **[Plan](~/.claude/plans/curried-rolling-origami.md)** - Full rewrite plan
- Rust reference: `rust` branch

## Architecture

```
std.Io Event Loop
├── Unix socket ──► RPC from editors
├── AI stdout   ──► Streaming responses
├── AI stdin    ◄── Send prompts
├── File watch  ──► Change notifications
└── Notes store ──► Markdown files
```

## Key Files

| File | Purpose |
|------|---------|
| `src/main.zig` | Entry point, event loop |
| `src/server.zig` | Socket server |
| `src/rpc.zig` | JSON-RPC dispatch |
| `src/ai.zig` | AI subprocess pipes |
| `src/storage.zig` | Markdown note storage |
| `src/treesitter.zig` | Dynamic grammar loading |
| `src/git.zig` | libgit2 bindings |

## RPC Protocol

`notes/` create,get,update,delete,list-for-file,list-by-node,list-project,search,backlinks,reattach,buffer-update
`index/` add-file,search (legacy)
`mnemos/` search,status,index-project,list-files,get-file,explain-region,project-meta,open-project,save-snapshot,load-snapshot,shutdown

## Conventions

- Staleness: `nodeTextHash`=SHA256, search +/-20 lines
- AI: Persistent subprocess (claude/codex), newline-delimited JSON
- Tree-sitter: Dynamic grammars only, `mnemos grammar build`
- Target: Zig 0.16 / nightly for std.Io async

## Editor Plugins

Located in `ui/`:
- `ui/neovim/` - Lua plugin
- `ui/emacs/` - Elisp package
- `ui/vscode/` - TypeScript extension

## Demo Driver

`cd ../mnemos-demo && swift run mnemos-demo <script> --show-labels`
See `.claude/rules/demo-permission.md` for permission requirements.
