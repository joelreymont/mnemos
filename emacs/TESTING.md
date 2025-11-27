# Emacs tests

Run ERT in batch from repo root:

```bash
emacs -Q --batch \
  -L emacs \
  -l hemis.el \
  -L emacs/tests \
  -l hemis-test.el \
  -f ert-run-tests-batch-and-exit
```

Notes:
- Rust/Tree-sitter tests skip automatically if `treesit` or `rust-ts-mode` are unavailable.
- Tests mock the backend JSON-RPC calls; the Lisp backend does not need to run.
