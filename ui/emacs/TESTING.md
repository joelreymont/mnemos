# Emacs tests

Run ERT in batch from repo root:

```bash
MNEMOS_BACKEND=/path/to/zig-out/bin/mnemos \
emacs -Q --batch \
  -L ui/emacs \
  -l mnemos.el \
  -L ui/emacs/tests \
  -l mnemos-test.el \
  -f ert-run-tests-batch-and-exit
```

Notes:
- Unit tests mock the backend JSON-RPC calls; integration tests use the real backend.
