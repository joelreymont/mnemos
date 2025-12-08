# Emacs tests

Run ERT in batch from repo root:

```bash
HEMIS_BACKEND=/path/to/target/debug/hemis \
emacs -Q --batch \
  -L ui/emacs \
  -l hemis.el \
  -L ui/emacs/tests \
  -l hemis-test.el \
  -f ert-run-tests-batch-and-exit
```

Notes:
- Unit tests mock the backend JSON-RPC calls; integration tests use the real backend.
