#!/usr/bin/env bash
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$root"
cargo test
if command -v emacs >/dev/null 2>&1; then
  echo "Running Emacs ERT against Rust backend (hemis-backend must be set in tests)."
  HEMIS_BACKEND="$root/target/debug/backend" emacs -Q --batch -L "$root/emacs" -l emacs/tests/hemis-test.el -f ert-run-tests-batch-and-exit
fi
