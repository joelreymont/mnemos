#!/usr/bin/env bash
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$root"
cargo test
if command -v emacs >/dev/null 2>&1; then
  backend="$root/target/debug/hemis"
  if [ ! -x "$backend" ]; then
    echo "Skipping Emacs ERT: hemis binary not found at $backend"
    exit 1
  fi
  echo "Running Emacs ERT against Rust backend..."
  HEMIS_BACKEND="$backend" emacs -Q --batch \
    -L "$root/ui/emacs" \
    -l hemis.el \
    -L "$root/ui/emacs/tests" \
    -l hemis-test.el \
    -f ert-run-tests-batch-and-exit
else
  echo "Skipping Emacs ERT hook: emacs binary not found."
fi
