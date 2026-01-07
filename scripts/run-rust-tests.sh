#!/usr/bin/env bash
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$root"
cargo test
if command -v emacs >/dev/null 2>&1; then
  backend="$root/target/debug/mnemos"
  if [ ! -x "$backend" ]; then
    echo "Skipping Emacs ERT: mnemos binary not found at $backend"
    exit 1
  fi
  echo "Running Emacs ERT against Rust backend..."
  MNEMOS_BACKEND="$backend" emacs -Q --batch \
    -L "$root/ui/emacs" \
    -l mnemos.el \
    -L "$root/ui/emacs/tests" \
    -l mnemos-test.el \
    -f ert-run-tests-batch-and-exit
else
  echo "Skipping Emacs ERT hook: emacs binary not found."
fi
