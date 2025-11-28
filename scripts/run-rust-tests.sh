#!/usr/bin/env bash
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$root"
cargo test
if command -v emacs >/dev/null 2>&1; then
  echo "Skipping Emacs ERT hook (backend selection still in progress)."
fi
