#!/usr/bin/env bash
set -euo pipefail

root="$(cd "$(dirname "$0")/.." && pwd)"

cd "$root"

echo "Running backend note persistence test..."
HEMIS_SKIP_SERVER=1 sbcl --script hemis-project/backend/test-notes.lisp

echo "Running Emacs UI tests..."
emacs -Q --batch -L emacs -l emacs/tests/hemis-test.el -f ert-run-tests-batch-and-exit

echo "All tests passed."
