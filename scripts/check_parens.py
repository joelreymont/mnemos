#!/usr/bin/env python3
"""
Quick parenthesis balance checker for Lisp/Elisp files.

Usage:
  python3 scripts/check_parens.py path1.el [path2.lisp ...]

It reports the first position where the stack underflows (extra closing),
or a positive balance at EOF (missing closing). Treats (), [], {} identically.
"""

import sys
from pathlib import Path


PAIRS = {"(": ")", "[": "]", "{": "}"}
OPEN = set(PAIRS.keys())
CLOSE = {v: k for k, v in PAIRS.items()}


def check_file(path: Path) -> int:
    text = path.read_text(encoding="utf-8", errors="replace")
    stack = []
    for idx, ch in enumerate(text, 1):
        if ch in OPEN:
            stack.append(ch)
        elif ch in CLOSE:
            if not stack:
                print(f"{path}: extra closing {ch!r} at char {idx}")
                return 1
            top = stack.pop()
            if CLOSE[ch] != top:
                print(
                    f"{path}: mismatched {ch!r} at char {idx}, expected closing for {top!r}"
                )
                return 1
    if stack:
        print(f"{path}: missing {len(stack)} closing bracket(s) for {''.join(stack)}")
        return 1
    print(f"{path}: balanced")
    return 0


def main(argv: list[str]) -> int:
    if len(argv) < 2:
        print(__doc__.strip())
        return 1
    status = 0
    for arg in argv[1:]:
        status |= check_file(Path(arg))
    return status


if __name__ == "__main__":
    sys.exit(main(sys.argv))
