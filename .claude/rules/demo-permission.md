# Demo Permission Rule

**Only run demo automation when the user explicitly instructs you to run it.**

The demo driver (`hemis-demo`) controls keyboard and mouse. Running it without permission:
- Interrupts the user's work
- Takes control of their screen
- Can cause data loss if the user is in the middle of something

## Required

Before running ANY demo command (`swift run hemis-demo`, etc.):
1. If the user explicitly told you to run the demo (e.g., “run the emacs demo”), treat that as permission for one invocation.
2. Otherwise, STOP and ASK: "May I run the demo?"
3. Do NOT assume permission from vague statements; only explicit run requests count.

## Permission is ONE-TIME ONLY

**Each explicit run request grants exactly ONE demo invocation.**

After running a demo (whether it succeeds or fails), you MUST ask again before running another unless the user issues a new explicit run instruction.

## This applies to

- `swift run hemis-demo`
- Any command in the `hemis-demo` directory that launches the demo
- Any MCP tool that triggers demo automation

**NO EXCEPTIONS. ALWAYS ASK FIRST. ASK AGAIN AFTER EACH RUN.**
