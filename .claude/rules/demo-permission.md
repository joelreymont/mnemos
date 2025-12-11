# Demo Permission Rule

**NEVER run the demo automation without EXPLICIT user permission.**

The demo driver (`hemis-demo`) controls keyboard and mouse. Running it without permission:
- Interrupts the user's work
- Takes control of their screen
- Can cause data loss if the user is in the middle of something

## Required

Before running ANY demo command (`swift run hemis-demo`, etc.):
1. STOP and ASK: "May I run the demo?"
2. WAIT for explicit "yes" or approval
3. Do NOT assume permission from phrases like "run demo" - the user may be telling you about running it themselves

## Permission is ONE-TIME ONLY

**Each permission grants exactly ONE demo invocation.**

After running a demo (whether it succeeds or fails), you MUST ask for permission again before running another. Previous permission does NOT carry over.

## This applies to

- `swift run hemis-demo`
- Any command in the `hemis-demo` directory that launches the demo
- Any MCP tool that triggers demo automation

**NO EXCEPTIONS. ALWAYS ASK FIRST. ASK AGAIN AFTER EACH RUN.**
