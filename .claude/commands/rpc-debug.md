# Debug RPC/Socket Issues

Diagnose and fix connection issues between editors and the Mnemos backend.

## Instructions

### Step 1: Check Backend Status
```bash
# Is the socket present?
ls -la ~/.mnemos/mnemos.sock ~/.mnemos/mnemos.lock 2>&1

# Is a mnemos process running?
pgrep -fl 'mnemos.*--serve'

# Check lock file contents
cat ~/.mnemos/mnemos.lock 2>/dev/null || echo "No lock file"
```

### Step 2: Check for Stale State
If socket exists but connection fails:
```bash
# Read PID from lock file
PID=$(cat ~/.mnemos/mnemos.lock 2>/dev/null)
if [ -n "$PID" ]; then
  # Check if process is alive
  kill -0 $PID 2>/dev/null && echo "Process $PID is alive" || echo "Process $PID is dead (stale)"
fi
```

### Step 3: Clean Up Stale State
```bash
pkill -f 'mnemos --serve'
rm -f ~/.mnemos/mnemos.sock ~/.mnemos/mnemos.lock
```

### Step 4: Check Backend Logs
```bash
tail -50 ~/.mnemos/mnemos.log
```

### Step 5: Test Fresh Connection
```bash
# Start backend manually and watch output
./target/debug/mnemos --serve
```

## Common Issues

### "Connection refused"
- Socket file exists but no process listening
- Fix: Remove stale socket and lock, restart backend

### "Socket closed" immediately
- Backend crashed during startup
- Check: `tail ~/.mnemos/mnemos.log` for errors

### Environment variables not passed
- **Emacs**: Use `make-process` with `process-environment`, not `call-process-shell-command`
- **Neovim**: Set env vars before shell command: `MNEMOS_DIR=x MNEMOS_DB_PATH=y ./mnemos --serve`

### Test isolation failures
- Tests polluting each other's state
- Fix: Use temp directories with custom `MNEMOS_DIR` and `MNEMOS_DB_PATH`
- Always call `rpc.stop()` (Neovim) or `mnemos-shutdown` (Emacs) in test cleanup

## Key Files
```
~/.mnemos/mnemos.sock   # Unix socket for RPC
~/.mnemos/mnemos.lock   # Lock file with server PID
~/.mnemos/mnemos.log    # Backend log output
~/.mnemos/mnemos.db     # SQLite database
```
