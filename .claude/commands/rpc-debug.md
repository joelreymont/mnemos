# Debug RPC/Socket Issues

Diagnose and fix connection issues between editors and the Hemis backend.

## Instructions

### Step 1: Check Backend Status
```bash
# Is the socket present?
ls -la ~/.hemis/hemis.sock ~/.hemis/hemis.lock 2>&1

# Is a hemis process running?
pgrep -fl 'hemis.*--serve'

# Check lock file contents
cat ~/.hemis/hemis.lock 2>/dev/null || echo "No lock file"
```

### Step 2: Check for Stale State
If socket exists but connection fails:
```bash
# Read PID from lock file
PID=$(cat ~/.hemis/hemis.lock 2>/dev/null)
if [ -n "$PID" ]; then
  # Check if process is alive
  kill -0 $PID 2>/dev/null && echo "Process $PID is alive" || echo "Process $PID is dead (stale)"
fi
```

### Step 3: Clean Up Stale State
```bash
pkill -f 'hemis --serve'
rm -f ~/.hemis/hemis.sock ~/.hemis/hemis.lock
```

### Step 4: Check Backend Logs
```bash
tail -50 ~/.hemis/hemis.log
```

### Step 5: Test Fresh Connection
```bash
# Start backend manually and watch output
./target/debug/hemis --serve
```

## Common Issues

### "Connection refused"
- Socket file exists but no process listening
- Fix: Remove stale socket and lock, restart backend

### "Socket closed" immediately
- Backend crashed during startup
- Check: `tail ~/.hemis/hemis.log` for errors

### Environment variables not passed
- **Emacs**: Use `make-process` with `process-environment`, not `call-process-shell-command`
- **Neovim**: Set env vars before shell command: `HEMIS_DIR=x HEMIS_DB_PATH=y ./hemis --serve`

### Test isolation failures
- Tests polluting each other's state
- Fix: Use temp directories with custom `HEMIS_DIR` and `HEMIS_DB_PATH`
- Always call `rpc.stop()` (Neovim) or `hemis-shutdown` (Emacs) in test cleanup

## Key Files
```
~/.hemis/hemis.sock   # Unix socket for RPC
~/.hemis/hemis.lock   # Lock file with server PID
~/.hemis/hemis.log    # Backend log output
~/.hemis/hemis.db     # SQLite database
```
