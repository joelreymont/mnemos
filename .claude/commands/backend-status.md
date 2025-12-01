# Check Backend Status

Quick diagnostic of the Hemis backend server.

## Instructions

Run these checks in sequence:

### 1. Process Status
```bash
echo "=== Hemis Processes ==="
pgrep -fl 'hemis' || echo "No hemis processes running"
```

### 2. Socket and Lock Files
```bash
echo -e "\n=== Socket/Lock Status ==="
ls -la ~/.hemis/hemis.sock ~/.hemis/hemis.lock 2>&1
```

### 3. Lock File Contents
```bash
echo -e "\n=== Lock File PID ==="
cat ~/.hemis/hemis.lock 2>/dev/null || echo "No lock file"
```

### 4. Recent Logs
```bash
echo -e "\n=== Recent Log (last 20 lines) ==="
tail -20 ~/.hemis/hemis.log 2>/dev/null || echo "No log file"
```

### 5. Database Status
```bash
echo -e "\n=== Database ==="
ls -lh ~/.hemis/hemis.db 2>/dev/null || echo "No database"
```

## Quick Actions

### Start Backend
```bash
./target/debug/hemis --serve &
```

### Stop Backend
```bash
pkill -f 'hemis --serve'
```

### Clean Restart
```bash
pkill -f 'hemis --serve'
rm -f ~/.hemis/hemis.sock ~/.hemis/hemis.lock
./target/debug/hemis --serve &
```

### View Live Logs
```bash
tail -f ~/.hemis/hemis.log
```

## Health Check Output Format

Report status as:
| Component | Status |
|-----------|--------|
| Process | running/stopped |
| Socket | present/missing |
| Lock | valid/stale/missing |
| Database | X MB / missing |
