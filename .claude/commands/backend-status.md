# Check Backend Status

Quick diagnostic of the Mnemos backend server.

## Instructions

Run these checks in sequence:

### 1. Process Status
```bash
echo "=== Mnemos Processes ==="
pgrep -fl 'mnemos' || echo "No mnemos processes running"
```

### 2. Socket and Lock Files
```bash
echo -e "\n=== Socket/Lock Status ==="
ls -la ~/.mnemos/mnemos.sock ~/.mnemos/mnemos.lock 2>&1
```

### 3. Lock File Contents
```bash
echo -e "\n=== Lock File PID ==="
cat ~/.mnemos/mnemos.lock 2>/dev/null || echo "No lock file"
```

### 4. Recent Logs
```bash
echo -e "\n=== Recent Log (last 20 lines) ==="
tail -20 ~/.mnemos/mnemos.log 2>/dev/null || echo "No log file"
```

### 5. Database Status
```bash
echo -e "\n=== Database ==="
ls -lh ~/.mnemos/mnemos.db 2>/dev/null || echo "No database"
```

## Quick Actions

### Start Backend
```bash
./target/debug/mnemos --serve &
```

### Stop Backend
```bash
pkill -f 'mnemos --serve'
```

### Clean Restart
```bash
pkill -f 'mnemos --serve'
rm -f ~/.mnemos/mnemos.sock ~/.mnemos/mnemos.lock
./target/debug/mnemos --serve &
```

### View Live Logs
```bash
tail -f ~/.mnemos/mnemos.log
```

## Health Check Output Format

Report status as:
| Component | Status |
|-----------|--------|
| Process | running/stopped |
| Socket | present/missing |
| Lock | valid/stale/missing |
| Database | X MB / missing |
