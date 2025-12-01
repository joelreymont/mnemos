# Build and Verify Backend

Build the Rust backend and verify it works correctly.

## Instructions

1. **Build in debug mode** (faster compilation):
   ```bash
   cd /Users/joel/Work/hemis && cargo build 2>&1
   ```

2. **Check for warnings** that might indicate issues:
   - Unused variables/imports (usually fine)
   - Deprecated APIs (should fix)
   - Unsafe code warnings (important)

3. **Verify the binary works**:
   ```bash
   ./target/debug/hemis --version 2>&1
   ```

4. **Run backend tests**:
   ```bash
   cargo test 2>&1
   ```

5. **Report results**:
   - Build: success/failed
   - Warnings: count and severity
   - Tests: X passed, Y failed
   - Binary version: output

## For Release Builds
Use `cargo build --release` for optimized builds. These take longer but run faster.

## Common Issues
- **Lock file conflict**: Another hemis process is running. Kill it with `pkill -f 'hemis --serve'`
- **Database locked**: Remove `~/.hemis/hemis.db-lock` or the test's temp directory
- **Socket stale**: Remove `~/.hemis/hemis.sock` and `~/.hemis/hemis.lock`
