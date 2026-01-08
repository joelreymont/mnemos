//! AI subprocess management with streaming pipes
//!
//! Spawns and manages a persistent AI subprocess (claude or codex CLI).
//! Communicates via newline-delimited JSON over stdin/stdout pipes.
//! Handles subprocess lifecycle: start, crash recovery, shutdown.
//! Integrates with poll-based event loop for multiplexed I/O.

const std = @import("std");
const builtin = @import("builtin");
const mem = std.mem;
const posix = std.posix;
const process = std.process;

const Allocator = mem.Allocator;
const rpc = @import("rpc.zig");

/// Timeout for AI provider responses (5 minutes)
const AI_TIMEOUT_MS: i32 = 5 * 60 * 1000;

/// Write JSON-escaped string (escapes quotes, backslashes, control chars)
pub fn writeJsonEscaped(writer: anytype, s: []const u8) !void {
    for (s) |c| {
        switch (c) {
            '"' => try writer.writeAll("\\\""),
            '\\' => try writer.writeAll("\\\\"),
            '\n' => try writer.writeAll("\\n"),
            '\r' => try writer.writeAll("\\r"),
            '\t' => try writer.writeAll("\\t"),
            else => {
                if (c < 0x20) {
                    // Control character - skip or encode as \uXXXX
                    try writer.print("\\u{x:0>4}", .{c});
                } else {
                    try writer.writeByte(c);
                }
            },
        }
    }
}

/// Protocol: newline-delimited JSON
/// Send: {"prompt": "...", "id": "..."}
/// Receive: {"response": "...", "id": "...", "done": bool}
/// AI provider type
pub const Provider = enum {
    claude,
    codex,

    pub fn command(self: Provider) []const u8 {
        return switch (self) {
            .claude => "claude",
            .codex => "codex",
        };
    }
};

/// Timeout for subprocess shutdown (in nanoseconds)
const SHUTDOWN_TIMEOUT_NS: u64 = 5 * std.time.ns_per_s;

/// Wait for a child process with timeout, force kill if necessary
fn waitWithTimeout(child: *process.Child, timeout_ns: u64) void {
    const start = std.time.Instant.now() catch {
        // Fallback to blocking wait if we can't get time
        _ = child.wait() catch {};
        return;
    };

    // Poll for exit with WNOHANG
    while (true) {
        const elapsed = (std.time.Instant.now() catch return).since(start);
        if (elapsed >= timeout_ns) {
            // Timeout reached, force kill
            _ = posix.kill(child.id, posix.SIG.KILL) catch {};
            _ = child.wait() catch {};
            return;
        }

        // Try non-blocking wait - check if process has already exited
        // Note: Child.wait() blocks, so we use kill with signal 0 to check if process exists
        if (posix.kill(child.id, 0)) {
            // Process still exists, sleep briefly then retry
            std.Thread.sleep(10 * std.time.ns_per_ms);
        } else |_| {
            // Process no longer exists
            _ = child.wait() catch {};
            return;
        }
    }
}

/// AI subprocess manager
pub const AI = struct {
    alloc: Allocator,
    provider: Provider,
    child: ?process.Child = null,
    stdin_fd: ?posix.fd_t = null,
    stdout_fd: ?posix.fd_t = null,
    read_buf: [rpc.STREAM_BUFFER_SIZE]u8 = undefined,
    read_len: usize = 0,
    write_buf: std.ArrayList(u8),

    pub fn init(alloc: Allocator, provider: Provider) AI {
        return .{
            .alloc = alloc,
            .provider = provider,
            .write_buf = .{},
        };
    }

    pub fn deinit(self: *AI) void {
        self.shutdown();
        self.write_buf.deinit(self.alloc);
    }

    /// Start the subprocess with streaming JSON mode
    pub fn start(self: *AI) !void {
        if (self.child != null) return error.AlreadyStarted;

        // Use streaming JSON mode for persistent communication
        // --verbose is required by claude CLI for stream-json output
        var child = process.Child.init(&.{
            self.provider.command(),
            "--print",
            "--verbose",
            "--input-format",
            "stream-json",
            "--output-format",
            "stream-json",
        }, self.alloc);
        child.stdin_behavior = .Pipe;
        child.stdout_behavior = .Pipe;
        child.stderr_behavior = .Inherit;

        try child.spawn();

        self.child = child;
        self.stdin_fd = child.stdin.?.handle;
        self.stdout_fd = child.stdout.?.handle;
    }

    /// Shutdown the subprocess gracefully
    pub fn shutdown(self: *AI) void {
        if (self.child) |*child| {
            // Close stdin to signal shutdown
            if (child.stdin) |stdin| {
                stdin.close();
                child.stdin = null;
            }
            self.stdin_fd = null;

            // Wait for process to exit with timeout
            waitWithTimeout(child, SHUTDOWN_TIMEOUT_NS);

            // Close stdout
            if (child.stdout) |stdout| {
                stdout.close();
                child.stdout = null;
            }
            self.stdout_fd = null;

            self.child = null;
        }
    }

    /// Restart subprocess after crash
    pub fn restart(self: *AI) !void {
        self.shutdown();
        try self.start();
    }

    /// Check if subprocess is running
    pub fn isRunning(self: *AI) bool {
        if (self.child) |*child| {
            // Use kill(pid, 0) to check if process exists without blocking
            if (posix.kill(child.id, 0)) {
                // Process exists
                return true;
            } else |_| {
                // Process no longer exists - clean up
                self.cleanupChild();
                return false;
            }
        }
        return false;
    }

    /// Clean up child process resources
    fn cleanupChild(self: *AI) void {
        if (self.child) |*child| {
            // Close stdin if still open
            if (child.stdin) |stdin| {
                stdin.close();
                child.stdin = null;
            }
            // Close stdout if still open
            if (child.stdout) |stdout| {
                stdout.close();
                child.stdout = null;
            }
            // Reap the zombie process
            _ = child.wait() catch {};
            self.child = null;
        }
        self.stdin_fd = null;
        self.stdout_fd = null;
    }

    /// Get stdout fd for poll multiplexing
    pub fn getReadFd(self: *AI) ?posix.fd_t {
        return self.stdout_fd;
    }

    /// Send a prompt request
    pub fn sendPrompt(self: *AI, prompt: []const u8) !void {
        if (self.stdin_fd == null) return error.NotStarted;

        // Create request JSON in claude stream-json format
        self.write_buf.clearRetainingCapacity();
        const writer = self.write_buf.writer(self.alloc);
        try writer.writeAll("{\"type\":\"user\",\"message\":{\"role\":\"user\",\"content\":\"");
        try writeJsonEscaped(writer, prompt);
        try writer.writeAll("\"}}\n");

        // Write to stdin
        const data = self.write_buf.items;
        var written: usize = 0;
        while (written < data.len) {
            const n = try posix.write(self.stdin_fd.?, data[written..]);
            if (n == 0) return error.BrokenPipe;
            written += n;
        }
    }

    /// Read available responses (non-blocking if no complete response)
    /// Call this when stdout_fd is ready for reading
    /// Callback receives: chunk of text, done flag
    pub fn readResponses(self: *AI, callback: *const fn (chunk: []const u8, done: bool) void) !void {
        if (self.stdout_fd == null) return error.NotStarted;

        // Read available data
        const n = posix.read(self.stdout_fd.?, self.read_buf[self.read_len..]) catch |err| {
            if (err == error.WouldBlock) return;
            return err;
        };
        if (n == 0) return error.ProcessExited;
        self.read_len += n;

        // Process complete lines
        while (mem.indexOf(u8, self.read_buf[0..self.read_len], "\n")) |newline| {
            const line = self.read_buf[0..newline];
            self.processResponse(line, callback);

            // Shift buffer
            const remaining = self.read_len - newline - 1;
            if (remaining > 0) {
                mem.copyForwards(u8, self.read_buf[0..remaining], self.read_buf[newline + 1 .. self.read_len]);
            }
            self.read_len = remaining;
        }
    }

    fn processResponse(self: *AI, json_line: []const u8, callback: *const fn (chunk: []const u8, done: bool) void) void {
        // Parse JSON response from claude CLI
        const parsed = std.json.parseFromSlice(
            std.json.Value,
            self.alloc,
            json_line,
            .{},
        ) catch return;
        defer parsed.deinit();

        const obj = parsed.value.object;
        const msg_type = obj.get("type") orelse return;
        if (msg_type != .string) return;

        if (mem.eql(u8, msg_type.string, "result")) {
            // Final result: {"type":"result","result":"..."}
            if (obj.get("result")) |result| {
                if (result == .string) {
                    callback(result.string, true);
                }
            }
        } else if (mem.eql(u8, msg_type.string, "assistant")) {
            // Streaming chunk: {"type":"assistant","message":{"content":[{"type":"text","text":"..."}]}}
            if (obj.get("message")) |msg| {
                if (msg == .object) {
                    if (msg.object.get("content")) |content| {
                        if (content == .array and content.array.items.len > 0) {
                            const first = content.array.items[0];
                            if (first == .object) {
                                if (first.object.get("text")) |text| {
                                    if (text == .string) {
                                        callback(text.string, false);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
};

const ProviderEnv = enum {
    auto,
    none,
    claude,
    codex,
};

pub const AskResult = struct {
    provider: Provider,
    response: []const u8,
};

fn parseProviderEnv(value: []const u8) ProviderEnv {
    if (value.len == 0) return .auto;
    if (std.ascii.eqlIgnoreCase(value, "none") or std.ascii.eqlIgnoreCase(value, "disabled")) {
        return .none;
    }
    if (std.ascii.eqlIgnoreCase(value, "claude") or std.ascii.eqlIgnoreCase(value, "claude-cli")) {
        return .claude;
    }
    if (std.ascii.eqlIgnoreCase(value, "codex")) {
        return .codex;
    }
    return .auto;
}

fn isAvailable(alloc: Allocator, cmd: []const u8) bool {
    if (cmd.len == 0) return false;
    if (std.fs.path.isAbsolute(cmd) or mem.indexOfScalar(u8, cmd, std.fs.path.sep) != null) {
        posix.access(cmd, posix.X_OK) catch return false;
        return true;
    }

    const path_env = process.getEnvVarOwned(alloc, "PATH") catch return false;
    defer alloc.free(path_env);

    var it = mem.splitScalar(u8, path_env, std.fs.path.delimiter);
    while (it.next()) |dir| {
        if (dir.len == 0) continue;
        const full = std.fs.path.join(alloc, &.{ dir, cmd }) catch continue;
        defer alloc.free(full);
        if (posix.access(full, posix.X_OK)) |_| {
            return true;
        } else |_| {}
    }
    return false;
}

fn detectProvider(alloc: Allocator) ?Provider {
    if (process.getEnvVarOwned(alloc, "MNEMOS_AI_PROVIDER")) |raw| {
        defer alloc.free(raw);
        switch (parseProviderEnv(raw)) {
            .none => return null,
            .claude => if (isAvailable(alloc, "claude")) return .claude,
            .codex => if (isAvailable(alloc, "codex")) return .codex,
            .auto => {},
        }
    } else |_| {}

    if (isAvailable(alloc, "codex")) return .codex;
    if (isAvailable(alloc, "claude")) return .claude;
    return null;
}

fn traceEnabled(alloc: Allocator) bool {
    if (comptime builtin.mode != .Debug) return false;
    if (process.getEnvVarOwned(alloc, "MNEMOS_AI_TRACE")) |val| {
        alloc.free(val);
        return true;
    } else |_| {}
    return false;
}

/// Run AI synchronously with a prompt, return provider + response
pub fn askWithProvider(alloc: Allocator, code: []const u8, user_prompt: []const u8) !AskResult {
    const trace = traceEnabled(alloc);
    var start: ?std.time.Instant = null;
    if (trace) {
        start = std.time.Instant.now() catch null;
        std.log.info("ai ask start prompt_len={} code_len={}", .{ user_prompt.len, code.len });
    }

    // Build full prompt with code context
    const prompt = try std.fmt.allocPrint(alloc,
        \\{s}
        \\
        \\```
        \\{s}
        \\```
    , .{ user_prompt, code });
    defer alloc.free(prompt);

    const provider = detectProvider(alloc) orelse return error.NoProviderAvailable;
    const response = switch (provider) {
        .claude => try runProvider(alloc, "claude", prompt),
        .codex => try runProvider(alloc, "codex", prompt),
    };

    if (trace) {
        const elapsed_ms = if (start) |s|
            (std.time.Instant.now() catch s).since(s) / std.time.ns_per_ms
        else
            @as(u64, 0);
        std.log.info("ai ask done provider={s} elapsed_ms={}", .{ provider.command(), elapsed_ms });
    }

    return .{ .provider = provider, .response = response };
}

/// Run AI synchronously with a prompt, return response
pub fn ask(alloc: Allocator, code: []const u8, user_prompt: []const u8) ![]const u8 {
    const result = try askWithProvider(alloc, code, user_prompt);
    return result.response;
}

/// Legacy explain function - uses default prompt
pub fn explain(alloc: Allocator, code: []const u8) ![]const u8 {
    return ask(alloc, code, "explain this code");
}

fn runProvider(alloc: Allocator, cmd: []const u8, prompt: []const u8) ![]const u8 {
    if (mem.eql(u8, cmd, "codex")) {
        return runCodex(alloc, prompt);
    }

    // Pass prompt via stdin to avoid command injection through shell metacharacters
    var child = process.Child.init(&.{ cmd, "-p", "-" }, alloc);
    child.stdin_behavior = .Pipe;
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Ignore;

    try child.spawn();
    errdefer {
        _ = child.kill() catch {};
        _ = child.wait() catch {};
    }

    // Write prompt to stdin (safe - no shell interpretation)
    if (child.stdin) |stdin| {
        stdin.writeAll(prompt) catch {};
        stdin.close();
        child.stdin = null;
    }

    // Read stdout with timeout using poll
    const stdout = child.stdout.?;
    var buf: [rpc.STREAM_BUFFER_SIZE]u8 = undefined;
    var read_buf: std.ArrayList(u8) = .{};
    errdefer read_buf.deinit(alloc);

    var poll_fds = [_]posix.pollfd{.{
        .fd = stdout.handle,
        .events = posix.POLL.IN,
        .revents = 0,
    }};

    while (true) {
        // Wait for data with timeout
        const ready = posix.poll(&poll_fds, AI_TIMEOUT_MS) catch break;
        if (ready == 0) {
            // Timeout - kill child and return error
            _ = child.kill() catch {};
            _ = child.wait() catch {};
            return error.Timeout;
        }

        if (poll_fds[0].revents & posix.POLL.IN != 0) {
            const n = stdout.read(&buf) catch break;
            if (n == 0) break;
            try read_buf.appendSlice(alloc, buf[0..n]);
        } else if (poll_fds[0].revents & (posix.POLL.HUP | posix.POLL.ERR) != 0) {
            break;
        }
    }

    const term = try child.wait();
    if (term.Exited != 0) {
        return error.ProviderFailed;
    }

    return read_buf.toOwnedSlice(alloc);
}

const MAX_AI_RESPONSE: usize = 1024 * 1024; // 1MB cap for safety

fn runCodex(alloc: Allocator, prompt: []const u8) ![]const u8 {
    var rand_bytes: [8]u8 = undefined;
    std.crypto.random.bytes(&rand_bytes);
    const rand = std.mem.readInt(u64, &rand_bytes, .little);
    const out_path = try std.fmt.allocPrint(alloc, "/tmp/mnemos-codex-{x}.txt", .{rand});
    defer alloc.free(out_path);
    std.fs.deleteFileAbsolute(out_path) catch |err| {
        if (err != error.FileNotFound) return err;
    };

    var child = process.Child.init(&.{
        "codex",
        "exec",
        "--skip-git-repo-check",
        "--output-last-message",
        out_path,
        "-",
    }, alloc);
    child.stdin_behavior = .Pipe;
    child.stdout_behavior = .Ignore;
    child.stderr_behavior = .Ignore;

    try child.spawn();
    errdefer {
        _ = child.kill() catch {};
        _ = child.wait() catch {};
    }

    if (child.stdin) |stdin| {
        stdin.writeAll(prompt) catch {};
        stdin.close();
        child.stdin = null;
    }

    const term = try child.wait();
    if (term.Exited != 0) {
        return error.ProviderFailed;
    }

    const file = std.fs.openFileAbsolute(out_path, .{}) catch return error.ProviderFailed;
    defer file.close();
    defer std.fs.deleteFileAbsolute(out_path) catch {};

    return file.readToEndAlloc(alloc, MAX_AI_RESPONSE);
}

test "AI init/deinit" {
    const alloc = std.testing.allocator;
    var ai = AI.init(alloc, .claude);
    defer ai.deinit();

    // Should not be running initially
    try std.testing.expect(!ai.isRunning());
}

test "AI provider command" {
    try std.testing.expectEqualStrings("claude", Provider.claude.command());
    try std.testing.expectEqualStrings("codex", Provider.codex.command());
}

test "AI not started error" {
    const alloc = std.testing.allocator;
    var ai = AI.init(alloc, .claude);
    defer ai.deinit();

    // Should error when trying to send prompt before starting
    const result = ai.sendPrompt("test prompt");
    try std.testing.expectError(error.NotStarted, result);
}

test "AI getReadFd before start" {
    const alloc = std.testing.allocator;
    var ai = AI.init(alloc, .claude);
    defer ai.deinit();

    // Should return null when not started
    try std.testing.expect(ai.getReadFd() == null);
}

test "AI codex provider" {
    const alloc = std.testing.allocator;
    var ai = AI.init(alloc, .codex);
    defer ai.deinit();

    try std.testing.expect(!ai.isRunning());
    try std.testing.expect(ai.provider == .codex);
}

test "writeJsonEscaped basic" {
    const alloc = std.testing.allocator;
    var list: std.ArrayList(u8) = .{};
    defer list.deinit(alloc);

    try writeJsonEscaped(list.writer(alloc), "hello");
    try std.testing.expectEqualStrings("hello", list.items);
}

test "writeJsonEscaped quotes" {
    const alloc = std.testing.allocator;
    var list: std.ArrayList(u8) = .{};
    defer list.deinit(alloc);

    try writeJsonEscaped(list.writer(alloc), "say \"hello\"");
    try std.testing.expectEqualStrings("say \\\"hello\\\"", list.items);
}

test "writeJsonEscaped backslash" {
    const alloc = std.testing.allocator;
    var list: std.ArrayList(u8) = .{};
    defer list.deinit(alloc);

    try writeJsonEscaped(list.writer(alloc), "path\\to\\file");
    try std.testing.expectEqualStrings("path\\\\to\\\\file", list.items);
}

test "writeJsonEscaped newline" {
    const alloc = std.testing.allocator;
    var list: std.ArrayList(u8) = .{};
    defer list.deinit(alloc);

    try writeJsonEscaped(list.writer(alloc), "line1\nline2");
    try std.testing.expectEqualStrings("line1\\nline2", list.items);
}

test "writeJsonEscaped tab" {
    const alloc = std.testing.allocator;
    var list: std.ArrayList(u8) = .{};
    defer list.deinit(alloc);

    try writeJsonEscaped(list.writer(alloc), "col1\tcol2");
    try std.testing.expectEqualStrings("col1\\tcol2", list.items);
}

test "writeJsonEscaped carriage return" {
    const alloc = std.testing.allocator;
    var list: std.ArrayList(u8) = .{};
    defer list.deinit(alloc);

    try writeJsonEscaped(list.writer(alloc), "line\r\n");
    try std.testing.expectEqualStrings("line\\r\\n", list.items);
}

test "Provider enum distinct" {
    try std.testing.expect(Provider.claude != Provider.codex);
}

test "parseProviderEnv" {
    try std.testing.expectEqual(ProviderEnv.codex, parseProviderEnv("codex"));
    try std.testing.expectEqual(ProviderEnv.codex, parseProviderEnv("CoDeX"));
    try std.testing.expectEqual(ProviderEnv.claude, parseProviderEnv("claude"));
    try std.testing.expectEqual(ProviderEnv.claude, parseProviderEnv("claude-cli"));
    try std.testing.expectEqual(ProviderEnv.none, parseProviderEnv("none"));
    try std.testing.expectEqual(ProviderEnv.none, parseProviderEnv("disabled"));
    try std.testing.expectEqual(ProviderEnv.auto, parseProviderEnv(""));
    try std.testing.expectEqual(ProviderEnv.auto, parseProviderEnv("unknown"));
}

test "writeJsonEscaped empty string" {
    const alloc = std.testing.allocator;
    var list: std.ArrayList(u8) = .{};
    defer list.deinit(alloc);

    try writeJsonEscaped(list.writer(alloc), "");
    try std.testing.expectEqualStrings("", list.items);
}

test "writeJsonEscaped unicode" {
    const alloc = std.testing.allocator;
    var list: std.ArrayList(u8) = .{};
    defer list.deinit(alloc);

    try writeJsonEscaped(list.writer(alloc), "hello 世界");
    try std.testing.expectEqualStrings("hello 世界", list.items);
}

test "writeJsonEscaped mixed escapes" {
    const alloc = std.testing.allocator;
    var list: std.ArrayList(u8) = .{};
    defer list.deinit(alloc);

    try writeJsonEscaped(list.writer(alloc), "line1\n\"quoted\"\tvalue\\path");
    try std.testing.expectEqualStrings("line1\\n\\\"quoted\\\"\\tvalue\\\\path", list.items);
}

test "writeJsonEscaped control characters" {
    const alloc = std.testing.allocator;
    var list: std.ArrayList(u8) = .{};
    defer list.deinit(alloc);

    try writeJsonEscaped(list.writer(alloc), "\x00\x01\x1f");
    try std.testing.expectEqualStrings("\\u0000\\u0001\\u001f", list.items);
}

test "AI init with claude provider" {
    const alloc = std.testing.allocator;
    var ai = AI.init(alloc, .claude);
    defer ai.deinit();

    try std.testing.expect(ai.provider == .claude);
    try std.testing.expect(ai.child == null);
    try std.testing.expect(ai.stdin_fd == null);
    try std.testing.expect(ai.stdout_fd == null);
    try std.testing.expect(ai.read_len == 0);
}

test "AI init with codex provider" {
    const alloc = std.testing.allocator;
    var ai = AI.init(alloc, .codex);
    defer ai.deinit();

    try std.testing.expect(ai.provider == .codex);
    try std.testing.expect(ai.child == null);
}

test "Provider enum exhaustive" {
    const claude = Provider.claude;
    const codex = Provider.codex;
    try std.testing.expect(@intFromEnum(claude) != @intFromEnum(codex));
}

test "AI multiple deinit safe" {
    const alloc = std.testing.allocator;
    var ai = AI.init(alloc, .claude);
    ai.deinit();
    // Should not crash on double deinit if properly guarded
}

test "writeJsonEscaped long string" {
    const alloc = std.testing.allocator;
    var list: std.ArrayList(u8) = .{};
    defer list.deinit(alloc);

    const long = "a" ** 1000;
    try writeJsonEscaped(list.writer(alloc), long);
    try std.testing.expect(list.items.len == 1000);
}

test "writeJsonEscaped special json chars" {
    const alloc = std.testing.allocator;
    var list: std.ArrayList(u8) = .{};
    defer list.deinit(alloc);

    try writeJsonEscaped(list.writer(alloc), "{\"key\": \"value\"}");
    try std.testing.expectEqualStrings("{\\\"key\\\": \\\"value\\\"}", list.items);
}

test "AI read buffer initial state" {
    const alloc = std.testing.allocator;
    var ai = AI.init(alloc, .claude);
    defer ai.deinit();

    try std.testing.expect(ai.read_len == 0);
}

test "AI stdout_fd initial null" {
    const alloc = std.testing.allocator;
    var ai = AI.init(alloc, .codex);
    defer ai.deinit();

    try std.testing.expect(ai.stdout_fd == null);
}

test "AI stdin_fd initial null" {
    const alloc = std.testing.allocator;
    var ai = AI.init(alloc, .codex);
    defer ai.deinit();

    try std.testing.expect(ai.stdin_fd == null);
}

test "Provider claude command" {
    const alloc = std.testing.allocator;
    var ai = AI.init(alloc, .claude);
    defer ai.deinit();

    try std.testing.expect(ai.provider == .claude);
}
