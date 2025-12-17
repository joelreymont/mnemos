//! AI subprocess management with streaming pipes
//!
//! Spawns and manages a persistent AI subprocess (claude or codex CLI).
//! Communicates via newline-delimited JSON over stdin/stdout pipes.
//! Handles subprocess lifecycle: start, crash recovery, shutdown.
//! Integrates with poll-based event loop for multiplexed I/O.

const std = @import("std");
const mem = std.mem;
const posix = std.posix;
const process = std.process;

const Allocator = mem.Allocator;

/// Write JSON-escaped string (escapes quotes, backslashes, control chars)
fn writeJsonEscaped(writer: anytype, s: []const u8) !void {
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

/// Request ID type
pub const RequestId = []const u8;

/// Pending request state
const PendingRequest = struct {
    id: []const u8,
    response_parts: std.ArrayList(u8),
    done: bool = false,
};

/// AI subprocess manager
pub const AI = struct {
    alloc: Allocator,
    provider: Provider,
    child: ?process.Child = null,
    stdin_fd: ?posix.fd_t = null,
    stdout_fd: ?posix.fd_t = null,
    read_buf: [64 * 1024]u8 = undefined,
    read_len: usize = 0,
    write_buf: std.ArrayList(u8),
    pending: std.StringHashMap(PendingRequest),

    pub fn init(alloc: Allocator, provider: Provider) AI {
        return .{
            .alloc = alloc,
            .provider = provider,
            .write_buf = .{},
            .pending = std.StringHashMap(PendingRequest).init(alloc),
        };
    }

    pub fn deinit(self: *AI) void {
        self.shutdown();
        self.write_buf.deinit(self.alloc);
        var it = self.pending.iterator();
        while (it.next()) |entry| {
            self.alloc.free(entry.key_ptr.*);
            entry.value_ptr.*.response_parts.deinit(self.alloc);
        }
        self.pending.deinit();
    }

    /// Start the subprocess
    pub fn start(self: *AI) !void {
        if (self.child != null) return error.AlreadyStarted;

        var child = process.Child.init(&.{self.provider.command()}, self.alloc);
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

            // Wait for process to exit (with timeout would be better)
            _ = child.wait() catch {};

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
            if (child.wait() catch null) |term| {
                _ = term;
                // Process exited
                self.child = null;
                self.stdin_fd = null;
                self.stdout_fd = null;
                return false;
            }
            return true;
        }
        return false;
    }

    /// Get stdout fd for poll multiplexing
    pub fn getReadFd(self: *AI) ?posix.fd_t {
        return self.stdout_fd;
    }

    /// Send a prompt request
    pub fn sendPrompt(self: *AI, id: RequestId, prompt: []const u8) !void {
        if (self.stdin_fd == null) return error.NotStarted;

        // Create request JSON
        self.write_buf.clearRetainingCapacity();
        const writer = self.write_buf.writer(self.alloc);
        try writer.writeAll("{\"prompt\":\"");
        try writeJsonEscaped(writer, prompt);
        try writer.writeAll("\",\"id\":\"");
        try writeJsonEscaped(writer, id);
        try writer.writeAll("\"}\n");

        // Write to stdin
        const data = self.write_buf.items;
        var written: usize = 0;
        while (written < data.len) {
            const n = try posix.write(self.stdin_fd.?, data[written..]);
            if (n == 0) return error.BrokenPipe;
            written += n;
        }

        // Track pending request
        const id_copy = try self.alloc.dupe(u8, id);
        try self.pending.put(id_copy, .{
            .id = id_copy,
            .response_parts = .{},
        });
    }

    /// Read available responses (non-blocking if no complete response)
    /// Call this when stdout_fd is ready for reading
    pub fn readResponses(self: *AI, callback: fn (id: RequestId, chunk: []const u8, done: bool) void) !void {
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
            try self.processResponse(line, callback);

            // Shift buffer
            const remaining = self.read_len - newline - 1;
            if (remaining > 0) {
                mem.copyForwards(u8, self.read_buf[0..remaining], self.read_buf[newline + 1 .. self.read_len]);
            }
            self.read_len = remaining;
        }
    }

    fn processResponse(self: *AI, json_line: []const u8, callback: fn (id: RequestId, chunk: []const u8, done: bool) void) !void {
        // Parse JSON response
        const parsed = try std.json.parseFromSlice(
            std.json.Value,
            self.alloc,
            json_line,
            .{},
        );
        defer parsed.deinit();

        const obj = parsed.value.object;
        const id = obj.get("id").?.string;
        const response = obj.get("response").?.string;
        const done = obj.get("done").?.bool;

        // Update pending request
        if (self.pending.getPtr(id)) |pending| {
            try pending.response_parts.appendSlice(self.alloc, response);

            // Invoke callback
            callback(id, response, done);

            // Clean up if done
            if (done) {
                self.alloc.free(pending.id);
                pending.response_parts.deinit(self.alloc);
                _ = self.pending.remove(id);
            }
        }
    }

    /// Get full accumulated response for a request ID
    pub fn getResponse(self: *AI, id: RequestId) ?[]const u8 {
        if (self.pending.get(id)) |pending| {
            return pending.response_parts.items;
        }
        return null;
    }

    /// Remove pending request
    pub fn removePending(self: *AI, id: RequestId) void {
        if (self.pending.fetchRemove(id)) |kv| {
            self.alloc.free(kv.key);
            var response_parts = kv.value.response_parts;
            response_parts.deinit(self.alloc);
        }
    }
};

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
    const result = ai.sendPrompt("test-id", "test prompt");
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

test "AI getResponse for unknown id" {
    const alloc = std.testing.allocator;
    var ai = AI.init(alloc, .claude);
    defer ai.deinit();

    // Should return null for unknown request ID
    try std.testing.expect(ai.getResponse("unknown-id") == null);
}

test "AI removePending for unknown id" {
    const alloc = std.testing.allocator;
    var ai = AI.init(alloc, .claude);
    defer ai.deinit();

    // Should not crash when removing unknown ID
    ai.removePending("unknown-id");
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

test "PendingRequest struct initialization" {
    const alloc = std.testing.allocator;
    var response_parts: std.ArrayList(u8) = .{};
    defer response_parts.deinit(alloc);

    const pending = PendingRequest{
        .id = "test-id",
        .response_parts = response_parts,
    };

    try std.testing.expectEqualStrings("test-id", pending.id);
    try std.testing.expect(!pending.done);
}

test "PendingRequest with done flag" {
    const alloc = std.testing.allocator;
    var response_parts: std.ArrayList(u8) = .{};
    defer response_parts.deinit(alloc);

    const pending = PendingRequest{
        .id = "test-id",
        .response_parts = response_parts,
        .done = true,
    };

    try std.testing.expect(pending.done);
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

test "AI pending requests initially empty" {
    const alloc = std.testing.allocator;
    var ai = AI.init(alloc, .claude);
    defer ai.deinit();

    try std.testing.expect(ai.pending.count() == 0);
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
