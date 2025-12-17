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

    pub fn init(alloc: Allocator, provider: Provider) !AI {
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
            entry.value_ptr.response_parts.deinit(self.alloc);
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
        const writer = self.write_buf.writer();
        try writer.writeAll("{\"prompt\":");
        try std.json.encodeJsonString(prompt, .{}, writer);
        try writer.writeAll(",\"id\":");
        try std.json.encodeJsonString(id, .{}, writer);
        try writer.writeAll("}\n");

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
            kv.value.response_parts.deinit(self.alloc);
        }
    }
};

test "AI init/deinit" {
    const alloc = std.testing.allocator;
    var ai = try AI.init(alloc, .claude);
    defer ai.deinit();

    // Should not be running initially
    try std.testing.expect(!ai.isRunning());
}

test "AI provider command" {
    try std.testing.expectEqualStrings("claude", Provider.claude.command());
    try std.testing.expectEqualStrings("codex", Provider.codex.command());
}
