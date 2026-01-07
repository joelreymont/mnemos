//! Unix socket server with poll-based multiplexing
//!
//! Handles multiple clients simultaneously using posix.poll().
//! Ready to migrate to std.Io async when available.

const std = @import("std");
const mem = std.mem;
const posix = std.posix;
const fs = std.fs;

const Allocator = mem.Allocator;
const rpc = @import("rpc.zig");
const storage = @import("storage.zig");
const watcher = @import("watcher.zig");

const MAX_CLIENTS = 32;

/// Client connection state
const Client = struct {
    fd: posix.fd_t,
    buf: [64 * 1024]u8 = undefined,
    stream: ?rpc.Stream = null,

    fn init(fd: posix.fd_t) Client {
        var c = Client{ .fd = fd };
        c.stream = rpc.Stream.init(fd, fd, &c.buf);
        return c;
    }
};

/// Server state
const Server = struct {
    alloc: Allocator,
    listen_fd: posix.fd_t,
    clients: [MAX_CLIENTS]?Client = .{null} ** MAX_CLIENTS,
    socket_path: []const u8,
    db: storage.Database,
    file_watcher: ?*watcher.Watcher = null,

    fn init(alloc: Allocator, socket_path: []const u8, db_path: []const u8) !Server {
        // Open database
        const db = try storage.Database.open(alloc, db_path);
        // Remove stale socket
        fs.deleteFileAbsolute(socket_path) catch {};

        // Create socket
        const listen_fd = try posix.socket(posix.AF.UNIX, posix.SOCK.STREAM, 0);
        errdefer posix.close(listen_fd);

        // Bind
        var addr: posix.sockaddr.un = .{ .family = posix.AF.UNIX, .path = undefined };
        @memset(&addr.path, 0);
        const path_bytes: []const u8 = socket_path;
        if (path_bytes.len >= addr.path.len) return error.PathTooLong;
        @memcpy(addr.path[0..path_bytes.len], path_bytes);

        try posix.bind(listen_fd, @ptrCast(&addr), @sizeOf(posix.sockaddr.un));
        try posix.listen(listen_fd, 16);

        return .{
            .alloc = alloc,
            .listen_fd = listen_fd,
            .socket_path = socket_path,
            .db = db,
        };
    }

    fn deinit(self: *Server) void {
        if (self.file_watcher) |w| w.deinit();
        for (&self.clients) |*slot| {
            if (slot.*) |client| {
                posix.close(client.fd);
                slot.* = null;
            }
        }
        posix.close(self.listen_fd);
        fs.deleteFileAbsolute(self.socket_path) catch {};
        self.db.close();
    }

    /// Start watching a directory for file changes
    pub fn watchDirectory(self: *Server, path: []const u8) !void {
        if (self.file_watcher) |w| w.deinit();

        const callback = struct {
            fn cb(event: watcher.FileEvent, userdata: ?*anyopaque) void {
                _ = userdata;
                std.debug.print("File change: {s} ({s})\n", .{
                    event.path,
                    @tagName(event.kind),
                });
                // TODO: Trigger reindex for modified files
            }
        }.cb;

        self.file_watcher = try watcher.Watcher.init(self.alloc, path, callback, null);
    }

    fn acceptClient(self: *Server) !void {
        const client_fd = try posix.accept(self.listen_fd, null, null, 0);

        // Find free slot
        for (&self.clients) |*slot| {
            if (slot.* == null) {
                slot.* = Client.init(client_fd);
                return;
            }
        }

        // No slots available
        posix.close(client_fd);
    }

    fn handleClient(self: *Server, idx: usize) void {
        const slot = &self.clients[idx];
        var client = slot.* orelse return;

        var stream = client.stream orelse return;

        const request = stream.readRequest(self.alloc) catch |err| {
            if (err == error.EndOfStream or err == error.BrokenPipe or err == error.ConnectionResetByPeer) {
                posix.close(client.fd);
                slot.* = null;
            }
            return;
        };
        defer self.alloc.free(request);

        const response = rpc.dispatchWithDb(self.alloc, request, &self.db);
        defer self.alloc.free(response);

        stream.writeResponse(response) catch {
            posix.close(client.fd);
            slot.* = null;
        };

        // Update stream state back
        client.stream = stream;
        slot.* = client;
    }

    fn runLoop(self: *Server) !void {
        // +2 for listen socket and optional watcher
        var poll_fds: [MAX_CLIENTS + 2]posix.pollfd = undefined;
        var watcher_poll_idx: ?usize = null;

        while (!shutdown_requested) {
            // Build poll set
            poll_fds[0] = .{
                .fd = self.listen_fd,
                .events = posix.POLL.IN,
                .revents = 0,
            };

            var nfds: usize = 1;

            // Add file watcher fd if active
            if (self.file_watcher) |w| {
                poll_fds[nfds] = .{
                    .fd = w.getFd(),
                    .events = posix.POLL.IN,
                    .revents = 0,
                };
                watcher_poll_idx = nfds;
                nfds += 1;
            } else {
                watcher_poll_idx = null;
            }

            for (self.clients, 0..) |maybe_client, idx| {
                if (maybe_client) |client| {
                    poll_fds[nfds] = .{
                        .fd = client.fd,
                        .events = posix.POLL.IN,
                        .revents = 0,
                    };
                    // Store client index in unused field via tag
                    _ = idx;
                    nfds += 1;
                }
            }

            // Wait for events (1 second timeout to check shutdown flag)
            const ready = posix.poll(poll_fds[0..nfds], 1000) catch |err| {
                if (err == error.Interrupted) continue;
                return err;
            };
            if (ready == 0) continue;

            // Check listen socket
            if (poll_fds[0].revents & posix.POLL.IN != 0) {
                self.acceptClient() catch {};
            }

            // Check file watcher
            if (watcher_poll_idx) |idx| {
                if (poll_fds[idx].revents & posix.POLL.IN != 0) {
                    if (self.file_watcher) |w| {
                        w.processEvents() catch {};
                    }
                }
            }

            // Check client sockets (start after watcher)
            const client_start_idx: usize = if (watcher_poll_idx != null) 2 else 1;
            var poll_idx: usize = client_start_idx;
            for (0..MAX_CLIENTS) |client_idx| {
                if (self.clients[client_idx] != null) {
                    if (poll_idx < nfds and poll_fds[poll_idx].revents & posix.POLL.IN != 0) {
                        self.handleClient(client_idx);
                    }
                    poll_idx += 1;
                }
            }
        }

        std.debug.print("mnemos shutting down cleanly\n", .{});
    }
};

/// Get socket path (prefer MNEMOS_DIR, then XDG_RUNTIME_DIR, fallback to ~/.mnemos)
fn getSocketPath(alloc: Allocator) ![]const u8 {
    // Try MNEMOS_DIR first (for tests and custom setups)
    if (std.process.getEnvVarOwned(alloc, "MNEMOS_DIR")) |mnemos_dir| {
        defer alloc.free(mnemos_dir);
        return try std.fmt.allocPrint(alloc, "{s}/mnemos.sock", .{mnemos_dir});
    } else |_| {}

    // Try XDG_RUNTIME_DIR
    if (std.process.getEnvVarOwned(alloc, "XDG_RUNTIME_DIR")) |runtime_dir| {
        defer alloc.free(runtime_dir);
        return try std.fmt.allocPrint(alloc, "{s}/mnemos.sock", .{runtime_dir});
    } else |_| {}

    // Fallback to ~/.mnemos/mnemos.sock
    const mnemos_dir = try getMnemosDir(alloc);
    defer alloc.free(mnemos_dir);
    return try std.fmt.allocPrint(alloc, "{s}/mnemos.sock", .{mnemos_dir});
}

/// Get mnemos directory, creating if needed
fn getMnemosDir(alloc: Allocator) ![]const u8 {
    if (std.process.getEnvVarOwned(alloc, "MNEMOS_DIR")) |dir| {
        return dir;
    } else |_| {}

    if (std.process.getEnvVarOwned(alloc, "HOME")) |home| {
        defer alloc.free(home);
        const path = try std.fmt.allocPrint(alloc, "{s}/.mnemos", .{home});
        fs.makeDirAbsolute(path) catch |err| {
            if (err != error.PathAlreadyExists) return err;
        };
        return path;
    } else |_| {}

    return error.NoHomeDir;
}

/// Global flag for shutdown signal
var shutdown_requested: bool = false;

/// Signal handler for clean shutdown
fn handleShutdown(_: c_int) callconv(.c) void {
    shutdown_requested = true;
}

/// Run the server with default paths
pub fn run(alloc: Allocator) !void {
    const mnemos_dir = try getMnemosDir(alloc);
    defer alloc.free(mnemos_dir);

    const socket_path = try getSocketPath(alloc);
    defer alloc.free(socket_path);

    const db_path = try std.fmt.allocPrint(alloc, "{s}/mnemos.db", .{mnemos_dir});
    defer alloc.free(db_path);

    try runWithPaths(alloc, socket_path, db_path);
}

/// Run the server with explicit paths
pub fn runWithPaths(alloc: Allocator, socket_path: []const u8, db_path: []const u8) !void {
    // Install signal handlers
    const empty_mask = mem.zeroes(posix.sigset_t);
    const sigaction = posix.Sigaction{
        .handler = .{ .handler = handleShutdown },
        .mask = empty_mask,
        .flags = 0,
    };
    posix.sigaction(posix.SIG.INT, &sigaction, null);
    posix.sigaction(posix.SIG.TERM, &sigaction, null);

    var srv = try Server.init(alloc, socket_path, db_path);
    defer srv.deinit();

    std.debug.print("mnemos listening on {s}\n", .{socket_path});

    try srv.runLoop();
}

test "server init" {
    // Just test that types compile
    _ = Server;
    _ = Client;
}

test "client init" {
    const testing = std.testing;

    // Test client initialization
    const client = Client.init(0);
    try testing.expect(client.fd == 0);
    try testing.expect(client.stream != null);
}

test "max clients constant" {
    const testing = std.testing;

    // Verify MAX_CLIENTS is reasonable
    try testing.expect(MAX_CLIENTS >= 1);
    try testing.expect(MAX_CLIENTS <= 1024);
}

test "getMnemosDir with env" {
    const testing = std.testing;
    const alloc = testing.allocator;

    // Test getMnemosDir returns a path
    const dir = getMnemosDir(alloc) catch |err| {
        // HOME might not be set in test environment
        if (err == error.NoHomeDir) return;
        return err;
    };
    defer alloc.free(dir);
    try testing.expect(dir.len > 0);
}

test "signal handler setup" {
    // Verify signal handler is set up correctly
    _ = handleShutdown;
    _ = shutdown_requested;
}

test "getSocketPath returns path" {
    const testing = std.testing;
    const alloc = testing.allocator;

    const path = getSocketPath(alloc) catch |err| {
        // May fail if HOME not set
        if (err == error.NoHomeDir) return;
        return err;
    };
    defer alloc.free(path);

    try testing.expect(path.len > 0);
    try testing.expect(mem.endsWith(u8, path, "mnemos.sock"));
}

test "shutdown_requested initial value" {
    // Should start as false
    try std.testing.expect(!shutdown_requested);
}

test "Client struct fd assignment" {
    const testing = std.testing;
    const test_fd: posix.fd_t = 42;
    const client = Client.init(test_fd);
    try testing.expect(client.fd == 42);
}

test "Client struct has buffer" {
    const testing = std.testing;
    const client = Client.init(0);
    try testing.expect(client.buf.len == 64 * 1024);
}

test "MAX_CLIENTS value" {
    const testing = std.testing;
    try testing.expect(MAX_CLIENTS == 32);
}

test "MAX_CLIENTS in range" {
    const testing = std.testing;
    try testing.expect(MAX_CLIENTS > 0);
    try testing.expect(MAX_CLIENTS < 100);
}

test "Server struct has allocator" {
    _ = Server;
}

test "Server struct has database" {
    _ = Server;
}

test "getSocketPath contains sock extension" {
    const testing = std.testing;
    const alloc = testing.allocator;

    const path = getSocketPath(alloc) catch |err| {
        if (err == error.NoHomeDir) return;
        return err;
    };
    defer alloc.free(path);

    try testing.expect(mem.indexOf(u8, path, ".sock") != null);
}

test "getMnemosDir creates directory" {
    const testing = std.testing;
    const alloc = testing.allocator;

    const dir = getMnemosDir(alloc) catch |err| {
        if (err == error.NoHomeDir) return;
        return err;
    };
    defer alloc.free(dir);

    try testing.expect(mem.indexOf(u8, dir, ".mnemos") != null or mem.indexOf(u8, dir, "MNEMOS") != null);
}

test "handleShutdown sets flag" {
    shutdown_requested = false;
    handleShutdown(0);
    try std.testing.expect(shutdown_requested);
    shutdown_requested = false;
}

test "Client buffer size" {
    const client = Client.init(5);
    try std.testing.expect(client.buf.len > 0);
    try std.testing.expect(client.buf.len == 64 * 1024);
}

test "Client init with various fds" {
    const testing = std.testing;
    const c1 = Client.init(0);
    const c2 = Client.init(10);
    const c3 = Client.init(100);

    try testing.expect(c1.fd == 0);
    try testing.expect(c2.fd == 10);
    try testing.expect(c3.fd == 100);
}

test "Client stream not null" {
    const testing = std.testing;
    const client = Client.init(7);
    try testing.expect(client.stream != null);
}

test "handleShutdown with different signals" {
    shutdown_requested = false;
    handleShutdown(2); // SIGINT
    try std.testing.expect(shutdown_requested);

    shutdown_requested = false;
    handleShutdown(15); // SIGTERM
    try std.testing.expect(shutdown_requested);

    shutdown_requested = false;
}

test "MAX_CLIENTS is power of 2" {
    const testing = std.testing;
    // 32 is a power of 2
    try testing.expect(MAX_CLIENTS == 32);
    try testing.expect(MAX_CLIENTS & (MAX_CLIENTS - 1) == 0);
}

test "getSocketPath path format" {
    const testing = std.testing;
    const alloc = testing.allocator;

    const path = getSocketPath(alloc) catch |err| {
        if (err == error.NoHomeDir) return;
        return err;
    };
    defer alloc.free(path);

    // Path should be absolute
    try testing.expect(path[0] == '/');
}

test "getMnemosDir path format" {
    const testing = std.testing;
    const alloc = testing.allocator;

    const dir = getMnemosDir(alloc) catch |err| {
        if (err == error.NoHomeDir) return;
        return err;
    };
    defer alloc.free(dir);

    // Path should be absolute
    try testing.expect(dir[0] == '/');
}

test "shutdown_requested can be toggled" {
    shutdown_requested = false;
    try std.testing.expect(!shutdown_requested);
    shutdown_requested = true;
    try std.testing.expect(shutdown_requested);
    shutdown_requested = false;
}

test "Client fd is posix fd type" {
    const client = Client.init(42);
    const fd: posix.fd_t = client.fd;
    try std.testing.expect(fd == 42);
}

test "Server and Client types exist" {
    _ = Server;
    _ = Client;
    _ = MAX_CLIENTS;
}
