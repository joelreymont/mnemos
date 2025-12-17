const std = @import("std");
const fs = std.fs;
const posix = std.posix;
const mem = std.mem;
const process = std.process;

const Allocator = mem.Allocator;

const server = @import("server.zig");
const rpc = @import("rpc.zig");
const storage = @import("storage.zig");

pub fn main() !void {
    const page_alloc = std.heap.page_allocator;
    var arena = std.heap.ArenaAllocator.init(page_alloc);
    defer arena.deinit();
    const alloc = arena.allocator();

    const args = try process.argsAlloc(alloc);

    // Parse args
    var stdio_mode = false;
    for (args[1..]) |arg| {
        if (mem.eql(u8, arg, "--stdio")) {
            stdio_mode = true;
        } else if (mem.eql(u8, arg, "--help") or mem.eql(u8, arg, "-h")) {
            printHelp();
            return;
        } else if (mem.eql(u8, arg, "--version") or mem.eql(u8, arg, "-v")) {
            printVersion();
            return;
        }
    }

    if (stdio_mode) {
        try runStdio(alloc);
    } else {
        try server.run(alloc);
    }
}

fn runStdio(alloc: Allocator) !void {
    const stdin_fd = fs.File.stdin().handle;
    const stdout_fd = fs.File.stdout().handle;

    var buf: [64 * 1024]u8 = undefined;
    var stream = rpc.Stream.init(stdin_fd, stdout_fd, &buf);

    while (true) {
        const request = stream.readRequest(alloc) catch |err| {
            if (err == error.EndOfStream) break;
            return err;
        };
        defer alloc.free(request);

        const response = rpc.dispatch(alloc, request);
        defer alloc.free(response);

        try stream.writeResponse(response);
    }
}

fn printHelp() void {
    const help =
        \\hemis - A second brain for your code
        \\
        \\USAGE:
        \\    hemis [OPTIONS]
        \\
        \\OPTIONS:
        \\    --stdio     Run in stdio mode (for editor integration)
        \\    --version   Print version
        \\    --help      Print this help
        \\
        \\Without flags, runs as daemon on Unix socket ~/.hemis/hemis.sock
        \\
    ;
    fs.File.stdout().writeAll(help) catch {};
}

fn printVersion() void {
    fs.File.stdout().writeAll("hemis 0.1.0 (zig)\n") catch {};
}

test "basic" {
    // Import other modules to run their tests
    _ = @import("rpc.zig");
    _ = @import("server.zig");
    _ = @import("storage.zig");
}
