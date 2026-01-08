const std = @import("std");
const fs = std.fs;
const posix = std.posix;
const mem = std.mem;
const process = std.process;

const Allocator = mem.Allocator;

const server = @import("server.zig");
const rpc = @import("rpc.zig");
const storage = @import("storage.zig");
const grammar = @import("grammar.zig");
const paths = @import("paths.zig");
const version = @import("version.zig");

const Mode = enum {
    stdio,
    socket,
};

const Config = struct {
    mode: Mode = .socket,
    socket_path: ?[]const u8 = null,
    project_root: ?[]const u8 = null,

    fn deinit(self: *Config, alloc: Allocator) void {
        if (self.socket_path) |p| alloc.free(p);
        if (self.project_root) |p| alloc.free(p);
    }
};

pub fn main() !void {
    const page_alloc = std.heap.page_allocator;
    var arena = std.heap.ArenaAllocator.init(page_alloc);
    defer arena.deinit();
    const alloc = arena.allocator();

    var config = try parseArgs(alloc);
    defer config.deinit(alloc);

    try loadConfigFile(alloc, &config);

    switch (config.mode) {
        .stdio => try runStdio(alloc, &config),
        .socket => try runSocket(alloc, &config),
    }
}

fn parseArgs(alloc: Allocator) !Config {
    var args_iter = try process.argsWithAllocator(alloc);
    defer args_iter.deinit();

    _ = args_iter.next();

    var config = Config{};

    while (args_iter.next()) |arg| {
        // Check for subcommands first
        if (mem.eql(u8, arg, "grammar")) {
            // Collect remaining args for grammar subcommand
            var sub_args: std.ArrayListUnmanaged([]const u8) = .empty;
            defer sub_args.deinit(alloc);
            while (args_iter.next()) |sub_arg| {
                try sub_args.append(alloc, sub_arg);
            }
            try grammar.run(alloc, sub_args.items);
            process.exit(0);
        }

        if (mem.eql(u8, arg, "--stdio")) {
            config.mode = .stdio;
        } else if (mem.eql(u8, arg, "--serve")) {
            config.mode = .socket;
        } else if (mem.eql(u8, arg, "--socket")) {
            config.mode = .socket;
            const path = args_iter.next() orelse {
                try printError("--socket requires a path argument");
                process.exit(1);
            };
            config.socket_path = try alloc.dupe(u8, path);
        } else if (mem.eql(u8, arg, "--project")) {
            const path = args_iter.next() orelse {
                try printError("--project requires a path argument");
                process.exit(1);
            };
            config.project_root = try alloc.dupe(u8, path);
        } else if (mem.eql(u8, arg, "--help") or mem.eql(u8, arg, "-h")) {
            printHelp();
            process.exit(0);
        } else if (mem.eql(u8, arg, "--version") or mem.eql(u8, arg, "-v")) {
            printVersion();
            process.exit(0);
        } else {
            try printError("Unknown argument: ");
            try fs.File.stderr().writeAll(arg);
            try fs.File.stderr().writeAll("\n");
            try fs.File.stderr().writeAll("Use --help for usage information\n");
            process.exit(1);
        }
    }

    return config;
}

/// Config file JSON schema
const FileConfig = struct {
    socket_path: ?[]const u8 = null,
    project_root: ?[]const u8 = null,
};

fn loadConfigFile(alloc: Allocator, config: *Config) !void {
    const config_path = paths.getConfigPath(alloc) catch |err| {
        std.log.debug("Could not determine config path: {}", .{err});
        return;
    };
    defer alloc.free(config_path);

    const file = fs.openFileAbsolute(config_path, .{}) catch |err| {
        // FileNotFound is normal - config file is optional
        if (err != error.FileNotFound) {
            std.log.warn("Failed to open config file {s}: {}", .{ config_path, err });
        }
        return;
    };
    defer file.close();

    const content = file.readToEndAlloc(alloc, 1024 * 1024) catch |err| {
        std.log.warn("Failed to read config file {s}: {}", .{ config_path, err });
        return;
    };
    defer alloc.free(content);

    const parsed = std.json.parseFromSlice(
        FileConfig,
        alloc,
        content,
        .{ .ignore_unknown_fields = true },
    ) catch |err| {
        std.log.warn("Failed to parse config file {s}: {}", .{ config_path, err });
        return;
    };
    defer parsed.deinit();

    const file_config = parsed.value;

    // Only apply file config values if not already set via command line
    if (file_config.socket_path) |path| {
        if (config.socket_path == null) {
            config.socket_path = try alloc.dupe(u8, path);
        }
    }

    if (file_config.project_root) |path| {
        if (config.project_root == null) {
            config.project_root = try alloc.dupe(u8, path);
        }
    }
}

// getConfigPath moved to paths.zig

fn getSocketPath(alloc: Allocator, config: *const Config) ![]const u8 {
    if (config.socket_path) |path| {
        return try alloc.dupe(u8, path);
    }
    // Delegate to paths module for consistent behavior
    return paths.getSocketPath(alloc);
}

fn getCurrentDir(alloc: Allocator) ![]const u8 {
    return fs.cwd().realpathAlloc(alloc, ".") catch |err| {
        try printError("Failed to get current directory");
        return err;
    };
}

fn getProjectRoot(alloc: Allocator, config: *const Config) ![]const u8 {
    if (config.project_root) |path| {
        return fs.cwd().realpathAlloc(alloc, path) catch |err| {
            try printError("Failed to resolve project root");
            return err;
        };
    }
    return getCurrentDir(alloc);
}

fn runStdio(alloc: Allocator, config: *const Config) !void {
    _ = config;
    const stdin_fd = fs.File.stdin().handle;
    const stdout_fd = fs.File.stdout().handle;

    var buf: [rpc.STREAM_BUFFER_SIZE]u8 = undefined;
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

fn runSocket(alloc: Allocator, config: *const Config) !void {
    const socket_path = try getSocketPath(alloc, config);
    defer alloc.free(socket_path);

    const project_root = try getProjectRoot(alloc, config);
    defer alloc.free(project_root);

    try server.runWithPaths(alloc, socket_path, project_root);
}

fn printError(msg: []const u8) !void {
    try fs.File.stderr().writeAll("error: ");
    try fs.File.stderr().writeAll(msg);
    try fs.File.stderr().writeAll("\n");
}

fn printHelp() void {
    const help =
        \\mnemos - A second brain for your code
        \\
        \\USAGE:
        \\    mnemos [OPTIONS]
        \\    mnemos grammar <COMMAND>
        \\
        \\OPTIONS:
        \\    --stdio              Run in stdio mode (for editor integration)
        \\    --serve              Run as Unix socket server (alias for --socket)
        \\    --socket <path>      Run as Unix socket server at specific path
        \\    --project <path>     Set project root directory (default: current directory)
        \\    --version, -v        Print version
        \\    --help, -h           Print this help
        \\
        \\SUBCOMMANDS:
        \\    grammar              Manage tree-sitter grammars (fetch, build, list)
        \\
        \\ENVIRONMENT:
        \\    XDG_RUNTIME_DIR      Used for default socket path if set
        \\    XDG_CONFIG_HOME      Used for config file location (~/.config/mnemos/config.json)
        \\    HOME                 Fallback for config file location
        \\
        \\CONFIG FILE:
        \\    Config file location: $XDG_CONFIG_HOME/mnemos/config.json
        \\                      or: ~/.config/mnemos/config.json
        \\
        \\    Example config.json:
        \\    {
        \\      "socket_path": "/tmp/mnemos.sock",
        \\      "project_root": "/path/to/project"
        \\    }
        \\
        \\    Command-line arguments override config file settings.
        \\
    ;
    fs.File.stdout().writeAll(help) catch {};
}

fn printVersion() void {
    const version_str = "Mnemos v" ++ version.full ++ "\n";
    fs.File.stdout().writeAll(version_str) catch {};
}

test "basic" {
    // Import other modules to run their tests
    _ = @import("rpc.zig");
    _ = @import("server.zig");
    _ = @import("storage.zig");
    _ = @import("git.zig");
    _ = @import("watcher.zig");
    _ = @import("ai.zig");
    _ = @import("treesitter.zig");
    _ = @import("grammar.zig");
    _ = @import("proptest.zig");
}

test "config default values" {
    const config = Config{};
    try std.testing.expectEqual(Mode.socket, config.mode);
    try std.testing.expect(config.socket_path == null);
    try std.testing.expect(config.project_root == null);
}

test "mode enum values" {
    try std.testing.expect(Mode.stdio != Mode.socket);
    const m: Mode = .stdio;
    try std.testing.expectEqual(Mode.stdio, m);
}

test "version constant" {
    try std.testing.expect(version.semver.len > 0);
    try std.testing.expect(mem.indexOf(u8, version.semver, ".") != null);
}

test "git hash constant" {
    try std.testing.expect(version.git_hash.len > 0);
}

test "full version string" {
    try std.testing.expect(version.full.len > 0);
    try std.testing.expect(mem.indexOf(u8, version.full, "(") != null);
}

test "config deinit frees allocated fields" {
    const testing = std.testing;
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var config = Config{
        .socket_path = try alloc.dupe(u8, "/tmp/test.sock"),
        .project_root = try alloc.dupe(u8, "/home/test"),
    };
    config.deinit(alloc);
}

test "config with null fields" {
    const testing = std.testing;
    const alloc = testing.allocator;

    var config = Config{};
    config.deinit(alloc);
}

test "config with partial allocation" {
    const testing = std.testing;
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var config = Config{
        .socket_path = try alloc.dupe(u8, "/tmp/test.sock"),
        .project_root = null,
    };
    config.deinit(alloc);
}

test "mode enum comparison" {
    const testing = std.testing;
    const stdio_mode: Mode = .stdio;
    const socket_mode: Mode = .socket;

    try testing.expect(stdio_mode == .stdio);
    try testing.expect(socket_mode == .socket);
    try testing.expect(stdio_mode != socket_mode);
}

test "getConfigPath returns valid path" {
    const testing = std.testing;
    const alloc = testing.allocator;

    const path = paths.getConfigPath(alloc) catch |err| {
        if (err == paths.PathError.NoHomeDir) return;
        return err;
    };
    defer alloc.free(path);

    try testing.expect(path.len > 0);
    try testing.expect(mem.endsWith(u8, path, "mnemos/config.json"));
}

test "getSocketPath from config" {
    const testing = std.testing;
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const config = Config{
        .socket_path = try alloc.dupe(u8, "/custom/socket.sock"),
    };

    const path = try getSocketPath(alloc, &config);
    defer alloc.free(path);

    try testing.expect(mem.eql(u8, path, "/custom/socket.sock"));
}

test "getSocketPath without config returns path" {
    const testing = std.testing;
    const alloc = testing.allocator;

    const config = Config{};
    const path = try getSocketPath(alloc, &config);
    defer alloc.free(path);

    try testing.expect(path.len > 0);
    try testing.expect(mem.endsWith(u8, path, "mnemos.sock"));
}

test "getProjectRoot from config" {
    const testing = std.testing;
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const project_root = try tmp_dir.dir.realpathAlloc(alloc, ".");
    defer alloc.free(project_root);
    const config = Config{
        .project_root = try alloc.dupe(u8, project_root),
    };

    const resolved = try getProjectRoot(alloc, &config);
    defer alloc.free(resolved);
    try testing.expect(mem.eql(u8, resolved, project_root));
}

test "getProjectRoot default current dir" {
    const testing = std.testing;
    const alloc = testing.allocator;

    const config = Config{};
    const resolved = try getProjectRoot(alloc, &config);
    defer alloc.free(resolved);

    try testing.expect(resolved.len > 0);
    try testing.expect(resolved[0] == '/');
}

test "getCurrentDir returns path" {
    const testing = std.testing;
    const alloc = testing.allocator;

    const dir = try getCurrentDir(alloc);
    defer alloc.free(dir);

    try testing.expect(dir.len > 0);
    try testing.expect(dir[0] == '/');
}

// Skip stdout tests - they can hang in test environment
// test "printHelp does not crash" { printHelp(); }
// test "printVersion does not crash" { printVersion(); }
