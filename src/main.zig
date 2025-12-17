const std = @import("std");
const fs = std.fs;
const posix = std.posix;
const mem = std.mem;
const process = std.process;
const build_options = @import("build_options");

const Allocator = mem.Allocator;

const server = @import("server.zig");
const rpc = @import("rpc.zig");
const storage = @import("storage.zig");

const VERSION = "0.1.0";
const GIT_HASH = build_options.git_hash;

const Mode = enum {
    stdio,
    socket,
};

const Config = struct {
    mode: Mode = .socket,
    socket_path: ?[]const u8 = null,
    project_root: ?[]const u8 = null,
    db_path: ?[]const u8 = null,

    fn deinit(self: *Config, alloc: Allocator) void {
        if (self.socket_path) |p| alloc.free(p);
        if (self.project_root) |p| alloc.free(p);
        if (self.db_path) |p| alloc.free(p);
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
        if (mem.eql(u8, arg, "--stdio")) {
            config.mode = .stdio;
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
        } else if (mem.eql(u8, arg, "--db")) {
            const path = args_iter.next() orelse {
                try printError("--db requires a path argument");
                process.exit(1);
            };
            config.db_path = try alloc.dupe(u8, path);
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

fn loadConfigFile(alloc: Allocator, config: *Config) !void {
    const config_path = getConfigPath(alloc) catch return;
    defer alloc.free(config_path);

    const file = fs.openFileAbsolute(config_path, .{}) catch return;
    defer file.close();

    const content = file.readToEndAlloc(alloc, 1024 * 1024) catch return;
    defer alloc.free(content);

    var parsed = std.json.parseFromSlice(
        std.json.Value,
        alloc,
        content,
        .{},
    ) catch return;
    defer parsed.deinit();

    const root = parsed.value.object;

    if (root.get("socket_path")) |val| {
        if (val == .string and config.socket_path == null) {
            config.socket_path = try alloc.dupe(u8, val.string);
        }
    }

    if (root.get("project_root")) |val| {
        if (val == .string and config.project_root == null) {
            config.project_root = try alloc.dupe(u8, val.string);
        }
    }

    if (root.get("db_path")) |val| {
        if (val == .string and config.db_path == null) {
            config.db_path = try alloc.dupe(u8, val.string);
        }
    }
}

fn getConfigPath(alloc: Allocator) ![]const u8 {
    if (process.getEnvVarOwned(alloc, "XDG_CONFIG_HOME")) |xdg_config| {
        defer alloc.free(xdg_config);
        return try std.fmt.allocPrint(alloc, "{s}/hemis/config.json", .{xdg_config});
    } else |_| {}

    if (process.getEnvVarOwned(alloc, "HOME")) |home| {
        defer alloc.free(home);
        return try std.fmt.allocPrint(alloc, "{s}/.config/hemis/config.json", .{home});
    } else |_| {}

    return error.NoHomeDir;
}

fn getSocketPath(alloc: Allocator, config: *const Config) ![]const u8 {
    if (config.socket_path) |path| {
        return try alloc.dupe(u8, path);
    }

    if (process.getEnvVarOwned(alloc, "XDG_RUNTIME_DIR")) |runtime_dir| {
        defer alloc.free(runtime_dir);
        return try std.fmt.allocPrint(alloc, "{s}/hemis.sock", .{runtime_dir});
    } else |_| {}

    return try std.fmt.allocPrint(alloc, "/tmp/hemis.sock", .{});
}

fn getDbPath(alloc: Allocator, config: *const Config) ![]const u8 {
    if (config.db_path) |path| {
        return try alloc.dupe(u8, path);
    }

    const project_root = config.project_root orelse try getCurrentDir(alloc);
    defer if (config.project_root == null) alloc.free(project_root);

    const hemis_dir = try std.fmt.allocPrint(alloc, "{s}/.hemis", .{project_root});
    defer alloc.free(hemis_dir);

    fs.makeDirAbsolute(hemis_dir) catch |err| {
        if (err != error.PathAlreadyExists) return err;
    };

    return try std.fmt.allocPrint(alloc, "{s}/db.sqlite", .{hemis_dir});
}

fn getCurrentDir(alloc: Allocator) ![]const u8 {
    return fs.cwd().realpathAlloc(alloc, ".") catch |err| {
        try printError("Failed to get current directory");
        return err;
    };
}

fn runStdio(alloc: Allocator, config: *const Config) !void {
    _ = config;
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

fn runSocket(alloc: Allocator, config: *const Config) !void {
    const socket_path = try getSocketPath(alloc, config);
    defer alloc.free(socket_path);

    const db_path = try getDbPath(alloc, config);
    defer alloc.free(db_path);

    try server.runWithPaths(alloc, socket_path, db_path);
}

fn printError(msg: []const u8) !void {
    try fs.File.stderr().writeAll("error: ");
    try fs.File.stderr().writeAll(msg);
    try fs.File.stderr().writeAll("\n");
}

fn printHelp() void {
    const help =
        \\hemis - A second brain for your code
        \\
        \\USAGE:
        \\    hemis [OPTIONS]
        \\
        \\OPTIONS:
        \\    --stdio              Run in stdio mode (for editor integration)
        \\    --socket <path>      Run as Unix socket server (default: /tmp/hemis.sock)
        \\    --project <path>     Set project root directory (default: current directory)
        \\    --db <path>          Database file path (default: .hemis/db.sqlite)
        \\    --version, -v        Print version
        \\    --help, -h           Print this help
        \\
        \\ENVIRONMENT:
        \\    XDG_RUNTIME_DIR      Used for default socket path if set
        \\    XDG_CONFIG_HOME      Used for config file location (~/.config/hemis/config.json)
        \\    HOME                 Fallback for config file location
        \\
        \\CONFIG FILE:
        \\    Config file location: $XDG_CONFIG_HOME/hemis/config.json
        \\                      or: ~/.config/hemis/config.json
        \\
        \\    Example config.json:
        \\    {
        \\      "socket_path": "/tmp/hemis.sock",
        \\      "project_root": "/path/to/project",
        \\      "db_path": "/path/to/db.sqlite"
        \\    }
        \\
        \\    Command-line arguments override config file settings.
        \\
    ;
    fs.File.stdout().writeAll(help) catch {};
}

fn printVersion() void {
    const version_str = "Hemis v" ++ VERSION ++ " (" ++ GIT_HASH ++ ")\n";
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
}

test "config default values" {
    const config = Config{};
    try std.testing.expectEqual(Mode.socket, config.mode);
    try std.testing.expect(config.socket_path == null);
    try std.testing.expect(config.project_root == null);
    try std.testing.expect(config.db_path == null);
}

test "mode enum values" {
    try std.testing.expect(Mode.stdio != Mode.socket);
    const m: Mode = .stdio;
    try std.testing.expectEqual(Mode.stdio, m);
}

test "version constant" {
    try std.testing.expect(VERSION.len > 0);
    try std.testing.expect(mem.indexOf(u8, VERSION, ".") != null);
}

test "git hash constant" {
    try std.testing.expect(GIT_HASH.len > 0);
}
