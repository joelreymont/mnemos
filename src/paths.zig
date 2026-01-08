//! Shared path resolution utilities
//!
//! Centralizes environment variable lookups and path construction
//! for socket paths, config directories, and grammars.

const std = @import("std");
const fs = std.fs;
const process = std.process;
const mem = std.mem;
const Allocator = mem.Allocator;

pub const PathError = error{
    NoHomeDir,
    OutOfMemory,
};

/// Get HOME directory from environment
pub fn getHomeDir(alloc: Allocator) ![]const u8 {
    return process.getEnvVarOwned(alloc, "HOME") catch return PathError.NoHomeDir;
}

/// Get mnemos directory (~/.mnemos), creating if needed
/// Prefers MNEMOS_DIR env var if set
pub fn getMnemosDir(alloc: Allocator) ![]const u8 {
    if (process.getEnvVarOwned(alloc, "MNEMOS_DIR")) |dir| {
        return dir;
    } else |_| {}

    const home = try getHomeDir(alloc);
    defer alloc.free(home);

    const path = try std.fmt.allocPrint(alloc, "{s}/.mnemos", .{home});
    fs.makeDirAbsolute(path) catch |err| {
        if (err != error.PathAlreadyExists) {
            alloc.free(path);
            return PathError.OutOfMemory;
        }
    };
    return path;
}

/// Get config directory (~/.config/mnemos)
/// Prefers XDG_CONFIG_HOME if set
pub fn getConfigDir(alloc: Allocator) ![]const u8 {
    if (process.getEnvVarOwned(alloc, "XDG_CONFIG_HOME")) |xdg| {
        defer alloc.free(xdg);
        return try std.fmt.allocPrint(alloc, "{s}/mnemos", .{xdg});
    } else |_| {}

    const home = try getHomeDir(alloc);
    defer alloc.free(home);
    return try std.fmt.allocPrint(alloc, "{s}/.config/mnemos", .{home});
}

/// Get config file path (~/.config/mnemos/config.json)
pub fn getConfigPath(alloc: Allocator) ![]const u8 {
    if (process.getEnvVarOwned(alloc, "XDG_CONFIG_HOME")) |xdg| {
        defer alloc.free(xdg);
        return try std.fmt.allocPrint(alloc, "{s}/mnemos/config.json", .{xdg});
    } else |_| {}

    const home = try getHomeDir(alloc);
    defer alloc.free(home);
    return try std.fmt.allocPrint(alloc, "{s}/.config/mnemos/config.json", .{home});
}

/// Get socket path for IPC
/// Priority: MNEMOS_DIR -> XDG_RUNTIME_DIR -> ~/.mnemos
pub fn getSocketPath(alloc: Allocator) ![]const u8 {
    // Try MNEMOS_DIR first (for tests and custom setups)
    if (process.getEnvVarOwned(alloc, "MNEMOS_DIR")) |mnemos_dir| {
        defer alloc.free(mnemos_dir);
        return try std.fmt.allocPrint(alloc, "{s}/mnemos.sock", .{mnemos_dir});
    } else |_| {}

    // Try XDG_RUNTIME_DIR
    if (process.getEnvVarOwned(alloc, "XDG_RUNTIME_DIR")) |runtime_dir| {
        defer alloc.free(runtime_dir);
        return try std.fmt.allocPrint(alloc, "{s}/mnemos.sock", .{runtime_dir});
    } else |_| {}

    // Fallback to ~/.mnemos/mnemos.sock
    const mnemos_dir = try getMnemosDir(alloc);
    defer alloc.free(mnemos_dir);
    return try std.fmt.allocPrint(alloc, "{s}/mnemos.sock", .{mnemos_dir});
}

/// Get grammars directory (~/.config/mnemos/grammars)
pub fn getGrammarsDir(alloc: Allocator) ![]const u8 {
    if (process.getEnvVarOwned(alloc, "XDG_CONFIG_HOME")) |xdg| {
        defer alloc.free(xdg);
        return try std.fmt.allocPrint(alloc, "{s}/mnemos/grammars", .{xdg});
    } else |_| {}

    const home = try getHomeDir(alloc);
    defer alloc.free(home);
    return try std.fmt.allocPrint(alloc, "{s}/.config/mnemos/grammars", .{home});
}

// Tests

test "getMnemosDir returns path" {
    const alloc = std.testing.allocator;
    const dir = getMnemosDir(alloc) catch |err| {
        if (err == PathError.NoHomeDir) return;
        return err;
    };
    defer alloc.free(dir);
    try std.testing.expect(dir.len > 0);
}

test "getSocketPath returns path" {
    const alloc = std.testing.allocator;
    const path = getSocketPath(alloc) catch |err| {
        if (err == PathError.NoHomeDir) return;
        return err;
    };
    defer alloc.free(path);
    try std.testing.expect(mem.endsWith(u8, path, "mnemos.sock"));
}

test "getConfigPath returns path" {
    const alloc = std.testing.allocator;
    const path = getConfigPath(alloc) catch |err| {
        if (err == PathError.NoHomeDir) return;
        return err;
    };
    defer alloc.free(path);
    try std.testing.expect(mem.endsWith(u8, path, "config.json"));
}

test "getGrammarsDir returns path" {
    const alloc = std.testing.allocator;
    const dir = getGrammarsDir(alloc) catch |err| {
        if (err == PathError.NoHomeDir) return;
        return err;
    };
    defer alloc.free(dir);
    try std.testing.expect(mem.endsWith(u8, dir, "grammars"));
}
