const std = @import("std");
const fs = std.fs;
const process = std.process;
const mem = std.mem;
const Allocator = mem.Allocator;

/// Get grammars directory (~/.config/mnemos/grammars)
fn getGrammarsDir(alloc: Allocator) ![]const u8 {
    if (process.getEnvVarOwned(alloc, "XDG_CONFIG_HOME")) |xdg| {
        defer alloc.free(xdg);
        return try std.fmt.allocPrint(alloc, "{s}/mnemos/grammars", .{xdg});
    } else |_| {}

    if (process.getEnvVarOwned(alloc, "HOME")) |home| {
        defer alloc.free(home);
        return try std.fmt.allocPrint(alloc, "{s}/.config/mnemos/grammars", .{home});
    } else |_| {}

    return error.NoHomeDir;
}

/// Validate git URL - only allow https:// and git://
fn validateGitUrl(url: []const u8) !void {
    if (!mem.startsWith(u8, url, "https://") and !mem.startsWith(u8, url, "git://")) {
        return error.InvalidGitUrl;
    }
    // Reject shell metacharacters
    for (url) |c| {
        if (c == ';' or c == '|' or c == '&' or c == '$' or c == '`' or c == '\n' or c == '\r') {
            return error.InvalidGitUrl;
        }
    }
}

/// Validate git ref name
fn validateGitRef(ref: []const u8) !void {
    if (ref.len == 0 or ref.len > 256) return error.InvalidGitRef;
    if (ref[0] == '-') return error.InvalidGitRef;
    if (mem.indexOf(u8, ref, "..") != null or mem.indexOf(u8, ref, "//") != null) {
        return error.InvalidGitRef;
    }
    for (ref) |c| {
        const valid = std.ascii.isAlphanumeric(c) or c == '-' or c == '_' or c == '/' or c == '.';
        if (!valid) return error.InvalidGitRef;
    }
}

/// Run command and return exit status
fn runCommand(alloc: Allocator, argv: []const []const u8, cwd: ?[]const u8) !u8 {
    var child = process.Child.init(argv, alloc);
    child.cwd = cwd;
    child.stderr_behavior = .Inherit;
    child.stdout_behavior = .Inherit;

    try child.spawn();
    const term = try child.wait();

    return switch (term) {
        .Exited => |code| code,
        else => error.ProcessFailed,
    };
}

/// Print message to stderr
fn printErr(comptime fmt: []const u8, args: anytype) void {
    var buf: [4096]u8 = undefined;
    const msg = std.fmt.bufPrint(&buf, fmt, args) catch return;
    fs.File.stderr().writeAll(msg) catch {};
}

/// Print message to stdout
fn print(comptime fmt: []const u8, args: anytype) void {
    var buf: [4096]u8 = undefined;
    const msg = std.fmt.bufPrint(&buf, fmt, args) catch return;
    fs.File.stdout().writeAll(msg) catch {};
}

/// Check if path exists
fn pathExists(path: []const u8) bool {
    fs.accessAbsolute(path, .{}) catch return false;
    return true;
}

/// Grammar subcommand entry point
pub fn run(alloc: Allocator, args: []const []const u8) !void {
    if (args.len == 0) {
        printHelp();
        return;
    }

    const cmd = args[0];

    if (mem.eql(u8, cmd, "help") or mem.eql(u8, cmd, "--help") or mem.eql(u8, cmd, "-h")) {
        printHelp();
    } else if (mem.eql(u8, cmd, "list")) {
        try list(alloc);
    } else if (mem.eql(u8, cmd, "fetch")) {
        if (args.len < 2) {
            printErr("error: 'grammar fetch' requires a grammar name\n", .{});
            printErr("Usage: mnemos grammar fetch <name>\n", .{});
            printErr("       mnemos grammar fetch --all\n", .{});
            process.exit(1);
        }
        if (mem.eql(u8, args[1], "--all")) {
            try fetchAll(alloc);
        } else {
            try fetch(alloc, args[1]);
        }
    } else if (mem.eql(u8, cmd, "build")) {
        if (args.len < 2) {
            printErr("error: 'grammar build' requires a grammar name\n", .{});
            printErr("Usage: mnemos grammar build <name>\n", .{});
            printErr("       mnemos grammar build --all\n", .{});
            process.exit(1);
        }
        if (mem.eql(u8, args[1], "--all")) {
            try buildAll(alloc);
        } else {
            try build(alloc, args[1]);
        }
    } else {
        printErr("error: unknown grammar subcommand: {s}\n", .{cmd});
        printHelp();
        process.exit(1);
    }
}

fn printHelp() void {
    const help =
        \\mnemos grammar - Manage tree-sitter grammars
        \\
        \\USAGE:
        \\    mnemos grammar <COMMAND>
        \\
        \\COMMANDS:
        \\    list              List installed grammars
        \\    fetch <name>      Fetch grammar source from git
        \\    fetch --all       Fetch all common grammars
        \\    build <name>      Build a grammar from source
        \\    build --all       Build all fetched grammars
        \\    help              Show this help message
        \\
        \\EXAMPLES:
        \\    mnemos grammar fetch rust
        \\    mnemos grammar build rust
        \\
        \\LOCATIONS:
        \\    Sources: ~/.config/mnemos/grammars/sources/<name>/
        \\    Built:   ~/.config/mnemos/grammars/libtree-sitter-<name>.{dylib,so}
        \\
    ;
    fs.File.stdout().writeAll(help) catch {};
}

/// List installed grammars
fn list(alloc: Allocator) !void {
    const grammars_dir = getGrammarsDir(alloc) catch {
        printErr("error: could not determine grammars directory (HOME not set?)\n", .{});
        process.exit(1);
    };
    defer alloc.free(grammars_dir);

    const lib_ext = if (@import("builtin").os.tag == .macos) ".dylib" else ".so";

    // List built libraries
    print("Installed grammars ({s}):\n", .{grammars_dir});

    var dir = fs.openDirAbsolute(grammars_dir, .{ .iterate = true }) catch |err| {
        if (err == error.FileNotFound) {
            print("  (none - run 'mnemos grammar fetch <name>' then 'mnemos grammar build <name>')\n", .{});
            return;
        }
        printErr("error: could not open grammars directory: {s}\n", .{@errorName(err)});
        process.exit(1);
    };
    defer dir.close();

    var count: u32 = 0;
    var iter = dir.iterate();
    while (try iter.next()) |entry| {
        if (entry.kind != .file) continue;
        const name = entry.name;
        if (mem.startsWith(u8, name, "libtree-sitter-") and mem.endsWith(u8, name, lib_ext)) {
            const lang_start = "libtree-sitter-".len;
            const lang_end = name.len - lib_ext.len;
            if (lang_end > lang_start) {
                print("  {s}\n", .{name[lang_start..lang_end]});
                count += 1;
            }
        }
    }

    if (count == 0) {
        print("  (none)\n", .{});
    }

    // List fetched sources
    const sources_dir = try std.fmt.allocPrint(alloc, "{s}/sources", .{grammars_dir});
    defer alloc.free(sources_dir);

    var sources = fs.openDirAbsolute(sources_dir, .{ .iterate = true }) catch {
        return;
    };
    defer sources.close();

    print("\nFetched sources ({s}/sources/):\n", .{grammars_dir});
    var src_count: u32 = 0;
    var src_iter = sources.iterate();
    while (try src_iter.next()) |entry| {
        if (entry.kind == .directory) {
            print("  {s}\n", .{entry.name});
            src_count += 1;
        }
    }
    if (src_count == 0) {
        print("  (none)\n", .{});
    }
}

/// Fetch grammar source from git
fn fetch(alloc: Allocator, name: []const u8) !void {
    const grammars_dir = getGrammarsDir(alloc) catch {
        printErr("error: could not determine grammars directory (HOME not set?)\n", .{});
        process.exit(1);
    };
    defer alloc.free(grammars_dir);

    const sources_dir = try std.fmt.allocPrint(alloc, "{s}/sources", .{grammars_dir});
    defer alloc.free(sources_dir);

    // Create directories
    fs.makeDirAbsolute(grammars_dir) catch |err| {
        if (err != error.PathAlreadyExists) {
            printErr("error: could not create {s}: {s}\n", .{ grammars_dir, @errorName(err) });
            process.exit(1);
        }
    };
    fs.makeDirAbsolute(sources_dir) catch |err| {
        if (err != error.PathAlreadyExists) {
            printErr("error: could not create {s}: {s}\n", .{ sources_dir, @errorName(err) });
            process.exit(1);
        }
    };

    const target_dir = try std.fmt.allocPrint(alloc, "{s}/{s}", .{ sources_dir, name });
    defer alloc.free(target_dir);

    // Default URL
    const git_url = try std.fmt.allocPrint(alloc, "https://github.com/tree-sitter/tree-sitter-{s}", .{name});
    defer alloc.free(git_url);

    validateGitUrl(git_url) catch {
        printErr("error: invalid git URL: {s}\n", .{git_url});
        process.exit(1);
    };

    // Check if already exists
    const exists = pathExists(target_dir);

    if (exists) {
        print("Updating {s}...\n", .{name});
        const exit_code = runCommand(alloc, &.{ "git", "pull", "--ff-only" }, target_dir) catch |err| {
            printErr("error: git pull failed: {s}\n", .{@errorName(err)});
            process.exit(1);
        };
        if (exit_code != 0) {
            printErr("warning: git pull failed (exit {d}), trying fresh clone...\n", .{exit_code});
            fs.deleteTreeAbsolute(target_dir) catch {};
        } else {
            print("Updated {s}\n", .{name});
            return;
        }
    }

    print("Fetching {s} from {s}...\n", .{ name, git_url });
    const exit_code = runCommand(alloc, &.{ "git", "clone", "--depth", "1", git_url, target_dir }, null) catch |err| {
        printErr("error: git clone failed: {s}\n", .{@errorName(err)});
        process.exit(1);
    };

    if (exit_code != 0) {
        printErr("error: git clone failed (exit {d})\n", .{exit_code});
        printErr("Make sure the grammar exists at: {s}\n", .{git_url});
        process.exit(1);
    }

    print("Fetched {s} to {s}\n", .{ name, target_dir });
}

/// Fetch common grammars
fn fetchAll(alloc: Allocator) !void {
    const common = [_][]const u8{
        "rust",     "python",  "javascript", "typescript", "c",
        "cpp",      "go",      "java",       "ruby",       "bash",
        "json",     "yaml",    "toml",       "html",       "css",
        "markdown", "zig",     "lua",        "swift",      "kotlin",
        "elixir",   "haskell", "ocaml",      "scala",      "php",
    };

    var fetched: u32 = 0;
    var failed: u32 = 0;

    for (common) |name| {
        fetch(alloc, name) catch {
            failed += 1;
            continue;
        };
        fetched += 1;
    }

    print("\nFetched {d} grammars", .{fetched});
    if (failed > 0) {
        print(" ({d} failed)", .{failed});
    }
    print("\n", .{});
}

/// Build grammar from source
fn build(alloc: Allocator, name: []const u8) !void {
    const grammars_dir = getGrammarsDir(alloc) catch {
        printErr("error: could not determine grammars directory (HOME not set?)\n", .{});
        process.exit(1);
    };
    defer alloc.free(grammars_dir);

    const sources_dir = try std.fmt.allocPrint(alloc, "{s}/sources", .{grammars_dir});
    defer alloc.free(sources_dir);

    const source_dir = try std.fmt.allocPrint(alloc, "{s}/{s}", .{ sources_dir, name });
    defer alloc.free(source_dir);

    // Check source exists
    fs.accessAbsolute(source_dir, .{}) catch {
        printErr("error: source not found for '{s}'\n", .{name});
        printErr("Run 'mnemos grammar fetch {s}' first\n", .{name});
        process.exit(1);
    };

    // Find src directory (always src_path since we generate into it if needed)
    const src_path = try std.fmt.allocPrint(alloc, "{s}/src", .{source_dir});
    defer alloc.free(src_path);

    const grammar_js = try std.fmt.allocPrint(alloc, "{s}/grammar.js", .{source_dir});
    defer alloc.free(grammar_js);

    if (!pathExists(src_path)) {
        if (pathExists(grammar_js)) {
            // Need to generate parser first
            print("Generating parser for {s}...\n", .{name});
            const exit_code = runCommand(alloc, &.{ "npx", "tree-sitter", "generate" }, source_dir) catch |err| {
                printErr("error: tree-sitter generate failed: {s}\n", .{@errorName(err)});
                printErr("Make sure tree-sitter-cli is installed: npm install -g tree-sitter-cli\n", .{});
                process.exit(1);
            };
            if (exit_code != 0) {
                printErr("error: tree-sitter generate failed (exit {d})\n", .{exit_code});
                printErr("Make sure tree-sitter-cli is installed: npm install -g tree-sitter-cli\n", .{});
                process.exit(1);
            }
        } else {
            printErr("error: cannot find src/ directory or grammar.js in {s}\n", .{source_dir});
            process.exit(1);
        }
    }

    const src_dir = src_path;

    // Check parser.c exists
    const parser_c = try std.fmt.allocPrint(alloc, "{s}/parser.c", .{src_dir});
    defer alloc.free(parser_c);

    fs.accessAbsolute(parser_c, .{}) catch {
        printErr("error: parser.c not found in {s}\n", .{src_dir});
        process.exit(1);
    };

    // Output path
    const lib_ext = if (@import("builtin").os.tag == .macos) "dylib" else "so";
    const output_name = try std.fmt.allocPrint(alloc, "libtree-sitter-{s}.{s}", .{ name, lib_ext });
    defer alloc.free(output_name);

    const output_path = try std.fmt.allocPrint(alloc, "{s}/{s}", .{ grammars_dir, output_name });
    defer alloc.free(output_path);

    // Ensure grammars dir exists
    fs.makeDirAbsolute(grammars_dir) catch |err| {
        if (err != error.PathAlreadyExists) {
            printErr("error: could not create {s}: {s}\n", .{ grammars_dir, @errorName(err) });
            process.exit(1);
        }
    };

    print("Building {s}...\n", .{name});

    // Check for scanner files
    const scanner_c = try std.fmt.allocPrint(alloc, "{s}/scanner.c", .{src_dir});
    defer alloc.free(scanner_c);

    const scanner_cc = try std.fmt.allocPrint(alloc, "{s}/scanner.cc", .{src_dir});
    defer alloc.free(scanner_cc);

    const has_scanner_c = pathExists(scanner_c);
    const has_scanner_cc = pathExists(scanner_cc);

    const include_arg = try std.fmt.allocPrint(alloc, "-I{s}", .{src_dir});
    defer alloc.free(include_arg);

    // Try C compilation first
    if (!has_scanner_cc) {
        var args: std.ArrayListUnmanaged([]const u8) = .empty;
        defer args.deinit(alloc);

        try args.appendSlice(alloc, &.{ "cc", "-shared", "-fPIC", "-O2", include_arg });
        try args.append(alloc, parser_c);
        if (has_scanner_c) {
            try args.append(alloc, scanner_c);
        }
        try args.appendSlice(alloc, &.{ "-o", output_path });

        const exit_code = runCommand(alloc, args.items, null) catch |err| {
            printErr("error: cc failed: {s}\n", .{@errorName(err)});
            process.exit(1);
        };

        if (exit_code == 0) {
            print("Built {s} -> {s}\n", .{ name, output_path });
            return;
        }
    }

    // Try C++ if we have scanner.cc or C failed
    if (has_scanner_cc) {
        var args: std.ArrayListUnmanaged([]const u8) = .empty;
        defer args.deinit(alloc);

        try args.appendSlice(alloc, &.{ "c++", "-shared", "-fPIC", "-O2", include_arg });
        try args.append(alloc, parser_c);
        try args.append(alloc, scanner_cc);
        try args.appendSlice(alloc, &.{ "-o", output_path });

        // Add C++ stdlib
        if (@import("builtin").os.tag == .macos) {
            try args.append(alloc, "-lc++");
        } else {
            try args.append(alloc, "-lstdc++");
        }

        const exit_code = runCommand(alloc, args.items, null) catch |err| {
            printErr("error: c++ failed: {s}\n", .{@errorName(err)});
            process.exit(1);
        };

        if (exit_code != 0) {
            printErr("error: compilation failed for {s} (exit {d})\n", .{ name, exit_code });
            process.exit(1);
        }

        print("Built {s} -> {s}\n", .{ name, output_path });
        return;
    }

    printErr("error: compilation failed for {s}\n", .{name});
    process.exit(1);
}

/// Build all fetched grammars
fn buildAll(alloc: Allocator) !void {
    const grammars_dir = getGrammarsDir(alloc) catch {
        printErr("error: could not determine grammars directory (HOME not set?)\n", .{});
        process.exit(1);
    };
    defer alloc.free(grammars_dir);

    const sources_dir = try std.fmt.allocPrint(alloc, "{s}/sources", .{grammars_dir});
    defer alloc.free(sources_dir);

    var dir = fs.openDirAbsolute(sources_dir, .{ .iterate = true }) catch |err| {
        if (err == error.FileNotFound) {
            print("No grammar sources found. Run 'mnemos grammar fetch --all' first.\n", .{});
            return;
        }
        printErr("error: could not open {s}: {s}\n", .{ sources_dir, @errorName(err) });
        process.exit(1);
    };
    defer dir.close();

    var built: u32 = 0;
    var failed: u32 = 0;

    var iter = dir.iterate();
    while (try iter.next()) |entry| {
        if (entry.kind != .directory) continue;

        build(alloc, entry.name) catch {
            failed += 1;
            continue;
        };
        built += 1;
    }

    print("\nBuilt {d} grammars", .{built});
    if (failed > 0) {
        print(" ({d} failed)", .{failed});
    }
    print("\n", .{});
}

test "validateGitUrl" {
    try validateGitUrl("https://github.com/tree-sitter/tree-sitter-rust");
    try validateGitUrl("git://github.com/tree-sitter/tree-sitter-rust");

    const invalid = validateGitUrl("http://example.com");
    try std.testing.expectError(error.InvalidGitUrl, invalid);

    const shell = validateGitUrl("https://example.com; rm -rf /");
    try std.testing.expectError(error.InvalidGitUrl, shell);
}

test "validateGitRef" {
    try validateGitRef("main");
    try validateGitRef("v1.0.0");
    try validateGitRef("feature/foo");

    const dash = validateGitRef("-flag");
    try std.testing.expectError(error.InvalidGitRef, dash);

    const dots = validateGitRef("foo/../bar");
    try std.testing.expectError(error.InvalidGitRef, dots);
}
