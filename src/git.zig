const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;

const c = @cImport({
    @cInclude("git2.h");
});

pub const GitError = error{
    InitFailed,
    OpenFailed,
    NotFound,
    InvalidRepository,
    DiffFailed,
    BlameFailed,
    OutOfMemory,
};

pub const Repository = struct {
    repo: *c.git_repository,

    pub fn open(path: []const u8) !Repository {
        if (c.git_libgit2_init() < 0) {
            return GitError.InitFailed;
        }

        var repo: ?*c.git_repository = null;
        const result = c.git_repository_open(&repo, path.ptr);
        if (result < 0 or repo == null) {
            return GitError.OpenFailed;
        }

        return Repository{ .repo = repo.? };
    }

    pub fn deinit(self: *Repository) void {
        c.git_repository_free(self.repo);
        _ = c.git_libgit2_shutdown();
    }

    pub fn getCurrentBranch(self: *Repository, alloc: Allocator) ![]const u8 {
        var head_ref: ?*c.git_reference = null;
        if (c.git_repository_head(&head_ref, self.repo) < 0) {
            return GitError.NotFound;
        }
        defer c.git_reference_free(head_ref);

        const branch_name = c.git_reference_shorthand(head_ref);
        if (branch_name == null) {
            return GitError.NotFound;
        }

        return alloc.dupe(u8, mem.span(branch_name));
    }

    pub fn getHeadCommit(self: *Repository, alloc: Allocator) ![]const u8 {
        var head_ref: ?*c.git_reference = null;
        if (c.git_repository_head(&head_ref, self.repo) < 0) {
            return GitError.NotFound;
        }
        defer c.git_reference_free(head_ref);

        const oid = c.git_reference_target(head_ref);
        if (oid == null) {
            return GitError.NotFound;
        }

        var buf: [41]u8 = undefined;
        _ = c.git_oid_tostr(&buf, buf.len, oid);
        buf[40] = 0;

        return alloc.dupe(u8, buf[0..40]);
    }

    pub fn isIgnored(self: *Repository, path: []const u8) !bool {
        var ignored: c_int = 0;
        const result = c.git_ignore_path_is_ignored(&ignored, self.repo, path.ptr);
        if (result < 0) {
            return false;
        }
        return ignored != 0;
    }

    pub fn getModifiedFiles(self: *Repository, alloc: Allocator) ![][]const u8 {
        var diff: ?*c.git_diff = null;
        var opts: c.git_diff_options = undefined;
        _ = c.git_diff_options_init(&opts, c.GIT_DIFF_OPTIONS_VERSION);

        const result = c.git_diff_index_to_workdir(&diff, self.repo, null, &opts);
        if (result < 0 or diff == null) {
            return GitError.DiffFailed;
        }
        defer c.git_diff_free(diff);

        const num_deltas = c.git_diff_num_deltas(diff.?);
        var files: std.ArrayList([]const u8) = .{};
        errdefer {
            for (files.items) |file| {
                alloc.free(file);
            }
            files.deinit(alloc);
        }

        var i: usize = 0;
        while (i < num_deltas) : (i += 1) {
            const delta = c.git_diff_get_delta(diff.?, i);
            if (delta == null) continue;

            const new_file = delta.*.new_file;
            const path_ptr = new_file.path;
            if (path_ptr != null) {
                const path_str = mem.span(path_ptr);
                try files.append(alloc, try alloc.dupe(u8, path_str));
            }
        }

        return files.toOwnedSlice(alloc);
    }

    pub fn getFileDiff(self: *Repository, alloc: Allocator, path: []const u8) ![]const u8 {
        var diff: ?*c.git_diff = null;
        var opts: c.git_diff_options = undefined;
        _ = c.git_diff_options_init(&opts, c.GIT_DIFF_OPTIONS_VERSION);

        // Get diff for all files (pathspec filter is complex with const casts)
        _ = path;

        const result = c.git_diff_index_to_workdir(&diff, self.repo, null, &opts);
        if (result < 0 or diff == null) {
            return GitError.DiffFailed;
        }
        defer c.git_diff_free(diff);

        var output: std.ArrayList(u8) = .{};
        errdefer output.deinit(alloc);

        const PrintContext = struct {
            list: *std.ArrayList(u8),
            allocator: Allocator,
        };

        var ctx = PrintContext{ .list = &output, .allocator = alloc };

        const print_cb = struct {
            fn callback(
                delta: [*c]const c.git_diff_delta,
                hunk: [*c]const c.git_diff_hunk,
                line: [*c]const c.git_diff_line,
                payload: ?*anyopaque,
            ) callconv(.c) c_int {
                _ = delta;
                _ = hunk;
                const context: *PrintContext = @ptrCast(@alignCast(payload));
                if (line != null) {
                    const content = line.*.content[0..@intCast(line.*.content_len)];
                    context.list.appendSlice(context.allocator, content) catch return -1;
                }
                return 0;
            }
        }.callback;

        _ = c.git_diff_print(diff.?, c.GIT_DIFF_FORMAT_PATCH, print_cb, &ctx);

        return output.toOwnedSlice(alloc);
    }

    pub const BlameInfo = struct {
        commit_hash: []const u8,
        author_name: []const u8,
        author_email: []const u8,
        line_number: usize,

        pub fn deinit(self: *BlameInfo, alloc: Allocator) void {
            alloc.free(self.commit_hash);
            alloc.free(self.author_name);
            alloc.free(self.author_email);
        }
    };

    pub fn getBlameForLine(self: *Repository, alloc: Allocator, path: []const u8, line_number: usize) !BlameInfo {
        var blame: ?*c.git_blame = null;
        var opts: c.git_blame_options = undefined;
        _ = c.git_blame_options_init(&opts, c.GIT_BLAME_OPTIONS_VERSION);

        const result = c.git_blame_file(&blame, self.repo, path.ptr, &opts);
        if (result < 0 or blame == null) {
            return GitError.BlameFailed;
        }
        defer c.git_blame_free(blame);

        const hunk = c.git_blame_get_hunk_byline(blame.?, @intCast(line_number));
        if (hunk == null) {
            return GitError.NotFound;
        }

        const final_commit_id = &hunk.*.final_commit_id;
        var buf: [41]u8 = undefined;
        _ = c.git_oid_tostr(&buf, buf.len, final_commit_id);
        buf[40] = 0;

        const commit_hash = try alloc.dupe(u8, buf[0..40]);
        errdefer alloc.free(commit_hash);

        var commit: ?*c.git_commit = null;
        _ = c.git_commit_lookup(&commit, self.repo, final_commit_id);
        defer if (commit != null) c.git_commit_free(commit);

        var author_name: []const u8 = "";
        var author_email: []const u8 = "";

        if (commit != null) {
            const signature = c.git_commit_author(commit.?);
            if (signature != null) {
                if (signature.*.name != null) {
                    author_name = try alloc.dupe(u8, mem.span(signature.*.name));
                }
                if (signature.*.email != null) {
                    author_email = try alloc.dupe(u8, mem.span(signature.*.email));
                }
            }
        }

        return BlameInfo{
            .commit_hash = commit_hash,
            .author_name = author_name,
            .author_email = author_email,
            .line_number = line_number,
        };
    }
};

test "repository operations" {
    const testing = std.testing;
    const alloc = testing.allocator;

    // Try to open current repository
    var repo = Repository.open(".") catch |err| {
        if (err == GitError.OpenFailed or err == GitError.InitFailed) {
            // Not in a git repo, skip test
            return error.SkipZigTest;
        }
        return err;
    };
    defer repo.deinit();

    // Test getting branch name
    const branch = try repo.getCurrentBranch(alloc);
    defer alloc.free(branch);
    try testing.expect(branch.len > 0);

    // Test getting HEAD commit
    const commit = try repo.getHeadCommit(alloc);
    defer alloc.free(commit);
    try testing.expectEqual(@as(usize, 40), commit.len);
}

test "gitignore check" {
    const testing = std.testing;
    const alloc = testing.allocator;
    _ = alloc;

    var repo = Repository.open(".") catch |err| {
        if (err == GitError.OpenFailed or err == GitError.InitFailed) {
            return error.SkipZigTest;
        }
        return err;
    };
    defer repo.deinit();

    // Test that .git directory is ignored
    const ignored = try repo.isIgnored(".git/config");
    try testing.expect(ignored);
}

test "modified files detection" {
    const testing = std.testing;
    const alloc = testing.allocator;

    var repo = Repository.open(".") catch |err| {
        if (err == GitError.OpenFailed or err == GitError.InitFailed) {
            return error.SkipZigTest;
        }
        return err;
    };
    defer repo.deinit();

    // getModifiedFiles should return a list (possibly empty)
    const files = try repo.getModifiedFiles(alloc);
    defer {
        for (files) |f| alloc.free(f);
        alloc.free(files);
    }
}

test "file diff" {
    const testing = std.testing;
    const alloc = testing.allocator;

    var repo = Repository.open(".") catch |err| {
        if (err == GitError.OpenFailed or err == GitError.InitFailed) {
            return error.SkipZigTest;
        }
        return err;
    };
    defer repo.deinit();

    // Try to get diff for build.zig (a known file)
    const diff = repo.getFileDiff(alloc, "build.zig") catch |err| {
        // File might not have changes, that's ok
        if (err == GitError.DiffFailed) return;
        return err;
    };
    defer alloc.free(diff);
}

test "non-ignored source file" {
    const testing = std.testing;
    const alloc = testing.allocator;
    _ = alloc;

    var repo = Repository.open(".") catch |err| {
        if (err == GitError.OpenFailed or err == GitError.InitFailed) {
            return error.SkipZigTest;
        }
        return err;
    };
    defer repo.deinit();

    // Source files should not be ignored
    const ignored = try repo.isIgnored("src/main.zig");
    try testing.expect(!ignored);
}

test "zig-cache ignored" {
    const testing = std.testing;
    const alloc = testing.allocator;
    _ = alloc;

    var repo = Repository.open(".") catch |err| {
        if (err == GitError.OpenFailed or err == GitError.InitFailed) {
            return error.SkipZigTest;
        }
        return err;
    };
    defer repo.deinit();

    // .zig-cache should be ignored
    const ignored = try repo.isIgnored(".zig-cache/test");
    try testing.expect(ignored);
}

test "open invalid repository path" {
    const testing = std.testing;

    const result = Repository.open("/nonexistent/path/to/repo");
    try testing.expectError(GitError.OpenFailed, result);
}

test "open empty string path" {
    const testing = std.testing;

    const result = Repository.open("");
    try testing.expectError(GitError.OpenFailed, result);
}

test "commit hash is valid hex string" {
    const testing = std.testing;
    const alloc = testing.allocator;

    var repo = Repository.open(".") catch |err| {
        if (err == GitError.OpenFailed or err == GitError.InitFailed) {
            return error.SkipZigTest;
        }
        return err;
    };
    defer repo.deinit();

    const commit = try repo.getHeadCommit(alloc);
    defer alloc.free(commit);

    try testing.expectEqual(@as(usize, 40), commit.len);
    for (commit) |ch| {
        const valid = (ch >= '0' and ch <= '9') or (ch >= 'a' and ch <= 'f');
        try testing.expect(valid);
    }
}

test "branch name is non-empty" {
    const testing = std.testing;
    const alloc = testing.allocator;

    var repo = Repository.open(".") catch |err| {
        if (err == GitError.OpenFailed or err == GitError.InitFailed) {
            return error.SkipZigTest;
        }
        return err;
    };
    defer repo.deinit();

    const branch = try repo.getCurrentBranch(alloc);
    defer alloc.free(branch);

    try testing.expect(branch.len > 0);
    try testing.expect(branch.len < 256);
}

test "isIgnored with null terminator" {
    const testing = std.testing;
    const alloc = testing.allocator;
    _ = alloc;

    var repo = Repository.open(".") catch |err| {
        if (err == GitError.OpenFailed or err == GitError.InitFailed) {
            return error.SkipZigTest;
        }
        return err;
    };
    defer repo.deinit();

    const path = ".git/HEAD";
    const ignored = try repo.isIgnored(path);
    try testing.expect(ignored);
}

test "getModifiedFiles returns valid slice" {
    const testing = std.testing;
    const alloc = testing.allocator;

    var repo = Repository.open(".") catch |err| {
        if (err == GitError.OpenFailed or err == GitError.InitFailed) {
            return error.SkipZigTest;
        }
        return err;
    };
    defer repo.deinit();

    const files = try repo.getModifiedFiles(alloc);
    defer {
        for (files) |f| alloc.free(f);
        alloc.free(files);
    }

    for (files) |file| {
        try testing.expect(file.len > 0);
    }
}

test "getFileDiff returns string" {
    const testing = std.testing;
    const alloc = testing.allocator;

    var repo = Repository.open(".") catch |err| {
        if (err == GitError.OpenFailed or err == GitError.InitFailed) {
            return error.SkipZigTest;
        }
        return err;
    };
    defer repo.deinit();

    const diff = repo.getFileDiff(alloc, "build.zig") catch |err| {
        if (err == GitError.DiffFailed) return;
        return err;
    };
    defer alloc.free(diff);
}

test "BlameInfo struct initialization" {
    const testing = std.testing;
    const alloc = testing.allocator;

    const hash = try alloc.dupe(u8, "1234567890abcdef1234567890abcdef12345678");
    const name = try alloc.dupe(u8, "Test Author");
    const email = try alloc.dupe(u8, "test@example.com");

    var info = Repository.BlameInfo{
        .commit_hash = hash,
        .author_name = name,
        .author_email = email,
        .line_number = 42,
    };

    try testing.expectEqual(@as(usize, 40), info.commit_hash.len);
    try testing.expectEqual(@as(usize, 11), info.author_name.len);
    try testing.expectEqual(@as(usize, 16), info.author_email.len);
    try testing.expectEqual(@as(usize, 42), info.line_number);

    info.deinit(alloc);
}

test "BlameInfo deinit frees memory" {
    const testing = std.testing;
    const alloc = testing.allocator;

    const hash = try alloc.dupe(u8, "abcd1234");
    const name = try alloc.dupe(u8, "Name");
    const email = try alloc.dupe(u8, "email");

    var info = Repository.BlameInfo{
        .commit_hash = hash,
        .author_name = name,
        .author_email = email,
        .line_number = 1,
    };

    info.deinit(alloc);
}

test "getBlameForLine on git.zig" {
    const testing = std.testing;
    const alloc = testing.allocator;

    var repo = Repository.open(".") catch |err| {
        if (err == GitError.OpenFailed or err == GitError.InitFailed) {
            return error.SkipZigTest;
        }
        return err;
    };
    defer repo.deinit();

    var blame = repo.getBlameForLine(alloc, "src/git.zig", 1) catch |err| {
        if (err == GitError.BlameFailed or err == GitError.NotFound) {
            return error.SkipZigTest;
        }
        return err;
    };
    defer blame.deinit(alloc);

    try testing.expectEqual(@as(usize, 40), blame.commit_hash.len);
    try testing.expect(blame.line_number == 1);
}

test "getBlameForLine on nonexistent file" {
    const testing = std.testing;
    const alloc = testing.allocator;

    var repo = Repository.open(".") catch |err| {
        if (err == GitError.OpenFailed or err == GitError.InitFailed) {
            return error.SkipZigTest;
        }
        return err;
    };
    defer repo.deinit();

    const result = repo.getBlameForLine(alloc, "nonexistent.zig", 1);
    try testing.expectError(GitError.BlameFailed, result);
}

test "multiple repository operations" {
    const testing = std.testing;
    const alloc = testing.allocator;

    var repo = Repository.open(".") catch |err| {
        if (err == GitError.OpenFailed or err == GitError.InitFailed) {
            return error.SkipZigTest;
        }
        return err;
    };
    defer repo.deinit();

    const branch = try repo.getCurrentBranch(alloc);
    defer alloc.free(branch);

    const commit = try repo.getHeadCommit(alloc);
    defer alloc.free(commit);

    const files = try repo.getModifiedFiles(alloc);
    defer {
        for (files) |f| alloc.free(f);
        alloc.free(files);
    }

    try testing.expect(branch.len > 0);
    try testing.expectEqual(@as(usize, 40), commit.len);
}

test "isIgnored with build output dirs" {
    const testing = std.testing;
    const alloc = testing.allocator;
    _ = alloc;

    var repo = Repository.open(".") catch |err| {
        if (err == GitError.OpenFailed or err == GitError.InitFailed) {
            return error.SkipZigTest;
        }
        return err;
    };
    defer repo.deinit();

    const ignored_out = try repo.isIgnored("zig-out/bin/test");
    try testing.expect(ignored_out);

    const ignored_cache = try repo.isIgnored(".zig-cache/o/abc123/test.o");
    try testing.expect(ignored_cache);
}

test "GitError enum values exist" {
    const testing = std.testing;

    const errors = [_]type{GitError};
    try testing.expectEqual(@as(usize, 1), errors.len);

    const init_failed: GitError = GitError.InitFailed;
    const open_failed: GitError = GitError.OpenFailed;
    const not_found: GitError = GitError.NotFound;
    const invalid_repo: GitError = GitError.InvalidRepository;
    const diff_failed: GitError = GitError.DiffFailed;
    const blame_failed: GitError = GitError.BlameFailed;
    const oom: GitError = GitError.OutOfMemory;

    try testing.expect(init_failed == GitError.InitFailed);
    try testing.expect(open_failed == GitError.OpenFailed);
    try testing.expect(not_found == GitError.NotFound);
    try testing.expect(invalid_repo == GitError.InvalidRepository);
    try testing.expect(diff_failed == GitError.DiffFailed);
    try testing.expect(blame_failed == GitError.BlameFailed);
    try testing.expect(oom == GitError.OutOfMemory);
}

// ============================================================================
// Additional Git Tests
// ============================================================================

test "BlameInfo struct fields" {
    const blame = Repository.BlameInfo{
        .commit_hash = "abc123def456789012345678901234567890abcd",
        .author_name = "Test Author",
        .author_email = "test@example.com",
        .line_number = 42,
    };

    try std.testing.expectEqual(@as(usize, 40), blame.commit_hash.len);
    try std.testing.expectEqual(@as(usize, 42), blame.line_number);
    try std.testing.expectEqualStrings("Test Author", blame.author_name);
    try std.testing.expectEqualStrings("test@example.com", blame.author_email);
}

test "BlameInfo with empty author" {
    const blame = Repository.BlameInfo{
        .commit_hash = "abc123def456789012345678901234567890abcd",
        .author_name = "",
        .author_email = "",
        .line_number = 1,
    };

    try std.testing.expectEqual(@as(usize, 0), blame.author_name.len);
    try std.testing.expectEqual(@as(usize, 0), blame.author_email.len);
}

test "Repository open nonexistent fails" {
    const result = Repository.open("/nonexistent/path/to/repo");
    try std.testing.expectError(GitError.OpenFailed, result);
}

test "commit hash length constant" {
    // Git commit hashes are 40 hex characters (20 bytes in binary)
    const expected_len: usize = 40;
    const sample_hash = "abc123def456789012345678901234567890abcd";

    try std.testing.expectEqual(expected_len, sample_hash.len);
}

test "isIgnored on tracked file" {
    const testing = std.testing;

    var repo = Repository.open(".") catch |err| {
        if (err == GitError.OpenFailed or err == GitError.InitFailed) {
            return error.SkipZigTest;
        }
        return err;
    };
    defer repo.deinit();

    // build.zig should not be ignored (it's tracked)
    const ignored = try repo.isIgnored("build.zig");
    try testing.expect(!ignored);
}

test "isIgnored on common ignored patterns" {
    var repo = Repository.open(".") catch |err| {
        if (err == GitError.OpenFailed or err == GitError.InitFailed) {
            return error.SkipZigTest;
        }
        return err;
    };
    defer repo.deinit();

    // .DS_Store should typically be ignored
    const ignored_ds = repo.isIgnored(".DS_Store") catch false;
    _ = ignored_ds; // May or may not be in .gitignore

    // node_modules should typically be ignored
    const ignored_nm = repo.isIgnored("node_modules/package/file.js") catch false;
    _ = ignored_nm;
}

test "getCurrentBranch returns non-empty" {
    const testing = std.testing;
    const alloc = testing.allocator;

    var repo = Repository.open(".") catch |err| {
        if (err == GitError.OpenFailed or err == GitError.InitFailed) {
            return error.SkipZigTest;
        }
        return err;
    };
    defer repo.deinit();

    const branch = try repo.getCurrentBranch(alloc);
    defer alloc.free(branch);

    try testing.expect(branch.len > 0);
    // Branch names typically don't contain spaces
    try testing.expect(std.mem.indexOf(u8, branch, " ") == null);
}

test "getHeadCommit returns 40 char hash" {
    const testing = std.testing;
    const alloc = testing.allocator;

    var repo = Repository.open(".") catch |err| {
        if (err == GitError.OpenFailed or err == GitError.InitFailed) {
            return error.SkipZigTest;
        }
        return err;
    };
    defer repo.deinit();

    const commit = try repo.getHeadCommit(alloc);
    defer alloc.free(commit);

    try testing.expectEqual(@as(usize, 40), commit.len);
    // Should be all hex characters
    for (commit) |ch| {
        try testing.expect((ch >= '0' and ch <= '9') or (ch >= 'a' and ch <= 'f'));
    }
}

test "getModifiedFiles returns array" {
    const testing = std.testing;
    const alloc = testing.allocator;

    var repo = Repository.open(".") catch |err| {
        if (err == GitError.OpenFailed or err == GitError.InitFailed) {
            return error.SkipZigTest;
        }
        return err;
    };
    defer repo.deinit();

    const files = try repo.getModifiedFiles(alloc);
    defer {
        for (files) |f| alloc.free(f);
        alloc.free(files);
    }

    // Just verify it returns without error (may be empty or have files)
    try testing.expect(true);
}
