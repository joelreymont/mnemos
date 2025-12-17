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
        var files = std.ArrayList([]const u8).init(alloc);
        errdefer {
            for (files.items) |file| {
                alloc.free(file);
            }
            files.deinit();
        }

        var i: usize = 0;
        while (i < num_deltas) : (i += 1) {
            const delta = c.git_diff_get_delta(diff.?, i);
            if (delta == null) continue;

            const new_file = delta.*.new_file;
            const path = new_file.path;
            if (path != null) {
                const path_str = mem.span(path);
                try files.append(try alloc.dupe(u8, path_str));
            }
        }

        return files.toOwnedSlice();
    }

    pub fn getFileDiff(self: *Repository, alloc: Allocator, path: []const u8) ![]const u8 {
        var diff: ?*c.git_diff = null;
        var opts: c.git_diff_options = undefined;
        _ = c.git_diff_options_init(&opts, c.GIT_DIFF_OPTIONS_VERSION);

        // Set pathspec to filter for specific file
        const pathspec: [*c]const [*c]const u8 = @ptrCast(&path.ptr);
        opts.pathspec.strings = pathspec;
        opts.pathspec.count = 1;

        const result = c.git_diff_index_to_workdir(&diff, self.repo, null, &opts);
        if (result < 0 or diff == null) {
            return GitError.DiffFailed;
        }
        defer c.git_diff_free(diff);

        var output = std.ArrayList(u8).init(alloc);
        errdefer output.deinit();

        const PrintContext = struct {
            list: *std.ArrayList(u8),
        };

        var ctx = PrintContext{ .list = &output };

        const print_cb = struct {
            fn callback(
                delta: [*c]const c.git_diff_delta,
                hunk: [*c]const c.git_diff_hunk,
                line: [*c]const c.git_diff_line,
                payload: ?*anyopaque,
            ) callconv(.C) c_int {
                _ = delta;
                _ = hunk;
                const context: *PrintContext = @ptrCast(@alignCast(payload));
                if (line != null) {
                    const content = line.*.content[0..@intCast(line.*.content_len)];
                    context.list.appendSlice(content) catch return -1;
                }
                return 0;
            }
        }.callback;

        _ = c.git_diff_print(diff.?, c.GIT_DIFF_FORMAT_PATCH, print_cb, &ctx);

        return output.toOwnedSlice();
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
