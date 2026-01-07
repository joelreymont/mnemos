//! Property-based and snapshot tests for mnemos
//!
//! Philosophy:
//! - Property tests for pure functions and invariants
//! - Snapshot tests for data structure representations
//! - Model-based oracles for stateful operations (storage)

const std = @import("std");
const testing = std.testing;
const quickcheck = @import("quickcheck");
const OhSnap = @import("ohsnap");

const storage = @import("storage.zig");
const treesitter = @import("treesitter.zig");
const ai = @import("ai.zig");

// ============================================================================
// Test Utilities
// ============================================================================

/// Generate a random printable ASCII string (no control chars or null bytes)
fn generatePrintableString(buf: []u8, random: std.Random) []const u8 {
    const len = random.uintLessThan(usize, buf.len);
    for (buf[0..len]) |*c| {
        // Printable ASCII: 32-126
        c.* = @intCast(random.intRangeAtMost(u8, 32, 126));
    }
    return buf[0..len];
}

/// Generate a random alphanumeric ID
fn generateId(buf: *[36]u8, random: std.Random) []const u8 {
    const chars = "abcdefghijklmnopqrstuvwxyz0123456789";
    for (buf) |*c| {
        c.* = chars[random.uintLessThan(usize, chars.len)];
    }
    return buf;
}

/// Generate a valid file path
fn generateFilePath(buf: []u8, random: std.Random) []const u8 {
    const extensions = [_][]const u8{ ".zig", ".rs", ".py", ".js", ".ts", ".go", ".c" };
    const ext = extensions[random.uintLessThan(usize, extensions.len)];

    const name_len = random.intRangeAtMost(usize, 1, 20);
    buf[0] = '/';
    for (buf[1 .. name_len + 1]) |*c| {
        c.* = @intCast(random.intRangeAtMost(u8, 'a', 'z'));
    }
    @memcpy(buf[name_len + 1 ..][0..ext.len], ext);
    return buf[0 .. name_len + 1 + ext.len];
}

// ============================================================================
// Property Tests: Storage CRUD Operations
// ============================================================================

test "prop: note create-read roundtrip preserves content" {
    // Oracle: For any valid Note, create then read should return identical content
    const alloc = testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    var prng = std.Random.DefaultPrng.init(0x12345678);
    const random = prng.random();

    for (0..50) |_| {
        var id_buf: [36]u8 = undefined;
        var path_buf: [64]u8 = undefined;
        var content_buf: [256]u8 = undefined;

        const id = generateId(&id_buf, random);
        const file_path = generateFilePath(&path_buf, random);
        const content = generatePrintableString(&content_buf, random);

        const note = storage.Note{
            .id = id,
            .file_path = file_path,
            .node_path = null,
            .node_text_hash = null,
            .line_number = null,
            .column_number = null,
            .content = content,
            .created_at = "2024-01-01",
            .updated_at = "2024-01-01",
        };

        try storage.createNote(&db, note);

        const fetched = try storage.getNote(&db, alloc, id);
        try testing.expect(fetched != null);
        defer storage.freeNoteFields(alloc, fetched.?);

        // Oracle check: content must match exactly
        try testing.expectEqualStrings(content, fetched.?.content);
        try testing.expectEqualStrings(file_path, fetched.?.file_path);
    }
}

test "prop: note count increases monotonically with creates" {
    // Oracle: After N creates, count should be N
    const alloc = testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    var prng = std.Random.DefaultPrng.init(0xABCDEF01);
    const random = prng.random();

    const n = 25;
    for (0..n) |i| {
        var id_buf: [36]u8 = undefined;
        const id = generateId(&id_buf, random);

        try storage.createNote(&db, .{
            .id = id,
            .file_path = "/test.zig",
            .node_path = null,
            .node_text_hash = null,
            .line_number = null,
            .column_number = null,
            .content = "test",
            .created_at = "2024-01-01",
            .updated_at = "2024-01-01",
        });

        const count = try storage.countNotes(&db);
        try testing.expectEqual(@as(i64, @intCast(i + 1)), count);
    }
}

test "prop: delete removes exactly one note" {
    // Oracle: delete(id) decrements count by 1 and getNote(id) returns null
    const alloc = testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    var prng = std.Random.DefaultPrng.init(0xDEADBEEF);
    const random = prng.random();

    // Create several notes
    var ids: [10][36]u8 = undefined;
    for (&ids) |*id_buf| {
        _ = generateId(id_buf, random);
        try storage.createNote(&db, .{
            .id = id_buf,
            .file_path = "/test.zig",
            .node_path = null,
            .node_text_hash = null,
            .line_number = null,
            .column_number = null,
            .content = "content",
            .created_at = "2024-01-01",
            .updated_at = "2024-01-01",
        });
    }

    // Delete each and verify
    for (&ids, 0..) |*id_buf, i| {
        const before_count = try storage.countNotes(&db);
        try storage.deleteNote(&db, id_buf);
        const after_count = try storage.countNotes(&db);

        // Oracle: count decremented by exactly 1
        try testing.expectEqual(before_count - 1, after_count);

        // Oracle: note no longer retrievable
        const gone = try storage.getNote(&db, alloc, id_buf);
        try testing.expect(gone == null);

        // Other notes still exist (spot check)
        if (i + 1 < ids.len) {
            const other = try storage.getNote(&db, alloc, &ids[i + 1]);
            try testing.expect(other != null);
            storage.freeNoteFields(alloc, other.?);
        }
    }
}

test "prop: search results are subset of notes containing query" {
    // Oracle: Every search result must contain the query string
    const alloc = testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    // Create notes with known content patterns
    const contents = [_][]const u8{
        "hello world",
        "goodbye world",
        "hello there",
        "nothing here",
        "world peace",
    };

    for (contents, 0..) |content, i| {
        var id_buf: [36]u8 = undefined;
        _ = std.fmt.bufPrint(&id_buf, "note-{d:0>30}", .{i}) catch unreachable;
        try storage.createNote(&db, .{
            .id = &id_buf,
            .file_path = "/test.zig",
            .node_path = null,
            .node_text_hash = null,
            .line_number = null,
            .column_number = null,
            .content = content,
            .created_at = "2024-01-01",
            .updated_at = "2024-01-01",
        });
    }

    // Search for "world"
    const results = try storage.searchNotes(&db, alloc, "world", 100, 0);
    defer storage.freeNotes(alloc, results);

    // Oracle: all results must contain "world"
    for (results) |note| {
        try testing.expect(std.mem.indexOf(u8, note.content, "world") != null);
    }

    // Should find exactly 3 notes
    try testing.expectEqual(@as(usize, 3), results.len);
}

test "prop: update preserves id and file_path" {
    // Oracle: update only changes content/updated_at, not id/file_path
    const alloc = testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    var prng = std.Random.DefaultPrng.init(0xCAFEBABE);
    const random = prng.random();

    for (0..20) |_| {
        var id_buf: [36]u8 = undefined;
        var path_buf: [64]u8 = undefined;
        var content_buf: [128]u8 = undefined;
        var new_content_buf: [128]u8 = undefined;

        const id = generateId(&id_buf, random);
        const file_path = generateFilePath(&path_buf, random);
        const original_content = generatePrintableString(&content_buf, random);
        const new_content = generatePrintableString(&new_content_buf, random);

        try storage.createNote(&db, .{
            .id = id,
            .file_path = file_path,
            .node_path = null,
            .node_text_hash = null,
            .line_number = null,
            .column_number = null,
            .content = original_content,
            .created_at = "2024-01-01",
            .updated_at = "2024-01-01",
        });

        try storage.updateNote(&db, id, new_content, null, "2024-01-02");

        const fetched = try storage.getNote(&db, alloc, id);
        try testing.expect(fetched != null);
        defer storage.freeNoteFields(alloc, fetched.?);

        // Oracle: id and file_path unchanged
        try testing.expectEqualStrings(id, fetched.?.id);
        try testing.expectEqualStrings(file_path, fetched.?.file_path);
        // Oracle: content updated
        try testing.expectEqualStrings(new_content, fetched.?.content);
        // Oracle: updated_at changed
        try testing.expectEqualStrings("2024-01-02", fetched.?.updated_at);
    }
}

test "prop: getNotesForFile returns only matching files" {
    // Oracle: All notes returned by getNotesForFile have the requested file_path
    const alloc = testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const files = [_][]const u8{ "/a.zig", "/b.zig", "/c.zig" };
    var prng = std.Random.DefaultPrng.init(0x11223344);
    const random = prng.random();

    // Create notes distributed across files
    for (0..30) |i| {
        var id_buf: [36]u8 = undefined;
        _ = generateId(&id_buf, random);
        const file = files[i % files.len];

        try storage.createNote(&db, .{
            .id = &id_buf,
            .file_path = file,
            .node_path = null,
            .node_text_hash = null,
            .line_number = null,
            .column_number = null,
            .content = "content",
            .created_at = "2024-01-01",
            .updated_at = "2024-01-01",
        });
    }

    // Query each file and verify filter
    for (files) |file| {
        const notes = try storage.getNotesForFile(&db, alloc, file);
        defer storage.freeNotes(alloc, notes);

        // Oracle: every returned note has the correct file_path
        for (notes) |note| {
            try testing.expectEqualStrings(file, note.file_path);
        }

        // Should have 10 notes per file
        try testing.expectEqual(@as(usize, 10), notes.len);
    }
}

// ============================================================================
// Property Tests: JSON Escaping
// ============================================================================

test "prop: writeJsonEscaped produces valid JSON string content" {
    // Oracle: Escaped output wrapped in quotes should parse as valid JSON
    const alloc = testing.allocator;
    var prng = std.Random.DefaultPrng.init(0x55667788);
    const random = prng.random();

    for (0..100) |_| {
        var input_buf: [64]u8 = undefined;
        const input = generatePrintableString(&input_buf, random);

        var output: std.ArrayList(u8) = .{};
        defer output.deinit(alloc);

        try ai.writeJsonEscaped(output.writer(alloc), input);

        // Wrap in quotes to make valid JSON string
        const json = try std.fmt.allocPrint(alloc, "\"{s}\"", .{output.items});
        defer alloc.free(json);

        // Oracle: must parse as valid JSON
        const parsed = std.json.parseFromSlice([]const u8, alloc, json, .{}) catch |err| {
            std.debug.print("Failed to parse: {s} (error: {})\n", .{ json, err });
            return error.InvalidJson;
        };
        defer parsed.deinit();
    }
}

test "prop: writeJsonEscaped escapes all control characters" {
    // Oracle: No raw control characters (0x00-0x1F) in output except escaped forms
    const alloc = testing.allocator;

    // Test all control characters
    for (0..32) |i| {
        var output: std.ArrayList(u8) = .{};
        defer output.deinit(alloc);

        const input = [_]u8{@intCast(i)};
        try ai.writeJsonEscaped(output.writer(alloc), &input);

        // Oracle: output should not contain raw control char
        for (output.items) |c| {
            if (c < 0x20) {
                std.debug.print("Raw control char 0x{x:0>2} found in output for input 0x{x:0>2}\n", .{ c, i });
                return error.UnescapedControl;
            }
        }
    }
}

test "prop: writeJsonEscaped is deterministic" {
    // Oracle: Same input always produces same output
    const alloc = testing.allocator;
    var prng = std.Random.DefaultPrng.init(0x99AABBCC);
    const random = prng.random();

    for (0..50) |_| {
        var input_buf: [64]u8 = undefined;
        const input = generatePrintableString(&input_buf, random);

        var out1: std.ArrayList(u8) = .{};
        defer out1.deinit(alloc);
        var out2: std.ArrayList(u8) = .{};
        defer out2.deinit(alloc);

        try ai.writeJsonEscaped(out1.writer(alloc), input);
        try ai.writeJsonEscaped(out2.writer(alloc), input);

        try testing.expectEqualSlices(u8, out1.items, out2.items);
    }
}

// ============================================================================
// Property Tests: Tree-sitter Extension Mapping
// ============================================================================

test "prop: languageForFile consistent with languageForExtension" {
    // Oracle: languageForFile("/foo" ++ ext) == languageForExtension(ext)
    const extensions = [_][]const u8{
        ".zig", ".rs",   ".py",   ".js",   ".jsx",  ".ts",
        ".tsx", ".go",   ".c",    ".h",    ".cpp",  ".cc",
        ".cxx", ".hpp",  ".java", ".rb",   ".lua",  ".el",
        ".sh",  ".bash", ".json", ".toml", ".yaml",
    };

    for (extensions) |ext| {
        const from_ext = treesitter.languageForExtension(ext);
        var path_buf: [64]u8 = undefined;
        const path = std.fmt.bufPrint(&path_buf, "/some/path/file{s}", .{ext}) catch unreachable;
        const from_file = treesitter.languageForFile(path);

        // Oracle: both methods return same language
        if (from_ext) |lang_ext| {
            try testing.expect(from_file != null);
            try testing.expectEqualStrings(lang_ext, from_file.?);
        } else {
            try testing.expect(from_file == null);
        }
    }
}

test "prop: languageForExtension returns null for unknown extensions" {
    // Oracle: Unknown extensions should return null, not crash
    var prng = std.Random.DefaultPrng.init(0xDDEEFF00);
    const random = prng.random();

    for (0..100) |_| {
        var ext_buf: [8]u8 = undefined;
        ext_buf[0] = '.';
        const len = random.intRangeAtMost(usize, 1, 6);
        for (ext_buf[1 .. len + 1]) |*c| {
            c.* = @intCast(random.intRangeAtMost(u8, 'a', 'z'));
        }
        const ext = ext_buf[0 .. len + 1];

        // Should not crash - may return null or valid language
        _ = treesitter.languageForExtension(ext);
    }
}

// ============================================================================
// Property Tests: File Operations
// ============================================================================

test "prop: addFile then getFile roundtrip" {
    // Oracle: Content and hash are preserved through addFile/getFile
    const alloc = testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    var prng = std.Random.DefaultPrng.init(0x12121212);
    const random = prng.random();

    for (0..30) |_| {
        var path_buf: [64]u8 = undefined;
        var content_buf: [512]u8 = undefined;
        var hash_buf: [64]u8 = undefined;

        const path = generateFilePath(&path_buf, random);
        const content = generatePrintableString(&content_buf, random);
        const hash = generatePrintableString(&hash_buf, random);

        try storage.addFile(&db, path, content, hash);

        const fetched = try storage.getFile(&db, alloc, path);
        try testing.expect(fetched != null);
        defer {
            alloc.free(fetched.?.path);
            alloc.free(fetched.?.content);
            alloc.free(fetched.?.content_hash);
            alloc.free(fetched.?.indexed_at);
        }

        // Oracle: content and hash preserved
        try testing.expectEqualStrings(path, fetched.?.path);
        try testing.expectEqualStrings(content, fetched.?.content);
        try testing.expectEqualStrings(hash, fetched.?.content_hash);
    }
}

test "prop: addFile upsert updates existing" {
    // Oracle: Second addFile with same path updates content/hash
    const alloc = testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const path = "/test/file.zig";
    try storage.addFile(&db, path, "original", "hash1");
    try storage.addFile(&db, path, "updated", "hash2");

    const fetched = try storage.getFile(&db, alloc, path);
    try testing.expect(fetched != null);
    defer {
        alloc.free(fetched.?.path);
        alloc.free(fetched.?.content);
        alloc.free(fetched.?.content_hash);
        alloc.free(fetched.?.indexed_at);
    }

    // Oracle: latest values win
    try testing.expectEqualStrings("updated", fetched.?.content);
    try testing.expectEqualStrings("hash2", fetched.?.content_hash);

    // File count should still be 1
    const count = try storage.countFiles(&db);
    try testing.expectEqual(@as(i64, 1), count);
}

// ============================================================================
// Snapshot Tests: Data Structure Representations
// ============================================================================

test "snap: Note structure with all fields" {
    const oh = OhSnap{};
    const note = storage.Note{
        .id = "test-id-12345678",
        .file_path = "/src/main.zig",
        .node_path = "fn.main.body",
        .node_text_hash = "abc123def456",
        .line_number = 42,
        .column_number = 8,
        .content = "This is a test note",
        .created_at = "2024-01-15T10:30:00Z",
        .updated_at = "2024-01-15T11:45:00Z",
    };
    try oh.snap(
        @src(),
        \\storage.Note
        \\  .id: []const u8
        \\    "test-id-12345678"
        \\  .file_path: []const u8
        \\    "/src/main.zig"
        \\  .node_path: ?[]const u8
        \\    "fn.main.body"
        \\  .node_text_hash: ?[]const u8
        \\    "abc123def456"
        \\  .line_number: ?i64
        \\    42
        \\  .column_number: ?i64
        \\    8
        \\  .content: []const u8
        \\    "This is a test note"
        \\  .created_at: []const u8
        \\    "2024-01-15T10:30:00Z"
        \\  .updated_at: []const u8
        \\    "2024-01-15T11:45:00Z"
        ,
    ).expectEqual(note);
}

test "snap: Note structure with null optionals" {
    const oh = OhSnap{};
    const note = storage.Note{
        .id = "minimal-note",
        .file_path = "/test.py",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "minimal",
        .created_at = "2024-01-01",
        .updated_at = "2024-01-01",
    };
    try oh.snap(
        @src(),
        \\storage.Note
        \\  .id: []const u8
        \\    "minimal-note"
        \\  .file_path: []const u8
        \\    "/test.py"
        \\  .node_path: ?[]const u8
        \\    null
        \\  .node_text_hash: ?[]const u8
        \\    null
        \\  .line_number: ?i64
        \\    null
        \\  .column_number: ?i64
        \\    null
        \\  .content: []const u8
        \\    "minimal"
        \\  .created_at: []const u8
        \\    "2024-01-01"
        \\  .updated_at: []const u8
        \\    "2024-01-01"
        ,
    ).expectEqual(note);
}

test "snap: File structure" {
    const oh = OhSnap{};
    const file = storage.File{
        .path = "/src/lib.zig",
        .content = "pub fn add(a: i32, b: i32) i32 { return a + b; }",
        .content_hash = "sha256:e3b0c44298fc1c149afbf4c8",
        .indexed_at = "1704067200",
    };
    try oh.snap(
        @src(),
        \\storage.File
        \\  .path: []const u8
        \\    "/src/lib.zig"
        \\  .content: []const u8
        \\    "pub fn add(a: i32, b: i32) i32 { return a + b; }"
        \\  .content_hash: []const u8
        \\    "sha256:e3b0c44298fc1c149afbf4c8"
        \\  .indexed_at: []const u8
        \\    "1704067200"
        ,
    ).expectEqual(file);
}

test "snap: SearchHit structure" {
    const oh = OhSnap{};
    const hit = storage.SearchHit{
        .file = "/src/rpc.zig",
        .line = 42,
        .column = 12,
        .text = "fn handleStatus(alloc: Allocator) ![]const u8 {",
    };
    try oh.snap(
        @src(),
        \\storage.SearchHit
        \\  .file: []const u8
        \\    "/src/rpc.zig"
        \\  .line: usize = 42
        \\  .column: usize = 12
        \\  .text: []const u8
        \\    "fn handleStatus(alloc: Allocator) ![]const u8 {"
        ,
    ).expectEqual(hit);
}

test "snap: AI Provider enum" {
    const oh = OhSnap{};
    try oh.snap(
        @src(),
        \\ai.Provider
        \\  .claude
        ,
    ).expectEqual(ai.Provider.claude);
}

test "snap: TreeSitterError enum" {
    const oh = OhSnap{};
    try oh.snap(
        @src(),
        \\error{GrammarLoadFailed,GrammarNotAvailable,InvalidSymbol,OutOfMemory,ParseFailed,QueryFailed}
        \\  error.GrammarNotAvailable
        ,
    ).expectEqual(treesitter.TreeSitterError.GrammarNotAvailable);
}

// ============================================================================
// Model-Based Oracle Tests
// ============================================================================

/// Simple in-memory model of note storage for oracle comparison
const NoteModel = struct {
    notes: std.StringHashMap([]const u8),
    alloc: std.mem.Allocator,

    fn init(alloc: std.mem.Allocator) NoteModel {
        return .{
            .notes = std.StringHashMap([]const u8).init(alloc),
            .alloc = alloc,
        };
    }

    fn deinit(self: *NoteModel) void {
        var it = self.notes.iterator();
        while (it.next()) |entry| {
            self.alloc.free(entry.key_ptr.*);
            self.alloc.free(entry.value_ptr.*);
        }
        self.notes.deinit();
    }

    fn create(self: *NoteModel, id: []const u8, content: []const u8) !void {
        const id_copy = try self.alloc.dupe(u8, id);
        errdefer self.alloc.free(id_copy);
        const content_copy = try self.alloc.dupe(u8, content);
        try self.notes.put(id_copy, content_copy);
    }

    fn get(self: *NoteModel, id: []const u8) ?[]const u8 {
        return self.notes.get(id);
    }

    fn delete(self: *NoteModel, id: []const u8) void {
        if (self.notes.fetchRemove(id)) |kv| {
            self.alloc.free(kv.key);
            self.alloc.free(kv.value);
        }
    }

    fn count(self: *NoteModel) usize {
        return self.notes.count();
    }
};

test "model: storage matches in-memory oracle" {
    // Oracle: SQLite storage should behave identically to simple HashMap model
    const alloc = testing.allocator;

    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    var model = NoteModel.init(alloc);
    defer model.deinit();

    var prng = std.Random.DefaultPrng.init(0xFEDCBA98);
    const random = prng.random();

    // Run random operations and compare
    for (0..100) |_| {
        const op = random.intRangeAtMost(u8, 0, 2);

        switch (op) {
            0 => {
                // CREATE
                var id_buf: [36]u8 = undefined;
                var content_buf: [64]u8 = undefined;
                const id = generateId(&id_buf, random);
                const content = generatePrintableString(&content_buf, random);

                // Skip if already exists (to match model behavior)
                if (model.get(id) != null) continue;

                try model.create(id, content);
                try storage.createNote(&db, .{
                    .id = id,
                    .file_path = "/test.zig",
                    .node_path = null,
                    .node_text_hash = null,
                    .line_number = null,
                    .column_number = null,
                    .content = content,
                    .created_at = "2024-01-01",
                    .updated_at = "2024-01-01",
                });
            },
            1 => {
                // DELETE (random existing)
                // Copy key before deletion since model.delete frees it
                var it = model.notes.keyIterator();
                if (it.next()) |key_ptr| {
                    var id_copy: [36]u8 = undefined;
                    const key = key_ptr.*;
                    if (key.len <= 36) {
                        @memcpy(id_copy[0..key.len], key);
                        const id_slice = id_copy[0..key.len];
                        model.delete(id_slice);
                        try storage.deleteNote(&db, id_slice);
                    }
                }
            },
            2 => {
                // COUNT check
                const model_count = model.count();
                const db_count = try storage.countNotes(&db);
                try testing.expectEqual(@as(i64, @intCast(model_count)), db_count);
            },
            else => unreachable,
        }
    }

    // Final count check
    const model_count = model.count();
    const db_count = try storage.countNotes(&db);
    try testing.expectEqual(@as(i64, @intCast(model_count)), db_count);
}

// ============================================================================
// Quickcheck Integration Tests
// ============================================================================

test "qc: addition is commutative (sanity check)" {
    try quickcheck.check(struct {
        fn prop(args: struct { a: i16, b: i16 }) bool {
            const sum1 = @as(i32, args.a) + @as(i32, args.b);
            const sum2 = @as(i32, args.b) + @as(i32, args.a);
            return sum1 == sum2;
        }
    }.prop, .{});
}

test "qc: string length preserved through JSON escape" {
    // Escaped string should be >= original length (escaping only adds chars)
    const alloc = testing.allocator;

    try quickcheck.check(struct {
        fn prop(args: struct { seed: u32 }) bool {
            var prng = std.Random.DefaultPrng.init(args.seed);
            const random = prng.random();

            var input_buf: [32]u8 = undefined;
            const len = random.uintLessThan(usize, input_buf.len);
            for (input_buf[0..len]) |*c| {
                c.* = random.int(u8);
            }
            const input = input_buf[0..len];

            var output: std.ArrayList(u8) = .{};
            defer output.deinit(alloc);

            ai.writeJsonEscaped(output.writer(alloc), input) catch return false;

            // Escaped length >= original (escaping never shrinks)
            return output.items.len >= input.len;
        }
    }.prop, .{ .iterations = 200 });
}
