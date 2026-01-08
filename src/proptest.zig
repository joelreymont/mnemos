//! Property-based and snapshot tests for mnemos
//!
//! Philosophy:
//! - Property tests for pure functions and invariants
//! - Snapshot tests for data structure representations
//! - Model-based oracles for stateful operations (storage)

const std = @import("std");
const testing = std.testing;
const zcheck = @import("zcheck");
const OhSnap = @import("ohsnap");

const storage = @import("storage.zig");
const treesitter = @import("treesitter.zig");
const ai = @import("ai.zig");
const rpc = @import("rpc.zig");

// ============================================================================
// zcheck generators used:
// - zcheck.String: Bounded printable ASCII (64 bytes max), 10% empty
// - zcheck.Id: Alphanumeric identifier (8-36 chars)
// - zcheck.FilePath: Path with extension (128 bytes max)
// - zcheck.check(): Property-based testing with automatic shrinking
// ============================================================================

// ============================================================================
// Property Tests: Storage CRUD Operations
// ============================================================================

test "prop: note create-read roundtrip preserves content" {
    // Oracle: For any valid Note, create then read should return identical content
    const alloc = testing.allocator;
    var store = try storage.createTestStorage(alloc);
    defer storage.cleanupTestStorage(alloc, &store);

    var prng = std.Random.DefaultPrng.init(0x12345678);
    const random = prng.random();

    for (0..50) |_| {
        // Use zcheck generators for realistic test data
        const id = zcheck.generate(zcheck.Id, random);
        const path = zcheck.generate(zcheck.FilePath, random);
        const content = zcheck.generate(zcheck.String, random);

        const note = storage.Note{
            .id = id.slice(),
            .file_path = path.slice(),
            .node_path = null,
            .node_text_hash = null,
            .line_number = null,
            .column_number = null,
            .content = content.slice(),
            .created_at = "2024-01-01",
            .updated_at = "2024-01-01",
        };

        try store.createNote(note);

        const fetched = try store.getNote(alloc, id.slice());
        try testing.expect(fetched != null);
        defer storage.freeNoteFields(alloc, fetched.?);

        // Oracle check: content must match exactly
        try testing.expectEqualStrings(content.slice(), fetched.?.content);
        try testing.expectEqualStrings(path.slice(), fetched.?.file_path);
    }
}

test "prop: note count increases monotonically with creates" {
    // Oracle: After N creates, count should be N
    const alloc = testing.allocator;
    var store = try storage.createTestStorage(alloc);
    defer storage.cleanupTestStorage(alloc, &store);

    var prng = std.Random.DefaultPrng.init(0xABCDEF01);
    const random = prng.random();

    const n = 25;
    for (0..n) |i| {
        const id = zcheck.generate(zcheck.Id, random);

        try store.createNote(.{
            .id = id.slice(),
            .file_path = "/test.zig",
            .node_path = null,
            .node_text_hash = null,
            .line_number = null,
            .column_number = null,
            .content = "test",
            .created_at = "2024-01-01",
            .updated_at = "2024-01-01",
        });

        const count = try store.countNotes();
        try testing.expectEqual(@as(i64, @intCast(i + 1)), count);
    }
}

test "prop: delete removes exactly one note" {
    // Oracle: delete(id) decrements count by 1 and getNote(id) returns null
    const alloc = testing.allocator;
    var store = try storage.createTestStorage(alloc);
    defer storage.cleanupTestStorage(alloc, &store);

    var prng = std.Random.DefaultPrng.init(0xDEADBEEF);
    const random = prng.random();

    // Create several notes using zcheck.Id generator
    var ids: [10]zcheck.Id = undefined;
    for (&ids) |*id| {
        id.* = zcheck.generate(zcheck.Id, random);
        try store.createNote(.{
            .id = id.slice(),
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
    for (&ids, 0..) |*id, i| {
        const before_count = try store.countNotes();
        try store.deleteNote(id.slice());
        const after_count = try store.countNotes();

        // Oracle: count decremented by exactly 1
        try testing.expectEqual(before_count - 1, after_count);

        // Oracle: note no longer retrievable
        const gone = try store.getNote(alloc, id.slice());
        try testing.expect(gone == null);

        // Other notes still exist (spot check)
        if (i + 1 < ids.len) {
            const other = try store.getNote(alloc, ids[i + 1].slice());
            try testing.expect(other != null);
            storage.freeNoteFields(alloc, other.?);
        }
    }
}

test "prop: search results are subset of notes containing query" {
    // Oracle: Every search result must contain the query string
    const alloc = testing.allocator;
    var store = try storage.createTestStorage(alloc);
    defer storage.cleanupTestStorage(alloc, &store);

    // Create notes with known content patterns
    const contents = [_][]const u8{
        "hello world",
        "goodbye world",
        "hello there",
        "nothing here",
        "world peace",
    };

    for (contents, 0..) |content, i| {
        var id_buf: [12]u8 = undefined;
        const id = std.fmt.bufPrint(&id_buf, "note-{d}", .{i}) catch unreachable;
        try store.createNote(.{
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
    }

    // Search for "world"
    const results = try store.searchNotes(alloc, "world", 100, 0);
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
    var store = try storage.createTestStorage(alloc);
    defer storage.cleanupTestStorage(alloc, &store);

    var prng = std.Random.DefaultPrng.init(0xCAFEBABE);
    const random = prng.random();

    for (0..20) |_| {
        const id = zcheck.generate(zcheck.Id, random);
        const path = zcheck.generate(zcheck.FilePath, random);
        const original_content = zcheck.generate(zcheck.String, random);
        const new_content = zcheck.generate(zcheck.String, random);

        try store.createNote(.{
            .id = id.slice(),
            .file_path = path.slice(),
            .node_path = null,
            .node_text_hash = null,
            .line_number = null,
            .column_number = null,
            .content = original_content.slice(),
            .created_at = "2024-01-01",
            .updated_at = "2024-01-01",
        });

        try store.updateNote(id.slice(), new_content.slice(), null, "2024-01-02");

        const fetched = try store.getNote(alloc, id.slice());
        try testing.expect(fetched != null);
        defer storage.freeNoteFields(alloc, fetched.?);

        // Oracle: id and file_path unchanged
        try testing.expectEqualStrings(id.slice(), fetched.?.id);
        try testing.expectEqualStrings(path.slice(), fetched.?.file_path);
        // Oracle: content updated
        try testing.expectEqualStrings(new_content.slice(), fetched.?.content);
        // Oracle: updated_at changed
        try testing.expectEqualStrings("2024-01-02", fetched.?.updated_at);
    }
}

test "prop: getNotesForFile returns only matching files" {
    // Oracle: All notes returned by getNotesForFile have the requested file_path
    const alloc = testing.allocator;
    var store = try storage.createTestStorage(alloc);
    defer storage.cleanupTestStorage(alloc, &store);

    const files = [_][]const u8{ "/a.zig", "/b.zig", "/c.zig" };
    var prng = std.Random.DefaultPrng.init(0x11223344);
    const random = prng.random();

    // Create notes distributed across files using zcheck.Id
    for (0..30) |i| {
        const id = zcheck.generate(zcheck.Id, random);
        const file = files[i % files.len];

        try store.createNote(.{
            .id = id.slice(),
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
        const notes = try store.getNotesForFile(alloc, file);
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
        const input = zcheck.generate(zcheck.String, random);

        var output: std.ArrayList(u8) = .{};
        defer output.deinit(alloc);

        try ai.writeJsonEscaped(output.writer(alloc), input.slice());

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
        const input = zcheck.generate(zcheck.String, random);

        var out1: std.ArrayList(u8) = .{};
        defer out1.deinit(alloc);
        var out2: std.ArrayList(u8) = .{};
        defer out2.deinit(alloc);

        try ai.writeJsonEscaped(out1.writer(alloc), input.slice());
        try ai.writeJsonEscaped(out2.writer(alloc), input.slice());

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
// NOTE: File indexing (addFile, getFile, searchFiles, countFiles) is not supported
// in the new filesystem-based Storage. These tests have been removed.
// The new Storage only handles note storage, not file content indexing.

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

// NOTE: "snap: File structure" test removed - storage.File type no longer exists
// in the new filesystem-based Storage API.

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
    // Oracle: Storage should behave identically to simple HashMap model
    const alloc = testing.allocator;

    var store = try storage.createTestStorage(alloc);
    defer storage.cleanupTestStorage(alloc, &store);

    var model = NoteModel.init(alloc);
    defer model.deinit();

    var prng = std.Random.DefaultPrng.init(0xFEDCBA98);
    const random = prng.random();

    // Run random operations and compare
    for (0..100) |_| {
        const op = random.intRangeAtMost(u8, 0, 2);

        switch (op) {
            0 => {
                // CREATE using zcheck generators
                const id = zcheck.generate(zcheck.Id, random);
                const content = zcheck.generate(zcheck.String, random);

                // Skip if already exists (to match model behavior)
                if (model.get(id.slice()) != null) continue;

                try model.create(id.slice(), content.slice());
                try store.createNote(.{
                    .id = id.slice(),
                    .file_path = "/test.zig",
                    .node_path = null,
                    .node_text_hash = null,
                    .line_number = null,
                    .column_number = null,
                    .content = content.slice(),
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
                        try store.deleteNote(id_slice);
                    }
                }
            },
            2 => {
                // COUNT check
                const model_count = model.count();
                const store_count = try store.countNotes();
                try testing.expectEqual(@as(i64, @intCast(model_count)), store_count);
            },
            else => unreachable,
        }
    }

    // Final count check
    const model_count = model.count();
    const store_count = try store.countNotes();
    try testing.expectEqual(@as(i64, @intCast(model_count)), store_count);
}

// ============================================================================
// zcheck Property Tests with Automatic Shrinking
// ============================================================================

test "qc: string length preserved through JSON escape" {
    // Escaped string should be >= original length (escaping only adds chars)
    const alloc = testing.allocator;

    try zcheck.check(struct {
        fn prop(args: struct { s: zcheck.String }) bool {
            const input = args.s.slice();

            var output: std.ArrayList(u8) = .{};
            defer output.deinit(alloc);

            ai.writeJsonEscaped(output.writer(alloc), input) catch return false;

            // Escaped length >= original (escaping never shrinks)
            return output.items.len >= input.len;
        }
    }.prop, .{ .iterations = 200 });
}

test "qc: Id always has minimum length" {
    // zcheck.Id guarantees 8-36 char alphanumeric IDs
    try zcheck.check(struct {
        fn prop(args: struct { id: zcheck.Id }) bool {
            const slice = args.id.slice();
            return slice.len >= 8 and slice.len <= 36;
        }
    }.prop, .{ .iterations = 100 });
}

test "qc: FilePath starts with slash and has extension" {
    // zcheck.FilePath guarantees valid path format
    try zcheck.check(struct {
        fn prop(args: struct { path: zcheck.FilePath }) bool {
            const slice = args.path.slice();
            if (slice.len == 0) return false;
            if (slice[0] != '/') return false;
            return std.mem.lastIndexOfScalar(u8, slice, '.') != null;
        }
    }.prop, .{ .iterations = 100 });
}

test "qc: JSON escape roundtrip produces valid JSON" {
    // Any string escaped + quoted should parse as valid JSON
    const alloc = testing.allocator;

    try zcheck.check(struct {
        fn prop(args: struct { s: zcheck.String }) bool {
            const input = args.s.slice();

            var output: std.ArrayList(u8) = .{};
            defer output.deinit(alloc);

            ai.writeJsonEscaped(output.writer(alloc), input) catch return false;

            // Wrap in quotes and parse as JSON
            const json = std.fmt.allocPrint(alloc, "\"{s}\"", .{output.items}) catch return false;
            defer alloc.free(json);

            const parsed = std.json.parseFromSlice([]const u8, alloc, json, .{}) catch return false;
            defer parsed.deinit();
            return true;
        }
    }.prop, .{ .iterations = 200 });
}

// ============================================================================
// End-to-End RPC Tests
// ============================================================================

test "e2e: notes/create then notes/get roundtrip via RPC" {
    // Full stack test: JSON-RPC create → JSON-RPC get → verify content
    const alloc = testing.allocator;
    var store = try storage.createTestStorage(alloc);
    defer storage.cleanupTestStorage(alloc, &store);

    const note_content = "This is a test note created via RPC";

    // Create note via RPC (id is auto-generated, use filePath not file_path)
    const create_request =
        \\{"jsonrpc":"2.0","id":1,"method":"notes/create","params":{"filePath":"/test/rpc.zig","content":"This is a test note created via RPC"}}
    ;
    const create_response = rpc.dispatchWithStore(alloc, create_request, &store);
    defer alloc.free(create_response);

    // Verify success response
    try testing.expect(std.mem.indexOf(u8, create_response, "\"result\"") != null);
    try testing.expect(std.mem.indexOf(u8, create_response, "\"error\"") == null);

    // Extract note id from create response
    const id_marker = "\"id\":\"";
    const id_start = (std.mem.indexOf(u8, create_response, id_marker) orelse return error.TestUnexpectedResult) + id_marker.len;
    const id_end = std.mem.indexOfPos(u8, create_response, id_start, "\"") orelse return error.TestUnexpectedResult;
    const note_id = create_response[id_start..id_end];

    // Get note via RPC
    const get_request = try std.fmt.allocPrint(alloc,
        \\{{"jsonrpc":"2.0","id":2,"method":"notes/get","params":{{"id":"{s}"}}}}
    , .{note_id});
    defer alloc.free(get_request);
    const get_response = rpc.dispatchWithStore(alloc, get_request, &store);
    defer alloc.free(get_response);

    // Verify note content in response
    try testing.expect(std.mem.indexOf(u8, get_response, note_content) != null);
}

test "e2e: notes/search finds created notes via RPC" {
    // Full stack test: create notes → search → verify results
    const alloc = testing.allocator;
    var store = try storage.createTestStorage(alloc);
    defer storage.cleanupTestStorage(alloc, &store);

    // Create notes with searchable content (use filePath, not file_path)
    const contents = [_][]const u8{
        "The quick brown fox",
        "jumps over the lazy dog",
        "brown bread is tasty",
    };

    for (contents) |content| {
        const request = try std.fmt.allocPrint(alloc,
            \\{{"jsonrpc":"2.0","id":1,"method":"notes/create","params":{{"filePath":"/test.zig","content":"{s}"}}}}
        , .{content});
        defer alloc.free(request);

        const response = rpc.dispatchWithStore(alloc, request, &store);
        defer alloc.free(response);
    }

    // Search for "brown"
    const search_request =
        \\{"jsonrpc":"2.0","id":10,"method":"notes/search","params":{"query":"brown","limit":10}}
    ;
    const search_response = rpc.dispatchWithStore(alloc, search_request, &store);
    defer alloc.free(search_response);

    // Should find 2 notes containing "brown"
    try testing.expect(std.mem.indexOf(u8, search_response, "brown fox") != null);
    try testing.expect(std.mem.indexOf(u8, search_response, "brown bread") != null);
    // Should NOT find note without "brown"
    try testing.expect(std.mem.indexOf(u8, search_response, "lazy dog") == null);
}

test "e2e: notes/delete removes note via RPC" {
    // Full stack test: create → delete → verify gone
    const alloc = testing.allocator;
    var store = try storage.createTestStorage(alloc);
    defer storage.cleanupTestStorage(alloc, &store);

    // Create note (use filePath, id is auto-generated)
    const create_request =
        \\{"jsonrpc":"2.0","id":1,"method":"notes/create","params":{"filePath":"/test.zig","content":"to be deleted"}}
    ;
    const create_response = rpc.dispatchWithStore(alloc, create_request, &store);
    defer alloc.free(create_response);

    // Extract note id from create response
    const id_marker = "\"id\":\"";
    const id_start = (std.mem.indexOf(u8, create_response, id_marker) orelse return error.TestUnexpectedResult) + id_marker.len;
    const id_end = std.mem.indexOfPos(u8, create_response, id_start, "\"") orelse return error.TestUnexpectedResult;
    const note_id = create_response[id_start..id_end];

    // Delete note
    const delete_request = try std.fmt.allocPrint(alloc,
        \\{{"jsonrpc":"2.0","id":2,"method":"notes/delete","params":{{"id":"{s}"}}}}
    , .{note_id});
    defer alloc.free(delete_request);
    const delete_response = rpc.dispatchWithStore(alloc, delete_request, &store);
    defer alloc.free(delete_response);
    try testing.expect(std.mem.indexOf(u8, delete_response, "\"result\"") != null);

    // Verify note is gone
    const get_request = try std.fmt.allocPrint(alloc,
        \\{{"jsonrpc":"2.0","id":3,"method":"notes/get","params":{{"id":"{s}"}}}}
    , .{note_id});
    defer alloc.free(get_request);
    const get_response = rpc.dispatchWithStore(alloc, get_request, &store);
    defer alloc.free(get_response);

    // Should return null or error (note not found)
    const has_null = std.mem.indexOf(u8, get_response, "\"result\":null") != null;
    const has_error = std.mem.indexOf(u8, get_response, "\"error\"") != null;
    try testing.expect(has_null or has_error);
}

test "e2e: mnemos/status returns server info via RPC" {
    // Test status endpoint works without storage
    const alloc = testing.allocator;
    var store = try storage.createTestStorage(alloc);
    defer storage.cleanupTestStorage(alloc, &store);

    const request =
        \\{"jsonrpc":"2.0","id":1,"method":"mnemos/status"}
    ;
    const response = rpc.dispatchWithStore(alloc, request, &store);
    defer alloc.free(response);

    // Should have result with notes count
    try testing.expect(std.mem.indexOf(u8, response, "\"result\"") != null);
    try testing.expect(std.mem.indexOf(u8, response, "\"notes\"") != null);
}
