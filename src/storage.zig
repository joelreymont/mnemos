//! SQLite storage layer
//!
//! Provides database operations for notes, files, and indexes.

const std = @import("std");
const mem = std.mem;
const fs = std.fs;

const Allocator = mem.Allocator;

const c = @cImport({
    @cInclude("sqlite3.h");
});

// SQLITE_TRANSIENT is defined as ((void(*)(void*))-1) which doesn't translate well
// Use SQLITE_STATIC (0) for now - we ensure data lives long enough
const SQLITE_STATIC: c.sqlite3_destructor_type = null;

pub const SqliteError = error{
    OpenFailed,
    PrepareFailed,
    StepFailed,
    BindFailed,
    Busy,
    Constraint,
    OutOfMemory,
};

/// SQLite database connection
pub const Database = struct {
    db: *c.sqlite3,
    alloc: Allocator,

    pub fn open(alloc: Allocator, path: []const u8) !Database {
        // Null-terminate the path
        const path_z = try alloc.dupeZ(u8, path);
        defer alloc.free(path_z);

        var db: ?*c.sqlite3 = null;
        const rc = c.sqlite3_open(path_z.ptr, &db);
        if (rc != c.SQLITE_OK or db == null) {
            if (db) |d| _ = c.sqlite3_close(d);
            return SqliteError.OpenFailed;
        }

        var self = Database{ .db = db.?, .alloc = alloc };

        // Enable WAL mode for better concurrency
        try self.exec("PRAGMA journal_mode=WAL");
        try self.exec("PRAGMA foreign_keys=ON");

        // Create tables
        try self.createTables();

        return self;
    }

    pub fn close(self: *Database) void {
        _ = c.sqlite3_close(self.db);
    }

    fn exec(self: *Database, sql: []const u8) !void {
        const sql_z = try self.alloc.dupeZ(u8, sql);
        defer self.alloc.free(sql_z);

        var err_msg: [*c]u8 = null;
        const rc = c.sqlite3_exec(self.db, sql_z.ptr, null, null, &err_msg);
        if (err_msg) |msg| {
            c.sqlite3_free(msg);
        }
        if (rc != c.SQLITE_OK) {
            return SqliteError.StepFailed;
        }
    }

    fn createTables(self: *Database) !void {
        try self.exec(
            \\CREATE TABLE IF NOT EXISTS notes (
            \\    id TEXT PRIMARY KEY,
            \\    file_path TEXT NOT NULL,
            \\    node_path TEXT,
            \\    node_text_hash TEXT,
            \\    line_number INTEGER,
            \\    column_number INTEGER,
            \\    content TEXT NOT NULL,
            \\    created_at TEXT NOT NULL,
            \\    updated_at TEXT NOT NULL
            \\)
        );

        try self.exec(
            \\CREATE INDEX IF NOT EXISTS idx_notes_file ON notes(file_path)
        );

        // Migration: add column_number if missing (for existing databases)
        self.exec("ALTER TABLE notes ADD COLUMN column_number INTEGER") catch {};

        try self.exec(
            \\CREATE TABLE IF NOT EXISTS files (
            \\    path TEXT PRIMARY KEY,
            \\    content TEXT,
            \\    content_hash TEXT,
            \\    indexed_at TEXT
            \\)
        );
    }

    /// Prepare a statement
    pub fn prepare(self: *Database, sql: []const u8) !Statement {
        const sql_z = try self.alloc.dupeZ(u8, sql);
        defer self.alloc.free(sql_z);

        var stmt: ?*c.sqlite3_stmt = null;
        const rc = c.sqlite3_prepare_v2(self.db, sql_z.ptr, @intCast(sql_z.len), &stmt, null);
        if (rc != c.SQLITE_OK or stmt == null) {
            return SqliteError.PrepareFailed;
        }

        return Statement{ .stmt = stmt.?, .alloc = self.alloc };
    }
};

/// Prepared statement
pub const Statement = struct {
    stmt: *c.sqlite3_stmt,
    alloc: Allocator,

    pub fn deinit(self: *Statement) void {
        _ = c.sqlite3_finalize(self.stmt);
    }

    pub fn reset(self: *Statement) void {
        _ = c.sqlite3_reset(self.stmt);
        _ = c.sqlite3_clear_bindings(self.stmt);
    }

    pub fn bindText(self: *Statement, idx: c_int, value: []const u8) !void {
        const rc = c.sqlite3_bind_text(self.stmt, idx, value.ptr, @intCast(value.len), SQLITE_STATIC);
        if (rc != c.SQLITE_OK) return SqliteError.BindFailed;
    }

    pub fn bindInt(self: *Statement, idx: c_int, value: i64) !void {
        const rc = c.sqlite3_bind_int64(self.stmt, idx, value);
        if (rc != c.SQLITE_OK) return SqliteError.BindFailed;
    }

    pub fn bindNull(self: *Statement, idx: c_int) !void {
        const rc = c.sqlite3_bind_null(self.stmt, idx);
        if (rc != c.SQLITE_OK) return SqliteError.BindFailed;
    }

    pub fn step(self: *Statement) !bool {
        const rc = c.sqlite3_step(self.stmt);
        return switch (rc) {
            c.SQLITE_ROW => true,
            c.SQLITE_DONE => false,
            c.SQLITE_BUSY => SqliteError.Busy,
            c.SQLITE_CONSTRAINT => SqliteError.Constraint,
            else => SqliteError.StepFailed,
        };
    }

    pub fn columnText(self: *Statement, idx: c_int) ?[]const u8 {
        const ptr = c.sqlite3_column_text(self.stmt, idx);
        if (ptr == null) return null;
        const len = c.sqlite3_column_bytes(self.stmt, idx);
        if (len <= 0) return "";
        return ptr[0..@intCast(len)];
    }

    pub fn columnInt(self: *Statement, idx: c_int) i64 {
        return c.sqlite3_column_int64(self.stmt, idx);
    }

    /// Check if column is NULL (use to distinguish NULL from 0)
    pub fn columnIsNull(self: *Statement, idx: c_int) bool {
        return c.sqlite3_column_type(self.stmt, idx) == c.SQLITE_NULL;
    }

    /// Get optional int (returns null for NULL values, not for 0)
    pub fn columnOptionalInt(self: *Statement, idx: c_int) ?i64 {
        if (self.columnIsNull(idx)) return null;
        return self.columnInt(idx);
    }
};

/// Note record
pub const Note = struct {
    id: []const u8,
    file_path: []const u8,
    node_path: ?[]const u8,
    node_text_hash: ?[]const u8,
    line_number: ?i64,
    column_number: ?i64,
    content: []const u8,
    created_at: []const u8,
    updated_at: []const u8,
};

/// Create a new note
pub fn createNote(db: *Database, note: Note) !void {
    var stmt = try db.prepare(
        \\INSERT INTO notes (id, file_path, node_path, node_text_hash, line_number, column_number, content, created_at, updated_at)
        \\VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9)
    );
    defer stmt.deinit();

    try stmt.bindText(1, note.id);
    try stmt.bindText(2, note.file_path);
    if (note.node_path) |np| try stmt.bindText(3, np) else try stmt.bindNull(3);
    if (note.node_text_hash) |h| try stmt.bindText(4, h) else try stmt.bindNull(4);
    if (note.line_number) |ln| try stmt.bindInt(5, ln) else try stmt.bindNull(5);
    if (note.column_number) |cn| try stmt.bindInt(6, cn) else try stmt.bindNull(6);
    try stmt.bindText(7, note.content);
    try stmt.bindText(8, note.created_at);
    try stmt.bindText(9, note.updated_at);

    _ = try stmt.step();
}

/// List notes for a project (all notes)
pub fn listProjectNotes(db: *Database, alloc: Allocator) ![]Note {
    var stmt = try db.prepare(
        \\SELECT id, file_path, node_path, node_text_hash, line_number, column_number, content, created_at, updated_at
        \\FROM notes ORDER BY created_at DESC
    );
    defer stmt.deinit();

    var notes: std.ArrayList(Note) = .{};
    errdefer notes.deinit(alloc);

    while (try stmt.step()) {
        const note = Note{
            .id = try alloc.dupe(u8, stmt.columnText(0) orelse ""),
            .file_path = try alloc.dupe(u8, stmt.columnText(1) orelse ""),
            .node_path = if (stmt.columnText(2)) |np| try alloc.dupe(u8, np) else null,
            .node_text_hash = if (stmt.columnText(3)) |h| try alloc.dupe(u8, h) else null,
            .line_number = stmt.columnOptionalInt(4),
            .column_number = stmt.columnOptionalInt(5),
            .content = try alloc.dupe(u8, stmt.columnText(6) orelse ""),
            .created_at = try alloc.dupe(u8, stmt.columnText(7) orelse ""),
            .updated_at = try alloc.dupe(u8, stmt.columnText(8) orelse ""),
        };
        try notes.append(alloc, note);
    }

    return notes.toOwnedSlice(alloc);
}

/// Get note count
pub fn countNotes(db: *Database) !i64 {
    var stmt = try db.prepare("SELECT COUNT(*) FROM notes");
    defer stmt.deinit();
    _ = try stmt.step();
    return stmt.columnInt(0);
}

/// Count all indexed files
pub fn countFiles(db: *Database) !i64 {
    var stmt = try db.prepare("SELECT COUNT(*) FROM files");
    defer stmt.deinit();
    _ = try stmt.step();
    return stmt.columnInt(0);
}

/// Get a note by ID
pub fn getNote(db: *Database, alloc: Allocator, id: []const u8) !?Note {
    var stmt = try db.prepare(
        \\SELECT id, file_path, node_path, node_text_hash, line_number, column_number, content, created_at, updated_at
        \\FROM notes WHERE id = ?1
    );
    defer stmt.deinit();

    try stmt.bindText(1, id);

    if (try stmt.step()) {
        return Note{
            .id = try alloc.dupe(u8, stmt.columnText(0) orelse ""),
            .file_path = try alloc.dupe(u8, stmt.columnText(1) orelse ""),
            .node_path = if (stmt.columnText(2)) |np| try alloc.dupe(u8, np) else null,
            .node_text_hash = if (stmt.columnText(3)) |h| try alloc.dupe(u8, h) else null,
            .line_number = stmt.columnOptionalInt(4),
            .column_number = stmt.columnOptionalInt(5),
            .content = try alloc.dupe(u8, stmt.columnText(6) orelse ""),
            .created_at = try alloc.dupe(u8, stmt.columnText(7) orelse ""),
            .updated_at = try alloc.dupe(u8, stmt.columnText(8) orelse ""),
        };
    }
    return null;
}

/// Delete a note by ID
pub fn deleteNote(db: *Database, id: []const u8) !void {
    var stmt = try db.prepare("DELETE FROM notes WHERE id = ?1");
    defer stmt.deinit();
    try stmt.bindText(1, id);
    _ = try stmt.step();
}

/// Update a note
pub fn updateNote(db: *Database, id: []const u8, content: ?[]const u8, tags: ?[]const u8, updated_at: []const u8) !void {
    if (content) |text| {
        var stmt = try db.prepare("UPDATE notes SET content = ?1, updated_at = ?2 WHERE id = ?3");
        defer stmt.deinit();
        try stmt.bindText(1, text);
        try stmt.bindText(2, updated_at);
        try stmt.bindText(3, id);
        _ = try stmt.step();
    }
    _ = tags; // Tags not yet implemented in schema
}

/// Reattach note to new location (line, node_path, node_text_hash)
pub fn reattachNote(db: *Database, id: []const u8, file_path: ?[]const u8, line: i64, node_path: ?[]const u8, node_text_hash: ?[]const u8, updated_at: []const u8) !void {
    var stmt = try db.prepare(
        \\UPDATE notes SET file_path = COALESCE(?1, file_path), line_number = ?2,
        \\node_path = ?3, node_text_hash = ?4, updated_at = ?5 WHERE id = ?6
    );
    defer stmt.deinit();
    if (file_path) |fp| try stmt.bindText(1, fp) else try stmt.bindNull(1);
    try stmt.bindInt(2, line);
    if (node_path) |np| try stmt.bindText(3, np) else try stmt.bindNull(3);
    if (node_text_hash) |h| try stmt.bindText(4, h) else try stmt.bindNull(4);
    try stmt.bindText(5, updated_at);
    try stmt.bindText(6, id);
    _ = try stmt.step();
}

/// Escape LIKE metacharacters (%, _, \) for SQLite LIKE queries
fn escapeLikePattern(alloc: Allocator, query: []const u8) ![]const u8 {
    // Count characters needing escape
    var extra: usize = 0;
    for (query) |ch| {
        if (ch == '%' or ch == '_' or ch == '\\') extra += 1;
    }

    if (extra == 0) {
        // No escaping needed, just wrap with %
        return std.fmt.allocPrint(alloc, "%{s}%", .{query});
    }

    // Build escaped pattern
    var result = try alloc.alloc(u8, query.len + extra + 2); // +2 for surrounding %
    var i: usize = 0;
    result[i] = '%';
    i += 1;
    for (query) |ch| {
        if (ch == '%' or ch == '_' or ch == '\\') {
            result[i] = '\\';
            i += 1;
        }
        result[i] = ch;
        i += 1;
    }
    result[i] = '%';
    return result;
}

/// Search notes by query
pub fn searchNotes(db: *Database, alloc: Allocator, query: []const u8, limit: i64, offset: i64) ![]Note {
    var stmt = try db.prepare(
        \\SELECT id, file_path, node_path, node_text_hash, line_number, column_number, content, created_at, updated_at
        \\FROM notes WHERE content LIKE ?1 ESCAPE '\' ORDER BY created_at DESC LIMIT ?2 OFFSET ?3
    );
    defer stmt.deinit();

    const pattern = try escapeLikePattern(alloc, query);
    defer alloc.free(pattern);

    try stmt.bindText(1, pattern);
    try stmt.bindInt(2, limit);
    try stmt.bindInt(3, offset);

    var notes: std.ArrayList(Note) = .{};
    errdefer notes.deinit(alloc);

    while (try stmt.step()) {
        const note = Note{
            .id = try alloc.dupe(u8, stmt.columnText(0) orelse ""),
            .file_path = try alloc.dupe(u8, stmt.columnText(1) orelse ""),
            .node_path = if (stmt.columnText(2)) |np| try alloc.dupe(u8, np) else null,
            .node_text_hash = if (stmt.columnText(3)) |h| try alloc.dupe(u8, h) else null,
            .line_number = stmt.columnOptionalInt(4),
            .column_number = stmt.columnOptionalInt(5),
            .content = try alloc.dupe(u8, stmt.columnText(6) orelse ""),
            .created_at = try alloc.dupe(u8, stmt.columnText(7) orelse ""),
            .updated_at = try alloc.dupe(u8, stmt.columnText(8) orelse ""),
        };
        try notes.append(alloc, note);
    }

    return notes.toOwnedSlice(alloc);
}

/// Get notes for a file
pub fn getNotesForFile(db: *Database, alloc: Allocator, file_path: []const u8) ![]Note {
    var stmt = try db.prepare(
        \\SELECT id, file_path, node_path, node_text_hash, line_number, column_number, content, created_at, updated_at
        \\FROM notes WHERE file_path = ?1 ORDER BY created_at DESC
    );
    defer stmt.deinit();

    try stmt.bindText(1, file_path);

    var notes: std.ArrayList(Note) = .{};
    errdefer notes.deinit(alloc);

    while (try stmt.step()) {
        const note = Note{
            .id = try alloc.dupe(u8, stmt.columnText(0) orelse ""),
            .file_path = try alloc.dupe(u8, stmt.columnText(1) orelse ""),
            .node_path = if (stmt.columnText(2)) |np| try alloc.dupe(u8, np) else null,
            .node_text_hash = if (stmt.columnText(3)) |h| try alloc.dupe(u8, h) else null,
            .line_number = stmt.columnOptionalInt(4),
            .column_number = stmt.columnOptionalInt(5),
            .content = try alloc.dupe(u8, stmt.columnText(6) orelse ""),
            .created_at = try alloc.dupe(u8, stmt.columnText(7) orelse ""),
            .updated_at = try alloc.dupe(u8, stmt.columnText(8) orelse ""),
        };
        try notes.append(alloc, note);
    }

    return notes.toOwnedSlice(alloc);
}

/// Delete all notes
pub fn clearNotes(db: *Database) !void {
    var stmt = try db.prepare("DELETE FROM notes");
    defer stmt.deinit();
    _ = try stmt.step();
}

/// File record
pub const File = struct {
    path: []const u8,
    content: []const u8,
    content_hash: []const u8,
    indexed_at: []const u8,
};

/// Add or update a file in the index
pub fn addFile(db: *Database, path: []const u8, content: []const u8, content_hash: []const u8) !void {
    var ts_buf: [32]u8 = undefined;
    const ts = std.time.timestamp();
    const timestamp = std.fmt.bufPrint(&ts_buf, "{d}", .{ts}) catch "0";

    var stmt = try db.prepare(
        \\INSERT INTO files (path, content, content_hash, indexed_at)
        \\VALUES (?1, ?2, ?3, ?4)
        \\ON CONFLICT(path) DO UPDATE SET content = ?2, content_hash = ?3, indexed_at = ?4
    );
    defer stmt.deinit();

    try stmt.bindText(1, path);
    try stmt.bindText(2, content);
    try stmt.bindText(3, content_hash);
    try stmt.bindText(4, timestamp);

    _ = try stmt.step();
}

/// Get a file record by path
pub fn getFile(db: *Database, alloc: Allocator, path: []const u8) !?File {
    var stmt = try db.prepare(
        \\SELECT path, content, content_hash, indexed_at FROM files WHERE path = ?1
    );
    defer stmt.deinit();

    try stmt.bindText(1, path);

    if (try stmt.step()) {
        return File{
            .path = try alloc.dupe(u8, stmt.columnText(0) orelse ""),
            .content = try alloc.dupe(u8, stmt.columnText(1) orelse ""),
            .content_hash = try alloc.dupe(u8, stmt.columnText(2) orelse ""),
            .indexed_at = try alloc.dupe(u8, stmt.columnText(3) orelse ""),
        };
    }

    return null;
}

/// Export all notes as JSON using std.json (matches import format)
pub fn exportNotesJson(db: *Database, alloc: Allocator) ![]const u8 {
    const notes = try listProjectNotes(db, alloc);
    defer freeNotes(alloc, notes);

    // Build array of SnapshotNote for serialization
    const snapshot = try alloc.alloc(SnapshotNote, notes.len);
    defer alloc.free(snapshot);

    for (notes, 0..) |note, i| {
        snapshot[i] = .{ .id = note.id, .filePath = note.file_path, .content = note.content };
    }

    return std.fmt.allocPrint(alloc, "{f}", .{std.json.fmt(snapshot, .{})});
}

/// JSON format for snapshot notes
const SnapshotNote = struct {
    id: []const u8,
    filePath: []const u8,
    content: []const u8,
};

/// Import notes from JSON snapshot
/// JSON format: [{"id":"...","filePath":"...","content":"..."},...]
pub fn importNotesJson(db: *Database, alloc: Allocator, json: []const u8) !usize {
    const parsed = std.json.parseFromSlice([]SnapshotNote, alloc, json, .{}) catch |err| {
        std.log.err("JSON parse error: {}", .{err});
        return error.InvalidJson;
    };
    defer parsed.deinit();

    var count: usize = 0;
    var ts_buf: [32]u8 = undefined;
    const timestamp = getTimestamp(&ts_buf);

    for (parsed.value) |sn| {
        const note = Note{
            .id = sn.id,
            .file_path = sn.filePath,
            .node_path = null,
            .node_text_hash = null,
            .line_number = null,
            .column_number = null,
            .content = sn.content,
            .created_at = timestamp,
            .updated_at = timestamp,
        };
        createNote(db, note) catch continue;
        count += 1;
    }

    return count;
}

/// Get current timestamp as seconds since epoch
fn getTimestamp(buf: *[32]u8) []const u8 {
    const now = std.time.timestamp();
    const secs: u64 = @intCast(now);
    return std.fmt.bufPrint(buf, "{d}", .{secs}) catch "0";
}

/// Search files by path pattern
pub fn searchFiles(db: *Database, alloc: Allocator, query: []const u8) ![]File {
    var stmt = try db.prepare(
        \\SELECT path, content, content_hash, indexed_at FROM files
        \\WHERE path LIKE ?1 ESCAPE '\' ORDER BY indexed_at DESC
    );
    defer stmt.deinit();

    const pattern = try escapeLikePattern(alloc, query);
    defer alloc.free(pattern);

    try stmt.bindText(1, pattern);

    var files: std.ArrayList(File) = .{};
    errdefer files.deinit(alloc);

    while (try stmt.step()) {
        const file = File{
            .path = try alloc.dupe(u8, stmt.columnText(0) orelse ""),
            .content = try alloc.dupe(u8, stmt.columnText(1) orelse ""),
            .content_hash = try alloc.dupe(u8, stmt.columnText(2) orelse ""),
            .indexed_at = try alloc.dupe(u8, stmt.columnText(3) orelse ""),
        };
        try files.append(alloc, file);
    }

    return files.toOwnedSlice(alloc);
}

/// Search hit for content search
pub const SearchHit = struct {
    file: []const u8,
    line: usize,
    column: usize,
    text: []const u8,
};

/// Search all file content for a query, return matching lines
pub fn searchContent(db: *Database, alloc: Allocator, query: []const u8) ![]SearchHit {
    if (query.len == 0) return &[_]SearchHit{};

    var stmt = try db.prepare(
        \\SELECT path, content FROM files
    );
    defer stmt.deinit();

    var hits: std.ArrayList(SearchHit) = .{};
    errdefer hits.deinit(alloc);

    // Convert query to lowercase for case-insensitive matching
    const query_lower = try std.ascii.allocLowerString(alloc, query);
    defer alloc.free(query_lower);

    while (try stmt.step()) {
        const path = stmt.columnText(0) orelse continue;
        const content = stmt.columnText(1) orelse continue;

        // Search each line for the query
        var line_num: usize = 1;
        var line_start: usize = 0;
        for (content, 0..) |ch, i| {
            if (ch == '\n' or i == content.len - 1) {
                const line_end = if (ch == '\n') i else i + 1;
                const line = content[line_start..line_end];

                // Case-insensitive search
                const line_lower = std.ascii.allocLowerString(alloc, line) catch continue;
                defer alloc.free(line_lower);

                if (std.mem.indexOf(u8, line_lower, query_lower)) |col| {
                    const hit = SearchHit{
                        .file = try alloc.dupe(u8, path),
                        .line = line_num,
                        .column = col,
                        .text = try alloc.dupe(u8, line),
                    };
                    try hits.append(alloc, hit);

                    // Limit results
                    if (hits.items.len >= 100) break;
                }

                line_num += 1;
                line_start = i + 1;
            }
        }
    }

    return hits.toOwnedSlice(alloc);
}

/// Free search hits
pub fn freeSearchHits(alloc: Allocator, hits: []const SearchHit) void {
    for (hits) |hit| {
        alloc.free(hit.file);
        alloc.free(hit.text);
    }
    alloc.free(hits);
}

test "database open close" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    const count = try countNotes(&db);
    try std.testing.expectEqual(@as(i64, 0), count);
}

test "note CRUD operations" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    // Create
    const note = Note{
        .id = "test-123",
        .file_path = "/src/main.zig",
        .node_path = "fn.main",
        .node_text_hash = "abc123",
        .line_number = 42,
        .column_number = null,
        .content = "Test note content",
        .created_at = "2024-01-01",
        .updated_at = "2024-01-01",
    };
    try createNote(&db, note);

    // Count
    const count = try countNotes(&db);
    try std.testing.expectEqual(@as(i64, 1), count);

    // Get
    const fetched = try getNote(&db, alloc, "test-123");
    try std.testing.expect(fetched != null);
    defer {
        alloc.free(fetched.?.id);
        alloc.free(fetched.?.file_path);
        if (fetched.?.node_path) |np| alloc.free(np);
        if (fetched.?.node_text_hash) |h| alloc.free(h);
        alloc.free(fetched.?.content);
        alloc.free(fetched.?.created_at);
        alloc.free(fetched.?.updated_at);
    }
    try std.testing.expectEqualStrings("Test note content", fetched.?.content);

    // Update
    try updateNote(&db, "test-123", "Updated content", null, "2024-01-02");
    const updated = try getNote(&db, alloc, "test-123");
    defer {
        alloc.free(updated.?.id);
        alloc.free(updated.?.file_path);
        if (updated.?.node_path) |np| alloc.free(np);
        if (updated.?.node_text_hash) |h| alloc.free(h);
        alloc.free(updated.?.content);
        alloc.free(updated.?.created_at);
        alloc.free(updated.?.updated_at);
    }
    try std.testing.expectEqualStrings("Updated content", updated.?.content);

    // Delete
    try deleteNote(&db, "test-123");
    const deleted = try getNote(&db, alloc, "test-123");
    try std.testing.expect(deleted == null);
}

test "search notes" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    try createNote(&db, .{
        .id = "note-1",
        .file_path = "/src/foo.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "Hello world test",
        .created_at = "2024-01-01",
        .updated_at = "2024-01-01",
    });
    try createNote(&db, .{
        .id = "note-2",
        .file_path = "/src/bar.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "Goodbye world",
        .created_at = "2024-01-02",
        .updated_at = "2024-01-02",
    });

    const results = try searchNotes(&db, alloc, "world", 10, 0);
    defer {
        for (results) |n| {
            alloc.free(n.id);
            alloc.free(n.file_path);
            if (n.node_path) |np| alloc.free(np);
            if (n.node_text_hash) |h| alloc.free(h);
            alloc.free(n.content);
            alloc.free(n.created_at);
            alloc.free(n.updated_at);
        }
        alloc.free(results);
    }
    try std.testing.expectEqual(@as(usize, 2), results.len);

    // Test limit
    const limited = try searchNotes(&db, alloc, "world", 1, 0);
    defer {
        for (limited) |n| {
            alloc.free(n.id);
            alloc.free(n.file_path);
            if (n.node_path) |np| alloc.free(np);
            if (n.node_text_hash) |h| alloc.free(h);
            alloc.free(n.content);
            alloc.free(n.created_at);
            alloc.free(n.updated_at);
        }
        alloc.free(limited);
    }
    try std.testing.expectEqual(@as(usize, 1), limited.len);
}

test "notes for file" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    try createNote(&db, .{
        .id = "note-1",
        .file_path = "/src/main.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "Note 1",
        .created_at = "2024-01-01",
        .updated_at = "2024-01-01",
    });
    try createNote(&db, .{
        .id = "note-2",
        .file_path = "/src/main.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "Note 2",
        .created_at = "2024-01-02",
        .updated_at = "2024-01-02",
    });
    try createNote(&db, .{
        .id = "note-3",
        .file_path = "/src/other.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "Note 3",
        .created_at = "2024-01-03",
        .updated_at = "2024-01-03",
    });

    const notes = try getNotesForFile(&db, alloc, "/src/main.zig");
    defer {
        for (notes) |n| {
            alloc.free(n.id);
            alloc.free(n.file_path);
            if (n.node_path) |np| alloc.free(np);
            if (n.node_text_hash) |h| alloc.free(h);
            alloc.free(n.content);
            alloc.free(n.created_at);
            alloc.free(n.updated_at);
        }
        alloc.free(notes);
    }
    try std.testing.expectEqual(@as(usize, 2), notes.len);
}

test "file indexing" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    try addFile(&db, "/src/main.zig", "fn main() {}", "hash123");
    try addFile(&db, "/src/lib.zig", "pub fn lib() {}", "hash456");

    const file = try getFile(&db, alloc, "/src/main.zig");
    try std.testing.expect(file != null);
    defer {
        alloc.free(file.?.path);
        alloc.free(file.?.content);
        alloc.free(file.?.content_hash);
        alloc.free(file.?.indexed_at);
    }
    try std.testing.expectEqualStrings("hash123", file.?.content_hash);

    // Update existing
    try addFile(&db, "/src/main.zig", "fn main() { updated }", "newhash");
    const updated = try getFile(&db, alloc, "/src/main.zig");
    defer {
        alloc.free(updated.?.path);
        alloc.free(updated.?.content);
        alloc.free(updated.?.content_hash);
        alloc.free(updated.?.indexed_at);
    }
    try std.testing.expectEqualStrings("newhash", updated.?.content_hash);

    // Search
    const results = try searchFiles(&db, alloc, "zig");
    defer {
        for (results) |f| {
            alloc.free(f.path);
            alloc.free(f.content);
            alloc.free(f.content_hash);
            alloc.free(f.indexed_at);
        }
        alloc.free(results);
    }
    try std.testing.expectEqual(@as(usize, 2), results.len);
}

test "clear notes" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    try createNote(&db, .{
        .id = "note-1",
        .file_path = "/test",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "content",
        .created_at = "2024-01-01",
        .updated_at = "2024-01-01",
    });

    try std.testing.expectEqual(@as(i64, 1), try countNotes(&db));
    try clearNotes(&db);
    try std.testing.expectEqual(@as(i64, 0), try countNotes(&db));
}

test "export notes json" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    try createNote(&db, .{
        .id = "export-1",
        .file_path = "/export.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "export test",
        .created_at = "2024-01-01",
        .updated_at = "2024-01-01",
    });

    const json = try exportNotesJson(&db, alloc);
    defer alloc.free(json);
    try std.testing.expect(std.mem.indexOf(u8, json, "export-1") != null);
    try std.testing.expect(std.mem.indexOf(u8, json, "export test") != null);
}

test "get note not found" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    const result = try getNote(&db, alloc, "nonexistent-id");
    try std.testing.expect(result == null);
}

test "note with node path and hash" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    try createNote(&db, .{
        .id = "node-note",
        .file_path = "/src/lib.zig",
        .node_path = "fn.myFunction.body",
        .node_text_hash = "sha256hash123",
        .line_number = 42,
        .column_number = null,
        .content = "Note on function body",
        .created_at = "2024-01-01",
        .updated_at = "2024-01-01",
    });

    const note = try getNote(&db, alloc, "node-note");
    try std.testing.expect(note != null);
    defer {
        alloc.free(note.?.id);
        alloc.free(note.?.file_path);
        if (note.?.node_path) |np| alloc.free(np);
        if (note.?.node_text_hash) |h| alloc.free(h);
        alloc.free(note.?.content);
        alloc.free(note.?.created_at);
        alloc.free(note.?.updated_at);
    }
    try std.testing.expectEqualStrings("fn.myFunction.body", note.?.node_path.?);
    try std.testing.expectEqualStrings("sha256hash123", note.?.node_text_hash.?);
    try std.testing.expectEqual(@as(i64, 42), note.?.line_number.?);
}

test "statement bind and step" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    var stmt = try db.prepare("SELECT ?1, ?2");
    defer stmt.deinit();

    try stmt.bindText(1, "hello");
    try stmt.bindInt(2, 42);

    const has_row = try stmt.step();
    try std.testing.expect(has_row);
    try std.testing.expectEqualStrings("hello", stmt.columnText(0).?);
    try std.testing.expectEqual(@as(i64, 42), stmt.columnInt(1));
}

test "statement reset" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    var stmt = try db.prepare("SELECT ?1");
    defer stmt.deinit();

    try stmt.bindText(1, "first");
    _ = try stmt.step();

    stmt.reset();
    try stmt.bindText(1, "second");
    const has_row = try stmt.step();
    try std.testing.expect(has_row);
    try std.testing.expectEqualStrings("second", stmt.columnText(0).?);
}

test "note with empty content" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    try createNote(&db, .{
        .id = "empty-note",
        .file_path = "/test.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "",
        .created_at = "2024-01-01",
        .updated_at = "2024-01-01",
    });

    const note = try getNote(&db, alloc, "empty-note");
    try std.testing.expect(note != null);
    defer {
        alloc.free(note.?.id);
        alloc.free(note.?.file_path);
        if (note.?.node_path) |np| alloc.free(np);
        if (note.?.node_text_hash) |h| alloc.free(h);
        alloc.free(note.?.content);
        alloc.free(note.?.created_at);
        alloc.free(note.?.updated_at);
    }
    try std.testing.expectEqualStrings("", note.?.content);
}

test "note with very long content" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    const long_content = try alloc.alloc(u8, 10000);
    defer alloc.free(long_content);
    for (long_content, 0..) |*byte, i| {
        byte.* = @intCast((i % 26) + 'a');
    }

    try createNote(&db, .{
        .id = "long-note",
        .file_path = "/test.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = long_content,
        .created_at = "2024-01-01",
        .updated_at = "2024-01-01",
    });

    const note = try getNote(&db, alloc, "long-note");
    try std.testing.expect(note != null);
    defer {
        alloc.free(note.?.id);
        alloc.free(note.?.file_path);
        if (note.?.node_path) |np| alloc.free(np);
        if (note.?.node_text_hash) |h| alloc.free(h);
        alloc.free(note.?.content);
        alloc.free(note.?.created_at);
        alloc.free(note.?.updated_at);
    }
    try std.testing.expectEqual(long_content.len, note.?.content.len);
}

test "note with special characters" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    const special_content = "Line 1\nLine 2\tTab\r\n\"Quotes\"\n'Single'\n\\Backslash\n\x00Null";
    try createNote(&db, .{
        .id = "special-note",
        .file_path = "/test.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = special_content,
        .created_at = "2024-01-01",
        .updated_at = "2024-01-01",
    });

    const note = try getNote(&db, alloc, "special-note");
    try std.testing.expect(note != null);
    defer {
        alloc.free(note.?.id);
        alloc.free(note.?.file_path);
        if (note.?.node_path) |np| alloc.free(np);
        if (note.?.node_text_hash) |h| alloc.free(h);
        alloc.free(note.?.content);
        alloc.free(note.?.created_at);
        alloc.free(note.?.updated_at);
    }
    try std.testing.expectEqualStrings(special_content, note.?.content);
}

test "note with unicode content" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    const unicode_content = "Hello 世界 🚀 Emoji test ñ é ü";
    try createNote(&db, .{
        .id = "unicode-note",
        .file_path = "/test.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = unicode_content,
        .created_at = "2024-01-01",
        .updated_at = "2024-01-01",
    });

    const note = try getNote(&db, alloc, "unicode-note");
    try std.testing.expect(note != null);
    defer {
        alloc.free(note.?.id);
        alloc.free(note.?.file_path);
        if (note.?.node_path) |np| alloc.free(np);
        if (note.?.node_text_hash) |h| alloc.free(h);
        alloc.free(note.?.content);
        alloc.free(note.?.created_at);
        alloc.free(note.?.updated_at);
    }
    try std.testing.expectEqualStrings(unicode_content, note.?.content);
}

test "duplicate note id constraint" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    try createNote(&db, .{
        .id = "dup-id",
        .file_path = "/test.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "First",
        .created_at = "2024-01-01",
        .updated_at = "2024-01-01",
    });

    const result = createNote(&db, .{
        .id = "dup-id",
        .file_path = "/test.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "Second",
        .created_at = "2024-01-02",
        .updated_at = "2024-01-02",
    });
    try std.testing.expectError(SqliteError.Constraint, result);
}

test "delete nonexistent note" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    try deleteNote(&db, "nonexistent-id");
    try std.testing.expectEqual(@as(i64, 0), try countNotes(&db));
}

test "update nonexistent note" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    try updateNote(&db, "nonexistent-id", "New content", null, "2024-01-01");
    try std.testing.expectEqual(@as(i64, 0), try countNotes(&db));
}

test "search notes case sensitivity" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    try createNote(&db, .{
        .id = "case-1",
        .file_path = "/test.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "UPPERCASE TEXT",
        .created_at = "2024-01-01",
        .updated_at = "2024-01-01",
    });
    try createNote(&db, .{
        .id = "case-2",
        .file_path = "/test.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "lowercase text",
        .created_at = "2024-01-02",
        .updated_at = "2024-01-02",
    });

    const upper_results = try searchNotes(&db, alloc, "UPPERCASE", 10, 0);
    defer {
        for (upper_results) |n| {
            alloc.free(n.id);
            alloc.free(n.file_path);
            if (n.node_path) |np| alloc.free(np);
            if (n.node_text_hash) |h| alloc.free(h);
            alloc.free(n.content);
            alloc.free(n.created_at);
            alloc.free(n.updated_at);
        }
        alloc.free(upper_results);
    }
    try std.testing.expectEqual(@as(usize, 1), upper_results.len);

    const lower_results = try searchNotes(&db, alloc, "lowercase", 10, 0);
    defer {
        for (lower_results) |n| {
            alloc.free(n.id);
            alloc.free(n.file_path);
            if (n.node_path) |np| alloc.free(np);
            if (n.node_text_hash) |h| alloc.free(h);
            alloc.free(n.content);
            alloc.free(n.created_at);
            alloc.free(n.updated_at);
        }
        alloc.free(lower_results);
    }
    try std.testing.expectEqual(@as(usize, 1), lower_results.len);
}

test "search notes with special characters" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    try createNote(&db, .{
        .id = "special-search",
        .file_path = "/test.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "Test with %percent and _underscore",
        .created_at = "2024-01-01",
        .updated_at = "2024-01-01",
    });

    const results = try searchNotes(&db, alloc, "%percent", 10, 0);
    defer {
        for (results) |n| {
            alloc.free(n.id);
            alloc.free(n.file_path);
            if (n.node_path) |np| alloc.free(np);
            if (n.node_text_hash) |h| alloc.free(h);
            alloc.free(n.content);
            alloc.free(n.created_at);
            alloc.free(n.updated_at);
        }
        alloc.free(results);
    }
    try std.testing.expectEqual(@as(usize, 1), results.len);
}

test "search notes with offset pagination" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    var i: usize = 0;
    while (i < 5) : (i += 1) {
        const id = try std.fmt.allocPrint(alloc, "note-{d}", .{i});
        defer alloc.free(id);
        try createNote(&db, .{
            .id = id,
            .file_path = "/test.zig",
            .node_path = null,
            .node_text_hash = null,
            .line_number = null,
            .column_number = null,
            .content = "common",
            .created_at = "2024-01-01",
            .updated_at = "2024-01-01",
        });
    }

    const page1 = try searchNotes(&db, alloc, "common", 2, 0);
    defer {
        for (page1) |n| {
            alloc.free(n.id);
            alloc.free(n.file_path);
            if (n.node_path) |np| alloc.free(np);
            if (n.node_text_hash) |h| alloc.free(h);
            alloc.free(n.content);
            alloc.free(n.created_at);
            alloc.free(n.updated_at);
        }
        alloc.free(page1);
    }
    try std.testing.expectEqual(@as(usize, 2), page1.len);

    const page2 = try searchNotes(&db, alloc, "common", 2, 2);
    defer {
        for (page2) |n| {
            alloc.free(n.id);
            alloc.free(n.file_path);
            if (n.node_path) |np| alloc.free(np);
            if (n.node_text_hash) |h| alloc.free(h);
            alloc.free(n.content);
            alloc.free(n.created_at);
            alloc.free(n.updated_at);
        }
        alloc.free(page2);
    }
    try std.testing.expectEqual(@as(usize, 2), page2.len);
}

test "search notes empty query" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    try createNote(&db, .{
        .id = "empty-search",
        .file_path = "/test.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "Any content",
        .created_at = "2024-01-01",
        .updated_at = "2024-01-01",
    });

    const results = try searchNotes(&db, alloc, "", 10, 0);
    defer {
        for (results) |n| {
            alloc.free(n.id);
            alloc.free(n.file_path);
            if (n.node_path) |np| alloc.free(np);
            if (n.node_text_hash) |h| alloc.free(h);
            alloc.free(n.content);
            alloc.free(n.created_at);
            alloc.free(n.updated_at);
        }
        alloc.free(results);
    }
    try std.testing.expectEqual(@as(usize, 1), results.len);
}

test "multiple files with same hash" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    try addFile(&db, "/src/file1.zig", "same content", "samehash");
    try addFile(&db, "/src/file2.zig", "same content", "samehash");

    const file1 = try getFile(&db, alloc, "/src/file1.zig");
    defer {
        alloc.free(file1.?.path);
        alloc.free(file1.?.content);
        alloc.free(file1.?.content_hash);
        alloc.free(file1.?.indexed_at);
    }
    const file2 = try getFile(&db, alloc, "/src/file2.zig");
    defer {
        alloc.free(file2.?.path);
        alloc.free(file2.?.content);
        alloc.free(file2.?.content_hash);
        alloc.free(file2.?.indexed_at);
    }
    try std.testing.expectEqualStrings("samehash", file1.?.content_hash);
    try std.testing.expectEqualStrings("samehash", file2.?.content_hash);
}

test "file not found" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    const result = try getFile(&db, alloc, "/nonexistent.zig");
    try std.testing.expect(result == null);
}

test "search files no matches" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    try addFile(&db, "/src/main.zig", "fn main() {}", "hash1");

    const results = try searchFiles(&db, alloc, "nonexistent");
    defer alloc.free(results);
    try std.testing.expectEqual(@as(usize, 0), results.len);
}

test "list notes multiple files" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    try createNote(&db, .{
        .id = "note-a",
        .file_path = "/a.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "Note A",
        .created_at = "2024-01-01",
        .updated_at = "2024-01-01",
    });
    try createNote(&db, .{
        .id = "note-b",
        .file_path = "/b.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "Note B",
        .created_at = "2024-01-02",
        .updated_at = "2024-01-02",
    });
    try createNote(&db, .{
        .id = "note-c",
        .file_path = "/c.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "Note C",
        .created_at = "2024-01-03",
        .updated_at = "2024-01-03",
    });

    const notes = try listProjectNotes(&db, alloc);
    defer {
        for (notes) |n| {
            alloc.free(n.id);
            alloc.free(n.file_path);
            if (n.node_path) |np| alloc.free(np);
            if (n.node_text_hash) |h| alloc.free(h);
            alloc.free(n.content);
            alloc.free(n.created_at);
            alloc.free(n.updated_at);
        }
        alloc.free(notes);
    }
    try std.testing.expectEqual(@as(usize, 3), notes.len);
}

test "timestamp handling different formats" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    try createNote(&db, .{
        .id = "ts-1",
        .file_path = "/test.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "Content",
        .created_at = "2024-01-01T00:00:00Z",
        .updated_at = "2024-01-01T00:00:00Z",
    });
    try createNote(&db, .{
        .id = "ts-2",
        .file_path = "/test.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "Content",
        .created_at = "1704067200",
        .updated_at = "1704067200",
    });

    const count = try countNotes(&db);
    try std.testing.expectEqual(@as(i64, 2), count);
}

test "update note with null content" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    try createNote(&db, .{
        .id = "update-null",
        .file_path = "/test.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "Original",
        .created_at = "2024-01-01",
        .updated_at = "2024-01-01",
    });

    try updateNote(&db, "update-null", null, null, "2024-01-02");
    const note = try getNote(&db, alloc, "update-null");
    defer {
        alloc.free(note.?.id);
        alloc.free(note.?.file_path);
        if (note.?.node_path) |np| alloc.free(np);
        if (note.?.node_text_hash) |h| alloc.free(h);
        alloc.free(note.?.content);
        alloc.free(note.?.created_at);
        alloc.free(note.?.updated_at);
    }
    try std.testing.expectEqualStrings("Original", note.?.content);
}

test "export empty notes json" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    const json = try exportNotesJson(&db, alloc);
    defer alloc.free(json);
    try std.testing.expectEqualStrings("[]", json);
}

test "bind null values" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    try createNote(&db, .{
        .id = "null-test",
        .file_path = "/test.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "Content",
        .created_at = "2024-01-01",
        .updated_at = "2024-01-01",
    });

    const note = try getNote(&db, alloc, "null-test");
    try std.testing.expect(note != null);
    defer {
        alloc.free(note.?.id);
        alloc.free(note.?.file_path);
        if (note.?.node_path) |np| alloc.free(np);
        if (note.?.node_text_hash) |h| alloc.free(h);
        alloc.free(note.?.content);
        alloc.free(note.?.created_at);
        alloc.free(note.?.updated_at);
    }
    try std.testing.expect(note.?.node_path == null);
    try std.testing.expect(note.?.node_text_hash == null);
    try std.testing.expect(note.?.line_number == null);
}

// ============================================================================
// Additional Storage Tests
// ============================================================================

/// Free all heap-allocated fields in a Note
pub fn freeNoteFields(alloc: Allocator, note: Note) void {
    alloc.free(note.id);
    alloc.free(note.file_path);
    if (note.node_path) |np| alloc.free(np);
    if (note.node_text_hash) |h| alloc.free(h);
    alloc.free(note.content);
    alloc.free(note.created_at);
    alloc.free(note.updated_at);
}

/// Free a slice of notes and all their fields
pub fn freeNotes(alloc: Allocator, notes: []const Note) void {
    for (notes) |note| freeNoteFields(alloc, note);
    alloc.free(notes);
}

test "countNotes returns correct count" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    try std.testing.expectEqual(@as(i64, 0), try countNotes(&db));

    try createNote(&db, .{
        .id = "note-1",
        .file_path = "/a.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "First",
        .created_at = "2024-01-01",
        .updated_at = "2024-01-01",
    });

    try std.testing.expectEqual(@as(i64, 1), try countNotes(&db));

    try createNote(&db, .{
        .id = "note-2",
        .file_path = "/b.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "Second",
        .created_at = "2024-01-01",
        .updated_at = "2024-01-01",
    });

    try std.testing.expectEqual(@as(i64, 2), try countNotes(&db));
}

test "clearNotes removes all notes" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    try createNote(&db, .{
        .id = "note-1",
        .file_path = "/a.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "First",
        .created_at = "2024-01-01",
        .updated_at = "2024-01-01",
    });
    try createNote(&db, .{
        .id = "note-2",
        .file_path = "/b.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "Second",
        .created_at = "2024-01-01",
        .updated_at = "2024-01-01",
    });

    try std.testing.expectEqual(@as(i64, 2), try countNotes(&db));

    try clearNotes(&db);

    try std.testing.expectEqual(@as(i64, 0), try countNotes(&db));
}

test "deleteNote removes specific note" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    try createNote(&db, .{
        .id = "keep-me",
        .file_path = "/a.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "Keep",
        .created_at = "2024-01-01",
        .updated_at = "2024-01-01",
    });
    try createNote(&db, .{
        .id = "delete-me",
        .file_path = "/b.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "Delete",
        .created_at = "2024-01-01",
        .updated_at = "2024-01-01",
    });

    try deleteNote(&db, "delete-me");

    try std.testing.expectEqual(@as(i64, 1), try countNotes(&db));

    const kept = try getNote(&db, alloc, "keep-me");
    try std.testing.expect(kept != null);
    defer {
        alloc.free(kept.?.id);
        alloc.free(kept.?.file_path);
        if (kept.?.node_path) |np| alloc.free(np);
        if (kept.?.node_text_hash) |h| alloc.free(h);
        alloc.free(kept.?.content);
        alloc.free(kept.?.created_at);
        alloc.free(kept.?.updated_at);
    }

    const deleted = try getNote(&db, alloc, "delete-me");
    try std.testing.expect(deleted == null);
}

test "updateNote changes content" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    try createNote(&db, .{
        .id = "update-test",
        .file_path = "/test.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "Original",
        .created_at = "2024-01-01",
        .updated_at = "2024-01-01",
    });

    try updateNote(&db, "update-test", "Updated content", null, "2024-01-02");

    const note = try getNote(&db, alloc, "update-test");
    try std.testing.expect(note != null);
    defer {
        alloc.free(note.?.id);
        alloc.free(note.?.file_path);
        if (note.?.node_path) |np| alloc.free(np);
        if (note.?.node_text_hash) |h| alloc.free(h);
        alloc.free(note.?.content);
        alloc.free(note.?.created_at);
        alloc.free(note.?.updated_at);
    }
    try std.testing.expectEqualStrings("Updated content", note.?.content);
    try std.testing.expectEqualStrings("2024-01-02", note.?.updated_at);
}

test "searchNotes finds matching notes" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    try createNote(&db, .{
        .id = "haystack-1",
        .file_path = "/a.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "The needle is hidden here",
        .created_at = "2024-01-01",
        .updated_at = "2024-01-01",
    });
    try createNote(&db, .{
        .id = "haystack-2",
        .file_path = "/b.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "Nothing special",
        .created_at = "2024-01-01",
        .updated_at = "2024-01-01",
    });

    const results = try searchNotes(&db, alloc, "needle", 10, 0);
    defer {
        for (results) |note| freeNoteFields(alloc, note);
        alloc.free(results);
    }

    try std.testing.expectEqual(@as(usize, 1), results.len);
    try std.testing.expectEqualStrings("haystack-1", results[0].id);
}

test "searchNotes respects limit" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    for (0..5) |i| {
        var id_buf: [32]u8 = undefined;
        const id = std.fmt.bufPrint(&id_buf, "note-{d}", .{i}) catch unreachable;
        try createNote(&db, .{
            .id = id,
            .file_path = "/test.zig",
            .node_path = null,
            .node_text_hash = null,
            .line_number = null,
            .column_number = null,
            .content = "searchable content",
            .created_at = "2024-01-01",
            .updated_at = "2024-01-01",
        });
    }

    const results = try searchNotes(&db, alloc, "searchable", 3, 0);
    defer {
        for (results) |note| freeNoteFields(alloc, note);
        alloc.free(results);
    }

    try std.testing.expectEqual(@as(usize, 3), results.len);
}

test "searchNotes respects offset" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    for (0..5) |i| {
        var id_buf: [32]u8 = undefined;
        const id = std.fmt.bufPrint(&id_buf, "note-{d}", .{i}) catch unreachable;
        try createNote(&db, .{
            .id = id,
            .file_path = "/test.zig",
            .node_path = null,
            .node_text_hash = null,
            .line_number = null,
            .column_number = null,
            .content = "searchable content",
            .created_at = "2024-01-01",
            .updated_at = "2024-01-01",
        });
    }

    const results = try searchNotes(&db, alloc, "searchable", 10, 2);
    defer {
        for (results) |note| freeNoteFields(alloc, note);
        alloc.free(results);
    }

    try std.testing.expectEqual(@as(usize, 3), results.len);
}

test "getNotesForFile returns only matching file" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    try createNote(&db, .{
        .id = "note-a",
        .file_path = "/target.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "In target",
        .created_at = "2024-01-01",
        .updated_at = "2024-01-01",
    });
    try createNote(&db, .{
        .id = "note-b",
        .file_path = "/other.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "In other",
        .created_at = "2024-01-01",
        .updated_at = "2024-01-01",
    });

    const results = try getNotesForFile(&db, alloc, "/target.zig");
    defer {
        for (results) |note| freeNoteFields(alloc, note);
        alloc.free(results);
    }

    try std.testing.expectEqual(@as(usize, 1), results.len);
    try std.testing.expectEqualStrings("note-a", results[0].id);
}

test "addFile and getFile roundtrip" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    try addFile(&db, "/src/main.zig", "fn main() {}", "abc123hash");

    const file = try getFile(&db, alloc, "/src/main.zig");
    try std.testing.expect(file != null);
    defer {
        alloc.free(file.?.path);
        alloc.free(file.?.content);
        alloc.free(file.?.content_hash);
        alloc.free(file.?.indexed_at);
    }
    try std.testing.expectEqualStrings("/src/main.zig", file.?.path);
    try std.testing.expectEqualStrings("fn main() {}", file.?.content);
    try std.testing.expectEqualStrings("abc123hash", file.?.content_hash);
}

test "getFile returns null for missing file" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    const file = try getFile(&db, alloc, "/nonexistent.zig");
    try std.testing.expect(file == null);
}

test "searchFiles finds matching paths" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    try addFile(&db, "/src/main.zig", "fn main() {}", "hash1");
    try addFile(&db, "/src/lib.zig", "fn lib() {}", "hash2");
    try addFile(&db, "/test/main.zig", "fn test_main() {}", "hash3");

    const results = try searchFiles(&db, alloc, "main");
    defer {
        for (results) |f| {
            alloc.free(f.path);
            alloc.free(f.content);
            alloc.free(f.content_hash);
            alloc.free(f.indexed_at);
        }
        alloc.free(results);
    }

    try std.testing.expectEqual(@as(usize, 2), results.len);
}

test "exportNotesJson includes all notes" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    try createNote(&db, .{
        .id = "note-1",
        .file_path = "/a.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "First",
        .created_at = "2024-01-01",
        .updated_at = "2024-01-01",
    });
    try createNote(&db, .{
        .id = "note-2",
        .file_path = "/b.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "Second",
        .created_at = "2024-01-01",
        .updated_at = "2024-01-01",
    });

    const json = try exportNotesJson(&db, alloc);
    defer alloc.free(json);

    try std.testing.expect(std.mem.indexOf(u8, json, "note-1") != null);
    try std.testing.expect(std.mem.indexOf(u8, json, "note-2") != null);
    try std.testing.expect(std.mem.indexOf(u8, json, "First") != null);
    try std.testing.expect(std.mem.indexOf(u8, json, "Second") != null);
}

test "note unicode content roundtrip" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    try createNote(&db, .{
        .id = "unicode-note",
        .file_path = "/日本語.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "こんにちは世界 🎉",
        .created_at = "2024-01-01",
        .updated_at = "2024-01-01",
    });

    const note = try getNote(&db, alloc, "unicode-note");
    try std.testing.expect(note != null);
    defer {
        alloc.free(note.?.id);
        alloc.free(note.?.file_path);
        if (note.?.node_path) |np| alloc.free(np);
        if (note.?.node_text_hash) |h| alloc.free(h);
        alloc.free(note.?.content);
        alloc.free(note.?.created_at);
        alloc.free(note.?.updated_at);
    }
    try std.testing.expectEqualStrings("こんにちは世界 🎉", note.?.content);
}

test "note very long content roundtrip" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    const long_content = "x" ** 10000;
    try createNote(&db, .{
        .id = "long-note",
        .file_path = "/test.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = long_content,
        .created_at = "2024-01-01",
        .updated_at = "2024-01-01",
    });

    const note = try getNote(&db, alloc, "long-note");
    try std.testing.expect(note != null);
    defer {
        alloc.free(note.?.id);
        alloc.free(note.?.file_path);
        if (note.?.node_path) |np| alloc.free(np);
        if (note.?.node_text_hash) |h| alloc.free(h);
        alloc.free(note.?.content);
        alloc.free(note.?.created_at);
        alloc.free(note.?.updated_at);
    }
    try std.testing.expectEqual(@as(usize, 10000), note.?.content.len);
}

test "listProjectNotes returns all notes" {
    const alloc = std.testing.allocator;
    var db = try Database.open(alloc, ":memory:");
    defer db.close();

    try createNote(&db, .{
        .id = "note-a",
        .file_path = "/a.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "A",
        .created_at = "2024-01-01",
        .updated_at = "2024-01-01",
    });
    try createNote(&db, .{
        .id = "note-b",
        .file_path = "/b.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "B",
        .created_at = "2024-01-02",
        .updated_at = "2024-01-02",
    });

    const notes = try listProjectNotes(&db, alloc);
    defer {
        for (notes) |note| freeNoteFields(alloc, note);
        alloc.free(notes);
    }

    try std.testing.expectEqual(@as(usize, 2), notes.len);
}
