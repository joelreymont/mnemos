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
            \\    content TEXT NOT NULL,
            \\    created_at TEXT NOT NULL,
            \\    updated_at TEXT NOT NULL
            \\)
        );

        try self.exec(
            \\CREATE INDEX IF NOT EXISTS idx_notes_file ON notes(file_path)
        );

        try self.exec(
            \\CREATE TABLE IF NOT EXISTS files (
            \\    path TEXT PRIMARY KEY,
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
};

/// Note record
pub const Note = struct {
    id: []const u8,
    file_path: []const u8,
    node_path: ?[]const u8,
    node_text_hash: ?[]const u8,
    line_number: ?i64,
    content: []const u8,
    created_at: []const u8,
    updated_at: []const u8,
};

/// Create a new note
pub fn createNote(db: *Database, note: Note) !void {
    var stmt = try db.prepare(
        \\INSERT INTO notes (id, file_path, node_path, node_text_hash, line_number, content, created_at, updated_at)
        \\VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8)
    );
    defer stmt.deinit();

    try stmt.bindText(1, note.id);
    try stmt.bindText(2, note.file_path);
    if (note.node_path) |np| try stmt.bindText(3, np) else try stmt.bindNull(3);
    if (note.node_text_hash) |h| try stmt.bindText(4, h) else try stmt.bindNull(4);
    if (note.line_number) |ln| try stmt.bindInt(5, ln) else try stmt.bindNull(5);
    try stmt.bindText(6, note.content);
    try stmt.bindText(7, note.created_at);
    try stmt.bindText(8, note.updated_at);

    _ = try stmt.step();
}

/// List notes for a project (all notes)
pub fn listProjectNotes(db: *Database, alloc: Allocator) ![]Note {
    var stmt = try db.prepare(
        \\SELECT id, file_path, node_path, node_text_hash, line_number, content, created_at, updated_at
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
            .line_number = if (stmt.columnInt(4) != 0) stmt.columnInt(4) else null,
            .content = try alloc.dupe(u8, stmt.columnText(5) orelse ""),
            .created_at = try alloc.dupe(u8, stmt.columnText(6) orelse ""),
            .updated_at = try alloc.dupe(u8, stmt.columnText(7) orelse ""),
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

/// Get a note by ID
pub fn getNote(db: *Database, alloc: Allocator, id: []const u8) !?Note {
    var stmt = try db.prepare(
        \\SELECT id, file_path, node_path, node_text_hash, line_number, content, created_at, updated_at
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
            .line_number = if (stmt.columnInt(4) != 0) stmt.columnInt(4) else null,
            .content = try alloc.dupe(u8, stmt.columnText(5) orelse ""),
            .created_at = try alloc.dupe(u8, stmt.columnText(6) orelse ""),
            .updated_at = try alloc.dupe(u8, stmt.columnText(7) orelse ""),
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

/// Search notes by query
pub fn searchNotes(db: *Database, alloc: Allocator, query: []const u8, limit: i64, offset: i64) ![]Note {
    var stmt = try db.prepare(
        \\SELECT id, file_path, node_path, node_text_hash, line_number, content, created_at, updated_at
        \\FROM notes WHERE content LIKE ?1 ORDER BY created_at DESC LIMIT ?2 OFFSET ?3
    );
    defer stmt.deinit();

    const pattern = try std.fmt.allocPrint(alloc, "%{s}%", .{query});
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
            .line_number = if (stmt.columnInt(4) != 0) stmt.columnInt(4) else null,
            .content = try alloc.dupe(u8, stmt.columnText(5) orelse ""),
            .created_at = try alloc.dupe(u8, stmt.columnText(6) orelse ""),
            .updated_at = try alloc.dupe(u8, stmt.columnText(7) orelse ""),
        };
        try notes.append(alloc, note);
    }

    return notes.toOwnedSlice(alloc);
}

/// Get notes for a file
pub fn getNotesForFile(db: *Database, alloc: Allocator, file_path: []const u8) ![]Note {
    var stmt = try db.prepare(
        \\SELECT id, file_path, node_path, node_text_hash, line_number, content, created_at, updated_at
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
            .line_number = if (stmt.columnInt(4) != 0) stmt.columnInt(4) else null,
            .content = try alloc.dupe(u8, stmt.columnText(5) orelse ""),
            .created_at = try alloc.dupe(u8, stmt.columnText(6) orelse ""),
            .updated_at = try alloc.dupe(u8, stmt.columnText(7) orelse ""),
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
    content_hash: []const u8,
    indexed_at: []const u8,
};

/// Add or update a file in the index
pub fn addFile(db: *Database, path: []const u8, content_hash: []const u8) !void {
    var ts_buf: [32]u8 = undefined;
    const ts = std.time.timestamp();
    const timestamp = std.fmt.bufPrint(&ts_buf, "{d}", .{ts}) catch "0";

    var stmt = try db.prepare(
        \\INSERT INTO files (path, content_hash, indexed_at)
        \\VALUES (?1, ?2, ?3)
        \\ON CONFLICT(path) DO UPDATE SET content_hash = ?2, indexed_at = ?3
    );
    defer stmt.deinit();

    try stmt.bindText(1, path);
    try stmt.bindText(2, content_hash);
    try stmt.bindText(3, timestamp);

    _ = try stmt.step();
}

/// Get a file record by path
pub fn getFile(db: *Database, alloc: Allocator, path: []const u8) !?File {
    var stmt = try db.prepare(
        \\SELECT path, content_hash, indexed_at FROM files WHERE path = ?1
    );
    defer stmt.deinit();

    try stmt.bindText(1, path);

    if (try stmt.step()) {
        return File{
            .path = try alloc.dupe(u8, stmt.columnText(0) orelse ""),
            .content_hash = try alloc.dupe(u8, stmt.columnText(1) orelse ""),
            .indexed_at = try alloc.dupe(u8, stmt.columnText(2) orelse ""),
        };
    }

    return null;
}

/// Export all notes as JSON
pub fn exportNotesJson(db: *Database, alloc: Allocator) ![]const u8 {
    const notes = try listProjectNotes(db, alloc);
    defer {
        for (notes) |note| {
            alloc.free(note.id);
            alloc.free(note.file_path);
            if (note.node_path) |np| alloc.free(np);
            if (note.node_text_hash) |h| alloc.free(h);
            alloc.free(note.content);
            alloc.free(note.created_at);
            alloc.free(note.updated_at);
        }
        alloc.free(notes);
    }

    var buf: std.ArrayList(u8) = .{};
    errdefer buf.deinit(alloc);

    try buf.appendSlice(alloc, "[");
    for (notes, 0..) |note, i| {
        if (i > 0) try buf.appendSlice(alloc, ",");
        const item = try std.fmt.allocPrint(alloc,
            \\{{"id":"{s}","filePath":"{s}","content":"{s}"}}
        , .{ note.id, note.file_path, note.content });
        defer alloc.free(item);
        try buf.appendSlice(alloc, item);
    }
    try buf.appendSlice(alloc, "]");

    return buf.toOwnedSlice(alloc);
}

/// Search files by path pattern
pub fn searchFiles(db: *Database, alloc: Allocator, query: []const u8) ![]File {
    var stmt = try db.prepare(
        \\SELECT path, content_hash, indexed_at FROM files
        \\WHERE path LIKE ?1 ORDER BY indexed_at DESC
    );
    defer stmt.deinit();

    const pattern = try std.fmt.allocPrint(alloc, "%{s}%", .{query});
    defer alloc.free(pattern);

    try stmt.bindText(1, pattern);

    var files: std.ArrayList(File) = .{};
    errdefer files.deinit(alloc);

    while (try stmt.step()) {
        const file = File{
            .path = try alloc.dupe(u8, stmt.columnText(0) orelse ""),
            .content_hash = try alloc.dupe(u8, stmt.columnText(1) orelse ""),
            .indexed_at = try alloc.dupe(u8, stmt.columnText(2) orelse ""),
        };
        try files.append(alloc, file);
    }

    return files.toOwnedSlice(alloc);
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

    try addFile(&db, "/src/main.zig", "hash123");
    try addFile(&db, "/src/lib.zig", "hash456");

    const file = try getFile(&db, alloc, "/src/main.zig");
    try std.testing.expect(file != null);
    defer {
        alloc.free(file.?.path);
        alloc.free(file.?.content_hash);
        alloc.free(file.?.indexed_at);
    }
    try std.testing.expectEqualStrings("hash123", file.?.content_hash);

    // Update existing
    try addFile(&db, "/src/main.zig", "newhash");
    const updated = try getFile(&db, alloc, "/src/main.zig");
    defer {
        alloc.free(updated.?.path);
        alloc.free(updated.?.content_hash);
        alloc.free(updated.?.indexed_at);
    }
    try std.testing.expectEqualStrings("newhash", updated.?.content_hash);

    // Search
    const results = try searchFiles(&db, alloc, "zig");
    defer {
        for (results) |f| {
            alloc.free(f.path);
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
