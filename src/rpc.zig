//! JSON-RPC 2.0 framing and dispatch
//!
//! Supports two framing modes:
//! - Content-Length (LSP-style): "Content-Length: N\r\n\r\n{json}"
//! - Newline-delimited: "{json}\n"

const std = @import("std");
const mem = std.mem;
const posix = std.posix;

const Allocator = mem.Allocator;
const storage = @import("storage.zig");
const git = @import("git.zig");
const treesitter = @import("treesitter.zig");

/// RPC stream for reading/writing framed messages
pub const Stream = struct {
    read_fd: posix.fd_t,
    write_fd: posix.fd_t,
    buf: []u8,
    buf_len: usize = 0,

    pub fn init(read_fd: posix.fd_t, write_fd: posix.fd_t, buf: []u8) Stream {
        return .{
            .read_fd = read_fd,
            .write_fd = write_fd,
            .buf = buf,
        };
    }

    /// Read a complete request (Content-Length framed or newline-delimited)
    pub fn readRequest(self: *Stream, alloc: Allocator) ![]u8 {
        // Try to read more data if buffer is low
        if (self.buf_len < 20) {
            const n = try posix.read(self.read_fd, self.buf[self.buf_len..]);
            if (n == 0) return error.EndOfStream;
            self.buf_len += n;
        }

        const data = self.buf[0..self.buf_len];

        // Check for Content-Length header
        if (mem.startsWith(u8, data, "Content-Length:")) {
            return self.readContentLength(alloc);
        }

        // Fall back to newline-delimited
        return self.readNewlineDelimited(alloc);
    }

    fn readContentLength(self: *Stream, alloc: Allocator) ![]u8 {
        // Find header end
        while (true) {
            const data = self.buf[0..self.buf_len];
            if (mem.indexOf(u8, data, "\r\n\r\n")) |header_end| {
                const header = data[0..header_end];

                // Parse Content-Length
                const cl_start = (mem.indexOf(u8, header, "Content-Length:") orelse return error.InvalidHeader) + 15;
                const cl_end = mem.indexOfAny(u8, header[cl_start..], "\r\n") orelse (header.len - cl_start);
                const cl_str = mem.trim(u8, header[cl_start..][0..cl_end], " \t");
                const content_len = std.fmt.parseInt(usize, cl_str, 10) catch return error.InvalidHeader;

                const body_start = header_end + 4;
                const total_needed = body_start + content_len;

                // Read until we have the full body
                while (self.buf_len < total_needed) {
                    const n = try posix.read(self.read_fd, self.buf[self.buf_len..]);
                    if (n == 0) return error.EndOfStream;
                    self.buf_len += n;
                }

                // Copy body and shift buffer
                const body = try alloc.dupe(u8, self.buf[body_start..total_needed]);
                const remaining = self.buf_len - total_needed;
                if (remaining > 0) {
                    mem.copyForwards(u8, self.buf[0..remaining], self.buf[total_needed..self.buf_len]);
                }
                self.buf_len = remaining;

                return body;
            }

            // Need more data for header
            const n = try posix.read(self.read_fd, self.buf[self.buf_len..]);
            if (n == 0) return error.EndOfStream;
            self.buf_len += n;
        }
    }

    fn readNewlineDelimited(self: *Stream, alloc: Allocator) ![]u8 {
        while (true) {
            const data = self.buf[0..self.buf_len];
            if (mem.indexOf(u8, data, "\n")) |newline| {
                // Copy line (excluding newline)
                var end = newline;
                if (end > 0 and data[end - 1] == '\r') end -= 1;
                const line = try alloc.dupe(u8, data[0..end]);

                // Shift buffer
                const remaining = self.buf_len - newline - 1;
                if (remaining > 0) {
                    mem.copyForwards(u8, self.buf[0..remaining], self.buf[newline + 1 .. self.buf_len]);
                }
                self.buf_len = remaining;

                return line;
            }

            // Need more data
            const n = try posix.read(self.read_fd, self.buf[self.buf_len..]);
            if (n == 0) return error.EndOfStream;
            self.buf_len += n;
        }
    }

    /// Write a response with Content-Length framing
    pub fn writeResponse(self: *Stream, response: []const u8) !void {
        var header_buf: [64]u8 = undefined;
        const header = std.fmt.bufPrint(&header_buf, "Content-Length: {d}\r\n\r\n", .{response.len}) catch unreachable;

        _ = try posix.write(self.write_fd, header);
        _ = try posix.write(self.write_fd, response);
    }
};

/// Dispatch a JSON-RPC request and return response
pub fn dispatch(alloc: Allocator, request: []const u8) []u8 {
    return dispatchWithDb(alloc, request, null);
}

/// Dispatch with database connection
pub fn dispatchWithDb(alloc: Allocator, request: []const u8, db: ?*storage.Database) []u8 {
    // Simple JSON parsing - extract method and id
    const method = extractString(request, "\"method\"") orelse {
        return makeError(alloc, extractId(request), -32600, "Invalid Request: missing method");
    };

    const id = extractId(request);

    // Dispatch to handler
    const result = handleMethod(alloc, method, request, db) catch |err| {
        return makeError(alloc, id, -32603, @errorName(err));
    };
    defer alloc.free(result);

    return makeResult(alloc, id, result);
}

fn handleMethod(alloc: Allocator, method: []const u8, request: []const u8, db: ?*storage.Database) ![]const u8 {
    if (mem.eql(u8, method, "hemis/status")) {
        return handleStatus(alloc, db);
    } else if (mem.eql(u8, method, "hemis/shutdown")) {
        std.process.exit(0);
    } else if (mem.eql(u8, method, "notes/create")) {
        return handleNotesCreate(alloc, request, db);
    } else if (mem.eql(u8, method, "notes/list-project")) {
        return handleNotesListProject(alloc, db);
    } else if (mem.eql(u8, method, "notes/get")) {
        return handleNotesGet(alloc, request, db);
    } else if (mem.eql(u8, method, "notes/delete")) {
        return handleNotesDelete(alloc, request, db);
    } else if (mem.eql(u8, method, "notes/update")) {
        return handleNotesUpdate(alloc, request, db);
    } else if (mem.eql(u8, method, "notes/search")) {
        return handleNotesSearch(alloc, request, db);
    } else if (mem.eql(u8, method, "notes/backlinks")) {
        return handleNotesBacklinks(alloc, request, db);
    } else if (mem.eql(u8, method, "notes/anchor")) {
        return handleNotesAnchor(alloc, request, db);
    } else if (mem.eql(u8, method, "notes/list-for-file")) {
        return handleNotesListForFile(alloc, request, db);
    } else if (mem.eql(u8, method, "notes/list-by-node")) {
        return handleNotesListByNode(alloc, request, db);
    } else if (mem.eql(u8, method, "notes/get-at-position")) {
        return handleNotesGetAtPosition(alloc, request, db);
    } else if (mem.eql(u8, method, "notes/buffer-update")) {
        return handleNotesBufferUpdate(alloc, request, db);
    } else if (mem.eql(u8, method, "hemis/open-project")) {
        return handleOpenProject(alloc);
    } else if (mem.eql(u8, method, "hemis/search")) {
        return handleHemisSearch(alloc, request, db);
    } else if (mem.eql(u8, method, "hemis/index-project")) {
        return handleIndexProject(alloc);
    } else if (mem.eql(u8, method, "hemis/project-meta")) {
        return handleProjectMeta(alloc, db);
    } else if (mem.eql(u8, method, "hemis/save-snapshot")) {
        return handleSaveSnapshot(alloc, request, db);
    } else if (mem.eql(u8, method, "hemis/load-snapshot")) {
        return handleLoadSnapshot(alloc, request, db);
    } else if (mem.eql(u8, method, "hemis/file-context")) {
        return handleFileContext(alloc, request, db);
    } else if (mem.eql(u8, method, "hemis/explain-region")) {
        return handleExplainRegion(alloc, request);
    } else if (mem.eql(u8, method, "hemis/buffer-context")) {
        return handleBufferContext(alloc, request, db);
    } else if (mem.eql(u8, method, "hemis/graph")) {
        return handleGraph(alloc, db);
    } else if (mem.eql(u8, method, "hemis/tasks")) {
        return handleTasks(alloc);
    } else if (mem.eql(u8, method, "hemis/task-status")) {
        return handleTaskStatus(alloc, request);
    } else if (mem.eql(u8, method, "hemis/task-list")) {
        return handleTaskList(alloc);
    } else if (mem.eql(u8, method, "index/add-file")) {
        return handleIndexAddFile(alloc, request, db);
    } else if (mem.eql(u8, method, "index/search")) {
        return handleIndexSearch(alloc, request, db);
    }

    return error.MethodNotFound;
}

fn handleStatus(alloc: Allocator, db: ?*storage.Database) ![]const u8 {
    const note_count: i64 = if (db) |d| storage.countNotes(d) catch 0 else 0;

    // Try to get git info
    var repo = git.Repository.open(".") catch null;
    defer if (repo) |*r| r.deinit();

    const branch = if (repo) |*r| r.getCurrentBranch(alloc) catch null else null;
    defer if (branch) |b| alloc.free(b);
    const branch_str = branch orelse "unknown";

    return try std.fmt.allocPrint(alloc,
        \\{{"version":"0.1.0","language":"zig","status":"ok","noteCount":{d},"gitBranch":"{s}"}}
    , .{ note_count, branch_str });
}

fn handleNotesCreate(alloc: Allocator, request: []const u8, db: ?*storage.Database) ![]const u8 {
    const database = db orelse return error.NoDatabaseConnection;

    // Extract params
    const file_path = extractNestedString(request, "\"params\"", "\"filePath\"") orelse
        return error.MissingFilePath;
    const content = extractNestedString(request, "\"params\"", "\"content\"") orelse
        return error.MissingContent;
    const node_path = extractNestedString(request, "\"params\"", "\"nodePath\"");

    // Generate UUID-like ID
    var id_buf: [36]u8 = undefined;
    const id = generateId(&id_buf);

    // Get current timestamp
    var ts_buf: [32]u8 = undefined;
    const timestamp = getTimestamp(&ts_buf);

    const note = storage.Note{
        .id = id,
        .file_path = file_path,
        .node_path = node_path,
        .node_text_hash = null,
        .line_number = null,
        .content = content,
        .created_at = timestamp,
        .updated_at = timestamp,
    };

    try storage.createNote(database, note);

    return try std.fmt.allocPrint(alloc,
        \\{{"id":"{s}","filePath":"{s}","content":"{s}"}}
    , .{ id, file_path, escapeJson(alloc, content) catch content });
}

fn handleNotesListProject(alloc: Allocator, db: ?*storage.Database) ![]const u8 {
    const database = db orelse return error.NoDatabaseConnection;

    const notes = try storage.listProjectNotes(database, alloc);
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

    // Build JSON array
    var buf: std.ArrayList(u8) = .{};
    errdefer buf.deinit(alloc);

    try buf.appendSlice(alloc, "[");
    for (notes, 0..) |note, i| {
        if (i > 0) try buf.appendSlice(alloc, ",");
        const escaped_content = escapeJson(alloc, note.content) catch note.content;
        defer if (escaped_content.ptr != note.content.ptr) alloc.free(escaped_content);

        const item = try std.fmt.allocPrint(alloc,
            \\{{"id":"{s}","filePath":"{s}","content":"{s}"}}
        , .{ note.id, note.file_path, escaped_content });
        defer alloc.free(item);
        try buf.appendSlice(alloc, item);
    }
    try buf.appendSlice(alloc, "]");

    return buf.toOwnedSlice(alloc);
}

fn handleNotesGet(alloc: Allocator, request: []const u8, db: ?*storage.Database) ![]const u8 {
    const database = db orelse return error.NoDatabaseConnection;

    const id = extractNestedString(request, "\"params\"", "\"id\"") orelse
        return error.MissingId;

    const note_opt = try storage.getNote(database, alloc, id);
    if (note_opt) |note| {
        defer {
            alloc.free(note.id);
            alloc.free(note.file_path);
            if (note.node_path) |np| alloc.free(np);
            if (note.node_text_hash) |h| alloc.free(h);
            alloc.free(note.content);
            alloc.free(note.created_at);
            alloc.free(note.updated_at);
        }

        const escaped_content = escapeJson(alloc, note.content) catch note.content;
        defer if (escaped_content.ptr != note.content.ptr) alloc.free(escaped_content);

        return try std.fmt.allocPrint(alloc,
            \\{{"id":"{s}","filePath":"{s}","content":"{s}"}}
        , .{ note.id, note.file_path, escaped_content });
    }

    return error.NoteNotFound;
}

fn handleNotesDelete(alloc: Allocator, request: []const u8, db: ?*storage.Database) ![]const u8 {
    const database = db orelse return error.NoDatabaseConnection;

    const id = extractNestedString(request, "\"params\"", "\"id\"") orelse
        return error.MissingId;

    try storage.deleteNote(database, id);

    return try std.fmt.allocPrint(alloc, "{{\"ok\":true}}", .{});
}

fn handleNotesUpdate(alloc: Allocator, request: []const u8, db: ?*storage.Database) ![]const u8 {
    const database = db orelse return error.NoDatabaseConnection;

    const id = extractNestedString(request, "\"params\"", "\"id\"") orelse
        return error.MissingId;
    const text = extractNestedString(request, "\"params\"", "\"text\"");
    const tags = extractNestedString(request, "\"params\"", "\"tags\"");

    var ts_buf: [32]u8 = undefined;
    const timestamp = getTimestamp(&ts_buf);

    try storage.updateNote(database, id, text, tags, timestamp);

    const note_opt = try storage.getNote(database, alloc, id);
    if (note_opt) |note| {
        defer {
            alloc.free(note.id);
            alloc.free(note.file_path);
            if (note.node_path) |np| alloc.free(np);
            if (note.node_text_hash) |h| alloc.free(h);
            alloc.free(note.content);
            alloc.free(note.created_at);
            alloc.free(note.updated_at);
        }

        const escaped_content = escapeJson(alloc, note.content) catch note.content;
        defer if (escaped_content.ptr != note.content.ptr) alloc.free(escaped_content);

        return try std.fmt.allocPrint(alloc,
            \\{{"id":"{s}","filePath":"{s}","content":"{s}"}}
        , .{ note.id, note.file_path, escaped_content });
    }

    return error.NoteNotFound;
}

fn handleNotesSearch(alloc: Allocator, request: []const u8, db: ?*storage.Database) ![]const u8 {
    const database = db orelse return error.NoDatabaseConnection;

    const query = extractNestedString(request, "\"params\"", "\"query\"") orelse
        return error.MissingQuery;
    const limit = extractNestedInt(request, "\"params\"", "\"limit\"") orelse 50;
    const offset = extractNestedInt(request, "\"params\"", "\"offset\"") orelse 0;

    const notes = try storage.searchNotes(database, alloc, query, limit, offset);
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

    return formatNotesArray(alloc, notes);
}

fn handleNotesBacklinks(alloc: Allocator, request: []const u8, db: ?*storage.Database) ![]const u8 {
    _ = request;
    _ = db;
    return try std.fmt.allocPrint(alloc, "[]", .{});
}

fn handleNotesAnchor(alloc: Allocator, request: []const u8, db: ?*storage.Database) ![]const u8 {
    _ = request;
    _ = db;
    return try std.fmt.allocPrint(alloc, "{{\"line\":0,\"column\":0}}", .{});
}

fn handleNotesListForFile(alloc: Allocator, request: []const u8, db: ?*storage.Database) ![]const u8 {
    const database = db orelse return error.NoDatabaseConnection;

    const file_path = extractNestedString(request, "\"params\"", "\"file\"") orelse
        return error.MissingFile;

    const notes = try storage.getNotesForFile(database, alloc, file_path);
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

    return formatNotesArray(alloc, notes);
}

fn handleNotesListByNode(alloc: Allocator, request: []const u8, db: ?*storage.Database) ![]const u8 {
    _ = request;
    _ = db;
    return try std.fmt.allocPrint(alloc, "[]", .{});
}

fn handleNotesGetAtPosition(alloc: Allocator, request: []const u8, db: ?*storage.Database) ![]const u8 {
    _ = request;
    _ = db;
    return try std.fmt.allocPrint(alloc, "null", .{});
}

fn handleNotesBufferUpdate(alloc: Allocator, request: []const u8, db: ?*storage.Database) ![]const u8 {
    _ = request;
    _ = db;
    return try std.fmt.allocPrint(alloc, "{{\"ok\":true}}", .{});
}

// hemis/* handlers

fn handleOpenProject(alloc: Allocator) ![]const u8 {
    return try std.fmt.allocPrint(alloc, "{{\"ok\":true}}", .{});
}

fn handleHemisSearch(alloc: Allocator, request: []const u8, db: ?*storage.Database) ![]const u8 {
    const database = db orelse return error.NoDatabaseConnection;

    const query = extractNestedString(request, "\"params\"", "\"query\"") orelse
        return error.MissingQuery;

    const notes = try storage.searchNotes(database, alloc, query, 50, 0);
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
    try buf.appendSlice(alloc, "{\"results\":[");
    for (notes, 0..) |note, i| {
        if (i > 0) try buf.appendSlice(alloc, ",");
        const escaped_content = escapeJson(alloc, note.content) catch note.content;
        defer if (escaped_content.ptr != note.content.ptr) alloc.free(escaped_content);

        const item = try std.fmt.allocPrint(alloc,
            \\{{"id":"{s}","filePath":"{s}","content":"{s}"}}
        , .{ note.id, note.file_path, escaped_content });
        defer alloc.free(item);
        try buf.appendSlice(alloc, item);
    }
    try buf.appendSlice(alloc, "]}");

    return buf.toOwnedSlice(alloc);
}

fn handleIndexProject(alloc: Allocator) ![]const u8 {
    return try std.fmt.allocPrint(alloc, "{{\"ok\":true,\"indexed\":0}}", .{});
}

fn handleProjectMeta(alloc: Allocator, db: ?*storage.Database) ![]const u8 {
    const note_count: i64 = if (db) |d| storage.countNotes(d) catch 0 else 0;

    // Try to get git info
    var repo = git.Repository.open(".") catch null;
    defer if (repo) |*r| r.deinit();

    const branch = if (repo) |*r| r.getCurrentBranch(alloc) catch null else null;
    defer if (branch) |b| alloc.free(b);
    const branch_str = branch orelse "unknown";

    const commit = if (repo) |*r| r.getHeadCommit(alloc) catch null else null;
    defer if (commit) |c| alloc.free(c);
    const commit_str = commit orelse "unknown";

    return try std.fmt.allocPrint(alloc,
        \\{{"noteCount":{d},"indexed":0,"gitBranch":"{s}","gitCommit":"{s}"}}
    , .{ note_count, branch_str, commit_str });
}

fn handleSaveSnapshot(alloc: Allocator, request: []const u8, db: ?*storage.Database) ![]const u8 {
    const database = db orelse return error.NoDatabaseConnection;

    const path = extractNestedString(request, "\"params\"", "\"path\"") orelse
        return error.MissingPath;

    const json = try storage.exportNotesJson(database, alloc);
    defer alloc.free(json);

    const path_z = try alloc.dupeZ(u8, path);
    defer alloc.free(path_z);

    const file = try std.fs.cwd().createFile(path_z, .{});
    defer file.close();

    try file.writeAll(json);

    return try std.fmt.allocPrint(alloc, "{{\"ok\":true,\"path\":\"{s}\"}}", .{path});
}

fn handleLoadSnapshot(alloc: Allocator, request: []const u8, db: ?*storage.Database) ![]const u8 {
    const database = db orelse return error.NoDatabaseConnection;

    const path = extractNestedString(request, "\"params\"", "\"path\"") orelse
        return error.MissingPath;

    const path_z = try alloc.dupeZ(u8, path);
    defer alloc.free(path_z);

    const file = try std.fs.cwd().openFile(path_z, .{});
    defer file.close();

    const json = try file.readToEndAlloc(alloc, 100 * 1024 * 1024);
    defer alloc.free(json);

    try storage.clearNotes(database);

    return try std.fmt.allocPrint(alloc, "{{\"ok\":true,\"path\":\"{s}\"}}", .{path});
}

fn handleFileContext(alloc: Allocator, request: []const u8, db: ?*storage.Database) ![]const u8 {
    const database = db orelse return error.NoDatabaseConnection;

    const file = extractNestedString(request, "\"params\"", "\"file\"") orelse
        return error.MissingFile;

    const notes = try storage.getNotesForFile(database, alloc, file);
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
    const file_part = try std.fmt.allocPrint(alloc, "{{\"file\":\"{s}\",\"notes\":[", .{file});
    defer alloc.free(file_part);
    try buf.appendSlice(alloc, file_part);

    for (notes, 0..) |note, i| {
        if (i > 0) try buf.appendSlice(alloc, ",");
        const escaped_content = escapeJson(alloc, note.content) catch note.content;
        defer if (escaped_content.ptr != note.content.ptr) alloc.free(escaped_content);

        const item = try std.fmt.allocPrint(alloc,
            \\{{"id":"{s}","content":"{s}"}}
        , .{ note.id, escaped_content });
        defer alloc.free(item);
        try buf.appendSlice(alloc, item);
    }
    try buf.appendSlice(alloc, "]}");

    return buf.toOwnedSlice(alloc);
}

fn handleExplainRegion(alloc: Allocator, request: []const u8) ![]const u8 {
    const file = extractNestedString(request, "\"params\"", "\"file\"") orelse
        return error.MissingFile;

    return try std.fmt.allocPrint(alloc,
        \\{{"file":"{s}","info":"Region info"}}
    , .{file});
}

fn handleBufferContext(alloc: Allocator, request: []const u8, db: ?*storage.Database) ![]const u8 {
    const database = db orelse return error.NoDatabaseConnection;

    const file = extractNestedString(request, "\"params\"", "\"file\"") orelse
        return error.MissingFile;

    const note_count: i64 = storage.countNotes(database) catch 0;

    return try std.fmt.allocPrint(alloc,
        \\{{"file":"{s}","noteCount":{d}}}
    , .{ file, note_count });
}

fn handleGraph(alloc: Allocator, db: ?*storage.Database) ![]const u8 {
    const database = db orelse return error.NoDatabaseConnection;

    const notes = try storage.listProjectNotes(database, alloc);
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
    try buf.appendSlice(alloc, "{\"nodes\":[");
    for (notes, 0..) |note, i| {
        if (i > 0) try buf.appendSlice(alloc, ",");
        const item = try std.fmt.allocPrint(alloc,
            \\{{"id":"{s}","label":"{s}"}}
        , .{ note.id, note.file_path });
        defer alloc.free(item);
        try buf.appendSlice(alloc, item);
    }
    try buf.appendSlice(alloc, "],\"edges\":[]}");

    return buf.toOwnedSlice(alloc);
}

fn handleTasks(alloc: Allocator) ![]const u8 {
    return try std.fmt.allocPrint(alloc, "{{\"tasks\":[]}}", .{});
}

fn handleTaskStatus(alloc: Allocator, request: []const u8) ![]const u8 {
    const task_id = extractNestedString(request, "\"params\"", "\"taskId\"") orelse
        return error.MissingTaskId;

    return try std.fmt.allocPrint(alloc,
        \\{{"taskId":"{s}","status":"unknown"}}
    , .{task_id});
}

fn handleTaskList(alloc: Allocator) ![]const u8 {
    return try std.fmt.allocPrint(alloc, "{{\"tasks\":[]}}", .{});
}

// index/* handlers

fn handleIndexAddFile(alloc: Allocator, request: []const u8, db: ?*storage.Database) ![]const u8 {
    const database = db orelse return error.NoDatabaseConnection;

    const file_path = extractNestedString(request, "\"params\"", "\"file\"") orelse
        return error.MissingFile;
    const content = extractNestedString(request, "\"params\"", "\"content\"") orelse
        return error.MissingContent;

    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update(content);
    var hash_buf: [32]u8 = undefined;
    hasher.final(&hash_buf);

    const hex_chars = "0123456789abcdef";
    var hash_hex: [64]u8 = undefined;
    for (hash_buf, 0..) |byte, i| {
        hash_hex[i * 2] = hex_chars[byte >> 4];
        hash_hex[i * 2 + 1] = hex_chars[byte & 0x0F];
    }

    try storage.addFile(database, file_path, &hash_hex);

    return try std.fmt.allocPrint(alloc,
        \\{{"file":"{s}","contentHash":"{s}"}}
    , .{ file_path, &hash_hex });
}

fn handleIndexSearch(alloc: Allocator, request: []const u8, db: ?*storage.Database) ![]const u8 {
    const database = db orelse return error.NoDatabaseConnection;

    const query = extractNestedString(request, "\"params\"", "\"query\"") orelse
        return error.MissingQuery;

    const files = try storage.searchFiles(database, alloc, query);
    defer {
        for (files) |file| {
            alloc.free(file.path);
            alloc.free(file.content_hash);
            alloc.free(file.indexed_at);
        }
        alloc.free(files);
    }

    var buf: std.ArrayList(u8) = .{};
    errdefer buf.deinit(alloc);

    try buf.appendSlice(alloc, "[");
    for (files, 0..) |file, i| {
        if (i > 0) try buf.appendSlice(alloc, ",");
        const item = try std.fmt.allocPrint(alloc,
            \\{{"path":"{s}","contentHash":"{s}","indexedAt":"{s}"}}
        , .{ file.path, file.content_hash, file.indexed_at });
        defer alloc.free(item);
        try buf.appendSlice(alloc, item);
    }
    try buf.appendSlice(alloc, "]");

    return buf.toOwnedSlice(alloc);
}

fn formatNotesArray(alloc: Allocator, notes: []const storage.Note) ![]const u8 {
    var buf: std.ArrayList(u8) = .{};
    errdefer buf.deinit(alloc);

    try buf.appendSlice(alloc, "[");
    for (notes, 0..) |note, i| {
        if (i > 0) try buf.appendSlice(alloc, ",");
        const escaped_content = escapeJson(alloc, note.content) catch note.content;
        defer if (escaped_content.ptr != note.content.ptr) alloc.free(escaped_content);

        const item = try std.fmt.allocPrint(alloc,
            \\{{"id":"{s}","filePath":"{s}","content":"{s}"}}
        , .{ note.id, note.file_path, escaped_content });
        defer alloc.free(item);
        try buf.appendSlice(alloc, item);
    }
    try buf.appendSlice(alloc, "]");

    return buf.toOwnedSlice(alloc);
}

/// Extract nested string value
fn extractNestedString(json_str: []const u8, outer_key: []const u8, inner_key: []const u8) ?[]const u8 {
    const outer_pos = mem.indexOf(u8, json_str, outer_key) orelse return null;
    const after_outer = json_str[outer_pos..];
    return extractString(after_outer, inner_key);
}

/// Extract nested int value
fn extractNestedInt(json_str: []const u8, outer_key: []const u8, inner_key: []const u8) ?i64 {
    const outer_pos = mem.indexOf(u8, json_str, outer_key) orelse return null;
    const after_outer = json_str[outer_pos..];
    const key_pos = mem.indexOf(u8, after_outer, inner_key) orelse return null;
    const after_key = after_outer[key_pos + inner_key.len ..];

    const colon = mem.indexOf(u8, after_key, ":") orelse return null;
    const after_colon = mem.trimLeft(u8, after_key[colon + 1 ..], " \t\n\r");

    var end: usize = 0;
    for (after_colon, 0..) |c, i| {
        if (c == ',' or c == '}' or c == ' ' or c == '\t' or c == '\n' or c == '\r') {
            end = i;
            break;
        }
        end = i + 1;
    }

    const num_str = after_colon[0..end];
    return std.fmt.parseInt(i64, num_str, 10) catch null;
}

/// Generate a simple ID (timestamp + random suffix)
fn generateId(buf: *[36]u8) []const u8 {
    const ts = std.time.timestamp();
    const rand = @as(u64, @truncate(@as(u128, @bitCast(std.time.nanoTimestamp()))));
    _ = std.fmt.bufPrint(buf, "{x:0>16}-{x:0>8}", .{ ts, @as(u32, @truncate(rand)) }) catch return "unknown";
    return buf[0..25];
}

/// Get ISO timestamp
fn getTimestamp(buf: *[32]u8) []const u8 {
    const ts = std.time.timestamp();
    const secs: u64 = @intCast(ts);
    // Simple ISO format: seconds since epoch (good enough for now)
    _ = std.fmt.bufPrint(buf, "{d}", .{secs}) catch return "0";
    var len: usize = 0;
    for (buf) |c| {
        if (c == 0) break;
        len += 1;
    }
    return buf[0..len];
}

/// Escape JSON string
fn escapeJson(alloc: Allocator, s: []const u8) ![]const u8 {
    var needs_escape = false;
    for (s) |c| {
        if (c == '"' or c == '\\' or c == '\n' or c == '\r' or c == '\t') {
            needs_escape = true;
            break;
        }
    }
    if (!needs_escape) return s;

    var buf: std.ArrayList(u8) = .{};
    for (s) |c| {
        switch (c) {
            '"' => try buf.appendSlice(alloc, "\\\""),
            '\\' => try buf.appendSlice(alloc, "\\\\"),
            '\n' => try buf.appendSlice(alloc, "\\n"),
            '\r' => try buf.appendSlice(alloc, "\\r"),
            '\t' => try buf.appendSlice(alloc, "\\t"),
            else => try buf.append(alloc, c),
        }
    }
    return buf.toOwnedSlice(alloc);
}

/// Extract a string value for a given key from JSON
fn extractString(json_str: []const u8, key: []const u8) ?[]const u8 {
    const key_pos = mem.indexOf(u8, json_str, key) orelse return null;
    const after_key = json_str[key_pos + key.len ..];

    // Find the colon
    const colon = mem.indexOf(u8, after_key, ":") orelse return null;
    const after_colon = mem.trimLeft(u8, after_key[colon + 1 ..], " \t\n\r");

    // Must start with quote
    if (after_colon.len == 0 or after_colon[0] != '"') return null;

    // Find end quote
    const end = mem.indexOf(u8, after_colon[1..], "\"") orelse return null;
    return after_colon[1..][0..end];
}

/// Extract the id field (as raw JSON)
fn extractId(json_str: []const u8) ?[]const u8 {
    const key_pos = mem.indexOf(u8, json_str, "\"id\"") orelse return null;
    const after_key = json_str[key_pos + 4 ..];

    const colon = mem.indexOf(u8, after_key, ":") orelse return null;
    const after_colon = mem.trimLeft(u8, after_key[colon + 1 ..], " \t\n\r");

    // Find end of value (comma, brace, or end)
    var end: usize = 0;
    var in_string = false;
    for (after_colon, 0..) |c, i| {
        if (c == '"' and (i == 0 or after_colon[i - 1] != '\\')) {
            in_string = !in_string;
        }
        if (!in_string and (c == ',' or c == '}')) {
            end = i;
            break;
        }
        end = i + 1;
    }

    return mem.trim(u8, after_colon[0..end], " \t\n\r");
}

fn makeResult(alloc: Allocator, id: ?[]const u8, result: []const u8) []u8 {
    const id_str = id orelse "null";
    return std.fmt.allocPrint(alloc,
        \\{{"jsonrpc":"2.0","id":{s},"result":{s}}}
    , .{ id_str, result }) catch &.{};
}

fn makeError(alloc: Allocator, id: ?[]const u8, code: i32, message: []const u8) []u8 {
    const id_str = id orelse "null";
    return std.fmt.allocPrint(alloc,
        \\{{"jsonrpc":"2.0","id":{s},"error":{{"code":{d},"message":"{s}"}}}}
    , .{ id_str, code, message }) catch &.{};
}

test "dispatch status" {
    const alloc = std.testing.allocator;
    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"hemis/status\"}";
    const resp = dispatch(alloc, req);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"version\"") != null);
    try std.testing.expect(mem.indexOf(u8, resp, "\"zig\"") != null);
}

test "dispatch with database" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    // Status with db
    const status_req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"hemis/status\"}";
    const status_resp = dispatchWithDb(alloc, status_req, &db);
    defer alloc.free(status_resp);
    try std.testing.expect(mem.indexOf(u8, status_resp, "\"noteCount\":0") != null);
}

test "dispatch notes create and get" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    // Create
    const create_req =
        \\{"jsonrpc":"2.0","id":1,"method":"notes/create","params":{"filePath":"/test.zig","content":"Test note"}}
    ;
    const create_resp = dispatchWithDb(alloc, create_req, &db);
    defer alloc.free(create_resp);
    try std.testing.expect(mem.indexOf(u8, create_resp, "\"id\"") != null);
    try std.testing.expect(mem.indexOf(u8, create_resp, "\"filePath\":\"/test.zig\"") != null);

    // List project
    const list_req = "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"notes/list-project\"}";
    const list_resp = dispatchWithDb(alloc, list_req, &db);
    defer alloc.free(list_resp);
    try std.testing.expect(mem.indexOf(u8, list_resp, "Test note") != null);
}

test "dispatch method not found" {
    const alloc = std.testing.allocator;
    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"unknown/method\"}";
    const resp = dispatch(alloc, req);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"error\"") != null);
    try std.testing.expect(mem.indexOf(u8, resp, "MethodNotFound") != null);
}

test "dispatch missing method" {
    const alloc = std.testing.allocator;
    const req = "{\"jsonrpc\":\"2.0\",\"id\":1}";
    const resp = dispatch(alloc, req);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"error\"") != null);
    try std.testing.expect(mem.indexOf(u8, resp, "Invalid Request") != null);
}

test "extract string" {
    const json = "{\"method\":\"test\",\"id\":123}";
    const method = extractString(json, "\"method\"");
    try std.testing.expect(method != null);
    try std.testing.expectEqualStrings("test", method.?);
}

test "extract nested string" {
    const json = "{\"params\":{\"file\":\"/path/to/file\",\"content\":\"hello\"}}";
    const file = extractNestedString(json, "\"params\"", "\"file\"");
    try std.testing.expect(file != null);
    try std.testing.expectEqualStrings("/path/to/file", file.?);
}

test "extract nested int" {
    const json = "{\"params\":{\"limit\":50,\"offset\":10}}";
    const limit = extractNestedInt(json, "\"params\"", "\"limit\"");
    const offset = extractNestedInt(json, "\"params\"", "\"offset\"");
    try std.testing.expect(limit != null);
    try std.testing.expect(offset != null);
    try std.testing.expectEqual(@as(i64, 50), limit.?);
    try std.testing.expectEqual(@as(i64, 10), offset.?);
}

test "escape json" {
    const alloc = std.testing.allocator;

    // No escape needed
    const simple = try escapeJson(alloc, "hello");
    try std.testing.expectEqualStrings("hello", simple);

    // With escapes
    const with_quotes = try escapeJson(alloc, "say \"hello\"");
    defer alloc.free(with_quotes);
    try std.testing.expectEqualStrings("say \\\"hello\\\"", with_quotes);

    const with_newline = try escapeJson(alloc, "line1\nline2");
    defer alloc.free(with_newline);
    try std.testing.expectEqualStrings("line1\\nline2", with_newline);
}

test "dispatch hemis project meta" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"hemis/project-meta\"}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"noteCount\"") != null);
}

test "dispatch index add and search" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    // Add file
    const add_req =
        \\{"jsonrpc":"2.0","id":1,"method":"index/add-file","params":{"file":"/src/main.zig","content":"const std = @import(\"std\");"}}
    ;
    const add_resp = dispatchWithDb(alloc, add_req, &db);
    defer alloc.free(add_resp);
    try std.testing.expect(mem.indexOf(u8, add_resp, "\"contentHash\"") != null);

    // Search
    const search_req =
        \\{"jsonrpc":"2.0","id":2,"method":"index/search","params":{"query":"zig"}}
    ;
    const search_resp = dispatchWithDb(alloc, search_req, &db);
    defer alloc.free(search_resp);
    try std.testing.expect(mem.indexOf(u8, search_resp, "/src/main.zig") != null);
}
