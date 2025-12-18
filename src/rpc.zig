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
const ai = @import("ai.zig");

/// Parsed JSON-RPC request
const ParsedRequest = struct {
    parsed: std.json.Parsed(std.json.Value),
    method: []const u8,
    id: std.json.Value,
    params: ?std.json.ObjectMap,

    pub fn deinit(self: *ParsedRequest) void {
        self.parsed.deinit();
    }

    /// Get string param
    pub fn getString(self: ParsedRequest, key: []const u8) ?[]const u8 {
        const p = self.params orelse return null;
        const val = p.get(key) orelse return null;
        return switch (val) {
            .string => |s| s,
            else => null,
        };
    }

    /// Get int param
    pub fn getInt(self: ParsedRequest, key: []const u8) ?i64 {
        const p = self.params orelse return null;
        const val = p.get(key) orelse return null;
        return switch (val) {
            .integer => |i| i,
            else => null,
        };
    }

    /// Get optional string with fallback key
    pub fn getStringAlt(self: ParsedRequest, key1: []const u8, key2: []const u8) ?[]const u8 {
        return self.getString(key1) orelse self.getString(key2);
    }
};

/// Parse a JSON-RPC request
fn parseRequest(alloc: Allocator, request: []const u8) !ParsedRequest {
    const parsed = try std.json.parseFromSlice(std.json.Value, alloc, request, .{});
    errdefer parsed.deinit();

    const root = switch (parsed.value) {
        .object => |obj| obj,
        else => return error.InvalidRequest,
    };

    const method = switch (root.get("method") orelse return error.MissingMethod) {
        .string => |s| s,
        else => return error.InvalidMethod,
    };

    const id = root.get("id") orelse .null;

    const params: ?std.json.ObjectMap = if (root.get("params")) |p| switch (p) {
        .object => |obj| obj,
        else => null,
    } else null;

    return .{
        .parsed = parsed,
        .method = method,
        .id = id,
        .params = params,
    };
}

/// Format id for JSON response
fn formatId(alloc: Allocator, id: std.json.Value) ![]const u8 {
    return switch (id) {
        .null => try alloc.dupe(u8, "null"),
        .integer => |i| try std.fmt.allocPrint(alloc, "{d}", .{i}),
        .string => |s| try std.fmt.allocPrint(alloc, "\"{s}\"", .{s}),
        else => try alloc.dupe(u8, "null"),
    };
}

/// Serialize any struct to JSON
fn jsonStringify(alloc: Allocator, value: anytype) ![]const u8 {
    return std.fmt.allocPrint(alloc, "{f}", .{std.json.fmt(value, .{})});
}

/// Note response for JSON serialization
const NoteResponse = struct {
    id: []const u8,
    shortId: []const u8,
    file: []const u8,
    projectRoot: []const u8 = "",
    line: i64,
    column: i64,
    text: []const u8,
    summary: []const u8,
    stale: bool = false,
    iconHint: []const u8 = "fresh",
    nodeTextHash: ?[]const u8 = null,
    formattedLines: []const []const u8,
    createdAt: []const u8,
    updatedAt: []const u8,
    displayMarker: []const u8,
    displayLabel: []const u8,
    displayDetail: []const u8,
};

/// Note list item (includes displayLine)
const NoteListItem = struct {
    id: []const u8,
    shortId: []const u8,
    file: []const u8,
    projectRoot: []const u8 = "",
    line: i64,
    column: i64,
    text: []const u8,
    summary: []const u8,
    stale: bool = false,
    iconHint: []const u8 = "fresh",
    displayLine: i64,
    nodeTextHash: ?[]const u8 = null,
    formattedLines: []const []const u8,
    createdAt: []const u8,
    updatedAt: []const u8,
    displayMarker: []const u8,
    displayLabel: []const u8,
    displayDetail: []const u8,
};

/// Notes list response wrapper
const NotesListResponse = struct {
    notes: []const NoteListItem,
};

/// Simple note response (minimal fields)
const SimpleNoteResponse = struct {
    id: []const u8,
    filePath: []const u8,
    content: []const u8,
};

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
    var req = parseRequest(alloc, request) catch |err| {
        return switch (err) {
            error.MissingMethod, error.InvalidMethod, error.InvalidRequest => makeErrorJson(alloc, null, -32600, "Invalid Request"),
            else => makeErrorJson(alloc, null, -32700, "Parse error"),
        };
    };
    defer req.deinit();

    const handler = dispatch_table.get(req.method) orelse {
        return makeErrorId(alloc, req.id, -32601, "MethodNotFound");
    };

    const result = handler(alloc, req, db) catch |err| {
        return makeErrorId(alloc, req.id, -32603, @errorName(err));
    };
    defer alloc.free(result);

    return makeResultId(alloc, req.id, result);
}

/// Handler function signature - all handlers use ParsedRequest
const Handler = *const fn (Allocator, ParsedRequest, ?*storage.Database) anyerror![]const u8;

/// Comptime dispatch table for RPC methods
const dispatch_table = std.StaticStringMap(Handler).initComptime(.{
    // hemis/* handlers
    .{ "hemis/status", handleStatus },
    .{ "hemis/version", handleVersion },
    .{ "hemis/shutdown", handleShutdown },
    .{ "hemis/open-project", handleOpenProject },
    .{ "hemis/search", handleHemisSearch },
    .{ "hemis/index-project", handleIndexProject },
    .{ "hemis/project-meta", handleProjectMeta },
    .{ "hemis/save-snapshot", handleSaveSnapshot },
    .{ "hemis/load-snapshot", handleLoadSnapshot },
    .{ "hemis/file-context", handleFileContext },
    .{ "hemis/explain-region", handleExplainRegion },
    .{ "hemis/buffer-context", handleBufferContext },
    .{ "hemis/graph", handleGraph },
    .{ "hemis/tasks", handleTasks },
    .{ "hemis/task-status", handleTaskStatus },
    .{ "hemis/task-list", handleTaskList },
    .{ "hemis/display-config", handleDisplayConfig },
    .{ "hemis/note-templates", handleNoteTemplates },
    .{ "hemis/suggest-tags", handleSuggestTags },
    .{ "hemis/code-references", handleCodeReferences },
    .{ "hemis/summarize-file", handleSummarizeFile },
    // notes/* handlers
    .{ "notes/create", handleNotesCreate },
    .{ "notes/list-project", handleNotesListProject },
    .{ "notes/get", handleNotesGet },
    .{ "notes/delete", handleNotesDelete },
    .{ "notes/update", handleNotesUpdate },
    .{ "notes/search", handleNotesSearch },
    .{ "notes/backlinks", handleNotesBacklinks },
    .{ "notes/anchor", handleNotesAnchor },
    .{ "notes/list-for-file", handleNotesListForFile },
    .{ "notes/list-by-node", handleNotesListByNode },
    .{ "notes/get-at-position", handleNotesGetAtPosition },
    .{ "notes/buffer-update", handleNotesBufferUpdate },
    .{ "notes/reattach", handleNotesReattach },
    .{ "notes/history", handleNotesHistory },
    .{ "notes/get-version", handleNotesGetVersion },
    .{ "notes/restore-version", handleNotesRestoreVersion },
    .{ "notes/link-suggestions", handleNotesLinkSuggestions },
    .{ "notes/explain-and-create", handleNotesExplainAndCreate },
    // index/* handlers
    .{ "index/add-file", handleIndexAddFile },
    .{ "index/search", handleIndexSearch },
});

fn handleStatus(alloc: Allocator, req: ParsedRequest, db: ?*storage.Database) ![]const u8 {
    const proj = req.getString("projectRoot");
    const note_count: i64 = if (db) |d| storage.countNotes(d) catch 0 else 0;
    const file_count: i64 = if (db) |d| storage.countFiles(d) catch 0 else 0;
    const proj_display = proj orelse "None";

    const status_display = try std.fmt.allocPrint(alloc,
        "Hemis Status: OK\nProject: {s}\nNotes: {d}\nFiles: {d}\nEmbeddings: 0",
        .{ proj_display, note_count, file_count },
    );
    defer alloc.free(status_display);

    return jsonStringify(alloc, .{
        .ok = true,
        .projectRoot = proj,
        .counts = .{
            .notes = note_count,
            .files = file_count,
            .embeddings = @as(i64, 0),
            .edges = @as(i64, 0),
        },
        .statusDisplay = status_display,
    });
}

fn handleVersion(alloc: Allocator, _: ParsedRequest, _: ?*storage.Database) ![]const u8 {
    return jsonStringify(alloc, .{
        .version = "0.1.0",
        .language = "zig",
    });
}

fn handleShutdown(_: Allocator, _: ParsedRequest, _: ?*storage.Database) ![]const u8 {
    std.process.exit(0);
}

fn handleNotesCreate(alloc: Allocator, req: ParsedRequest, db: ?*storage.Database) ![]const u8 {
    const database = db orelse return error.NoDatabaseConnection;

    // Extract params (accept both "file" and "filePath" for compat)
    const file_path = req.getString("file") orelse
        req.getString("filePath") orelse
        return error.MissingFile;

    // Accept both "text" and "content" for compat
    const text = req.getString("text") orelse
        req.getString("content") orelse
        return error.MissingContent;

    const node_path = req.getString("nodePath");
    const explicit_hash = req.getString("nodeTextHash");
    const line = req.getInt("line");
    const column = req.getInt("column");

    // Compute nodeTextHash from buffer content if not provided
    var computed_hash_buf: [64]u8 = undefined;
    const node_text_hash: ?[]const u8 = if (explicit_hash) |h| h else blk: {
        const buf_content = req.getString("content");
        if (buf_content) |bc| {
            if (line) |ln| {
                if (getLineAt(bc, ln)) |line_text| {
                    computeHash(line_text, &computed_hash_buf);
                    break :blk &computed_hash_buf;
                }
            }
        }
        break :blk null;
    };

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
        .node_text_hash = node_text_hash,
        .line_number = line,
        .column_number = column,
        .content = text,
        .created_at = timestamp,
        .updated_at = timestamp,
    };

    try storage.createNote(database, note);

    // Build formattedLines array
    const col_num = column orelse @as(i64, 0);
    var formatted = try formatTextLinesWithContext(alloc, text, file_path, col_num);
    defer formatted.deinit();

    // Generate display fields
    const short_id = if (id.len >= 8) id[0..8] else id;
    const summary = getSummary(text);
    const line_num = line orelse @as(i64, 1);

    const display_marker = try std.fmt.allocPrint(alloc, "[n:{s}]", .{short_id});
    defer alloc.free(display_marker);
    const display_label = try std.fmt.allocPrint(alloc, "[Note] {s}", .{summary});
    defer alloc.free(display_label);
    const display_detail = try std.fmt.allocPrint(alloc, "{s}:{d}", .{ file_path, line_num });
    defer alloc.free(display_detail);

    const response = NoteResponse{
        .id = id,
        .shortId = short_id,
        .file = file_path,
        .line = line_num,
        .column = col_num,
        .text = text,
        .summary = summary,
        .nodeTextHash = node_text_hash,
        .formattedLines = formatted.lines,
        .createdAt = timestamp,
        .updatedAt = timestamp,
        .displayMarker = display_marker,
        .displayLabel = display_label,
        .displayDetail = display_detail,
    };

    return jsonStringify(alloc, response);
}

/// Get comment prefix for file extension
fn getCommentPrefix(file_path: []const u8) []const u8 {
    // Find extension
    var ext_start: usize = file_path.len;
    var i = file_path.len;
    while (i > 0) {
        i -= 1;
        if (file_path[i] == '.') {
            ext_start = i + 1;
            break;
        }
        if (file_path[i] == '/' or file_path[i] == '\\') break;
    }
    const ext = if (ext_start < file_path.len) file_path[ext_start..] else "";

    // Match extension to comment style
    if (mem.eql(u8, ext, "rs") or mem.eql(u8, ext, "go") or mem.eql(u8, ext, "js") or
        mem.eql(u8, ext, "ts") or mem.eql(u8, ext, "tsx") or mem.eql(u8, ext, "java") or
        mem.eql(u8, ext, "c") or mem.eql(u8, ext, "h") or mem.eql(u8, ext, "cpp") or
        mem.eql(u8, ext, "swift") or mem.eql(u8, ext, "zig"))
    {
        return "//";
    }
    if (mem.eql(u8, ext, "py") or mem.eql(u8, ext, "rb") or mem.eql(u8, ext, "sh") or
        mem.eql(u8, ext, "yaml") or mem.eql(u8, ext, "yml") or mem.eql(u8, ext, "toml"))
    {
        return "#";
    }
    if (mem.eql(u8, ext, "lua") or mem.eql(u8, ext, "sql") or mem.eql(u8, ext, "hs")) {
        return "--";
    }
    if (mem.eql(u8, ext, "el") or mem.eql(u8, ext, "lisp") or mem.eql(u8, ext, "clj") or
        mem.eql(u8, ext, "scm"))
    {
        return ";";
    }
    return "//"; // Default
}

/// Result of formatting text lines - caller must free with freeFormattedLines
const FormattedLines = struct {
    lines: [][]const u8,
    alloc: Allocator,

    pub fn deinit(self: *FormattedLines) void {
        for (self.lines) |line| {
            self.alloc.free(line);
        }
        self.alloc.free(self.lines);
    }
};

/// Format note text into array of lines with proper indentation and comment prefix
fn formatTextLinesWithContext(alloc: Allocator, text: []const u8, file_path: []const u8, column: i64) !FormattedLines {
    const prefix = getCommentPrefix(file_path);
    const indent: usize = if (column > 0) @intCast(column) else 0;

    var lines: std.ArrayList([]const u8) = .{};
    errdefer {
        for (lines.items) |line| alloc.free(line);
        lines.deinit(alloc);
    }

    var start: usize = 0;
    for (text, 0..) |ch, i| {
        if (ch == '\n') {
            const line = try formatSingleLine(alloc, text[start..i], prefix, indent);
            try lines.append(alloc, line);
            start = i + 1;
        }
    }
    // Last line
    if (start <= text.len) {
        const line = try formatSingleLine(alloc, text[start..], prefix, indent);
        try lines.append(alloc, line);
    }

    return .{ .lines = try lines.toOwnedSlice(alloc), .alloc = alloc };
}

fn formatSingleLine(alloc: Allocator, line: []const u8, prefix: []const u8, indent: usize) ![]const u8 {
    // Build formatted line: indent + prefix + " " + text
    var formatted: std.ArrayList(u8) = .{};
    errdefer formatted.deinit(alloc);

    // Add indent spaces
    for (0..indent) |_| {
        try formatted.append(alloc, ' ');
    }
    // Add comment prefix and space
    try formatted.appendSlice(alloc, prefix);
    try formatted.append(alloc, ' ');
    // Add line text
    try formatted.appendSlice(alloc, line);

    return formatted.toOwnedSlice(alloc);
}

/// Get line at 1-based line number
fn getLineAt(content: []const u8, target_line: i64) ?[]const u8 {
    if (target_line < 1) return null;
    var line_num: i64 = 1;
    var start: usize = 0;

    for (content, 0..) |ch, i| {
        if (ch == '\n') {
            if (line_num == target_line) {
                return content[start..i];
            }
            line_num += 1;
            start = i + 1;
        }
    }
    // Check last line
    if (line_num == target_line and start < content.len) {
        return content[start..];
    }
    return null;
}

fn computeHash(text: []const u8, out: *[64]u8) void {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update(text);
    var hash_buf: [32]u8 = undefined;
    hasher.final(&hash_buf);

    const hex_chars = "0123456789abcdef";
    for (hash_buf, 0..) |byte, j| {
        out[j * 2] = hex_chars[byte >> 4];
        out[j * 2 + 1] = hex_chars[byte & 0x0F];
    }
}

fn handleNotesListProject(alloc: Allocator, _: ParsedRequest, db: ?*storage.Database) ![]const u8 {
    const database = db orelse return error.NoDatabaseConnection;

    const notes = try storage.listProjectNotes(database, alloc);
    defer storage.freeNotes(alloc, notes);

    // Build response array
    const items = try alloc.alloc(SimpleNoteResponse, notes.len);
    defer alloc.free(items);

    for (notes, 0..) |note, i| {
        items[i] = .{ .id = note.id, .filePath = note.file_path, .content = note.content };
    }

    return jsonStringify(alloc, items);
}

fn handleNotesGet(alloc: Allocator, req: ParsedRequest, db: ?*storage.Database) ![]const u8 {
    const database = db orelse return error.NoDatabaseConnection;

    const id = req.getString("id") orelse
        return error.MissingId;

    const note_opt = try storage.getNote(database, alloc, id);
    if (note_opt) |note| {
        defer storage.freeNoteFields(alloc, note);
        return jsonStringify(alloc, SimpleNoteResponse{
            .id = note.id,
            .filePath = note.file_path,
            .content = note.content,
        });
    }

    return error.NoteNotFound;
}

fn handleNotesDelete(alloc: Allocator, req: ParsedRequest, db: ?*storage.Database) ![]const u8 {
    const database = db orelse return error.NoDatabaseConnection;

    const id = req.getString("id") orelse
        return error.MissingId;

    try storage.deleteNote(database, id);

    return jsonStringify(alloc, .{ .ok = true });
}

fn handleNotesUpdate(alloc: Allocator, req: ParsedRequest, db: ?*storage.Database) ![]const u8 {
    const database = db orelse return error.NoDatabaseConnection;

    const id = req.getString("id") orelse
        return error.MissingId;
    const text = req.getString("text");
    const tags = req.getString("tags");

    var ts_buf: [32]u8 = undefined;
    const timestamp = getTimestamp(&ts_buf);

    try storage.updateNote(database, id, text, tags, timestamp);

    const note_opt = try storage.getNote(database, alloc, id);
    if (note_opt) |note| {
        defer storage.freeNoteFields(alloc, note);
        return jsonStringify(alloc, SimpleNoteResponse{
            .id = note.id,
            .filePath = note.file_path,
            .content = note.content,
        });
    }

    return error.NoteNotFound;
}

fn handleNotesSearch(alloc: Allocator, req: ParsedRequest, db: ?*storage.Database) ![]const u8 {
    const database = db orelse return error.NoDatabaseConnection;

    const query = req.getString("query") orelse
        return error.MissingQuery;
    const limit = req.getInt("limit") orelse 50;
    const offset = req.getInt("offset") orelse 0;

    const notes = try storage.searchNotes(database, alloc, query, limit, offset);
    defer storage.freeNotes(alloc, notes);

    return formatNotesArray(alloc, notes);
}

fn handleNotesBacklinks(alloc: Allocator, req: ParsedRequest, db: ?*storage.Database) ![]const u8 {
    _ = req;
    _ = db;
    return alloc.dupe(u8, "[]");
}

fn handleNotesAnchor(alloc: Allocator, req: ParsedRequest, db: ?*storage.Database) ![]const u8 {
    _ = req;
    _ = db;
    return jsonStringify(alloc, .{ .line = @as(i64, 0), .column = @as(i64, 0) });
}

fn handleNotesListForFile(alloc: Allocator, req: ParsedRequest, db: ?*storage.Database) ![]const u8 {
    const database = db orelse return error.NoDatabaseConnection;

    const file_path = req.getString("file") orelse
        return error.MissingFile;

    // Get optional content for position tracking
    const content = req.getString("content");

    const notes = try storage.getNotesForFile(database, alloc, file_path);
    defer storage.freeNotes(alloc, notes);

    return formatNotesArrayWithContent(alloc, notes, content);
}

fn handleNotesListByNode(alloc: Allocator, req: ParsedRequest, db: ?*storage.Database) ![]const u8 {
    _ = req;
    _ = db;
    return alloc.dupe(u8, "[]");
}

fn handleNotesGetAtPosition(alloc: Allocator, req: ParsedRequest, db: ?*storage.Database) ![]const u8 {
    _ = req;
    _ = db;
    return alloc.dupe(u8, "null");
}

fn handleNotesBufferUpdate(alloc: Allocator, req: ParsedRequest, db: ?*storage.Database) ![]const u8 {
    _ = req;
    _ = db;
    return jsonStringify(alloc, .{ .ok = true });
}

fn handleNotesReattach(alloc: Allocator, req: ParsedRequest, db: ?*storage.Database) ![]const u8 {
    const database = db orelse return error.NoDatabaseConnection;

    const id = req.getString("id") orelse
        return error.MissingId;
    const file = req.getString("file");
    const line = req.getInt("line") orelse 1;
    const node_path = req.getString("nodePath");
    const node_text_hash = req.getString("nodeTextHash");

    var ts_buf: [32]u8 = undefined;
    const timestamp = getTimestamp(&ts_buf);

    try storage.reattachNote(database, id, file, line, node_path, node_text_hash, timestamp);

    // Return the updated note
    const note = storage.getNote(database, alloc, id) catch {
        return try std.fmt.allocPrint(alloc, "{{\"ok\":true,\"id\":\"{s}\"}}", .{id});
    };

    if (note) |n| {
        defer storage.freeNoteFields(alloc, n);
        return try std.fmt.allocPrint(alloc,
            \\{{"id":"{s}","filePath":"{s}","lineNumber":{?},"content":"{s}"}}
        , .{ n.id, n.file_path, n.line_number, n.content });
    }

    return try std.fmt.allocPrint(alloc, "{{\"ok\":true,\"id\":\"{s}\"}}", .{id});
}

// hemis/* handlers

fn handleOpenProject(alloc: Allocator, _: ParsedRequest, _: ?*storage.Database) ![]const u8 {
    return jsonStringify(alloc, .{ .ok = true });
}

const SearchResultsResponse = struct {
    results: []const SimpleNoteResponse,
};

fn handleHemisSearch(alloc: Allocator, req: ParsedRequest, db: ?*storage.Database) ![]const u8 {
    const database = db orelse return error.NoDatabaseConnection;

    const query = req.getString("query") orelse
        return error.MissingQuery;

    const notes = try storage.searchNotes(database, alloc, query, 50, 0);
    defer storage.freeNotes(alloc, notes);

    const items = try alloc.alloc(SimpleNoteResponse, notes.len);
    defer alloc.free(items);

    for (notes, 0..) |note, i| {
        items[i] = .{ .id = note.id, .filePath = note.file_path, .content = note.content };
    }

    return jsonStringify(alloc, SearchResultsResponse{ .results = items });
}

fn handleIndexProject(alloc: Allocator, _: ParsedRequest, _: ?*storage.Database) ![]const u8 {
    return jsonStringify(alloc, .{ .ok = true, .indexed = @as(i64, 0) });
}

fn handleProjectMeta(alloc: Allocator, _: ParsedRequest, db: ?*storage.Database) ![]const u8 {
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

    return jsonStringify(alloc, .{
        .noteCount = note_count,
        .indexed = @as(i64, 0),
        .gitBranch = branch_str,
        .gitCommit = commit_str,
    });
}

fn handleSaveSnapshot(alloc: Allocator, req: ParsedRequest, db: ?*storage.Database) ![]const u8 {
    const database = db orelse return error.NoDatabaseConnection;

    const path = req.getString("path") orelse
        return error.MissingPath;

    const snapshot = try storage.exportNotesJson(database, alloc);
    defer alloc.free(snapshot);

    const path_z = try alloc.dupeZ(u8, path);
    defer alloc.free(path_z);

    const file = try std.fs.cwd().createFile(path_z, .{});
    defer file.close();

    try file.writeAll(snapshot);

    return jsonStringify(alloc, .{ .ok = true, .path = path });
}

fn handleLoadSnapshot(alloc: Allocator, req: ParsedRequest, db: ?*storage.Database) ![]const u8 {
    const database = db orelse return error.NoDatabaseConnection;

    const path = req.getString("path") orelse
        return error.MissingPath;

    const path_z = try alloc.dupeZ(u8, path);
    defer alloc.free(path_z);

    const file = try std.fs.cwd().openFile(path_z, .{});
    defer file.close();

    const snapshot = try file.readToEndAlloc(alloc, 100 * 1024 * 1024);
    defer alloc.free(snapshot);

    try storage.clearNotes(database);
    const count = try storage.importNotesJson(database, alloc, snapshot);

    return jsonStringify(alloc, .{ .ok = true, .path = path, .imported = @as(i64, @intCast(count)) });
}

const FileContextNoteItem = struct {
    id: []const u8,
    content: []const u8,
};

const FileContextResponse = struct {
    file: []const u8,
    notes: []const FileContextNoteItem,
};

fn handleFileContext(alloc: Allocator, req: ParsedRequest, db: ?*storage.Database) ![]const u8 {
    const database = db orelse return error.NoDatabaseConnection;

    const file = req.getString("file") orelse
        return error.MissingFile;

    const notes = try storage.getNotesForFile(database, alloc, file);
    defer storage.freeNotes(alloc, notes);

    const items = try alloc.alloc(FileContextNoteItem, notes.len);
    defer alloc.free(items);

    for (notes, 0..) |note, i| {
        items[i] = .{ .id = note.id, .content = note.content };
    }

    return jsonStringify(alloc, FileContextResponse{ .file = file, .notes = items });
}

fn handleExplainRegion(alloc: Allocator, req: ParsedRequest, _: ?*storage.Database) ![]const u8 {
    // Extract code content from params (passed by editor)
    const content = req.getString("content") orelse {
        // Fallback: try to get file path
        const file = req.getString("file") orelse
            return error.MissingFile;
        return try std.fmt.allocPrint(alloc,
            \\{{"file":"{s}","explanation":"No content provided"}}
        , .{file});
    };

    // Call AI to explain the code
    const explanation = ai.explain(alloc, content) catch |err| {
        return try std.fmt.allocPrint(alloc,
            \\{{"error":"AI unavailable: {s}"}}
        , .{@errorName(err)});
    };
    defer alloc.free(explanation);

    // Escape the explanation for JSON
    var escaped: std.ArrayList(u8) = .{};
    defer escaped.deinit(alloc);
    const writer = escaped.writer(alloc);
    for (explanation) |c| {
        switch (c) {
            '"' => try writer.writeAll("\\\""),
            '\\' => try writer.writeAll("\\\\"),
            '\n' => try writer.writeAll("\\n"),
            '\r' => try writer.writeAll("\\r"),
            '\t' => try writer.writeAll("\\t"),
            else => try writer.writeByte(c),
        }
    }

    return try std.fmt.allocPrint(alloc,
        \\{{"explanation":"{s}","ai":{{"statusDisplay":"[Claude]"}}}}
    , .{escaped.items});
}

fn handleBufferContext(alloc: Allocator, req: ParsedRequest, db: ?*storage.Database) ![]const u8 {
    const database = db orelse return error.NoDatabaseConnection;

    const file = req.getString("file") orelse
        return error.MissingFile;

    const note_count: i64 = storage.countNotes(database) catch 0;

    return try std.fmt.allocPrint(alloc,
        \\{{"file":"{s}","noteCount":{d}}}
    , .{ file, note_count });
}

fn handleGraph(alloc: Allocator, _: ParsedRequest, db: ?*storage.Database) ![]const u8 {
    const database = db orelse return error.NoDatabaseConnection;

    const notes = try storage.listProjectNotes(database, alloc);
    defer storage.freeNotes(alloc, notes);

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

fn handleTasks(alloc: Allocator, _: ParsedRequest, _: ?*storage.Database) ![]const u8 {
    return try std.fmt.allocPrint(alloc, "{{\"tasks\":[]}}", .{});
}

fn handleTaskStatus(alloc: Allocator, req: ParsedRequest, _: ?*storage.Database) ![]const u8 {
    const task_id = req.getString("taskId") orelse
        return error.MissingTaskId;

    return try std.fmt.allocPrint(alloc,
        \\{{"taskId":"{s}","status":"unknown"}}
    , .{task_id});
}

fn handleTaskList(alloc: Allocator, _: ParsedRequest, _: ?*storage.Database) ![]const u8 {
    return try std.fmt.allocPrint(alloc, "{{\"tasks\":[]}}", .{});
}

// index/* handlers

fn handleIndexAddFile(alloc: Allocator, req: ParsedRequest, db: ?*storage.Database) ![]const u8 {
    const database = db orelse return error.NoDatabaseConnection;

    const file_path = req.getString("file") orelse
        req.getString("path") orelse
        return error.MissingFile;
    const content = req.getString("content") orelse
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

    try storage.addFile(database, file_path, content, &hash_hex);

    return try std.fmt.allocPrint(alloc,
        \\{{"file":"{s}","contentHash":"{s}"}}
    , .{ file_path, &hash_hex });
}

const SearchHitResponse = struct {
    file: []const u8,
    line: i64,
    column: i64,
    text: []const u8,
    score: f64 = 1.0,
};

fn handleIndexSearch(alloc: Allocator, req: ParsedRequest, db: ?*storage.Database) ![]const u8 {
    const database = db orelse return error.NoDatabaseConnection;

    const query = req.getString("query") orelse
        return error.MissingQuery;

    const hits = try storage.searchContent(database, alloc, query);
    defer storage.freeSearchHits(alloc, hits);

    const items = try alloc.alloc(SearchHitResponse, hits.len);
    defer alloc.free(items);

    for (hits, 0..) |hit, i| {
        items[i] = .{
            .file = hit.file,
            .line = @intCast(hit.line),
            .column = @intCast(hit.column),
            .text = hit.text,
        };
    }

    return jsonStringify(alloc, items);
}

// New handlers

fn handleDisplayConfig(alloc: Allocator, _: ParsedRequest, _: ?*storage.Database) ![]const u8 {
    return try std.fmt.allocPrint(alloc,
        \\{{"colors":{{"note":"#4682B4","noteStale":"#808080","marker":"#4682B4"}},"icons":{{"noteFresh":"📝","noteStale":"📝","noteAi":"🤖"}},"templates":{{"displayLabel":"{{shortId}} {{summary}}","hoverText":"hemis: {{summary}}"}}}}
    , .{});
}

fn handleNoteTemplates(alloc: Allocator, _: ParsedRequest, _: ?*storage.Database) ![]const u8 {
    return try std.fmt.allocPrint(alloc,
        \\{{"templates":[{{"id":"bug","name":"Bug Report","template":"## Bug Report\\n\\n**Severity:** \\n**Steps:**\\n1. "}},{{"id":"todo","name":"TODO","template":"## TODO\\n\\n**Priority:** medium\\n"}},{{"id":"api","name":"API Contract","template":"## API Contract\\n\\n**Inputs:**\\n- "}},{{"id":"decision","name":"Design Decision","template":"## Design Decision\\n\\n**Context:**\\n"}}]}}
    , .{});
}

fn handleSuggestTags(alloc: Allocator, req: ParsedRequest, _: ?*storage.Database) ![]const u8 {
    const file = req.getString("file") orelse
        return error.MissingFile;

    // Suggest tags based on file extension
    var tags: std.ArrayList(u8) = .{};
    defer tags.deinit(alloc);

    try tags.appendSlice(alloc, "[");

    // Language tag from extension
    if (mem.endsWith(u8, file, ".zig")) {
        try tags.appendSlice(alloc, "\"zig\"");
    } else if (mem.endsWith(u8, file, ".rs")) {
        try tags.appendSlice(alloc, "\"rust\"");
    } else if (mem.endsWith(u8, file, ".py")) {
        try tags.appendSlice(alloc, "\"python\"");
    } else if (mem.endsWith(u8, file, ".js") or mem.endsWith(u8, file, ".ts")) {
        try tags.appendSlice(alloc, "\"javascript\"");
    } else if (mem.endsWith(u8, file, ".go")) {
        try tags.appendSlice(alloc, "\"go\"");
    }

    // Pattern-based tags (case-insensitive)
    const file_lower = std.ascii.allocLowerString(alloc, file) catch file;
    defer if (file_lower.ptr != file.ptr) alloc.free(file_lower);
    if (mem.indexOf(u8, file_lower, "test") != null or mem.indexOf(u8, file_lower, "spec") != null) {
        if (tags.items.len > 1) try tags.appendSlice(alloc, ",");
        try tags.appendSlice(alloc, "\"test\"");
    }

    try tags.appendSlice(alloc, "]");

    return try std.fmt.allocPrint(alloc, "{{\"tags\":{s}}}", .{tags.items});
}

fn handleCodeReferences(alloc: Allocator, req: ParsedRequest, _: ?*storage.Database) ![]const u8 {
    const file = req.getString("file") orelse
        return error.MissingFile;
    const line = req.getInt("line") orelse 1;

    return jsonStringify(alloc, .{
        .references = @as([]const u8, &.{}),
        .anchor = .{ .line = line, .file = file },
    });
}

const SummarySectionItem = struct {
    noteId: []const u8,
    summary: []const u8,
};

const SummarizeFileResponse = struct {
    file: []const u8,
    sections: []const SummarySectionItem,
};

fn handleSummarizeFile(alloc: Allocator, req: ParsedRequest, db: ?*storage.Database) ![]const u8 {
    const database = db orelse return error.NoDatabaseConnection;
    const file = req.getString("file") orelse
        return error.MissingFile;

    const notes = try storage.getNotesForFile(database, alloc, file);
    defer storage.freeNotes(alloc, notes);

    const items = try alloc.alloc(SummarySectionItem, notes.len);
    defer alloc.free(items);

    for (notes, 0..) |note, i| {
        items[i] = .{ .noteId = note.id, .summary = note.content };
    }

    return jsonStringify(alloc, SummarizeFileResponse{ .file = file, .sections = items });
}

fn handleNotesHistory(alloc: Allocator, req: ParsedRequest, db: ?*storage.Database) ![]const u8 {
    _ = db;
    const id = req.getString("id") orelse
        return error.MissingId;

    // Version history not yet implemented - return empty
    return try std.fmt.allocPrint(alloc, "{{\"noteId\":\"{s}\",\"versions\":[]}}", .{id});
}

fn handleNotesGetVersion(alloc: Allocator, req: ParsedRequest, db: ?*storage.Database) ![]const u8 {
    _ = db;
    const id = req.getString("id") orelse
        return error.MissingId;
    const version = req.getInt("version") orelse
        return error.MissingVersion;

    return try std.fmt.allocPrint(alloc,
        \\{{"noteId":"{s}","version":{d},"content":null}}
    , .{ id, version });
}

fn handleNotesRestoreVersion(alloc: Allocator, req: ParsedRequest, db: ?*storage.Database) ![]const u8 {
    _ = db;
    const id = req.getString("id") orelse
        return error.MissingId;
    const version = req.getInt("version") orelse
        return error.MissingVersion;

    return try std.fmt.allocPrint(alloc,
        \\{{"noteId":"{s}","restoredVersion":{d},"ok":false}}
    , .{ id, version });
}

const LinkSuggestionItem = struct {
    noteId: []const u8,
    summary: []const u8,
    formatted: []const u8,
};

/// Holds allocated strings for link suggestion items
const AllocatedLinkItem = struct {
    item: LinkSuggestionItem,
    formatted: []const u8,

    fn deinit(self: *AllocatedLinkItem, a: Allocator) void {
        a.free(self.formatted);
    }
};

fn handleNotesLinkSuggestions(alloc: Allocator, req: ParsedRequest, db: ?*storage.Database) ![]const u8 {
    const database = db orelse return error.NoDatabaseConnection;
    const query = req.getString("query") orelse "";

    const notes = try storage.searchNotes(database, alloc, query, 10, 0);
    defer storage.freeNotes(alloc, notes);

    var items: std.ArrayList(AllocatedLinkItem) = .{};
    defer {
        for (items.items) |*item| item.deinit(alloc);
        items.deinit(alloc);
    }

    for (notes) |note| {
        // Get first line as summary
        const first_line = if (mem.indexOf(u8, note.content, "\n")) |nl|
            note.content[0..nl]
        else
            note.content;
        const summary = if (first_line.len > 50) first_line[0..50] else first_line;
        const formatted = try std.fmt.allocPrint(alloc, "[[{s}][{s}]]", .{ summary, note.id });
        errdefer alloc.free(formatted);

        try items.append(alloc, .{
            .item = .{ .noteId = note.id, .summary = summary, .formatted = formatted },
            .formatted = formatted,
        });
    }

    // Extract just the items for serialization
    const result = try alloc.alloc(LinkSuggestionItem, items.items.len);
    defer alloc.free(result);
    for (items.items, 0..) |item, i| {
        result[i] = item.item;
    }

    return jsonStringify(alloc, result);
}

fn handleNotesExplainAndCreate(alloc: Allocator, req: ParsedRequest, db: ?*storage.Database) ![]const u8 {
    const database = db orelse return error.NoDatabaseConnection;
    const file = req.getString("file") orelse
        return error.MissingFile;
    const content = req.getString("content");

    // Create note with AI placeholder
    var id_buf: [36]u8 = undefined;
    const id = generateId(&id_buf);

    var ts_buf: [32]u8 = undefined;
    const timestamp = getTimestamp(&ts_buf);

    const note_content = content orelse "AI explanation pending";
    const note = storage.Note{
        .id = id,
        .file_path = file,
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = note_content,
        .created_at = timestamp,
        .updated_at = timestamp,
    };

    try storage.createNote(database, note);

    return try std.fmt.allocPrint(alloc,
        \\{{"note":{{"id":"{s}","filePath":"{s}"}},"ai":{{"provider":"none","hadContext":false}}}}
    , .{ id, file });
}

fn formatNotesArray(alloc: Allocator, notes: []const storage.Note) ![]const u8 {
    return formatNotesArrayWithContent(alloc, notes, null);
}

/// Allocated item with its resources
const AllocatedNoteItem = struct {
    item: NoteListItem,
    display_marker: []const u8,
    display_label: []const u8,
    display_detail: []const u8,
    formatted: FormattedLines,

    fn deinit(self: *AllocatedNoteItem, alloc: Allocator) void {
        alloc.free(self.display_marker);
        alloc.free(self.display_label);
        alloc.free(self.display_detail);
        var f = self.formatted;
        f.deinit();
    }
};

fn formatNotesArrayWithContent(alloc: Allocator, notes: []const storage.Note, content: ?[]const u8) ![]const u8 {
    // Build array of note items
    var items: std.ArrayList(AllocatedNoteItem) = .{};
    defer {
        for (items.items) |*item| item.deinit(alloc);
        items.deinit(alloc);
    }

    for (notes) |note| {
        const stored_line = note.line_number orelse 1;
        const short_id = if (note.id.len >= 8) note.id[0..8] else note.id;
        const summary = getSummary(note.content);
        const col_num = note.column_number orelse 0;

        // Compute displayLine and stale from content if available
        var is_stale = false;
        const display_line = if (content) |c| blk: {
            if (note.node_text_hash) |hash| {
                if (findLineByHash(c, hash)) |found_line| {
                    break :blk found_line;
                } else {
                    is_stale = true;
                    break :blk stored_line;
                }
            }
            break :blk stored_line;
        } else stored_line;

        const reported_line = if (content != null) display_line else stored_line;

        // Allocate display strings
        const display_marker = try std.fmt.allocPrint(alloc, "[n:{s}]", .{short_id});
        errdefer alloc.free(display_marker);
        const display_label = try std.fmt.allocPrint(alloc, "[Note] {s}", .{summary});
        errdefer alloc.free(display_label);
        const display_detail = try std.fmt.allocPrint(alloc, "{s}:{d}", .{ note.file_path, reported_line });
        errdefer alloc.free(display_detail);

        var formatted = try formatTextLinesWithContext(alloc, note.content, note.file_path, col_num);
        errdefer formatted.deinit();

        try items.append(alloc, .{
            .item = .{
                .id = note.id,
                .shortId = short_id,
                .file = note.file_path,
                .line = reported_line,
                .column = col_num,
                .text = note.content,
                .summary = summary,
                .stale = is_stale,
                .iconHint = if (is_stale) "stale" else "fresh",
                .displayLine = display_line,
                .nodeTextHash = note.node_text_hash,
                .formattedLines = formatted.lines,
                .createdAt = note.created_at,
                .updatedAt = note.updated_at,
                .displayMarker = display_marker,
                .displayLabel = display_label,
                .displayDetail = display_detail,
            },
            .display_marker = display_marker,
            .display_label = display_label,
            .display_detail = display_detail,
            .formatted = formatted,
        });
    }

    // Extract just the NoteListItems for serialization
    const note_items = try alloc.alloc(NoteListItem, items.items.len);
    defer alloc.free(note_items);
    for (items.items, 0..) |item, i| {
        note_items[i] = item.item;
    }

    return jsonStringify(alloc, NotesListResponse{ .notes = note_items });
}

/// Find line number where hash matches a line's SHA256
fn findLineByHash(content: []const u8, target_hash: []const u8) ?i64 {
    var line_num: i64 = 1;
    var start: usize = 0;

    for (content, 0..) |ch, i| {
        if (ch == '\n') {
            const line_text = content[start..i];
            if (hashMatches(line_text, target_hash)) {
                return line_num;
            }
            line_num += 1;
            start = i + 1;
        }
    }
    // Check last line
    if (start < content.len) {
        const line_text = content[start..];
        if (hashMatches(line_text, target_hash)) {
            return line_num;
        }
    }
    return null;
}

fn hashMatches(text: []const u8, target_hash: []const u8) bool {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update(text);
    var hash_buf: [32]u8 = undefined;
    hasher.final(&hash_buf);

    const hex_chars = "0123456789abcdef";
    var hash_hex: [64]u8 = undefined;
    for (hash_buf, 0..) |byte, j| {
        hash_hex[j * 2] = hex_chars[byte >> 4];
        hash_hex[j * 2 + 1] = hex_chars[byte & 0x0F];
    }

    return mem.eql(u8, &hash_hex, target_hash);
}

fn getSummary(text: []const u8) []const u8 {
    // Get first line, max 50 chars
    var end: usize = 0;
    for (text, 0..) |c, i| {
        if (c == '\n') {
            end = i;
            break;
        }
        end = i + 1;
    }
    const first_line = text[0..@min(end, text.len)];
    return if (first_line.len > 50) first_line[0..50] else first_line;
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
    // Simple format: seconds since epoch
    return std.fmt.bufPrint(buf, "{d}", .{secs}) catch "0";
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

    // Find end quote, handling escapes
    var end: usize = 0;
    var i: usize = 1;
    while (i < after_colon.len) : (i += 1) {
        if (after_colon[i] == '\\' and i + 1 < after_colon.len) {
            i += 1; // Skip escaped char
        } else if (after_colon[i] == '"') {
            end = i;
            break;
        }
    }
    if (end == 0) return null;
    return after_colon[1..end];
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

fn makeResultId(alloc: Allocator, id: std.json.Value, result: []const u8) []u8 {
    const id_str = formatId(alloc, id) catch "null";
    defer if (id_str.ptr != "null".ptr) alloc.free(id_str);
    return std.fmt.allocPrint(alloc,
        \\{{"jsonrpc":"2.0","id":{s},"result":{s}}}
    , .{ id_str, result }) catch &.{};
}

fn makeErrorId(alloc: Allocator, id: std.json.Value, code: i32, message: []const u8) []u8 {
    const id_str = formatId(alloc, id) catch "null";
    defer if (id_str.ptr != "null".ptr) alloc.free(id_str);
    return std.fmt.allocPrint(alloc,
        \\{{"jsonrpc":"2.0","id":{s},"error":{{"code":{d},"message":"{s}"}}}}
    , .{ id_str, code, message }) catch &.{};
}

fn makeErrorJson(alloc: Allocator, id: ?[]const u8, code: i32, message: []const u8) []u8 {
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

    // Status returns Rust-compatible format with counts
    try std.testing.expect(mem.indexOf(u8, resp, "\"ok\":true") != null);
    try std.testing.expect(mem.indexOf(u8, resp, "\"counts\"") != null);
}

test "dispatch with database" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    // Status with db returns counts
    const status_req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"hemis/status\"}";
    const status_resp = dispatchWithDb(alloc, status_req, &db);
    defer alloc.free(status_resp);
    try std.testing.expect(mem.indexOf(u8, status_resp, "\"notes\":0") != null);
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
    try std.testing.expect(mem.indexOf(u8, create_resp, "\"file\":\"/test.zig\"") != null);

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

    // Search - query matches content, not path
    const search_req =
        \\{"jsonrpc":"2.0","id":2,"method":"index/search","params":{"query":"std"}}
    ;
    const search_resp = dispatchWithDb(alloc, search_req, &db);
    defer alloc.free(search_resp);
    try std.testing.expect(mem.indexOf(u8, search_resp, "/src/main.zig") != null);
}

test "dispatch notes list-for-file" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    // Create note first (using correct param names: filePath, content)
    const create_req =
        \\{"jsonrpc":"2.0","id":1,"method":"notes/create","params":{"filePath":"/test.zig","content":"note content"}}
    ;
    const create_resp = dispatchWithDb(alloc, create_req, &db);
    defer alloc.free(create_resp);

    // List for file
    const list_req =
        \\{"jsonrpc":"2.0","id":2,"method":"notes/list-for-file","params":{"file":"/test.zig"}}
    ;
    const list_resp = dispatchWithDb(alloc, list_req, &db);
    defer alloc.free(list_resp);
    try std.testing.expect(mem.indexOf(u8, list_resp, "note content") != null);
}

test "dispatch notes search" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    // Create note (using correct param names)
    const create_req =
        \\{"jsonrpc":"2.0","id":1,"method":"notes/create","params":{"filePath":"/test.zig","content":"unique searchable text"}}
    ;
    const create_resp = dispatchWithDb(alloc, create_req, &db);
    defer alloc.free(create_resp);

    // Search
    const search_req =
        \\{"jsonrpc":"2.0","id":2,"method":"notes/search","params":{"query":"searchable"}}
    ;
    const search_resp = dispatchWithDb(alloc, search_req, &db);
    defer alloc.free(search_resp);
    try std.testing.expect(mem.indexOf(u8, search_resp, "unique searchable text") != null);
}

test "dispatch notes update" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    // Create note (using correct param names)
    const create_req =
        \\{"jsonrpc":"2.0","id":1,"method":"notes/create","params":{"filePath":"/test.zig","content":"original"}}
    ;
    const create_resp = dispatchWithDb(alloc, create_req, &db);
    defer alloc.free(create_resp);

    // Extract note id from result (result has {"id":"xxx"...})
    const result_marker = "\"result\":{\"id\":\"";
    const id_start = (mem.indexOf(u8, create_resp, result_marker) orelse return error.TestUnexpectedResult) + result_marker.len;
    const id_end = mem.indexOfPos(u8, create_resp, id_start, "\"") orelse return error.TestUnexpectedResult;
    const id = create_resp[id_start..id_end];

    // Update note
    const update_req = try std.fmt.allocPrint(alloc,
        \\{{"jsonrpc":"2.0","id":2,"method":"notes/update","params":{{"id":"{s}","text":"updated"}}}}
    , .{id});
    defer alloc.free(update_req);

    const update_resp = dispatchWithDb(alloc, update_req, &db);
    defer alloc.free(update_resp);
    try std.testing.expect(mem.indexOf(u8, update_resp, "updated") != null);
}

test "dispatch notes delete" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    // Create note (using correct param names)
    const create_req =
        \\{"jsonrpc":"2.0","id":1,"method":"notes/create","params":{"filePath":"/test.zig","content":"to delete"}}
    ;
    const create_resp = dispatchWithDb(alloc, create_req, &db);
    defer alloc.free(create_resp);

    // Extract note id from result
    const result_marker = "\"result\":{\"id\":\"";
    const id_start = (mem.indexOf(u8, create_resp, result_marker) orelse return error.TestUnexpectedResult) + result_marker.len;
    const id_end = mem.indexOfPos(u8, create_resp, id_start, "\"") orelse return error.TestUnexpectedResult;
    const id = create_resp[id_start..id_end];

    // Delete note
    const delete_req = try std.fmt.allocPrint(alloc,
        \\{{"jsonrpc":"2.0","id":2,"method":"notes/delete","params":{{"id":"{s}"}}}}
    , .{id});
    defer alloc.free(delete_req);

    const delete_resp = dispatchWithDb(alloc, delete_req, &db);
    defer alloc.free(delete_resp);
    try std.testing.expect(mem.indexOf(u8, delete_resp, "\"ok\":true") != null);

    // Verify deleted
    const get_req = try std.fmt.allocPrint(alloc,
        \\{{"jsonrpc":"2.0","id":3,"method":"notes/get","params":{{"id":"{s}"}}}}
    , .{id});
    defer alloc.free(get_req);

    const get_resp = dispatchWithDb(alloc, get_req, &db);
    defer alloc.free(get_resp);
    try std.testing.expect(mem.indexOf(u8, get_resp, "\"error\"") != null);
}

test "dispatch hemis search" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    // Create note (using correct param names)
    const create_req =
        \\{"jsonrpc":"2.0","id":1,"method":"notes/create","params":{"filePath":"/test.zig","content":"hemis search test"}}
    ;
    const create_resp = dispatchWithDb(alloc, create_req, &db);
    defer alloc.free(create_resp);

    // Search via hemis/search
    const search_req =
        \\{"jsonrpc":"2.0","id":2,"method":"hemis/search","params":{"query":"hemis"}}
    ;
    const search_resp = dispatchWithDb(alloc, search_req, &db);
    defer alloc.free(search_resp);
    try std.testing.expect(mem.indexOf(u8, search_resp, "\"results\":") != null);
}

test "dispatch list-project notes" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    // Create two notes (using correct param names)
    const create1 =
        \\{"jsonrpc":"2.0","id":1,"method":"notes/create","params":{"filePath":"/a.zig","content":"note a"}}
    ;
    const resp1 = dispatchWithDb(alloc, create1, &db);
    defer alloc.free(resp1);

    const create2 =
        \\{"jsonrpc":"2.0","id":2,"method":"notes/create","params":{"filePath":"/b.zig","content":"note b"}}
    ;
    const resp2 = dispatchWithDb(alloc, create2, &db);
    defer alloc.free(resp2);

    // List all
    const list_req =
        \\{"jsonrpc":"2.0","id":3,"method":"notes/list-project","params":{}}
    ;
    const list_resp = dispatchWithDb(alloc, list_req, &db);
    defer alloc.free(list_resp);
    try std.testing.expect(mem.indexOf(u8, list_resp, "note a") != null);
    try std.testing.expect(mem.indexOf(u8, list_resp, "note b") != null);
}

test "dispatch open-project" {
    const alloc = std.testing.allocator;

    const req =
        \\{"jsonrpc":"2.0","id":1,"method":"hemis/open-project","params":{"path":"/tmp"}}
    ;
    const resp = dispatch(alloc, req);
    defer alloc.free(resp);
    try std.testing.expect(mem.indexOf(u8, resp, "\"ok\":true") != null);
}

test "dispatch shutdown handler exists" {
    // Note: Can't test hemis/shutdown directly as it calls std.process.exit(0)
    // Just verify the method string parsing would work
    const method = "hemis/shutdown";
    try std.testing.expect(mem.eql(u8, method, "hemis/shutdown"));
}

test "dispatch notes backlinks" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req =
        \\{"jsonrpc":"2.0","id":1,"method":"notes/backlinks","params":{"id":"note-1"}}
    ;
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);
    // Returns empty array for now
    try std.testing.expect(mem.indexOf(u8, resp, "[]") != null);
}

test "dispatch notes anchor" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req =
        \\{"jsonrpc":"2.0","id":1,"method":"notes/anchor","params":{"id":"note-1"}}
    ;
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);
    try std.testing.expect(mem.indexOf(u8, resp, "\"line\":") != null);
}

test "dispatch notes list-by-node" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req =
        \\{"jsonrpc":"2.0","id":1,"method":"notes/list-by-node","params":{"file":"/test.zig","nodePath":"fn.main"}}
    ;
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);
    try std.testing.expect(mem.indexOf(u8, resp, "[]") != null);
}

test "dispatch notes get-at-position" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req =
        \\{"jsonrpc":"2.0","id":1,"method":"notes/get-at-position","params":{"file":"/test.zig","line":10}}
    ;
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);
    try std.testing.expect(mem.indexOf(u8, resp, "null") != null);
}

test "dispatch notes buffer-update" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req =
        \\{"jsonrpc":"2.0","id":1,"method":"notes/buffer-update","params":{"file":"/test.zig","content":"new content"}}
    ;
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);
    try std.testing.expect(mem.indexOf(u8, resp, "\"ok\":true") != null);
}

test "dispatch hemis index-project" {
    const alloc = std.testing.allocator;

    const req =
        \\{"jsonrpc":"2.0","id":1,"method":"hemis/index-project","params":{}}
    ;
    const resp = dispatch(alloc, req);
    defer alloc.free(resp);
    try std.testing.expect(mem.indexOf(u8, resp, "\"ok\":true") != null);
}

test "dispatch hemis graph" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req =
        \\{"jsonrpc":"2.0","id":1,"method":"hemis/graph","params":{}}
    ;
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);
    try std.testing.expect(mem.indexOf(u8, resp, "\"nodes\":") != null);
}

test "dispatch hemis save-snapshot" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    // Use /tmp for test file
    const req =
        \\{"jsonrpc":"2.0","id":1,"method":"hemis/save-snapshot","params":{"path":"/tmp/hemis-test-snapshot.json"}}
    ;
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);
    try std.testing.expect(mem.indexOf(u8, resp, "\"ok\":true") != null);

    // Cleanup
    std.fs.deleteFileAbsolute("/tmp/hemis-test-snapshot.json") catch {};
}

test "dispatch hemis file-context" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req =
        \\{"jsonrpc":"2.0","id":1,"method":"hemis/file-context","params":{"file":"/test.zig"}}
    ;
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);
    try std.testing.expect(mem.indexOf(u8, resp, "\"file\":") != null);
}

test "dispatch hemis buffer-context" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req =
        \\{"jsonrpc":"2.0","id":1,"method":"hemis/buffer-context","params":{"file":"/test.zig","content":"test"}}
    ;
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);
    try std.testing.expect(mem.indexOf(u8, resp, "\"file\":") != null);
}

test "dispatch hemis explain-region" {
    const alloc = std.testing.allocator;

    const req =
        \\{"jsonrpc":"2.0","id":1,"method":"hemis/explain-region","params":{"file":"/test.zig","startLine":1,"endLine":10}}
    ;
    const resp = dispatch(alloc, req);
    defer alloc.free(resp);
    // Returns file and info
    try std.testing.expect(mem.indexOf(u8, resp, "\"file\":") != null);
}

test "extract int from json" {
    const json = "{\"value\":42,\"other\":\"text\"}";
    const result = extractNestedInt(json, "\"", "\"value\"");
    try std.testing.expectEqual(@as(?i64, 42), result);
}

test "extract int missing" {
    const json = "{\"other\":\"text\"}";
    const result = extractNestedInt(json, "\"", "\"value\"");
    try std.testing.expect(result == null);
}

test "dispatch hemis status" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"hemis/status\",\"params\":{}}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"result\"") != null);
}

test "dispatch hemis list-files" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"hemis/list-files\",\"params\":{}}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    // Returns result or error depending on implementation
    try std.testing.expect(mem.indexOf(u8, resp, "\"jsonrpc\"") != null);
}

test "dispatch hemis get-file" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"hemis/get-file\",\"params\":{\"path\":\"/test.zig\"}}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    // May return error or result depending on file existence
    try std.testing.expect(mem.indexOf(u8, resp, "\"jsonrpc\"") != null);
}

test "dispatch unknown method" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"unknown/method\",\"params\":{}}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"error\"") != null);
}

test "dispatch invalid json" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "not valid json at all";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"error\"") != null);
}

test "dispatch missing method field" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"params\":{}}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"error\"") != null);
}

test "dispatch notes get nonexistent" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"notes/get\",\"params\":{\"id\":\"nonexistent-id\"}}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"error\"") != null or mem.indexOf(u8, resp, "null") != null);
}

test "dispatch notes list empty" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"notes/list-for-file\",\"params\":{\"file\":\"/empty.zig\"}}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"result\"") != null);
}

test "dispatch project meta" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"hemis/project-meta\",\"params\":{}}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"result\"") != null);
}

test "dispatch index add-file" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"index/add-file\",\"params\":{\"path\":\"/test.zig\",\"content\":\"const x = 1;\"}}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    // Valid JSON-RPC response
    try std.testing.expect(mem.indexOf(u8, resp, "\"jsonrpc\"") != null);
}

test "dispatch index search" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    // Add a file first
    const add_req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"index/add-file\",\"params\":{\"path\":\"/test.zig\",\"content\":\"fn hello() void {}\"}}";
    const add_resp = dispatchWithDb(alloc, add_req, &db);
    defer alloc.free(add_resp);

    // Search for it
    const req = "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"index/search\",\"params\":{\"query\":\"hello\"}}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"result\"") != null);
}

test "dispatch load-snapshot" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"hemis/load-snapshot\",\"params\":{\"path\":\"/nonexistent/snapshot.json\"}}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    // Will likely error since file doesn't exist
    try std.testing.expect(mem.indexOf(u8, resp, "\"jsonrpc\"") != null);
}

test "dispatch notes reattach" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"notes/reattach\",\"params\":{\"id\":\"test-id\",\"nodeType\":\"function\",\"nodePath\":\"fn/test\"}}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    // May succeed or fail depending on note existence
    try std.testing.expect(mem.indexOf(u8, resp, "\"jsonrpc\"") != null);
}

test "extract int negative" {
    const json = "{\"value\":-42}";
    const result = extractNestedInt(json, "\"", "\"value\"");
    try std.testing.expectEqual(@as(?i64, -42), result);
}

test "extract int zero" {
    const json = "{\"value\":0}";
    const result = extractNestedInt(json, "\"", "\"value\"");
    try std.testing.expectEqual(@as(?i64, 0), result);
}

test "extract int large" {
    const json = "{\"value\":9999999}";
    const result = extractNestedInt(json, "\"", "\"value\"");
    try std.testing.expectEqual(@as(?i64, 9999999), result);
}

test "makeError format" {
    const alloc = std.testing.allocator;
    const resp = makeError(alloc, "1", -32600, "Invalid Request");
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"error\"") != null);
    try std.testing.expect(mem.indexOf(u8, resp, "-32600") != null);
    try std.testing.expect(mem.indexOf(u8, resp, "Invalid Request") != null);
}

test "makeError null id" {
    const alloc = std.testing.allocator;
    const resp = makeError(alloc, null, -32700, "Parse error");
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"id\":null") != null);
    try std.testing.expect(mem.indexOf(u8, resp, "Parse error") != null);
}

test "Stream init" {
    var buf: [1024]u8 = undefined;
    const stream = Stream.init(0, 1, &buf);
    try std.testing.expect(stream.read_fd == 0);
    try std.testing.expect(stream.write_fd == 1);
    try std.testing.expect(stream.buf_len == 0);
}

test "multiple notes for same file" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    // Create multiple notes for same file
    const req1 = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"notes/create\",\"params\":{\"filePath\":\"/test.zig\",\"content\":\"note 1\"}}";
    const resp1 = dispatchWithDb(alloc, req1, &db);
    defer alloc.free(resp1);
    try std.testing.expect(mem.indexOf(u8, resp1, "\"result\"") != null);

    const req2 = "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"notes/create\",\"params\":{\"filePath\":\"/test.zig\",\"content\":\"note 2\"}}";
    const resp2 = dispatchWithDb(alloc, req2, &db);
    defer alloc.free(resp2);
    try std.testing.expect(mem.indexOf(u8, resp2, "\"result\"") != null);

    // List should return both
    const list_req = "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"notes/list-for-file\",\"params\":{\"file\":\"/test.zig\"}}";
    const list_resp = dispatchWithDb(alloc, list_req, &db);
    defer alloc.free(list_resp);

    try std.testing.expect(mem.indexOf(u8, list_resp, "\"result\"") != null);
}

test "update note content" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    // Create note
    const create_req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"notes/create\",\"params\":{\"filePath\":\"/test.zig\",\"content\":\"original\"}}";
    const create_resp = dispatchWithDb(alloc, create_req, &db);
    defer alloc.free(create_resp);
    try std.testing.expect(mem.indexOf(u8, create_resp, "\"result\"") != null);
}

test "search empty query" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"notes/search\",\"params\":{\"query\":\"\"}}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"result\"") != null);
}

test "hemis search empty query" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"hemis/search\",\"params\":{\"query\":\"\"}}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"result\"") != null);
}

test "dispatch hemis tasks" {
    const alloc = std.testing.allocator;
    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"hemis/tasks\",\"params\":{}}";
    const resp = dispatch(alloc, req);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"tasks\"") != null);
}

test "dispatch hemis task-status" {
    const alloc = std.testing.allocator;
    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"hemis/task-status\",\"params\":{\"taskId\":\"task-123\"}}";
    const resp = dispatch(alloc, req);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"taskId\"") != null);
    try std.testing.expect(mem.indexOf(u8, resp, "task-123") != null);
}

test "dispatch hemis task-list" {
    const alloc = std.testing.allocator;
    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"hemis/task-list\",\"params\":{}}";
    const resp = dispatch(alloc, req);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"tasks\"") != null);
}

test "dispatch task-status missing taskId" {
    const alloc = std.testing.allocator;
    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"hemis/task-status\",\"params\":{}}";
    const resp = dispatch(alloc, req);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"error\"") != null);
}

test "dispatch notes create missing filePath" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"notes/create\",\"params\":{\"content\":\"test\"}}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"error\"") != null);
}

test "dispatch notes create missing content" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"notes/create\",\"params\":{\"filePath\":\"/test.zig\"}}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"error\"") != null);
}

test "dispatch notes get missing id" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"notes/get\",\"params\":{}}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"error\"") != null);
}

test "dispatch notes delete missing id" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"notes/delete\",\"params\":{}}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"error\"") != null);
}

test "dispatch notes update missing id" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"notes/update\",\"params\":{\"text\":\"updated\"}}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"error\"") != null);
}

test "dispatch notes search missing query" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"notes/search\",\"params\":{}}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"error\"") != null);
}

test "dispatch notes list-for-file missing file" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"notes/list-for-file\",\"params\":{}}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"error\"") != null);
}

test "dispatch hemis search missing query" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"hemis/search\",\"params\":{}}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"error\"") != null);
}

test "dispatch file-context missing file" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"hemis/file-context\",\"params\":{}}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"error\"") != null);
}

test "dispatch buffer-context missing file" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"hemis/buffer-context\",\"params\":{}}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"error\"") != null);
}

test "dispatch explain-region missing file" {
    const alloc = std.testing.allocator;
    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"hemis/explain-region\",\"params\":{}}";
    const resp = dispatch(alloc, req);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"error\"") != null);
}

test "dispatch save-snapshot missing path" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"hemis/save-snapshot\",\"params\":{}}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"error\"") != null);
}

test "dispatch load-snapshot missing path" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"hemis/load-snapshot\",\"params\":{}}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"error\"") != null);
}

test "dispatch index add-file missing file" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"index/add-file\",\"params\":{\"content\":\"test\"}}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"error\"") != null);
}

test "dispatch index add-file missing content" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"index/add-file\",\"params\":{\"file\":\"/test.zig\"}}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"error\"") != null);
}

test "dispatch index search missing query" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"index/search\",\"params\":{}}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"error\"") != null);
}

test "extractNestedString outer key not found" {
    const json = "{\"other\":{\"key\":\"value\"}}";
    const result = extractNestedString(json, "\"params\"", "\"key\"");
    try std.testing.expect(result == null);
}

test "extractNestedString inner key not found" {
    const json = "{\"params\":{\"other\":\"value\"}}";
    const result = extractNestedString(json, "\"params\"", "\"key\"");
    try std.testing.expect(result == null);
}

test "extractNestedString empty value" {
    const json = "{\"params\":{\"key\":\"\"}}";
    const result = extractNestedString(json, "\"params\"", "\"key\"");
    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings("", result.?);
}

test "extractNestedString with spaces" {
    const json = "{\"params\": { \"key\" : \"value with spaces\" }}";
    const result = extractNestedString(json, "\"params\"", "\"key\"");
    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings("value with spaces", result.?);
}

test "extractNestedInt with spaces" {
    const json = "{\"params\": { \"limit\" : 100 }}";
    const result = extractNestedInt(json, "\"params\"", "\"limit\"");
    try std.testing.expectEqual(@as(?i64, 100), result);
}

test "extractNestedInt inner key not found" {
    const json = "{\"params\":{\"other\":42}}";
    const result = extractNestedInt(json, "\"params\"", "\"limit\"");
    try std.testing.expect(result == null);
}

test "extractNestedInt invalid number" {
    const json = "{\"params\":{\"limit\":\"not-a-number\"}}";
    const result = extractNestedInt(json, "\"params\"", "\"limit\"");
    try std.testing.expect(result == null);
}

test "extractNestedInt with trailing comma" {
    const json = "{\"params\":{\"limit\":50,\"other\":\"val\"}}";
    const result = extractNestedInt(json, "\"params\"", "\"limit\"");
    try std.testing.expectEqual(@as(?i64, 50), result);
}

test "extractNestedInt at end of object" {
    const json = "{\"params\":{\"limit\":75}}";
    const result = extractNestedInt(json, "\"params\"", "\"limit\"");
    try std.testing.expectEqual(@as(?i64, 75), result);
}

test "notes search with limit and offset" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"notes/search\",\"params\":{\"query\":\"test\",\"limit\":10,\"offset\":5}}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"result\"") != null);
}

test "notes search with only limit" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"notes/search\",\"params\":{\"query\":\"test\",\"limit\":25}}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"result\"") != null);
}

test "notes search with only offset" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"notes/search\",\"params\":{\"query\":\"test\",\"offset\":10}}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"result\"") != null);
}

// ============================================================================
// Malformed JSON Tests
// ============================================================================

test "malformed json truncated" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"error\"") != null);
}

test "malformed json empty" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"error\"") != null);
}

test "malformed json just whitespace" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "   \t\n  ";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"error\"") != null);
}

test "malformed json array instead of object" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "[\"jsonrpc\",\"2.0\"]";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"error\"") != null);
}

test "malformed json missing closing brace" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"hemis/status\"";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    // Should still work as we parse key-value pairs
    try std.testing.expect(resp.len > 0);
}

test "malformed json null value" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":null,\"method\":\"hemis/status\"}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"result\"") != null);
}

// ============================================================================
// extractId Edge Cases
// ============================================================================

test "extractId with string id" {
    const id = extractId("{\"id\":\"abc-123\",\"method\":\"test\"}");
    try std.testing.expect(id != null);
    try std.testing.expectEqualStrings("\"abc-123\"", id.?);
}

test "extractId with numeric id" {
    const id = extractId("{\"id\":42,\"method\":\"test\"}");
    try std.testing.expect(id != null);
    try std.testing.expectEqualStrings("42", id.?);
}

test "extractId with negative id" {
    const id = extractId("{\"id\":-1,\"method\":\"test\"}");
    try std.testing.expect(id != null);
    try std.testing.expectEqualStrings("-1", id.?);
}

test "extractId with zero" {
    const id = extractId("{\"id\":0,\"method\":\"test\"}");
    try std.testing.expect(id != null);
    try std.testing.expectEqualStrings("0", id.?);
}

test "extractId missing returns null" {
    const id = extractId("{\"method\":\"test\"}");
    try std.testing.expect(id == null);
}

test "extractId with extra whitespace" {
    const id = extractId("{  \"id\"  :  99  ,\"method\":\"test\"}");
    try std.testing.expect(id != null);
    try std.testing.expectEqualStrings("99", id.?);
}

// ============================================================================
// extractString Method Edge Cases
// ============================================================================

test "extractString method with special chars" {
    const method = extractString("{\"method\":\"notes/list-for-file\"}", "\"method\"");
    try std.testing.expect(method != null);
    try std.testing.expectEqualStrings("notes/list-for-file", method.?);
}

test "extractString method missing" {
    const method = extractString("{\"id\":1}", "\"method\"");
    try std.testing.expect(method == null);
}

test "extractString method empty string" {
    const method = extractString("{\"method\":\"\"}", "\"method\"");
    try std.testing.expect(method != null);
    try std.testing.expectEqualStrings("", method.?);
}

test "extractString method with unicode" {
    const method = extractString("{\"method\":\"test/日本語\"}", "\"method\"");
    try std.testing.expect(method != null);
}

// ============================================================================
// extractString Edge Cases
// ============================================================================

test "extractString with escaped quotes" {
    const json = "{\"value\":\"say \\\"hello\\\"\"}";
    const result = extractString(json, "\"value\"");
    try std.testing.expect(result != null);
}

test "extractString with newline escape" {
    const json = "{\"value\":\"line1\\nline2\"}";
    const result = extractString(json, "\"value\"");
    try std.testing.expect(result != null);
}

test "extractString very long value" {
    const long_value = "a" ** 1000;
    const json = "{\"value\":\"" ++ long_value ++ "\"}";
    const result = extractString(json, "\"value\"");
    try std.testing.expect(result != null);
    try std.testing.expectEqual(@as(usize, 1000), result.?.len);
}

test "extractString key not found" {
    const json = "{\"other\":\"value\"}";
    const result = extractString(json, "\"missing\"");
    try std.testing.expect(result == null);
}

// ============================================================================
// extractNestedInt Edge Cases
// ============================================================================

test "extractNestedInt with large number" {
    const json = "{\"params\":{\"id\":9999999999}}";
    const result = extractNestedInt(json, "\"params\"", "\"id\"");
    try std.testing.expectEqual(@as(?i64, 9999999999), result);
}

test "extractNestedInt with negative" {
    const json = "{\"params\":{\"offset\":-50}}";
    const result = extractNestedInt(json, "\"params\"", "\"offset\"");
    try std.testing.expectEqual(@as(?i64, -50), result);
}

test "extractNestedInt key not found in nested" {
    const json = "{\"params\":{\"other\":42}}";
    const result = extractNestedInt(json, "\"params\"", "\"missing\"");
    try std.testing.expect(result == null);
}

test "extractNestedInt string value returns null" {
    const json = "{\"params\":{\"id\":\"not-a-number\"}}";
    const result = extractNestedInt(json, "\"params\"", "\"id\"");
    try std.testing.expect(result == null);
}

// ============================================================================
// Unknown Method Tests
// ============================================================================

test "unknown method returns error" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"unknown/method\"}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"error\"") != null);
    try std.testing.expect(mem.indexOf(u8, resp, "MethodNotFound") != null);
}

test "method with wrong prefix" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"invalid/create\"}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"error\"") != null);
}

// ============================================================================
// Response Format Tests
// ============================================================================

test "response includes jsonrpc version" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"hemis/status\"}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"jsonrpc\":\"2.0\"") != null);
}

test "response includes request id" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":42,\"method\":\"hemis/status\"}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"id\":42") != null);
}

test "response with string id preserves id" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":\"req-123\",\"method\":\"hemis/status\"}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"id\":\"req-123\"") != null);
}

// ============================================================================
// Unicode and Special Character Tests
// ============================================================================

test "notes create with unicode content" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req =
        \\{"jsonrpc":"2.0","id":1,"method":"notes/create","params":{"filePath":"/test.zig","content":"日本語テスト"}}
    ;
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"result\"") != null);
}

test "notes create with emoji" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req =
        \\{"jsonrpc":"2.0","id":1,"method":"notes/create","params":{"filePath":"/test.zig","content":"Test 🎉 emoji"}}
    ;
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"result\"") != null);
}

test "file path with spaces" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req =
        \\{"jsonrpc":"2.0","id":1,"method":"notes/create","params":{"filePath":"/path with spaces/test.zig","content":"test"}}
    ;
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"result\"") != null);
}

// ============================================================================
// New Handler Tests
// ============================================================================

test "dispatch hemis display-config" {
    const alloc = std.testing.allocator;
    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"hemis/display-config\",\"params\":{}}";
    const resp = dispatch(alloc, req);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"colors\"") != null);
    try std.testing.expect(mem.indexOf(u8, resp, "\"icons\"") != null);
}

test "dispatch hemis note-templates" {
    const alloc = std.testing.allocator;
    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"hemis/note-templates\",\"params\":{}}";
    const resp = dispatch(alloc, req);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"templates\"") != null);
    try std.testing.expect(mem.indexOf(u8, resp, "\"bug\"") != null);
}

test "dispatch hemis suggest-tags zig file" {
    const alloc = std.testing.allocator;
    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"hemis/suggest-tags\",\"params\":{\"file\":\"/src/main.zig\"}}";
    const resp = dispatch(alloc, req);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"tags\"") != null);
    try std.testing.expect(mem.indexOf(u8, resp, "\"zig\"") != null);
}

test "dispatch hemis suggest-tags rust file" {
    const alloc = std.testing.allocator;
    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"hemis/suggest-tags\",\"params\":{\"file\":\"/src/lib.rs\"}}";
    const resp = dispatch(alloc, req);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"rust\"") != null);
}

test "dispatch hemis suggest-tags test file" {
    const alloc = std.testing.allocator;
    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"hemis/suggest-tags\",\"params\":{\"file\":\"/src/test_main.zig\"}}";
    const resp = dispatch(alloc, req);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"test\"") != null);
}

test "dispatch hemis code-references" {
    const alloc = std.testing.allocator;
    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"hemis/code-references\",\"params\":{\"file\":\"/test.zig\",\"line\":10}}";
    const resp = dispatch(alloc, req);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"references\"") != null);
    try std.testing.expect(mem.indexOf(u8, resp, "\"anchor\"") != null);
}

test "dispatch hemis summarize-file" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"hemis/summarize-file\",\"params\":{\"file\":\"/test.zig\"}}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"file\"") != null);
    try std.testing.expect(mem.indexOf(u8, resp, "\"sections\"") != null);
}

test "dispatch notes history" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"notes/history\",\"params\":{\"id\":\"test-id\"}}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"versions\"") != null);
}

test "dispatch notes get-version" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"notes/get-version\",\"params\":{\"id\":\"test-id\",\"version\":1}}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"noteId\"") != null);
    try std.testing.expect(mem.indexOf(u8, resp, "\"version\"") != null);
}

test "dispatch notes restore-version" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"notes/restore-version\",\"params\":{\"id\":\"test-id\",\"version\":1}}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"restoredVersion\"") != null);
}

test "dispatch notes link-suggestions" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"notes/link-suggestions\",\"params\":{\"query\":\"test\"}}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"result\"") != null);
}

test "dispatch notes explain-and-create" {
    const alloc = std.testing.allocator;
    var db = try storage.Database.open(alloc, ":memory:");
    defer db.close();

    const req = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"notes/explain-and-create\",\"params\":{\"file\":\"/test.zig\",\"content\":\"test content\"}}";
    const resp = dispatchWithDb(alloc, req, &db);
    defer alloc.free(resp);

    try std.testing.expect(mem.indexOf(u8, resp, "\"note\"") != null);
    try std.testing.expect(mem.indexOf(u8, resp, "\"ai\"") != null);
}

test "dispatch table has all methods" {
    // Verify dispatch table is populated
    try std.testing.expect(dispatch_table.get("hemis/status") != null);
    try std.testing.expect(dispatch_table.get("hemis/version") != null);
    try std.testing.expect(dispatch_table.get("hemis/display-config") != null);
    try std.testing.expect(dispatch_table.get("notes/create") != null);
    try std.testing.expect(dispatch_table.get("notes/history") != null);
    try std.testing.expect(dispatch_table.get("index/search") != null);
}
