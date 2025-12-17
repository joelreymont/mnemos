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
    }

    return error.MethodNotFound;
}

fn handleStatus(alloc: Allocator, db: ?*storage.Database) ![]const u8 {
    const note_count: i64 = if (db) |d| storage.countNotes(d) catch 0 else 0;
    return try std.fmt.allocPrint(alloc,
        \\{{"version":"0.1.0","language":"zig","status":"ok","noteCount":{d}}}
    , .{note_count});
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

/// Extract nested string value
fn extractNestedString(json_str: []const u8, outer_key: []const u8, inner_key: []const u8) ?[]const u8 {
    const outer_pos = mem.indexOf(u8, json_str, outer_key) orelse return null;
    const after_outer = json_str[outer_pos..];
    return extractString(after_outer, inner_key);
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
