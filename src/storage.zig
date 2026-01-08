//! Markdown-based storage layer
//!
//! Stores notes as individual .md files with YAML frontmatter in .mnemos/notes/
//! Provides an in-memory index for fast lookups by file path.

const std = @import("std");
const mem = std.mem;
const fs = std.fs;
const process = std.process;
const Allocator = mem.Allocator;

pub const StorageError = error{
    NotFound,
    InvalidFrontmatter,
    InvalidJson,
    IoError,
    OutOfMemory,
    InvalidNoteId,
    InvalidFieldValue,
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

/// Search hit for content search
pub const SearchHit = struct {
    file: []const u8,
    line: usize,
    column: usize,
    text: []const u8,
};

// Size limits
const MAX_NOTE_SIZE: usize = 10 * 1024 * 1024; // 10MB max note file
const MAX_CONTENT_SIZE: usize = 1024 * 1024; // 1MB max for search content files
const MAX_RESULTS: usize = 100; // Max search results
const FRONTMATTER_BUFFER: usize = 512; // Bytes to read for frontmatter

/// Normalize a file path for consistent indexing
/// Strips leading "./" to avoid duplicates like "./foo" vs "foo"
fn normalizePath(path: []const u8) []const u8 {
    var result = path;
    // Strip leading "./"
    while (result.len >= 2 and mem.startsWith(u8, result, "./")) {
        result = result[2..];
    }
    return result;
}

fn resolveNotesDir(alloc: Allocator, project_root: []const u8) ![]const u8 {
    if (process.getEnvVarOwned(alloc, "MNEMOS_NOTES_PATH")) |env_path| {
        if (env_path.len == 0) {
            alloc.free(env_path);
        } else if (std.fs.path.isAbsolute(env_path)) {
            return env_path;
        } else {
            defer alloc.free(env_path);
            return std.fs.path.join(alloc, &.{ project_root, env_path });
        }
    } else |_| {}

    return std.fs.path.join(alloc, &.{ project_root, ".mnemos", "notes" });
}

/// Markdown-based storage for notes
pub const Storage = struct {
    alloc: Allocator,
    notes_dir: []const u8,
    project_root: []const u8,
    /// Maps file_path -> ArrayList of note IDs
    index: std.StringHashMap(std.ArrayList([]const u8)),
    /// Maps src note id -> list of dst note ids (links)
    edges_by_src: std.StringHashMap(std.ArrayList([]const u8)),
    /// Maps dst note id -> list of src note ids (backlinks)
    edges_by_dst: std.StringHashMap(std.ArrayList([]const u8)),
    rg_path: ?[]const u8,
    /// Cached note count for O(1) lookups
    note_count: usize,

    /// Initialize storage, creating directories and rebuilding index
    pub fn init(alloc: Allocator, project_root: []const u8) !Storage {
        const notes_dir = try resolveNotesDir(alloc, project_root);
        defer alloc.free(notes_dir);
        return initWithNotesDir(alloc, project_root, notes_dir);
    }

    pub fn initWithNotesDir(alloc: Allocator, project_root: []const u8, notes_dir: []const u8) !Storage {
        const owned_root = try alloc.dupe(u8, project_root);
        errdefer alloc.free(owned_root);

        const owned_notes_dir = try alloc.dupe(u8, notes_dir);
        errdefer alloc.free(owned_notes_dir);

        // Ensure project .mnemos exists for snapshots and metadata
        const mnemos_dir = try std.fs.path.join(alloc, &.{ owned_root, ".mnemos" });
        defer alloc.free(mnemos_dir);

        fs.cwd().makePath(mnemos_dir) catch |err| {
            if (err != error.PathAlreadyExists) return StorageError.IoError;
        };
        fs.cwd().makePath(owned_notes_dir) catch |err| {
            if (err != error.PathAlreadyExists) return StorageError.IoError;
        };

        var result = Storage{
            .alloc = alloc,
            .notes_dir = owned_notes_dir,
            .project_root = owned_root,
            .index = std.StringHashMap(std.ArrayList([]const u8)).init(alloc),
            .edges_by_src = std.StringHashMap(std.ArrayList([]const u8)).init(alloc),
            .edges_by_dst = std.StringHashMap(std.ArrayList([]const u8)).init(alloc),
            .rg_path = detectRipgrep(alloc),
            .note_count = 0,
        };

        // Rebuild index from existing files (also updates note_count)
        try result.rebuildIndex();

        return result;
    }

    /// Free all resources
    pub fn deinit(self: *Storage) void {
        // Free index entries
        var it = self.index.iterator();
        while (it.next()) |entry| {
            // Free all note IDs in the list
            for (entry.value_ptr.items) |id| {
                self.alloc.free(id);
            }
            entry.value_ptr.deinit(self.alloc);
            // Free the file_path key
            self.alloc.free(entry.key_ptr.*);
        }
        self.index.deinit();
        self.clearEdges();
        self.edges_by_src.deinit();
        self.edges_by_dst.deinit();

        if (self.rg_path) |p| self.alloc.free(p);
        self.alloc.free(self.notes_dir);
        self.alloc.free(self.project_root);
    }

    /// Create a new note
    pub fn createNote(self: *Storage, note: Note) !void {
        try validateNoteId(note.id);

        const filepath = try self.noteFilePath(note.id);
        defer self.alloc.free(filepath);

        const content = try writeFrontmatter(self.alloc, note);
        defer self.alloc.free(content);

        try writeFileAtomic(self.alloc, filepath, content);

        // Add to index and update count
        try self.addToIndex(note.file_path, note.id);
        self.note_count += 1;
        try self.updateEdgesForNote(note.id, note.content);
    }

    /// Get a note by ID
    pub fn getNote(self: *Storage, alloc: Allocator, id: []const u8) !?Note {
        try validateNoteId(id);

        const filepath = try self.noteFilePath(id);
        defer self.alloc.free(filepath);

        const file = fs.cwd().openFile(filepath, .{}) catch |err| {
            if (err == error.FileNotFound) return null;
            return StorageError.IoError;
        };
        defer file.close();

        const content = file.readToEndAlloc(alloc, MAX_NOTE_SIZE) catch return StorageError.IoError;
        defer alloc.free(content);

        return try parseFrontmatter(alloc, id, content);
    }

    /// Update a note's content
    pub fn updateNote(self: *Storage, id: []const u8, content: ?[]const u8, tags: ?[]const u8, updated_at: []const u8) !void {
        _ = tags; // Tags not yet implemented
        try validateNoteId(id);

        const existing = try self.getNote(self.alloc, id);
        if (existing == null) return; // Note doesn't exist, nothing to update

        defer freeNoteFields(self.alloc, existing.?);

        const new_note = Note{
            .id = existing.?.id,
            .file_path = existing.?.file_path,
            .node_path = existing.?.node_path,
            .node_text_hash = existing.?.node_text_hash,
            .line_number = existing.?.line_number,
            .column_number = existing.?.column_number,
            .content = content orelse existing.?.content,
            .created_at = existing.?.created_at,
            .updated_at = updated_at,
        };

        const filepath = try self.noteFilePath(id);
        defer self.alloc.free(filepath);

        const file_content = try writeFrontmatter(self.alloc, new_note);
        defer self.alloc.free(file_content);

        try writeFileAtomic(self.alloc, filepath, file_content);
        try self.updateEdgesForNote(id, new_note.content);
    }

    /// Delete a note by ID
    pub fn deleteNote(self: *Storage, id: []const u8) !void {
        try validateNoteId(id);

        // First get the note to find its file_path for index removal
        const note = try self.getNote(self.alloc, id);
        if (note == null) return; // Already doesn't exist
        defer freeNoteFields(self.alloc, note.?);

        // Remove from index
        self.removeFromIndex(note.?.file_path, id);
        self.removeEdgesForNote(id);

        // Delete the file
        const filepath = try self.noteFilePath(id);
        defer self.alloc.free(filepath);

        fs.cwd().deleteFile(filepath) catch |err| {
            std.log.warn("deleteNote: failed to delete '{s}': {s}", .{ filepath, @errorName(err) });
        };

        // Update count
        if (self.note_count > 0) self.note_count -= 1;
    }

    /// Reattach note to new location
    pub fn reattachNote(
        self: *Storage,
        id: []const u8,
        file_path: ?[]const u8,
        line: i64,
        column: ?i64,
        node_path: ?[]const u8,
        node_text_hash: ?[]const u8,
        updated_at: []const u8,
    ) !void {
        try validateNoteId(id);

        const existing = try self.getNote(self.alloc, id);
        if (existing == null) return;
        defer freeNoteFields(self.alloc, existing.?);

        const old_file_path = existing.?.file_path;
        const new_file_path = file_path orelse old_file_path;

        // Update index if file_path changed
        if (file_path != null and !mem.eql(u8, old_file_path, new_file_path)) {
            self.removeFromIndex(old_file_path, id);
            try self.addToIndex(new_file_path, id);
        }

        // Create updated note - need to duplicate strings for the new file path if changed
        const owned_file_path = if (file_path != null)
            try self.alloc.dupe(u8, new_file_path)
        else
            try self.alloc.dupe(u8, existing.?.file_path);
        defer self.alloc.free(owned_file_path);

        const owned_node_path = if (node_path) |np| try self.alloc.dupe(u8, np) else null;
        defer if (owned_node_path) |np| self.alloc.free(np);

        const owned_hash = if (node_text_hash) |h| try self.alloc.dupe(u8, h) else null;
        defer if (owned_hash) |h| self.alloc.free(h);

        const new_note = Note{
            .id = existing.?.id,
            .file_path = owned_file_path,
            .node_path = owned_node_path,
            .node_text_hash = owned_hash,
            .line_number = line,
            .column_number = column orelse existing.?.column_number,
            .content = existing.?.content,
            .created_at = existing.?.created_at,
            .updated_at = updated_at,
        };

        const filepath = try self.noteFilePath(id);
        defer self.alloc.free(filepath);

        const content = try writeFrontmatter(self.alloc, new_note);
        defer self.alloc.free(content);

        try writeFileAtomic(self.alloc, filepath, content);
    }

    /// Maximum notes to return from listProjectNotes to prevent memory exhaustion
    const MAX_PROJECT_NOTES: usize = 10000;

    /// List all notes in the project (with safety limit)
    pub fn listProjectNotes(self: *Storage, alloc: Allocator) ![]Note {
        return self.listProjectNotesPaged(alloc, MAX_PROJECT_NOTES, 0);
    }

    /// List notes with pagination support
    pub fn listProjectNotesPaged(self: *Storage, alloc: Allocator, limit: usize, offset: usize) ![]Note {
        var notes: std.ArrayList(Note) = .{};
        errdefer {
            for (notes.items) |note| freeNoteFields(alloc, note);
            notes.deinit(alloc);
        }

        var dir = fs.cwd().openDir(self.notes_dir, .{ .iterate = true }) catch |err| {
            if (err == error.FileNotFound) return notes.toOwnedSlice(alloc);
            return StorageError.IoError;
        };
        defer dir.close();

        var iter = dir.iterate();
        var skipped: usize = 0;

        while (iter.next() catch return StorageError.IoError) |entry| {
            if (entry.kind != .file) continue;
            if (!mem.endsWith(u8, entry.name, ".md")) continue;

            // Apply offset
            if (skipped < offset) {
                skipped += 1;
                continue;
            }

            // Apply limit
            if (notes.items.len >= limit) break;

            // Extract ID from filename (remove .md)
            const id = entry.name[0 .. entry.name.len - 3];

            if (try self.getNote(alloc, id)) |note| {
                try notes.append(alloc, note);
            }
        }

        // Sort by created_at descending
        const items = notes.items;
        std.mem.sort(Note, items, {}, notesByCreatedAtDesc);

        return notes.toOwnedSlice(alloc);
    }

    /// Get notes for a specific file using the index
    pub fn getNotesForFile(self: *Storage, alloc: Allocator, file_path: []const u8) ![]Note {
        const normalized = normalizePath(file_path);
        var notes: std.ArrayList(Note) = .{};
        errdefer {
            for (notes.items) |note| freeNoteFields(alloc, note);
            notes.deinit(alloc);
        }

        if (self.index.get(normalized)) |id_list| {
            for (id_list.items) |id| {
                if (try self.getNote(alloc, id)) |note| {
                    try notes.append(alloc, note);
                }
            }
        }

        // Sort by created_at descending
        const items = notes.items;
        std.mem.sort(Note, items, {}, notesByCreatedAtDesc);

        return notes.toOwnedSlice(alloc);
    }

    /// Count total notes (O(1) using cached count)
    pub fn countNotes(self: *Storage) !i64 {
        return @intCast(self.note_count);
    }

    pub fn countEdges(self: *Storage) usize {
        var count: usize = 0;
        var it = self.edges_by_src.iterator();
        while (it.next()) |entry| {
            count += entry.value_ptr.items.len;
        }
        return count;
    }

    /// Add a file to the index (stub - not implemented in markdown storage)
    pub fn addFile(_: *Storage, _: []const u8, _: []const u8, _: []const u8) !void {
        // No-op: markdown storage doesn't index file content
    }

    /// Search source file content (uses ripgrep if available, otherwise fallback)
    pub fn searchContent(self: *Storage, alloc: Allocator, query: []const u8) ![]SearchHit {
        if (query.len == 0) return alloc.alloc(SearchHit, 0);

        if (self.rg_path) |rg| {
            return self.searchContentWithRipgrep(alloc, rg, query);
        }
        return self.searchContentFallback(alloc, query);
    }

    /// Search source files using ripgrep with JSON output
    fn searchContentWithRipgrep(self: *Storage, alloc: Allocator, rg: []const u8, query: []const u8) ![]SearchHit {
        var hits: std.ArrayList(SearchHit) = .{};
        errdefer {
            for (hits.items) |hit| {
                alloc.free(hit.file);
                alloc.free(hit.text);
            }
            hits.deinit(alloc);
        }

        // Run ripgrep with JSON output: rg --json -n -i query --glob '!.mnemos' --glob '!.git' .
        const result = std.process.Child.run(.{
            .allocator = alloc,
            .argv = &.{ rg, "--json", "-n", "-i", "--glob", "!.mnemos", "--glob", "!.git", "--glob", "!node_modules", "--", query, self.project_root },
            .max_output_bytes = MAX_NOTE_SIZE, // 10MB max
        }) catch |err| {
            std.log.warn("ripgrep failed ({s}), using fallback search", .{@errorName(err)});
            return self.searchContentFallback(alloc, query);
        };
        defer alloc.free(result.stdout);
        defer alloc.free(result.stderr);

        // Parse JSON lines output
        var line_iter = mem.splitScalar(u8, result.stdout, '\n');
        while (line_iter.next()) |line| {
            if (line.len == 0) continue;
            if (hits.items.len >= MAX_RESULTS) break; // Limit results

            // Parse JSON line
            const parsed = std.json.parseFromSlice(std.json.Value, alloc, line, .{}) catch continue;
            defer parsed.deinit();

            const obj = parsed.value.object;
            const msg_type = obj.get("type") orelse continue;
            if (!mem.eql(u8, msg_type.string, "match")) continue;

            const data = obj.get("data") orelse continue;
            const data_obj = data.object;

            // Extract path
            const path_obj = data_obj.get("path") orelse continue;
            const path_text = path_obj.object.get("text") orelse continue;

            // Extract line number
            const line_num = data_obj.get("line_number") orelse continue;

            // Extract line text from submatches
            const submatches = data_obj.get("submatches") orelse continue;
            if (submatches.array.items.len == 0) continue;

            const lines_obj = data_obj.get("lines") orelse continue;
            const line_text = lines_obj.object.get("text") orelse continue;

            // Get column from first submatch
            var col: usize = 0;
            if (submatches.array.items.len > 0) {
                const submatch = submatches.array.items[0].object;
                if (submatch.get("start")) |start| {
                    col = std.math.cast(usize, start.integer) orelse 0;
                }
            }

            // Skip invalid line numbers
            const hit_line: usize = std.math.cast(usize, line_num.integer) orelse continue;

            const hit = SearchHit{
                .file = try alloc.dupe(u8, path_text.string),
                .line = hit_line,
                .column = col,
                .text = try alloc.dupe(u8, mem.trim(u8, line_text.string, "\r\n")),
            };
            try hits.append(alloc, hit);
        }

        return hits.toOwnedSlice(alloc);
    }

    /// Fallback content search without ripgrep - walks directory tree
    fn searchContentFallback(self: *Storage, alloc: Allocator, query: []const u8) ![]SearchHit {
        var hits: std.ArrayList(SearchHit) = .{};
        errdefer {
            for (hits.items) |hit| {
                alloc.free(hit.file);
                alloc.free(hit.text);
            }
            hits.deinit(alloc);
        }

        const query_lower = try std.ascii.allocLowerString(alloc, query);
        defer alloc.free(query_lower);

        // Walk project directory
        var dir = fs.cwd().openDir(self.project_root, .{ .iterate = true }) catch |err| {
            if (err == error.FileNotFound) return hits.toOwnedSlice(alloc);
            return StorageError.IoError;
        };
        defer dir.close();

        var walker = dir.walk(alloc) catch return StorageError.IoError;
        defer walker.deinit();

        while (walker.next() catch null) |entry| {
            if (hits.items.len >= MAX_RESULTS) break;
            if (entry.kind != .file) continue;

            // Skip hidden dirs and common non-source dirs
            if (mem.startsWith(u8, entry.path, ".mnemos")) continue;
            if (mem.startsWith(u8, entry.path, ".git")) continue;
            if (mem.indexOf(u8, entry.path, "node_modules") != null) continue;
            if (mem.indexOf(u8, entry.path, ".zig-cache") != null) continue;

            // Skip binary/non-text extensions
            const ext = std.fs.path.extension(entry.path);
            if (isBinaryExtension(ext)) continue;

            // Read file content - skip unreadable files (permissions, symlinks, etc)
            const file = dir.openFile(entry.path, .{}) catch |err| {
                std.log.debug("searchContent: skipping unreadable file {s}: {}", .{ entry.path, err });
                continue;
            };
            defer file.close();

            const content = file.readToEndAlloc(alloc, MAX_CONTENT_SIZE) catch |err| {
                std.log.debug("searchContent: skipping large/unreadable file {s}: {}", .{ entry.path, err });
                continue;
            };
            defer alloc.free(content);

            // Search line by line
            try searchLinesInContent(alloc, self.project_root, entry.path, content, query_lower, &hits);
        }

        return hits.toOwnedSlice(alloc);
    }

    fn isBinaryExtension(ext: []const u8) bool {
        const binary_exts = [_][]const u8{
            ".png",  ".jpg",   ".jpeg", ".gif", ".ico", ".webp", ".bmp",
            ".woff", ".woff2", ".ttf",  ".otf", ".eot", ".o",    ".a",
            ".so",   ".dylib", ".exe",  ".dll", ".zip", ".tar",  ".gz",
            ".bz2",  ".xz",    ".7z",   ".pdf", ".doc", ".docx", ".xls",
            ".xlsx",
        };
        for (binary_exts) |bin_ext| {
            if (mem.eql(u8, ext, bin_ext)) return true;
        }
        return false;
    }

    fn searchLinesInContent(
        alloc: Allocator,
        project_root: []const u8,
        rel_path: []const u8,
        content: []const u8,
        query_lower: []const u8,
        hits: *std.ArrayList(SearchHit),
    ) !void {
        var line_num: usize = 1;
        var line_start: usize = 0;

        for (content, 0..) |ch, i| {
            if (ch == '\n' or i == content.len - 1) {
                const line_end = if (ch == '\n') i else i + 1;
                const line_text = content[line_start..line_end];

                // Case-insensitive search without allocation
                if (indexOfIgnoreCase(line_text, query_lower)) |col| {
                    if (hits.items.len >= MAX_RESULTS) return;

                    const full_path = try std.fs.path.join(alloc, &.{ project_root, rel_path });
                    errdefer alloc.free(full_path);

                    const text = try alloc.dupe(u8, line_text);
                    errdefer alloc.free(text);

                    try hits.append(alloc, SearchHit{
                        .file = full_path,
                        .line = line_num,
                        .column = col,
                        .text = text,
                    });
                }

                line_num += 1;
                line_start = i + 1;
            }
        }
    }

    /// Delete all notes
    pub fn clearNotes(self: *Storage) !void {
        self.clearIndex();
        self.clearEdges();
        self.note_count = 0;

        // Delete all .md files
        var dir = fs.cwd().openDir(self.notes_dir, .{ .iterate = true }) catch |err| {
            if (err == error.FileNotFound) return;
            return StorageError.IoError;
        };
        defer dir.close();

        var iter = dir.iterate();
        var to_delete: std.ArrayList([]const u8) = .{};
        defer {
            for (to_delete.items) |name| self.alloc.free(name);
            to_delete.deinit(self.alloc);
        }

        while (iter.next() catch return StorageError.IoError) |entry| {
            if (entry.kind != .file) continue;
            if (!mem.endsWith(u8, entry.name, ".md")) continue;
            try to_delete.append(self.alloc, try self.alloc.dupe(u8, entry.name));
        }

        for (to_delete.items) |name| {
            dir.deleteFile(name) catch |err| {
                std.log.warn("clearNotes: failed to delete '{s}': {s}", .{ name, @errorName(err) });
            };
        }
    }

    /// Search notes by query (uses ripgrep if available, otherwise fallback)
    pub fn searchNotes(self: *Storage, alloc: Allocator, query: []const u8, limit: i64, offset: i64) ![]Note {
        if (self.rg_path) |rg| {
            return self.searchWithRipgrep(alloc, rg, query, limit, offset);
        }
        return self.searchFallback(alloc, query, limit, offset);
    }

    /// Search using ripgrep
    fn searchWithRipgrep(self: *Storage, alloc: Allocator, rg: []const u8, query: []const u8, limit: i64, offset: i64) ![]Note {
        var notes: std.ArrayList(Note) = .{};
        errdefer {
            for (notes.items) |note| freeNoteFields(alloc, note);
            notes.deinit(alloc);
        }

        // Run ripgrep to find matching files
        const result = std.process.Child.run(.{
            .allocator = alloc,
            .argv = &.{ rg, "-l", "-i", "--", query, self.notes_dir },
        }) catch |err| {
            std.log.warn("ripgrep failed ({s}), using fallback note search", .{@errorName(err)});
            return self.searchFallback(alloc, query, limit, offset);
        };
        defer alloc.free(result.stdout);
        defer alloc.free(result.stderr);

        // Parse output - each line is a matching file path
        var line_iter = mem.splitScalar(u8, result.stdout, '\n');
        var skipped: i64 = 0;
        var collected: i64 = 0;

        while (line_iter.next()) |line| {
            if (line.len == 0) continue;

            // Extract ID from path
            const basename = std.fs.path.basename(line);
            if (!mem.endsWith(u8, basename, ".md")) continue;
            const id = basename[0 .. basename.len - 3];

            // Apply offset
            if (skipped < offset) {
                skipped += 1;
                continue;
            }

            // Apply limit
            if (collected >= limit) break;

            if (try self.getNote(alloc, id)) |note| {
                try notes.append(alloc, note);
                collected += 1;
            }
        }

        return notes.toOwnedSlice(alloc);
    }

    /// Fallback search without ripgrep
    fn searchFallback(self: *Storage, alloc: Allocator, query: []const u8, limit: i64, offset: i64) ![]Note {
        var notes: std.ArrayList(Note) = .{};
        errdefer {
            for (notes.items) |note| freeNoteFields(alloc, note);
            notes.deinit(alloc);
        }

        const query_lower = try std.ascii.allocLowerString(alloc, query);
        defer alloc.free(query_lower);

        var dir = fs.cwd().openDir(self.notes_dir, .{ .iterate = true }) catch |err| {
            if (err == error.FileNotFound) return notes.toOwnedSlice(alloc);
            return StorageError.IoError;
        };
        defer dir.close();

        var iter = dir.iterate();
        var skipped: i64 = 0;
        var collected: i64 = 0;

        while (iter.next() catch return StorageError.IoError) |entry| {
            if (entry.kind != .file) continue;
            if (!mem.endsWith(u8, entry.name, ".md")) continue;

            const id = entry.name[0 .. entry.name.len - 3];

            if (try self.getNote(alloc, id)) |note| {
                // Case-insensitive content match without allocation
                if (indexOfIgnoreCase(note.content, query_lower) != null) {
                    if (skipped < offset) {
                        skipped += 1;
                        freeNoteFields(alloc, note);
                        continue;
                    }

                    if (collected >= limit) {
                        freeNoteFields(alloc, note);
                        break;
                    }

                    try notes.append(alloc, note);
                    collected += 1;
                } else {
                    freeNoteFields(alloc, note);
                }
            }
        }

        return notes.toOwnedSlice(alloc);
    }

    /// Export all notes as JSON
    pub fn exportNotesJson(self: *Storage, alloc: Allocator) ![]const u8 {
        const notes = try self.listProjectNotes(alloc);
        defer freeNotes(alloc, notes);

        // Build array of SnapshotNote for serialization
        const snapshot = try alloc.alloc(SnapshotNote, notes.len);
        defer alloc.free(snapshot);

        for (notes, 0..) |note, i| {
            const created_at = std.fmt.parseInt(i64, note.created_at, 10) catch 0;
            const updated_at = std.fmt.parseInt(i64, note.updated_at, 10) catch created_at;
            snapshot[i] = .{
                .id = note.id,
                .file = note.file_path,
                .projectRoot = self.project_root,
                .line = note.line_number orelse 1,
                .column = note.column_number orelse 0,
                .nodePath = note.node_path,
                .tags = "[]",
                .text = note.content,
                .summary = summarizeText(note.content),
                .commitSha = null,
                .blobSha = null,
                .nodeTextHash = note.node_text_hash,
                .createdAt = created_at,
                .updatedAt = updated_at,
            };
        }

        var edges: std.ArrayList(SnapshotEdge) = .{};
        defer edges.deinit(alloc);
        var edge_id: i64 = 1;
        var edge_it = self.edges_by_src.iterator();
        while (edge_it.next()) |entry| {
            for (entry.value_ptr.items) |dst| {
                try edges.append(alloc, .{
                    .id = edge_id,
                    .src = entry.key_ptr.*,
                    .dst = dst,
                    .kind = "link",
                    .projectRoot = self.project_root,
                    .updatedAt = @intCast(std.time.timestamp()),
                });
                edge_id += 1;
            }
        }

        const payload = SnapshotPayload{
            .version = 1,
            .projectRoot = self.project_root,
            .createdAt = @intCast(std.time.timestamp()),
            .counts = .{
                .files = 0,
                .notes = @intCast(notes.len),
                .embeddings = 0,
                .edges = @intCast(edges.items.len),
            },
            .notes = snapshot,
            .files = &.{},
            .embeddings = &.{},
            .edges = edges.items,
        };

        return std.fmt.allocPrint(alloc, "{f}", .{std.json.fmt(payload, .{})});
    }

    /// Import notes from JSON. Returns number of successfully imported notes.
    pub fn importNotesJson(self: *Storage, alloc: Allocator, json: []const u8) !usize {
        const parsed = std.json.parseFromSlice(SnapshotPayload, alloc, json, .{}) catch {
            return StorageError.InvalidJson;
        };
        defer parsed.deinit();

        var count: usize = 0;
        var failed: usize = 0;
        self.clearNotes() catch {};
        self.clearIndex();
        self.clearEdges();
        self.note_count = 0;

        for (parsed.value.notes) |sn| {
            // Validate content size to prevent memory exhaustion
            if (sn.text.len > MAX_CONTENT_SIZE) {
                std.log.warn("importNotesJson: skipping note '{s}': content too large ({d} bytes)", .{ sn.id, sn.text.len });
                failed += 1;
                continue;
            }

            var created_buf: [32]u8 = undefined;
            const created_at = std.fmt.bufPrint(&created_buf, "{d}", .{sn.createdAt}) catch "0";
            var updated_buf: [32]u8 = undefined;
            const updated_at = std.fmt.bufPrint(&updated_buf, "{d}", .{sn.updatedAt}) catch created_at;

            const note = Note{
                .id = sn.id,
                .file_path = sn.file,
                .node_path = sn.nodePath,
                .node_text_hash = sn.nodeTextHash,
                .line_number = sn.line,
                .column_number = sn.column,
                .content = sn.text,
                .created_at = created_at,
                .updated_at = updated_at,
            };
            self.createNote(note) catch |err| {
                std.log.warn("importNotesJson: failed to import note '{s}': {s}", .{ sn.id, @errorName(err) });
                failed += 1;
                continue;
            };
            count += 1;
        }

        if (failed > 0) {
            std.log.warn("importNotesJson: {d} of {d} notes failed to import", .{ failed, parsed.value.notes.len });
        }

        return count;
    }

    /// Rebuild the in-memory index from disk
    pub fn rebuildIndex(self: *Storage) !void {
        self.clearIndex();
        self.clearEdges();
        self.note_count = 0;

        // Scan directory
        var dir = fs.cwd().openDir(self.notes_dir, .{ .iterate = true }) catch |err| {
            if (err == error.FileNotFound) return;
            return StorageError.IoError;
        };
        defer dir.close();

        var iter = dir.iterate();
        while (iter.next() catch return StorageError.IoError) |entry| {
            if (entry.kind != .file) continue;
            if (!mem.endsWith(u8, entry.name, ".md")) continue;

            const id = entry.name[0 .. entry.name.len - 3];

            const file = dir.openFile(entry.name, .{}) catch continue;
            defer file.close();

            const content = file.readToEndAlloc(self.alloc, MAX_NOTE_SIZE) catch continue;
            defer self.alloc.free(content);

            const note = parseFrontmatter(self.alloc, id, content) catch continue;
            defer freeNoteFields(self.alloc, note);

            try self.addToIndex(note.file_path, note.id);
            try self.updateEdgesForNote(note.id, note.content);
            self.note_count += 1;
        }
    }

    /// Add a note ID to the index for a file path
    fn addToIndex(self: *Storage, file_path: []const u8, id: []const u8) !void {
        // Normalize path to avoid duplicates like "./foo" vs "foo"
        const normalized = normalizePath(file_path);

        const owned_id = try self.alloc.dupe(u8, id);
        errdefer self.alloc.free(owned_id);

        const gop = self.index.getOrPut(normalized) catch {
            return error.OutOfMemory;
        };

        if (!gop.found_existing) {
            const owned_key = self.alloc.dupe(u8, normalized) catch {
                // Remove the dangling hashmap entry
                _ = self.index.remove(normalized);
                return error.OutOfMemory;
            };
            gop.key_ptr.* = owned_key;
            gop.value_ptr.* = .{};

            gop.value_ptr.append(self.alloc, owned_id) catch {
                // Remove the dangling hashmap entry with its key
                self.alloc.free(owned_key);
                _ = self.index.remove(normalized);
                return error.OutOfMemory;
            };
        } else {
            try gop.value_ptr.append(self.alloc, owned_id);
        }
    }

    /// Remove a note ID from the index
    fn removeFromIndex(self: *Storage, file_path: []const u8, id: []const u8) void {
        const normalized = normalizePath(file_path);
        if (self.index.getPtr(normalized)) |list| {
            var i: usize = 0;
            while (i < list.items.len) {
                if (mem.eql(u8, list.items[i], id)) {
                    self.alloc.free(list.items[i]);
                    _ = list.swapRemove(i);
                } else {
                    i += 1;
                }
            }
        }
    }

    /// Build the full path to a note file. Caller owns returned memory.
    fn noteFilePath(self: *Storage, id: []const u8) ![]const u8 {
        const filename = try std.fmt.allocPrint(self.alloc, "{s}.md", .{id});
        defer self.alloc.free(filename);
        return std.fs.path.join(self.alloc, &.{ self.notes_dir, filename });
    }

    /// Clear all entries from the in-memory index
    fn clearIndex(self: *Storage) void {
        var it = self.index.iterator();
        while (it.next()) |entry| {
            for (entry.value_ptr.items) |id| {
                self.alloc.free(id);
            }
            entry.value_ptr.deinit(self.alloc);
            self.alloc.free(entry.key_ptr.*);
        }
        self.index.clearAndFree();
    }

    fn clearEdges(self: *Storage) void {
        var it_src = self.edges_by_src.iterator();
        while (it_src.next()) |entry| {
            for (entry.value_ptr.items) |id| {
                self.alloc.free(id);
            }
            entry.value_ptr.deinit(self.alloc);
            self.alloc.free(entry.key_ptr.*);
        }
        self.edges_by_src.clearAndFree();

        var it_dst = self.edges_by_dst.iterator();
        while (it_dst.next()) |entry| {
            for (entry.value_ptr.items) |id| {
                self.alloc.free(id);
            }
            entry.value_ptr.deinit(self.alloc);
            self.alloc.free(entry.key_ptr.*);
        }
        self.edges_by_dst.clearAndFree();
    }

    /// Update edge maps for a note's content.
    fn updateEdgesForNote(self: *Storage, id: []const u8, content: []const u8) !void {
        self.removeEdgesForNote(id);
        var links = try extractLinks(self.alloc, content);
        defer links.deinit(self.alloc);

        for (links.items) |link_id| {
            try self.addEdgeEntry(&self.edges_by_src, id, link_id);
            try self.addEdgeEntry(&self.edges_by_dst, link_id, id);
        }
    }

    /// Remove all edges originating from a note id.
    fn removeEdgesForNote(self: *Storage, id: []const u8) void {
        if (self.edges_by_src.get(id)) |list| {
            for (list.items) |dst| {
                self.removeEdgeEntry(&self.edges_by_dst, dst, id);
            }
        }
        self.removeEdgeList(&self.edges_by_src, id);
    }

    fn addEdgeEntry(
        self: *Storage,
        map: *std.StringHashMap(std.ArrayList([]const u8)),
        key: []const u8,
        value: []const u8,
    ) !void {
        const gop = try map.getOrPut(key);
        if (!gop.found_existing) {
            const owned_key = try self.alloc.dupe(u8, key);
            errdefer self.alloc.free(owned_key);
            gop.key_ptr.* = owned_key;
            gop.value_ptr.* = .{};
        }

        var list = gop.value_ptr;
        for (list.items) |existing| {
            if (mem.eql(u8, existing, value)) return;
        }

        const owned_value = try self.alloc.dupe(u8, value);
        errdefer self.alloc.free(owned_value);
        try list.append(self.alloc, owned_value);
    }

    fn removeEdgeEntry(
        self: *Storage,
        map: *std.StringHashMap(std.ArrayList([]const u8)),
        key: []const u8,
        value: []const u8,
    ) void {
        if (map.getPtr(key)) |list| {
            var i: usize = 0;
            while (i < list.items.len) {
                if (mem.eql(u8, list.items[i], value)) {
                    self.alloc.free(list.items[i]);
                    _ = list.swapRemove(i);
                } else {
                    i += 1;
                }
            }
            if (list.items.len == 0) {
                self.removeEdgeList(map, key);
            }
        }
    }

    fn removeEdgeList(
        self: *Storage,
        map: *std.StringHashMap(std.ArrayList([]const u8)),
        key: []const u8,
    ) void {
        if (map.fetchRemove(key)) |entry| {
            var list = entry.value;
            for (list.items) |id| {
                self.alloc.free(id);
            }
            list.deinit(self.alloc);
            self.alloc.free(entry.key);
        }
    }

    pub fn getBacklinks(self: *Storage, alloc: Allocator, id: []const u8) ![]Note {
        if (self.edges_by_dst.get(id)) |list| {
            var notes: std.ArrayList(Note) = .{};
            errdefer {
                for (notes.items) |note| freeNoteFields(alloc, note);
                notes.deinit(alloc);
            }

            for (list.items) |src_id| {
                if (try self.getNote(alloc, src_id)) |note| {
                    try notes.append(alloc, note);
                }
            }

            return notes.toOwnedSlice(alloc);
        }

        return alloc.alloc(Note, 0);
    }
};

const SnapshotCounts = struct {
    files: i64,
    notes: i64,
    embeddings: i64,
    edges: i64,
};

/// JSON format for snapshot notes
const SnapshotNote = struct {
    id: []const u8,
    file: []const u8,
    projectRoot: []const u8,
    line: i64,
    column: i64,
    nodePath: ?[]const u8 = null,
    tags: []const u8,
    text: []const u8,
    summary: []const u8,
    commitSha: ?[]const u8 = null,
    blobSha: ?[]const u8 = null,
    nodeTextHash: ?[]const u8 = null,
    createdAt: i64,
    updatedAt: i64,
};

const SnapshotFile = struct {
    file: []const u8,
    projectRoot: []const u8,
    content: []const u8,
    updatedAt: i64,
};

const SnapshotEmbedding = struct {
    file: []const u8,
    projectRoot: []const u8,
    vector: []const u8,
    text: []const u8,
    updatedAt: i64,
};

const SnapshotEdge = struct {
    id: i64,
    src: []const u8,
    dst: []const u8,
    kind: []const u8,
    projectRoot: []const u8,
    updatedAt: i64,
};

const SnapshotPayload = struct {
    version: i64,
    projectRoot: ?[]const u8 = null,
    createdAt: i64,
    counts: SnapshotCounts,
    notes: []SnapshotNote,
    files: []SnapshotFile = &.{},
    embeddings: []SnapshotEmbedding = &.{},
    edges: []SnapshotEdge = &.{},
};

/// Compare notes by created_at descending (newest first)
fn notesByCreatedAtDesc(_: void, a: Note, b: Note) bool {
    return mem.order(u8, b.created_at, a.created_at) == .lt;
}

pub fn summarizeText(text: []const u8) []const u8 {
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

/// Frontmatter boundary positions
const FrontmatterBounds = struct {
    /// Start of frontmatter content (after opening ---)
    start: usize,
    /// End of frontmatter content (before closing ---)
    end: usize,
    /// Start of body content (after closing --- and newline)
    body_start: usize,
};

/// Find YAML frontmatter boundaries, handling both LF and CRLF line endings
fn findFrontmatterBounds(content: []const u8) ?FrontmatterBounds {
    const has_crlf = mem.startsWith(u8, content, "---\r\n");
    const has_lf = mem.startsWith(u8, content, "---\n");
    if (!has_crlf and !has_lf) return null;

    const start_offset: usize = if (has_crlf) 5 else 4;
    const end_marker_lf = mem.indexOf(u8, content[start_offset..], "\n---\n");
    const end_marker_crlf = mem.indexOf(u8, content[start_offset..], "\r\n---\r\n");

    const end_offset: usize = blk: {
        if (end_marker_crlf) |crlf| {
            if (end_marker_lf) |lf| {
                break :blk if (crlf < lf) crlf else lf;
            }
            break :blk crlf;
        }
        if (end_marker_lf) |lf| break :blk lf;
        return null;
    };

    const end_marker_len: usize = if (end_marker_crlf != null and (end_marker_lf == null or end_marker_crlf.? <= end_marker_lf.?)) 7 else 5;
    const body_start = start_offset + end_offset + end_marker_len;

    return .{
        .start = start_offset,
        .end = start_offset + end_offset,
        .body_start = body_start,
    };
}

/// Case-insensitive substring search without allocation
/// Assumes needle is already lowercase
fn indexOfIgnoreCase(haystack: []const u8, needle: []const u8) ?usize {
    if (needle.len > haystack.len) return null;
    if (needle.len == 0) return 0;

    const end = haystack.len - needle.len + 1;
    outer: for (0..end) |i| {
        for (needle, 0..) |nc, j| {
            const hc = haystack[i + j];
            if (std.ascii.toLower(hc) != nc) continue :outer;
        }
        return i;
    }
    return null;
}

/// Validate note ID to prevent path traversal attacks
/// Returns error if ID contains path separators, "..", or control characters
fn validateNoteId(id: []const u8) StorageError!void {
    if (id.len == 0) return StorageError.InvalidNoteId;
    if (id.len > 256) return StorageError.InvalidNoteId; // Reasonable max length

    for (id) |c| {
        // Reject path separators
        if (c == '/' or c == '\\') return StorageError.InvalidNoteId;
        // Reject control characters
        if (c < 0x20) return StorageError.InvalidNoteId;
        // Reject null byte
        if (c == 0) return StorageError.InvalidNoteId;
    }

    // Reject ".." anywhere in the ID
    if (mem.indexOf(u8, id, "..") != null) return StorageError.InvalidNoteId;
}

fn extractLinks(alloc: Allocator, content: []const u8) !std.ArrayList([]const u8) {
    var links: std.ArrayList([]const u8) = .{};

    var i: usize = 0;
    while (i + 4 <= content.len) {
        if (content[i] == '[' and content[i + 1] == '[') {
            const desc_start = i + 2;
            const sep = findSeq(content, desc_start, "][") orelse {
                i += 2;
                continue;
            };
            const id_start = sep + 2;
            const end = findSeq(content, id_start, "]]") orelse {
                i = id_start;
                continue;
            };
            const id = content[id_start..end];
            if (isUuid(id) and !containsLink(links.items, id)) {
                try links.append(alloc, id);
            }
            i = end + 2;
            continue;
        }
        i += 1;
    }

    return links;
}

fn findSeq(haystack: []const u8, start: usize, needle: []const u8) ?usize {
    if (needle.len == 0 or start + needle.len > haystack.len) return null;
    var i = start;
    while (i + needle.len <= haystack.len) : (i += 1) {
        if (mem.eql(u8, haystack[i .. i + needle.len], needle)) return i;
    }
    return null;
}

fn containsLink(items: []const []const u8, id: []const u8) bool {
    for (items) |item| {
        if (mem.eql(u8, item, id)) return true;
    }
    return false;
}

fn isUuid(id: []const u8) bool {
    if (id.len != 36) return false;
    for (id, 0..) |c, idx| {
        switch (idx) {
            8, 13, 18, 23 => if (c != '-') return false,
            else => if (!std.ascii.isHex(c)) return false,
        }
    }
    return true;
}

/// Parse YAML frontmatter from file content
fn parseFrontmatter(alloc: Allocator, id: []const u8, content: []const u8) !Note {
    const bounds = findFrontmatterBounds(content) orelse return StorageError.InvalidFrontmatter;

    const frontmatter = content[bounds.start..bounds.end];
    const body = if (bounds.body_start < content.len) content[bounds.body_start..] else "";

    // Parse frontmatter fields
    var file_path: ?[]const u8 = null;
    var line_number: ?i64 = null;
    var column_number: ?i64 = null;
    var node_path: ?[]const u8 = null;
    var node_text_hash: ?[]const u8 = null;
    var created_at: ?[]const u8 = null;
    var updated_at: ?[]const u8 = null;

    var line_iter = mem.splitScalar(u8, frontmatter, '\n');
    while (line_iter.next()) |raw_line| {
        // Strip trailing CR for CRLF compatibility
        const line = mem.trimRight(u8, raw_line, "\r");
        if (mem.startsWith(u8, line, "file: ")) {
            file_path = line[6..];
        } else if (mem.startsWith(u8, line, "line: ")) {
            line_number = std.fmt.parseInt(i64, line[6..], 10) catch null;
        } else if (mem.startsWith(u8, line, "column: ")) {
            column_number = std.fmt.parseInt(i64, line[8..], 10) catch null;
        } else if (mem.startsWith(u8, line, "node_path: ")) {
            node_path = line[11..];
        } else if (mem.startsWith(u8, line, "node_text_hash: ")) {
            node_text_hash = line[16..];
        } else if (mem.startsWith(u8, line, "created: ")) {
            created_at = line[9..];
        } else if (mem.startsWith(u8, line, "updated: ")) {
            updated_at = line[9..];
        }
    }

    if (file_path == null or created_at == null or updated_at == null) {
        return StorageError.InvalidFrontmatter;
    }

    return Note{
        .id = try alloc.dupe(u8, id),
        .file_path = try alloc.dupe(u8, file_path.?),
        .node_path = if (node_path) |np| try alloc.dupe(u8, np) else null,
        .node_text_hash = if (node_text_hash) |h| try alloc.dupe(u8, h) else null,
        .line_number = line_number,
        .column_number = column_number,
        .content = try alloc.dupe(u8, body),
        .created_at = try alloc.dupe(u8, created_at.?),
        .updated_at = try alloc.dupe(u8, updated_at.?),
    };
}

/// Extract just the file_path from frontmatter (for index rebuilding)
fn extractFilePath(content: []const u8) ?[]const u8 {
    const bounds = findFrontmatterBounds(content) orelse return null;
    const frontmatter = content[bounds.start..bounds.end];

    var line_iter = mem.splitScalar(u8, frontmatter, '\n');
    while (line_iter.next()) |raw_line| {
        const line = mem.trimRight(u8, raw_line, "\r");
        if (mem.startsWith(u8, line, "file: ")) {
            return line[6..];
        }
    }
    return null;
}

/// Write content to file atomically (write to .tmp then rename)
fn writeFileAtomic(alloc: Allocator, path: []const u8, content: []const u8) StorageError!void {
    const tmp_path = std.fmt.allocPrint(alloc, "{s}.tmp", .{path}) catch return StorageError.OutOfMemory;
    defer alloc.free(tmp_path);

    // Write to temp file
    const file = fs.cwd().createFile(tmp_path, .{}) catch return StorageError.IoError;
    file.writeAll(content) catch {
        file.close();
        fs.cwd().deleteFile(tmp_path) catch {};
        return StorageError.IoError;
    };
    file.close();

    // Atomic rename
    fs.cwd().rename(tmp_path, path) catch {
        fs.cwd().deleteFile(tmp_path) catch {};
        return StorageError.IoError;
    };
}

/// Validate frontmatter fields don't contain newlines (YAML injection prevention)
fn validateFrontmatterFields(fields: []const ?[]const u8) StorageError!void {
    for (fields) |maybe_field| {
        if (maybe_field) |field| {
            if (mem.indexOf(u8, field, "\n") != null or mem.indexOf(u8, field, "\r") != null) {
                return StorageError.InvalidFieldValue;
            }
        }
    }
}

/// Generate YAML frontmatter for a note
fn writeFrontmatter(alloc: Allocator, note: Note) ![]const u8 {
    // Validate that frontmatter fields don't contain newlines (YAML injection)
    try validateFrontmatterFields(&.{
        note.file_path,
        note.node_path,
        note.node_text_hash,
        note.created_at,
        note.updated_at,
    });

    var buf: std.ArrayList(u8) = .{};
    errdefer buf.deinit(alloc);

    try buf.appendSlice(alloc, "---\n");
    try buf.appendSlice(alloc, "file: ");
    try buf.appendSlice(alloc, note.file_path);
    try buf.append(alloc, '\n');

    if (note.line_number) |ln| {
        var ln_buf: [32]u8 = undefined;
        const ln_str = std.fmt.bufPrint(&ln_buf, "line: {d}\n", .{ln}) catch unreachable;
        try buf.appendSlice(alloc, ln_str);
    }

    if (note.column_number) |cn| {
        var cn_buf: [32]u8 = undefined;
        const cn_str = std.fmt.bufPrint(&cn_buf, "column: {d}\n", .{cn}) catch unreachable;
        try buf.appendSlice(alloc, cn_str);
    }

    if (note.node_path) |np| {
        try buf.appendSlice(alloc, "node_path: ");
        try buf.appendSlice(alloc, np);
        try buf.append(alloc, '\n');
    }

    if (note.node_text_hash) |h| {
        try buf.appendSlice(alloc, "node_text_hash: ");
        try buf.appendSlice(alloc, h);
        try buf.append(alloc, '\n');
    }

    try buf.appendSlice(alloc, "created: ");
    try buf.appendSlice(alloc, note.created_at);
    try buf.append(alloc, '\n');

    try buf.appendSlice(alloc, "updated: ");
    try buf.appendSlice(alloc, note.updated_at);
    try buf.append(alloc, '\n');

    try buf.appendSlice(alloc, "---\n");
    try buf.appendSlice(alloc, note.content);

    return buf.toOwnedSlice(alloc);
}

/// Detect if ripgrep is available in PATH
fn detectRipgrep(alloc: Allocator) ?[]const u8 {
    // Try to run 'which rg'
    const result = std.process.Child.run(.{
        .allocator = alloc,
        .argv = &.{ "which", "rg" },
    }) catch return null;
    defer alloc.free(result.stderr);
    defer alloc.free(result.stdout);

    if (result.term.Exited == 0 and result.stdout.len > 0) {
        // Trim all whitespace (newlines, spaces, etc.)
        const path = mem.trim(u8, result.stdout, &std.ascii.whitespace);
        if (path.len > 0) {
            return alloc.dupe(u8, path) catch null;
        }
    }
    return null;
}

/// Get current timestamp as seconds since epoch
fn getTimestamp(buf: *[32]u8) []const u8 {
    const now = std.time.timestamp();
    const secs: u64 = std.math.cast(u64, now) orelse 0;
    return std.fmt.bufPrint(buf, "{d}", .{secs}) catch "0";
}

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

/// Free search hits
pub fn freeSearchHits(alloc: Allocator, hits: []const SearchHit) void {
    for (hits) |hit| {
        alloc.free(hit.file);
        alloc.free(hit.text);
    }
    alloc.free(hits);
}

// ============================================================================
// Tests
// ============================================================================

pub fn createTestStorage(alloc: Allocator) !Storage {
    // Use a unique temp directory for each test
    const timestamp = std.time.timestamp();
    var rand_buf: [4]u8 = undefined;
    std.crypto.random.bytes(&rand_buf);
    const rand_val = std.mem.readInt(u32, &rand_buf, .little);

    const tmp_dir = try std.fmt.allocPrint(alloc, "/tmp/mnemos-test-{d}-{d}", .{ timestamp, rand_val });
    errdefer alloc.free(tmp_dir);

    fs.cwd().makePath(tmp_dir) catch {};

    const notes_dir = try std.fs.path.join(alloc, &.{ tmp_dir, ".mnemos", "notes" });
    defer alloc.free(notes_dir);

    const store = try Storage.initWithNotesDir(alloc, tmp_dir, notes_dir);
    alloc.free(tmp_dir); // Storage owns its copy
    return store;
}

pub fn cleanupTestStorage(alloc: Allocator, store: *Storage) void {
    // Remove all files
    store.clearNotes() catch {};

    // Try to remove directories
    fs.cwd().deleteTree(store.project_root) catch {};

    store.deinit();
    _ = alloc;
}

test "storage init and deinit" {
    const alloc = std.testing.allocator;
    var store = try createTestStorage(alloc);
    defer cleanupTestStorage(alloc, &store);

    const count = try store.countNotes();
    try std.testing.expectEqual(@as(i64, 0), count);
}

test "note create and get" {
    const alloc = std.testing.allocator;
    var store = try createTestStorage(alloc);
    defer cleanupTestStorage(alloc, &store);

    const note = Note{
        .id = "test-123",
        .file_path = "/src/main.zig",
        .node_path = "fn.main",
        .node_text_hash = "abc123",
        .line_number = 42,
        .column_number = 8,
        .content = "Test note content",
        .created_at = "1704672000",
        .updated_at = "1704672000",
    };
    try store.createNote(note);

    const count = try store.countNotes();
    try std.testing.expectEqual(@as(i64, 1), count);

    const fetched = try store.getNote(alloc, "test-123");
    try std.testing.expect(fetched != null);
    defer freeNoteFields(alloc, fetched.?);

    try std.testing.expectEqualStrings("test-123", fetched.?.id);
    try std.testing.expectEqualStrings("/src/main.zig", fetched.?.file_path);
    try std.testing.expectEqualStrings("fn.main", fetched.?.node_path.?);
    try std.testing.expectEqualStrings("abc123", fetched.?.node_text_hash.?);
    try std.testing.expectEqual(@as(i64, 42), fetched.?.line_number.?);
    try std.testing.expectEqual(@as(i64, 8), fetched.?.column_number.?);
    try std.testing.expectEqualStrings("Test note content", fetched.?.content);
}

test "note update" {
    const alloc = std.testing.allocator;
    var store = try createTestStorage(alloc);
    defer cleanupTestStorage(alloc, &store);

    try store.createNote(.{
        .id = "update-test",
        .file_path = "/test.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "Original content",
        .created_at = "1704672000",
        .updated_at = "1704672000",
    });

    try store.updateNote("update-test", "Updated content", null, "1704672100");

    const updated = try store.getNote(alloc, "update-test");
    try std.testing.expect(updated != null);
    defer freeNoteFields(alloc, updated.?);

    try std.testing.expectEqualStrings("Updated content", updated.?.content);
    try std.testing.expectEqualStrings("1704672100", updated.?.updated_at);
}

test "note delete" {
    const alloc = std.testing.allocator;
    var store = try createTestStorage(alloc);
    defer cleanupTestStorage(alloc, &store);

    try store.createNote(.{
        .id = "delete-me",
        .file_path = "/test.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "To be deleted",
        .created_at = "1704672000",
        .updated_at = "1704672000",
    });

    try std.testing.expectEqual(@as(i64, 1), try store.countNotes());

    try store.deleteNote("delete-me");

    try std.testing.expectEqual(@as(i64, 0), try store.countNotes());

    const deleted = try store.getNote(alloc, "delete-me");
    try std.testing.expect(deleted == null);
}

test "get note not found" {
    const alloc = std.testing.allocator;
    var store = try createTestStorage(alloc);
    defer cleanupTestStorage(alloc, &store);

    const result = try store.getNote(alloc, "nonexistent");
    try std.testing.expect(result == null);
}

test "list project notes" {
    const alloc = std.testing.allocator;
    var store = try createTestStorage(alloc);
    defer cleanupTestStorage(alloc, &store);

    try store.createNote(.{
        .id = "note-a",
        .file_path = "/a.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "Note A",
        .created_at = "1704672000",
        .updated_at = "1704672000",
    });
    try store.createNote(.{
        .id = "note-b",
        .file_path = "/b.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "Note B",
        .created_at = "1704672100",
        .updated_at = "1704672100",
    });

    const notes = try store.listProjectNotes(alloc);
    defer freeNotes(alloc, notes);

    try std.testing.expectEqual(@as(usize, 2), notes.len);
}

test "get notes for file using index" {
    const alloc = std.testing.allocator;
    var store = try createTestStorage(alloc);
    defer cleanupTestStorage(alloc, &store);

    try store.createNote(.{
        .id = "note-1",
        .file_path = "/target.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "Note 1",
        .created_at = "1704672000",
        .updated_at = "1704672000",
    });
    try store.createNote(.{
        .id = "note-2",
        .file_path = "/target.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "Note 2",
        .created_at = "1704672100",
        .updated_at = "1704672100",
    });
    try store.createNote(.{
        .id = "note-3",
        .file_path = "/other.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "Note 3",
        .created_at = "1704672200",
        .updated_at = "1704672200",
    });

    const notes = try store.getNotesForFile(alloc, "/target.zig");
    defer freeNotes(alloc, notes);

    try std.testing.expectEqual(@as(usize, 2), notes.len);
}

test "clear notes" {
    const alloc = std.testing.allocator;
    var store = try createTestStorage(alloc);
    defer cleanupTestStorage(alloc, &store);

    try store.createNote(.{
        .id = "note-1",
        .file_path = "/a.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "Note 1",
        .created_at = "1704672000",
        .updated_at = "1704672000",
    });
    try store.createNote(.{
        .id = "note-2",
        .file_path = "/b.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "Note 2",
        .created_at = "1704672100",
        .updated_at = "1704672100",
    });

    try std.testing.expectEqual(@as(i64, 2), try store.countNotes());

    try store.clearNotes();

    try std.testing.expectEqual(@as(i64, 0), try store.countNotes());
}

test "search notes fallback" {
    const alloc = std.testing.allocator;
    var store = try createTestStorage(alloc);
    defer cleanupTestStorage(alloc, &store);

    // Disable ripgrep for this test
    if (store.rg_path) |p| {
        store.alloc.free(p);
        store.rg_path = null;
    }

    try store.createNote(.{
        .id = "haystack-1",
        .file_path = "/a.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "The needle is hidden here",
        .created_at = "1704672000",
        .updated_at = "1704672000",
    });
    try store.createNote(.{
        .id = "haystack-2",
        .file_path = "/b.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "Nothing special",
        .created_at = "1704672100",
        .updated_at = "1704672100",
    });

    const results = try store.searchNotes(alloc, "needle", 10, 0);
    defer freeNotes(alloc, results);

    try std.testing.expectEqual(@as(usize, 1), results.len);
    try std.testing.expectEqualStrings("haystack-1", results[0].id);
}

test "search notes with limit" {
    const alloc = std.testing.allocator;
    var store = try createTestStorage(alloc);
    defer cleanupTestStorage(alloc, &store);

    // Disable ripgrep for this test
    if (store.rg_path) |p| {
        store.alloc.free(p);
        store.rg_path = null;
    }

    for (0..5) |i| {
        var id_buf: [32]u8 = undefined;
        const id = std.fmt.bufPrint(&id_buf, "note-{d}", .{i}) catch unreachable;
        try store.createNote(.{
            .id = id,
            .file_path = "/test.zig",
            .node_path = null,
            .node_text_hash = null,
            .line_number = null,
            .column_number = null,
            .content = "searchable content",
            .created_at = "1704672000",
            .updated_at = "1704672000",
        });
    }

    const results = try store.searchNotes(alloc, "searchable", 3, 0);
    defer freeNotes(alloc, results);

    try std.testing.expectEqual(@as(usize, 3), results.len);
}

test "reattach note" {
    const alloc = std.testing.allocator;
    var store = try createTestStorage(alloc);
    defer cleanupTestStorage(alloc, &store);

    try store.createNote(.{
        .id = "reattach-test",
        .file_path = "/old.zig",
        .node_path = "fn.old",
        .node_text_hash = "oldhash",
        .line_number = 10,
        .column_number = null,
        .content = "Note content",
        .created_at = "1704672000",
        .updated_at = "1704672000",
    });

    try store.reattachNote("reattach-test", "/new.zig", 20, null, "fn.new", "newhash", "1704672100");

    const note = try store.getNote(alloc, "reattach-test");
    try std.testing.expect(note != null);
    defer freeNoteFields(alloc, note.?);

    try std.testing.expectEqualStrings("/new.zig", note.?.file_path);
    try std.testing.expectEqual(@as(i64, 20), note.?.line_number.?);
    try std.testing.expectEqualStrings("fn.new", note.?.node_path.?);
    try std.testing.expectEqualStrings("newhash", note.?.node_text_hash.?);
}

test "export and import json" {
    const alloc = std.testing.allocator;
    var store = try createTestStorage(alloc);
    defer cleanupTestStorage(alloc, &store);

    try store.createNote(.{
        .id = "export-1",
        .file_path = "/export.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "Export test content",
        .created_at = "1704672000",
        .updated_at = "1704672000",
    });

    const json = try store.exportNotesJson(alloc);
    defer alloc.free(json);

    try std.testing.expect(mem.indexOf(u8, json, "export-1") != null);
    try std.testing.expect(mem.indexOf(u8, json, "Export test content") != null);

    // Clear and reimport
    try store.clearNotes();
    try std.testing.expectEqual(@as(i64, 0), try store.countNotes());

    const count = try store.importNotesJson(alloc, json);
    try std.testing.expectEqual(@as(usize, 1), count);
    try std.testing.expectEqual(@as(i64, 1), try store.countNotes());
}

test "note with unicode content" {
    const alloc = std.testing.allocator;
    var store = try createTestStorage(alloc);
    defer cleanupTestStorage(alloc, &store);

    try store.createNote(.{
        .id = "unicode-note",
        .file_path = "/日本語.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "こんにちは世界 🎉",
        .created_at = "1704672000",
        .updated_at = "1704672000",
    });

    const note = try store.getNote(alloc, "unicode-note");
    try std.testing.expect(note != null);
    defer freeNoteFields(alloc, note.?);

    try std.testing.expectEqualStrings("こんにちは世界 🎉", note.?.content);
    try std.testing.expectEqualStrings("/日本語.zig", note.?.file_path);
}

test "note with special characters" {
    const alloc = std.testing.allocator;
    var store = try createTestStorage(alloc);
    defer cleanupTestStorage(alloc, &store);

    const special_content = "Line 1\nLine 2\tTab\n\"Quotes\"\n'Single'\n\\Backslash";
    try store.createNote(.{
        .id = "special-note",
        .file_path = "/test.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = special_content,
        .created_at = "1704672000",
        .updated_at = "1704672000",
    });

    const note = try store.getNote(alloc, "special-note");
    try std.testing.expect(note != null);
    defer freeNoteFields(alloc, note.?);

    try std.testing.expectEqualStrings(special_content, note.?.content);
}

test "note with empty content" {
    const alloc = std.testing.allocator;
    var store = try createTestStorage(alloc);
    defer cleanupTestStorage(alloc, &store);

    try store.createNote(.{
        .id = "empty-note",
        .file_path = "/test.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "",
        .created_at = "1704672000",
        .updated_at = "1704672000",
    });

    const note = try store.getNote(alloc, "empty-note");
    try std.testing.expect(note != null);
    defer freeNoteFields(alloc, note.?);

    try std.testing.expectEqualStrings("", note.?.content);
}

test "rebuild index" {
    const alloc = std.testing.allocator;
    var store = try createTestStorage(alloc);
    defer cleanupTestStorage(alloc, &store);

    try store.createNote(.{
        .id = "index-test",
        .file_path = "/indexed.zig",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "Index test",
        .created_at = "1704672000",
        .updated_at = "1704672000",
    });

    // Verify index works
    const notes = try store.getNotesForFile(alloc, "/indexed.zig");
    defer freeNotes(alloc, notes);
    try std.testing.expectEqual(@as(usize, 1), notes.len);

    // Clear and rebuild index
    store.clearIndex();

    // Index should be empty now
    const empty_notes = try store.getNotesForFile(alloc, "/indexed.zig");
    defer freeNotes(alloc, empty_notes);
    try std.testing.expectEqual(@as(usize, 0), empty_notes.len);

    // Rebuild
    try store.rebuildIndex();

    // Index should work again
    const rebuilt_notes = try store.getNotesForFile(alloc, "/indexed.zig");
    defer freeNotes(alloc, rebuilt_notes);
    try std.testing.expectEqual(@as(usize, 1), rebuilt_notes.len);
}

test "update nonexistent note does nothing" {
    const alloc = std.testing.allocator;
    var store = try createTestStorage(alloc);
    defer cleanupTestStorage(alloc, &store);

    // Should not error
    try store.updateNote("nonexistent", "New content", null, "1704672000");
    try std.testing.expectEqual(@as(i64, 0), try store.countNotes());
}

test "delete nonexistent note does nothing" {
    const alloc = std.testing.allocator;
    var store = try createTestStorage(alloc);
    defer cleanupTestStorage(alloc, &store);

    // Should not error
    try store.deleteNote("nonexistent");
    try std.testing.expectEqual(@as(i64, 0), try store.countNotes());
}

test "parseFrontmatter handles valid input" {
    const alloc = std.testing.allocator;
    const content =
        \\---
        \\file: /src/main.zig
        \\line: 42
        \\column: 8
        \\node_path: fn.main
        \\node_text_hash: abc123
        \\created: 1704672000
        \\updated: 1704672100
        \\---
        \\Note body here
    ;

    const note = try parseFrontmatter(alloc, "test-id", content);
    defer freeNoteFields(alloc, note);

    try std.testing.expectEqualStrings("test-id", note.id);
    try std.testing.expectEqualStrings("/src/main.zig", note.file_path);
    try std.testing.expectEqual(@as(i64, 42), note.line_number.?);
    try std.testing.expectEqual(@as(i64, 8), note.column_number.?);
    try std.testing.expectEqualStrings("fn.main", note.node_path.?);
    try std.testing.expectEqualStrings("abc123", note.node_text_hash.?);
    try std.testing.expectEqualStrings("1704672000", note.created_at);
    try std.testing.expectEqualStrings("1704672100", note.updated_at);
    try std.testing.expectEqualStrings("Note body here", note.content);
}

test "parseFrontmatter handles minimal input" {
    const alloc = std.testing.allocator;
    const content =
        \\---
        \\file: /test.zig
        \\created: 1704672000
        \\updated: 1704672000
        \\---
        \\
    ;

    const note = try parseFrontmatter(alloc, "minimal", content);
    defer freeNoteFields(alloc, note);

    try std.testing.expectEqualStrings("/test.zig", note.file_path);
    try std.testing.expect(note.line_number == null);
    try std.testing.expect(note.node_path == null);
}

test "parseFrontmatter rejects invalid input" {
    const alloc = std.testing.allocator;

    // No frontmatter
    const result1 = parseFrontmatter(alloc, "id", "Just content");
    try std.testing.expectError(StorageError.InvalidFrontmatter, result1);

    // Missing required fields
    const result2 = parseFrontmatter(alloc, "id",
        \\---
        \\file: /test.zig
        \\---
        \\content
    );
    try std.testing.expectError(StorageError.InvalidFrontmatter, result2);
}

test "writeFrontmatter produces valid output" {
    const alloc = std.testing.allocator;
    const note = Note{
        .id = "write-test",
        .file_path = "/src/main.zig",
        .node_path = "fn.main",
        .node_text_hash = "hash123",
        .line_number = 42,
        .column_number = 8,
        .content = "Note content",
        .created_at = "1704672000",
        .updated_at = "1704672100",
    };

    const output = try writeFrontmatter(alloc, note);
    defer alloc.free(output);

    // Parse it back
    const parsed = try parseFrontmatter(alloc, "write-test", output);
    defer freeNoteFields(alloc, parsed);

    try std.testing.expectEqualStrings(note.file_path, parsed.file_path);
    try std.testing.expectEqual(note.line_number, parsed.line_number);
    try std.testing.expectEqualStrings(note.content, parsed.content);
}

test "parseFrontmatter handles CRLF line endings" {
    const alloc = std.testing.allocator;
    const crlf_content = "---\r\nfile: /src/main.zig\r\nline: 42\r\ncreated: 1704672000\r\nupdated: 1704672100\r\n---\r\nNote body with CRLF";

    const note = try parseFrontmatter(alloc, "crlf-test", crlf_content);
    defer freeNoteFields(alloc, note);

    try std.testing.expectEqualStrings("/src/main.zig", note.file_path);
    try std.testing.expectEqual(@as(i64, 42), note.line_number.?);
    try std.testing.expectEqualStrings("Note body with CRLF", note.content);
}

test "extractFilePath handles CRLF line endings" {
    const crlf_content = "---\r\nfile: /test/path.zig\r\ncreated: 123\r\n---\r\ncontent";

    const file_path = extractFilePath(crlf_content);
    try std.testing.expect(file_path != null);
    try std.testing.expectEqualStrings("/test/path.zig", file_path.?);
}

test "indexOfIgnoreCase finds case-insensitive matches" {
    // Basic matches
    try std.testing.expectEqual(@as(?usize, 0), indexOfIgnoreCase("hello", "hello"));
    try std.testing.expectEqual(@as(?usize, 0), indexOfIgnoreCase("HELLO", "hello"));
    try std.testing.expectEqual(@as(?usize, 0), indexOfIgnoreCase("HeLLo", "hello"));

    // Substring
    try std.testing.expectEqual(@as(?usize, 6), indexOfIgnoreCase("world HELLO there", "hello"));
    try std.testing.expectEqual(@as(?usize, 4), indexOfIgnoreCase("the needle is here", "needle"));
    try std.testing.expectEqual(@as(?usize, 4), indexOfIgnoreCase("the NEEDLE is here", "needle"));

    // No match
    try std.testing.expectEqual(@as(?usize, null), indexOfIgnoreCase("hello", "world"));
    try std.testing.expectEqual(@as(?usize, null), indexOfIgnoreCase("short", "longer"));

    // Edge cases
    try std.testing.expectEqual(@as(?usize, 0), indexOfIgnoreCase("x", ""));
    try std.testing.expectEqual(@as(?usize, null), indexOfIgnoreCase("", "x"));
}

test "validateNoteId rejects path traversal" {
    // Path separators
    try std.testing.expectError(StorageError.InvalidNoteId, validateNoteId("../etc/passwd"));
    try std.testing.expectError(StorageError.InvalidNoteId, validateNoteId("foo/bar"));
    try std.testing.expectError(StorageError.InvalidNoteId, validateNoteId("foo\\bar"));

    // Double dots
    try std.testing.expectError(StorageError.InvalidNoteId, validateNoteId(".."));
    try std.testing.expectError(StorageError.InvalidNoteId, validateNoteId("foo..bar"));

    // Control characters
    try std.testing.expectError(StorageError.InvalidNoteId, validateNoteId("foo\x00bar"));
    try std.testing.expectError(StorageError.InvalidNoteId, validateNoteId("foo\nbar"));

    // Empty
    try std.testing.expectError(StorageError.InvalidNoteId, validateNoteId(""));

    // Valid IDs
    try validateNoteId("abc123");
    try validateNoteId("18f5a2b3-89abcdef");
    try validateNoteId("note-with-dashes");
    try validateNoteId("note_with_underscores");
}

test "writeFrontmatter rejects YAML injection" {
    const alloc = std.testing.allocator;

    // Newline in file_path
    const bad_path = writeFrontmatter(alloc, .{
        .id = "test",
        .file_path = "/test\nnode_path: malicious",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "content",
        .created_at = "123",
        .updated_at = "123",
    });
    try std.testing.expectError(StorageError.InvalidFieldValue, bad_path);

    // Carriage return
    const bad_cr = writeFrontmatter(alloc, .{
        .id = "test",
        .file_path = "/test\rmalicious",
        .node_path = null,
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "content",
        .created_at = "123",
        .updated_at = "123",
    });
    try std.testing.expectError(StorageError.InvalidFieldValue, bad_cr);

    // Newline in node_path
    const bad_node_path = writeFrontmatter(alloc, .{
        .id = "test",
        .file_path = "/test.zig",
        .node_path = "fn.main\ncreated: 999999",
        .node_text_hash = null,
        .line_number = null,
        .column_number = null,
        .content = "content",
        .created_at = "123",
        .updated_at = "123",
    });
    try std.testing.expectError(StorageError.InvalidFieldValue, bad_node_path);
}
