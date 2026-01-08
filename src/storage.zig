//! Markdown-based storage layer
//!
//! Stores notes as individual .md files with YAML frontmatter in .mnemos/notes/
//! Provides an in-memory index for fast lookups by file path.

const std = @import("std");
const mem = std.mem;
const fs = std.fs;
const Allocator = mem.Allocator;

pub const StorageError = error{
    NotFound,
    InvalidFrontmatter,
    InvalidJson,
    IoError,
    OutOfMemory,
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

/// Backward compatibility alias - use Storage instead
pub const Database = Storage;

/// Markdown-based storage for notes
pub const Storage = struct {
    alloc: Allocator,
    notes_dir: []const u8,
    project_root: []const u8,
    /// Maps file_path -> ArrayList of note IDs
    index: std.StringHashMap(std.ArrayList([]const u8)),
    rg_path: ?[]const u8,

    /// Backward compatibility: open is an alias for init
    /// Note: path is treated as project_root, not a database file
    pub fn open(alloc: Allocator, path: []const u8) !Storage {
        // For ":memory:" or similar SQLite patterns, use a temp directory
        const project_root = if (mem.eql(u8, path, ":memory:"))
            "/tmp/mnemos-test"
        else
            path;
        return init(alloc, project_root);
    }

    /// Backward compatibility: close is an alias for deinit
    pub fn close(self: *Storage) void {
        self.deinit();
    }

    /// Initialize storage, creating directories and rebuilding index
    pub fn init(alloc: Allocator, project_root: []const u8) !Storage {
        const owned_root = try alloc.dupe(u8, project_root);
        errdefer alloc.free(owned_root);

        const notes_dir = try std.fs.path.join(alloc, &.{ owned_root, ".mnemos", "notes" });
        errdefer alloc.free(notes_dir);

        // Create .mnemos/notes/ directory
        const mnemos_dir = try std.fs.path.join(alloc, &.{ owned_root, ".mnemos" });
        defer alloc.free(mnemos_dir);

        fs.cwd().makePath(mnemos_dir) catch |err| {
            if (err != error.PathAlreadyExists) return StorageError.IoError;
        };
        fs.cwd().makePath(notes_dir) catch |err| {
            if (err != error.PathAlreadyExists) return StorageError.IoError;
        };

        var result = Storage{
            .alloc = alloc,
            .notes_dir = notes_dir,
            .project_root = owned_root,
            .index = std.StringHashMap(std.ArrayList([]const u8)).init(alloc),
            .rg_path = detectRipgrep(alloc),
        };

        // Rebuild index from existing files
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

        if (self.rg_path) |p| self.alloc.free(p);
        self.alloc.free(self.notes_dir);
        self.alloc.free(self.project_root);
    }

    /// Create a new note
    pub fn createNote(self: *Storage, note: Note) !void {
        // Write the note file
        const filename = try std.fmt.allocPrint(self.alloc, "{s}.md", .{note.id});
        defer self.alloc.free(filename);

        const filepath = try std.fs.path.join(self.alloc, &.{ self.notes_dir, filename });
        defer self.alloc.free(filepath);

        const content = try writeFrontmatter(self.alloc, note);
        defer self.alloc.free(content);

        const file = fs.cwd().createFile(filepath, .{}) catch return StorageError.IoError;
        defer file.close();
        file.writeAll(content) catch return StorageError.IoError;

        // Add to index
        try self.addToIndex(note.file_path, note.id);
    }

    /// Get a note by ID
    pub fn getNote(self: *Storage, alloc: Allocator, id: []const u8) !?Note {
        const filename = try std.fmt.allocPrint(self.alloc, "{s}.md", .{id});
        defer self.alloc.free(filename);

        const filepath = try std.fs.path.join(self.alloc, &.{ self.notes_dir, filename });
        defer self.alloc.free(filepath);

        const file = fs.cwd().openFile(filepath, .{}) catch |err| {
            if (err == error.FileNotFound) return null;
            return StorageError.IoError;
        };
        defer file.close();

        const content = file.readToEndAlloc(alloc, 10 * 1024 * 1024) catch return StorageError.IoError;
        defer alloc.free(content);

        return try parseFrontmatter(alloc, id, content);
    }

    /// Update a note's content
    pub fn updateNote(self: *Storage, id: []const u8, content: ?[]const u8, tags: ?[]const u8, updated_at: []const u8) !void {
        _ = tags; // Tags not yet implemented

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

        const filename = try std.fmt.allocPrint(self.alloc, "{s}.md", .{id});
        defer self.alloc.free(filename);

        const filepath = try std.fs.path.join(self.alloc, &.{ self.notes_dir, filename });
        defer self.alloc.free(filepath);

        const file_content = try writeFrontmatter(self.alloc, new_note);
        defer self.alloc.free(file_content);

        const file = fs.cwd().createFile(filepath, .{}) catch return StorageError.IoError;
        defer file.close();
        file.writeAll(file_content) catch return StorageError.IoError;
    }

    /// Delete a note by ID
    pub fn deleteNote(self: *Storage, id: []const u8) !void {
        // First get the note to find its file_path for index removal
        const note = try self.getNote(self.alloc, id);
        if (note == null) return; // Already doesn't exist
        defer freeNoteFields(self.alloc, note.?);

        // Remove from index
        self.removeFromIndex(note.?.file_path, id);

        // Delete the file
        const filename = try std.fmt.allocPrint(self.alloc, "{s}.md", .{id});
        defer self.alloc.free(filename);

        const filepath = try std.fs.path.join(self.alloc, &.{ self.notes_dir, filename });
        defer self.alloc.free(filepath);

        fs.cwd().deleteFile(filepath) catch {};
    }

    /// Reattach note to new location
    pub fn reattachNote(self: *Storage, id: []const u8, file_path: ?[]const u8, line: i64, node_path: ?[]const u8, node_text_hash: ?[]const u8, updated_at: []const u8) !void {
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
            .column_number = existing.?.column_number,
            .content = existing.?.content,
            .created_at = existing.?.created_at,
            .updated_at = updated_at,
        };

        const filename = try std.fmt.allocPrint(self.alloc, "{s}.md", .{id});
        defer self.alloc.free(filename);

        const filepath = try std.fs.path.join(self.alloc, &.{ self.notes_dir, filename });
        defer self.alloc.free(filepath);

        const content = try writeFrontmatter(self.alloc, new_note);
        defer self.alloc.free(content);

        const file = fs.cwd().createFile(filepath, .{}) catch return StorageError.IoError;
        defer file.close();
        file.writeAll(content) catch return StorageError.IoError;
    }

    /// List all notes in the project
    pub fn listProjectNotes(self: *Storage, alloc: Allocator) ![]Note {
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
        while (iter.next() catch return StorageError.IoError) |entry| {
            if (entry.kind != .file) continue;
            if (!mem.endsWith(u8, entry.name, ".md")) continue;

            // Extract ID from filename (remove .md)
            const id = entry.name[0 .. entry.name.len - 3];

            if (try self.getNote(alloc, id)) |note| {
                try notes.append(alloc, note);
            }
        }

        // Sort by created_at descending
        const items = notes.items;
        std.mem.sort(Note, items, {}, struct {
            fn lessThan(_: void, a: Note, b: Note) bool {
                return mem.order(u8, b.created_at, a.created_at) == .lt;
            }
        }.lessThan);

        return notes.toOwnedSlice(alloc);
    }

    /// Get notes for a specific file using the index
    pub fn getNotesForFile(self: *Storage, alloc: Allocator, file_path: []const u8) ![]Note {
        var notes: std.ArrayList(Note) = .{};
        errdefer {
            for (notes.items) |note| freeNoteFields(alloc, note);
            notes.deinit(alloc);
        }

        if (self.index.get(file_path)) |id_list| {
            for (id_list.items) |id| {
                if (try self.getNote(alloc, id)) |note| {
                    try notes.append(alloc, note);
                }
            }
        }

        // Sort by created_at descending
        const items = notes.items;
        std.mem.sort(Note, items, {}, struct {
            fn lessThan(_: void, a: Note, b: Note) bool {
                return mem.order(u8, b.created_at, a.created_at) == .lt;
            }
        }.lessThan);

        return notes.toOwnedSlice(alloc);
    }

    /// Count total notes
    pub fn countNotes(self: *Storage) !i64 {
        var count: i64 = 0;

        var dir = fs.cwd().openDir(self.notes_dir, .{ .iterate = true }) catch |err| {
            if (err == error.FileNotFound) return 0;
            return StorageError.IoError;
        };
        defer dir.close();

        var iter = dir.iterate();
        while (iter.next() catch return StorageError.IoError) |entry| {
            if (entry.kind != .file) continue;
            if (!mem.endsWith(u8, entry.name, ".md")) continue;
            count += 1;
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
            .argv = &.{ rg, "--json", "-n", "-i", "--glob", "!.mnemos", "--glob", "!.git", "--glob", "!node_modules", query, self.project_root },
            .max_output_bytes = 10 * 1024 * 1024, // 10MB max
        }) catch {
            return self.searchContentFallback(alloc, query);
        };
        defer alloc.free(result.stdout);
        defer alloc.free(result.stderr);

        // Parse JSON lines output
        var line_iter = mem.splitScalar(u8, result.stdout, '\n');
        while (line_iter.next()) |line| {
            if (line.len == 0) continue;
            if (hits.items.len >= 100) break; // Limit results

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
                    col = @intCast(start.integer);
                }
            }

            const hit = SearchHit{
                .file = try alloc.dupe(u8, path_text.string),
                .line = @intCast(line_num.integer),
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
            if (hits.items.len >= 100) break;
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

            const content = file.readToEndAlloc(alloc, 1024 * 1024) catch |err| {
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

                // Case-insensitive search
                const line_lower = try std.ascii.allocLowerString(alloc, line_text);
                defer alloc.free(line_lower);

                if (mem.indexOf(u8, line_lower, query_lower)) |col| {
                    if (hits.items.len >= 100) return;

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
        // Clear index
        var it = self.index.iterator();
        while (it.next()) |entry| {
            for (entry.value_ptr.items) |id| {
                self.alloc.free(id);
            }
            entry.value_ptr.deinit(self.alloc);
            self.alloc.free(entry.key_ptr.*);
        }
        self.index.clearAndFree();

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
            dir.deleteFile(name) catch {};
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
            .argv = &.{ rg, "-l", "-i", query, self.notes_dir },
        }) catch {
            // Fallback if rg fails
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
                // Case-insensitive content match
                const content_lower = std.ascii.allocLowerString(alloc, note.content) catch {
                    freeNoteFields(alloc, note);
                    continue;
                };
                defer alloc.free(content_lower);

                if (mem.indexOf(u8, content_lower, query_lower) != null) {
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
            snapshot[i] = .{ .id = note.id, .filePath = note.file_path, .content = note.content };
        }

        return std.fmt.allocPrint(alloc, "{f}", .{std.json.fmt(snapshot, .{})});
    }

    /// Import notes from JSON
    pub fn importNotesJson(self: *Storage, alloc: Allocator, json: []const u8) !usize {
        const parsed = std.json.parseFromSlice([]SnapshotNote, alloc, json, .{}) catch {
            return StorageError.InvalidJson;
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
            self.createNote(note) catch continue;
            count += 1;
        }

        return count;
    }

    /// Rebuild the in-memory index from disk
    pub fn rebuildIndex(self: *Storage) !void {
        // Clear existing index
        var it = self.index.iterator();
        while (it.next()) |entry| {
            for (entry.value_ptr.items) |id| {
                self.alloc.free(id);
            }
            entry.value_ptr.deinit(self.alloc);
            self.alloc.free(entry.key_ptr.*);
        }
        self.index.clearAndFree();

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

            // Read just enough to parse frontmatter file_path
            const filepath = try std.fs.path.join(self.alloc, &.{ self.notes_dir, entry.name });
            defer self.alloc.free(filepath);

            const file = fs.cwd().openFile(filepath, .{}) catch continue;
            defer file.close();

            const content = file.readToEndAlloc(self.alloc, 10 * 1024 * 1024) catch continue;
            defer self.alloc.free(content);

            // Quick frontmatter parse for file_path only
            if (extractFilePath(content)) |file_path| {
                try self.addToIndex(file_path, id);
            }
        }
    }

    /// Add a note ID to the index for a file path
    fn addToIndex(self: *Storage, file_path: []const u8, id: []const u8) !void {
        const owned_id = try self.alloc.dupe(u8, id);
        errdefer self.alloc.free(owned_id);

        const result = self.index.getOrPut(file_path) catch return error.OutOfMemory;
        if (!result.found_existing) {
            // Need to own the key
            result.key_ptr.* = try self.alloc.dupe(u8, file_path);
            result.value_ptr.* = .{};
        }
        try result.value_ptr.append(self.alloc, owned_id);
    }

    /// Remove a note ID from the index
    fn removeFromIndex(self: *Storage, file_path: []const u8, id: []const u8) void {
        if (self.index.getPtr(file_path)) |list| {
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
};

/// JSON format for snapshot notes
const SnapshotNote = struct {
    id: []const u8,
    filePath: []const u8,
    content: []const u8,
};

/// Parse YAML frontmatter from file content
fn parseFrontmatter(alloc: Allocator, id: []const u8, content: []const u8) !Note {
    // Find frontmatter boundaries
    if (!mem.startsWith(u8, content, "---\n")) {
        return StorageError.InvalidFrontmatter;
    }

    const end_marker = mem.indexOf(u8, content[4..], "\n---\n") orelse
        return StorageError.InvalidFrontmatter;
    const frontmatter = content[4 .. 4 + end_marker];
    const body_start = 4 + end_marker + 5; // Skip past "\n---\n"
    const body = if (body_start < content.len) content[body_start..] else "";

    // Parse frontmatter fields
    var file_path: ?[]const u8 = null;
    var line_number: ?i64 = null;
    var column_number: ?i64 = null;
    var node_path: ?[]const u8 = null;
    var node_text_hash: ?[]const u8 = null;
    var created_at: ?[]const u8 = null;
    var updated_at: ?[]const u8 = null;

    var line_iter = mem.splitScalar(u8, frontmatter, '\n');
    while (line_iter.next()) |line| {
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
    if (!mem.startsWith(u8, content, "---\n")) return null;

    const end_marker = mem.indexOf(u8, content[4..], "\n---\n") orelse return null;
    const frontmatter = content[4 .. 4 + end_marker];

    var line_iter = mem.splitScalar(u8, frontmatter, '\n');
    while (line_iter.next()) |line| {
        if (mem.startsWith(u8, line, "file: ")) {
            return line[6..];
        }
    }
    return null;
}

/// Generate YAML frontmatter for a note
fn writeFrontmatter(alloc: Allocator, note: Note) ![]const u8 {
    var buf: std.ArrayList(u8) = .{};
    errdefer buf.deinit(alloc);

    try buf.appendSlice(alloc, "---\n");
    try buf.appendSlice(alloc, "file: ");
    try buf.appendSlice(alloc, note.file_path);
    try buf.append(alloc, '\n');

    if (note.line_number) |ln| {
        const ln_str = try std.fmt.allocPrint(alloc, "line: {d}\n", .{ln});
        defer alloc.free(ln_str);
        try buf.appendSlice(alloc, ln_str);
    }

    if (note.column_number) |cn| {
        const cn_str = try std.fmt.allocPrint(alloc, "column: {d}\n", .{cn});
        defer alloc.free(cn_str);
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

    if (result.term.Exited == 0 and result.stdout.len > 0) {
        // Trim trailing newline
        const path = mem.trimRight(u8, result.stdout, "\n");
        const owned = alloc.dupe(u8, path) catch {
            alloc.free(result.stdout);
            return null;
        };
        alloc.free(result.stdout);
        return owned;
    }
    alloc.free(result.stdout);
    return null;
}

/// Get current timestamp as seconds since epoch
fn getTimestamp(buf: *[32]u8) []const u8 {
    const now = std.time.timestamp();
    const secs: u64 = @intCast(now);
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

    const store = try Storage.init(alloc, tmp_dir);
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

    try store.reattachNote("reattach-test", "/new.zig", 20, "fn.new", "newhash", "1704672100");

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
    var it = store.index.iterator();
    while (it.next()) |entry| {
        for (entry.value_ptr.items) |id| {
            store.alloc.free(id);
        }
        entry.value_ptr.deinit(store.alloc);
        store.alloc.free(entry.key_ptr.*);
    }
    store.index.clearAndFree();

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
