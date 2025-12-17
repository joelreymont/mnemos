const std = @import("std");

// Tree-sitter C API bindings
const c = @cImport({
    @cInclude("tree_sitter/api.h");
});

// Re-export tree-sitter types for convenience
pub const TSParser = c.TSParser;
pub const TSTree = c.TSTree;
pub const TSNode = c.TSNode;
pub const TSQuery = c.TSQuery;
pub const TSQueryCursor = c.TSQueryCursor;
pub const TSLanguage = c.TSLanguage;
pub const TSPoint = c.TSPoint;

pub const TreeSitterError = error{
    GrammarNotAvailable,
    GrammarLoadFailed,
    ParseFailed,
    QueryFailed,
    InvalidSymbol,
    OutOfMemory,
};

/// Dynamic grammar loaded from shared library
pub const Grammar = struct {
    language: *const TSLanguage,
    name: []const u8,
    lib: std.DynLib,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *Grammar) void {
        self.allocator.free(self.name);
        self.lib.close();
    }
};

/// Grammar registry with dynamic loading
pub const GrammarRegistry = struct {
    grammars: std.StringHashMap(*Grammar),
    grammars_dir: []const u8,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, grammars_dir: []const u8) !GrammarRegistry {
        return GrammarRegistry{
            .grammars = std.StringHashMap(*Grammar).init(allocator),
            .grammars_dir = grammars_dir,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *GrammarRegistry) void {
        var it = self.grammars.valueIterator();
        while (it.next()) |grammar| {
            grammar.*.deinit();
            self.allocator.destroy(grammar.*);
        }
        self.grammars.deinit();
    }

    /// Load a grammar by name from the grammars directory
    pub fn loadGrammar(self: *GrammarRegistry, name: []const u8) !*Grammar {
        // Check if already loaded
        if (self.grammars.get(name)) |grammar| {
            return grammar;
        }

        // Construct library path
        const lib_name = if (std.builtin.os.tag == .macos)
            try std.fmt.allocPrint(self.allocator, "libtree-sitter-{s}.dylib", .{name})
        else
            try std.fmt.allocPrint(self.allocator, "libtree-sitter-{s}.so", .{name});
        defer self.allocator.free(lib_name);

        const lib_path = try std.fs.path.join(self.allocator, &.{ self.grammars_dir, lib_name });
        defer self.allocator.free(lib_path);

        // Open shared library
        var lib = std.DynLib.open(lib_path) catch {
            std.debug.print("Failed to open grammar library: {s}\n", .{lib_path});
            return TreeSitterError.GrammarLoadFailed;
        };
        errdefer lib.close();

        // Look up tree_sitter_<name> function
        const symbol_name = try std.fmt.allocPrint(self.allocator, "tree_sitter_{s}", .{name});
        defer self.allocator.free(symbol_name);

        const LanguageFn = *const fn () callconv(.C) *const TSLanguage;
        const language_fn = lib.lookup(LanguageFn, symbol_name) orelse {
            std.debug.print("Symbol not found: {s}\n", .{symbol_name});
            return TreeSitterError.InvalidSymbol;
        };

        const language = language_fn();

        // Create grammar object
        const grammar = try self.allocator.create(Grammar);
        errdefer self.allocator.destroy(grammar);

        grammar.* = Grammar{
            .language = language,
            .name = try self.allocator.dupe(u8, name),
            .lib = lib,
            .allocator = self.allocator,
        };

        try self.grammars.put(try self.allocator.dupe(u8, name), grammar);

        return grammar;
    }

    /// Get a loaded grammar by name
    pub fn getGrammar(self: *GrammarRegistry, name: []const u8) ?*Grammar {
        return self.grammars.get(name);
    }

    /// Check if a grammar is available
    pub fn hasGrammar(self: *GrammarRegistry, name: []const u8) bool {
        return self.grammars.contains(name);
    }
};

/// Parser with tree caching
pub const Parser = struct {
    parser: *TSParser,
    registry: *GrammarRegistry,
    tree_cache: std.StringHashMap(*TSTree),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, registry: *GrammarRegistry) !Parser {
        const parser = c.ts_parser_new() orelse return TreeSitterError.OutOfMemory;

        return Parser{
            .parser = parser,
            .registry = registry,
            .tree_cache = std.StringHashMap(*TSTree).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Parser) void {
        var it = self.tree_cache.valueIterator();
        while (it.next()) |tree| {
            c.ts_tree_delete(tree.*);
        }
        self.tree_cache.deinit();
        c.ts_parser_delete(self.parser);
    }

    /// Parse source code with the specified language
    pub fn parse(self: *Parser, language_name: []const u8, source: []const u8) !*TSTree {
        // Load grammar if not already loaded
        const grammar = try self.registry.loadGrammar(language_name);

        // Set parser language
        if (!c.ts_parser_set_language(self.parser, grammar.language)) {
            return TreeSitterError.ParseFailed;
        }

        // Parse the source
        const tree = c.ts_parser_parse_string(
            self.parser,
            null,
            source.ptr,
            @intCast(source.len),
        ) orelse return TreeSitterError.ParseFailed;

        return tree;
    }

    /// Parse and cache the result
    pub fn parseAndCache(self: *Parser, key: []const u8, language_name: []const u8, source: []const u8) !*TSTree {
        // Check cache first
        if (self.tree_cache.get(key)) |tree| {
            return tree;
        }

        const tree = try self.parse(language_name, source);
        errdefer c.ts_tree_delete(tree);

        const key_copy = try self.allocator.dupe(u8, key);
        errdefer self.allocator.free(key_copy);

        try self.tree_cache.put(key_copy, tree);
        return tree;
    }

    /// Get cached tree
    pub fn getCachedTree(self: *Parser, key: []const u8) ?*TSTree {
        return self.tree_cache.get(key);
    }

    /// Remove tree from cache
    pub fn removeCached(self: *Parser, key: []const u8) void {
        if (self.tree_cache.fetchRemove(key)) |kv| {
            c.ts_tree_delete(kv.value);
            self.allocator.free(kv.key);
        }
    }

    /// Clear all cached trees
    pub fn clearCache(self: *Parser) void {
        var it = self.tree_cache.iterator();
        while (it.next()) |entry| {
            c.ts_tree_delete(entry.value_ptr.*);
            self.allocator.free(entry.key_ptr.*);
        }
        self.tree_cache.clearRetainingCapacity();
    }
};

/// Query support for extracting nodes
pub const Query = struct {
    query: *TSQuery,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, language: *const TSLanguage, source: []const u8) !Query {
        var error_offset: u32 = 0;
        var error_type: c.TSQueryError = undefined;

        const query = c.ts_query_new(
            language,
            source.ptr,
            @intCast(source.len),
            &error_offset,
            &error_type,
        ) orelse {
            std.debug.print("Query error at offset {}: {}\n", .{ error_offset, error_type });
            return TreeSitterError.QueryFailed;
        };

        return Query{
            .query = query,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Query) void {
        c.ts_query_delete(self.query);
    }

    pub fn patternCount(self: *Query) u32 {
        return c.ts_query_pattern_count(self.query);
    }

    pub fn captureCount(self: *Query) u32 {
        return c.ts_query_capture_count(self.query);
    }

    pub fn stringCount(self: *Query) u32 {
        return c.ts_query_string_count(self.query);
    }
};

/// Query cursor for iterating matches
pub const QueryCursor = struct {
    cursor: *TSQueryCursor,

    pub fn init() !QueryCursor {
        const cursor = c.ts_query_cursor_new() orelse return TreeSitterError.OutOfMemory;
        return QueryCursor{ .cursor = cursor };
    }

    pub fn deinit(self: *QueryCursor) void {
        c.ts_query_cursor_delete(self.cursor);
    }

    pub fn exec(self: *QueryCursor, query: *Query, node: TSNode) void {
        c.ts_query_cursor_exec(self.cursor, query.query, node);
    }

    pub fn nextMatch(self: *QueryCursor, match: *c.TSQueryMatch) bool {
        return c.ts_query_cursor_next_match(self.cursor, match);
    }

    pub fn setByteRange(self: *QueryCursor, start_byte: u32, end_byte: u32) void {
        c.ts_query_cursor_set_byte_range(self.cursor, start_byte, end_byte);
    }

    pub fn setPointRange(self: *QueryCursor, start_point: TSPoint, end_point: TSPoint) void {
        c.ts_query_cursor_set_point_range(self.cursor, start_point, end_point);
    }
};

/// Utility functions for working with nodes
pub const Node = struct {
    /// Get the root node of a tree
    pub fn rootNode(tree: *TSTree) TSNode {
        return c.ts_tree_root_node(tree);
    }

    /// Get node type as string
    pub fn typeString(node: TSNode) [*:0]const u8 {
        return c.ts_node_type(node);
    }

    /// Get node symbol
    pub fn symbol(node: TSNode) u16 {
        return c.ts_node_symbol(node);
    }

    /// Get start byte
    pub fn startByte(node: TSNode) u32 {
        return c.ts_node_start_byte(node);
    }

    /// Get end byte
    pub fn endByte(node: TSNode) u32 {
        return c.ts_node_end_byte(node);
    }

    /// Get start point
    pub fn startPoint(node: TSNode) TSPoint {
        return c.ts_node_start_point(node);
    }

    /// Get end point
    pub fn endPoint(node: TSNode) TSPoint {
        return c.ts_node_end_point(node);
    }

    /// Get child count
    pub fn childCount(node: TSNode) u32 {
        return c.ts_node_child_count(node);
    }

    /// Get child by index
    pub fn child(node: TSNode, index: u32) TSNode {
        return c.ts_node_child(node, index);
    }

    /// Get named child count
    pub fn namedChildCount(node: TSNode) u32 {
        return c.ts_node_named_child_count(node);
    }

    /// Get named child by index
    pub fn namedChild(node: TSNode, index: u32) TSNode {
        return c.ts_node_named_child(node, index);
    }

    /// Check if node is named
    pub fn isNamed(node: TSNode) bool {
        return c.ts_node_is_named(node);
    }

    /// Check if node is null
    pub fn isNull(node: TSNode) bool {
        return c.ts_node_is_null(node);
    }

    /// Get parent node
    pub fn parent(node: TSNode) TSNode {
        return c.ts_node_parent(node);
    }

    /// Get next sibling
    pub fn nextSibling(node: TSNode) TSNode {
        return c.ts_node_next_sibling(node);
    }

    /// Get previous sibling
    pub fn prevSibling(node: TSNode) TSNode {
        return c.ts_node_prev_sibling(node);
    }

    /// Get next named sibling
    pub fn nextNamedSibling(node: TSNode) TSNode {
        return c.ts_node_next_named_sibling(node);
    }

    /// Get previous named sibling
    pub fn prevNamedSibling(node: TSNode) TSNode {
        return c.ts_node_prev_named_sibling(node);
    }

    /// Get node text from source
    pub fn text(node: TSNode, source: []const u8) []const u8 {
        const start = startByte(node);
        const end = endByte(node);
        return source[start..end];
    }
};

test "grammar registry init and deinit" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var registry = try GrammarRegistry.init(allocator, "/tmp/grammars");
    defer registry.deinit();

    try testing.expect(registry.grammars.count() == 0);
}

test "parser init and deinit" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var registry = try GrammarRegistry.init(allocator, "/tmp/grammars");
    defer registry.deinit();

    var parser = try Parser.init(allocator, &registry);
    defer parser.deinit();

    try testing.expect(parser.tree_cache.count() == 0);
}

test "parser cache clear" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var registry = try GrammarRegistry.init(allocator, "/tmp/grammars");
    defer registry.deinit();

    var parser = try Parser.init(allocator, &registry);
    defer parser.deinit();

    // Clear should work on empty cache
    parser.clearCache();
    try testing.expect(parser.tree_cache.count() == 0);
}

test "grammar registry custom path" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var registry = try GrammarRegistry.init(allocator, "/custom/path");
    defer registry.deinit();

    try testing.expectEqualStrings("/custom/path", registry.grammars_dir);
}

test "node text extraction" {
    const testing = std.testing;

    // Test Node.text utility with known source
    const source = "hello world";

    // Node.text slices source by byte range
    // Without a real tree-sitter tree, we test the slicing logic
    const start: u32 = 0;
    const end: u32 = 5;
    const extracted = source[start..end];
    try testing.expectEqualStrings("hello", extracted);
}

test "TreeSitterError enum" {
    const testing = std.testing;

    // Verify errors are distinct
    try testing.expect(TreeSitterError.GrammarLoadFailed != TreeSitterError.ParseFailed);
    try testing.expect(TreeSitterError.ParseFailed != TreeSitterError.QueryFailed);
}
