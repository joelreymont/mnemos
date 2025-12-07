//! Parser service for tree-sitter
//!
//! Handles parsing files and caching parse trees.

use lru::LruCache;
use sha2::{Digest, Sha256};
use std::collections::HashMap;
use std::num::NonZeroUsize;
use std::path::{Path, PathBuf};
use std::time::Instant;

use crate::config::LanguageSettings;
use crate::grammar::GrammarRegistry;
use crate::{Result, TreeSitterError};

/// Maximum number of parse trees to cache (LRU eviction when exceeded)
const MAX_CACHED_TREES: usize = 100;

/// Cached parse tree with content for incremental parsing
struct CachedTree {
    tree: tree_sitter::Tree,
    content_hash: String,
    /// Stored content for computing edits in incremental parsing
    content: String,
    #[allow(dead_code)]
    parsed_at: Instant,
}

/// Parser service manages parsing and tree caching
pub struct ParserService {
    /// Grammar registry
    registry: GrammarRegistry,

    /// LRU cache for parse trees by file path (bounded to prevent memory exhaustion)
    tree_cache: LruCache<PathBuf, CachedTree>,

    /// Parser instances by language (reusable)
    parsers: HashMap<String, tree_sitter::Parser>,
}

impl ParserService {
    /// Create a new parser service
    pub fn new(registry: GrammarRegistry) -> Self {
        Self {
            registry,
            tree_cache: LruCache::new(NonZeroUsize::new(MAX_CACHED_TREES).unwrap()),
            parsers: HashMap::new(),
        }
    }

    /// Parse file content, returning cached tree if content unchanged.
    /// Uses incremental parsing when previous tree exists for faster reparsing.
    pub fn parse(&mut self, file: &Path, content: &str) -> Result<&tree_sitter::Tree> {
        let content_hash = Self::hash_content(content);
        let file_buf = file.to_path_buf();

        // Check cache - LRU's get() promotes the entry
        if let Some(cached) = self.tree_cache.get(&file_buf) {
            if cached.content_hash == content_hash {
                return Ok(&self.tree_cache.get(&file_buf).unwrap().tree);
            }
        }

        // Get language for file
        let lang_name = self
            .registry
            .language_for_file(file)
            .ok_or_else(|| TreeSitterError::GrammarNotAvailable(file.display().to_string()))?;

        let language = self
            .registry
            .get(lang_name)
            .ok_or_else(|| TreeSitterError::GrammarNotAvailable(lang_name.to_string()))?;

        // Get or create parser for this language
        let parser = self
            .parsers
            .entry(lang_name.to_string())
            .or_default();

        parser
            .set_language(language.inner())
            .map_err(|e| TreeSitterError::ParseFailed(e.to_string()))?;

        // Check for incremental parsing opportunity
        let old_tree = self.tree_cache.pop(&file_buf).and_then(|mut cached| {
            // Compute edit from old content to new content
            if let Some(edit) = Self::compute_edit(&cached.content, content) {
                // Apply the edit to the tree
                cached.tree.edit(&edit);
                Some(cached.tree)
            } else {
                None // Edit too complex, fall back to full parse
            }
        });

        // Parse (incrementally if we have an edited old tree)
        let tree = parser
            .parse(content, old_tree.as_ref())
            .ok_or_else(|| TreeSitterError::ParseFailed("Parser returned None".to_string()))?;

        // Cache the result (LRU will evict oldest if at capacity)
        self.tree_cache.put(
            file_buf.clone(),
            CachedTree {
                tree,
                content_hash,
                content: content.to_string(),
                parsed_at: Instant::now(),
            },
        );

        Ok(&self.tree_cache.get(&file_buf).unwrap().tree)
    }

    /// Compute a tree-sitter InputEdit from old content to new content.
    /// Returns None if the edit is too complex (e.g., multiple disjoint changes).
    fn compute_edit(old_content: &str, new_content: &str) -> Option<tree_sitter::InputEdit> {
        let old_bytes = old_content.as_bytes();
        let new_bytes = new_content.as_bytes();

        // Find common prefix
        let prefix_len = old_bytes
            .iter()
            .zip(new_bytes.iter())
            .take_while(|(a, b)| a == b)
            .count();

        // Find common suffix (not overlapping with prefix)
        let old_suffix_start = old_bytes.len();
        let new_suffix_start = new_bytes.len();
        let max_suffix = old_suffix_start.saturating_sub(prefix_len).min(new_suffix_start.saturating_sub(prefix_len));

        let suffix_len = old_bytes[old_suffix_start.saturating_sub(max_suffix)..]
            .iter()
            .rev()
            .zip(new_bytes[new_suffix_start.saturating_sub(max_suffix)..].iter().rev())
            .take_while(|(a, b)| a == b)
            .count();

        // Compute edit boundaries
        let start_byte = prefix_len;
        let old_end_byte = old_bytes.len().saturating_sub(suffix_len);
        let new_end_byte = new_bytes.len().saturating_sub(suffix_len);

        // Compute positions
        let start_position = Self::byte_to_point(old_content, start_byte);
        let old_end_position = Self::byte_to_point(old_content, old_end_byte);
        let new_end_position = Self::byte_to_point(new_content, new_end_byte);

        Some(tree_sitter::InputEdit {
            start_byte,
            old_end_byte,
            new_end_byte,
            start_position,
            old_end_position,
            new_end_position,
        })
    }

    /// Convert byte offset to tree-sitter Point (row, column)
    fn byte_to_point(content: &str, byte_offset: usize) -> tree_sitter::Point {
        let text = &content[..byte_offset.min(content.len())];
        let row = text.chars().filter(|&c| c == '\n').count();
        let last_newline = text.rfind('\n').map(|i| i + 1).unwrap_or(0);
        let column = text[last_newline..].len();
        tree_sitter::Point::new(row, column)
    }

    /// Find the significant node at a position
    /// Walks up from the node at position to find a semantically meaningful node
    pub fn node_at_position<'a>(
        tree: &'a tree_sitter::Tree,
        content: &str,
        line: u32,
        column: u32,
        settings: Option<&LanguageSettings>,
    ) -> Option<tree_sitter::Node<'a>> {
        // Find first non-whitespace column if at column 0
        let col = if column == 0 {
            Self::first_nonwhitespace_column(content, line).unwrap_or(0)
        } else {
            column
        };

        // Get node at position (0-indexed)
        let point = tree_sitter::Point::new(line as usize, col as usize);
        let node = tree
            .root_node()
            .named_descendant_for_point_range(point, point)?;

        // Walk up to find significant node
        Self::find_significant_node(node, settings)
    }

    /// Find first non-whitespace column on a line
    fn first_nonwhitespace_column(content: &str, line: u32) -> Option<u32> {
        content.lines().nth(line as usize).and_then(|line_text| {
            line_text
                .chars()
                .position(|c| !c.is_whitespace())
                .map(|pos| pos as u32)
        })
    }

    /// Walk up the tree to find a significant node
    fn find_significant_node<'a>(
        mut node: tree_sitter::Node<'a>,
        settings: Option<&LanguageSettings>,
    ) -> Option<tree_sitter::Node<'a>> {
        let original_line = node.start_position().row;

        loop {
            let node_type = node.kind();

            // Stop if we've gone to a different line
            if node.start_position().row != original_line {
                return Some(node);
            }

            // Check if this is a container node (too large)
            let is_container = settings
                .map(|s| s.container_nodes.iter().any(|n| n == node_type))
                .unwrap_or(false);

            if is_container {
                // Return the previous node (child that's significant)
                return Some(node);
            }

            // Check if this is a skip node (too small)
            let is_skip = settings
                .map(|s| s.skip_nodes.iter().any(|n| n == node_type))
                .unwrap_or(false);

            if !is_skip {
                // This is a significant node
                return Some(node);
            }

            // Move up to parent
            match node.parent() {
                Some(parent) => node = parent,
                None => return Some(node), // Reached root
            }
        }
    }

    /// Compute SHA256 hash of node's first line
    pub fn node_hash(node: &tree_sitter::Node, content: &str) -> String {
        let text = Self::node_text(node, content);
        Self::hash_content(&text)
    }

    /// Get node text (first line only for multi-line nodes)
    pub fn node_text(node: &tree_sitter::Node, content: &str) -> String {
        let start = node.start_byte();
        let end = node.end_byte();

        if start >= content.len() || end > content.len() {
            return String::new();
        }

        let text = &content[start..end];

        // Use only first line for multi-line nodes
        text.lines().next().unwrap_or("").to_string()
    }

    /// Search for a node with matching hash within a line range
    /// Returns the line number where the hash was found
    pub fn find_node_by_hash(
        tree: &tree_sitter::Tree,
        content: &str,
        target_hash: &str,
        min_line: u32,
        max_line: u32,
        settings: Option<&LanguageSettings>,
    ) -> Option<u32> {
        Self::search_tree_for_hash(
            tree.root_node(),
            content,
            target_hash,
            min_line,
            max_line,
            settings,
        )
    }

    /// Recursively search tree for matching hash
    fn search_tree_for_hash(
        node: tree_sitter::Node,
        content: &str,
        target_hash: &str,
        min_line: u32,
        max_line: u32,
        settings: Option<&LanguageSettings>,
    ) -> Option<u32> {
        let start_line = node.start_position().row as u32;
        let end_line = node.end_position().row as u32;

        // Skip subtrees entirely outside the search range
        if end_line < min_line || start_line > max_line {
            return None;
        }

        // Check this node if it's within range and significant
        if start_line >= min_line && start_line <= max_line && node.is_named() {
            let node_type = node.kind();

            // Check if significant (not skip, not container)
            let is_skip = settings
                .map(|s| s.skip_nodes.iter().any(|n| n == node_type))
                .unwrap_or(false);
            let is_container = settings
                .map(|s| s.container_nodes.iter().any(|n| n == node_type))
                .unwrap_or(false);

            if !is_skip && !is_container {
                let hash = Self::node_hash(&node, content);
                if hash == target_hash {
                    return Some(start_line);
                }
            }
        }

        // Recurse into children
        let mut cursor = node.walk();
        for child in node.named_children(&mut cursor) {
            if let Some(line) = Self::search_tree_for_hash(
                child,
                content,
                target_hash,
                min_line,
                max_line,
                settings,
            ) {
                return Some(line);
            }
        }

        None
    }

    /// Find the closest significant node to a target line within a range
    /// Used as fallback when exact hash not found (note is stale)
    pub fn closest_significant_node(
        tree: &tree_sitter::Tree,
        _content: &str,
        target_line: u32,
        min_line: u32,
        max_line: u32,
        settings: Option<&LanguageSettings>,
    ) -> Option<u32> {
        let mut closest: Option<(u32, u32)> = None; // (line, distance)

        Self::find_closest_node(
            tree.root_node(),
            target_line,
            min_line,
            max_line,
            settings,
            &mut closest,
        );

        closest.map(|(line, _)| line)
    }

    /// Recursively find closest significant node
    fn find_closest_node(
        node: tree_sitter::Node,
        target_line: u32,
        min_line: u32,
        max_line: u32,
        settings: Option<&LanguageSettings>,
        closest: &mut Option<(u32, u32)>,
    ) {
        let start_line = node.start_position().row as u32;
        let end_line = node.end_position().row as u32;

        // Skip subtrees entirely outside the search range
        if end_line < min_line || start_line > max_line {
            return;
        }

        // Check this node
        if start_line >= min_line && start_line <= max_line && node.is_named() {
            let node_type = node.kind();

            let is_skip = settings
                .map(|s| s.skip_nodes.iter().any(|n| n == node_type))
                .unwrap_or(false);
            let is_container = settings
                .map(|s| s.container_nodes.iter().any(|n| n == node_type))
                .unwrap_or(false);

            if !is_skip && !is_container {
                let mut distance = start_line.abs_diff(target_line);

                // Bias towards nodes after target (code usually moves down)
                if start_line > target_line {
                    distance = distance.saturating_sub(1);
                }

                match closest {
                    Some((_, best_dist)) if distance < *best_dist => {
                        *closest = Some((start_line, distance));
                    }
                    None => {
                        *closest = Some((start_line, distance));
                    }
                    _ => {}
                }
            }
        }

        // Recurse into children
        let mut cursor = node.walk();
        for child in node.named_children(&mut cursor) {
            Self::find_closest_node(
                child,
                target_line,
                min_line,
                max_line,
                settings,
                closest,
            );
        }
    }

    /// Get node path (hierarchy) at position
    pub fn node_path(
        tree: &tree_sitter::Tree,
        content: &str,
        line: u32,
        column: u32,
        settings: Option<&LanguageSettings>,
    ) -> Vec<String> {
        let Some(node) = Self::node_at_position(tree, content, line, column, settings) else {
            return Vec::new();
        };

        let mut path = Vec::new();
        let mut current = Some(node);

        while let Some(n) = current {
            if n.is_named() {
                path.push(n.kind().to_string());
            }
            current = n.parent();
        }

        path.reverse();
        path
    }

    /// Hash content using SHA256
    fn hash_content(content: &str) -> String {
        let mut hasher = Sha256::new();
        hasher.update(content.as_bytes());
        format!("{:x}", hasher.finalize())
    }

    /// Get grammar registry
    pub fn registry(&self) -> &GrammarRegistry {
        &self.registry
    }

    /// Check if a language is available
    pub fn has_language(&self, name: &str) -> bool {
        self.registry.has_language(name)
    }

    /// Get language name for a file
    pub fn language_for_file(&self, path: &Path) -> Option<&str> {
        self.registry.language_for_file(path)
    }

    /// Clear the tree cache (useful for testing or memory management)
    pub fn clear_cache(&mut self) {
        self.tree_cache.clear();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::default_config;

    fn create_parser_service() -> ParserService {
        let registry = GrammarRegistry::new(default_config());
        ParserService::new(registry)
    }

    #[test]
    fn test_parse_rust_file() {
        let mut service = create_parser_service();
        let content = r#"
fn main() {
    println!("Hello");
}

fn helper() {
    // comment
}
"#;
        let path = Path::new("test.rs");
        let tree = service.parse(path, content).unwrap();

        assert_eq!(tree.root_node().kind(), "source_file");
    }

    #[test]
    fn test_node_at_position() {
        let mut service = create_parser_service();
        let content = r#"fn main() {
    println!("Hello");
}

fn helper() {
    let x = 1;
}
"#;
        let path = Path::new("test.rs");
        let settings = service.registry().config().get_language("rust").cloned();
        let tree = service.parse(path, content).unwrap();

        // Line 0, column 0 should find function_item
        let node = ParserService::node_at_position(tree, content, 0, 0, settings.as_ref()).unwrap();
        assert_eq!(node.kind(), "function_item");

        // Line 4, should find the helper function
        let node = ParserService::node_at_position(tree, content, 4, 0, settings.as_ref()).unwrap();
        assert_eq!(node.kind(), "function_item");
    }

    #[test]
    fn test_node_hash() {
        let mut service = create_parser_service();
        let content = "fn main() {\n    println!(\"Hello\");\n}";
        let path = Path::new("test.rs");
        let tree = service.parse(path, content).unwrap();

        let node = tree.root_node().child(0).unwrap();
        let hash = ParserService::node_hash(&node, content);

        // Hash should be consistent
        let hash2 = ParserService::node_hash(&node, content);
        assert_eq!(hash, hash2);

        // Hash should be for first line only
        let text = ParserService::node_text(&node, content);
        assert_eq!(text, "fn main() {");
    }

    #[test]
    fn test_find_node_by_hash() {
        let mut service = create_parser_service();
        let content = r#"fn main() {
    println!("Hello");
}

fn helper() {
    let x = 1;
}
"#;
        let path = Path::new("test.rs");
        let settings = service.registry().config().get_language("rust").cloned();
        let tree = service.parse(path, content).unwrap();

        // Get hash of helper function
        let helper_node = ParserService::node_at_position(tree, content, 4, 0, settings.as_ref()).unwrap();
        let hash = ParserService::node_hash(&helper_node, content);

        // Should find it at line 4
        let found = ParserService::find_node_by_hash(tree, content, &hash, 0, 10, settings.as_ref()).unwrap();
        assert_eq!(found, 4);
    }

    #[test]
    fn test_node_path() {
        let mut service = create_parser_service();
        let content = r#"fn main() {
    let x = 1;
}
"#;
        let path = Path::new("test.rs");
        let settings = service.registry().config().get_language("rust").cloned();
        let tree = service.parse(path, content).unwrap();

        let node_path = ParserService::node_path(tree, content, 1, 4, settings.as_ref());
        assert!(node_path.contains(&"function_item".to_string()));
    }

    #[test]
    fn test_cache_reuse() {
        let mut service = create_parser_service();
        let content = "fn main() {}";
        let path = Path::new("test.rs");

        // First parse
        let _ = service.parse(path, content).unwrap();

        // Second parse with same content should use cache
        let _ = service.parse(path, content).unwrap();

        // Cache should have one entry
        assert_eq!(service.tree_cache.len(), 1);
    }

    #[test]
    fn test_cache_invalidation() {
        let mut service = create_parser_service();
        let path = Path::new("test.rs");

        // First parse
        let _ = service.parse(path, "fn main() {}").unwrap();

        // Parse with different content
        let _ = service.parse(path, "fn helper() {}").unwrap();

        // Cache should still have one entry (replaced)
        assert_eq!(service.tree_cache.len(), 1);
    }

    #[test]
    fn test_incremental_parse() {
        let mut service = create_parser_service();
        let path = Path::new("test.rs");

        // Initial parse
        let content1 = "fn main() {\n    let x = 1;\n}";
        let tree1 = service.parse(path, content1).unwrap();
        assert_eq!(tree1.root_node().kind(), "source_file");

        // Small edit - add a line
        let content2 = "fn main() {\n    let x = 1;\n    let y = 2;\n}";
        let tree2 = service.parse(path, content2).unwrap();
        assert_eq!(tree2.root_node().kind(), "source_file");

        // Tree should be valid after incremental parse
        let func = tree2.root_node().child(0).unwrap();
        assert_eq!(func.kind(), "function_item");
    }

    #[test]
    fn test_compute_edit() {
        // Test simple insertion
        let old = "fn main() {}";
        let new = "fn main() { let x = 1; }";
        let edit = ParserService::compute_edit(old, new).unwrap();
        assert_eq!(edit.start_byte, 11); // Position of '}'
        assert_eq!(edit.old_end_byte, 11);
        assert_eq!(edit.new_end_byte, 23);

        // Test simple deletion
        let old = "fn main() { let x = 1; }";
        let new = "fn main() {}";
        let edit = ParserService::compute_edit(old, new).unwrap();
        assert_eq!(edit.start_byte, 11);
        assert_eq!(edit.old_end_byte, 23);
        assert_eq!(edit.new_end_byte, 11);
    }

    #[test]
    fn test_byte_to_point() {
        let content = "line1\nline2\nline3";

        // Start of line 1
        let point = ParserService::byte_to_point(content, 0);
        assert_eq!(point.row, 0);
        assert_eq!(point.column, 0);

        // End of line 1 (before newline)
        let point = ParserService::byte_to_point(content, 5);
        assert_eq!(point.row, 0);
        assert_eq!(point.column, 5);

        // Start of line 2
        let point = ParserService::byte_to_point(content, 6);
        assert_eq!(point.row, 1);
        assert_eq!(point.column, 0);

        // Middle of line 2
        let point = ParserService::byte_to_point(content, 8);
        assert_eq!(point.row, 1);
        assert_eq!(point.column, 2);
    }
}
