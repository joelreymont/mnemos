//! Position tracking for notes
//!
//! Computes display positions and staleness for notes based on tree-sitter analysis.

use std::path::Path;

use crate::parser::ParserService;

/// Result of computing display position for a note
#[derive(Debug, Clone)]
pub struct DisplayPosition {
    /// Line where the note should be displayed (0-indexed)
    pub line: u32,

    /// Whether the note is stale (code was modified)
    pub stale: bool,

    /// Reason for the computed position (for debugging)
    pub reason: PositionReason,
}

/// Reason why a position was computed
#[derive(Debug, Clone, PartialEq)]
pub enum PositionReason {
    /// Hash matched at stored position
    ExactMatch,

    /// Hash found at a different line (code moved)
    FoundElsewhere { original_line: u32 },

    /// Hash not found, using closest significant node
    ClosestNode { original_line: u32 },

    /// Fallback to stored position (no tree-sitter or no nodes found)
    Fallback,

    /// Grammar not available for this language
    NoGrammar,
}

/// Compute display position for a note
///
/// Algorithm:
/// 1. Check stored position first (fast path)
/// 2. If no match, search nearby for matching hash
/// 3. If not found, note is stale - find closest significant node
/// 4. Fallback to stored position
pub fn compute_display_position(
    service: &mut ParserService,
    file: &Path,
    content: &str,
    stored_line: u32,
    stored_column: u32,
    node_text_hash: Option<&str>,
    search_radius: u32,
) -> DisplayPosition {
    // Get language for file
    let lang_name = match service.language_for_file(file) {
        Some(name) => name.to_string(),
        None => {
            return DisplayPosition {
                line: stored_line,
                stale: false,
                reason: PositionReason::NoGrammar,
            };
        }
    };

    // Get language settings before parsing (to avoid borrow issues)
    let settings = service.registry().config().get_language(&lang_name).cloned();

    // Parse file
    let tree = match service.parse(file, content) {
        Ok(tree) => tree,
        Err(_) => {
            return DisplayPosition {
                line: stored_line,
                stale: false,
                reason: PositionReason::Fallback,
            };
        }
    };

    // If no hash, use stored position
    let Some(hash) = node_text_hash else {
        return DisplayPosition {
            line: stored_line,
            stale: false,
            reason: PositionReason::Fallback,
        };
    };

    // Check stored position first
    if let Some(node) =
        ParserService::node_at_position(tree, content, stored_line, stored_column, settings.as_ref())
    {
        let current_hash = ParserService::node_hash(&node, content);
        if current_hash == hash {
            return DisplayPosition {
                line: stored_line,
                stale: false,
                reason: PositionReason::ExactMatch,
            };
        }
    }

    // Search nearby for matching hash
    let min_line = stored_line.saturating_sub(search_radius);
    let max_line = stored_line + search_radius;

    if let Some(found_line) =
        ParserService::find_node_by_hash(tree, content, hash, min_line, max_line, settings.as_ref())
    {
        return DisplayPosition {
            line: found_line,
            stale: false,
            reason: PositionReason::FoundElsewhere {
                original_line: stored_line,
            },
        };
    }

    // Hash not found - code was modified
    // Find closest significant node
    if let Some(closest_line) =
        ParserService::closest_significant_node(tree, content, stored_line, min_line, max_line, settings.as_ref())
    {
        return DisplayPosition {
            line: closest_line,
            stale: true,
            reason: PositionReason::ClosestNode {
                original_line: stored_line,
            },
        };
    }

    // Fallback to stored position
    DisplayPosition {
        line: stored_line,
        stale: true,
        reason: PositionReason::Fallback,
    }
}

/// Compute hash for a position in content
pub fn compute_hash_at_position(
    service: &mut ParserService,
    file: &Path,
    content: &str,
    line: u32,
    column: u32,
) -> Option<String> {
    let lang_name = service.language_for_file(file)?;
    let settings = service.registry().config().get_language(lang_name).cloned();
    let tree = service.parse(file, content).ok()?;
    let node = ParserService::node_at_position(tree, content, line, column, settings.as_ref())?;
    Some(ParserService::node_hash(&node, content))
}

/// Result of computing an anchor position for a note
#[derive(Debug, Clone)]
pub struct AnchorPosition {
    /// Adjusted line (0-indexed, start of significant node)
    pub line: u32,
    /// Adjusted column (0-indexed, start of significant node)
    pub column: u32,
    /// Node path from root to significant node
    pub node_path: Vec<String>,
    /// Hash of the significant node's first line
    pub node_text_hash: Option<String>,
}

/// Compute anchor position from raw cursor coordinates
///
/// This replicates the UI-side anchor logic:
/// 1. If column is 0 or in whitespace, find first non-whitespace column
/// 2. Find the significant node at that position
/// 3. Return the node's start position, path, and hash
///
/// UIs can call this with raw cursor position and get back the proper anchor.
pub fn compute_anchor_position(
    service: &mut ParserService,
    file: &Path,
    content: &str,
    cursor_line: u32,
    cursor_column: u32,
) -> AnchorPosition {
    let lang_name = match service.language_for_file(file) {
        Some(name) => name.to_string(),
        None => {
            // No grammar - return cursor position as-is
            return AnchorPosition {
                line: cursor_line,
                column: cursor_column,
                node_path: Vec::new(),
                node_text_hash: None,
            };
        }
    };

    let settings = service.registry().config().get_language(&lang_name).cloned();

    let tree = match service.parse(file, content) {
        Ok(tree) => tree,
        Err(_) => {
            return AnchorPosition {
                line: cursor_line,
                column: cursor_column,
                node_path: Vec::new(),
                node_text_hash: None,
            };
        }
    };

    // Find significant node at position (handles whitespace skipping internally)
    let Some(node) = ParserService::node_at_position(tree, content, cursor_line, cursor_column, settings.as_ref()) else {
        return AnchorPosition {
            line: cursor_line,
            column: cursor_column,
            node_path: Vec::new(),
            node_text_hash: None,
        };
    };

    // Get the node's start position as the anchor
    let start = node.start_position();
    let anchor_line = start.row as u32;
    let anchor_column = start.column as u32;

    // Only use node start if it's on the same line as cursor
    // This prevents anchoring to a parent node that starts many lines above
    let (final_line, final_column) = if anchor_line == cursor_line {
        (anchor_line, anchor_column)
    } else {
        (cursor_line, cursor_column)
    };

    // Compute node path
    let node_path = ParserService::node_path(tree, content, final_line, final_column, settings.as_ref());

    // Compute hash
    let node_text_hash = ParserService::node_at_position(tree, content, final_line, final_column, settings.as_ref()).map(|node| ParserService::node_hash(&node, content));

    AnchorPosition {
        line: final_line,
        column: final_column,
        node_path,
        node_text_hash,
    }
}

/// Compute node path at a position
pub fn compute_node_path(
    service: &mut ParserService,
    file: &Path,
    content: &str,
    line: u32,
    column: u32,
) -> Vec<String> {
    let Some(lang_name) = service.language_for_file(file) else {
        return Vec::new();
    };

    let settings = service.registry().config().get_language(lang_name).cloned();

    let Ok(tree) = service.parse(file, content) else {
        return Vec::new();
    };

    ParserService::node_path(tree, content, line, column, settings.as_ref())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::default_config;
    use crate::grammar::GrammarRegistry;

    fn create_service() -> ParserService {
        let registry = GrammarRegistry::new(default_config());
        ParserService::new(registry)
    }

    #[test]
    fn test_exact_match_at_stored_position() {
        let mut service = create_service();
        let content = r#"fn main() {
    println!("Hello");
}

fn helper() {
    let x = 1;
}
"#;
        let path = Path::new("test.rs");

        // First, compute the hash at line 4 (helper function)
        let hash = compute_hash_at_position(&mut service, path, content, 4, 0).unwrap();

        // Now compute display position - should find exact match
        let pos = compute_display_position(&mut service, path, content, 4, 0, Some(&hash), 20);

        assert_eq!(pos.line, 4);
        assert!(!pos.stale);
        assert_eq!(pos.reason, PositionReason::ExactMatch);
    }

    #[test]
    fn test_code_moved_down() {
        let mut service = create_service();

        // Original content - helper at line 4
        let original = r#"fn main() {
    println!("Hello");
}

fn helper() {
    let x = 1;
}
"#;

        // Get hash of helper function at original position
        let path = Path::new("test.rs");
        let hash = compute_hash_at_position(&mut service, path, original, 4, 0).unwrap();

        // New content - added comments, helper moved to line 7
        let new_content = r#"fn main() {
    println!("Hello");
}

// Comment 1
// Comment 2
// Comment 3
fn helper() {
    let x = 1;
}
"#;

        // Compute display position - should find at new location
        let pos = compute_display_position(&mut service, path, new_content, 4, 0, Some(&hash), 20);

        assert_eq!(pos.line, 7);
        assert!(!pos.stale);
        assert!(matches!(pos.reason, PositionReason::FoundElsewhere { .. }));
    }

    #[test]
    fn test_code_modified_stale() {
        let mut service = create_service();

        // Original content
        let original = r#"fn main() {
    println!("Hello");
}

fn helper() {
    let x = 1;
}
"#;

        // Get hash of helper function
        let path = Path::new("test.rs");
        let hash = compute_hash_at_position(&mut service, path, original, 4, 0).unwrap();

        // New content - helper was renamed
        let new_content = r#"fn main() {
    println!("Hello");
}

fn renamed_function() {
    let x = 1;
}
"#;

        // Compute display position - should be stale, at closest node
        let pos = compute_display_position(&mut service, path, new_content, 4, 0, Some(&hash), 20);

        assert!(pos.stale);
        // Should find the renamed function at line 4 as closest
        assert_eq!(pos.line, 4);
        assert!(matches!(pos.reason, PositionReason::ClosestNode { .. }));
    }

    #[test]
    fn test_no_hash_fallback() {
        let mut service = create_service();
        let content = "fn main() {}";
        let path = Path::new("test.rs");

        let pos = compute_display_position(&mut service, path, content, 5, 0, None, 20);

        assert_eq!(pos.line, 5);
        assert!(!pos.stale);
        assert_eq!(pos.reason, PositionReason::Fallback);
    }

    #[test]
    fn test_no_grammar_fallback() {
        let mut service = create_service();
        let content = "some unknown content";
        let path = Path::new("test.xyz"); // Unknown extension

        let pos =
            compute_display_position(&mut service, path, content, 5, 0, Some("somehash"), 20);

        assert_eq!(pos.line, 5);
        assert!(!pos.stale);
        assert_eq!(pos.reason, PositionReason::NoGrammar);
    }

    #[test]
    fn test_compute_node_path() {
        let mut service = create_service();
        let content = r#"fn main() {
    let x = 1;
}
"#;
        let path = Path::new("test.rs");

        let node_path = compute_node_path(&mut service, path, content, 0, 0);

        assert!(node_path.contains(&"source_file".to_string()));
        assert!(node_path.contains(&"function_item".to_string()));
    }
}
