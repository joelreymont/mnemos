use anyhow::Context;
use git::info_for_file;
use log::{debug, warn};
use index as idx;
use notes::{self, Note, NoteFilters};
use rpc::{Request, Response, INTERNAL_ERROR, INVALID_PARAMS, METHOD_NOT_FOUND, PARSE_ERROR};
use rusqlite::Connection;
use serde::{Deserialize, Serialize};
use serde_json::json;
use sha2::{Digest, Sha256};
use std::fs::{self, File};
use std::io::{BufRead, BufReader};
use std::path::Path;
use treesitter::{
    compute_anchor_position, compute_display_position, compute_node_path, default_config,
    GrammarRegistry, ParserService,
};

/// Resolve project root: use provided value, or compute from file path via git.
/// Falls back to file's parent directory if not in a git repo.
fn resolve_project_root(provided: Option<&str>, file: &str) -> String {
    if let Some(pr) = provided {
        return pr.to_string();
    }
    if let Some(git_root) = git::find_root(Path::new(file)) {
        return git_root;
    }
    // Fallback: file's parent directory
    Path::new(file)
        .parent()
        .unwrap_or(Path::new("."))
        .to_string_lossy()
        .into_owned()
}

/// Compute SHA256 hash of content, returning hex string
fn content_hash(content: &str) -> String {
    let mut hasher = Sha256::new();
    hasher.update(content.as_bytes());
    format!("{:x}", hasher.finalize())
}

/// Response wrapper for note list endpoints that includes content hash
#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
struct NotesListResponse {
    notes: Vec<Note>,
    #[serde(skip_serializing_if = "Option::is_none")]
    content_hash: Option<String>,
}

use events::Event;

/// Internal struct for file listing (used by index-project)
#[derive(Debug, Clone)]
struct FileInfo {
    file: String,
    #[allow(dead_code)]
    size: u64,
}

/// Result of list_files including truncation status
struct ListFilesResult {
    files: Vec<FileInfo>,
    truncated: bool,
}

pub mod ai_cli;
pub mod config;
pub mod display;
pub mod events;
pub mod jobs;
pub mod preload;
pub mod server;
pub mod snapshot;
pub mod version;
pub mod watcher;

const IGNORE_DIRS: &[&str] = &[
    ".git",
    "target",
    "node_modules",
    ".hg",
    ".svn",
    ".idea",
    ".direnv",
    "venv",
    "env",
    "__pycache__",
    "build",
];

/// Maximum directory depth to traverse (prevents runaway recursion)
const MAX_TRAVERSAL_DEPTH: usize = 50;

/// Maximum number of files to return from list_files
const MAX_FILE_COUNT: usize = 100_000;

fn list_files(root: &Path) -> anyhow::Result<ListFilesResult> {
    // Canonicalize path to prevent path traversal attacks
    let canonical_root = root.canonicalize()
        .with_context(|| format!("failed to canonicalize path: {}", root.display()))?;

    // Validate the path is a directory and not a system path
    if !canonical_root.is_dir() {
        return Err(anyhow::anyhow!("path is not a directory: {}", canonical_root.display()));
    }

    // Reject obvious system paths to prevent accidental traversal
    let path_str = canonical_root.to_string_lossy();
    if path_str == "/" || path_str.starts_with("/etc") || path_str.starts_with("/var")
        || path_str.starts_with("/usr") || path_str.starts_with("/bin")
        || path_str.starts_with("/sbin") || path_str.starts_with("/System")
        || path_str.starts_with("/Library") || path_str.starts_with("C:\\Windows")
    {
        return Err(anyhow::anyhow!("refusing to traverse system path: {}", canonical_root.display()));
    }

    // Track (path, depth) for depth limiting
    let mut stack = vec![(canonical_root.clone(), 0usize)];
    let mut files = Vec::new();
    let mut truncated = false;
    while let Some((dir, depth)) = stack.pop() {
        // Check depth limit
        if depth > MAX_TRAVERSAL_DEPTH {
            continue; // Skip directories beyond max depth
        }

        for entry in fs::read_dir(&dir)? {
            let entry = entry?;
            let path = entry.path();
            let file_type = entry.file_type()?;

            // Skip symbolic links entirely (TOCTOU protection)
            if file_type.is_symlink() {
                continue;
            }

            // Ensure traversed paths stay within the original root (additional safety)
            if let Ok(canonical_path) = path.canonicalize() {
                if !canonical_path.starts_with(&canonical_root) {
                    continue; // Skip paths that escape the root
                }
            }

            let name = entry.file_name().to_string_lossy().to_string();
            if file_type.is_dir() {
                if IGNORE_DIRS.contains(&name.as_str()) {
                    continue;
                }
                stack.push((path, depth + 1));
            } else if file_type.is_file() {
                // Check file count limit
                if files.len() >= MAX_FILE_COUNT {
                    truncated = true;
                    files.sort_by(|a: &FileInfo, b: &FileInfo| a.file.cmp(&b.file));
                    return Ok(ListFilesResult { files, truncated });
                }
                let size = entry.metadata().map(|m| m.len()).unwrap_or(0);
                files.push(FileInfo {
                    file: path.to_string_lossy().to_string(),
                    size,
                });
            }
        }
    }
    files.sort_by(|a, b| a.file.cmp(&b.file));
    Ok(ListFilesResult { files, truncated })
}

/// Compute human-readable analysis status display string
fn compute_analysis_status(analyzed: bool, analysis_stale: bool, has_file: bool) -> &'static str {
    if analyzed {
        if analysis_stale {
            "Stale (commit changed)"
        } else {
            "Up to date"
        }
    } else if has_file {
        "Has file but not tracked"
    } else {
        "Not analyzed"
    }
}

/// Convert 1-indexed user line to 0-indexed tree-sitter line
#[inline]
fn user_to_ts_line(line: i64) -> u32 {
    (line - 1).max(0) as u32
}

/// Convert 0-indexed tree-sitter line to 1-indexed user line
#[inline]
fn ts_to_user_line(line: u32) -> i64 {
    i64::from(line) + 1
}

/// Read only the specified line range from a file.
/// Line numbers are 1-indexed. Returns lines joined by newlines.
fn read_line_range(path: &Path, start_line: usize, end_line: usize) -> std::io::Result<String> {
    let file = File::open(path)?;
    let reader = BufReader::new(file);
    let mut lines = Vec::with_capacity(end_line - start_line + 1);

    for (idx, line_result) in reader.lines().enumerate() {
        let line_no = idx + 1;
        if line_no > end_line {
            break; // Done - no need to read further
        }
        if line_no >= start_line {
            lines.push(line_result?);
        }
    }

    Ok(lines.join("\n"))
}

pub fn handle(req: Request, db: &Connection, parser: &mut ParserService) -> Response {
    let id = req.id.clone();
    match req.method.as_str() {
        "hemis/open-project" => {
            if req
                .params
                .get("projectRoot")
                .and_then(|v| v.as_str())
                .is_some()
            {
                Response::result(id, json!({"ok": true}))
            } else {
                Response::error(id, INVALID_PARAMS, "missing projectRoot")
            }
        }
        "hemis/display-config" => {
            // Return display configuration for UIs (colors, icons, templates)
            // Single source of truth - UIs should use these values for consistency
            Response::result(id, json!({
                "colors": {
                    "note": "#4682B4",       // Steel Blue - fresh notes
                    "noteStale": "#808080",  // Gray - stale notes
                    "marker": "#4682B4"      // Marker/gutter color
                },
                "icons": {
                    "noteFresh": "📝",
                    "noteStale": "📝",       // Same icon, color indicates state
                    "noteAi": "🤖"
                },
                "templates": {
                    "displayLabel": "{shortId} {summary}",
                    "hoverText": "hemis: {summary}"
                }
            }))
        }
        "hemis/note-templates" => {
            // Return available note templates for structured note creation
            Response::result(id, json!({
                "templates": [
                    {
                        "id": "bug",
                        "name": "Bug Report",
                        "fields": ["severity", "reproduction", "status"],
                        "template": "## Bug Report\n\n**Severity:** \n**Steps to Reproduce:**\n1. \n\n**Expected:** \n**Actual:** \n**Status:** open"
                    },
                    {
                        "id": "todo",
                        "name": "TODO",
                        "fields": ["priority", "assignee", "deadline"],
                        "template": "## TODO\n\n**Priority:** medium\n**Assignee:** \n**Deadline:** \n\n**Description:**\n"
                    },
                    {
                        "id": "api",
                        "name": "API Contract",
                        "fields": ["inputs", "outputs", "invariants"],
                        "template": "## API Contract\n\n**Inputs:**\n- \n\n**Outputs:**\n- \n\n**Invariants:**\n- \n\n**Notes:**\n"
                    },
                    {
                        "id": "decision",
                        "name": "Design Decision",
                        "fields": ["context", "options", "rationale"],
                        "template": "## Design Decision\n\n**Context:**\n\n**Options Considered:**\n1. \n2. \n\n**Decision:**\n\n**Rationale:**\n"
                    },
                    {
                        "id": "review",
                        "name": "Code Review",
                        "fields": ["issue", "suggestion", "severity"],
                        "template": "## Code Review\n\n**Issue:**\n\n**Suggestion:**\n\n**Severity:** info"
                    }
                ]
            }))
        }
        "hemis/suggest-tags" => {
            // Suggest tags based on code context for rapid note capture
            let file = req.params.get("file").and_then(|v| v.as_str());
            let content = req.params.get("content").and_then(|v| v.as_str());
            let line = req.params.get("line").and_then(|v| v.as_i64()).unwrap_or(1);
            let column = req.params.get("column").and_then(|v| v.as_i64()).unwrap_or(0);

            let Some(file) = file else {
                return Response::error(id, INVALID_PARAMS, "missing file");
            };

            let mut tags: Vec<String> = Vec::new();
            let file_path = Path::new(file);

            // 1. Language tag from extension
            if let Some(lang) = parser.language_for_file(file_path) {
                tags.push(lang.to_string());
            }

            // 2. File pattern tags
            let file_lower = file.to_lowercase();
            if file_lower.contains("test") || file_lower.contains("spec") {
                tags.push("test".to_string());
            }
            if file_lower.contains("readme") || file_lower.contains("doc") {
                tags.push("docs".to_string());
            }

            // 3. Node type from tree-sitter (if content provided)
            if let Some(content) = content {
                let ts_line = user_to_ts_line(line);
                let ts_column = column.clamp(0, i64::from(u32::MAX)) as u32;
                let anchor = compute_anchor_position(parser, file_path, content, ts_line, ts_column);

                // Extract semantic tags from node path
                for node_type in &anchor.node_path {
                    match node_type.as_str() {
                        "function_item" | "function_definition" | "function_declaration" | "method_definition" => {
                            tags.push("function".to_string());
                        }
                        "struct_item" | "class_definition" | "class_declaration" | "interface_declaration" => {
                            tags.push("type".to_string());
                        }
                        "impl_item" | "method_declaration" => {
                            tags.push("impl".to_string());
                        }
                        "enum_item" | "enum_declaration" => {
                            tags.push("enum".to_string());
                        }
                        "const_item" | "static_item" | "variable_declaration" => {
                            tags.push("var".to_string());
                        }
                        _ => {}
                    }
                }
            }

            // Deduplicate and return
            tags.sort();
            tags.dedup();
            Response::result(id, json!({ "tags": tags }))
        }
        "hemis/graph" => {
            // Return graph of notes and their edges for visualization
            let project_root = req.params.get("projectRoot").and_then(|v| v.as_str());
            let note_id = req.params.get("noteId").and_then(|v| v.as_str());
            let _include_stale = req.params.get("includeStale").and_then(|v| v.as_bool()).unwrap_or(true);
            let max_depth = req.params.get("maxDepth").and_then(|v| v.as_u64()).unwrap_or(2) as usize;

            let Some(project_root) = project_root else {
                return Response::error(id, INVALID_PARAMS, "missing projectRoot");
            };

            // Query notes for project
            let mut stmt = match db.prepare(
                "SELECT id, file, line, summary, tags, node_text_hash FROM notes WHERE project_root = ? ORDER BY updated_at DESC LIMIT 500"
            ) {
                Ok(s) => s,
                Err(e) => return Response::error(id, INTERNAL_ERROR, e.to_string()),
            };

            let notes_iter = match stmt.query_map([project_root], |row| {
                Ok(json!({
                    "id": row.get::<_, String>(0)?,
                    "file": row.get::<_, String>(1)?,
                    "line": row.get::<_, i64>(2)?,
                    "summary": row.get::<_, String>(3)?,
                    "tags": row.get::<_, String>(4).ok().and_then(|s| serde_json::from_str::<serde_json::Value>(&s).ok()).unwrap_or(json!([])),
                    "hasHash": row.get::<_, Option<String>>(5)?.is_some()
                }))
            }) {
                Ok(iter) => iter,
                Err(e) => return Response::error(id, INTERNAL_ERROR, e.to_string()),
            };

            let nodes: Vec<serde_json::Value> = notes_iter.filter_map(|r| r.ok()).collect();
            let node_ids: std::collections::HashSet<String> = nodes.iter()
                .filter_map(|n| n.get("id").and_then(|v| v.as_str()).map(|s| s.to_string()))
                .collect();

            // Query edges for project
            let mut edge_stmt = match db.prepare(
                "SELECT src, dst, kind FROM edges WHERE project_root = ?"
            ) {
                Ok(s) => s,
                Err(e) => return Response::error(id, INTERNAL_ERROR, e.to_string()),
            };

            let edges_iter = match edge_stmt.query_map([project_root], |row| {
                Ok((
                    row.get::<_, String>(0)?,
                    row.get::<_, String>(1)?,
                    row.get::<_, String>(2)?,
                ))
            }) {
                Ok(iter) => iter,
                Err(e) => return Response::error(id, INTERNAL_ERROR, e.to_string()),
            };

            // Filter edges to only include those between known nodes
            let edges: Vec<serde_json::Value> = edges_iter
                .filter_map(|r| r.ok())
                .filter(|(src, dst, _)| node_ids.contains(src) && node_ids.contains(dst))
                .map(|(src, dst, kind)| json!({ "src": src, "dst": dst, "kind": kind }))
                .collect();

            // If noteId is provided, filter to connected subgraph
            let (filtered_nodes, filtered_edges) = if let Some(focus_id) = note_id {
                // BFS to find connected nodes up to max_depth
                let mut visited: std::collections::HashSet<String> = std::collections::HashSet::new();
                let mut frontier = vec![focus_id.to_string()];
                visited.insert(focus_id.to_string());

                for _ in 0..max_depth {
                    let mut next_frontier = Vec::new();
                    for node in &frontier {
                        for edge in &edges {
                            let src = edge.get("src").and_then(|v| v.as_str()).unwrap_or("");
                            let dst = edge.get("dst").and_then(|v| v.as_str()).unwrap_or("");
                            if src == node && !visited.contains(dst) {
                                visited.insert(dst.to_string());
                                next_frontier.push(dst.to_string());
                            }
                            if dst == node && !visited.contains(src) {
                                visited.insert(src.to_string());
                                next_frontier.push(src.to_string());
                            }
                        }
                    }
                    frontier = next_frontier;
                }

                let filtered_nodes: Vec<_> = nodes.into_iter()
                    .filter(|n| n.get("id").and_then(|v| v.as_str()).map(|id| visited.contains(id)).unwrap_or(false))
                    .collect();
                let filtered_edges: Vec<_> = edges.into_iter()
                    .filter(|e| {
                        let src = e.get("src").and_then(|v| v.as_str()).unwrap_or("");
                        let dst = e.get("dst").and_then(|v| v.as_str()).unwrap_or("");
                        visited.contains(src) && visited.contains(dst)
                    })
                    .collect();
                (filtered_nodes, filtered_edges)
            } else {
                (nodes, edges)
            };

            Response::result(id, json!({
                "nodes": filtered_nodes,
                "edges": filtered_edges
            }))
        }
        "hemis/tasks" => {
            // Scan indexed files for TODO/FIXME/HACK comments
            let project_root = req.params.get("projectRoot").and_then(|v| v.as_str());
            let limit = req.params.get("limit").and_then(|v| v.as_u64()).unwrap_or(100) as usize;

            let Some(project_root) = project_root else {
                return Response::error(id, INVALID_PARAMS, "missing projectRoot");
            };

            // Query indexed files for project
            let mut stmt = match db.prepare(
                "SELECT file, content FROM files WHERE project_root = ? AND content IS NOT NULL"
            ) {
                Ok(s) => s,
                Err(e) => return Response::error(id, INTERNAL_ERROR, e.to_string()),
            };

            let files_iter = match stmt.query_map([project_root], |row| {
                Ok((row.get::<_, String>(0)?, row.get::<_, String>(1)?))
            }) {
                Ok(iter) => iter,
                Err(e) => return Response::error(id, INTERNAL_ERROR, e.to_string()),
            };

            // Keywords to detect (case-insensitive)
            const TASK_KEYWORDS: &[&str] = &["TODO", "FIXME", "HACK", "XXX", "BUG"];
            let mut tasks: Vec<serde_json::Value> = Vec::new();

            for file_result in files_iter {
                if tasks.len() >= limit {
                    break;
                }
                let Ok((file, content)) = file_result else { continue };

                for (line_idx, line) in content.lines().enumerate() {
                    if tasks.len() >= limit {
                        break;
                    }
                    let upper = line.to_uppercase();
                    for keyword in TASK_KEYWORDS {
                        if let Some(pos) = upper.find(keyword) {
                            // Extract text after the keyword
                            let after_keyword = &line[pos + keyword.len()..];
                            let text = after_keyword.trim_start_matches(|c| c == ':' || c == ' ').trim();
                            tasks.push(json!({
                                "file": file,
                                "line": line_idx + 1,  // 1-indexed
                                "kind": *keyword,
                                "text": text,
                                "context": line.trim()
                            }));
                            break; // Only report first keyword per line
                        }
                    }
                }
            }

            Response::result(id, json!({ "tasks": tasks }))
        }
        "hemis/code-references" => {
            // Extract code references (identifiers, calls, types) at a position
            // Uses node path to identify semantic context
            let file = req.params.get("file").and_then(|v| v.as_str());
            let content = req.params.get("content").and_then(|v| v.as_str());
            let line = req.params.get("line").and_then(|v| v.as_i64()).unwrap_or(1);
            let column = req.params.get("column").and_then(|v| v.as_i64()).unwrap_or(0);

            let Some(file) = file else {
                return Response::error(id, INVALID_PARAMS, "missing file");
            };
            let Some(content) = content else {
                return Response::error(id, INVALID_PARAMS, "missing content");
            };

            let file_path = Path::new(file);
            let ts_line = user_to_ts_line(line);
            let ts_column = column.clamp(0, i64::from(u32::MAX)) as u32;

            // Get node path and anchor position
            let node_path = compute_node_path(parser, file_path, content, ts_line, ts_column);
            let anchor = compute_anchor_position(parser, file_path, content, ts_line, ts_column);

            // Build references from node path
            let mut references: Vec<serde_json::Value> = Vec::new();

            for (i, node_type) in node_path.iter().enumerate() {
                let ref_kind = match node_type.as_str() {
                    "call_expression" | "function_call" | "macro_invocation" => Some("call"),
                    "identifier" | "field_identifier" => Some("identifier"),
                    "type_identifier" | "primitive_type" => Some("type"),
                    "function_item" | "function_definition" => Some("function_def"),
                    "struct_item" | "class_definition" => Some("type_def"),
                    "impl_item" => Some("impl"),
                    _ => None,
                };

                if let Some(kind) = ref_kind {
                    references.push(json!({
                        "kind": kind,
                        "nodeType": node_type,
                        "depth": i
                    }));
                }
            }

            // Add anchor info
            Response::result(id, json!({
                "references": references,
                "anchor": {
                    "line": anchor.line + 1,  // Convert to 1-indexed
                    "column": anchor.column,
                    "hash": anchor.node_text_hash
                },
                "nodePath": node_path
            }))
        }
        "hemis/file-context" => {
            // Return contextual suggestions when opening a file
            // Includes: notes for file, backlinks, stale notes, related notes
            let file = req.params.get("file").and_then(|v| v.as_str());
            let project_root = req.params.get("projectRoot").and_then(|v| v.as_str());

            let Some(file) = file else {
                return Response::error(id, INVALID_PARAMS, "missing file");
            };
            let Some(project_root) = project_root else {
                return Response::error(id, INVALID_PARAMS, "missing projectRoot");
            };

            // Get notes for this file
            let filters = NoteFilters {
                file,
                project_root,
                node_path: None,
                commit: None,
                blob: None,
                include_stale: true,
            };
            let file_notes = notes::list_for_file(db, filters).unwrap_or_default();

            // Get stale notes count
            let stale_count = file_notes.iter().filter(|n| n.stale).count();

            // Get backlinks (notes that link to notes in this file)
            let note_ids: Vec<&str> = file_notes.iter().map(|n| n.id.as_str()).collect();
            let mut backlinks: Vec<serde_json::Value> = Vec::new();
            if !note_ids.is_empty() {
                let placeholders: String = note_ids.iter().map(|_| "?").collect::<Vec<_>>().join(",");
                let query = format!(
                    "SELECT DISTINCT n.id, n.file, n.line, n.summary FROM edges e \
                     JOIN notes n ON e.src = n.id \
                     WHERE e.dst IN ({}) AND n.file != ? LIMIT 10",
                    placeholders
                );
                if let Ok(mut stmt) = db.prepare(&query) {
                    let mut params: Vec<&dyn rusqlite::ToSql> = note_ids.iter().map(|id| id as &dyn rusqlite::ToSql).collect();
                    params.push(&file);
                    if let Ok(rows) = stmt.query_map(params.as_slice(), |row| {
                        Ok(json!({
                            "id": row.get::<_, String>(0)?,
                            "file": row.get::<_, String>(1)?,
                            "line": row.get::<_, i64>(2)?,
                            "summary": row.get::<_, String>(3)?
                        }))
                    }) {
                        backlinks = rows.filter_map(|r| r.ok()).collect();
                    }
                }
            }

            // Get recent notes in project for suggestions
            let mut recent_stmt = db.prepare(
                "SELECT id, file, line, summary FROM notes \
                 WHERE project_root = ? AND file != ? \
                 ORDER BY updated_at DESC LIMIT 5"
            ).ok();
            let recent_notes: Vec<serde_json::Value> = recent_stmt.as_mut()
                .and_then(|stmt| stmt.query_map([project_root, file], |row| {
                    Ok(json!({
                        "id": row.get::<_, String>(0)?,
                        "file": row.get::<_, String>(1)?,
                        "line": row.get::<_, i64>(2)?,
                        "summary": row.get::<_, String>(3)?
                    }))
                }).ok())
                .map(|rows| rows.filter_map(|r| r.ok()).collect())
                .unwrap_or_default();

            Response::result(id, json!({
                "notes": file_notes.len(),
                "staleNotes": stale_count,
                "backlinks": backlinks,
                "recentNotes": recent_notes,
                "suggestions": {
                    "hasStaleNotes": stale_count > 0,
                    "hasBacklinks": !backlinks.is_empty(),
                    "uncoveredFile": file_notes.is_empty()
                }
            }))
        }
        "hemis/explain-region" => {
            let file = req.params.get("file").and_then(|v| v.as_str());
            let project_root = req.params.get("projectRoot").and_then(|v| v.as_str());
            // Accept both nested format (start.line) and flat format (startLine)
            let start_line = req
                .params
                .get("start")
                .and_then(|s| s.get("line"))
                .and_then(|v| v.as_u64())
                .or_else(|| req.params.get("startLine").and_then(|v| v.as_u64()))
                .unwrap_or(1);
            let end_line = req
                .params
                .get("end")
                .and_then(|s| s.get("line"))
                .and_then(|v| v.as_u64())
                .or_else(|| req.params.get("endLine").and_then(|v| v.as_u64()))
                .unwrap_or(start_line);

            // Validate line range
            if start_line == 0 {
                return Response::error(id, INVALID_PARAMS, "startLine must be >= 1");
            }
            if end_line < start_line {
                return Response::error(id, INVALID_PARAMS, "endLine must be >= startLine");
            }
            // Cap line range to prevent excessive memory usage (use checked_sub for overflow safety)
            const MAX_LINE_RANGE: u64 = 10_000;
            const MAX_LINE_NUMBER: u64 = 10_000_000;
            if start_line > MAX_LINE_NUMBER || end_line > MAX_LINE_NUMBER {
                return Response::error(id, INVALID_PARAMS, format!("line number too large (max {})", MAX_LINE_NUMBER));
            }
            let line_range = end_line.checked_sub(start_line).unwrap_or(0);
            if line_range > MAX_LINE_RANGE {
                return Response::error(id, INVALID_PARAMS, format!("line range too large (max {} lines)", MAX_LINE_RANGE));
            }
            // Use TryFrom for safe conversion (defensive against platform differences)
            let start_line = match usize::try_from(start_line) {
                Ok(n) => n,
                Err(_) => return Response::error(id, INVALID_PARAMS, "line number out of range"),
            };
            let end_line = match usize::try_from(end_line) {
                Ok(n) => n,
                Err(_) => return Response::error(id, INVALID_PARAMS, "line number out of range"),
            };

            let use_ai = req
                .params
                .get("useAI")
                .and_then(|v| v.as_bool())
                .unwrap_or(false);
            let detailed = req
                .params
                .get("detailed")
                .and_then(|v| v.as_bool())
                .unwrap_or(false);
            let content_param = req.params.get("content").and_then(|v| v.as_str());
            if let Some(file) = file {
                // Get snippet: from content param (filter lines) or file (read only needed lines)
                let snippet_result = if let Some(c) = content_param {
                    // Content provided - filter to requested lines
                    Ok(c.lines()
                        .enumerate()
                        .filter(|(idx, _)| {
                            let line_no = idx + 1;
                            line_no >= start_line && line_no <= end_line
                        })
                        .map(|(_, l)| l)
                        .collect::<Vec<_>>()
                        .join("\n"))
                } else {
                    // Read only the requested line range from file
                    read_line_range(Path::new(file), start_line, end_line)
                };
                match snippet_result {
                    Ok(snippet) => {

                        if snippet.is_empty() {
                            return Response::result(id, json!({"content": "", "references": []}));
                        }

                        // If AI requested, try to get AI explanation
                        if use_ai {
                            // Find project root: param > git > file's parent dir
                            let root_path = if let Some(pr) = project_root {
                                Path::new(pr).to_path_buf()
                            } else if let Some(git_root) = git::find_root(Path::new(file)) {
                                Path::new(&git_root).to_path_buf()
                            } else {
                                Path::new(file)
                                    .parent()
                                    .unwrap_or(Path::new("."))
                                    .to_path_buf()
                            };

                            // Check if project is indexed (used for AI context)
                            // Note: We don't auto-index here as it would block on large repos
                            // The AI explanation will work without context, just less informed
                            let _is_indexed = storage::get_project_meta(db, root_path.to_string_lossy().as_ref())
                                .ok()
                                .flatten()
                                .map(|m| m.indexed_at.is_some())
                                .unwrap_or(false);

                            // Try AI explanation (works with or without project index)
                            match ai_cli::explain_region(&root_path, file, start_line, end_line, &snippet, detailed) {
                                Ok((provider, explanation, had_context)) => {
                                    let context_suffix = if had_context { " + project context" } else { "" };
                                    let ai_status = format!("[AI: {}{}]", provider.as_str(), context_suffix);
                                    Response::result(id, json!({
                                        "content": snippet,
                                        "explanation": explanation,
                                        "references": [],
                                        "ai": {
                                            "provider": provider.as_str(),
                                            "hadContext": had_context,
                                            "statusDisplay": ai_status
                                        }
                                    }))
                                }
                                Err(e) => {
                                    // AI failed, return snippet with error info
                                    let ai_status = format!("[AI error: {}]", e);
                                    Response::result(id, json!({
                                        "content": snippet,
                                        "references": [],
                                        "ai": {
                                            "error": e.to_string(),
                                            "statusDisplay": ai_status
                                        }
                                    }))
                                }
                            }
                        } else {
                            // No AI requested, just return snippet
                            Response::result(id, json!({"content": snippet, "references": []}))
                        }
                    }
                    Err(_) => Response::error(id, INTERNAL_ERROR, "operation failed"),
                }
            } else {
                Response::error(id, INVALID_PARAMS, "missing file")
            }
        }
        "hemis/search" => {
            let query = req
                .params
                .get("query")
                .and_then(|v| v.as_str())
                .unwrap_or("");
            // projectRoot is optional - computed from file if not provided
            let proj_param = req.params.get("projectRoot").and_then(|v| v.as_str());
            let file_param = req.params.get("file").and_then(|v| v.as_str());
            let proj = proj_param
                .map(|s| s.to_string())
                .or_else(|| file_param.map(|f| resolve_project_root(None, &f)));
            let include_notes = req
                .params
                .get("includeNotes")
                .and_then(|v| v.as_bool())
                .unwrap_or(true);
            // Validate vector array size (typical embedding dimensions: 384-4096)
            const MAX_VECTOR_DIM: usize = 8192;
            let query_vec: Option<Vec<f32>> = req
                .params
                .get("vector")
                .and_then(|v| v.as_array())
                .and_then(|arr| {
                    if arr.len() > MAX_VECTOR_DIM {
                        None // Reject oversized vectors
                    } else {
                        Some(
                            arr.iter()
                                .filter_map(|x| x.as_f64().map(|f| f as f32))
                                .collect::<Vec<f32>>(),
                        )
                    }
                });
            // Reject empty queries to avoid returning all indexed content
            if query.trim().is_empty() && query_vec.is_none() {
                return Response::result(id, json!([]));
            }
            match idx::search(db, query, proj.as_deref()) {
                Ok(mut results) => {
                    // If a query vector is provided, blend semantic hits.
                    if let Some(vec) = query_vec {
                        if let Ok(mut semantic_hits) = idx::semantic_search(db, &vec, proj.as_deref(), 5) {
                            results.append(&mut semantic_hits);
                        }
                    }
                    if include_notes {
                        if let Ok(notes) = notes::search(db, query, proj.as_deref(), None, 0) {
                            for n in notes {
                                // Safe conversion: skip notes with out-of-bounds coordinates
                                let line = match usize::try_from(n.line) {
                                    Ok(l) if l > 0 => l,
                                    _ => continue,
                                };
                                let column = match usize::try_from(n.column) {
                                    Ok(c) => c,
                                    _ => continue,
                                };
                                results.push(idx::SearchHit {
                                    file: n.file.clone(),
                                    line,
                                    column,
                                    text: n.summary.clone(),
                                    score: 0.5,
                                    kind: Some("note".into()),
                                    note_id: Some(n.id.clone()),
                                    note_summary: Some(n.summary.clone()),
                                    display_label: Some(format!("[Note] {}", n.summary)),
                                    display_detail: Some(format!("{}:{}", n.file, line)),
                                });
                            }
                        }
                    }
                    Response::result_from(id, results)
                }
                Err(_) => Response::error(id, INTERNAL_ERROR, "operation failed"),
            }
        }
        "hemis/index-project" => {
            // projectRoot is optional - computed from file if not provided
            let proj_param = req.params.get("projectRoot").and_then(|v| v.as_str());
            let file_param = req.params.get("file").and_then(|v| v.as_str());
            let root_opt = proj_param
                .map(|s| s.to_string())
                .or_else(|| file_param.map(|f| resolve_project_root(None, f)));
            if let Some(root) = root_opt.as_deref() {
                let include_ai = req
                    .params
                    .get("includeAI")
                    .and_then(|v| v.as_bool())
                    .unwrap_or(false);
                let async_mode = req
                    .params
                    .get("async")
                    .and_then(|v| v.as_bool())
                    .unwrap_or(false);

                // If async mode, queue the job and return task ID immediately
                if async_mode {
                    let task_id = jobs::generate_task_id();
                    jobs::job_queue().enqueue(jobs::Job::IndexProject {
                        task_id: task_id.clone(),
                        project_root: root.to_string(),
                        include_ai,
                    });
                    return Response::result(id, json!({
                        "ok": true,
                        "taskId": task_id,
                        "async": true,
                        "statusMessage": "Indexing queued"
                    }));
                }

                // Synchronous mode (existing behavior)
                let mut indexed = 0;
                let mut skipped = 0;
                // Max file size for indexing (1MB - same as search limit)
                const MAX_INDEX_FILE_SIZE: u64 = 1024 * 1024;
                match list_files(Path::new(root)) {
                    Ok(result) => {
                        let truncated = result.truncated;
                        for f in result.files {
                            // Skip oversized files to prevent memory exhaustion
                            let file_size = fs::metadata(&f.file)
                                .map(|m| m.len())
                                .unwrap_or(0);
                            if file_size > MAX_INDEX_FILE_SIZE {
                                skipped += 1;
                                continue;
                            }
                            if let Ok(content) = fs::read_to_string(&f.file) {
                                match idx::add_file(db, &f.file, root, &content) {
                                    Ok(Some(_)) => indexed += 1,
                                    Ok(None) => skipped += 1, // unchanged
                                    Err(e) => warn!("index failed for {}: {}", f.file, e),
                                }
                            }
                        }
                        // Update project_meta with indexing info
                        let commit_sha = git::head_commit(Path::new(root));
                        let _ = storage::set_project_indexed(db, root, commit_sha.as_deref());

                        // Pre-warm the Claude process in background for faster explain-region
                        if ai_cli::CliProvider::from_env() == Some(ai_cli::CliProvider::Claude) {
                            let root_path = Path::new(root).to_path_buf();
                            std::thread::spawn(move || {
                                if let Err(e) = ai_cli::warm_up_claude(&root_path) {
                                    debug!("[hemis] Claude warm-up failed: {}", e);
                                }
                            });
                        }

                        // Optionally run AI analysis
                        let mut ai_result: Option<serde_json::Value> = None;
                        if include_ai {
                            match ai_cli::analyze_repo(Path::new(root)) {
                                Ok(provider) => {
                                    let _ = storage::set_project_analyzed(
                                        db,
                                        root,
                                        commit_sha.as_deref(),
                                        provider.as_str(),
                                    );
                                    ai_result = Some(json!({
                                        "analyzed": true,
                                        "provider": provider.as_str()
                                    }));
                                }
                                Err(e) => {
                                    ai_result = Some(json!({
                                        "analyzed": false,
                                        "error": e.to_string()
                                    }));
                                }
                            }
                        }
                        // Emit index-complete event
                        events::emit(Event::IndexComplete {
                            project: root.to_string(),
                            files_indexed: indexed,
                        });

                        // Build human-readable status message
                        let status_message = match &ai_result {
                            Some(ai) if ai.get("analyzed") == Some(&json!(true)) => {
                                let provider = ai.get("provider").and_then(|v| v.as_str()).unwrap_or("AI");
                                format!("Project indexed: {} files, analyzed with {}", indexed, provider)
                            }
                            Some(ai) if ai.get("error").is_some() => {
                                let error = ai.get("error").and_then(|v| v.as_str()).unwrap_or("unknown");
                                format!("Project indexed: {} files (AI failed: {})", indexed, error)
                            }
                            _ => format!("Project indexed: {} files", indexed),
                        };

                        let mut result = json!({
                            "ok": true,
                            "indexed": indexed,
                            "skipped": skipped,
                            "truncated": truncated,
                            "projectRoot": root,
                            "statusMessage": status_message
                        });
                        if let Some(ai) = ai_result {
                            result["ai"] = ai;
                        }
                        Response::result(id, result)
                    }
                    Err(_) => Response::error(id, INTERNAL_ERROR, "operation failed"),
                }
            } else {
                Response::error(id, INVALID_PARAMS, "missing projectRoot")
            }
        }
        "hemis/save-snapshot" => {
            if let Some(path) = req.params.get("path").and_then(|v| v.as_str()) {
                let project_root = req.params.get("projectRoot").and_then(|v| v.as_str());
                match snapshot::create(db, project_root).and_then(|payload| {
                    let json_bytes = serde_json::to_vec_pretty(&payload)
                        .map_err(|e| anyhow::anyhow!("failed to serialize snapshot: {}", e))?;
                    fs::write(path, json_bytes)
                        .map(|_| payload.clone())
                        .map_err(|e| anyhow::anyhow!(e))
                }) {
                    Ok(payload) => Response::result(
                        id,
                        json!({"ok": true, "path": path, "counts": payload.get("counts")}),
                    ),
                    Err(_) => Response::error(id, INTERNAL_ERROR, "operation failed"),
                }
            } else {
                Response::error(id, INVALID_PARAMS, "missing path")
            }
        }
        "hemis/load-snapshot" => {
            // Size limit for snapshot files (10MB - reduced from 100MB to limit memory usage)
            const MAX_SNAPSHOT_SIZE: u64 = 10 * 1024 * 1024;

            if let Some(path) = req.params.get("path").and_then(|v| v.as_str()) {
                // Check file size before reading
                match fs::metadata(path) {
                    Ok(meta) if meta.len() > MAX_SNAPSHOT_SIZE => {
                        return Response::error(
                            id,
                            INVALID_PARAMS,
                            format!("snapshot file too large: {} bytes (max {})", meta.len(), MAX_SNAPSHOT_SIZE),
                        );
                    }
                    Err(_) => {
                        return Response::error(id, INTERNAL_ERROR, "failed to read snapshot file");
                    }
                    _ => {}
                }
                // Use streaming reader to avoid loading entire file into memory twice
                match std::fs::File::open(path) {
                    Ok(file) => {
                        let reader = std::io::BufReader::new(file);
                        match serde_json::from_reader::<_, serde_json::Value>(reader) {
                            Ok(val) => match snapshot::restore(db, &val) {
                                Ok(status) => Response::result(id, status),
                                Err(_) => Response::error(id, INTERNAL_ERROR, "snapshot restore failed"),
                            },
                            Err(_) => Response::error(id, INTERNAL_ERROR, "invalid snapshot JSON"),
                        }
                    }
                    Err(_) => Response::error(id, INTERNAL_ERROR, "failed to open snapshot file"),
                }
            } else {
                Response::error(id, INVALID_PARAMS, "missing path")
            }
        }
        "hemis/status" => {
            let proj = req.params.get("projectRoot").and_then(|v| v.as_str());
            match storage::counts(db, proj) {
                Ok(c) => {
                    // Build a human-readable status display
                    let status_display = format!(
                        "{} notes, {} files, {} edges",
                        c.notes, c.files, c.edges
                    );
                    Response::result(
                        id,
                        json!({
                            "ok": true,
                            "projectRoot": proj,
                            "counts": {
                                "notes": c.notes,
                                "files": c.files,
                                "embeddings": c.embeddings,
                                "edges": c.edges
                            },
                            "statusDisplay": status_display
                        }),
                    )
                }
                Err(_) => Response::error(id, INTERNAL_ERROR, "operation failed"),
            }
        }
        "hemis/project-meta" => {
            // projectRoot is optional - computed from file if not provided
            let proj_param = req.params.get("projectRoot").and_then(|v| v.as_str());
            let file_param = req.params.get("file").and_then(|v| v.as_str());
            let proj_opt = proj_param
                .map(|s| s.to_string())
                .or_else(|| file_param.map(|f| resolve_project_root(None, f)));
            if let Some(proj) = proj_opt.as_deref() {
                match storage::get_project_meta(db, proj) {
                    Ok(Some(meta)) => {
                        // Check if analysis is stale by comparing to current HEAD
                        let current_commit = git::head_commit(Path::new(proj));
                        let analysis_stale = meta.analysis_commit_sha.as_ref()
                            .map(|sha| current_commit.as_ref() != Some(sha))
                            .unwrap_or(true);
                        let has_analysis = ai_cli::has_analysis(Path::new(proj));
                        let ai_available = ai_cli::CliProvider::from_env().is_some();
                        let analyzed = meta.analyzed_at.is_some();
                        let analysis_status = compute_analysis_status(analyzed, analysis_stale, has_analysis);
                        let formatted_indexed_at = meta.indexed_at.map(notes::format_timestamp);
                        let formatted_analyzed_at = meta.analyzed_at.map(notes::format_timestamp);
                        Response::result(id, json!({
                            "projectRoot": proj,
                            "indexed": meta.indexed_at.is_some(),
                            "indexedAt": meta.indexed_at,
                            "formattedIndexedAt": formatted_indexed_at,
                            "indexedCommit": meta.indexed_commit_sha,
                            "analyzed": analyzed,
                            "analyzedAt": meta.analyzed_at,
                            "formattedAnalyzedAt": formatted_analyzed_at,
                            "analysisCommit": meta.analysis_commit_sha,
                            "analysisProvider": meta.analysis_provider,
                            "analysisStale": analysis_stale,
                            "analysisStatusDisplay": analysis_status,
                            "hasAnalysisFile": has_analysis,
                            "aiAvailable": ai_available,
                            "currentCommit": current_commit
                        }))
                    }
                    Ok(None) => {
                        // Project not tracked yet
                        let has_analysis = ai_cli::has_analysis(Path::new(proj));
                        let ai_available = ai_cli::CliProvider::from_env().is_some();
                        let current_commit = git::head_commit(Path::new(proj));
                        let analysis_status = compute_analysis_status(false, true, has_analysis);
                        Response::result(id, json!({
                            "projectRoot": proj,
                            "indexed": false,
                            "analyzed": false,
                            "analysisStale": true,
                            "analysisStatusDisplay": analysis_status,
                            "hasAnalysisFile": has_analysis,
                            "aiAvailable": ai_available,
                            "currentCommit": current_commit
                        }))
                    }
                    Err(_) => Response::error(id, INTERNAL_ERROR, "operation failed"),
                }
            } else {
                Response::error(id, INVALID_PARAMS, "missing projectRoot")
            }
        }
        "hemis/buffer-context" => {
            // Returns projectRoot, commit, blob, language for a file
            // UIs can drop their git shell calls and use this instead
            if let Some(file) = req.params.get("file").and_then(|v| v.as_str()) {
                let git_info = info_for_file(file);
                let project_root = git_info.as_ref().map(|g| g.root.as_str())
                    .or_else(|| git::find_root(Path::new(file)).as_deref().map(|_| ""))
                    .unwrap_or_else(|| Path::new(file).parent().map(|p| p.to_str().unwrap_or("")).unwrap_or(""));
                let project_root = if project_root.is_empty() {
                    git::find_root(Path::new(file)).unwrap_or_else(|| {
                        Path::new(file).parent().map(|p| p.to_string_lossy().into_owned()).unwrap_or_default()
                    })
                } else {
                    project_root.to_string()
                };
                let commit = git_info.as_ref().map(|g| g.commit.as_str());
                let blob = git_info.as_ref().and_then(|g| g.blob.as_deref());
                let language = parser.language_for_file(Path::new(file));
                Response::result(id, json!({
                    "projectRoot": project_root,
                    "commit": commit,
                    "blob": blob,
                    "language": language
                }))
            } else {
                Response::error(id, INVALID_PARAMS, "missing file")
            }
        }
        "notes/anchor" => {
            // Compute anchor position for cursor, returns line/column/nodePath/nodeTextHash
            // UIs can drop their Tree-sitter implementations
            let file = req.params.get("file").and_then(|v| v.as_str());
            let content = req.params.get("content").and_then(|v| v.as_str());
            let cursor_line = req.params.get("cursorLine").and_then(|v| v.as_u64()).map(|v| v as u32);
            let cursor_column = req.params.get("cursorColumn").and_then(|v| v.as_u64()).map(|v| v as u32);

            match (file, content, cursor_line, cursor_column) {
                (Some(file), Some(content), Some(line), Some(col)) => {
                    let anchor = compute_anchor_position(parser, Path::new(file), content, line, col);
                    Response::result(id, json!({
                        "line": anchor.line,
                        "column": anchor.column,
                        "nodePath": anchor.node_path,
                        "nodeTextHash": anchor.node_text_hash
                    }))
                }
                _ => Response::error(id, INVALID_PARAMS, "missing file, content, cursorLine, or cursorColumn")
            }
        }
        "notes/list-for-file" => {
            let file = req.params.get("file").and_then(|v| v.as_str());
            if let Some(file) = file {
                // projectRoot is now optional - computed from file if not provided
                let proj_param = req.params.get("projectRoot").and_then(|v| v.as_str());
                let proj = resolve_project_root(proj_param, file);
                // commit/blob are now optional - computed from file via git if not provided
                // Only auto-fill from git if NEITHER is explicitly provided (to allow staleness testing)
                let req_commit = req.params.get("commit").and_then(|v| v.as_str());
                let req_blob = req.params.get("blob").and_then(|v| v.as_str());
                let (commit, blob) = if req_commit.is_none() && req_blob.is_none() {
                    // Neither provided - auto-fill both from git
                    let git_info = info_for_file(file);
                    (
                        git_info.as_ref().map(|g| g.commit.clone()),
                        git_info.as_ref().and_then(|g| g.blob.clone()),
                    )
                } else {
                    // At least one explicitly provided - use what was given
                    (req_commit.map(String::from), req_blob.map(String::from))
                };
                let include_stale = req
                    .params
                    .get("includeStale")
                    .and_then(|v| v.as_bool())
                    .unwrap_or(false);
                let only_stale = req
                    .params
                    .get("onlyStale")
                    .and_then(|v| v.as_bool())
                    .unwrap_or(false);
                let filters = NoteFilters {
                    file,
                    project_root: &proj,
                    node_path: None,
                    commit: commit.as_deref(),
                    blob: blob.as_deref(),
                    include_stale,
                };
                // Optional formatting params
                let language = req.params.get("language").and_then(|v| v.as_str());
                let wrap_width = req
                    .params
                    .get("wrapWidth")
                    .and_then(|v| v.as_u64())
                    .map(|w| w as usize);
                // Extract content early for both position computation and hash
                let content_param = req.params.get("content").and_then(|v| v.as_str());
                match notes::list_for_file(db, filters) {
                    Ok(mut notes_list) => {
                        // If content is provided, compute display positions server-side
                        if let Some(content) = content_param {
                            let file_path = Path::new(file);
                            for note in &mut notes_list {
                                // Convert from 1-based (database) to 0-based (tree-sitter)
                                let ts_line = user_to_ts_line(note.line);
                                let ts_column = note.column.clamp(0, i64::from(u32::MAX)) as u32;
                                let pos = compute_display_position(
                                    parser,
                                    file_path,
                                    content,
                                    ts_line,
                                    ts_column,
                                    note.node_text_hash.as_deref(),
                                    20, // search_radius
                                );
                                // Convert result back from 0-based to 1-based (saturating to prevent overflow)
                                note.line = i64::from(pos.line).saturating_add(1);
                                note.stale = pos.stale;
                            }
                        }
                        // Compute formatted lines for all notes (always, for UI consistency)
                        // Use explicit language param if provided, otherwise derive from file
                        if let Some(lang) = language {
                            for note in &mut notes_list {
                                note.formatted_lines = Some(display::format_note_lines(
                                    &note.text,
                                    lang,
                                    wrap_width,
                                    note.stale,
                                ));
                            }
                        } else {
                            display::ensure_formatted_lines_all(&mut notes_list, wrap_width);
                        }
                        // Apply onlyStale filter after all staleness computation
                        let notes_list = if only_stale {
                            notes_list.into_iter().filter(|n| n.stale).collect()
                        } else {
                            notes_list
                        };
                        // Return wrapper with content hash if content was provided
                        let response = NotesListResponse {
                            notes: notes_list,
                            content_hash: content_param.map(content_hash),
                        };
                        Response::result_from(id, response)
                    }
                    Err(_) => Response::error(id, INTERNAL_ERROR, "operation failed"),
                }
            } else {
                Response::error(id, INVALID_PARAMS, "missing file")
            }
        }
        "notes/get-at-position" => {
            // Returns the note at a specific position in a file, or null if none
            let file = req.params.get("file").and_then(|v| v.as_str());
            let line = req.params.get("line").and_then(|v| v.as_i64());
            let content = req.params.get("content").and_then(|v| v.as_str());
            if let (Some(file), Some(target_line), Some(content)) = (file, line, content) {
                let proj_param = req.params.get("projectRoot").and_then(|v| v.as_str());
                let proj = resolve_project_root(proj_param, file);
                // Auto-fill git info
                let git_info = info_for_file(file);
                let commit = git_info.as_ref().map(|g| g.commit.clone());
                let blob = git_info.as_ref().and_then(|g| g.blob.clone());
                let filters = NoteFilters {
                    file,
                    project_root: &proj,
                    node_path: None,
                    commit: commit.as_deref(),
                    blob: blob.as_deref(),
                    include_stale: true,
                };
                match notes::list_for_file(db, filters) {
                    Ok(mut notes_list) => {
                        let file_path = Path::new(file);
                        // Compute display positions and find the matching note
                        for note in &mut notes_list {
                            let ts_line = user_to_ts_line(note.line);
                            let ts_column = note.column.clamp(0, i64::from(u32::MAX)) as u32;
                            let pos = compute_display_position(
                                parser,
                                file_path,
                                content,
                                ts_line,
                                ts_column,
                                note.node_text_hash.as_deref(),
                                20,
                            );
                            note.line = i64::from(pos.line).saturating_add(1);
                            note.stale = pos.stale;
                        }
                        // Find note at target line
                        let found = notes_list.into_iter().find(|n| n.line == target_line);
                        // Apply formatting if found
                        if let Some(mut note) = found {
                            display::ensure_formatted_lines(&mut note, None);
                            Response::result_from(id, Some(note))
                        } else {
                            Response::result_from(id, Option::<notes::Note>::None)
                        }
                    }
                    Err(_) => Response::error(id, INTERNAL_ERROR, "operation failed"),
                }
            } else {
                Response::error(id, INVALID_PARAMS, "missing file, line, or content")
            }
        }
        "notes/list-project" => {
            if let Some(proj) = req.params.get("projectRoot").and_then(|v| v.as_str()) {
                let limit = req
                    .params
                    .get("limit")
                    .and_then(|v| v.as_u64())
                    .map(|v| v as usize);
                let offset = req
                    .params
                    .get("offset")
                    .and_then(|v| v.as_u64())
                    .unwrap_or(0) as usize;
                match notes::list_project(db, proj, limit, offset) {
                    Ok(mut notes_list) => {
                        display::ensure_formatted_lines_all(&mut notes_list, None);
                        Response::result_from(id, notes_list)
                    }
                    Err(_) => Response::error(id, INTERNAL_ERROR, "operation failed"),
                }
            } else {
                Response::error(id, INVALID_PARAMS, "missing projectRoot")
            }
        }
        "notes/search" => {
            let query = req
                .params
                .get("query")
                .and_then(|v| v.as_str())
                .unwrap_or("");
            // projectRoot is optional - computed from file if not provided
            let proj_param = req.params.get("projectRoot").and_then(|v| v.as_str());
            let file_param = req.params.get("file").and_then(|v| v.as_str());
            let proj = proj_param
                .map(|s| s.to_string())
                .or_else(|| file_param.map(|f| resolve_project_root(None, f)));
            let limit = req
                .params
                .get("limit")
                .and_then(|v| v.as_u64())
                .map(|v| v as usize);
            let offset = req
                .params
                .get("offset")
                .and_then(|v| v.as_u64())
                .unwrap_or(0) as usize;
            match notes::search(db, query, proj.as_deref(), limit, offset) {
                Ok(mut notes_list) => {
                    display::ensure_formatted_lines_all(&mut notes_list, None);
                    Response::result_from(id, notes_list)
                }
                Err(_) => Response::error(id, INTERNAL_ERROR, "operation failed"),
            }
        }
        "notes/link-suggestions" => {
            // Returns note search results formatted as link suggestions
            let query = req
                .params
                .get("query")
                .and_then(|v| v.as_str())
                .unwrap_or("");
            let proj = req.params.get("projectRoot").and_then(|v| v.as_str());
            match notes::search(db, query, proj, Some(10), 0) {
                Ok(notes_list) => {
                    let suggestions: Vec<serde_json::Value> = notes_list
                        .into_iter()
                        .map(|n| {
                            let summary = n.text.lines().next().unwrap_or("").to_string();
                            let short_summary = if summary.len() > 50 {
                                format!("{}...", &summary[..47])
                            } else {
                                summary.clone()
                            };
                            serde_json::json!({
                                "noteId": n.id,
                                "summary": short_summary,
                                "formatted": format!("[[{}][{}]]", short_summary, n.id)
                            })
                        })
                        .collect();
                    Response::result_from(id, suggestions)
                }
                Err(_) => Response::error(id, INTERNAL_ERROR, "operation failed"),
            }
        }
        "notes/list-by-node" => {
            let file = req.params.get("file").and_then(|v| v.as_str());
            let node = req.params.get("nodePath").cloned();
            if let Some(file) = file {
                // projectRoot is now optional - computed from file if not provided
                let proj_param = req.params.get("projectRoot").and_then(|v| v.as_str());
                let proj = resolve_project_root(proj_param, file);
                // commit/blob are now optional - computed from file via git if not provided
                // Only auto-fill from git if NEITHER is explicitly provided (to allow staleness testing)
                let req_commit = req.params.get("commit").and_then(|v| v.as_str());
                let req_blob = req.params.get("blob").and_then(|v| v.as_str());
                let (commit, blob) = if req_commit.is_none() && req_blob.is_none() {
                    let git_info = info_for_file(file);
                    (
                        git_info.as_ref().map(|g| g.commit.clone()),
                        git_info.as_ref().and_then(|g| g.blob.clone()),
                    )
                } else {
                    (req_commit.map(String::from), req_blob.map(String::from))
                };
                let include_stale = req
                    .params
                    .get("includeStale")
                    .and_then(|v| v.as_bool())
                    .unwrap_or(false);
                let filters = NoteFilters {
                    file,
                    project_root: &proj,
                    node_path: node,
                    commit: commit.as_deref(),
                    blob: blob.as_deref(),
                    include_stale,
                };
                match notes::list_by_node(db, filters) {
                    Ok(mut notes_list) => {
                        display::ensure_formatted_lines_all(&mut notes_list, None);
                        Response::result_from(id, notes_list)
                    }
                    Err(_) => Response::error(id, INTERNAL_ERROR, "operation failed"),
                }
            } else {
                Response::error(id, INVALID_PARAMS, "missing file")
            }
        }
        "notes/create" => {
            let file = req.params.get("file").and_then(|v| v.as_str());
            if let Some(file) = file {
                // projectRoot is now optional - computed from file if not provided
                let proj_param = req.params.get("projectRoot").and_then(|v| v.as_str());
                let proj = resolve_project_root(proj_param, file);
                let line = req
                    .params
                    .get("line")
                    .and_then(|v| v.as_i64())
                    .map(|l| l.max(1)) // Clamp to >= 1
                    .unwrap_or(1);
                let column = req
                    .params
                    .get("column")
                    .and_then(|v| v.as_i64())
                    .unwrap_or(0)
                    .clamp(0, i64::from(u32::MAX)); // Clamp to valid u32 range
                let tags = req.params.get("tags").cloned().unwrap_or_else(|| json!([]));
                let text = req
                    .params
                    .get("text")
                    .and_then(|v| v.as_str())
                    .unwrap_or_default()
                    .trim();
                // If content is provided, compute anchor position, node_path, and hash server-side
                // This includes adjusting line/column to the significant node's start
                // Otherwise, use client-provided values (backwards compatibility)
                let content = req.params.get("content").and_then(|v| v.as_str());
                let (final_line, final_column, node_path, node_text_hash) = if let Some(content) = content {
                    let file_path = Path::new(file);
                    // Convert from 1-based (client) to 0-based (tree-sitter)
                    let ts_line = user_to_ts_line(line);
                    let ts_column = column as u32;
                    let anchor = compute_anchor_position(parser, file_path, content, ts_line, ts_column);
                    let node_path_value = if anchor.node_path.is_empty() {
                        None
                    } else {
                        serde_json::to_value(&anchor.node_path).ok()
                    };
                    // Convert back to 1-based for storage
                    let anchored_line = ts_to_user_line(anchor.line);
                    let anchored_column = i64::from(anchor.column);
                    (anchored_line, anchored_column, node_path_value, anchor.node_text_hash)
                } else {
                    // Backwards compatibility: use client-provided values
                    let node_path = req.params.get("nodePath").cloned();
                    let node_text_hash = req
                        .params
                        .get("nodeTextHash")
                        .and_then(|v| v.as_str())
                        .map(|s| s.to_string());
                    (line, column, node_path, node_text_hash)
                };
                let git = info_for_file(file);
                match notes::create(
                    db,
                    file,
                    &proj,
                    final_line,
                    final_column,
                    node_path,
                    tags,
                    text,
                    git,
                    node_text_hash,
                ) {
                    Ok(mut note) => {
                        // Emit note-created event
                        events::emit(Event::NoteCreated {
                            id: note.id.clone(),
                            file: file.to_string(),
                            line: final_line,
                            project_root: Some(proj.clone()),
                        });
                        display::ensure_formatted_lines(&mut note, None);
                        Response::result_from(id, note)
                    }
                    Err(_) => Response::error(id, INTERNAL_ERROR, "operation failed"),
                }
            } else {
                Response::error(id, INVALID_PARAMS, "missing file")
            }
        }
        "notes/explain-and-create" => {
            // Combined AI explain + note creation in one RPC call
            // UIs can trigger this for "explain this code and create a note" flow
            let file = req.params.get("file").and_then(|v| v.as_str());
            let content = req.params.get("content").and_then(|v| v.as_str());
            let start_line = req.params.get("startLine").and_then(|v| v.as_u64()).unwrap_or(1);
            let end_line = req.params.get("endLine").and_then(|v| v.as_u64()).unwrap_or(start_line);
            let detailed = req.params.get("detailed").and_then(|v| v.as_bool()).unwrap_or(false);

            match (file, content) {
                (Some(file), Some(content)) => {
                    // Validate line range
                    if start_line == 0 || end_line < start_line {
                        return Response::error(id, INVALID_PARAMS, "invalid line range");
                    }
                    let start_line_usize = start_line as usize;
                    let end_line_usize = end_line as usize;

                    // Extract snippet from content
                    let snippet: String = content.lines()
                        .enumerate()
                        .filter(|(idx, _)| {
                            let line_no = idx + 1;
                            line_no >= start_line_usize && line_no <= end_line_usize
                        })
                        .map(|(_, l)| l)
                        .collect::<Vec<_>>()
                        .join("\n");

                    if snippet.is_empty() {
                        return Response::error(id, INVALID_PARAMS, "empty line range");
                    }

                    // Get project root
                    let proj_param = req.params.get("projectRoot").and_then(|v| v.as_str());
                    let proj = resolve_project_root(proj_param, file);
                    let root_path = Path::new(&proj);

                    // Get AI explanation
                    let (explanation, ai_info) = match ai_cli::explain_region(root_path, file, start_line_usize, end_line_usize, &snippet, detailed) {
                        Ok((provider, explanation, had_context)) => {
                            (explanation, json!({
                                "provider": provider.as_str(),
                                "hadContext": had_context
                            }))
                        }
                        Err(e) => {
                            return Response::result(id, json!({
                                "note": null,
                                "ai": { "error": e.to_string() }
                            }));
                        }
                    };

                    // Compute anchor position for start of region
                    let ts_line = user_to_ts_line(start_line as i64);
                    let anchor = compute_anchor_position(parser, Path::new(file), content, ts_line, 0);
                    let node_path_value = if anchor.node_path.is_empty() {
                        None
                    } else {
                        serde_json::to_value(&anchor.node_path).ok()
                    };
                    let anchored_line = ts_to_user_line(anchor.line);
                    let anchored_column = i64::from(anchor.column);

                    // Create the note with the AI explanation
                    let git = info_for_file(file);
                    match notes::create(
                        db,
                        file,
                        &proj,
                        anchored_line,
                        anchored_column,
                        node_path_value,
                        json!([]),
                        &explanation,
                        git,
                        anchor.node_text_hash,
                    ) {
                        Ok(mut note) => {
                            events::emit(Event::NoteCreated {
                                id: note.id.clone(),
                                file: file.to_string(),
                                line: anchored_line,
                                project_root: Some(proj.clone()),
                            });
                            display::ensure_formatted_lines(&mut note, None);
                            Response::result(id, json!({
                                "note": note,
                                "ai": ai_info
                            }))
                        }
                        Err(_) => Response::error(id, INTERNAL_ERROR, "failed to create note"),
                    }
                }
                _ => Response::error(id, INVALID_PARAMS, "missing file or content")
            }
        }
        "notes/delete" => {
            if let Some(note_id) = req.params.get("id").and_then(|v| v.as_str()) {
                match notes::delete(db, note_id) {
                    Ok(ok) => {
                        if ok {
                            events::emit(Event::NoteDeleted {
                                id: note_id.to_string(),
                            });
                        }
                        Response::result(id, json!({"ok": ok}))
                    }
                    Err(_) => Response::error(id, INTERNAL_ERROR, "operation failed"),
                }
            } else {
                Response::error(id, INVALID_PARAMS, "missing id")
            }
        }
        "notes/update" => {
            if let Some(note_id) = req.params.get("id").and_then(|v| v.as_str()) {
                let text = req.params.get("text").and_then(|v| v.as_str()).map(str::trim);
                let tags = req.params.get("tags").cloned();

                // Save current state as version before updating
                if let Ok(current) = notes::get(db, note_id) {
                    let node_path_str = current
                        .node_path
                        .as_ref()
                        .and_then(|np| serde_json::to_string(np).ok());
                    let _ = storage::save_note_version(
                        db,
                        note_id,
                        Some(&current.text),
                        current.line,
                        current.column,
                        node_path_str.as_deref(),
                        current.commit_sha.as_deref(),
                        current.blob_sha.as_deref(),
                        Some("update"),
                    );
                }

                match notes::update(db, note_id, text, tags) {
                    Ok(mut note) => {
                        events::emit(Event::NoteUpdated {
                            id: note_id.to_string(),
                        });
                        display::ensure_formatted_lines(&mut note, None);
                        Response::result_from(id, note)
                    }
                    Err(_) => Response::error(id, INTERNAL_ERROR, "operation failed"),
                }
            } else {
                Response::error(id, INVALID_PARAMS, "missing id")
            }
        }
        "notes/get" => {
            if let Some(note_id) = req.params.get("id").and_then(|v| v.as_str()) {
                match notes::get(db, note_id) {
                    Ok(mut note) => {
                        display::ensure_formatted_lines(&mut note, None);
                        Response::result_from(id, note)
                    }
                    // "not found" is safe to expose (no sensitive details)
                    Err(e) if e.to_string().contains("not found") => {
                        Response::error(id, INVALID_PARAMS, "note not found")
                    }
                    Err(_) => Response::error(id, INTERNAL_ERROR, "operation failed"),
                }
            } else {
                Response::error(id, INVALID_PARAMS, "missing id")
            }
        }
        "notes/backlinks" => {
            if let Some(note_id) = req.params.get("id").and_then(|v| v.as_str()) {
                match notes::backlinks(db, note_id) {
                    Ok(mut notes_list) => {
                        display::ensure_formatted_lines_all(&mut notes_list, None);
                        Response::result_from(id, notes_list)
                    }
                    Err(_) => Response::error(id, INTERNAL_ERROR, "operation failed"),
                }
            } else {
                Response::error(id, INVALID_PARAMS, "missing id")
            }
        }
        "notes/history" => {
            // Get version history for a note
            if let Some(note_id) = req.params.get("id").and_then(|v| v.as_str()) {
                let limit = req
                    .params
                    .get("limit")
                    .and_then(|v| v.as_u64())
                    .map(|v| v as usize)
                    .unwrap_or(50);
                match storage::list_note_versions(db, note_id, limit) {
                    Ok(versions) => Response::result_from(id, versions),
                    Err(_) => Response::error(id, INTERNAL_ERROR, "operation failed"),
                }
            } else {
                Response::error(id, INVALID_PARAMS, "missing id")
            }
        }
        "notes/get-version" => {
            // Get a specific version of a note
            let note_id = req.params.get("id").and_then(|v| v.as_str());
            let version = req.params.get("version").and_then(|v| v.as_i64());
            if let (Some(note_id), Some(version)) = (note_id, version) {
                match storage::get_note_version(db, note_id, version) {
                    Ok(Some(v)) => Response::result_from(id, v),
                    Ok(None) => Response::error(id, INVALID_PARAMS, "version not found"),
                    Err(_) => Response::error(id, INTERNAL_ERROR, "operation failed"),
                }
            } else {
                Response::error(id, INVALID_PARAMS, "missing id or version")
            }
        }
        "notes/restore-version" => {
            // Restore a note to a previous version
            let note_id = req.params.get("id").and_then(|v| v.as_str());
            let version = req.params.get("version").and_then(|v| v.as_i64());
            if let (Some(note_id), Some(version)) = (note_id, version) {
                // First get the version to restore
                match storage::get_note_version(db, note_id, version) {
                    Ok(Some(v)) => {
                        // Save current state as a new version before restoring
                        if let Ok(current) = notes::get(db, note_id) {
                            let node_path_str = current
                                .node_path
                                .as_ref()
                                .and_then(|np| serde_json::to_string(np).ok());
                            let _ = storage::save_note_version(
                                db,
                                note_id,
                                Some(&current.text),
                                current.line,
                                current.column,
                                node_path_str.as_deref(),
                                current.commit_sha.as_deref(),
                                current.blob_sha.as_deref(),
                                Some("before_restore"),
                            );
                        }
                        // Update note with version data
                        match notes::update(db, note_id, v.text.as_deref(), None) {
                            Ok(mut note) => {
                                events::emit(Event::NoteUpdated {
                                    id: note_id.to_string(),
                                });
                                display::ensure_formatted_lines(&mut note, None);
                                Response::result_from(id, note)
                            }
                            Err(_) => Response::error(id, INTERNAL_ERROR, "restore failed"),
                        }
                    }
                    Ok(None) => Response::error(id, INVALID_PARAMS, "version not found"),
                    Err(_) => Response::error(id, INTERNAL_ERROR, "operation failed"),
                }
            } else {
                Response::error(id, INVALID_PARAMS, "missing id or version")
            }
        }
        "notes/reattach" => {
            let note_id = req.params.get("id").and_then(|v| v.as_str());
            let file = req.params.get("file").and_then(|v| v.as_str());
            if let Some(note_id) = note_id {
                let line = req
                    .params
                    .get("line")
                    .and_then(|v| v.as_i64())
                    .map(|l| l.max(1)) // Clamp to >= 1
                    .unwrap_or(1);
                let column = req
                    .params
                    .get("column")
                    .and_then(|v| v.as_i64())
                    .unwrap_or(0)
                    .clamp(0, i64::from(u32::MAX)); // Clamp to valid u32 range
                // If content is provided, compute anchor position, node_path and hash server-side
                let content = req.params.get("content").and_then(|v| v.as_str());
                let (final_line, final_column, node_path, node_text_hash) = if let (Some(content), Some(file)) = (content, file) {
                    let file_path = Path::new(file);
                    // Convert from 1-based (client) to 0-based (tree-sitter)
                    let ts_line = user_to_ts_line(line);
                    let ts_column = column as u32;
                    let anchor = compute_anchor_position(parser, file_path, content, ts_line, ts_column);
                    let node_path_value = if anchor.node_path.is_empty() {
                        None
                    } else {
                        serde_json::to_value(&anchor.node_path).ok()
                    };
                    // Convert back to 1-based for storage
                    let anchored_line = ts_to_user_line(anchor.line);
                    let anchored_column = i64::from(anchor.column);
                    (anchored_line, anchored_column, node_path_value, anchor.node_text_hash)
                } else {
                    // Backwards compatibility
                    let node_path = req.params.get("nodePath").cloned();
                    let node_text_hash = req
                        .params
                        .get("nodeTextHash")
                        .and_then(|v| v.as_str())
                        .map(|s| s.to_string());
                    (line, column, node_path, node_text_hash)
                };
                let git = file.and_then(info_for_file);
                match notes::reattach(db, note_id, final_line, final_column, node_path, git, node_text_hash) {
                    Ok(mut note) => {
                        display::ensure_formatted_lines(&mut note, None);
                        Response::result_from(id, note)
                    }
                    // "not found" is safe to expose (no sensitive details)
                    Err(e) if e.to_string().contains("not found") => {
                        Response::error(id, INVALID_PARAMS, "note not found")
                    }
                    Err(_) => Response::error(id, INTERNAL_ERROR, "operation failed"),
                }
            } else {
                Response::error(id, INVALID_PARAMS, "missing id")
            }
        }
        "notes/buffer-update" => {
            // Real-time position tracking: recompute positions for all notes in file
            let file = req.params.get("file").and_then(|v| v.as_str());
            let content = req.params.get("content").and_then(|v| v.as_str());
            if let (Some(file), Some(content)) = (file, content) {
                // Get all notes for this file
                let filters = NoteFilters {
                    file,
                    project_root: "",
                    node_path: None,
                    commit: None,
                    blob: None,
                    include_stale: true,
                };
                match notes::list_for_file(db, filters) {
                    Ok(mut notes_list) => {
                        let file_path = Path::new(file);
                        for note in &mut notes_list {
                            let old_line = note.line;
                            let old_stale = note.stale;
                            // Convert from 1-based (database) to 0-based (tree-sitter)
                            let ts_line = user_to_ts_line(note.line);
                            let ts_column = note.column.clamp(0, i64::from(u32::MAX)) as u32;
                            let pos = compute_display_position(
                                parser,
                                file_path,
                                content,
                                ts_line,
                                ts_column,
                                note.node_text_hash.as_deref(),
                                20,
                            );
                            // Convert result back from 0-based to 1-based (saturating to prevent overflow)
                            let new_line = i64::from(pos.line).saturating_add(1);
                            note.line = new_line;
                            note.stale = pos.stale;

                            // Emit event if position or staleness changed
                            if new_line != old_line || pos.stale != old_stale {
                                events::emit(Event::NotePositionChanged {
                                    id: note.id.clone(),
                                    file: file.to_string(),
                                    old_line,
                                    new_line,
                                    stale: pos.stale,
                                });
                            }
                        }
                        // Ensure formatted lines for all notes
                        display::ensure_formatted_lines_all(&mut notes_list, None);
                        // Return wrapper with content hash
                        let response = NotesListResponse {
                            notes: notes_list,
                            content_hash: Some(content_hash(content)),
                        };
                        Response::result_from(id, response)
                    }
                    Err(_) => Response::error(id, INTERNAL_ERROR, "operation failed"),
                }
            } else {
                Response::error(id, INVALID_PARAMS, "missing file/content")
            }
        }
        "index/add-file" => {
            let file = req.params.get("file").and_then(|v| v.as_str());
            let content = req
                .params
                .get("content")
                .and_then(|v| v.as_str())
                .unwrap_or("");
            if let Some(file) = file {
                // projectRoot is now optional - computed from file if not provided
                let proj_param = req.params.get("projectRoot").and_then(|v| v.as_str());
                let proj = resolve_project_root(proj_param, file);
                match idx::add_file(db, file, &proj, content) {
                    Ok(info) => Response::result_from(id, info),
                    Err(_) => Response::error(id, INTERNAL_ERROR, "operation failed"),
                }
            } else {
                Response::error(id, INVALID_PARAMS, "missing file")
            }
        }
        "index/search" => {
            let query = req
                .params
                .get("query")
                .and_then(|v| v.as_str())
                .unwrap_or("");
            let proj = req.params.get("projectRoot").and_then(|v| v.as_str());
            match idx::search(db, query, proj) {
                Ok(results) => Response::result_from(id, results),
                Err(_) => Response::error(id, INTERNAL_ERROR, "operation failed"),
            }
        }
        "hemis/summarize-file" => {
            // Return file summary with stitched code snippets, note references, and backlinks
            let file = req.params.get("file").and_then(|v| v.as_str());
            let content = req.params.get("content").and_then(|v| v.as_str());
            if let (Some(file), Some(content)) = (file, content) {
                let proj_param = req.params.get("projectRoot").and_then(|v| v.as_str());
                let proj = resolve_project_root(proj_param, file);

                // Get all notes for this file
                let filters = NoteFilters {
                    file,
                    project_root: &proj,
                    node_path: None,
                    commit: None,
                    blob: None,
                    include_stale: true,
                };
                let notes_list = notes::list_for_file(db, filters).unwrap_or_default();

                // Build sections with code snippets around each note
                let mut sections: Vec<serde_json::Value> = Vec::new();
                let lines: Vec<&str> = content.lines().collect();

                for note in &notes_list {
                    let line_idx = (note.line as usize).saturating_sub(1);
                    let context_before = 2;
                    let context_after = 2;
                    let start = line_idx.saturating_sub(context_before);
                    let end = (line_idx + context_after + 1).min(lines.len());

                    let snippet_lines: Vec<String> = lines[start..end]
                        .iter()
                        .enumerate()
                        .map(|(i, line)| {
                            let line_num = start + i + 1;
                            format!("{:4} | {}", line_num, line)
                        })
                        .collect();

                    // Get backlinks for this note
                    let backlinks = notes::backlinks(db, &note.id).unwrap_or_default();
                    let backlink_refs: Vec<serde_json::Value> = backlinks
                        .iter()
                        .map(|bl| {
                            json!({
                                "noteId": bl.id,
                                "summary": bl.summary,
                                "file": bl.file
                            })
                        })
                        .collect();

                    sections.push(json!({
                        "noteId": note.id,
                        "line": note.line,
                        "summary": note.summary,
                        "text": note.text,
                        "stale": note.stale,
                        "codeSnippet": snippet_lines.join("\n"),
                        "backlinks": backlink_refs
                    }));
                }

                // AI summary not implemented yet - would come from file analysis
                let ai_summary: Option<String> = None;

                let result = json!({
                    "file": file,
                    "projectRoot": proj,
                    "noteCount": notes_list.len(),
                    "staleCount": notes_list.iter().filter(|n| n.stale).count(),
                    "sections": sections,
                    "aiSummary": ai_summary
                });

                Response::result(id, result)
            } else {
                Response::error(id, INVALID_PARAMS, "missing file or content")
            }
        }
        "hemis/task-status" => {
            // Get status of a background task by ID
            if let Some(task_id) = req.params.get("taskId").and_then(|v| v.as_str()) {
                match jobs::job_queue().get_status(task_id) {
                    Some(info) => Response::result_from(id, info),
                    None => Response::error(id, INVALID_PARAMS, "task not found"),
                }
            } else {
                Response::error(id, INVALID_PARAMS, "missing taskId")
            }
        }
        "hemis/task-list" => {
            // List all tasks (optionally filtered by status)
            let status_filter = req.params.get("status").and_then(|v| v.as_str());
            let tasks = jobs::job_queue().list_tasks(status_filter);
            Response::result_from(id, tasks)
        }
        "shutdown" => Response::result(id, json!("shutting down")),
        _ => Response::error(
            id,
            METHOD_NOT_FOUND,
            format!("unknown method: {}", req.method),
        ),
    }
}

pub fn parse_and_handle(buf: &[u8], conn: &Connection, parser: &mut ParserService) -> Response {
    match rpc::parse_request(buf) {
        Ok(req) => handle(req, conn, parser),
        Err(_) => Response::error(None, PARSE_ERROR, "parse error"),
    }
}

/// Create a new ParserService with default configuration
pub fn create_parser_service() -> ParserService {
    let registry = GrammarRegistry::new(default_config());
    ParserService::new(registry)
}

#[cfg(test)]
mod tests {
    use super::*;
    use rusqlite::Connection;

    #[test]
    fn returns_parse_error_on_invalid_json() {
        let conn = Connection::open_in_memory().unwrap();
        let mut parser = create_parser_service();
        let resp = parse_and_handle(b"not json", &conn, &mut parser);
        assert!(resp.result.is_none());
        assert_eq!(resp.error.as_ref().unwrap().code, PARSE_ERROR);
    }

    #[test]
    fn shutdown_round_trip_succeeds() {
        let conn = Connection::open_in_memory().unwrap();
        let mut parser = create_parser_service();
        let resp = parse_and_handle(
            br#"{"jsonrpc":"2.0","id":1,"method":"shutdown","params":{}}"#,
            &conn,
            &mut parser,
        );
        assert_eq!(resp.result, Some(json!("shutting down")));
        assert!(resp.error.is_none());
    }
}
