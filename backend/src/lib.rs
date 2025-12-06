use anyhow::Context;
use git::info_for_file;
use index as idx;
use notes::{self, NoteFilters};
use rpc::{Request, Response, INTERNAL_ERROR, INVALID_PARAMS, METHOD_NOT_FOUND, PARSE_ERROR};
use rusqlite::Connection;
use serde_json::json;
use std::fs::{self, File};
use std::io::{BufRead, BufReader};
use std::path::Path;
use treesitter::{
    compute_anchor_position, compute_display_position, default_config, GrammarRegistry,
    ParserService,
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
                                    Response::result(id, json!({
                                        "content": snippet,
                                        "explanation": explanation,
                                        "references": [],
                                        "ai": {
                                            "provider": provider.as_str(),
                                            "hadContext": had_context
                                        }
                                    }))
                                }
                                Err(e) => {
                                    // AI failed, return snippet with error info
                                    Response::result(id, json!({
                                        "content": snippet,
                                        "references": [],
                                        "ai": {
                                            "error": e.to_string()
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
            let proj = req.params.get("projectRoot").and_then(|v| v.as_str());
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
            match idx::search(db, query, proj) {
                Ok(mut results) => {
                    // If a query vector is provided, blend semantic hits.
                    if let Some(vec) = query_vec {
                        if let Ok(mut semantic_hits) = idx::semantic_search(db, &vec, proj, 5) {
                            results.append(&mut semantic_hits);
                        }
                    }
                    if include_notes {
                        if let Ok(notes) = notes::search(db, query, proj, None, 0) {
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
            if let Some(root) = req.params.get("projectRoot").and_then(|v| v.as_str()) {
                let mut indexed = 0;
                let mut skipped = 0;
                let include_ai = req
                    .params
                    .get("includeAI")
                    .and_then(|v| v.as_bool())
                    .unwrap_or(false);
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
                                    Err(e) => eprintln!("index failed for {}: {}", f.file, e),
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
                                    eprintln!("[hemis] Claude warm-up failed: {}", e);
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

                        let mut result = json!({
                            "ok": true,
                            "indexed": indexed,
                            "skipped": skipped,
                            "truncated": truncated,
                            "projectRoot": root
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
                Ok(c) => Response::result(
                    id,
                    json!({
                        "ok": true,
                        "projectRoot": proj,
                        "counts": {
                            "notes": c.notes,
                            "files": c.files,
                            "embeddings": c.embeddings,
                            "edges": c.edges
                        }
                    }),
                ),
                Err(_) => Response::error(id, INTERNAL_ERROR, "operation failed"),
            }
        }
        "hemis/project-meta" => {
            if let Some(proj) = req.params.get("projectRoot").and_then(|v| v.as_str()) {
                match storage::get_project_meta(db, proj) {
                    Ok(Some(meta)) => {
                        // Check if analysis is stale by comparing to current HEAD
                        let current_commit = git::head_commit(Path::new(proj));
                        let analysis_stale = meta.analysis_commit_sha.as_ref()
                            .map(|sha| current_commit.as_ref() != Some(sha))
                            .unwrap_or(true);
                        let has_analysis = ai_cli::has_analysis(Path::new(proj));
                        let ai_available = ai_cli::CliProvider::from_env().is_some();
                        Response::result(id, json!({
                            "projectRoot": proj,
                            "indexed": meta.indexed_at.is_some(),
                            "indexedAt": meta.indexed_at,
                            "indexedCommit": meta.indexed_commit_sha,
                            "analyzed": meta.analyzed_at.is_some(),
                            "analyzedAt": meta.analyzed_at,
                            "analysisCommit": meta.analysis_commit_sha,
                            "analysisProvider": meta.analysis_provider,
                            "analysisStale": analysis_stale,
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
                        Response::result(id, json!({
                            "projectRoot": proj,
                            "indexed": false,
                            "analyzed": false,
                            "analysisStale": true,
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
                match notes::list_for_file(db, filters) {
                    Ok(mut notes_list) => {
                        // If content is provided, compute display positions server-side
                        if let Some(content) = req.params.get("content").and_then(|v| v.as_str()) {
                            let file_path = Path::new(file);
                            for note in &mut notes_list {
                                // Convert from 1-based (database) to 0-based (tree-sitter)
                                let ts_line = (note.line - 1).max(0) as u32;
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
                        // If language is provided, compute formatted lines server-side
                        // Uses file extension as fallback if no language specified
                        let lang = language
                            .map(String::from)
                            .or_else(|| {
                                Path::new(file)
                                    .extension()
                                    .and_then(|e| e.to_str())
                                    .map(String::from)
                            });
                        if let Some(lang) = lang {
                            for note in &mut notes_list {
                                note.formatted_lines = Some(display::format_note_lines(
                                    &note.text,
                                    &lang,
                                    wrap_width,
                                    note.stale,
                                ));
                            }
                        }
                        Response::result_from(id, notes_list)
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
                            let ts_line = (note.line - 1).max(0) as u32;
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
                            let lang = Path::new(file)
                                .extension()
                                .and_then(|e| e.to_str())
                                .map(String::from);
                            if let Some(lang) = lang {
                                note.formatted_lines =
                                    Some(display::format_note_lines(&note.text, &lang, None, note.stale));
                            }
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
                    Ok(n) => Response::result_from(id, n),
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
            let proj = req.params.get("projectRoot").and_then(|v| v.as_str());
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
            match notes::search(db, query, proj, limit, offset) {
                Ok(n) => Response::result_from(id, n),
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
                    Ok(n) => Response::result_from(id, n),
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
                    .unwrap_or_default();
                // If content is provided, compute anchor position, node_path, and hash server-side
                // This includes adjusting line/column to the significant node's start
                // Otherwise, use client-provided values (backwards compatibility)
                let content = req.params.get("content").and_then(|v| v.as_str());
                let (final_line, final_column, node_path, node_text_hash) = if let Some(content) = content {
                    let file_path = Path::new(file);
                    // Convert from 1-based (client) to 0-based (tree-sitter)
                    let ts_line = (line - 1).max(0) as u32;
                    let ts_column = column as u32;
                    let anchor = compute_anchor_position(parser, file_path, content, ts_line, ts_column);
                    let node_path_value = if anchor.node_path.is_empty() {
                        None
                    } else {
                        serde_json::to_value(&anchor.node_path).ok()
                    };
                    // Convert back to 1-based for storage
                    let anchored_line = i64::from(anchor.line) + 1;
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
                    Ok(n) => {
                        // Emit note-created event
                        events::emit(Event::NoteCreated {
                            id: n.id.clone(),
                            file: file.to_string(),
                            line: final_line,
                            project_root: Some(proj.clone()),
                        });
                        Response::result_from(id, n)
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
                    let ts_line = (start_line - 1) as u32;
                    let anchor = compute_anchor_position(parser, Path::new(file), content, ts_line, 0);
                    let node_path_value = if anchor.node_path.is_empty() {
                        None
                    } else {
                        serde_json::to_value(&anchor.node_path).ok()
                    };
                    let anchored_line = i64::from(anchor.line) + 1;
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
                        Ok(n) => {
                            events::emit(Event::NoteCreated {
                                id: n.id.clone(),
                                file: file.to_string(),
                                line: anchored_line,
                                project_root: Some(proj.clone()),
                            });
                            Response::result(id, json!({
                                "note": n,
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
                let text = req.params.get("text").and_then(|v| v.as_str());
                let tags = req.params.get("tags").cloned();
                match notes::update(db, note_id, text, tags) {
                    Ok(n) => {
                        events::emit(Event::NoteUpdated {
                            id: note_id.to_string(),
                        });
                        Response::result_from(id, n)
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
                    Ok(n) => Response::result_from(id, n),
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
                    Ok(n) => Response::result_from(id, n),
                    Err(_) => Response::error(id, INTERNAL_ERROR, "operation failed"),
                }
            } else {
                Response::error(id, INVALID_PARAMS, "missing id")
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
                    let ts_line = (line - 1).max(0) as u32;
                    let ts_column = column as u32;
                    let anchor = compute_anchor_position(parser, file_path, content, ts_line, ts_column);
                    let node_path_value = if anchor.node_path.is_empty() {
                        None
                    } else {
                        serde_json::to_value(&anchor.node_path).ok()
                    };
                    // Convert back to 1-based for storage
                    let anchored_line = i64::from(anchor.line) + 1;
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
                    Ok(n) => Response::result_from(id, n),
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
                            // Convert from 1-based (database) to 0-based (tree-sitter)
                            let ts_line = (note.line - 1).max(0) as u32;
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
                            note.line = i64::from(pos.line).saturating_add(1);
                            note.stale = pos.stale;
                        }
                        Response::result_from(id, notes_list)
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
