//! Display formatting for notes
//!
//! Provides server-side note formatting including:
//! - Comment prefix lookup by language/file extension
//! - Text wrapping at specified width

use std::path::Path;

/// Get comment prefix for a language or file extension
pub fn comment_prefix(lang_or_ext: &str) -> &'static str {
    match lang_or_ext.to_lowercase().as_str() {
        // Common languages
        "rust" | "rs" => "//",
        "go" => "//",
        "javascript" | "js" => "//",
        "typescript" | "ts" | "tsx" => "//",
        "java" => "//",
        "c" | "h" => "//",
        "cpp" | "cc" | "cxx" | "hpp" => "//",
        "csharp" | "cs" => "//",
        "swift" => "//",
        "kotlin" | "kt" => "//",
        "scala" => "//",
        "dart" => "//",
        "php" => "//",
        "scss" | "sass" => "//",

        // Hash-style comments
        "python" | "py" => "#",
        "ruby" | "rb" => "#",
        "shell" | "sh" | "bash" | "zsh" => "#",
        "yaml" | "yml" => "#",
        "toml" => "#",
        "perl" | "pl" => "#",
        "r" => "#",
        "makefile" | "make" => "#",
        "dockerfile" => "#",
        "gitignore" => "#",

        // Double-dash comments
        "lua" => "--",
        "sql" => "--",
        "haskell" | "hs" => "--",
        "elm" => "--",
        "ada" => "--",

        // Semicolon comments
        "lisp" | "el" | "clj" | "clojure" => ";",
        "scheme" | "scm" => ";",
        "elisp" | "emacs-lisp" => ";",
        "asm" | "s" => ";",

        // Special cases
        "vim" | "vimscript" => "\"",
        "latex" | "tex" => "%",
        "matlab" | "m" => "%",
        "erlang" | "erl" => "%",
        "fortran" | "f90" | "f95" => "!",

        // Default to C-style
        _ => "//",
    }
}

/// Get comment prefix from file path (uses extension)
pub fn comment_prefix_for_file(path: &Path) -> &'static str {
    path.extension()
        .and_then(|e| e.to_str())
        .map(comment_prefix)
        .unwrap_or("//")
}

/// Wrap text at specified width (in characters), breaking at word boundaries
pub fn wrap_text(text: &str, width: usize) -> Vec<String> {
    if width == 0 {
        return vec![text.to_string()];
    }

    let mut lines = Vec::new();

    for paragraph in text.lines() {
        if paragraph.trim().is_empty() {
            continue; // Skip empty lines
        }

        let char_count = paragraph.chars().count();
        if char_count <= width {
            lines.push(paragraph.to_string());
            continue;
        }

        let mut remaining = paragraph;
        while !remaining.is_empty() {
            let rem_chars = remaining.chars().count();
            if rem_chars <= width {
                lines.push(remaining.to_string());
                break;
            }

            // Find the byte index of the width-th character
            let width_byte_idx = remaining
                .char_indices()
                .nth(width)
                .map(|(i, _)| i)
                .unwrap_or(remaining.len());

            // Find last space within width (using byte slice up to char boundary)
            let slice = &remaining[..width_byte_idx];
            let break_pos = slice.rfind(' ').unwrap_or(width_byte_idx);

            let (line, rest) = remaining.split_at(break_pos);
            lines.push(line.trim_end().to_string());
            remaining = rest.trim_start();
        }
    }

    lines
}

/// Format note text as comment lines
///
/// Returns lines prefixed with the comment syntax for the given language,
/// wrapped to the specified width. Optionally indented to match source code.
pub fn format_note_lines(
    text: &str,
    lang_or_ext: &str,
    wrap_width: Option<usize>,
    stale: bool,
) -> Vec<String> {
    format_note_lines_with_indent(text, lang_or_ext, wrap_width, stale, 0)
}

/// Format note text as comment lines with indentation
///
/// Returns lines prefixed with indentation + comment syntax for the given language,
/// wrapped to the specified width.
pub fn format_note_lines_with_indent(
    text: &str,
    lang_or_ext: &str,
    wrap_width: Option<usize>,
    stale: bool,
    indent: usize,
) -> Vec<String> {
    let indent_str = " ".repeat(indent);
    let prefix = format!("{}{} ", indent_str, comment_prefix(lang_or_ext));
    let width = wrap_width.unwrap_or(70);
    let effective_width = width.saturating_sub(prefix.len());

    let stale_suffix = if stale { " [STALE]" } else { "" };

    let mut result = Vec::new();
    let wrapped = wrap_text(text, effective_width);
    let last_idx = wrapped.len().saturating_sub(1);

    for (i, line) in wrapped.into_iter().enumerate() {
        if i == last_idx {
            result.push(format!("{}{}{}", prefix, line, stale_suffix));
        } else {
            result.push(format!("{}{}", prefix, line));
        }
    }

    if result.is_empty() {
        result.push(format!("{}{}", prefix, stale_suffix.trim()));
    }

    result
}

/// Ensure a note has formatted_lines set.
/// Uses the file extension to determine comment syntax.
/// Uses the note's column for indentation (to match source code alignment).
/// Default width of 70 characters if no wrap_width specified.
pub fn ensure_formatted_lines(note: &mut notes::Note, wrap_width: Option<usize>) {
    if note.formatted_lines.is_some() {
        return;
    }
    let ext = Path::new(&note.file)
        .extension()
        .and_then(|e| e.to_str())
        .unwrap_or("txt");
    // Use note's column as indentation (column is 0-indexed)
    let indent = note.column.max(0) as usize;
    note.formatted_lines = Some(format_note_lines_with_indent(
        &note.text,
        ext,
        wrap_width,
        note.stale,
        indent,
    ));
}

/// Ensure all notes in a list have formatted_lines set.
pub fn ensure_formatted_lines_all(notes: &mut [notes::Note], wrap_width: Option<usize>) {
    for note in notes {
        ensure_formatted_lines(note, wrap_width);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck_macros::quickcheck;

    #[test]
    fn test_comment_prefix() {
        assert_eq!(comment_prefix("rust"), "//");
        assert_eq!(comment_prefix("python"), "#");
        assert_eq!(comment_prefix("lua"), "--");
        assert_eq!(comment_prefix("elisp"), ";");
        assert_eq!(comment_prefix("unknown"), "//");
    }

    #[test]
    fn test_comment_prefix_for_file() {
        assert_eq!(comment_prefix_for_file(Path::new("foo.rs")), "//");
        assert_eq!(comment_prefix_for_file(Path::new("bar.py")), "#");
        assert_eq!(comment_prefix_for_file(Path::new("script.lua")), "--");
    }

    #[test]
    fn test_wrap_text() {
        let text = "This is a long line that should be wrapped at word boundaries";
        let wrapped = wrap_text(text, 20);
        assert!(wrapped.iter().all(|l| l.len() <= 20));
        assert!(wrapped.len() > 1);
    }

    #[test]
    fn test_wrap_short_text() {
        let text = "Short";
        let wrapped = wrap_text(text, 20);
        assert_eq!(wrapped, vec!["Short"]);
    }

    #[test]
    fn test_format_note_lines() {
        let lines = format_note_lines("Hello world", "rust", Some(80), false);
        assert_eq!(lines, vec!["// Hello world"]);

        let lines = format_note_lines("Stale note", "python", Some(80), true);
        assert_eq!(lines, vec!["# Stale note [STALE]"]);
    }

    #[test]
    fn test_format_note_lines_with_indent() {
        // No indent (column 0)
        let lines = format_note_lines_with_indent("Hello", "rust", Some(80), false, 0);
        assert_eq!(lines, vec!["// Hello"]);

        // 4-space indent (typical function body)
        let lines = format_note_lines_with_indent("Hello", "rust", Some(80), false, 4);
        assert_eq!(lines, vec!["    // Hello"]);

        // 8-space indent (nested block)
        let lines = format_note_lines_with_indent("Hello", "rust", Some(80), false, 8);
        assert_eq!(lines, vec!["        // Hello"]);

        // With Python (hash comment)
        let lines = format_note_lines_with_indent("Hello", "python", Some(80), false, 4);
        assert_eq!(lines, vec!["    # Hello"]);

        // Multiline with indent
        let lines = format_note_lines_with_indent("Line one\nLine two", "rust", Some(80), false, 4);
        assert_eq!(lines, vec!["    // Line one", "    // Line two"]);

        // Stale with indent
        let lines = format_note_lines_with_indent("Stale", "rust", Some(80), true, 4);
        assert_eq!(lines, vec!["    // Stale [STALE]"]);
    }

    #[test]
    fn test_format_multiline() {
        let text = "Line one\nLine two";
        let lines = format_note_lines(text, "rust", Some(80), false);
        assert_eq!(lines, vec!["// Line one", "// Line two"]);
    }

    // Property-based tests

    #[quickcheck]
    fn prop_wrap_text_respects_width(text: String, width: u8) -> bool {
        let width = width as usize;
        if width == 0 {
            // Width 0 returns text as-is
            return true;
        }
        let wrapped = wrap_text(&text, width);
        // All lines should be <= width chars (words may exceed if no break point)
        wrapped.iter().all(|line| {
            let char_count = line.chars().count();
            // If line exceeds width, it must be a single word (no spaces to break on)
            char_count <= width || !line.contains(' ')
        })
    }

    #[quickcheck]
    fn prop_comment_prefix_never_empty(lang: String) -> bool {
        !comment_prefix(&lang).is_empty()
    }

    #[quickcheck]
    fn prop_lines_always_prefixed(text: String) -> bool {
        let lines = format_note_lines(&text, "rust", Some(80), false);
        // All lines must start with "// " (unless empty input)
        lines.iter().all(|line| line.starts_with("// "))
    }

    #[quickcheck]
    fn prop_stale_marker_on_last_line(text: String) -> bool {
        if text.trim().is_empty() {
            return true;
        }
        let lines = format_note_lines(&text, "rust", Some(80), true);
        if lines.is_empty() {
            return true;
        }
        // Only last line should have [STALE]
        let last_idx = lines.len() - 1;
        lines.iter().enumerate().all(|(i, line)| {
            if i == last_idx {
                line.contains("[STALE]")
            } else {
                !line.contains("[STALE]")
            }
        })
    }
}
