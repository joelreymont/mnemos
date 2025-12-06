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

/// Wrap text at specified width, breaking at word boundaries
pub fn wrap_text(text: &str, width: usize) -> Vec<String> {
    if width == 0 {
        return vec![text.to_string()];
    }

    let mut lines = Vec::new();

    for paragraph in text.lines() {
        if paragraph.trim().is_empty() {
            continue; // Skip empty lines
        }

        if paragraph.len() <= width {
            lines.push(paragraph.to_string());
            continue;
        }

        let mut remaining = paragraph;
        while !remaining.is_empty() {
            if remaining.len() <= width {
                lines.push(remaining.to_string());
                break;
            }

            // Find last space within width
            let break_pos = remaining[..width]
                .rfind(' ')
                .unwrap_or(width); // Hard break if no space

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
/// wrapped to the specified width.
pub fn format_note_lines(
    text: &str,
    lang_or_ext: &str,
    wrap_width: Option<usize>,
    stale: bool,
) -> Vec<String> {
    let prefix = format!("{} ", comment_prefix(lang_or_ext));
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
    fn test_format_multiline() {
        let text = "Line one\nLine two";
        let lines = format_note_lines(text, "rust", Some(80), false);
        assert_eq!(lines, vec!["// Line one", "// Line two"]);
    }
}
