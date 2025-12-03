//! Grammar registry for tree-sitter languages
//!
//! Manages bundled and dynamically loaded grammars.

use std::collections::HashMap;
use std::path::Path;

use crate::config::LanguageConfig;
use crate::{Result, TreeSitterError};

/// Wrapper around tree-sitter Language
#[derive(Clone)]
pub struct Language {
    inner: tree_sitter::Language,
    name: String,
}

impl Language {
    pub fn new(name: &str, inner: tree_sitter::Language) -> Self {
        Self {
            inner,
            name: name.to_string(),
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn inner(&self) -> &tree_sitter::Language {
        &self.inner
    }
}

/// Registry of available grammars
pub struct GrammarRegistry {
    /// Loaded grammars by language name
    grammars: HashMap<String, Language>,

    /// Extension to language name mapping
    extension_map: HashMap<String, String>,

    /// Configuration
    config: LanguageConfig,
}

impl GrammarRegistry {
    /// Create a new registry with bundled grammars
    pub fn new(config: LanguageConfig) -> Self {
        let extension_map = config.extension_map();
        let mut registry = Self {
            grammars: HashMap::new(),
            extension_map,
            config,
        };

        // Load bundled grammars
        registry.load_bundled_grammars();

        // Try to load user grammars (don't fail if directory doesn't exist)
        let _ = registry.load_user_grammars();

        registry
    }

    /// Load all bundled grammars
    fn load_bundled_grammars(&mut self) {
        // Rust
        self.grammars.insert(
            "rust".to_string(),
            Language::new("rust", tree_sitter_rust::LANGUAGE.into()),
        );

        // Python
        self.grammars.insert(
            "python".to_string(),
            Language::new("python", tree_sitter_python::LANGUAGE.into()),
        );

        // JavaScript
        self.grammars.insert(
            "javascript".to_string(),
            Language::new("javascript", tree_sitter_javascript::LANGUAGE.into()),
        );

        // TypeScript
        self.grammars.insert(
            "typescript".to_string(),
            Language::new(
                "typescript",
                tree_sitter_typescript::LANGUAGE_TYPESCRIPT.into(),
            ),
        );

        // TSX
        self.grammars.insert(
            "tsx".to_string(),
            Language::new("tsx", tree_sitter_typescript::LANGUAGE_TSX.into()),
        );

        // Go
        self.grammars.insert(
            "go".to_string(),
            Language::new("go", tree_sitter_go::LANGUAGE.into()),
        );

        // C
        self.grammars.insert(
            "c".to_string(),
            Language::new("c", tree_sitter_c::LANGUAGE.into()),
        );

        // C++
        self.grammars.insert(
            "cpp".to_string(),
            Language::new("cpp", tree_sitter_cpp::LANGUAGE.into()),
        );

        // Java
        self.grammars.insert(
            "java".to_string(),
            Language::new("java", tree_sitter_java::LANGUAGE.into()),
        );

        // Lua
        self.grammars.insert(
            "lua".to_string(),
            Language::new("lua", tree_sitter_lua::LANGUAGE.into()),
        );
    }

    /// Load user-installed grammars from disk
    fn load_user_grammars(&mut self) -> Result<()> {
        let grammars_dir = self.config.grammars_dir();

        if !grammars_dir.exists() {
            return Ok(());
        }

        for entry in std::fs::read_dir(&grammars_dir)? {
            let entry = entry?;
            let path = entry.path();

            // Look for .so (Linux) or .dylib (macOS) files
            let ext = path.extension().and_then(|e| e.to_str());
            if ext != Some("so") && ext != Some("dylib") {
                continue;
            }

            // Extract language name from filename
            // Supports both formats:
            // - libtree-sitter-<name>.<ext> (tree-sitter CLI output)
            // - <name>.<ext> (simple naming)
            let stem = path.file_stem().and_then(|s| s.to_str());
            let name = stem.map(|s| {
                if let Some(stripped) = s.strip_prefix("libtree-sitter-") {
                    stripped.to_string()
                } else {
                    s.to_string()
                }
            });

            if let Some(name) = name {
                match self.load_grammar_from_path(&name, &path) {
                    Ok(lang) => {
                        // User grammars override bundled ones
                        self.grammars.insert(name, lang);
                    }
                    Err(e) => {
                        eprintln!("Warning: Failed to load grammar from {:?}: {}", path, e);
                    }
                }
            }
        }

        Ok(())
    }

    /// Load a grammar from a shared library
    ///
    /// # Security Warning
    /// Loading shared libraries is inherently unsafe. A malicious library can
    /// execute arbitrary code. Only load libraries you trust.
    fn load_grammar_from_path(&self, name: &str, path: &Path) -> Result<Language> {
        // Security: Check file permissions - warn if world-writable
        #[cfg(unix)]
        {
            use std::os::unix::fs::MetadataExt;
            if let Ok(meta) = path.metadata() {
                let mode = meta.mode();
                // Check if world-writable (o+w = 0o002)
                if mode & 0o002 != 0 {
                    eprintln!(
                        "Security warning: Grammar library {:?} is world-writable. \
                         This is a security risk. Run: chmod o-w {:?}",
                        path, path
                    );
                }
                // Check if group-writable (g+w = 0o020)
                if mode & 0o020 != 0 {
                    eprintln!(
                        "Security warning: Grammar library {:?} is group-writable. \
                         Consider restricting permissions.",
                        path
                    );
                }
            }
        }

        // Safety: Loading shared libraries is inherently unsafe
        // We trust that grammars in the user's config directory are safe
        unsafe {
            let lib = libloading::Library::new(path).map_err(|e| {
                TreeSitterError::GrammarLoadFailed {
                    path: path.display().to_string(),
                    reason: e.to_string(),
                }
            })?;

            // Tree-sitter grammars export a function named tree_sitter_<name>
            let func_name = format!("tree_sitter_{}", name);
            let func: libloading::Symbol<unsafe extern "C" fn() -> tree_sitter::Language> =
                lib.get(func_name.as_bytes()).map_err(|e| {
                    TreeSitterError::GrammarLoadFailed {
                        path: path.display().to_string(),
                        reason: format!("Symbol {} not found: {}", func_name, e),
                    }
                })?;

            let language = func();

            // Leak the library so it stays loaded
            // This is intentional - grammars need to stay in memory
            std::mem::forget(lib);

            Ok(Language::new(name, language))
        }
    }

    /// Get a grammar by language name
    pub fn get(&self, name: &str) -> Option<&Language> {
        self.grammars.get(name)
    }

    /// Get a grammar by file extension
    pub fn get_for_extension(&self, ext: &str) -> Option<&Language> {
        self.extension_map
            .get(ext)
            .and_then(|name| self.grammars.get(name))
    }

    /// Get language name from file path
    pub fn language_for_file(&self, path: &Path) -> Option<&str> {
        path.extension()
            .and_then(|ext| ext.to_str())
            .and_then(|ext| self.extension_map.get(ext))
            .map(|s| s.as_str())
    }

    /// Check if a language is available
    pub fn has_language(&self, name: &str) -> bool {
        self.grammars.contains_key(name)
    }

    /// List all available languages
    pub fn available_languages(&self) -> Vec<&str> {
        self.grammars.keys().map(|s| s.as_str()).collect()
    }

    /// Get configuration
    pub fn config(&self) -> &LanguageConfig {
        &self.config
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::default_config;

    #[test]
    fn test_bundled_grammars_loaded() {
        let registry = GrammarRegistry::new(default_config());

        assert!(registry.has_language("rust"));
        assert!(registry.has_language("python"));
        assert!(registry.has_language("javascript"));
        assert!(registry.has_language("go"));
    }

    #[test]
    fn test_extension_lookup() {
        let registry = GrammarRegistry::new(default_config());

        assert!(registry.get_for_extension("rs").is_some());
        assert!(registry.get_for_extension("py").is_some());
        assert!(registry.get_for_extension("js").is_some());
        assert!(registry.get_for_extension("unknown").is_none());
    }

    #[test]
    fn test_language_for_file() {
        let registry = GrammarRegistry::new(default_config());

        assert_eq!(
            registry.language_for_file(Path::new("test.rs")),
            Some("rust")
        );
        assert_eq!(
            registry.language_for_file(Path::new("test.py")),
            Some("python")
        );
        assert_eq!(registry.language_for_file(Path::new("test.unknown")), None);
    }
}
