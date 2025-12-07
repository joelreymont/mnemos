//! Grammar registry for tree-sitter languages
//!
//! Manages bundled and dynamically loaded grammars.
//!
//! # Security
//! User-installed grammars require a `.sha256` hash file for integrity verification.
//! The hash file must contain the SHA-256 hash of the library file.

use log::warn;
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
                // Normalize name: hyphens -> underscores to prevent collisions
                // (e.g., "my-lib" and "my_lib" would both resolve to same symbol)
                let canonical_name = name.replace('-', "_");
                match self.load_grammar_from_path(&canonical_name, &path) {
                    Ok(lang) => {
                        // User grammars override bundled ones
                        self.grammars.insert(canonical_name, lang);
                    }
                    Err(e) => {
                        warn!("Failed to load grammar from {:?}: {}", path, e);
                    }
                }
            }
        }

        Ok(())
    }

    /// Load a grammar from a shared library
    ///
    /// # Security
    /// - Requires a `.sha256` hash file for integrity verification
    /// - Checks file permissions (warns on world/group-writable)
    /// - Loading shared libraries is inherently unsafe
    fn load_grammar_from_path(&self, name: &str, path: &Path) -> Result<Language> {
        // Security: Verify integrity via SHA-256 hash file
        let hash_path = path.with_extension(
            path.extension()
                .and_then(|e| e.to_str())
                .map(|e| format!("{}.sha256", e))
                .unwrap_or_else(|| "sha256".to_string()),
        );

        if !hash_path.exists() {
            return Err(TreeSitterError::GrammarLoadFailed {
                path: path.file_name().unwrap_or_default().to_string_lossy().to_string(),
                reason: "missing .sha256 hash file (required for security verification)".to_string(),
            });
        }

        // Read expected hash
        let expected_hash = std::fs::read_to_string(&hash_path)
            .map_err(|e| TreeSitterError::GrammarLoadFailed {
                path: path.file_name().unwrap_or_default().to_string_lossy().to_string(),
                reason: format!("failed to read hash file: {}", e),
            })?
            .trim()
            .to_lowercase();

        // Validate hash format (64 hex chars)
        if expected_hash.len() != 64 || !expected_hash.chars().all(|c| c.is_ascii_hexdigit()) {
            return Err(TreeSitterError::GrammarLoadFailed {
                path: path.file_name().unwrap_or_default().to_string_lossy().to_string(),
                reason: "invalid hash format (expected 64 hex characters)".to_string(),
            });
        }

        // Compute actual hash of library file
        let lib_bytes = std::fs::read(path).map_err(|e| TreeSitterError::GrammarLoadFailed {
            path: path.file_name().unwrap_or_default().to_string_lossy().to_string(),
            reason: format!("failed to read library: {}", e),
        })?;

        use sha2::{Digest, Sha256};
        use subtle::ConstantTimeEq;
        let actual_hash = format!("{:x}", Sha256::digest(&lib_bytes));

        // Use constant-time comparison to prevent timing attacks
        if actual_hash.as_bytes().ct_eq(expected_hash.as_bytes()).unwrap_u8() != 1 {
            return Err(TreeSitterError::GrammarLoadFailed {
                path: path.file_name().unwrap_or_default().to_string_lossy().to_string(),
                reason: "hash verification failed (library may have been tampered with)".to_string(),
            });
        }

        // Security: Block loading of world-writable or group-writable libraries
        #[cfg(unix)]
        {
            use std::os::unix::fs::MetadataExt;
            if let Ok(meta) = path.metadata() {
                let mode = meta.mode();
                // Block world-writable (o+w = 0o002)
                if mode & 0o002 != 0 {
                    return Err(TreeSitterError::GrammarLoadFailed {
                        path: path.file_name().unwrap_or_default().to_string_lossy().to_string(),
                        reason: "library is world-writable (security risk)".to_string(),
                    });
                }
                // Block group-writable (g+w = 0o020)
                if mode & 0o020 != 0 {
                    return Err(TreeSitterError::GrammarLoadFailed {
                        path: path.file_name().unwrap_or_default().to_string_lossy().to_string(),
                        reason: "library is group-writable (security risk)".to_string(),
                    });
                }
            }
        }

        // Validate grammar name (must be alphanumeric/underscore for safe symbol lookup)
        // Name should already be normalized (hyphens replaced with underscores by caller)
        if !name.chars().all(|c| c.is_ascii_alphanumeric() || c == '_') {
            return Err(TreeSitterError::GrammarLoadFailed {
                path: path.file_name().unwrap_or_default().to_string_lossy().to_string(),
                reason: format!("invalid grammar name '{}' (must be alphanumeric or underscore)", name),
            });
        }

        // Safety: Loading shared libraries is inherently unsafe
        // We've verified the hash, so we trust the library content
        unsafe {
            let lib = libloading::Library::new(path).map_err(|e| {
                TreeSitterError::GrammarLoadFailed {
                    path: path.file_name().unwrap_or_default().to_string_lossy().to_string(),
                    reason: e.to_string(),
                }
            })?;

            // Tree-sitter grammars export a function named tree_sitter_<name>
            let func_name = format!("tree_sitter_{}", name);
            let func: libloading::Symbol<unsafe extern "C" fn() -> tree_sitter::Language> =
                lib.get(func_name.as_bytes()).map_err(|e| {
                    TreeSitterError::GrammarLoadFailed {
                        path: path.file_name().unwrap_or_default().to_string_lossy().to_string(),
                        reason: format!("symbol {} not found: {}", func_name, e),
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
