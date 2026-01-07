//! Configuration for tree-sitter languages
//!
//! Loads from ~/.config/mnemos/languages.toml

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;

use crate::{Result, TreeSitterError};

/// Top-level configuration from languages.toml
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(default)]
pub struct LanguageConfig {
    /// Runtime directory for user-installed grammars
    #[serde(rename = "runtime-dir")]
    pub runtime_dir: Option<PathBuf>,

    /// Per-language settings
    #[serde(rename = "language", default)]
    pub languages: Vec<LanguageSettings>,

    /// User-installed grammar sources
    #[serde(rename = "grammar", default)]
    pub grammars: Vec<GrammarSource>,
}

/// Settings for a specific language
#[derive(Debug, Clone, Serialize, Deserialize)]
#[derive(Default)]
pub struct LanguageSettings {
    /// Language name (e.g., "rust", "python")
    pub name: String,

    /// File extensions for this language
    #[serde(rename = "file-types", default)]
    pub file_types: Vec<String>,

    /// Node types to skip (too small/granular)
    #[serde(rename = "skip-nodes", default)]
    pub skip_nodes: Vec<String>,

    /// Node types that are containers (too large)
    #[serde(rename = "container-nodes", default)]
    pub container_nodes: Vec<String>,
}

/// Source for user-installed grammar
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GrammarSource {
    /// Grammar name
    pub name: String,

    /// Source location
    pub source: GrammarSourceLocation,
}

/// Location of grammar source
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GrammarSourceLocation {
    /// Git repository URL
    pub git: Option<String>,

    /// Git revision (commit hash or tag)
    pub rev: Option<String>,

    /// Local path (alternative to git)
    pub path: Option<PathBuf>,
}

impl LanguageConfig {
    /// Get runtime directory, defaulting to ~/.config/mnemos
    pub fn runtime_dir(&self) -> PathBuf {
        self.runtime_dir.clone().unwrap_or_else(|| {
            dirs::config_dir()
                .unwrap_or_else(|| PathBuf::from("~/.config"))
                .join("mnemos")
        })
    }

    /// Get grammars directory
    pub fn grammars_dir(&self) -> PathBuf {
        self.runtime_dir().join("grammars")
    }

    /// Get settings for a language by name
    pub fn get_language(&self, name: &str) -> Option<&LanguageSettings> {
        self.languages.iter().find(|l| l.name == name)
    }

    /// Get language name from file extension
    pub fn language_for_extension(&self, ext: &str) -> Option<&str> {
        self.languages
            .iter()
            .find(|l| l.file_types.iter().any(|ft| ft == ext))
            .map(|l| l.name.as_str())
    }

    /// Build a lookup map from extension to language
    pub fn extension_map(&self) -> HashMap<String, String> {
        let mut map = HashMap::new();
        for lang in &self.languages {
            for ext in &lang.file_types {
                map.insert(ext.clone(), lang.name.clone());
            }
        }
        map
    }
}


/// Get the mnemos config directory.
/// Uses MNEMOS_CONFIG_DIR env var if set, otherwise ~/.config/mnemos
pub fn config_dir() -> PathBuf {
    std::env::var("MNEMOS_CONFIG_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|_| {
            dirs::config_dir()
                .unwrap_or_else(|| PathBuf::from("~/.config"))
                .join("mnemos")
        })
}

/// Load configuration from languages.toml
/// Returns default config with built-in language settings if file doesn't exist
pub fn load_config() -> Result<LanguageConfig> {
    let config_path = config_dir().join("languages.toml");

    if config_path.exists() {
        let content = std::fs::read_to_string(&config_path)?;
        let mut config: LanguageConfig = toml::from_str(&content)
            .map_err(|e| TreeSitterError::ConfigError(e.to_string()))?;

        // Override runtime_dir with config_dir if not explicitly set
        if config.runtime_dir.is_none() {
            config.runtime_dir = Some(config_dir());
        }

        // Merge with defaults for any missing languages
        merge_defaults(&mut config);
        Ok(config)
    } else {
        let mut config = default_config();
        config.runtime_dir = Some(config_dir());
        Ok(config)
    }
}

/// Merge user config with default language settings
fn merge_defaults(config: &mut LanguageConfig) {
    let defaults = default_config();

    for default_lang in defaults.languages {
        if !config.languages.iter().any(|l| l.name == default_lang.name) {
            config.languages.push(default_lang);
        }
    }
}

/// Default configuration with built-in language settings
pub fn default_config() -> LanguageConfig {
    LanguageConfig {
        runtime_dir: None,
        languages: vec![
            LanguageSettings {
                name: "rust".to_string(),
                file_types: vec!["rs".to_string()],
                skip_nodes: vec![
                    "identifier".to_string(),
                    "type_identifier".to_string(),
                    "field_identifier".to_string(),
                    "parameter".to_string(),
                    "parameters".to_string(),
                    "self_parameter".to_string(),
                    "visibility_modifier".to_string(),
                    "line_comment".to_string(),
                    "block_comment".to_string(),
                ],
                container_nodes: vec![
                    "source_file".to_string(),
                    "impl_item".to_string(),
                    "trait_item".to_string(),
                    "mod_item".to_string(),
                    "declaration_list".to_string(),
                    "block".to_string(),
                ],
            },
            LanguageSettings {
                name: "python".to_string(),
                file_types: vec!["py".to_string(), "pyi".to_string()],
                skip_nodes: vec![
                    "identifier".to_string(),
                    "argument".to_string(),
                    "parameter".to_string(),
                    "comment".to_string(),
                ],
                container_nodes: vec![
                    "module".to_string(),
                    "class_definition".to_string(),
                    "block".to_string(),
                ],
            },
            LanguageSettings {
                name: "javascript".to_string(),
                file_types: vec!["js".to_string(), "jsx".to_string(), "mjs".to_string()],
                skip_nodes: vec![
                    "identifier".to_string(),
                    "property_identifier".to_string(),
                    "shorthand_property_identifier".to_string(),
                    "comment".to_string(),
                ],
                container_nodes: vec![
                    "program".to_string(),
                    "statement_block".to_string(),
                ],
            },
            LanguageSettings {
                name: "typescript".to_string(),
                file_types: vec!["ts".to_string(), "tsx".to_string()],
                skip_nodes: vec![
                    "identifier".to_string(),
                    "property_identifier".to_string(),
                    "type_identifier".to_string(),
                    "comment".to_string(),
                ],
                container_nodes: vec![
                    "program".to_string(),
                    "statement_block".to_string(),
                ],
            },
            LanguageSettings {
                name: "go".to_string(),
                file_types: vec!["go".to_string()],
                skip_nodes: vec![
                    "identifier".to_string(),
                    "field_identifier".to_string(),
                    "type_identifier".to_string(),
                    "comment".to_string(),
                ],
                container_nodes: vec![
                    "source_file".to_string(),
                    "block".to_string(),
                ],
            },
            LanguageSettings {
                name: "lua".to_string(),
                file_types: vec!["lua".to_string()],
                skip_nodes: vec![
                    "identifier".to_string(),
                    "comment".to_string(),
                ],
                container_nodes: vec![
                    "chunk".to_string(),
                    "block".to_string(),
                ],
            },
            LanguageSettings {
                name: "c".to_string(),
                file_types: vec!["c".to_string(), "h".to_string()],
                skip_nodes: vec![
                    "identifier".to_string(),
                    "field_identifier".to_string(),
                    "type_identifier".to_string(),
                    "comment".to_string(),
                ],
                container_nodes: vec![
                    "translation_unit".to_string(),
                    "compound_statement".to_string(),
                ],
            },
            LanguageSettings {
                name: "cpp".to_string(),
                file_types: vec![
                    "cpp".to_string(),
                    "cc".to_string(),
                    "cxx".to_string(),
                    "hpp".to_string(),
                    "hxx".to_string(),
                ],
                skip_nodes: vec![
                    "identifier".to_string(),
                    "field_identifier".to_string(),
                    "type_identifier".to_string(),
                    "comment".to_string(),
                ],
                container_nodes: vec![
                    "translation_unit".to_string(),
                    "compound_statement".to_string(),
                    "declaration_list".to_string(),
                ],
            },
            LanguageSettings {
                name: "java".to_string(),
                file_types: vec!["java".to_string()],
                skip_nodes: vec![
                    "identifier".to_string(),
                    "type_identifier".to_string(),
                    "comment".to_string(),
                    "line_comment".to_string(),
                    "block_comment".to_string(),
                ],
                container_nodes: vec![
                    "program".to_string(),
                    "class_body".to_string(),
                    "block".to_string(),
                ],
            },
            LanguageSettings {
                name: "ruby".to_string(),
                file_types: vec!["rb".to_string(), "rake".to_string(), "gemspec".to_string()],
                skip_nodes: vec![
                    "identifier".to_string(),
                    "constant".to_string(),
                    "comment".to_string(),
                ],
                container_nodes: vec![
                    "program".to_string(),
                    "class".to_string(),
                    "module".to_string(),
                    "block".to_string(),
                    "do_block".to_string(),
                ],
            },
            LanguageSettings {
                name: "bash".to_string(),
                file_types: vec!["sh".to_string(), "bash".to_string(), "zsh".to_string()],
                skip_nodes: vec![
                    "variable_name".to_string(),
                    "word".to_string(),
                    "comment".to_string(),
                ],
                container_nodes: vec![
                    "program".to_string(),
                    "compound_statement".to_string(),
                    "subshell".to_string(),
                ],
            },
            LanguageSettings {
                name: "yaml".to_string(),
                file_types: vec!["yaml".to_string(), "yml".to_string()],
                skip_nodes: vec![
                    "plain_scalar".to_string(),
                    "string_scalar".to_string(),
                    "comment".to_string(),
                ],
                container_nodes: vec![
                    "stream".to_string(),
                    "document".to_string(),
                    "block_mapping".to_string(),
                    "block_sequence".to_string(),
                ],
            },
            LanguageSettings {
                name: "json".to_string(),
                file_types: vec!["json".to_string()],
                skip_nodes: vec![
                    "string".to_string(),
                    "number".to_string(),
                    "true".to_string(),
                    "false".to_string(),
                    "null".to_string(),
                ],
                container_nodes: vec![
                    "document".to_string(),
                    "object".to_string(),
                    "array".to_string(),
                ],
            },
            LanguageSettings {
                name: "toml".to_string(),
                file_types: vec!["toml".to_string()],
                skip_nodes: vec![
                    "string".to_string(),
                    "integer".to_string(),
                    "float".to_string(),
                    "boolean".to_string(),
                    "comment".to_string(),
                ],
                container_nodes: vec![
                    "document".to_string(),
                    "table".to_string(),
                    "array".to_string(),
                ],
            },
            LanguageSettings {
                name: "html".to_string(),
                file_types: vec!["html".to_string(), "htm".to_string()],
                skip_nodes: vec![
                    "tag_name".to_string(),
                    "attribute_name".to_string(),
                    "attribute_value".to_string(),
                    "text".to_string(),
                    "comment".to_string(),
                ],
                container_nodes: vec![
                    "document".to_string(),
                    "element".to_string(),
                ],
            },
            LanguageSettings {
                name: "css".to_string(),
                file_types: vec!["css".to_string()],
                skip_nodes: vec![
                    "property_name".to_string(),
                    "plain_value".to_string(),
                    "string_value".to_string(),
                    "integer_value".to_string(),
                    "float_value".to_string(),
                    "comment".to_string(),
                ],
                container_nodes: vec![
                    "stylesheet".to_string(),
                    "rule_set".to_string(),
                    "block".to_string(),
                ],
            },
            LanguageSettings {
                name: "markdown".to_string(),
                file_types: vec!["md".to_string(), "markdown".to_string()],
                skip_nodes: vec![
                    "text".to_string(),
                    "inline".to_string(),
                ],
                container_nodes: vec![
                    "document".to_string(),
                    "section".to_string(),
                ],
            },
        ],
        grammars: Vec::new(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config_has_rust() {
        let config = default_config();
        let rust = config.get_language("rust");
        assert!(rust.is_some());
        assert!(rust.unwrap().file_types.contains(&"rs".to_string()));
    }

    #[test]
    fn test_extension_lookup() {
        let config = default_config();
        assert_eq!(config.language_for_extension("rs"), Some("rust"));
        assert_eq!(config.language_for_extension("py"), Some("python"));
        assert_eq!(config.language_for_extension("js"), Some("javascript"));
        assert_eq!(config.language_for_extension("rb"), Some("ruby"));
        assert_eq!(config.language_for_extension("sh"), Some("bash"));
        assert_eq!(config.language_for_extension("yaml"), Some("yaml"));
        assert_eq!(config.language_for_extension("json"), Some("json"));
        assert_eq!(config.language_for_extension("toml"), Some("toml"));
        assert_eq!(config.language_for_extension("html"), Some("html"));
        assert_eq!(config.language_for_extension("css"), Some("css"));
        assert_eq!(config.language_for_extension("md"), Some("markdown"));
        assert_eq!(config.language_for_extension("unknown"), None);
    }

    #[test]
    fn test_default_config_has_all_languages() {
        let config = default_config();
        let expected = vec![
            "rust", "python", "javascript", "typescript", "go", "lua", "c", "cpp", "java",
            "ruby", "bash", "yaml", "json", "toml", "html", "css", "markdown",
        ];
        for lang in expected {
            assert!(
                config.get_language(lang).is_some(),
                "Missing language config for: {}",
                lang
            );
        }
    }

    #[test]
    fn test_parse_config() {
        let toml = r#"
            runtime-dir = "/custom/path"

            [[language]]
            name = "rust"
            file-types = ["rs"]
            skip-nodes = ["identifier"]
            container-nodes = ["source_file"]
        "#;

        let config: LanguageConfig = toml::from_str(toml).unwrap();
        assert_eq!(config.runtime_dir, Some(PathBuf::from("/custom/path")));
        assert_eq!(config.languages.len(), 1);
        assert_eq!(config.languages[0].name, "rust");
    }
}
