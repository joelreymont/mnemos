//! Hemis configuration
//!
//! Configuration is loaded from (in order of precedence):
//! 1. Config file specified via --config flag
//! 2. Environment variables (HEMIS_DB_PATH, HEMIS_AI_PROVIDER)
//! 3. Default config file (~/.config/hemis/config.toml)
//! 4. Default values
//!
//! Config file format (TOML):
//!   db-path = "/path/to/hemis.db"
//!   ai-provider = "claude"  # or "codex", "none"

use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

/// Hemis configuration
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(default)]
pub struct HemisConfig {
    /// Database path
    #[serde(rename = "db-path")]
    pub db_path: Option<String>,

    /// AI provider (codex, claude, none)
    #[serde(rename = "ai-provider")]
    pub ai_provider: Option<String>,
}

/// Get the hemis config directory (XDG compliant).
/// Uses HEMIS_CONFIG_DIR env var if set, otherwise ~/.config/hemis
pub fn config_dir() -> PathBuf {
    std::env::var("HEMIS_CONFIG_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|_| {
            dirs::config_dir()
                .unwrap_or_else(|| {
                    dirs::home_dir()
                        .map(|h| h.join(".config"))
                        .unwrap_or_else(|| PathBuf::from(".config"))
                })
                .join("hemis")
        })
}

/// Get the hemis data directory (~/.hemis or HEMIS_DIR).
pub fn data_dir() -> PathBuf {
    std::env::var("HEMIS_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|_| {
            dirs::home_dir()
                .map(|h| h.join(".hemis"))
                .unwrap_or_else(|| PathBuf::from(".hemis"))
        })
}

/// Load configuration from a specific path or the default location
pub fn load_config_from(path: Option<&Path>) -> HemisConfig {
    let config_path = path
        .map(PathBuf::from)
        .unwrap_or_else(|| config_dir().join("config.toml"));

    if config_path.exists() {
        match std::fs::read_to_string(&config_path) {
            Ok(content) => match toml::from_str(&content) {
                Ok(config) => config,
                Err(e) => {
                    eprintln!("Warning: Failed to parse {}: {}", config_path.display(), e);
                    HemisConfig::default()
                }
            },
            Err(e) => {
                eprintln!("Warning: Failed to read {}: {}", config_path.display(), e);
                HemisConfig::default()
            }
        }
    } else {
        HemisConfig::default()
    }
}

/// CLI overrides for configuration
#[derive(Debug, Clone, Default)]
pub struct CliOverrides {
    pub db_path: Option<String>,
    pub ai_provider: Option<String>,
}

/// Resolved configuration after loading from config file
#[derive(Debug, Clone)]
pub struct ResolvedConfig {
    pub db_path: String,
    pub ai_provider: Option<String>,
}

impl ResolvedConfig {
    /// Build resolved config.
    /// Precedence: CLI flags > --config file > env vars > default config file > defaults
    pub fn new(config_path: Option<&str>, overrides: CliOverrides) -> Self {
        let config = load_config_from(config_path.map(Path::new));
        let data_dir = data_dir();
        let _ = std::fs::create_dir_all(&data_dir);

        // DB path: CLI > config file > env var > default
        let db_path = overrides
            .db_path
            .or(config.db_path)
            .or_else(|| std::env::var("HEMIS_DB_PATH").ok())
            .unwrap_or_else(|| data_dir.join("hemis.db").to_string_lossy().into_owned());

        // AI provider: CLI > config file > env var > auto-detect
        let ai_provider = overrides
            .ai_provider
            .or(config.ai_provider)
            .or_else(|| std::env::var("HEMIS_AI_PROVIDER").ok());

        // Set AI provider env var so ai_cli module can pick it up
        if let Some(ref provider) = ai_provider {
            std::env::set_var("HEMIS_AI_PROVIDER", provider);
        }

        Self {
            db_path,
            ai_provider,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_config() {
        let toml = r#"
            db-path = "/custom/hemis.db"
            ai-provider = "claude"
        "#;

        let config: HemisConfig = toml::from_str(toml).unwrap();
        assert_eq!(config.db_path, Some("/custom/hemis.db".to_string()));
        assert_eq!(config.ai_provider, Some("claude".to_string()));
    }

    #[test]
    fn test_default_config() {
        let config = HemisConfig::default();
        assert!(config.db_path.is_none());
        assert!(config.ai_provider.is_none());
    }
}
