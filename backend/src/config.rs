//! Mnemos configuration
//!
//! Configuration is loaded from (in order of precedence):
//! 1. CLI flags (--db-path, --ai-provider)
//! 2. Environment variables (MNEMOS_DB_PATH, MNEMOS_AI_PROVIDER)
//! 3. Config file (~/.config/mnemos/config.toml or --config path)
//! 4. Default values
//!
//! Config file format (TOML):
//!   db-path = "/path/to/mnemos.db"
//!   ai-provider = "claude"  # or "codex", "none"

use log::warn;
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

/// Mnemos configuration
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(default)]
pub struct MnemosConfig {
    /// Database path
    #[serde(rename = "db-path")]
    pub db_path: Option<String>,

    /// AI provider (codex, claude, none)
    #[serde(rename = "ai-provider")]
    pub ai_provider: Option<String>,
}

/// Get the mnemos config directory.
/// Priority: MNEMOS_CONFIG_DIR env var > ~/.config/mnemos (if exists) > platform default
pub fn config_dir() -> PathBuf {
    // 1. Check env var first
    if let Ok(dir) = std::env::var("MNEMOS_CONFIG_DIR") {
        return PathBuf::from(dir);
    }

    // 2. Check XDG location (~/.config/mnemos) - preferred for CLI tools
    if let Some(home) = dirs::home_dir() {
        let xdg_config = home.join(".config").join("mnemos");
        if xdg_config.exists() {
            return xdg_config;
        }
    }

    // 3. Fall back to platform-native location (~/Library/Application Support/mnemos on macOS)
    dirs::config_dir()
        .unwrap_or_else(|| {
            dirs::home_dir()
                .map(|h| h.join(".config"))
                .unwrap_or_else(|| PathBuf::from(".config"))
        })
        .join("mnemos")
}

/// Get the mnemos data directory (~/.mnemos or MNEMOS_DIR).
pub fn data_dir() -> PathBuf {
    std::env::var("MNEMOS_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|_| {
            dirs::home_dir()
                .map(|h| h.join(".mnemos"))
                .unwrap_or_else(|| PathBuf::from(".mnemos"))
        })
}

/// Load configuration from a specific path or the default location
pub fn load_config_from(path: Option<&Path>) -> MnemosConfig {
    let config_path = path
        .map(PathBuf::from)
        .unwrap_or_else(|| config_dir().join("config.toml"));

    if config_path.exists() {
        match std::fs::read_to_string(&config_path) {
            Ok(content) => match toml::from_str(&content) {
                Ok(config) => config,
                Err(e) => {
                    warn!("Failed to parse {}: {}", config_path.display(), e);
                    MnemosConfig::default()
                }
            },
            Err(e) => {
                warn!("Failed to read {}: {}", config_path.display(), e);
                MnemosConfig::default()
            }
        }
    } else {
        MnemosConfig::default()
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
    /// Precedence: CLI flags > env vars > config file > defaults
    pub fn new(config_path: Option<&str>, overrides: CliOverrides) -> Self {
        let config = load_config_from(config_path.map(Path::new));
        let data_dir = data_dir();
        let _ = std::fs::create_dir_all(&data_dir);

        // DB path: CLI > env var > config file > default
        let db_path = overrides
            .db_path
            .or_else(|| std::env::var("MNEMOS_DB_PATH").ok())
            .or(config.db_path)
            .unwrap_or_else(|| data_dir.join("mnemos.db").to_string_lossy().into_owned());

        // AI provider: CLI > env var > config file > auto-detect
        let ai_provider = overrides
            .ai_provider
            .or_else(|| std::env::var("MNEMOS_AI_PROVIDER").ok())
            .or(config.ai_provider);

        // Set AI provider env var so ai_cli module can pick it up
        if let Some(ref provider) = ai_provider {
            std::env::set_var("MNEMOS_AI_PROVIDER", provider);
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
    use serial_test::serial;

    #[test]
    fn test_parse_config() {
        let toml = r#"
            db-path = "/custom/mnemos.db"
            ai-provider = "claude"
        "#;

        let config: MnemosConfig = toml::from_str(toml).unwrap();
        assert_eq!(config.db_path, Some("/custom/mnemos.db".to_string()));
        assert_eq!(config.ai_provider, Some("claude".to_string()));
    }

    #[test]
    fn test_default_config() {
        let config = MnemosConfig::default();
        assert!(config.db_path.is_none());
        assert!(config.ai_provider.is_none());
    }

    // Test precedence: CLI > env var > config file > default

    #[test]
    #[serial(env)]
    fn test_cli_overrides_env_var() {
        // CLI arg should take precedence over env var
        // Set env var
        std::env::set_var("MNEMOS_DB_PATH", "/env/db.db");
        std::env::set_var("MNEMOS_AI_PROVIDER", "env-provider");

        // CLI override
        let overrides = CliOverrides {
            db_path: Some("/cli/db.db".to_string()),
            ai_provider: Some("cli-provider".to_string()),
        };

        let config = ResolvedConfig::new(None, overrides);

        // CLI should win
        assert_eq!(config.db_path, "/cli/db.db");
        assert_eq!(config.ai_provider, Some("cli-provider".to_string()));

        // Cleanup
        std::env::remove_var("MNEMOS_DB_PATH");
        std::env::remove_var("MNEMOS_AI_PROVIDER");
    }

    #[test]
    #[serial(env)]
    fn test_env_var_overrides_config_file() {
        // Env var should take precedence over config file
        // Create temp config file
        let tmp = tempfile::TempDir::new().unwrap();
        let config_path = tmp.path().join("config.toml");
        std::fs::write(
            &config_path,
            r#"
            db-path = "/config/db.db"
            ai-provider = "config-provider"
            "#,
        )
        .unwrap();

        // Set env var (should override config)
        std::env::set_var("MNEMOS_DB_PATH", "/env/db.db");
        std::env::set_var("MNEMOS_AI_PROVIDER", "env-provider");

        // No CLI override
        let overrides = CliOverrides::default();

        let config = ResolvedConfig::new(Some(config_path.to_str().unwrap()), overrides);

        // Env var should win over config file
        assert_eq!(config.db_path, "/env/db.db");
        assert_eq!(config.ai_provider, Some("env-provider".to_string()));

        // Cleanup
        std::env::remove_var("MNEMOS_DB_PATH");
        std::env::remove_var("MNEMOS_AI_PROVIDER");
    }

    #[test]
    #[serial(env)]
    fn test_cli_overrides_config_file() {
        // CLI arg should take precedence over config file
        // Clear env vars
        std::env::remove_var("MNEMOS_DB_PATH");
        std::env::remove_var("MNEMOS_AI_PROVIDER");

        // Create temp config file
        let tmp = tempfile::TempDir::new().unwrap();
        let config_path = tmp.path().join("config.toml");
        std::fs::write(
            &config_path,
            r#"
            db-path = "/config/db.db"
            ai-provider = "config-provider"
            "#,
        )
        .unwrap();

        // CLI override
        let overrides = CliOverrides {
            db_path: Some("/cli/db.db".to_string()),
            ai_provider: Some("cli-provider".to_string()),
        };

        let config = ResolvedConfig::new(Some(config_path.to_str().unwrap()), overrides);

        // CLI should win over config file
        assert_eq!(config.db_path, "/cli/db.db");
        assert_eq!(config.ai_provider, Some("cli-provider".to_string()));
    }

    #[test]
    #[serial(env)]
    fn test_config_file_used_when_no_cli_or_env() {
        // Config file should be used when no CLI or env var is set
        // Clear env vars
        std::env::remove_var("MNEMOS_DB_PATH");
        std::env::remove_var("MNEMOS_AI_PROVIDER");

        // Create temp config file
        let tmp = tempfile::TempDir::new().unwrap();
        let config_path = tmp.path().join("config.toml");
        std::fs::write(
            &config_path,
            r#"
            db-path = "/config/db.db"
            ai-provider = "config-provider"
            "#,
        )
        .unwrap();

        // No CLI override
        let overrides = CliOverrides::default();

        let config = ResolvedConfig::new(Some(config_path.to_str().unwrap()), overrides);

        // Config file should be used
        assert_eq!(config.db_path, "/config/db.db");
        assert_eq!(config.ai_provider, Some("config-provider".to_string()));
    }

    #[test]
    #[serial(env)]
    fn test_default_used_when_nothing_set() {
        // Default should be used when nothing is set
        // Clear env vars
        std::env::remove_var("MNEMOS_DB_PATH");
        std::env::remove_var("MNEMOS_AI_PROVIDER");

        // Create empty config file
        let tmp = tempfile::TempDir::new().unwrap();
        let config_path = tmp.path().join("config.toml");
        std::fs::write(&config_path, "").unwrap();

        // No CLI override
        let overrides = CliOverrides::default();

        let config = ResolvedConfig::new(Some(config_path.to_str().unwrap()), overrides);

        // Default db_path should end with mnemos.db
        assert!(
            config.db_path.ends_with("mnemos.db"),
            "Expected default db_path to end with mnemos.db, got: {}",
            config.db_path
        );
        // Default ai_provider should be None
        assert_eq!(config.ai_provider, None);
    }
}
