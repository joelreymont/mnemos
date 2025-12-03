//! Tree-sitter integration for Hemis
//!
//! Provides server-side parsing and node tracking for code annotation positioning.
//! Supports bundled grammars (compiled in) and user-installed grammars (loaded from disk).

pub mod config;
mod grammar;
mod parser;
mod position;

pub use config::{config_dir, default_config, load_config, GrammarSource, GrammarSourceLocation, LanguageConfig, LanguageSettings};
pub use grammar::{GrammarRegistry, Language};
pub use parser::ParserService;
pub use position::{
    compute_display_position, compute_hash_at_position, compute_node_path, DisplayPosition,
    PositionReason,
};

use thiserror::Error;

#[derive(Error, Debug)]
pub enum TreeSitterError {
    #[error("Grammar not available for language: {0}")]
    GrammarNotAvailable(String),

    #[error("Failed to load grammar from {path}: {reason}")]
    GrammarLoadFailed { path: String, reason: String },

    #[error("Failed to parse file: {0}")]
    ParseFailed(String),

    #[error("Config error: {0}")]
    ConfigError(String),

    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),
}

pub type Result<T> = std::result::Result<T, TreeSitterError>;
