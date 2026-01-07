use serde::Serialize;

/// Protocol version - bump this when making breaking RPC changes.
/// UIs check this to determine compatibility.
pub const PROTOCOL_VERSION: u32 = 1;

/// Git commit hash embedded at build time.
pub const GIT_HASH: &str = env!("MNEMOS_GIT_HASH");

/// Version info returned by mnemos/version endpoint.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct VersionInfo {
    pub protocol_version: u32,
    pub git_hash: String,
    pub uptime_secs: u64,
    pub connections: usize,
}
