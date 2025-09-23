use clap::{Parser, Subcommand, ValueEnum};

/// Rust binding example for ip2region
///
/// `cargo run -- --xdb=../../../data/ip2region_v4.xdb bench ../../../data/ip.test.txt`
///
/// `cargo run -- --xdb=../../../data/ip2region_v4.xdb query`
///
#[derive(Parser)]
pub struct Command {
    /// xdb filepath, e.g. `../../../data/ip2region_v4.xdb`
    #[arg(long, env = "XDB")]
    pub xdb: String,
    #[arg(long, value_enum, default_value_t = CmdCachePolicy::FullMemory)]
    pub cache_policy: CmdCachePolicy,
    #[clap(subcommand)]
    pub action: Action,
}

#[derive(Subcommand)]
pub enum Action {
    /// Bench the ip search and output performance info
    Bench { check_file: String},
    /// Interactive input and output, querying one IP and get result at a time
    Query,
}

#[derive(Debug, PartialEq, ValueEnum, Clone, Copy, Default)]
pub enum CmdCachePolicy {
    #[default]
    FullMemory,
    NoCache,
    VectorIndex,
}
