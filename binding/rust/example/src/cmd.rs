use clap::{Parser, Subcommand, ValueEnum};

/// Rust binding example for ip2region
///
/// e.g
///
/// ```
///
/// export XDB='../../../data/ip2region_v4.xdb'  ## or export XDB='../../../data/ip2region_v6.xdb'
///
/// export CHECK='../../../data/ipv4_source.txt'  ## or export CHECK='../../../data/ipv6_source.txt'
///
/// cd binding/rust/example
///
/// ./searcher --xdb=$XDB bench $CHECK
///
/// ./searcher --xdb=$XDB query
///
/// ```
#[derive(Parser)]
pub struct Command {
    /// xdb filepath, e.g. `../../../data/ip2region_v4.xdb` or `../../../data/ip2region_v6.xdb`
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
    Bench { check_file: String },
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
