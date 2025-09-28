use std::time::Instant;

use clap::Parser;
use maker::{Command, Maker, Result};
use tracing::info;

/// Ip2Region database structure
/// See https://github.com/lionsoul2014/ip2region/blob/master/maker/golang/xdb/maker.go
fn main() -> Result<()> {
    tracing_subscriber::fmt::init();
    let now = Instant::now();
    let cmd = Command::parse();
    info!(?cmd, "Generate xdb");
    let mut maker = Maker::new(
        cmd.ip_version,
        cmd.index_policy,
        &cmd.src,
        &cmd.dst,
        cmd.filter_fields,
    )?;
    maker.start()?;

    info!(cost_time=?now.elapsed(), "Make completed");
    Ok(())
}
