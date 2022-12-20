use clap::{arg, ArgMatches, Command};

pub fn get_matches() -> ArgMatches {
    Command::new("ip2region")
        .version("0.1")
        .about("ip2region bin program")
        .arg(arg!(--xdb <xdb> "the xdb filepath, you can set this field like ../data/ip2region.xdb").required(true))
        .get_matches()
}
