use clap::{arg, ArgMatches, Command};

pub fn get_matches() -> ArgMatches {
    Command::new("ip2region")
        .version("0.1")
        .about("ip2region bin program")
        .long_about(
            "you can set environment XDB_FILEPATH=../data/ip2region or just use --xdb in command",
        )
        .arg(
            arg!(--xdb <xdb> "the xdb filepath, you can set this field like ../data/ip2region.xdb"),
        )
        .get_matches()
}
