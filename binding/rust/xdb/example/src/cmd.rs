use clap::{Arg, ArgMatches, Command};

pub fn get_matches() -> ArgMatches {
    let db_arg = Arg::new("db")
        .long("db")
        .help("the xdb filepath, you can set this field like \
        ../data/ip2region.xdb,if you dont set,\
         if will detect xdb file on ../data/ip2region.xdb, ../../data/ip2region.xdb, ../../../data/ip2region.xdb if exists");

    Command::new("ip2region")
        .version("0.1")
        .about("ip2region bin program")
        .long_about("you can set --db in command to specific the xdb filepath, default run query")
        .subcommand(Command::new("query").about("query test").arg(&db_arg))
        .subcommand(
            Command::new("bench")
                .about("bench test")
                .arg(
                    Arg::new("src")
                        .long("src")
                        .help("set this to specific source bench file")
                        .required(true),
                )
                .arg(&db_arg),
        )
        .get_matches()
}
