extern crate core;

use std::fs::File;
use std::io::Read;
use std::io::Write;
use std::net::Ipv4Addr;
use std::str::FromStr;
use std::time::Instant;

use clap::ArgMatches;

use xdb::{search_by_ip, searcher_init};

mod cmd;

/// set rust log level, if you don`t want print log, you can skip this
fn log_init() {
    let rust_log_key = "RUST_LOG";
    std::env::var(rust_log_key).unwrap_or_else(|_| {
        std::env::set_var(rust_log_key, "INFO");
        std::env::var(rust_log_key).unwrap()
    });
    tracing_subscriber::fmt::init();
}

fn bench_test(src_filepath: &str) {
    let now = Instant::now();
    let mut count = 0;
    let mut file = File::open(src_filepath).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();

    for line in contents.split('\n') {
        if !line.contains('|') {
            continue;
        }
        let ip_test_line = line.splitn(3, '|').collect::<Vec<&str>>();
        if ip_test_line.len() != 3 {
            panic!("this line {line} don`t have enough `|` for spilt");
        }
        let start_ip = Ipv4Addr::from_str(ip_test_line[0]).unwrap();
        let end_ip = Ipv4Addr::from_str(ip_test_line[1]).unwrap();
        if end_ip < start_ip {
            panic!("start ip({start_ip}) should not be greater than end ip({end_ip})")
        }
        let start_ip = u32::from(start_ip);
        let end_ip = u32::from(end_ip);
        let mid_ip = (((start_ip as u64) + (end_ip as u64)) >> 1) as u32;
        for ip in [
            start_ip,
            ((start_ip as u64 + mid_ip as u64) >> 1) as u32,
            mid_ip,
            ((mid_ip as u64 + end_ip as u64) >> 1) as u32,
            end_ip,
        ] {
            let result = search_by_ip(ip).unwrap();
            assert_eq!(result.as_str(), ip_test_line[2]);
            count += 1;
        }
    }
    println!(
        "Bench finished, total: {count},\
     took: {:?} ,\
      cost: {:?}/op",
        now.elapsed(),
        now.elapsed() / count
    )
}

fn query_test() {
    println!("ip2region xdb searcher test program, type `quit` or `Ctrl + c` to exit");
    loop {
        print!("ip2region>> ");
        std::io::stdout().flush().unwrap();
        let mut line = String::new();
        std::io::stdin().read_line(&mut line).unwrap();
        if line.contains("quit") {
            break;
        }
        let line = line.trim();
        let now = Instant::now();
        let result = search_by_ip(line);
        let cost = now.elapsed();
        println!("region: {result:?}, took: {cost:?}",);
    }
}

fn matches_for_searcher(matches: &ArgMatches) {
    if let Some(xdb_filepath) = matches.get_one::<String>("db") {
        searcher_init(Some(xdb_filepath.to_owned()))
    } else {
        searcher_init(None);
    }
}

fn main() {
    log_init();
    let matches = cmd::get_matches();
    if let Some(sub_matches) = matches.subcommand_matches("bench") {
        matches_for_searcher(sub_matches);
        let src_filepath = sub_matches.get_one::<String>("src").unwrap();

        bench_test(src_filepath);
    }

    if let Some(sub_matches) = matches.subcommand_matches("query") {
        matches_for_searcher(sub_matches);
        query_test()
    }
}
