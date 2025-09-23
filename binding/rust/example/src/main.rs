extern crate core;

use std::fs::File;
use std::io::{BufRead, BufReader};
use std::io::Write;
use std::net::Ipv4Addr;
use std::str::FromStr;
use std::time::Instant;

use clap::Parser;
use ip2region::{Searcher, CachePolicy};
use crate::cmd::{Action, CmdCachePolicy, Command};

mod cmd;

fn bench(searcher: &Searcher, check_filepath: &str) {
    let file = File::open(check_filepath).unwrap();
    let reader = BufReader::new(file);

    let lines = reader.lines().take(100_000).collect::<Vec<_>>();
    let now = Instant::now();
    let mut count = 0;

    for line in lines {
        let line = line.unwrap();
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
            let result = searcher.search(ip).unwrap();
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

fn query(searcher: &Searcher) {
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
        let result = searcher.search(line);
        let cost = now.elapsed();
        println!("region: {result:?}, took: {cost:?}",);
    }
}

fn main() {
    tracing_subscriber::fmt::init();

    let cmd = Command::parse();
    let cache_policy = match cmd.cache_policy {
        CmdCachePolicy::FullMemory => CachePolicy::FullMemory,
        CmdCachePolicy::VectorIndex => CachePolicy::VectorIndex,
        CmdCachePolicy::NoCache => CachePolicy::NoCache
    };
    
    let searcher = Searcher::new(cmd.xdb, cache_policy);
    match cmd.action {
        Action::Bench{ check_file} => bench(&searcher, &check_file),
        Action::Query => query(&searcher)
    }
}
