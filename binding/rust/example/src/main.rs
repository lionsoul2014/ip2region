use std::fs::File;
use std::io::Write;
use std::io::{BufRead, BufReader};
use std::net::IpAddr;
use std::str::FromStr;
use std::time::Instant;

use clap::Parser;
use ip2region::{CachePolicy, Searcher};
use tracing::info;

use crate::cmd::{Action, CmdCachePolicy, Command};

mod cmd;

macro_rules! perform_check {
    ($searcher:expr, $start_ip:expr, $end_ip:expr, $check:expr) => {{
        let start_ip = $start_ip;
        let end_ip = $end_ip;

        let mid_ip = (start_ip >> 1) + (end_ip >> 1);

        let mut checked = 0;
        let checks = [
            start_ip,
            (start_ip >> 1) + (mid_ip >> 1),
            mid_ip,
            (mid_ip >> 1) + (end_ip >> 1),
            end_ip,
        ];
        for ip in checks.iter() {
            if *ip < start_ip || *ip > end_ip {
                // IP not in start - end ip
                // This happens when start ip equals end ip
                continue;
            }
            let result = $searcher.search(*ip).unwrap();
            assert_eq!(result.as_str(), $check);
            checked += 1;
        }
        checked
    }};
}

fn check(searcher: &Searcher, start_ip: IpAddr, end_ip: IpAddr, check: &str) -> usize {
    match (start_ip, end_ip) {
        (IpAddr::V4(original_start_ip), IpAddr::V4(original_end_ip)) => {
            let start_ip = u32::from(original_start_ip);
            let end_ip = u32::from(original_end_ip);
            perform_check!(searcher, start_ip, end_ip, check)
        }
        (IpAddr::V6(original_start_ip), IpAddr::V6(original_end_ip)) => {
            let start_ip = u128::from(original_start_ip);
            let end_ip = u128::from(original_end_ip);
            perform_check!(searcher, start_ip, end_ip, check)
        }
        _ => panic!("invalid start ip and end ip"),
    }
}

fn bench(searcher: &Searcher, check_filepath: &str) {
    let file = File::open(check_filepath).unwrap();
    let reader = BufReader::new(file);

    let now = Instant::now();
    let mut count = 0;

    for line in reader.lines().map_while(Result::ok) {
        let ip_test_line = line.splitn(3, '|').collect::<Vec<&str>>();
        if ip_test_line.len() == 3 {
            let start_ip = IpAddr::from_str(ip_test_line[0]).unwrap();
            let end_ip = IpAddr::from_str(ip_test_line[1]).unwrap();
            count += check(searcher, start_ip, end_ip, ip_test_line[2]);
        }
    }
    info!(count, took=?now.elapsed(), avg_took=?(now.elapsed() / (count as u32)), "Benchmark finished");
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
        CmdCachePolicy::NoCache => CachePolicy::NoCache,
    };

    let searcher = Searcher::new(cmd.xdb, cache_policy).unwrap();
    match cmd.action {
        Action::Bench { check_file } => bench(&searcher, &check_file),
        Action::Query => query(&searcher),
    }
}
