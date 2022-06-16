extern crate ip2region;

use ip2region::*;
use std::env;
use std::io::{self, *};
use std::time::Instant;

// cargo run --release --features lazy
fn main() {
    let args = env::args().skip(1).collect::<Vec<String>>();

    println!(
        r#"ip2region cli test
+-----------------------------------------------------------------+
| ip2region  [db_file] [alrogrithm]                                               
| format  :  [ip] [alrogrithm]'                                     
| overview:  cargo run --release" 
| usage:     cargo run --release -- [db_file] [alrogrithm]"
| exit:      quit or exit or Ctrl+C                
+-----------------------------------------------------------------+`"#
    );

    if !args.is_empty() {
        let db_path = if args[0] != "." { &args[0] } else { DB_PATH };

        let mut ip2 = Ip2Region::new(db_path).unwrap();
        let ip2o = ip2.to_owned().unwrap();

        let alg = if args.len() > 1 {
            args[1].to_lowercase()
        } else {
            "memory".to_lowercase()
        };
        let mut buf = String::with_capacity(256);

        loop {
            buf.clear();

            print!("ip2region>>");
            io::stdout().flush().unwrap();

            let line = match io::stdin().read_line(&mut buf) {
                Ok(_) => buf.trim(),
                Err(e) => panic!("[Fatal]: Read String from Stdin Error: {:?}", e),
            };

            if line.is_empty() {
                continue;
            }

            if line == "quit" || line == "exit" {
                println!("[Info]: Thanks for your use, Bye.");
                break;
            }

            let ip_alg = line
                .split_whitespace()
                .filter(|s| !s.is_empty())
                .collect::<Vec<&str>>();
            let alg = if ip_alg.len() > 1 {
                ip_alg[1].to_owned()
            } else {
                alg.clone()
            };

            let start = Instant::now();
            let res = match alg.as_str() {
                "memory" => ip2o.memory_search(ip_alg[0]).map(|o| o.to_owned()),
                "binary" => ip2.binary_search(ip_alg[0]),
                "btree" | "b-tree" => ip2.btree_search(ip_alg[0]),

                miss => {
                    eprintln!("Not have the Algorithm: {:?}", miss);
                    continue;
                }
            };
            let end = start.elapsed().subsec_micros();

            match res {
                Ok(i) => {
                    println!("[{:6} {:06} microseconds]: {}", alg, end, i);
                }

                Err(e) => {
                    eprintln!("[Error]: {:?}", e);
                }
            };
        }
    } else {
        overview()
    }
}

fn overview() {
    let mut ip2 = Ip2Region::new(DB_PATH).unwrap();
    let ip2o = ip2.to_owned().unwrap();

    for ip in &[
        "117.136.105.202",
        "47.95.47.253",
        "127.0.0.1",
        "10.0.0.1",
        "1.1.1.1",
    ] {
        let start = Instant::now();
        let res = ip2o.memory_search(ip);
        let end = start.elapsed().subsec_micros();
        println!("memory {:06} microseconds: {:?}", end, res);

        #[cfg(feature = "lazy")]
        {
            let start = Instant::now();
            let res = memory_search(ip);
            let end = start.elapsed().subsec_micros();
            println!("lazy__ {:06} microseconds: {:?}", end, res);
        }

        let start = Instant::now();
        let res = ip2.binary_search(ip);
        let end = start.elapsed().subsec_micros();
        println!("binary {:06} microseconds: {:?}", end, res);

        let start = Instant::now();
        let res = ip2.btree_search(ip);
        let end = start.elapsed().subsec_micros();
        println!("btree  {:06} microseconds: {:?}", end, res);

        println!();
    }
}
