use std::io::Write;
use std::time::Instant;

use ip2region2::{searcher_init, search_by_ip};

mod cmd;

fn main() {
    /// set rust log level
    let rust_log_key = "RUST_LOG";
    std::env::var(rust_log_key).unwrap_or_else(|_| {
        std::env::set_var(rust_log_key, "INFO");
        std::env::var(rust_log_key).unwrap()
    });
    tracing_subscriber::fmt::init();

    /// init default xdb_filepath config
    /// if value if None, if will detect xdb file on ../data/ip2region.xdb, ../../data/ip2region.xdb, ../../../data/ip2region.xdb if exists
    let matches = cmd::get_matches();
    if let Some(xdb_filepath) = matches.get_one::<String>("xdb") {
        searcher_init(Some(xdb_filepath.to_owned()))
    } else {
        searcher_init(None);
    }


    println!("ip2region xdb searcher test program, type `quit` or `Ctrl + c` to exit");
    loop {
        print!("ip2region>> ");
        std::io::stdout().flush().unwrap();
        let mut line = String::new();
        std::io::stdin().read_line(&mut line).unwrap();
        if line.contains("quit") {
            break;
        }
        let now = Instant::now();
        let result = search_by_ip(line.trim());
        println!("region: {:?}, took: {:?}", result, now.elapsed());
    }
}
