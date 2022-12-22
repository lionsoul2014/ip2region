use std::env;
use std::io::Write;
use std::time::Instant;

mod cmd;

fn main() {
    env::var("XDB_FILEPATH").unwrap_or_else(|_| {
        let matches = cmd::get_matches();
        if let Some(xdb_filepath) = matches.get_one::<String>("xdb") {
            env::set_var("XDB_FILEPATH", xdb_filepath);
        }
        "".to_owned()
    });

    ip2region2::global_searcher();
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
        let result = ip2region2::search_by_ip(line.trim());
        println!("region: {:?}, took: {:?}", result, now.elapsed());
    }
}
