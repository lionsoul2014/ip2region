use std::env;
use std::io::Write;
use std::time::Instant;

mod cmd;

fn main() {
    env::var("XDB_FILEPATH").unwrap_or_else(|_| {
        let matches = cmd::get_matches();
        let xdb_filepath = matches
            .get_one::<String>("xdb")
            .expect("you must use --xdb in command or set XDB_FILEPATH environment");
        env::set_var("XDB_FILEPATH", xdb_filepath);
        xdb_filepath.to_owned()
    });

    search::global_searcher();
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
        let result = search::search_by_ip(line.trim());
        println!("region: {:?}, took: {:?}", result, now.elapsed());
    }
}
