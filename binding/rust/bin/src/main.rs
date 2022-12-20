mod cmd;

fn main() {
    let matches = cmd::get_matches();
    let xdb_filepath = matches.get_one::<String>("xdb").unwrap();
    println!("{xdb_filepath}----");
    // let result = search::search_by_ip("1.2.165.128");
    // println!("{:?}", result);
}
