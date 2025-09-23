use std::time::Instant;

use anyhow::Result;
use xdb_parse::utils::{load_file, search_ip};
fn main() -> Result<()> {
    let start = Instant::now();
    let path = "./assets/ip2region_v4.xdb";
    let data = load_file(path.into())?;
    // xdb_init(Some(path.into())).unwrap();
    let ret = search_ip("73.24.63.66", &data)?;
    // let ret = search_ip("2001:0db8:85a3:0000:0000:8a2e:0370:7334",CACHE_DATA.get().unwrap())?;

    // let ret = search_ip("73.24.63.66",&data)?;
    // let ret = search_ip("2001:0db8:85a3:0000:0000:8a2e:0370:7334",&data)?;
    let time = start.elapsed();
    println!("use time:{:?}-ret:{}", time, ret);
    Ok(())
}
