use std::env;
use std::error::Error;
use std::fmt;
use std::fmt::Formatter;
use std::fs::File;
use std::io::Read;
use std::path::Path;

use once_cell::sync::OnceCell;

use ip_value::ToUIntIP;

mod ip_value;

const HEADER_INFO_LENGTH: usize = 256;
const VECTOR_INDEX_COLS: usize = 256;
const VECTOR_INDEX_SIZE: usize = 8;
const SEGMENT_INDEX_SIZE: usize = 14;

/// store the xdb file in memory totally
pub struct Searcher {
    pub buffer: Vec<u8>,
}

impl Searcher {
    /// you can set the XDB_FILEPATH
    /// or super dir has data dir with the file ip2region.xdb
    /// it will check ../data/ip2region.xdb, ../../data/ip2region.xdb, ../../../data/ip2region.xdb
    pub fn new() -> Result<Self, Box<dyn Error>> {
        let xdb_filepath = env::var("XDB_FILEPATH")
            .unwrap_or_else(|_| {
                let prefix = "../".to_owned();
                for recurse in 1..4 {
                    let filepath = prefix.repeat(recurse) + "data/ip2region.xdb";
                    if Path::new(filepath.as_str()).exists() {
                        return filepath
                    }
                };
                panic!("you must set XDB_FILEPATH or put file in ../data/ip2region.xdb")
            });
        println!("load xdb searcher file at {xdb_filepath}");
        let mut f = File::open(xdb_filepath)?;
        let mut buffer = Vec::new();
        f.read_to_end(&mut buffer)?;
        Ok(Self { buffer })
    }
}

/// global init searcher thread safely
pub fn global_searcher() -> &'static Searcher {
    static SEARCHER: OnceCell<Searcher> = OnceCell::new();
    SEARCHER.get_or_init(|| {
        Searcher::new().unwrap()
    })
}

impl fmt::Display for Searcher {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "searcher_with_len {}", self.buffer.len())
    }
}

pub fn get_start_end_ptr(ip: u32) -> (usize, usize) {
    let il0= ((ip >> 24) & 0xFF) as usize;
    let il1 = ((ip >> 16) & 0xFF) as usize;
    let idx = VECTOR_INDEX_SIZE * (il0 * VECTOR_INDEX_COLS + il1);
    let start_point = HEADER_INFO_LENGTH + idx;

    let start_ptr = get_block_by_size(&global_searcher().buffer, start_point, 4);
    let end_ptr = get_block_by_size(&global_searcher().buffer, start_point + 4, 4);
    (start_ptr, end_ptr)
}

/// check https://mp.weixin.qq.com/s/ndjzu0BgaeBmDOCw5aqHUg for details
pub fn search_by_ip<T>(ip: T) -> Result<String, Box<dyn Error>>
where
    T: ToUIntIP,
{
    let ip = ip.to_u32_ip()?;
    let (start_ptr, end_ptr) = get_start_end_ptr(ip);
    let mut left: usize = 0;
    let mut right: usize = (end_ptr - start_ptr) / SEGMENT_INDEX_SIZE;

    while left <= right {
        let mid = (left + right) >> 1;
        let offset = &start_ptr + mid * SEGMENT_INDEX_SIZE;
        let buffer_ip_value = buffer_value(offset, SEGMENT_INDEX_SIZE);
        let start_ip = get_block_by_size(&buffer_ip_value, 0, 4);
        if &ip < &(start_ip as u32) {
            right = mid - 1;
        } else if &ip > &(get_block_by_size(&buffer_ip_value, 4, 4) as u32) {
            left = mid + 1;
        } else {
            let data_length = get_block_by_size(&buffer_ip_value, 8, 2);
            let data_offset = get_block_by_size(&buffer_ip_value, 10, 4);
            let result = String::from_utf8(
                buffer_value(data_offset, data_length)
                    .to_vec());
            return Ok(result?);
        }
    }
    Err("not matched".into())
}

pub fn start_end_buffer_value(bytes: &[u8], offset: usize, length: usize) -> &[u8] {
    &bytes[offset..offset+length]
}

pub fn buffer_value(offset: usize, length: usize) -> &'static [u8] {
    &global_searcher().buffer[offset..offset + length]
}

#[inline]
pub fn get_block_by_size<T>(bytes: &[T], offset: usize, length: usize) -> usize
where
    T: Clone,
    usize: From<T>,
{
    let mut result: usize = 0;
    for (index, value) in bytes[offset..offset+length].iter().enumerate() {
        result |= usize::from(value.clone()) << (index*8);
    }
    result
}

#[cfg(test)]
mod tests {
    use std::net::Ipv4Addr;
    use std::str::FromStr;
    use std::thread;

    use super::*;

    ///test all types find correct
    #[test]
    fn test_multi_type_ip() {
        search_by_ip("2.0.0.0").unwrap();
        search_by_ip("32").unwrap();
        search_by_ip(4294408949).unwrap();
        search_by_ip(Ipv4Addr::from_str("1.1.1.1").unwrap()).unwrap();
    }

    #[test]
    fn test_match_all_ip_correct() {
        let mut file = File::open("../../../data/ip.test.txt").unwrap();
        let mut contents = String::new();
        file.read_to_string(&mut contents).unwrap();
        for line in contents.split("\n") {
            if !line.contains("|") {
                continue;
            }
            let ip_test_line = line.splitn(3, "|").collect::<Vec<&str>>();
            let start_ip = Ipv4Addr::from_str(ip_test_line[0]).unwrap();
            let end_ip = Ipv4Addr::from_str(ip_test_line[1]).unwrap();
            for value in u32::from(start_ip)..u32::from(end_ip) + 1 {
                let result = search_by_ip(value).unwrap();
                assert_eq!(result.as_str(), ip_test_line[2])
            }
        }
    }

    #[test]
    fn test_multi_thread_only_load_xdb_once() {
        let handle = thread::spawn(|| {
            let result = search_by_ip("2.2.2.2").unwrap();
            println!("ip search in spawn: {result}");
        });
        let r = search_by_ip("1.1.1.1").unwrap();
        println!("ip search in main thread: {r}");
        handle.join().unwrap();
    }
}
