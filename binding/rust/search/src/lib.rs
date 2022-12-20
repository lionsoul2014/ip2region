mod ip_value;

use std::env;
use std::error::Error;
use std::fmt;
use std::fmt::Formatter;
use std::fs::File;
use std::io::Read;

use once_cell::sync::OnceCell;

use ip_value::ToUIntIP;

const HEADER_INFO_LENGTH: u32 = 256;
// const VECTOR_INDEX_ROWS: u32 = 256;
const VECTOR_INDEX_COLS: u32 = 256;
const VECTOR_INDEX_SIZE: u32 = 8;
const SEGMENT_INDEX_SIZE: usize = 14;

pub struct Searcher {
    pub buffer: Vec<u8>,
}

pub fn global_searcher() -> &'static Searcher {
    static SEARCHER: OnceCell<Searcher> = OnceCell::new();
    SEARCHER.get_or_init(|| {
        let xdp_filepath = env::var("XDB_FILEPATH").expect("you must set XDB_FILEPATH for search");
        println!("init xdb searcher at {xdp_filepath}");
        Searcher::new(xdp_filepath.as_str()).unwrap()
    })
}

impl fmt::Display for Searcher {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "searcher_with_len {}", self.buffer.len())
    }
}

pub fn search_by_ip<T>(ip: T) -> Result<String, Box<dyn Error>>
where
    T: ToUIntIP,
{
    let changed_value = ip.to_u32_ip()?;
    search_by_ip_u32(changed_value)
}

pub fn search_by_ip_u32(ip: u32) -> Result<String, Box<dyn Error>> {
    let il0 = (ip >> 24) & 0xFF;
    let il1 = (ip >> 16) & 0xFF;
    let idx = VECTOR_INDEX_SIZE * (il0 * VECTOR_INDEX_COLS + il1);

    let start_point = (HEADER_INFO_LENGTH + idx) as usize;
    let buffer = &global_searcher().buffer;
    let start_ptr = get_u32(buffer, start_point);
    let end_ptr = get_u32(buffer, start_point + 4);
    let mut left: usize = 0;
    let mut right: usize = ((end_ptr - start_ptr) as usize) / SEGMENT_INDEX_SIZE;
    while left <= right {
        let mid = (left + right) >> 1;
        let offset = (start_ptr as usize) + mid * SEGMENT_INDEX_SIZE;
        let buffer_ip_value = buffer_value(offset, SEGMENT_INDEX_SIZE);
        let start_ip = get_u32(buffer_ip_value, 0);
        if ip < start_ip {
            right = mid - 1;
        } else if ip > get_u32(buffer_ip_value, 4) {
            left = mid + 1;
        } else {
            let length = (buffer_ip_value[8] as usize & 0x000000FF)
                | (buffer_ip_value[9] as usize & 0x0000FF00);

            let offset = get_u32(buffer_ip_value, 10);
            let result = buffer_value(offset as usize, length)
                .iter()
                .map(|x| x.to_owned())
                .collect::<Vec<u8>>();
            return Ok(String::from_utf8(result)?);
        }
    }
    Err("not matched".into())
}

pub fn buffer_value(offset: usize, length: usize) -> &'static [u8] {
    &global_searcher().buffer[offset..offset + length]
}

impl Searcher {
    pub fn new(filepath: &str) -> Result<Self, Box<dyn Error>> {
        let mut f = File::open(filepath)?;
        let mut buffer = Vec::new();
        f.read_to_end(&mut buffer)?;
        Ok(Self { buffer })
    }
}

fn get_u32(bytes: &[u8], offset: usize) -> u32 {
    (bytes[offset] as u32) & 0x000000FF
        | ((bytes[offset + 1] as u32) << 8) & 0x0000FF00
        | ((bytes[offset + 2] as u32) << 16) & 0x00FF0000
        | ((bytes[offset + 3] as u32) << 24) & 0xFF000000
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::net::Ipv4Addr;
    use std::str::FromStr;
    use std::thread;

    const TEST_IP_FILEPATH: &str = "../../../data/ip.test.txt";

    ///test all types find correct
    #[test]
    fn test_search_by_ip() {
        search_by_ip("2.0.0.0").unwrap();
        search_by_ip("32").unwrap();
        search_by_ip(32).unwrap();
        search_by_ip(Ipv4Addr::from_str("1.1.1.1").unwrap()).unwrap();
    }

    /// test find ip correct use the file ip.test.txt in ../../data
    #[test]
    fn test_random_choose_ip() {
        let mut file = File::open(TEST_IP_FILEPATH).unwrap();
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
    fn test_multi_thread() {
        let handle = thread::spawn(|| {
            let result = search_by_ip("2.2.2.2").unwrap();
            println!("ip search in spawn: {result}");
        });
        let r = search_by_ip("1.1.1.1").unwrap();
        println!("ip search in main thread: {r}");
        handle.join().unwrap();
    }
}
