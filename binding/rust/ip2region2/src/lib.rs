use std::error::Error;
use std::fmt::Display;
use ip_value::ToUIntIP;

mod ip_value;
mod searcher;

pub use searcher::global_searcher;

const HEADER_INFO_LENGTH: usize = 256;
const VECTOR_INDEX_COLS: usize = 256;
const VECTOR_INDEX_SIZE: usize = 8;
const SEGMENT_INDEX_SIZE: usize = 14;


pub fn get_start_end_ptr(ip: u32) -> (usize, usize) {
    let il0 = ((ip >> 24) & 0xFF) as usize;
    let il1 = ((ip >> 16) & 0xFF) as usize;
    let idx = VECTOR_INDEX_SIZE * (il0 * VECTOR_INDEX_COLS + il1);
    let start_point = HEADER_INFO_LENGTH + idx;

    let start_ptr = get_block_by_size(global_searcher().buffer(), start_point, 4);
    let end_ptr = get_block_by_size(global_searcher().buffer(), start_point + 4, 4);
    (start_ptr, end_ptr)
}

/// check https://mp.weixin.qq.com/s/ndjzu0BgaeBmDOCw5aqHUg for details
pub fn search_by_ip<T>(ip: T) -> Result<String, Box<dyn Error>>
where
    T: ToUIntIP + Display,
{
    let ip = ip.to_u32_ip()?;
    let (start_ptr, end_ptr) = get_start_end_ptr(ip);
    let mut left: usize = 0;
    let mut right: usize = (end_ptr - start_ptr) / SEGMENT_INDEX_SIZE;

    while left <= right {
        let mid = (left + right) >> 1;
        let offset = start_ptr + mid * SEGMENT_INDEX_SIZE;
        let buffer_ip_value = buffer_value(offset, SEGMENT_INDEX_SIZE);
        let start_ip = get_block_by_size(buffer_ip_value, 0, 4);
        if ip < (start_ip as u32) {
            right = mid - 1;
        } else if ip > (get_block_by_size(buffer_ip_value, 4, 4) as u32) {
            left = mid + 1;
        } else {
            let data_length = get_block_by_size(buffer_ip_value, 8, 2);
            let data_offset = get_block_by_size(buffer_ip_value, 10, 4);
            let result = String::from_utf8(buffer_value(data_offset, data_length).to_vec());
            return Ok(result?);
        }
    }
    Err("not matched".into())
}

pub fn buffer_value(offset: usize, length: usize) -> &'static [u8] {
    &global_searcher().buffer()[offset..offset + length]
}

#[inline]
pub fn get_block_by_size<T>(bytes: &[T], offset: usize, length: usize) -> usize
where
    T: Clone,
    usize: From<T>,
{
    let mut result: usize = 0;
    for (index, value) in bytes[offset..offset + length].iter().enumerate() {
        result |= usize::from(value.clone()) << (index * 8);
    }
    result
}

#[cfg(test)]
mod tests {
    use std::net::Ipv4Addr;
    use std::str::FromStr;
    use std::thread;
    use std::fs::File;
    use std::io::Read;

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
