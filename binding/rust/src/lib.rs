use std::fs::File;
use std::io::Read;
use std::net::Ipv4Addr;

const HEADER_INFO_LENGTH: u32 = 256;
// const VECTOR_INDEX_ROWS: u32 = 256;
const VECTOR_INDEX_COLS: u32 = 256;
const VECTOR_INDEX_SIZE: u32 = 8;
const SEGMENT_INDEX_SIZE: usize = 14;

pub struct Searcher {
    pub buffer: Vec<u8>,
}

impl Searcher {
    pub fn new(filepath: &'static str) -> Result<Self, Box<dyn std::error::Error>> {
        let mut f = File::open(filepath)?;
        let mut buffer = Vec::new();
        f.read_to_end(&mut buffer)?;
        Ok(Self { buffer })
    }

    pub fn search_by_ip(&self, ip: &'static str) -> Result<String, Box<dyn std::error::Error>> {
        let ip = ip.parse::<Ipv4Addr>().unwrap_or_else(|_| {
            let ip = ip
                .parse::<u32>()
                .expect("ip is not a valid ip or valid int");
            Ipv4Addr::from(ip)
        });

        let ip = u32::from(ip);
        let il0 = (ip >> 24) & 0xFF;
        let il1 = (ip >> 16) & 0xFF;
        let idx = VECTOR_INDEX_SIZE * (il0 * VECTOR_INDEX_COLS + il1);

        let start_point = (HEADER_INFO_LENGTH + idx) as usize;
        let start_ptr = get_u32(&self.buffer, start_point);
        let end_ptr = get_u32(&self.buffer, start_point + 4);
        let mut left: usize = 0;
        let mut right: usize = ((end_ptr - start_ptr) as usize) / SEGMENT_INDEX_SIZE;
        while left <= right {
            let mid = (left + right) >> 1;
            let offset = (start_ptr as usize) + mid * SEGMENT_INDEX_SIZE;
            let buffer_ip_value = self.buffer_value(offset, SEGMENT_INDEX_SIZE);
            let start_ip = get_u32(buffer_ip_value, 0);
            if ip < start_ip {
                right = mid - 1;
            } else if ip > get_u32(buffer_ip_value, 4) {
                left = mid + 1;
            } else {
                let length = (buffer_ip_value[8] as usize & 0x000000FF)
                    | (buffer_ip_value[9] as usize & 0x0000FF00);

                let offset = get_u32(&buffer_ip_value, 10);
                let result = self
                    .buffer_value(offset as usize, length)
                    .iter()
                    .map(|x| x.to_owned())
                    .collect::<Vec<u8>>();
                return Ok(String::from_utf8(result)?);
            }
        }
        Err("not matched".into())
    }

    pub fn buffer_value(&self, offset: usize, length: usize) -> &[u8] {
        &self.buffer[offset..offset + length]
    }
}

fn get_u32(bytes: &[u8], offset: usize) -> u32 {
    let result = (bytes[offset] as u32) & 0x000000FF
        | ((bytes[offset + 1] as u32) << 8) & 0x0000FF00
        | ((bytes[offset + 2] as u32) << 16) & 0x00FF0000
        | ((bytes[offset + 3] as u32) << 24) & 0xFF000000;
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_search_by_ip() {
        let filepath = "../../data/ip2region.xdb";
        let searcher = Searcher::new(filepath).expect("load file error");
        let result = searcher.search_by_ip("2.0.0.0");
        println!("{:?}", result);
    }
}

fn main() {}
