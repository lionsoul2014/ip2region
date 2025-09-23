use std::borrow::Cow;
use std::fmt::Display;
use std::fs::File;
use std::io::{Read, Seek, SeekFrom};
use std::sync::OnceLock;

use tracing::{debug, trace, warn};

use crate::ToUIntIP;
use crate::error::{Ip2RegionError, Result};

const HEADER_INFO_LENGTH: usize = 256;
const VECTOR_INDEX_LENGTH: usize = 256 * 256 * 8;
const VECTOR_INDEX_COLS: usize = 256;
const VECTOR_INDEX_SIZE: usize = 8;
const SEGMENT_INDEX_SIZE: usize = 14;

static VECTOR_INDEX_CACHE: OnceLock<Vec<u8>> = OnceLock::new();
static FULL_CACHE: OnceLock<Vec<u8>> = OnceLock::new();

pub struct Searcher {
    pub filepath: String,
    pub cache_policy: CachePolicy,
}

#[derive(PartialEq, Debug)]
pub enum CachePolicy {
    NoCache,
    VectorIndex,
    FullMemory,
}

impl Searcher {
    pub fn new(filepath: String, cache_policy: CachePolicy) -> Self {
        Self {
            filepath,
            cache_policy,
        }
    }

    pub fn search<T>(&self, ip: T) -> Result<String>
    where
        T: ToUIntIP + Display,
    {
        let ip = ip.to_u32_ip()?;
        let il0 = ((ip >> 24) & 0xFF) as usize;
        let il1 = ((ip >> 16) & 0xFF) as usize;
        let start_point = VECTOR_INDEX_SIZE * (il0 * VECTOR_INDEX_COLS + il1);

        let vector_index = self.vector_index()?;
        let start_ptr = get_block_by_size(&vector_index, start_point, 4);
        let end_ptr = get_block_by_size(&vector_index, start_point + 4, 4);

        let mut left: usize = 0;
        let mut right: usize = (end_ptr - start_ptr) / SEGMENT_INDEX_SIZE;

        while left <= right {
            let mid = (left + right) >> 1;
            let offset = start_ptr + mid * SEGMENT_INDEX_SIZE;
            let buffer_ip_value = self.read_buf(offset, SEGMENT_INDEX_SIZE)?;

            let start_ip = get_block_by_size(&buffer_ip_value, 0, 4);
            if ip < (start_ip as u32) {
                right = mid - 1;
            } else if ip > (get_block_by_size(&buffer_ip_value, 4, 4) as u32) {
                left = mid + 1;
            } else {
                let data_length = get_block_by_size(&buffer_ip_value, 8, 2);
                let data_offset = get_block_by_size(&buffer_ip_value, 10, 4);
                let result = String::from_utf8(self.read_buf(data_offset, data_length)?.to_vec())?;
                return Ok(result);
            }
        }
        Err(Ip2RegionError::NoMatchedIP)
    }

    pub fn vector_index(&self) -> Result<Cow<'_, [u8]>> {
        if self.cache_policy.eq(&CachePolicy::NoCache) {
            return self.read_buf(HEADER_INFO_LENGTH, VECTOR_INDEX_LENGTH);
        }

        match VECTOR_INDEX_CACHE.get() {
            None => {
                debug!("Load vector index cache");
                let data = self
                    .read_buf(HEADER_INFO_LENGTH, VECTOR_INDEX_LENGTH)?
                    .to_vec();
                let _ = VECTOR_INDEX_CACHE
                    .set(data)
                    .inspect_err(|_| warn!("Vector index cache already initialized"));

                // Safety: VECTOR_INDEX_CACHE checked and set for empty before
                let cache = VECTOR_INDEX_CACHE.get().unwrap();
                Ok(Cow::Borrowed(cache))
            }
            Some(cache) => Ok(Cow::Borrowed(cache)),
        }
    }

    pub fn read_buf(&self, offset: usize, size: usize) -> Result<Cow<'_, [u8]>> {
        trace!(offset, size = size, "Read buffer");
        if self.cache_policy.ne(&CachePolicy::FullMemory) {
            debug!(filepath=?self.filepath, offset=offset, size=size, "Read buf without cache");
            let mut file = File::open(&self.filepath)?;
            file.seek(SeekFrom::Start(offset as u64))?;

            let mut buf = vec![0u8; size];
            file.take(size as u64).read_exact(&mut buf)?;
            return Ok(Cow::from(buf));
        }

        match FULL_CACHE.get() {
            None => {
                debug!(filepath=?self.filepath, "Load full cache");
                let mut file = File::open(&self.filepath)?;
                let mut buf = Vec::new();
                file.read_to_end(&mut buf)?;
                let _ = FULL_CACHE
                    .set(buf)
                    .inspect_err(|_| warn!("Full cache already initialized"));

                // Safety: FULL_CACHE checked and set for empty before
                let cache = FULL_CACHE.get().unwrap();
                Ok(Cow::from(&cache[offset..offset + size]))
            }
            Some(cache) => {
                let data = Cow::from(&cache[offset..offset + size]);
                Ok(data)
            }
        }
    }
}

#[inline]
pub fn get_block_by_size(bytes: &[u8], offset: usize, length: usize) -> usize {
    let mut result: usize = 0;
    for (index, value) in bytes[offset..offset + length].iter().enumerate() {
        result += usize::from(*value) << (index << 3);
    }
    result
}

#[cfg(test)]
mod tests {
    use std::fs::File;
    use std::io::{BufRead, BufReader};
    use std::net::Ipv4Addr;
    use std::str::FromStr;

    use super::*;

    const XDB_PATH: &str = "../../../data/ip2region_v4.xdb";
    const CHECK_PATH: &str = "../../../data/ipv4_source.txt";

    fn multi_type_ip(searcher: &Searcher) {
        searcher.search("2.0.0.0").unwrap();
        searcher.search("32").unwrap();
        searcher.search(4294408949).unwrap();
        searcher
            .search(Ipv4Addr::from_str("1.1.1.1").unwrap())
            .unwrap();
    }

    ///test all types find correct
    #[test]
    fn test_multi_type_ip() {
        for cache_policy in [
            CachePolicy::NoCache,
            CachePolicy::FullMemory,
            CachePolicy::VectorIndex,
        ] {
            multi_type_ip(&Searcher::new(XDB_PATH.to_owned(), cache_policy));
        }
    }

    fn match_ip_correct(searcher: &Searcher) {
        let file = File::open(CHECK_PATH).unwrap();
        let reader = BufReader::new(file);

        for line in reader.lines().take(100) {
            let line = line.unwrap();

            if !line.contains("|") {
                continue;
            }

            let ip_test_line = line.splitn(3, "|").collect::<Vec<&str>>();
            let start_ip = Ipv4Addr::from_str(ip_test_line[0]).unwrap();
            let end_ip = Ipv4Addr::from_str(ip_test_line[1]).unwrap();
            for _ in 0..10 {
                let value = rand::random_range(u32::from(start_ip)..u32::from(end_ip) + 1);
                let result = searcher.search(value).unwrap();
                assert_eq!(result.as_str(), ip_test_line[2])
            }
        }
    }

    #[test]
    fn test_match_ip_correct() {
        for cache_policy in [
            CachePolicy::NoCache,
            CachePolicy::FullMemory,
            CachePolicy::VectorIndex,
        ] {
            match_ip_correct(&Searcher::new(XDB_PATH.to_owned(), cache_policy));
        }
    }
}
