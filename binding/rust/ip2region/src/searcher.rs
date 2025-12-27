use std::borrow::Cow;
use std::fmt::Display;
use std::fs::File;
use std::io::{Read, Seek, SeekFrom};
use std::net::IpAddr;
use std::path::Path;
use std::sync::OnceLock;

use maker::{
    HEADER_INFO_LENGTH, Header, IpVersion, VECTOR_INDEX_COLS, VECTOR_INDEX_LENGTH,
    VECTOR_INDEX_SIZE,
};
use tracing::{debug, trace, warn};

use crate::error::{Ip2RegionError, Result};
use crate::ip_value::{CompareExt, IpValueExt};

pub struct Searcher {
    pub filepath: String,
    pub cache_policy: CachePolicy,
    pub header: Header,
    vector_cache: OnceLock<Vec<u8>>,
    full_cache: OnceLock<Vec<u8>>,
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum CachePolicy {
    NoCache,
    VectorIndex,
    FullMemory,
}

impl Searcher {
    pub fn new(filepath: String, cache_policy: CachePolicy) -> Result<Self> {
        let mut file = File::open(Path::new(&filepath))?;
        let mut buf = [0; HEADER_INFO_LENGTH];
        file.read_exact(&mut buf)?;

        let header = Header::try_from(&buf)?;
        debug!(?header, "Load xdb file with header");

        Ok(Self {
            filepath,
            cache_policy,
            header,
            vector_cache: OnceLock::new(),
            full_cache: OnceLock::new(),
        })
    }

    pub fn search<T>(&self, ip: T) -> Result<String>
    where
        T: IpValueExt + Display,
    {
        let ip = ip.to_ipaddr()?;

        let (il0, il1) = match (ip, self.header.ip_version()) {
            (IpAddr::V6(ip), IpVersion::V6) => (ip.octets()[0], ip.octets()[1]),
            (IpAddr::V4(ip), IpVersion::V4) => (ip.octets()[0], ip.octets()[1]),
            (_, IpVersion::V4) => return Err(Ip2RegionError::OnlyIPv4Version),
            (_, IpVersion::V6) => return Err(Ip2RegionError::OnlyIPv6Version),
        };

        let start_point = VECTOR_INDEX_SIZE * ((il0 as usize) * VECTOR_INDEX_COLS + (il1 as usize));
        let vector_index = self.vector_index()?;
        let start_ptr =
            u32::from_le_bytes(vector_index[start_point..start_point + 4].try_into()?) as usize;
        let end_ptr =
            u32::from_le_bytes(vector_index[start_point + 4..start_point + 8].try_into()?) as usize;

        // Binary search the segment index to get the region
        let segment_index_size = self.header.segment_index_size();
        let ip_bytes_len = self.header.ip_bytes_len();
        let ip_end_offset = ip_bytes_len * 2;

        let mut left: usize = 0;
        let mut right: usize = (end_ptr - start_ptr) / segment_index_size;

        while left <= right {
            let mid = (left + right) >> 1;
            let offset = start_ptr + mid * segment_index_size;
            let buffer_ip_value = self.read_buf(offset, segment_index_size)?;
            if ip.ip_lt(Cow::Borrowed(&buffer_ip_value[0..ip_bytes_len])) {
                let Some(m) = mid.checked_sub(1) else {
                    break
                };
                right = m;
            } else if ip.ip_gt(Cow::Borrowed(&buffer_ip_value[ip_bytes_len..ip_end_offset])) {
                left = mid + 1;
            } else {
                let data_length = u16::from_le_bytes([
                    buffer_ip_value[ip_end_offset],
                    buffer_ip_value[ip_end_offset + 1],
                ]);
                let data_offset = u32::from_le_bytes(
                    buffer_ip_value[ip_end_offset + 2..ip_end_offset + 6].try_into()?,
                );
                let result = String::from_utf8(
                    self.read_buf(data_offset as usize, data_length as usize)?
                        .to_vec(),
                )?;
                return Ok(result);
            }
        }
        // From xdb 3.0 version no matched IP result change to empty string,
        // so users should check string is empty.
        //
        // Err(Ip2RegionError::NoMatchedIP)
        Ok(String::new())
    }

    pub fn vector_index(&self) -> Result<Cow<'_, [u8]>> {
        if self.cache_policy.eq(&CachePolicy::NoCache) {
            return self.read_buf(HEADER_INFO_LENGTH, VECTOR_INDEX_LENGTH);
        }

        match self.vector_cache.get() {
            None => {
                debug!("Load vector index cache");
                let data = self
                    .read_buf(HEADER_INFO_LENGTH, VECTOR_INDEX_LENGTH)?
                    .to_vec();
                let _ = self
                    .vector_cache
                    .set(data)
                    .inspect_err(|_| warn!("Vector index cache already initialized"));

                // Safety: vector cache checked and set for empty before
                let cache = self.vector_cache.get().unwrap();
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

        match self.full_cache.get() {
            None => {
                debug!(filepath=?self.filepath, "Load full cache");
                let mut file = File::open(&self.filepath)?;
                let mut buf = Vec::new();
                file.read_to_end(&mut buf)?;
                let _ = self
                    .full_cache
                    .set(buf)
                    .inspect_err(|_| warn!("Full cache already initialized"));

                // Safety: FULL_CACHE checked and set for empty before
                let cache = self.full_cache.get().unwrap();
                Ok(Cow::from(&cache[offset..offset + size]))
            }
            Some(cache) => {
                let data = Cow::from(&cache[offset..offset + size]);
                Ok(data)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fs::File;
    use std::io::{BufRead, BufReader};
    use std::str::FromStr;

    use super::*;

    // Test ipv6 need after run command `git lfs pull`
    const IPV4_XDB_PATH: &str = "../../../data/ip2region_v4.xdb";
    const IPV4_CHECK_PATH: &str = "../../../data/ipv4_source.txt";
    const IPV6_XDB_PATH: &str = "../../../data/ip2region_v6.xdb";
    const IPV6_CHECK_PATH: &str = "../../../data/ipv6_source.txt";

    ///test all types find correct
    #[test]
    fn test_multi_type_ip() {
        for cache_policy in [
            CachePolicy::NoCache,
            CachePolicy::FullMemory,
            CachePolicy::VectorIndex,
        ] {
            let searcher = Searcher::new(IPV4_XDB_PATH.to_owned(), cache_policy).unwrap();
            searcher.search("1.0.1.0").unwrap();
            searcher.search("1.0.1.2").unwrap();
            searcher.search(0u32).unwrap();

            let searcher = Searcher::new(IPV6_XDB_PATH.to_owned(), cache_policy).unwrap();
            searcher.search("2c0f:fff1::").unwrap();
            searcher.search("2c0f:fff1::1").unwrap();
            searcher.search(111u128).unwrap();
        }
    }

    fn match_ip_correct(xdb_filepath: &str, check_path: &str, cache_policy: CachePolicy) {
        let searcher = Searcher::new(xdb_filepath.to_owned(), cache_policy).unwrap();

        let file = File::open(check_path).unwrap();
        let reader = BufReader::new(file);

        for line in reader.lines().take(10_000) {
            let line = line.unwrap();

            if !line.contains("|") {
                continue;
            }

            let ip_test_line = line.splitn(3, "|").collect::<Vec<&str>>();
            let start_ip = IpAddr::from_str(ip_test_line[0]).unwrap();
            let end_ip = IpAddr::from_str(ip_test_line[1]).unwrap();
            for _ in 0..3 {
                let result = match (start_ip, end_ip) {
                    (IpAddr::V4(start), IpAddr::V4(end)) => {
                        let value = rand::random_range(u32::from(start)..u32::from(end) + 1);
                        searcher.search(value).unwrap()
                    }
                    (IpAddr::V6(start), IpAddr::V6(end)) => {
                        let value = rand::random_range(u128::from(start)..u128::from(end) + 1);
                        searcher.search(value).unwrap()
                    }
                    _ => panic!("invalid ip address"),
                };
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
            match_ip_correct(IPV4_XDB_PATH, IPV4_CHECK_PATH, cache_policy);
            match_ip_correct(IPV6_XDB_PATH, IPV6_CHECK_PATH, cache_policy);
        }
    }
}
