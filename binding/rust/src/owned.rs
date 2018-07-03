use super::*;
use std::borrow::Cow;

#[cfg(feature = "lazy")]
include!("lazy.rs");

#[allow(non_snake_case)]
#[derive(Debug, Default, Clone, PartialEq)]
pub struct OwnedIpInfo {
    pub city_id: u32,
    pub country: String,
    pub region: String,
    pub province: String,
    pub city: String,
    pub ISP: String,
}

impl OwnedIpInfo {
    pub fn as_ref<'a>(&'a self) -> IpInfo<'a> {
        IpInfo {
            city_id: self.city_id,
            country: &self.country,
            region: &self.region,
            province: &self.province,
            city: &self.city,
            ISP: &self.ISP,
        }
    }
}

impl fmt::Display for OwnedIpInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_ref())
    }
}

pub struct OwnedIp2Region {
    // super block index info
    first_index_ptr: u32,
    // last_index_ptr: u32,
    total_blocks: u32,
    db_bin_bytes: Cow<'static, [u8]>,
}

impl OwnedIp2Region {
    pub fn new(path: &str) -> io::Result<Self> {
        let mut file = File::open(path)?;
        Self::new2(&mut file)
    }
    pub(crate) fn new2(file: &mut File) -> io::Result<Self> {
        let file_size = file.metadata()?.len();
        let mut bytes = Vec::with_capacity(file_size as usize);
        file.read_to_end(&mut bytes)?;

        let first_index_ptr = get_u32(&bytes[..], 0);
        let last_index_ptr = get_u32(&bytes[..], 4);
        let total_blocks = (last_index_ptr - first_index_ptr) / INDEX_BLOCK_LENGTH + 1;

        let db_bin_bytes = Cow::Owned(bytes);

        Ok(OwnedIp2Region {
            first_index_ptr,
            total_blocks,
            db_bin_bytes,
        })
    }

    pub fn memory_search<S: AsRef<str>>(&self, ip_str: S) -> Result<IpInfo> {
        let ip = ip_str.as_ref().parse()?;
        self.memory_search_ip(&ip)
    }

    pub fn memory_search_ip(&self, ip_addr: &IpAddr) -> Result<IpInfo> {
        let ip = ip2u32(ip_addr)?;
        let mut h = self.total_blocks;
        let (mut data_ptr, mut l) = (0u32, 0u32);
        while l <= h {
            let m = (l + h) >> 1;
            let p = self.first_index_ptr + m * INDEX_BLOCK_LENGTH;

            let sip = get_u32(&self.db_bin_bytes[..], p);
            if ip < sip {
                h = m - 1;
            } else {
                let eip = get_u32(&self.db_bin_bytes[..], p + 4);
                if ip > eip {
                    l = m + 1;
                } else {
                    data_ptr = get_u32(&self.db_bin_bytes[..], p + 8);
                    break;
                }
            }
        }

        if data_ptr == 0 {
            Err(Error::NotFound)?;
        }

        let data_len = (data_ptr >> 24) & 0xff;
        data_ptr = data_ptr & 0x00FFFFFF;
        get_ip_info(
            get_u32(&self.db_bin_bytes[..], data_ptr),
            &self.db_bin_bytes[(data_ptr + 4) as usize..(data_ptr + data_len) as usize],
        )
    }
}
