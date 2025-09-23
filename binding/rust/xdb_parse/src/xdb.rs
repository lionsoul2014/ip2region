use std::{
    fmt::Debug,
    net::{IpAddr, Ipv4Addr, Ipv6Addr},
    ops::{Deref, DerefMut}, str::FromStr,
};

use crate::{VECTOR_ROW_SIZE, error::XdbError};

///Header
#[derive(Debug, Default)]
pub struct Header {
    pub xdb_version: u16,
    pub cache_type: u16,
    pub generat_time: u32,
    pub index_base_address: u32,
    pub index_end_address: u32,
}

impl Header {
    pub fn new() -> Self {
        Header {
            xdb_version: 0,
            cache_type: 0,
            generat_time: 0,
            index_base_address: 0,
            index_end_address: 0,
        }
    }
    pub fn try_parse(data: &[u8]) -> Result<Self, XdbError> {
        Ok(Header {
            xdb_version: u16::from_ne_bytes(data[0..2].try_into()?),
            cache_type: u16::from_ne_bytes(data[2..4].try_into()?),
            generat_time: u32::from_ne_bytes(data[4..8].try_into()?),
            index_base_address: u32::from_ne_bytes(data[8..12].try_into()?),
            index_end_address: u32::from_ne_bytes(data[12..16].try_into()?),
        })
    }
}

///VectorIndex block struct
#[derive(Debug, Default)]
pub struct IndexBlock {
    pub index_start_address: u32,
    pub index_end_address: u32,
}

impl IndexBlock {
    pub fn new() -> Self {
        IndexBlock {
            index_start_address: 0,
            index_end_address: 0,
        }
    }
    pub fn try_parse(data: &[u8]) -> Result<Self, XdbError> {
        Ok(IndexBlock {
            index_start_address: u32::from_ne_bytes(data[0..4].try_into()?),
            index_end_address: u32::from_ne_bytes(data[4..8].try_into()?),
        })
    }
}

///Vector index segment struct
#[derive(Debug, Default)]
pub struct VectorIndex(pub Vec<IndexBlock>);

impl VectorIndex {
    pub fn new() -> Self {
        VectorIndex(Vec::with_capacity(VECTOR_ROW_SIZE * VECTOR_ROW_SIZE))
    }
    pub fn try_parse(data: &[u8]) -> Result<Self, XdbError> {
        let mut vec_index = VectorIndex::new();
        for i in 0..(VECTOR_ROW_SIZE * VECTOR_ROW_SIZE) {
            //parse each index block , everyone is 8 bytes
            let start = i * 8;
            let end = start + 8;
            let index_block = IndexBlock::try_parse(&data[start..end])?;
            vec_index.push(index_block);
        }
        Ok(vec_index)
    }
}

impl Deref for VectorIndex {
    type Target = Vec<IndexBlock>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for VectorIndex {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

///Binary index segment struct
#[derive(Default)]
pub struct SegmentIndex<T: FromLeBytes> {
    pub ip_start: T,
    pub ip_end: T,
    pub data_len: u16,
    pub data_ptr: u32,
}

impl<T> SegmentIndex<T>
where
    T: Default + Debug + FromLeBytes,
{
    pub fn new() -> Self {
        SegmentIndex {
            ip_start: T::default(),
            ip_end: T::default(),
            data_len: 0,
            data_ptr: 0,
        }
    }
    pub fn try_parse(data: &[u8]) -> Result<Self, XdbError> {
        let size_of_val = size_of::<T>();
        Ok(SegmentIndex {
            ip_start: T::from_le_bytes(data[0..size_of_val].try_into()?)?,
            ip_end: T::from_le_bytes(data[size_of_val..size_of_val * 2].try_into()?)?,
            data_len: u16::from_ne_bytes(
                data[(size_of_val * 2)..(size_of_val * 2) + 2].try_into()?,
            ),
            data_ptr: u32::from_ne_bytes(
                data[(size_of_val * 2) + 2..((size_of_val * 2) + 2) + 4].try_into()?,
            ),
        })
    }
}

impl std::fmt::Debug for SegmentIndex<u32> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "ip_start:{}, ip_end:{}, data_len:{}, data_ptr:{}",
            IpAddr::V4(Ipv4Addr::from(self.ip_start)).to_string(),
            IpAddr::V4(Ipv4Addr::from(self.ip_end)).to_string(),
            self.data_len,
            self.data_ptr
        )
    }
}

impl std::fmt::Debug for SegmentIndex<u128> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "ip_start:{}, ip_end:{}, data_len:{}, data_ptr:{}",
            IpAddr::V6(Ipv6Addr::from(self.ip_start)).to_string(),
            IpAddr::V6(Ipv6Addr::from(self.ip_end)).to_string(),
            self.data_len,
            self.data_ptr
        )
    }
}

pub trait FromLeBytes: Sized {
    fn from_le_bytes(bytes: &[u8]) -> Result<Self, XdbError>;
}

impl FromLeBytes for u32 {
    fn from_le_bytes(bytes: &[u8]) -> Result<Self, XdbError> {
        Ok(u32::from_ne_bytes(bytes.try_into()?))
    }
}

impl FromLeBytes for u128 {
    fn from_le_bytes(bytes: &[u8]) -> Result<Self, XdbError> {
        Ok(u128::from_ne_bytes(bytes.try_into()?))
    }
}

pub trait ToUSizeIp{
    fn to_usize_ip(&self) -> Result<u128, XdbError>;
}
impl ToUSizeIp for Ipv4Addr {
    fn to_usize_ip(&self) -> Result<u128,XdbError> {
        Ok(u32::from(*self) as u128)
    }
}

impl ToUSizeIp for &str {
    fn to_usize_ip(&self) -> Result<u128, XdbError> {
        // 优先尝试解析 IPv6
        if let Ok(ipv6) = Ipv6Addr::from_str(self) {
            return Ok(u128::from(ipv6));
        }
        // 再尝试解析 IPv4
        if let Ok(ipv4) = Ipv4Addr::from_str(self) {
            return Ok(u32::from(ipv4) as u128);
        }
        // 最后直接解析数字
        Ok(self.parse::<u128>()?)
    }
}


#[cfg(test)]
mod tests {
    use std::net::Ipv4Addr;

    use anyhow::Result;
    #[test]
    fn test_parse_ip_and_calculate_index() -> Result<()> {
        /*
            Index: 152552
            il0: 74, il1: 125
            ip: 1249717091        
         */
        // let ip = "2001:0db8:85a3:0000:0000:8a2e:0370:7334".to_usize_ip().unwrap();
        let ip = "74.125.43.99".parse::<Ipv4Addr>()?.to_bits();
        let il0 = ((ip >> 24) & 0xFF) as usize;
        let il1 = ((ip >> 16) & 0xFF) as usize;
        let idx = il0 * 256 * 8 + il1 * 8;
        println!("Index: {}", idx);
        println!("il0: {}, il1: {}", il0, il1);
        println!("ip: {}", ip);
        Ok(())
    }
}