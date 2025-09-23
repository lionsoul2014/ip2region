use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

use crate::error::Ip2RegionError;

pub const HEADER_INFO_LENGTH: usize = 256;

#[allow(dead_code)]
#[derive(Debug)]
pub struct Header {
    version: u16,
    index_policy: IndexPolicy,
    create_time: u32,
    start_index_ptr: u32,
    end_index_ptr: u32,
    ip_version: IpVersion,
    runtime_ptr_bytes: u16,
}

impl TryFrom<&[u8; 256]> for Header {
    type Error = Ip2RegionError;

    fn try_from(value: &[u8; 256]) -> Result<Self, Self::Error> {
        if value.len() < 20 {
            return Err(Ip2RegionError::HeaderParsed(
                "Header bytes too short".into(),
            ));
        }

        let index_policy_value = u16::from_le_bytes([value[2], value[3]]);
        let ip_version_value = u16::from_le_bytes([value[16], value[17]]);

        Ok(Header {
            version: u16::from_le_bytes([value[0], value[1]]),
            index_policy: IndexPolicy::from_u16(index_policy_value).ok_or_else(|| {
                Ip2RegionError::HeaderParsed(format!(
                    "Header index policy invalid: {index_policy_value}"
                ))
            })?,
            create_time: u32::from_le_bytes([value[4], value[5], value[6], value[7]]),
            start_index_ptr: u32::from_le_bytes([value[8], value[9], value[10], value[11]]),
            end_index_ptr: u32::from_le_bytes([value[12], value[13], value[14], value[15]]),

            ip_version: IpVersion::from_u16(ip_version_value).ok_or_else(|| {
                Ip2RegionError::HeaderParsed(format!(
                    "Header ip version invalid: {ip_version_value}"
                ))
            })?,
            runtime_ptr_bytes: u16::from_le_bytes([value[18], value[19]]),
        })
    }
}

#[derive(FromPrimitive, Debug)]
#[repr(u16)]
pub enum IndexPolicy {
    VectorIndex = 1,
    BTreeIndex = 2,
}

#[derive(FromPrimitive, Debug)]
#[repr(u16)]
pub enum IpVersion {
    V4 = 4,
    V6 = 6,
}

impl Header {
    pub fn bytes_len(&self) -> usize {
        match &self.ip_version {
            IpVersion::V4 => 4,
            IpVersion::V6 => 16,
        }
    }

    pub fn segment_index_size(&self) -> usize {
        match &self.ip_version {
            IpVersion::V4 => 14,
            IpVersion::V6 => 38,
        }
    }

    pub fn ip_version(&self) -> &IpVersion {
        &self.ip_version
    }
}
