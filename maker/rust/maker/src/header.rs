use std::fmt::Display;
use std::net::IpAddr;

use bytes::{BufMut, Bytes, BytesMut};
use clap::ValueEnum;
use itertools::Itertools;
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

use crate::error::{MakerError, Result};

pub const VERSION_NO: u16 = 3; // since 2025/09/01 (IPv6 supporting)
pub const HEADER_INFO_LENGTH: usize = 256;
pub const VECTOR_INDEX_COLS: usize = 256;
pub const VECTOR_INDEX_ROWS: usize = 256;
pub const VECTOR_INDEX_SIZE: usize = 8;
pub const VECTOR_INDEX_LENGTH: usize = VECTOR_INDEX_COLS * VECTOR_INDEX_ROWS * VECTOR_INDEX_SIZE;
pub const RUNTIME_PTR_SIZE: u16 = 4;
pub const REGION_START: u64 = (HEADER_INFO_LENGTH + VECTOR_INDEX_LENGTH) as u64;

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
    type Error = MakerError;

    fn try_from(value: &[u8; 256]) -> Result<Self> {
        if value.len() < 20 {
            return Err(MakerError::HeaderParsed("Header bytes too short".into()));
        }

        let index_policy_value = u16::from_le_bytes([value[2], value[3]]);
        let ip_version_value = u16::from_le_bytes([value[16], value[17]]);

        Ok(Header {
            version: u16::from_le_bytes([value[0], value[1]]),
            index_policy: IndexPolicy::from_u16(index_policy_value).ok_or_else(|| {
                MakerError::HeaderParsed(format!(
                    "Header index policy invalid: {index_policy_value}"
                ))
            })?,
            create_time: u32::from_le_bytes([value[4], value[5], value[6], value[7]]),
            start_index_ptr: u32::from_le_bytes([value[8], value[9], value[10], value[11]]),
            end_index_ptr: u32::from_le_bytes([value[12], value[13], value[14], value[15]]),

            ip_version: IpVersion::from_u16(ip_version_value).ok_or_else(|| {
                MakerError::HeaderParsed(format!("Header ip version invalid: {ip_version_value}"))
            })?,
            runtime_ptr_bytes: u16::from_le_bytes([value[18], value[19]]),
        })
    }
}

impl Header {
    pub fn new(index_policy: IndexPolicy, ip_version: IpVersion) -> Header {
        Header {
            version: VERSION_NO,
            index_policy,
            create_time: chrono::Utc::now().timestamp() as u32,
            start_index_ptr: 0,
            end_index_ptr: 0,
            ip_version,
            runtime_ptr_bytes: RUNTIME_PTR_SIZE,
        }
    }

    pub fn encode_bytes(&self, start_index_ptr: u32, end_index_ptr: u32) -> Bytes {
        let mut buf = BytesMut::with_capacity(HEADER_INFO_LENGTH);
        buf.put_u16_le(VERSION_NO);
        buf.put_u16_le(self.index_policy as u16);
        buf.put_u32_le(self.create_time);
        buf.put_u32_le(start_index_ptr);
        // index block end ptr
        buf.put_u32_le(end_index_ptr);
        buf.put_u16_le(self.ip_version as u16);
        buf.put_u16_le(self.runtime_ptr_bytes);
        buf.freeze()
    }
}

#[derive(FromPrimitive, Debug, Copy, Clone, ValueEnum)]
#[repr(u16)]
pub enum IndexPolicy {
    VectorIndex = 1,
    BTreeIndex = 2,
}

impl Display for IndexPolicy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IndexPolicy::VectorIndex => write!(f, "VectorIndex"),
            IndexPolicy::BTreeIndex => write!(f, "BTreeIndex"),
        }
    }
}

#[derive(FromPrimitive, Debug, Copy, Clone, ValueEnum, PartialEq)]
#[repr(u16)]
pub enum IpVersion {
    /// IPv4
    V4 = 4,
    /// Ipv6
    V6 = 6,
}

impl IpVersion {
    pub fn ip_bytes_len(&self) -> usize {
        match &self {
            IpVersion::V4 => 4,
            IpVersion::V6 => 16,
        }
    }

    pub fn segment_index_size(&self) -> usize {
        match &self {
            IpVersion::V4 => 14,
            IpVersion::V6 => 38,
        }
    }
}

impl Header {
    pub fn ip_bytes_len(&self) -> usize {
        self.ip_version.ip_bytes_len()
    }

    pub fn segment_index_size(&self) -> usize {
        self.ip_version.segment_index_size()
    }

    pub fn ip_version(&self) -> &IpVersion {
        &self.ip_version
    }
}

pub trait IPAddrExt {
    fn ipaddr_bytes(&self) -> Vec<u8>;

    fn encode_ipaddr_bytes(&self) -> Vec<u8>;
}

impl IPAddrExt for IpAddr {
    fn ipaddr_bytes(&self) -> Vec<u8> {
        match self {
            IpAddr::V4(addr) => addr.octets().to_vec(),
            IpAddr::V6(addr) => addr.octets().to_vec(),
        }
    }

    fn encode_ipaddr_bytes(&self) -> Vec<u8> {
        match self {
            IpAddr::V4(addr) => addr.octets().into_iter().rev().collect_vec(),
            IpAddr::V6(addr) => addr.octets().to_vec(),
        }
    }
}
