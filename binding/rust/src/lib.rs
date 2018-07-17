#[cfg(feature = "lazy")]
#[macro_use]
extern crate lazy_static;

use std::cell::RefCell;
use std::fs::File;
use std::io::{self, Read, Seek, SeekFrom};
use std::net::IpAddr;
use std::{fmt, str};

mod db;
mod error;
pub use error::{Error, Result};
mod owned;

#[doc(hidden)]
pub use db::DB_PATH;
pub use owned::{OwnedIp2Region, OwnedIpInfo};

#[cfg(feature = "lazy")]
use db::DB_BYTES;
#[cfg(feature = "lazy")]
pub use owned::{memory_search, memory_search_ip};

const INDEX_BLOCK_LENGTH: u32 = 12;
const TOTAL_HEADER_LENGTH: usize = 8192;

thread_local!(static BUF: RefCell<[u8;256]> = RefCell::new([0;256]));
thread_local!(static BUF_BTREE: RefCell<[u8;TOTAL_HEADER_LENGTH]> = RefCell::new([0;TOTAL_HEADER_LENGTH]));

#[allow(non_snake_case)]
#[derive(Debug, Default, Clone, PartialEq)]
pub struct IpInfo<'a> {
    pub city_id: u32,
    pub country: &'a str,
    pub region: &'a str,
    pub province: &'a str,
    pub city: &'a str,
    pub ISP: &'a str,
}

impl<'a> IpInfo<'a> {
    fn new(city_id: u32, fields: &[&'a str]) -> Self {
        if fields.len() < 5 {
            panic!(format!("invlid fields: {:?}", fields));
        }
        IpInfo {
            country: fields[0],
            region: fields[1],
            province: fields[2],
            city: fields[3],
            ISP: fields[4],
            city_id,
        }
    }
    pub fn to_owned(&self) -> OwnedIpInfo {
        OwnedIpInfo {
            city_id: self.city_id,
            country: self.country.to_owned(),
            region: self.region.to_owned(),
            province: self.province.to_owned(),
            city: self.city.to_owned(),
            ISP: self.ISP.to_owned(),
        }
    }
}

impl<'a> fmt::Display for IpInfo<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}|{}|{}|{}|{}|{}",
            self.city_id, self.country, self.region, self.province, self.city, self.ISP
        )
    }
}

fn get_ip_info(city_id: u32, line: &[u8]) -> Result<IpInfo> {
    let str = str::from_utf8(line)?;
    let fields = str.split('|').collect::<Vec<&str>>();
    Ok(IpInfo::new(city_id, &fields[..]))
}

fn get_u32(bytes: &[u8], offset: u32) -> u32 {
    let offset = offset as usize;
    let tmp = (bytes[offset] as i64) & 0x000000FF
        | ((bytes[offset + 1] as i64) << 8) & 0x0000FF00
        | ((bytes[offset + 2] as i64) << 16) & 0x00FF0000
        | ((bytes[offset + 3] as i64) << 24) & 0xFF000000;
    tmp as u32
}

fn ip2u32(ip: &IpAddr) -> Result<u32> {
    if ip.is_ipv6() {
        return Err(Error::UnsupportIpv6);
    }
    if ip.is_unspecified() {
        return Err(Error::IpIsUnspecified);
    }
    if ip.is_loopback() {
        return Err(Error::IpIsLoopback);
    }
    if ip.is_multicast() {
        return Err(Error::IpIsMulticast);
    }

    match ip {
        IpAddr::V4(v4) => {
            if v4.is_private() {
                return Err(Error::IpIsPrivate);
            }

            let mut sum: u32 = 0;
            for (i, n) in v4.octets().iter().enumerate() {
                sum += (*n as u32) << 24 - 8 * i;
            }
            return Ok(sum);
        }
        IpAddr::V6(_v6) => unreachable!(),
    }
}

pub struct Ip2Region {
    // db file handler
    db_file: File,

    //header block info
    header_sip: Vec<u32>,
    header_ptr: Vec<u32>,
    header_len: u32,

    // super block index info
    first_index_ptr: u32,
    last_index_ptr: u32,
    total_blocks: u32,
}

impl Ip2Region {
    pub fn new(path: &str) -> io::Result<Self> {
        let file = File::open(path)?;
        Ok(Ip2Region {
            db_file: file,
            header_sip: Vec::new(),
            header_ptr: Vec::new(),
            header_len: 0,

            first_index_ptr: 0,
            last_index_ptr: 0,
            total_blocks: 0,
        })
    }
    pub fn to_owned(&mut self) -> Result<OwnedIp2Region> {
        OwnedIp2Region::new2(&mut self.db_file).map_err(Error::Io)
    }

    pub fn binary_search<S: AsRef<str>>(&mut self, ip_str: S) -> Result<OwnedIpInfo> {
        let ip = ip_str.as_ref().parse::<IpAddr>()?;
        self.binary_search_ip(&ip)
    }
    pub fn binary_search_ip(&mut self, ip_addr: &IpAddr) -> Result<OwnedIpInfo> {
        BUF.with(|buf| {
            let mut buf = buf.borrow_mut();

            if self.total_blocks == 0 {
                self.db_file.seek(SeekFrom::Start(0))?;
                self.db_file.read_exact(&mut buf[..8])?;
                self.first_index_ptr = get_u32(&buf[..8], 0);
                self.last_index_ptr = get_u32(&buf[..8], 4);
                self.total_blocks =
                    (self.last_index_ptr - self.first_index_ptr) / INDEX_BLOCK_LENGTH + 1;
            }
            let ip = ip2u32(ip_addr)?;
            let mut h = self.total_blocks;
            let (mut data_ptr, mut l) = (0u32, 0u32);
            while l <= h {
                let m = (l + h) >> 1;
                let p = self.first_index_ptr + m * INDEX_BLOCK_LENGTH;
                self.db_file.seek(SeekFrom::Start(p as u64))?;
                self.db_file
                    .read_exact(&mut buf[0..INDEX_BLOCK_LENGTH as usize])?;
                let sip = get_u32(&buf[..INDEX_BLOCK_LENGTH as usize], 0);
                if ip < sip {
                    h = m - 1;
                } else {
                    let eip = get_u32(&buf[..INDEX_BLOCK_LENGTH as usize], 4);
                    if ip > eip {
                        l = m + 1;
                    } else {
                        data_ptr = get_u32(&buf[..INDEX_BLOCK_LENGTH as usize], 8);
                        break;
                    }
                }
            }
            if data_ptr == 0 {
                Err(Error::NotFound)?;
            }

            let data_len = (data_ptr >> 24) & 0xff;
            data_ptr = data_ptr & 0x00FFFFFF;

            self.db_file.seek(SeekFrom::Start(data_ptr as u64))?;
            self.db_file.read_exact(&mut buf[0..data_len as usize])?;

            get_ip_info(
                get_u32(&buf[..data_len as usize], 0),
                &buf[4..data_len as usize],
            ).map(|i| i.to_owned())
        })
    }

    pub fn btree_search<S: AsRef<str>>(&mut self, ip_str: S) -> Result<OwnedIpInfo> {
        let ip = ip_str.as_ref().parse::<IpAddr>()?;
        self.btree_search_ip(&ip)
    }
    pub fn btree_search_ip(&mut self, ip_addr: &IpAddr) -> Result<OwnedIpInfo> {
        BUF_BTREE.with(|buf| {
            let mut buf = buf.borrow_mut();

            if self.header_len == 0 {
                self.db_file.seek(SeekFrom::Start(8))?;
                self.db_file.read_exact(&mut buf[0..TOTAL_HEADER_LENGTH])?;

                let (mut i, mut idx) = (0, 0);
                while i < TOTAL_HEADER_LENGTH {
                    let sip = get_u32(&buf[0..TOTAL_HEADER_LENGTH], i as u32);
                    let idx_ptr = get_u32(&buf[0..TOTAL_HEADER_LENGTH], i as u32 + 4);
                    if idx_ptr == 0 {
                        break;
                    }
                    self.header_sip.push(sip);
                    self.header_ptr.push(idx_ptr);
                    i += 8;
                    idx += 1;
                }
                self.header_len = idx
            }

            let ip = ip2u32(ip_addr)?;
            let mut h = self.header_len;
            let (mut sptr, mut eptr, mut l) = (0u32, 0u32, 0u32);

            while l <= h {
                let m = (l + h) >> 1;
                if m < self.header_len {
                    if ip == self.header_sip[m as usize] {
                        if m > 0 {
                            sptr = self.header_ptr[m as usize - 1];
                            eptr = self.header_ptr[m as usize];
                        } else {
                            sptr = self.header_ptr[m as usize];
                            eptr = self.header_ptr[m as usize + 1];
                        }
                        break;
                    }
                    if ip < self.header_sip[m as usize] {
                        if m == 0 {
                            sptr = self.header_ptr[m as usize];
                            eptr = self.header_ptr[m as usize + 1];
                            break;
                        } else if ip > self.header_sip[m as usize - 1] {
                            sptr = self.header_ptr[m as usize - 1];
                            eptr = self.header_ptr[m as usize];
                            break;
                        }
                        h = m - 1
                    } else {
                        if m == self.header_len - 1 {
                            println!("m/hl: {}/{}", m, self.header_len);
                            sptr = self.header_ptr[m as usize - 1];
                            eptr = self.header_ptr[m as usize];
                            break;
                        } else if ip <= self.header_sip[m as usize + 1] {
                            sptr = self.header_ptr[m as usize];
                            eptr = self.header_ptr[m as usize + 1];
                            break;
                        }
                        l = m + 1
                    }
                }
            }

            if sptr == 0 {
                Err(Error::NotFound)?;
            }
            let block_len = eptr - sptr;
            self.db_file.seek(SeekFrom::Start(sptr as u64))?;
            let buf_size = (block_len + INDEX_BLOCK_LENGTH) as usize;
            self.db_file.read_exact(&mut buf[..buf_size])?;

            let mut data_ptr = 0;
            h = block_len / INDEX_BLOCK_LENGTH;
            l = 0;

            while l <= h {
                let m = (l + h) >> 1;
                let p = m * INDEX_BLOCK_LENGTH;
                let sip = get_u32(&buf[..buf_size], p);
                if ip < sip {
                    h = m - 1;
                } else {
                    let eip = get_u32(&buf[..buf_size], p + 4);
                    if ip > eip {
                        l = m + 1;
                    } else {
                        data_ptr = get_u32(&buf[..buf_size], p + 8);
                        break;
                    }
                }
            }
            if data_ptr == 0 {
                Err(Error::NotFound)?;
            }

            let data_len = (data_ptr >> 24) & 0xff;
            data_ptr = data_ptr & 0x00FFFFFF;

            self.db_file.seek(SeekFrom::Start(data_ptr as u64))?;
            self.db_file.read_exact(&mut buf[0..data_len as usize])?;

            get_ip_info(
                get_u32(&buf[..data_len as usize], 0),
                &buf[4..data_len as usize],
            ).map(|i| i.to_owned())
        })
    }
}

//  cargo test --features lazy  -- --nocapture
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let mut ip2 = Ip2Region::new(DB_PATH).unwrap();
        let ip2o = ip2.to_owned().unwrap();

        for ip in vec![
            "117.136.105.202",
            "47.95.47.253",
            "127.0.0.1",
            "10.0.0.1",
            "1.1.1.1",
        ] {
            #[cfg(feature = "lazy")]
            {
                println!("lzay__: {:?}", memory_search(ip));
                if ip2o.memory_search(ip).is_ok() {
                    assert_eq!(ip2o.memory_search(ip).unwrap(), memory_search(ip).unwrap());
                } else {
                    assert!(memory_search(ip).is_err());
                }
            }

            println!("memory: {:?}", ip2o.memory_search(ip));
            println!("binary: {:?}", ip2.binary_search(ip));
            println!("btree : {:?}", ip2.btree_search(ip));

            if ip2o.memory_search(ip).is_ok() {
                assert_eq!(
                    ip2o.memory_search(ip).unwrap().to_owned(),
                    ip2.binary_search(ip).unwrap()
                );
                assert_eq!(
                    ip2o.memory_search(ip).unwrap().to_owned(),
                    ip2.btree_search(ip).unwrap()
                );
            } else {
                assert!(ip2.binary_search(ip).is_err());
                assert!(ip2.btree_search(ip).is_err());
            }
            println!();
        }
    }
}
