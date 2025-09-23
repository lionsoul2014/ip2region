use std::borrow::Cow;
use std::net::{IpAddr, Ipv4Addr, Ipv6Addr};
use std::str::FromStr;

use crate::error::{Ip2RegionError, Result};

pub trait IpValueExt {
    fn to_ipaddr(self) -> Result<IpAddr>;
}

impl IpValueExt for &str {
    fn to_ipaddr(self) -> Result<IpAddr> {
        IpAddr::from_str(self).map_err(|_| Ip2RegionError::ParseIpaddressFailed)
    }
}

impl IpValueExt for u32 {
    fn to_ipaddr(self) -> Result<IpAddr> {
        Ok(IpAddr::V4(Ipv4Addr::from(self)))
    }
}

impl IpValueExt for Ipv4Addr {
    fn to_ipaddr(self) -> Result<IpAddr> {
        Ok(IpAddr::V4(self))
    }
}

impl IpValueExt for Ipv6Addr {
    fn to_ipaddr(self) -> Result<IpAddr> {
        Ok(IpAddr::V6(self))
    }
}

impl IpValueExt for u128 {
    fn to_ipaddr(self) -> Result<IpAddr> {
        Ok(IpAddr::V6(Ipv6Addr::from(self)))
    }
}

pub trait CompareExt {
    fn ip_lt(&self, other: Cow<'_, [u8]>) -> bool;
    fn ip_gt(&self, other: Cow<'_, [u8]>) -> bool;
}

impl CompareExt for IpAddr {
    fn ip_lt(&self, other: Cow<'_, [u8]>) -> bool {
        match self {
            IpAddr::V4(ip) => ip.octets() < [other[3], other[2], other[1], other[0]],
            IpAddr::V6(ip) => ip.octets() < other[0..16].try_into().unwrap(),
        }
    }

    fn ip_gt(&self, other: Cow<'_, [u8]>) -> bool {
        match self {
            IpAddr::V4(ip) => ip.octets() > [other[3], other[2], other[1], other[0]],
            IpAddr::V6(ip) => ip.octets() > other[0..16].try_into().unwrap(),
        }
    }
}
