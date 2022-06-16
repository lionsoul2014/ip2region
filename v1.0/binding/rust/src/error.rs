use std::{self, io, net, str};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    Utf8(str::Utf8Error),
    Addr(net::AddrParseError),
    /// `224.0.0.0` ~ `239.255.255.255`
    ///
    // `ff00::/8`
    IpIsMulticast,
    /// `0.0.0.0`
    IpIsUnspecified,
    /// `127.0.0.0/8`
    IpIsLoopback,
    ///1. `10.0.0.0/8`
    ///
    ///2. `172.16.0.0/12`
    ///
    ///3. `192.168.0.0/16`
    IpIsPrivate,
    /// Unsupport Ipv6 Now
    UnsupportIpv6,
    NotFound,
}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        Error::Io(e)
    }
}
impl From<str::Utf8Error> for Error {
    fn from(e: str::Utf8Error) -> Self {
        Error::Utf8(e)
    }
}

impl From<net::AddrParseError> for Error {
    fn from(e: net::AddrParseError) -> Self {
        Error::Addr(e)
    }
}
