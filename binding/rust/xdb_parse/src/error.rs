use thiserror::Error;


///XdbError struct
#[derive(Debug, Error)]
pub enum XdbError {
    #[error("Invalid IP: {0}")]
    InvalidIP(String),
    #[error("Invalid IP Version: {0}")]
    InvalidIPVersion(String),
    #[error("Header Parse Error: {0}")]
    HeaderParseError(String),
    #[error("RangeIndexOut:{0}")]
    RangeIndexOutError(String),

    #[error("AddrParseError: {0}")]
    AddrParseError(#[from] std::net::AddrParseError),
    #[error("ParseInt Error: {0}")]
    ParseIntError(#[from] std::num::ParseIntError),
    #[error("Io Error:{0}")]
    IoError(#[from] std::io::Error),
    #[error("InfallibleError: {0}")]
    InfallibleError(#[from] std::convert::Infallible),
    #[error("Invalid Ip Format")]
    InvalidIpUTF8Format(#[from] std::string::FromUtf8Error),
    #[error("Slice Error: {0}")]
    SliceError(#[from] std::array::TryFromSliceError),
}
