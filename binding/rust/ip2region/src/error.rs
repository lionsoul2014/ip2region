#[derive(Debug, thiserror::Error)]
pub enum Ip2RegionError {
    #[error("Io error: {0}")]
    IoError(#[from] std::io::Error),

    #[error("From UTF-8 error: {0}")]
    Utf8Error(#[from] std::string::FromUtf8Error),

    #[error("Parse invalid IP address")]
    ParseIpaddress(#[from] std::num::ParseIntError),

    #[error("No matched Ipaddress")]
    NoMatchedIP,
}

pub type Result<T> = std::result::Result<T, Ip2RegionError>;
