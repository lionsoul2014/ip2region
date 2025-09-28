#[derive(Debug, thiserror::Error)]
pub enum MakerError {
    #[error("Io error: {0}")]
    IoError(#[from] std::io::Error),

    #[error("Header parse error: {0}")]
    HeaderParsed(String),

    #[error("Parse line src ip, dst ip, region failed for line: {0}")]
    ParseIPRegion(String),

    #[error("Invalid sip/eip version")]
    InvalidIPVersion,

    #[error("Ipaddr parse error: {0}")]
    IpaddrParseError(#[from] std::net::AddrParseError),

    #[error("Region filter fields value too big, limit: {limit}, actual: {actual}")]
    RegionFilterFieldsTooBig { limit: usize, actual: usize },

    #[error("Empty segments")]
    EmptySegments,

    #[error("Try from int failed")]
    TryFromIntError(#[from] std::num::TryFromIntError),

    #[error("Try from slice failed")]
    TryFromSliceFailed(#[from] std::array::TryFromSliceError),

    #[error("Region could not found")]
    RegionNotFound,
}

pub type Result<T> = std::result::Result<T, MakerError>;
