use std::{self, io, num, str};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    Str(&'static str),
    Io(io::Error),
    Utf8(str::Utf8Error),
    Int(num::ParseIntError),
}

impl From<&'static str> for Error {
    fn from(e: &'static str) -> Self {
        Error::Str(e)
    }
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
impl From<num::ParseIntError> for Error {
    fn from(e: num::ParseIntError) -> Self {
        Error::Int(e)
    }
}
