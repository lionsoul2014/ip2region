use std::env;
use std::error::Error;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::fs::File;
use std::io::Read;
use std::path::Path;

use once_cell::sync::OnceCell;

pub enum CachePolicy {
    Never,
    VecIndex,
    Full,
}

/// store the xdb file in memory totally
pub struct Searcher {
    vec_cache: Vec<u8>,
    full_cache: Vec<u8>,
}

impl Searcher {
    pub fn new(xdb_filepath: Option<&str>, cache_policy: Option<CachePolicy>) -> Result<Self, Box<dyn Error>> {
        let xdb_filepath = xdb_filepath.unwrap_or_else(|_| {
            Searcher::default_detect_xdb_file().unwrap().as_str()
        });
        println!("load xdb searcher file at {xdb_filepath}");
        let mut f = File::open(xdb_filepath)?;
        let mut buffer = Vec::new();
        f.read_to_end(&mut buffer)?;
        Ok(Self { buffer })
    }

    /// it will check ../data/ip2region.xdb, ../../data/ip2region.xdb, ../../../data/ip2region.xdb
    fn default_detect_xdb_file() -> Result<String, Box<dyn Error>> {
        let prefix = "../".to_owned();
        for recurse in 1..4 {
            let filepath = prefix.repeat(recurse) + "data/ip2region.xdb";
            if Path::new(filepath.as_str()).exists() {
                return Ok(filepath);
            }
        }
        Err("default filepath not find the xdb file, so you must set xdb_filepath".into())
    }

    pub fn buffer(&self) -> &Vec<u8> {
        self.full_cache.as_ref()
    }

    pub fn vec_cache(&self) -> &Vec<u8> {
        self.vec_cache.as_ref()
    }
}

/// global init searcher thread safely
pub fn global_searcher() -> &'static Searcher {
    static SEARCHER: OnceCell<Searcher> = OnceCell::new();
    SEARCHER.get_or_init(|| Searcher::new().unwrap())
}

impl Display for Searcher {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "searcher_with_len {}", self.buffer.len())
    }
}
