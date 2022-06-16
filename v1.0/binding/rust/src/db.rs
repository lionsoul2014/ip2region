pub const DB_PATH: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/../../data/ip2region.db");

#[cfg(feature = "lazy")]
pub static DB_BYTES: &'static [u8] = include_bytes!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../../data/ip2region.db"
));
