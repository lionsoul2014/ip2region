pub mod error;
pub mod utils;
pub mod xdb;

pub (crate) const  TOTAL_HEADER_SIZE: usize = 256;

//VectorIndex row and column size
pub (crate) const  VECTOR_COL_SIZE: usize = 256;
pub (crate) const  VECTOR_ROW_SIZE: usize = 256;
pub (crate) const  VECTOR_INDEX_BLOCK_SIZE: usize = 8;
//VectorIndex total size
pub (crate) const  TOTAL_VECTOR_INDEX_SIZE: usize =
    VECTOR_COL_SIZE * VECTOR_ROW_SIZE * VECTOR_INDEX_BLOCK_SIZE;

//last segments,ipv4 and ipv6 block size 
pub (crate) const  IPV4_SEGMENT_INDEX_BLOCK_SIZE: u32 = 14;
pub (crate) const  IPV6_SEGMENT_INDEX_BLOCK_SIZE: u32 = 38;


pub use utils::{load_file,search_ip};
