use super::DB_BYTES;

lazy_static! {
    static ref OWNED_IP_2_REGION: OwnedIp2Region = {
        OwnedIp2Region {
            db_bin_bytes: Cow::Borrowed(DB_BYTES),
            first_index_ptr: get_u32(&DB_BYTES[..], 0),
            total_blocks: (get_u32(&DB_BYTES[..], 4) - get_u32(&DB_BYTES[..], 0))
                / INDEX_BLOCK_LENGTH + 1,
        }
    };
}

pub fn memory_search(ip_str: &str) -> Result<IpInfo> {
    OWNED_IP_2_REGION.memory_search(ip_str)
}
