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

pub fn memory_search<S: AsRef<str>>(ip_str: S) -> Result<IpInfo<'static>> {
    OWNED_IP_2_REGION.memory_search(ip_str)
}

pub fn memory_search_ip(ip_addr: &IpAddr) -> Result<IpInfo> {
    OWNED_IP_2_REGION.memory_search_ip(ip_addr)
}
