use std::{
    fs::File,
    io::Read,
    net::{IpAddr, Ipv4Addr, Ipv6Addr},
    path::PathBuf,
    str::FromStr,
};

use crate::{
    TOTAL_HEADER_SIZE, IPV4_SEGMENT_INDEX_BLOCK_SIZE, IPV6_SEGMENT_INDEX_BLOCK_SIZE,
    TOTAL_VECTOR_INDEX_SIZE, VECTOR_COL_SIZE, VECTOR_INDEX_BLOCK_SIZE,
    error::XdbError,
    xdb::{Header, SegmentIndex},
};
///ip database file load to memery
pub fn load_file(path: PathBuf) -> Result<Vec<u8>, XdbError> {
    let mut data = File::open(path)?;
    let mut buf = Vec::new();
    data.read_to_end(&mut buf)?;
    Ok(buf)
}
///A trivial function
pub fn print_ip(header: Header, data: &[u8], ip_version: &str) -> Result<(), XdbError> {

    if ip_version == "ipv4" {
        let count = (header.index_end_address - header.index_base_address + 1)
            / IPV4_SEGMENT_INDEX_BLOCK_SIZE;

        for i in 0..count {
            let index_start_addr =
                header.index_base_address + i as u32 * IPV4_SEGMENT_INDEX_BLOCK_SIZE;
            let index_end_addr = index_start_addr + IPV4_SEGMENT_INDEX_BLOCK_SIZE;
            let segment_block = SegmentIndex::<u32>::try_parse(
                &data[(index_start_addr as usize)..(index_end_addr as usize)],
            )?;
            let ret = String::from_utf8(
                data[segment_block.data_ptr as usize
                    ..(segment_block.data_ptr + segment_block.data_len as u32) as usize]
                    .to_vec(),
            )?;
            let mut res = String::new();
            res.push_str(Ipv4Addr::from(segment_block.ip_start).to_string().as_str());
            res.push_str("|");
            res.push_str(Ipv4Addr::from(segment_block.ip_end).to_string().as_str());
            res.push_str("|");
            res.push_str(ret.as_str());

            println!("{}", res);
        }
    } else if ip_version == "ipv6" {
        let count = (header.index_end_address - header.index_base_address + 1)
            / IPV6_SEGMENT_INDEX_BLOCK_SIZE;
        for i in 0..count {
            let index_start_addr =
                header.index_base_address + i as u32 * IPV6_SEGMENT_INDEX_BLOCK_SIZE;
            let index_end_addr = index_start_addr + IPV6_SEGMENT_INDEX_BLOCK_SIZE;
            let segment_block = SegmentIndex::<u128>::try_parse(
                &data[(index_start_addr as usize)..(index_end_addr as usize)],
            )?;
            let ret = String::from_utf8(
                data[segment_block.data_ptr as usize
                    ..(segment_block.data_ptr + segment_block.data_len as u32) as usize]
                    .to_vec(),
            )?;
            let mut res = String::new();
            res.push_str(Ipv6Addr::from(segment_block.ip_start).to_string().as_str());
            res.push_str("|");
            res.push_str(Ipv6Addr::from(segment_block.ip_end).to_string().as_str());
            res.push_str("|");
            res.push_str(ret.as_str());
        }
    } else {
        return Err(XdbError::InvalidIPVersion("Invalid IP version".into()));
    }
    Ok(())
}

///search ipv4 or ipv6
pub fn search_ip(ip: &str, data: &[u8]) -> Result<String, XdbError> {
    let ip = parse_ip_or_number(ip)?;
    match ip {
        IpAddr::V4(ipv4) => {
            let ip = ipv4.to_bits();
            let il0 = ((ip >> 24) & 0xFF) as usize;
            let il1 = ((ip >> 16) & 0xFF) as usize;

            let idx = il0 as u32 * VECTOR_COL_SIZE as u32 * VECTOR_INDEX_BLOCK_SIZE as u32
                + il1 as u32 * VECTOR_INDEX_BLOCK_SIZE as u32;
            let vec_segment: &[u8] =
                &data[TOTAL_HEADER_SIZE..(TOTAL_HEADER_SIZE + TOTAL_VECTOR_INDEX_SIZE)];
            let slice = &vec_segment[(idx as usize)..(idx as usize + VECTOR_INDEX_BLOCK_SIZE)];
            let start_ptr = u32::from_le_bytes(slice[0..4].try_into()?);
            let end_ptr = u32::from_le_bytes(slice[4..8].try_into()?);

            let mut left: usize = 0;
            let mut right: usize =
                ((end_ptr - start_ptr) / IPV4_SEGMENT_INDEX_BLOCK_SIZE as u32) as usize;

            while left <= right {
                let mid = (left + right) / 2;
                let offset = start_ptr as usize + mid * IPV4_SEGMENT_INDEX_BLOCK_SIZE as usize;
                let ip_value = &data[offset..offset + IPV4_SEGMENT_INDEX_BLOCK_SIZE as usize];
                let start_ip = u32::from_le_bytes(ip_value[0..4].try_into()?);
                if ip < (start_ip as u32) {
                    right = mid - 1;
                } else if ip > (u32::from_le_bytes(ip_value[4..8].try_into()?)) {
                    left = mid + 1;
                } else {
                    let data_length = u16::from_le_bytes(ip_value[8..10].try_into()?);
                    let data_offset = u32::from_le_bytes(ip_value[10..14].try_into()?);
                    let result = String::from_utf8(
                        data[data_offset as usize..(data_offset as usize + data_length as usize)]
                            .to_vec(),
                    )?;
                    return Ok(result);
                }
            }
            //now get vector segment data
        }
        IpAddr::V6(ipv6) => {
            let ip = ipv6.to_bits();
            // For IPv6, use the first two bytes (16 bits) similar to IPv4
            let il0 = ((ip >> 120) & 0xFF) as usize; // First byte (bits 120-127)
            let il1 = ((ip >> 112) & 0xFF) as usize; // Second byte (bits 112-119)
            let vec_segment: &[u8] =
                &data[TOTAL_HEADER_SIZE..(TOTAL_HEADER_SIZE + TOTAL_VECTOR_INDEX_SIZE)];
            // Calculate index (same formula as IPv4)
            let idx = il0 as u32 * VECTOR_COL_SIZE as u32 * VECTOR_INDEX_BLOCK_SIZE as u32
                + il1 as u32 * VECTOR_INDEX_BLOCK_SIZE as u32;
            let slice = &vec_segment[(idx as usize)..(idx as usize + VECTOR_INDEX_BLOCK_SIZE)];
            let start_ptr = u32::from_le_bytes(slice[0..4].try_into()?);
            let end_ptr = u32::from_le_bytes(slice[4..8].try_into()?);

            let mut left: usize = 0;
            let mut right: usize =
                ((end_ptr - start_ptr) / IPV6_SEGMENT_INDEX_BLOCK_SIZE as u32) as usize;

            while left <= right {
                let mid = (left + right) / 2;
                let offset = start_ptr as usize + mid * IPV6_SEGMENT_INDEX_BLOCK_SIZE as usize;
                let ip_value = &data[offset..offset + IPV6_SEGMENT_INDEX_BLOCK_SIZE as usize];

                // Parse u128 start and end IP addresses
                let start_ip = u128::from_le_bytes(ip_value[0..16].try_into()?);
                let end_ip = u128::from_le_bytes(ip_value[16..32].try_into()?);

                if ip < start_ip {
                    right = mid - 1;
                } else if ip > end_ip {
                    left = mid + 1;
                } else {
                    let data_length = u16::from_le_bytes(ip_value[32..34].try_into()?);
                    let data_offset = u32::from_le_bytes(ip_value[34..38].try_into()?);
                    let result = String::from_utf8(
                        data[data_offset as usize..(data_offset as usize + data_length as usize)]
                            .to_vec(),
                    )?;
                    return Ok(result);
                }
            }
        }
    };
    Ok("".into())
}

///Numeric string ip parse to standard ip format
fn parse_ip_or_number(ip: &str) -> Result<IpAddr, XdbError> {
    // 1. normal ip byparse
    if let Ok(ip_addr) = IpAddr::from_str(ip) {
        return Ok(ip_addr);
    }

    // 2. parse to IPv4
    if let Ok(num) = ip.parse::<u32>() {
        return Ok(IpAddr::V4(Ipv4Addr::from(num)));
    }

    // 3. parse to IPv6
    if let Ok(num) = ip.parse::<u128>() {
        return Ok(IpAddr::V6(Ipv6Addr::from(num)));
    }
    Err(XdbError::InvalidIP("InvalidIP or IP error".into()))
}


#[cfg(test)]
mod tests {
    use std::{sync::Arc, thread, time::Instant};

    use anyhow::Result;

    use crate::utils::{load_file, parse_ip_or_number, search_ip};

    #[test]
    fn test_multi_thread_only_load_xdb_once() -> Result<()> {
        /*
        ────────────
            Nextest run ID a4d3234a-55bf-4ce9-a5aa-d6244d36ade8 with nextest profile: default
                Starting 1 test across 2 binaries (1 test skipped)
                Running [ 00:00:00] 0/1: 0 running, 0 passed, 0 skipped
                PASS [   0.276s] xdb-parse utils::tests::test_multi_thread_only_load_xdb_once
            Nextest run ID c1650311-801c-4628-83c5-fe1a46469326 with nextest profile: default
                Starting 1 test across 2 binaries (1 test skipped)
                Running [ 00:00:00] 0/1: 0 running, 0 passed, 0 skipped
                PASS [   0.021s] xdb-parse utils::tests::test_multi_thread_only_load_xdb_once
         */
        let start = Instant::now();
        let path = "./assets/ip2region_v6.xdb";
        let data = load_file(path.into())?;
        let data = Arc::new(data);
        // let path = "./assets/ip2region_v6.xdb";

        let data_clone = Arc::clone(&data);
        let handle = thread::spawn(move || {
            // let result = search_ip("2.2.2.2",&get_cache()).unwrap();
            // let result =search_ip("2001:0db8:85a3:0000:0000:8a2e:0370:7334", &get_cache()).unwrap();
           search_ip("2408:8352:da10:1ad:c283:c9ff:fec6:4046", &data_clone).unwrap();
            // println!("ip search in spawn: {result}");
        });
        // let r = search_ip("2001:0db8:85a3:0000:0000:8a2e:0370:7334", &get_cache()).unwrap();
        search_ip("2408:8352:da10:1ad:c283:c9ff:fec6:4046", &data).unwrap();
        // println!("ip search in main thread: {r}");
        let time = start.elapsed();
        println!("use time:{:?}", time);
        handle.join().unwrap();
        Ok(())
    }

    #[test]
    fn test_use_once_cell_search_ipv4() -> Result<()> {
        /*
        ---- utils::tests::test_use_once_cell_search_ipv4 stdout ----
            use time:5.3644ms-ret:美国|佛罗里达|0|康卡斯特
         */
        let start = Instant::now();
        let path = "./assets/ip2region_v4.xdb";
        let data = load_file(path.into())?;

        let ret = search_ip("73.24.63.66", &data)?;
        let time = start.elapsed();
        println!("use time:{:?}-ret:{}", time, ret);
        Ok(())
    }
    #[test]
    fn test_use_once_cell_search_ipv6() -> Result<()> {
        /*
        ---- utils::tests::test_use_once_cell_search_ipv6 stdout ----
                use time:251.6989ms-ret:美国|亚拉巴马州|杰斐逊|专线用户
         */
        let start = Instant::now();
        let path = "./assets/ip2region_v6.xdb";

        // xdb_init(Some(path.into())).unwrap();
        let data = load_file(path.into())?;
        let ret = search_ip("2001:0db8:85a3:0000:0000:8a2e:0370:7334", &data)?;
        let time = start.elapsed();
        println!("use time:{:?}-ret:{}", time, ret);
        Ok(())
    }

    #[test]
    fn test_parse_ip() -> Result<()> {
        println!("{:?}", parse_ip_or_number("192.168.1.1")?); // V4
        println!("{:?}", parse_ip_or_number("2400:3200::1")?); // V6
        println!("{:?}", parse_ip_or_number("3232235776")?); // V4
        Ok(())
    }
}
