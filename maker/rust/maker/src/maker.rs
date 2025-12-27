use std::collections::HashMap;
use std::fs::File;
use std::io::{Seek, SeekFrom, Write};
use std::sync::Arc;

use bytes::{BufMut, BytesMut};
use itertools::Itertools;
use tracing::{info, trace};

use crate::error::{MakerError, Result};
use crate::header::{IPAddrExt, IndexPolicy, IpVersion, VECTOR_INDEX_ROWS};
use crate::segment::Segment;
use crate::{HEADER_INFO_LENGTH, Header, REGION_START, VECTOR_INDEX_COLS, VECTOR_INDEX_SIZE};

pub struct Maker {
    ip_version: IpVersion,
    dst_file: File,
    region_pool: HashMap<Arc<String>, u32>,
    vector_index: [[[u8; VECTOR_INDEX_SIZE]; VECTOR_INDEX_ROWS]; VECTOR_INDEX_COLS],
    segments: Vec<Segment>,
    header: Header,
}

impl Maker {
    pub fn new(
        ip_version: IpVersion,
        index_policy: IndexPolicy,
        src_filepath: &str,
        end_filepath: &str,
        filter_fields: Vec<usize>,
    ) -> Result<Self> {
        let header = Header::new(index_policy, ip_version);

        let segments = Segment::from_file(src_filepath, ip_version, &filter_fields)?;
        if segments.is_empty() {
            return Err(MakerError::EmptySegments);
        }

        let mut region_pool = HashMap::with_capacity(segments.len());
        let mut dst_file = File::create(end_filepath)?;
        let mut region_buf = BytesMut::new();
        let mut current = u32::try_from(REGION_START)?;
        for region in segments.iter().map(|s| s.region.clone()).unique() {
            region_buf.extend_from_slice(region.as_bytes());
            let region_len = region.len() as u32;
            region_pool.insert(region, current);
            current += region_len;
        }
        dst_file.seek(SeekFrom::Start(REGION_START))?;
        dst_file.write_all(region_buf.as_ref())?;
        info!("Load region pool successfully");

        Ok(Self {
            ip_version,
            dst_file,
            region_pool,
            vector_index: [[[0; VECTOR_INDEX_SIZE]; VECTOR_INDEX_ROWS]; VECTOR_INDEX_COLS],
            segments,
            header,
        })
    }

    fn set_vector_index(&mut self, ip: &[u8], ptr: u32) -> Result<()> {
        let (l0, l1) = (ip[0] as usize, ip[1] as usize);
        let block = &mut self.vector_index[l0][l1];

        if block[0..4].eq(&[0; 4]) {
            block[0..4].copy_from_slice(&ptr.to_le_bytes());
        }
        let end_value = ptr + self.ip_version.segment_index_size() as u32;
        block[4..].copy_from_slice(&end_value.to_le_bytes());
        Ok(())
    }

    pub fn start(&mut self) -> Result<()> {
        let start_index_ptr = u32::try_from(self.dst_file.stream_position()?)?;
        let mut segment_count = 0;
        let mut buf =
            BytesMut::with_capacity(self.ip_version.segment_index_size() * self.segments.len());

        for segment in std::mem::take(&mut self.segments) {
            let region_ptr = *self
                .region_pool
                .get(&segment.region)
                .ok_or(MakerError::RegionNotFound)?;
            let region_len = u16::try_from(segment.region.len())?;

            trace!(?segment, "before segment split");
            for seg in segment.split()? {
                self.set_vector_index(
                    &seg.start_ip.ipaddr_bytes(),
                    start_index_ptr + buf.len() as u32,
                )?;

                buf.put_slice(&seg.start_ip.encode_ipaddr_bytes());
                buf.put_slice(&seg.end_ip.encode_ipaddr_bytes());
                buf.put_u16_le(region_len);
                buf.put_u32_le(region_ptr);
                segment_count += 1;
            }
        }

        info!(
            region_pool_len = self.region_pool.len(),
            segment_count, "Write segment index buffer"
        );
        self.dst_file
            .seek(SeekFrom::Start(start_index_ptr as u64))?;
        self.dst_file.write_all(buf.as_ref())?;

        info!("Write header buffer");
        let header_buf = self.header.encode_bytes(
            start_index_ptr,
            start_index_ptr + (buf.len() as u32) - (self.ip_version.segment_index_size() as u32),
        );
        self.dst_file.seek(SeekFrom::Start(0))?;
        self.dst_file.write_all(header_buf.as_ref())?;

        info!("Write vector index buffer");
        self.dst_file
            .seek(SeekFrom::Start(HEADER_INFO_LENGTH as u64))?;
        self.dst_file
            .write_all(self.vector_index.as_flattened().as_flattened())?;
        Ok(())
    }
}
