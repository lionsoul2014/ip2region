use std::fs::File;
use std::io::{BufRead, BufReader};
use std::net::{IpAddr, Ipv4Addr, Ipv6Addr};
use std::str::FromStr;
use std::sync::Arc;

use itertools::Itertools;
use tracing::{debug, info, trace};

use crate::IpVersion;
use crate::error::{MakerError, Result};
use crate::header::IPAddrExt;

pub trait IpPlusEq {
    fn ip_plus_eq(&self, other: &Self) -> bool;
}

impl IpPlusEq for IpAddr {
    fn ip_plus_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (IpAddr::V4(start), IpAddr::V4(end)) => Ipv4Addr::from(u32::from(*start) + 1).eq(end),
            (IpAddr::V6(start), IpAddr::V6(end)) => Ipv6Addr::from(u128::from(*start) + 1).eq(end),
            _ => false,
        }
    }
}

#[derive(Debug)]
pub struct Segment {
    pub start_ip: IpAddr,
    pub end_ip: IpAddr,
    pub region: Arc<String>,
}

fn region_filter(region: &str, filter_fields: &[usize]) -> Result<String> {
    if filter_fields.is_empty() {
        return Ok(region.to_owned());
    }
    let fields = region.split('|').collect::<Vec<_>>();
    let filtered = filter_fields
        .iter()
        .map(|idx| {
            fields
                .get(*idx)
                .ok_or(MakerError::RegionFilterFieldsTooBig {
                    limit: fields.len(),
                    actual: *idx,
                })
        })
        .collect::<Result<Vec<_>>>()?;
    Ok(filtered.into_iter().join("|"))
}

impl Segment {
    pub fn from_file(
        src_filepath: &str,
        ip_version: IpVersion,
        filter_fields: &[usize],
    ) -> Result<Vec<Segment>> {
        info!("Read src file");

        let mut last = None;
        let mut segments = vec![];

        let reader = BufReader::new(File::open(src_filepath)?);
        for original_line in reader.lines() {
            let original_line = original_line?;
            let line = original_line.trim();
            if line.is_empty() || line.starts_with('#') {
                continue;
            }

            trace!(?line, "Processing line");
            let v = line.splitn(3, '|').collect::<Vec<_>>();
            if v.len() != 3 {
                return Err(MakerError::ParseIPRegion(line.to_owned()));
            }
            let (start_ip, end_ip, region) = (v[0], v[1], v[2]);

            let (start_ip, end_ip) = if ip_version.eq(&IpVersion::V4) {
                (
                    IpAddr::V4(Ipv4Addr::from_str(start_ip)?),
                    IpAddr::V4(Ipv4Addr::from_str(end_ip)?),
                )
            } else {
                (
                    IpAddr::V6(Ipv6Addr::from_str(start_ip)?),
                    IpAddr::V6(Ipv6Addr::from_str(end_ip)?),
                )
            };
            if start_ip.gt(&end_ip) {
                return Err(MakerError::ParseIPRegion(line.to_owned()));
            }

            let segment = Segment {
                start_ip,
                end_ip,
                region: Arc::new(region_filter(region, filter_fields)?),
            };
            match last.take() {
                None => {
                    last = Some(segment);
                }
                Some(mut l)
                    if segment.region.eq(&l.region) && l.end_ip.ip_plus_eq(&segment.start_ip) =>
                {
                    l.end_ip = segment.end_ip;
                    last = Some(l);
                }
                Some(seg) => {
                    segments.push(seg);
                    last = Some(segment);
                }
            }
        }

        if let Some(last) = last {
            segments.push(last);
        }

        info!(length = segments.len(), "load segments");
        Ok(segments)
    }

    pub fn split(self) -> Result<Vec<Segment>> {
        let start_bytes = self.start_ip.ipaddr_bytes();
        let end_bytes = self.end_ip.ipaddr_bytes();

        let start_byte = u16::from_be_bytes([start_bytes[0], start_bytes[1]]);
        let end_byte = u16::from_be_bytes([end_bytes[0], end_bytes[1]]);

        let segments = (start_byte..=end_byte)
            .map(|index| {
                let sip = if index == start_byte {
                    self.start_ip
                } else if self.start_ip.is_ipv4() {
                    IpAddr::from(Ipv4Addr::from((index as u32) << 16))
                } else {
                    IpAddr::from(Ipv6Addr::from((index as u128) << 112))
                };

                let eip = if index == end_byte {
                    self.end_ip
                } else if self.start_ip.is_ipv4() {
                    let mask = (1 << 16) - 1;
                    let v = (index as u32) << 16;
                    IpAddr::from(Ipv4Addr::from(v | mask))
                } else {
                    let mask = (1 << 112) - 1;
                    let v = (index as u128) << 112;
                    IpAddr::from(Ipv6Addr::from(v | mask))
                };

                trace!(?index, ?sip, ?eip, ?self.region, "in split segment");
                Segment {
                    start_ip: sip,
                    end_ip: eip,
                    region: self.region.clone(),
                }
            })
            .collect_vec();
        debug!(?self, length = segments.len(), "Try to index segment");

        Ok(segments)
    }
}
