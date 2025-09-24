use std::net::Ipv6Addr;
use std::ops::Range;
use std::str::FromStr;

use criterion::{Criterion, criterion_group, criterion_main};

use ip2region::{CachePolicy, Searcher};

macro_rules! bench_search {
    ($name:ident, $xdb:expr, $cache_policy:expr, $range:ident) => {
        fn $name(c: &mut Criterion) {
            let searcher = Searcher::new($xdb.to_owned(), $cache_policy).unwrap();
            let range = $range();

            c.bench_function(stringify!($name), |b| {
                b.iter(|| {
                    searcher.search(rand::random_range(range.clone())).unwrap();
                })
            });
        }
    };
}

fn ipv4_range() -> Range<u32> {
    0..((1_u64 << 32) - 1) as u32
}

/// The range of IPv6 is too large, and the value range needs to be limited to
/// make the benchmark test results closer to the production environment
fn ipv6_range() -> Range<u128> {
    let start = u128::from(Ipv6Addr::from_str("2000::").unwrap());
    let end = u128::from(Ipv6Addr::from_str("2004::").unwrap());
    start..end
}

const IPV4_XDB: &str = "../../../data/ip2region_v4.xdb";
const IPV6_XDB: &str = "../../../data/ip2region_v6.xdb";

bench_search!(
    ipv4_no_memory_bench,
    IPV4_XDB,
    CachePolicy::NoCache,
    ipv4_range
);
bench_search!(
    ipv4_vector_index_cache_bench,
    IPV4_XDB,
    CachePolicy::VectorIndex,
    ipv4_range
);
bench_search!(
    ipv4_full_memory_cache_bench,
    IPV4_XDB,
    CachePolicy::FullMemory,
    ipv4_range
);
bench_search!(
    ipv6_no_memory_bench,
    IPV6_XDB,
    CachePolicy::NoCache,
    ipv6_range
);
bench_search!(
    ipv6_vector_index_cache_bench,
    IPV6_XDB,
    CachePolicy::VectorIndex,
    ipv6_range
);
bench_search!(
    ipv6_full_memory_cache_bench,
    IPV6_XDB,
    CachePolicy::FullMemory,
    ipv6_range
);

criterion_group!(
    benches,
    ipv4_no_memory_bench,
    ipv4_vector_index_cache_bench,
    ipv4_full_memory_cache_bench,
    ipv6_no_memory_bench,
    ipv6_vector_index_cache_bench,
    ipv6_full_memory_cache_bench
);
criterion_main!(benches);
