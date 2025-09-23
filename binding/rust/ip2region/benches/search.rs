use criterion::{Criterion, criterion_group, criterion_main};
use rand;

use ip2region::{CachePolicy, Searcher};

macro_rules! bench_search {
    ($name:ident, $xdb:expr, $cache_policy:expr, $ty:ty) => {
        fn $name(c: &mut Criterion) {
            c.bench_function(stringify!($name), |b| {
                let searcher = Searcher::new($xdb.to_owned(), $cache_policy).unwrap();
                b.iter(|| {
                    searcher.search(rand::random::<$ty>()).unwrap();
                })
            });
        }
    };
}

const IPV4_XDB: &'static str = "../../../data/ip2region_v4.xdb";
const IPV6_XDB: &'static str = "../../../data/ip2region_v6.xdb";

bench_search!(ipv4_no_memory_bench, IPV4_XDB, CachePolicy::NoCache, u32);
bench_search!(
    ipv4_vector_index_cache_bench,
    IPV4_XDB,
    CachePolicy::VectorIndex,
    u32
);
bench_search!(
    ipv4_full_memory_cache_bench,
    IPV4_XDB,
    CachePolicy::FullMemory,
    u32
);
bench_search!(ipv6_no_memory_bench, IPV6_XDB, CachePolicy::NoCache, u128);
bench_search!(
    ipv6_vector_index_cache_bench,
    IPV6_XDB,
    CachePolicy::VectorIndex,
    u128
);
bench_search!(
    ipv6_full_memory_cache_bench,
    IPV6_XDB,
    CachePolicy::FullMemory,
    u128
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
