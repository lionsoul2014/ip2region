use criterion::{Criterion, criterion_group, criterion_main};
use rand;

use ip2region::{CachePolicy, Searcher};

const XDB_FILEPATH: &'static str = "../../../data/ip2region_v4.xdb";

macro_rules! bench_search {
    ($name:ident, $cache_policy:expr) => {
        fn $name(c: &mut Criterion) {
            c.bench_function(stringify!($name), |b| {
                let searcher = Searcher::new(XDB_FILEPATH.to_owned(), $cache_policy);
                b.iter(|| {
                    searcher.search(rand::random::<u32>()).unwrap();
                })
            });
        }
    };
}

bench_search!(no_memory_bench, CachePolicy::NoCache);
bench_search!(vector_index_cache_bench, CachePolicy::VectorIndex);
bench_search!(full_memory_cache_bench, CachePolicy::FullMemory);

criterion_group!(
    benches,
    no_memory_bench,
    vector_index_cache_bench,
    full_memory_cache_bench,
);
criterion_main!(benches);
