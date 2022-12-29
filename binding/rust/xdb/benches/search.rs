use criterion::{black_box, criterion_group, criterion_main, Criterion};
use rand;

use xdb::searcher::{
    get_block_by_size, get_full_cache, get_vector_index_cache, search_by_ip, searcher_init,
};

fn search_by_ip_bench(c: &mut Criterion) {
    c.bench_function("search_by_ip_bench", |b| {
        searcher_init(None);
        b.iter(|| {
            search_by_ip(rand::random::<u32>()).unwrap();
        })
    });
}

fn get_block_by_size_bench(c: &mut Criterion) {
    c.bench_function("get_block_by_size_bench", |b| {
        b.iter(|| {
            black_box(get_block_by_size(
                get_full_cache(),
                rand::random::<u16>() as usize,
                4,
            ));
        })
    });
}

fn get_full_cache_bench(c: &mut Criterion) {
    c.bench_function("get_full_cache_bench", |b| {
        b.iter(|| {
            black_box(get_full_cache());
        })
    });
}

fn get_vec_index_cache_bench(c: &mut Criterion) {
    c.bench_function("get_vec_index_cache_bench", |b| {
        b.iter(|| {
            black_box(get_vector_index_cache());
        })
    });
}

criterion_group!(
    benches,
    search_by_ip_bench,
    get_block_by_size_bench,
    get_full_cache_bench,
    get_vec_index_cache_bench,
);
criterion_main!(benches);
