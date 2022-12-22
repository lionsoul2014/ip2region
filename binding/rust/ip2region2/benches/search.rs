use criterion::{criterion_group, criterion_main, Criterion};
use rand;

use ip2region2::{buffer_value, get_block_by_size, get_start_end_ptr, global_searcher, search_by_ip};

fn ip_search_bench(c: &mut Criterion) {
    c.bench_function("ip_search_bench", |b| {
        b.iter(|| {
            search_by_ip(rand::random::<u32>()).unwrap();
        })
    });
}

fn buffer_value_bench(c: &mut Criterion) {
    c.bench_function("buffer_value", |b| {
        b.iter(|| {
            let offset = rand::random::<u16>();
            let length = rand::random::<u8>();
            buffer_value(offset as usize, length as usize);
        });
    });
}

fn get_block_by_size_bench(c: &mut Criterion) {
    c.bench_function("get_block_by_size", |b| {
        b.iter(|| {
            get_block_by_size(
                &global_searcher().buffer(),
                rand::random::<u16>() as usize,
                4,
            );
        })
    });
}

fn get_start_end_ptr_bench(c: &mut Criterion) {
    c.bench_function("get_start_end_ptr", |b| {
        b.iter(|| {
            get_start_end_ptr(rand::random::<u32>());
        })
    });
}

criterion_group!(
    benches,
    ip_search_bench,
    buffer_value_bench,
    get_block_by_size_bench,
    get_start_end_ptr_bench
);
criterion_main!(benches);
