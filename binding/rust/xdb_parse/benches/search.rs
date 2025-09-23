use criterion::{Criterion,criterion_group, criterion_main};
use std::hint::black_box;
use rand;

use xdb_parse::utils::{load_file, search_ip};

fn search_by_ipv4_bench(c: &mut Criterion) {
    c.bench_function("search_by_ipv4_bench", |b| {
        let path = "./assets/ip2region_v4.xdb";
        let data = load_file(path.into()).unwrap();
        b.iter(|| {
            let ip = rand::random::<u32>().to_string();
            search_ip(&ip, &data).unwrap();
        })
    });
}
fn search_by_ipv6_bench(c: &mut Criterion) {
    c.bench_function("search_by_ipv6_bench", |b| {
        let path = "./assets/ip2region_v6.xdb";
        let data = load_file(path.into()).unwrap();
        b.iter(|| {
            let ip = rand::random::<u128>().to_string();
            search_ip(&ip, &data).unwrap();
        })
    });
}

fn get_block_by_size_bench(c: &mut Criterion) {
    c.bench_function("get_block_by_size_bench", |b| {
        let path = "./assets/ip2region_v4.xdb";
        let data = load_file(path.into()).unwrap();
        b.iter(|| {
            let start = rand::random::<u16>() as usize;
            let end = start + 4;
            black_box(u32::from_le_bytes(
                data[start..end]
                    .try_into()
                    .unwrap(),
            ));
        })
    });
}

criterion_group!(
    benches,
    search_by_ipv4_bench,
    search_by_ipv6_bench,
    get_block_by_size_bench,
);
criterion_main!(benches);
