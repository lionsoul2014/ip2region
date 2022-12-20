use criterion::{Criterion, criterion_group, criterion_main};
use rand;

use search::search_by_ip;

fn ip_search_benchmark(c: &mut Criterion) {
    c.bench_function("ip_search_bench", |b| b.iter(|| {
        let ip = rand::random::<u32>();
        search_by_ip(ip).unwrap();
    }));
}

criterion_group!(benches, ip_search_benchmark);
criterion_main!(benches);
