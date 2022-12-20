// use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};
// use rand;
//
// fn ip_search_benchmark(c: &mut Criterion) {
//     let searcher = search::Searcher::new("../../../data/ip2region.xdb").unwrap();
//     c.bench_with_input(BenchmarkId::new("searcher", searcher),
//     &searcher, |b, &seacher| {
//             b.iter(|| seacher.search_by_ip(black_box(rand::random::<u32>())));
//         });
// }
//
// criterion_group!(benches, ip_search_benchmark);
// criterion_main!(benches);
