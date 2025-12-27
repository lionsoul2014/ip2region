
#include "bench.h"

namespace xdb {

bench_t::bench_t(const std::string &file_name, int version, int policy)
    : search(file_name, version, policy) {
}

void bench_t::test_one(const ip_t &ip, const string region) {
    if (search.search(ip.to_string()) != region)
        xdb::log_exit("failed: " + ip.to_string() + " " + region);
    sum_io_count += search.get_io_count();
    sum_cost_time += search.get_cost_time();
    sum_count++;
}

void bench_t::test_line(char *buf) {
    size_t buf_len = strlen(buf);
    if (buf_len == 0)
        return;
    buf[buf_len - 1] = '\0';  // 去掉换行符

    node_t node(buf);

    // 只测五个
    for (int i = 0; i < 5 && node.ip1 < node.ip2; ++i) {
        test_one(node.ip1, node.region);
        node.ip1 = node.ip1 + 1;
    }
    test_one(node.ip2, node.region);
}

void bench_t::test_file(const std::string &file_name) {
    FILE *f = fopen(file_name.data(), "r");
    if (f == NULL)
        xdb::log_exit("can't open " + file_name);
    char buf[1024];
    while (fgets(buf, sizeof(buf), f) != NULL)
        test_line(buf);
}

void bench_t::test(const string &file_name) {
    sum_io_count  = 0;
    sum_cost_time = 0;
    sum_count     = 0;

    unsigned long long tv1 = xdb::get_time();
    test_file(file_name);
    unsigned long long tv2 = xdb::get_time();

    double took = (tv2 - tv1) * 1.0 / 1000 / 1000;
    double cost = sum_cost_time * 1.0 / sum_count;

    printf(
        "total: %llu, took: %8.2fs, cost: %6.2fμs/op, io "
        "count: "
        "%llu\n",
        sum_count,
        took,
        cost,
        sum_io_count);
}

}  // namespace xdb
