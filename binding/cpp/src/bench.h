#ifndef BENCH_H
#define BENCH_H

#include "search.h"

namespace xdb {

class bench_t {
public:
    bench_t(const string &file_name, int version, int policy);

    void test(const string &file_name);

private:
    void test_one(const ip_t &ip, const string region);
    void test_line(char *buf);
    void test_file(const std::string &file_name);

    search_t search;

    unsigned long long sum_io_count;
    unsigned long long sum_cost_time;
    unsigned long long sum_count;
};

}  // namespace xdb
#endif
