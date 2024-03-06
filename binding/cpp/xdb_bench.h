#ifndef XDB_BENCH_H
#define XDB_BENCH_H

#include "xdb_search.h"

class xdb_bench_t {
  public:
    xdb_bench_t(const std::string &file_name);

    void init_file();
    void init_vector_index();
    void init_content();

    void bench(const std::string &file_name);

  private:
    void bench_test_one(unsigned int ip_uint, const char *region);
    void bench_test_line(char *buf);
    void bench_test_file(const std::string &file_name);

    xdb_search_t xdb_search;

    unsigned long long sum_io_count;
    unsigned long long sum_cost_time;
    unsigned long long sum_count;
};

#endif
