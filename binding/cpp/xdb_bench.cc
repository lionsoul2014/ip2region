
#include "xdb_bench.h"

#include <arpa/inet.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>

#include <iostream>
#include <vector>

static void log_exit(const std::string &msg) {
    std::cout << msg << std::endl;
    exit(-1);
}

static unsigned long long get_time() {
    struct timeval tv1;
    gettimeofday(&tv1, NULL);
    return (unsigned long long)tv1.tv_sec * 1000 * 1000 + tv1.tv_usec;
}

static bool ip2uint(const char *buf, unsigned int &ip) {
    struct in_addr addr;
    if (inet_pton(AF_INET, buf, &addr) == 0)
        return false;
    // 网络字节序为大端存储, 在此转换为小端存储
    ip = (((addr.s_addr >> 0) & 0xFF) << 24) |
         (((addr.s_addr >> 8) & 0xFF) << 16) |
         (((addr.s_addr >> 16) & 0xFF) << 8) |
         (((addr.s_addr >> 24) & 0xFF) << 0);
    return true;
}

static std::string uint2ip(unsigned int ip) {
    char buf[16];
    snprintf(buf,
             sizeof(buf),
             "%d.%d.%d.%d",
             (ip >> 24) & 0xFF,
             (ip >> 16) & 0xFF,
             (ip >> 8) & 0xFF,
             ip & 0xFF);
    return std::string(buf);
}

xdb_bench_t::xdb_bench_t(const std::string &file_name) : xdb_search(file_name) {
}

void xdb_bench_t::init_file() {
    xdb_search.init_file();
}

void xdb_bench_t::init_vector_index() {
    xdb_search.init_vector_index();
}

void xdb_bench_t::init_content() {
    xdb_search.init_content();
}

void xdb_bench_t::bench_test_one(unsigned int ip_uint, const char *region) {
    if (xdb_search.search(uint2ip(ip_uint)) != region)
        log_exit("failed: " + uint2ip(ip_uint));
    sum_io_count += xdb_search.get_io_count();
    sum_cost_time += xdb_search.get_cost_time();
    sum_count++;
}

void xdb_bench_t::bench_test_line(char *buf) {
    size_t buf_len = strlen(buf);
    if (buf_len == 0)
        return;
    buf[buf_len - 1] = '\0';  // 去掉换行符

    char *pos1 = strchr(buf, '|');

    if (pos1 == NULL)
        log_exit("invalid data: " + std::string(buf));
    char *pos2 = strchr(pos1 + 1, '|');
    if (pos2 == NULL)
        log_exit("invalid data: " + std::string(buf));
    *pos1 = '\0';
    *pos2 = '\0';

    unsigned int ip1, ip2;
    if (!ip2uint(buf, ip1) || !ip2uint(pos1 + 1, ip2) || ip1 > ip2) {
        *pos1 = *pos2 = '|';
        log_exit(std::string("invalid data: ") + buf);
    }

    const char *region = pos2 + 1;

    unsigned int              ip_mid = ip1 + (ip2 - ip1) / 2;
    std::vector<unsigned int> ip_vec;
    ip_vec.push_back(ip1);
    ip_vec.push_back(ip1 + (ip_mid - ip1) / 2);
    ip_vec.push_back(ip_mid);
    ip_vec.push_back(ip_mid + (ip2 - ip_mid) / 2);
    ip_vec.push_back(ip2);

    for (auto &d : ip_vec)
        bench_test_one(d, region);
}

void xdb_bench_t::bench_test_file(const std::string &file_name) {
    FILE *f = fopen(file_name.data(), "r");
    if (f == NULL)
        log_exit("can't open " + file_name);
    char buf[1024];
    while (fgets(buf, sizeof(buf), f) != NULL)
        bench_test_line(buf);
}

void xdb_bench_t::bench(const std::string &file_name) {
    sum_io_count  = 0;
    sum_cost_time = 0;
    sum_count     = 0;

    unsigned long long tv1 = get_time();
    bench_test_file(file_name);
    unsigned long long tv2 = get_time();

    double took = (tv2 - tv1) * 1.0 / 1000 / 1000;
    double cost = sum_cost_time * 1.0 / sum_count;

    printf(
        "total: %llu, took: %.2f s, cost: %.2f μs/op, io "
        "count: "
        "%llu\n",
        sum_count,
        took,
        cost,
        sum_io_count);
}
