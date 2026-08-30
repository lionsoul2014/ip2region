
#include "../src/make.h"
#include "../src/search.h"

static_assert(sizeof(xdb::make_t) < 1024,
              "make_t must keep its vector index off the stack");

void test(const std::string& prompt,
          const std::string& filename_xdb,
          const std::string& filename_src,
          int                version

) {
    std::cout << prompt;
    xdb::make_t(filename_xdb, filename_src, version);
}

void expect_region(const std::string& filename_xdb,
                   int version,
                   const std::string& ip,
                   const std::string& region) {
    xdb::search_t search(filename_xdb, version, xdb::policy_content);
    if (search.search(ip) != region)
        xdb::log_exit("generated xdb query failed for " + ip);
}

int main() {
    test("生成 ipv4 的 xdb 文件, ",
         "../../data/ipv4_source.txt",
         "./ip2region_v4.xdb",
         xdb::ipv4);

    test("生成 ipv6 的 xdb 文件, ",
         "../../data/ipv6_source.txt",
         "./ip2region_v6.xdb",
         xdb::ipv6);

    expect_region("./ip2region_v4.xdb",
                  xdb::ipv4,
                  "1.2.3.4",
                  "Australia|Queensland|Brisbane|0|AU");
    expect_region("./ip2region_v6.xdb",
                  xdb::ipv6,
                  "2001:200:124::",
                  "Japan|Tokyo|Asagaya-minami|WIDE Project|JP");

    return 0;
}
