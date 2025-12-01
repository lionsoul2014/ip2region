
#include "../src/make.h"

void test(const std::string& prompt,
          const std::string& filename_xdb,
          const std::string& filename_src,
          int                version

) {
    std::cout << prompt;
    xdb::make_t(filename_xdb, filename_src, version);
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

    return 0;
}
