
#include "../src/make.h"
#include "../src/search.h"

static_assert(sizeof(xdb::make_t) < 1024,
              "make_t must keep its vector index off the stack");

std::string join_path(const std::string& directory, const std::string& filename) {
    return directory + "/" + filename;
}

void make_xdb(const std::string& prompt,
              const std::string& source_path,
              const std::string& xdb_path,
              int                version) {
    std::cout << prompt;
    xdb::make_t(source_path, xdb_path, version);
}

void expect_region(const std::string& filename_xdb,
                   int version,
                   const std::string& ip,
                   const std::string& region) {
    xdb::search_t search(filename_xdb, version, xdb::policy_content);
    if (search.search(ip) != region)
        xdb::log_exit("generated xdb query failed for " + ip);
}

int main(int argc, char* argv[]) {
    if (argc != 1 && argc != 3)
        xdb::log_exit("usage: make [source-directory output-directory]");

    const bool        fixture_mode = argc == 3;
    const std::string source_dir   = fixture_mode ? argv[1] : "../../data";
    const std::string output_dir   = fixture_mode ? argv[2] : ".";
    const std::string ipv4_xdb     = join_path(output_dir, "ip2region_v4.xdb");
    const std::string ipv6_xdb     = join_path(output_dir, "ip2region_v6.xdb");

    make_xdb("生成 ipv4 的 xdb 文件, ",
             join_path(source_dir, "ipv4_source.txt"),
             ipv4_xdb,
             xdb::ipv4);

    make_xdb("生成 ipv6 的 xdb 文件, ",
             join_path(source_dir, "ipv6_source.txt"),
             ipv6_xdb,
             xdb::ipv6);

    if (fixture_mode) {
        const std::string reserved_region = "Reserved|Reserved|Reserved|0|ZZ";
        const std::string example_region  = "Example|Documentation|Fixture|Test ISP|EX";

        expect_region(ipv4_xdb, xdb::ipv4, "0.0.0.0", reserved_region);
        expect_region(ipv4_xdb, xdb::ipv4, "1.2.3.0", example_region);
        expect_region(ipv4_xdb, xdb::ipv4, "1.2.3.255", example_region);
        expect_region(ipv4_xdb, xdb::ipv4, "1.2.4.0", reserved_region);

        expect_region(ipv6_xdb, xdb::ipv6, "::", reserved_region);
        expect_region(ipv6_xdb, xdb::ipv6, "2001:db8::", example_region);
        expect_region(ipv6_xdb, xdb::ipv6, "2001:db8::ffff", example_region);
        expect_region(ipv6_xdb, xdb::ipv6, "2001:db8::1:0", reserved_region);
    }

    return 0;
}
