
#include "../src/bench.h"

std::map<int, std::string> prompt;

void test_ipv4(int policy) {
    std::cout << "testing IPv4, " << prompt[policy];
    xdb::bench_t("../../data/ip2region_v4.xdb", xdb::ipv4, policy)
        .test("../../data/ipv4_source.txt");
}

void test_ipv6(int policy) {
    std::cout << "testing IPv6, " << prompt[policy];
    xdb::bench_t("../../data/ip2region_v6.xdb", xdb::ipv6, policy)
        .test("../../data/ipv6_source.txt");
}

int main() {
    prompt[xdb::policy_file]    = "    No cache, ";
    prompt[xdb::policy_vector]  = "Vector index, ";
    prompt[xdb::policy_content] = "  Full cache, ";

    test_ipv4(xdb::policy_file);
    test_ipv4(xdb::policy_vector);
    test_ipv4(xdb::policy_content);

    test_ipv6(xdb::policy_file);
    test_ipv6(xdb::policy_vector);
    test_ipv6(xdb::policy_content);
    return 0;
}
