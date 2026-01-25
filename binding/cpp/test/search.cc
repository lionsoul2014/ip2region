
#include "../src/search.h"

std::map<int, std::string> prompt;

void test(xdb::search_t& s, const std::string& ip, const std::string& region) {
    if (s.search(ip) != region)
        xdb::log_exit("测试失败, ip " + ip + ", region " + region);
}

void test_ipv4(int policy) {
    std::cout << "测试 IPv4 " << prompt[policy];

    xdb::search_t s("../../data/ip2region_v4.xdb", xdb::ipv4, policy);
    test(s, "0.0.0.0", "Reserved|Reserved|Reserved|0|0");
    test(s, "1.2.3.4", "Australia|Queensland|Brisbane|0|AU");

    std::cout << " 成功" << std::endl;
}

void test_ipv6(int policy) {
    std::cout << "测试 IPv6 " << prompt[policy];

    xdb::search_t s("../../data/ip2region_v6.xdb", xdb::ipv6, policy);
    test(s, "::1", "");
    test(s, "2001:200:124::", "Japan|Tokyo|Asagaya-minami|WIDE Project|JP");
    test(s, "2001:200:124::", "Japan|Tokyo|Asagaya-minami|WIDE Project|JP");
    test(s, "240e:3b7:3273:51d0:cd38:8ae1:e3c0:b708", "中国|广东省|深圳市|电信|CN");

    std::cout << " 成功" << std::endl;
}

int main() {
    prompt[xdb::policy_file]    = "  不缓存:";
    prompt[xdb::policy_vector]  = "部分缓存:";
    prompt[xdb::policy_content] = "全部缓存:";

    test_ipv4(xdb::policy_file);
    test_ipv4(xdb::policy_vector);
    test_ipv4(xdb::policy_content);

    test_ipv6(xdb::policy_file);
    test_ipv6(xdb::policy_vector);
    test_ipv6(xdb::policy_content);
    return 0;
}
