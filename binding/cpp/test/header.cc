
#include "../src/header.h"

void test(const std::string& prompt, const std::string& file_name) {
    std::cout << prompt << std::endl;

    xdb::header_t head(fopen(file_name.data(), "r"));

    std::cout << "版本号: " << head.version() << std::endl;
    std::cout << "缓存策略: " << head.index_policy() << std::endl;

    time_t     rawtime = head.create_at();
    struct tm* info    = localtime(&rawtime);
    char       buf[80];
    strftime(buf, 80, "%Y-%m-%d %H:%M:%S", info);

    std::cout << "文件生成时间: " << buf << std::endl;
    std::cout << "索引起始地址: " << head.index_start() << std::endl;
    std::cout << "索引结束地址: " << head.index_end() << std::endl;
    std::cout << "IP版本: " << head.ip_version() << std::endl;
    std::cout << "指针字节数: " << head.ptr() << std::endl;

    std::cout << std::endl;
}

int main() {
    test("测试 IPv4", "../../data/ip2region_v4.xdb");
    test("测试 IPv6", "../../data/ip2region_v6.xdb");
    return 0;
}
