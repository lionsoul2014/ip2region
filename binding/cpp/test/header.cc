
#include "../src/header.h"

void test(const std::string& prompt, const std::string& file_name) {
    std::cout << prompt << std::endl;

    FILE* db = xdb::open_file(file_name, "rb");
    if (db == NULL)
        xdb::log_exit("can't open " + file_name);
    xdb::header_t head(db);
    fclose(db);

    std::cout << "Version: " << head.version() << std::endl;
    std::cout << "CachePolicy: " << head.index_policy() << std::endl;

    time_t     rawtime = head.create_at();
    struct tm* info    = localtime(&rawtime);
    char       buf[80];
    strftime(buf, 80, "%Y-%m-%d %H:%M:%S", info);

    std::cout << "Created: " << buf << std::endl;
    std::cout << "Index start ptr: " << head.index_start() << std::endl;
    std::cout << "Index end ptr: " << head.index_end() << std::endl;
    std::cout << "IP verison: " << head.ip_version() << std::endl;
    std::cout << "Ptr bytes: " << head.ptr() << std::endl;

    std::cout << std::endl;
}

int main() {
    test("Testing IPv4", "../../data/ip2region_v4.xdb");
    test("Testing IPv6", "../../data/ip2region_v6.xdb");
    return 0;
}
