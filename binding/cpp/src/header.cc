
#include "header.h"

namespace xdb {

header_t::header_t(FILE* db) {
    read_bin(0, header, sizeof(header), db);
}

header_t::~header_t() {
}

int header_t::version() {
    return to_int(header, 2);  // 版本号(2)
}

int header_t::index_policy() {
    return to_int(header + 2, 2);  // 缓存策略(2)
}

int header_t::create_at() {
    return to_int(header + 4, 4);  // 文件生成时间(4)
}

int header_t::index_start() {
    return to_int(header + 8, 4);  // 索引起始地址(4)
}

int header_t::index_end() {
    return to_int(header + 12, 4);  // 索引结束地址(4)
}

int header_t::ip_version() {
    return to_int(header + 16, 2);  // IP 版本(2)
}

int header_t::ptr() {
    return to_int(header + 18, 2);  // 指针字节数(2)
}

}  // namespace xdb
