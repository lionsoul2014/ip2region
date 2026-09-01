
#include "header.h"

namespace xdb {

header_t::header_t(FILE* db) {
    read_bin(0, header, sizeof(header), db);
}

header_t::~header_t() {
}

int header_t::version() {
    return to_int(header, 2);  // xdb structure version
}

int header_t::index_policy() {
    return to_int(header + 2, 2);  // cache policy
}

int header_t::create_at() {
    return to_int(header + 4, 4);  // created unix timestamp
}

int header_t::index_start() {
    return to_int(header + 8, 4);  // index start ptr
}

int header_t::index_end() {
    return to_int(header + 12, 4);  // index end ptr
}

int header_t::ip_version() {
    return to_int(header + 16, 2);  // IP version
}

int header_t::ptr() {
    return to_int(header + 18, 2);  // ptr bytes
}

}  // namespace xdb
