
#include "base.h"

#include <chrono>

namespace xdb {

int ip_version;  // ip version
int ip_size;     // ip bytes
int content_size;

void init_xdb(int version) {
    ip_version   = version;
    ip_size      = version == ipv4 ? 4 : 16;
    content_size = ip_size * 2 + 2 + 4;
}

void log_exit(const string &msg) {
    std::cout << msg << std::endl;
    exit(-1);
}

FILE *open_file(const string &path, const char *mode) {
#ifdef _MSC_VER
    FILE *file = NULL;
    return fopen_s(&file, path.c_str(), mode) == 0 ? file : NULL;
#else
    return fopen(path.c_str(), mode);
#endif
}

void read_bin(int index, char *buf, size_t len, FILE *db) {
    fseek(db, index, SEEK_SET);
    if (fread(buf, 1, len, db) != len)
        log_exit(__func__);
}

unsigned to_uint(const char *buf) {
    return static_cast<unsigned char>(buf[0]) |
           (static_cast<unsigned>(static_cast<unsigned char>(buf[1])) << 8) |
           (static_cast<unsigned>(static_cast<unsigned char>(buf[2])) << 16) |
           (static_cast<unsigned>(static_cast<unsigned char>(buf[3])) << 24);
}

unsigned to_ushort(const char *buf) {
    return static_cast<unsigned char>(buf[0]) |
           (static_cast<unsigned>(static_cast<unsigned char>(buf[1])) << 8);
}

unsigned to_int(const char *buf, int n) {
    return n == 2 ? to_ushort(buf) : to_uint(buf);
}

void write_uint(unsigned data, char buf[]) {
    buf[0] = (data >> 0) & 0xFF;
    buf[1] = (data >> 8) & 0xFF;
    buf[2] = (data >> 16) & 0xFF;
    buf[3] = (data >> 24) & 0xFF;
}

void write_uint(unsigned data, FILE *dst) {
    char buf[4];
    write_uint(data, buf);
    fwrite(buf, 1, sizeof(buf), dst);
}

void write_ushort(unsigned data, char buf[]) {
    buf[0] = (data >> 0) & 0xFF;
    buf[1] = (data >> 8) & 0xFF;
}

void write_ushort(unsigned data, FILE *dst) {
    char buf[2];
    write_ushort(data, buf);
    fwrite(buf, 1, sizeof(buf), dst);
}

void write_string(const char *buf, unsigned len, FILE *dst) {
    fwrite(buf, 1, len, dst);
}

unsigned long long get_time() {
    using std::chrono::duration_cast;
    using std::chrono::microseconds;
    using std::chrono::steady_clock;

    return duration_cast<microseconds>(steady_clock::now().time_since_epoch()).count();
}

}  // namespace xdb
