
#include "base.h"

namespace xdb {

int ip_version;  // ip 版本
int ip_size;     // ip 占的字节数
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

void read_bin(int index, char *buf, size_t len, FILE *db) {
    fseek(db, index, SEEK_SET);
    if (fread(buf, 1, len, db) != len)
        log_exit(__func__);
}

unsigned to_uint(const char *buf) {
    return ((buf[0]) & 0x000000FF) | ((buf[1] << 8) & 0x0000FF00) |
           ((buf[2] << 16) & 0x00FF0000) | ((buf[3] << 24) & 0xFF000000);
}

unsigned to_ushort(const char *buf) {
    return ((buf[0]) & 0x000000FF) | ((buf[1] << 8) & 0x0000FF00);
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
    struct timeval tv1;
    gettimeofday(&tv1, NULL);
    return (unsigned long long)tv1.tv_sec * 1000 * 1000 + tv1.tv_usec;
}

}  // namespace xdb
