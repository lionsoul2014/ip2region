
#include "xdb_make.h"

#include <arpa/inet.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#include <iostream>

static void log_exit(const std::string &msg) {
    std::cout << msg << std::endl;
    exit(-1);
}

static unsigned long long get_time() {
    struct timeval tv1;
    gettimeofday(&tv1, NULL);
    return (unsigned long long)tv1.tv_sec * 1000 * 1000 + tv1.tv_usec;
}

static void write_uint(unsigned int data, char buf[]) {
    buf[0] = (data >> 0) & 0xFF;
    buf[1] = (data >> 8) & 0xFF;
    buf[2] = (data >> 16) & 0xFF;
    buf[3] = (data >> 24) & 0xFF;
}

static void write_uint(unsigned int data, FILE *dst) {
    char buf[4];
    write_uint(data, buf);
    fwrite(buf, 1, sizeof(buf), dst);
}

static void write_ushort(unsigned short data, char buf[]) {
    buf[0] = (data >> 0) & 0xFF;
    buf[1] = (data >> 8) & 0xFF;
}

static void write_ushort(unsigned short data, FILE *dst) {
    char buf[2];
    write_ushort(data, buf);
    fwrite(buf, 1, sizeof(buf), dst);
}

static void write_string(const char *buf, unsigned int len, FILE *dst) {
    fwrite(buf, 1, len, dst);
}

static bool ip2uint(const char *buf, unsigned int &ip) {
    struct in_addr addr;
    if (inet_pton(AF_INET, buf, &addr) == 0)
        return false;
    // 网络字节序为大端存储, 在此转换为小端存储
    ip = (((addr.s_addr >> 0) & 0xFF) << 24) |
         (((addr.s_addr >> 8) & 0xFF) << 16) |
         (((addr.s_addr >> 16) & 0xFF) << 8) |
         (((addr.s_addr >> 24) & 0xFF) << 0);
    return true;
}

static std::string uint2ip(unsigned int ip) {
    char buf[16];
    snprintf(buf,
             sizeof(buf),
             "%d.%d.%d.%d",
             (ip >> 24) & 0xFF,
             (ip >> 16) & 0xFF,
             (ip >> 8) & 0xFF,
             ip & 0xFF);
    return std::string(buf);
}

void xdb_make_t::vector_index_push_back(unsigned int row,
                                        unsigned int col,
                                        unsigned int ip1,
                                        unsigned int ip2,
                                        const char  *region_str) {
    char buf[8];
    write_uint(ip1, buf);
    write_uint(ip2, buf + 4);

    vector_index[row][col].push_back(std::make_pair<std::string, std::string>(
        std::string(buf, sizeof(buf)), region_str));
}

void xdb_make_t::vector_index_push_back(unsigned int ip1,
                                        unsigned int ip2,
                                        const char  *region_str) {
    unsigned int ip1_1 = (ip1 >> 24) & 0xFF;
    unsigned int ip1_2 = (ip1 >> 16) & 0xFF;
    unsigned int ip2_1 = (ip2 >> 24) & 0xFF;
    unsigned int ip2_2 = (ip2 >> 16) & 0xFF;

    if (ip1_1 == ip2_1 && ip1_2 == ip2_2) {
        vector_index_push_back(ip1_1, ip1_2, ip1, ip2, region_str);
        return;
    }

    vector_index_push_back(ip1_1, ip1_2, ip1, ip1 | 0x0000FFFF, region_str);
    vector_index_push_back(ip2_1, ip2_2, ip2 & 0xFFFF0000, ip2, region_str);

    for (;;) {
        ++ip1_2;
        if (ip1_2 == 256) {
            ++ip1_1;
            ip1_2 = 0;
        }
        if (ip1_1 == ip2_1 && ip1_2 == ip2_2)
            break;
        ip1 = (ip1_1 << 24) | (ip1_2 << 16);
        vector_index_push_back(ip1_1, ip1_2, ip1, ip1 | 0x0000FFFF, region_str);
    }
}

void xdb_make_t::handle_input_help(char *buf) {
    static unsigned int region_index = vector_index_length + header_length;
    static unsigned int next_ip      = 0;

    // 去掉多余的空
    unsigned int buf_len = strlen(buf);
    while (buf_len > 0 && isspace(buf[buf_len - 1]))
        --buf_len;
    if (buf_len == 0)
        return;
    buf[buf_len] = '\0';

    char *pos1 = strchr(buf, '|');

    if (pos1 == NULL)
        log_exit("invalid data: " + std::string(buf));
    char *pos2 = strchr(pos1 + 1, '|');
    if (pos2 == NULL)
        log_exit("invalid data: " + std::string(buf));
    *pos1 = '\0';
    *pos2 = '\0';

    const char *region_str = pos2 + 1;

    unsigned int ip1, ip2;
    if (!ip2uint(buf, ip1) || !ip2uint(pos1 + 1, ip2) || ip1 > ip2 ||
        *region_str == '\0') {
        *pos1 = *pos2 = '|';
        log_exit(std::string("invalid data: ") + buf);
    }

    if (next_ip != ip1)
        log_exit("ip 不连续: " + uint2ip(ip1));
    next_ip = ip2 + 1;

    if (region.find(region_str) == region.end()) {
        region[region_str] = region_index;
        region_index += strlen(region_str);
    }

    vector_index_push_back(ip1, ip2, region_str);
}

void xdb_make_t::handle_input(const std::string &file_name) {
    FILE *src = fopen(file_name.data(), "r");
    if (src == NULL)
        log_exit("can't open " + file_name);

    char buf[1024];
    while (fgets(buf, sizeof(buf), src) != NULL)
        handle_input_help(buf);
    fclose(src);
}

void xdb_make_t::handle_header() {
    char buf[header_length];
    memset(buf, 0, header_length);
    write_ushort(2, buf);             // 版本号
    write_ushort(1, buf + 2);         // 缓存策略
    write_uint(time(NULL), buf + 4);  // 时间
    // 索引
    unsigned int content_left = header_length + vector_index_length;
    for (auto &d : region)
        content_left += d.first.size();
    unsigned int content_right = content_left;

    for (int i = 0; i < vector_index_rows; ++i)
        for (int j = 0; j < vector_index_cols; ++j)
            content_right += vector_index[i][j].size() * segment_index_size;
    content_right -= segment_index_size;
    write_uint(content_left, buf + 8);
    write_uint(content_right, buf + 12);
    write_string(buf, header_length, dst);
}

void xdb_make_t::handle_vector_index() {
    unsigned int index = header_length + vector_index_length;
    for (auto &d : region)
        index += d.first.size();
    for (unsigned i = 0; i < vector_index_rows; ++i)
        for (unsigned j = 0; j < vector_index_cols; ++j) {
            write_uint(index, dst);
            index += segment_index_size * vector_index[i][j].size();
            write_uint(index, dst);
        }
}

void xdb_make_t::handle_region() {
    for (auto &d : region) {
        fseek(dst, d.second, SEEK_SET);
        write_string(d.first.data(), d.first.size(), dst);
    }
}

void xdb_make_t::handle_content() {
    fseek(dst, 0, SEEK_END);
    for (unsigned i = 0; i < vector_index_rows; ++i)
        for (unsigned j = 0; j < vector_index_cols; ++j)
            for (auto d : vector_index[i][j]) {
                write_string(d.first.data(), d.first.size(), dst);
                write_ushort(d.second.size(), dst);
                write_uint(region[d.second], dst);
            }
}

xdb_make_t::xdb_make_t(const std::string &file_name_src,
                       const std::string &file_name_dst) {
    unsigned long long tv1 = get_time();

    handle_input(file_name_src);

    dst = fopen(file_name_dst.data(), "w");
    if (dst == NULL)
        log_exit("can't open " + std::string(file_name_dst));

    handle_header();
    handle_vector_index();
    handle_region();
    handle_content();

    fclose(dst);

    unsigned long long tv2 = get_time();
    printf("took: %.2fs\n", (tv2 - tv1) * 1.0 / 1000 / 1000);
}
