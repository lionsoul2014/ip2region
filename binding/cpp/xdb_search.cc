
#include "xdb_search.h"

#include <arpa/inet.h>
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

static void read_bin(int index, char *buf, size_t len, FILE *db) {
    fseek(db, index, SEEK_SET);
    if (fread(buf, 1, len, db) != len)
        log_exit(__func__);
}

static unsigned int read_uint(const char *buf) {
    return ((buf[0]) & 0x000000FF) | ((buf[1] << 8) & 0x0000FF00) |
           ((buf[2] << 16) & 0x00FF0000) | ((buf[3] << 24) & 0xFF000000);
}

static unsigned short read_ushort(const char *buf) {
    return ((buf[0]) & 0x000000FF) | ((buf[1] << 8) & 0x0000FF00);
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

void xdb_search_t::get_content_index(unsigned int  ip,
                                     unsigned int &left,
                                     unsigned int &right) {
    unsigned int ip_1  = (ip >> 24) & 0xFF;
    unsigned int ip_2  = (ip >> 16) & 0xFF;
    unsigned int index = (ip_1 * vector_index_cols + ip_2) * vector_index_size;

    if (content != NULL) {
        left  = read_uint(content + index + header_length);
        right = read_uint(content + index + header_length + 4);
    } else if (vector_index != NULL) {
        left  = read_uint(vector_index + index);
        right = read_uint(vector_index + index + 4);
    } else {
        ++io_count;
        char buf[8];
        read_bin(header_length + index, buf, sizeof(buf), db);
        left  = read_uint(buf);
        right = read_uint(buf + 4);
    }
}

void xdb_search_t::get_content(unsigned int    index,
                               unsigned int   &ip_left,
                               unsigned int   &ip_right,
                               unsigned short &region_len,
                               unsigned int   &region_index) {
    char        buf[segment_index_size];  // 4 + 4 + 2 + 4
    const char *p;

    if (content != NULL) {
        p = content + index;
    } else {
        ++io_count;
        read_bin(index, buf, sizeof(buf), db);
        p = buf;
    }
    ip_left      = read_uint(p);
    ip_right     = read_uint(p + 4);
    region_len   = read_ushort(p + 8);
    region_index = read_uint(p + 10);
}

std::string xdb_search_t::get_region(unsigned int index, unsigned short len) {
    if (content != NULL) {
        return std::string(content + index, len);
    } else {
        ++io_count;
        char *buf = (char *)malloc(sizeof(char) * len);
        read_bin(index, buf, len, db);
        std::string res(buf, len);
        free(buf);
        return res;
    }
}

xdb_search_t::xdb_search_t(const std::string &file_name) {
    db           = fopen(file_name.data(), "r");
    vector_index = NULL;
    content      = NULL;

    if (db == NULL)
        log_exit("can't open " + file_name);
}

void xdb_search_t::init_file() {
}

void xdb_search_t::init_vector_index() {
    vector_index = (char *)malloc(vector_index_length);
    read_bin(header_length, vector_index, vector_index_length, db);
}

void xdb_search_t::init_content() {
    fseek(db, 0, SEEK_END);
    unsigned int size = ftell(db);
    content           = (char *)malloc(size);
    read_bin(0, content, size, db);
}

xdb_search_t::~xdb_search_t() {
    if (db != NULL) {
        fclose(db);
        db = NULL;
    }
    if (vector_index != NULL) {
        free(vector_index);
        vector_index = NULL;
    }
    if (content != NULL) {
        free(content);
        content = NULL;
    }
}

unsigned long long xdb_search_t::get_io_count() {
    return io_count;
}

unsigned long long xdb_search_t::get_cost_time() {
    return cost_time;
}

std::string xdb_search_t::search(const std::string &ip_str) {
    unsigned long long t1 = get_time();

    unsigned int ip_uint;
    if (!ip2uint(ip_str.data(), ip_uint))
        return "invalid ip: " + ip_str;
    std::string region = search(ip_uint);

    unsigned long long t2 = get_time();
    cost_time             = t2 - t1;
    return region;
}

std::string xdb_search_t::search(unsigned int ip_uint) {
    io_count = 0;

    unsigned int content_index_left, content_index_right;
    get_content_index(ip_uint, content_index_left, content_index_right);

    unsigned int   left, right, mid;
    unsigned int   ip_left, ip_right;
    unsigned short region_len;
    unsigned int   region_index;
    unsigned int   mid_index;

    left  = 0;
    right = (content_index_right - content_index_left) / segment_index_size;

    for (;;) {
        mid       = left + (right - left) / 2;
        mid_index = content_index_left + mid * segment_index_size;
        get_content(mid_index, ip_left, ip_right, region_len, region_index);

        if (ip_left > ip_uint)
            right = mid - 1;
        else if (ip_right < ip_uint)
            left = mid + 1;
        else
            return get_region(region_index, region_len);
    }
}
