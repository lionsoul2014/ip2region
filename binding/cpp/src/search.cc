
#include "search.h"

namespace xdb {

search_t::search_t(const string &file, int version, int p)
    : db(fopen(file.data(), "r")), header(db), policy(p) {
    init_xdb(version);

    if (db == NULL)
        log_exit("can't open " + file);
    if (header.ip_version() != version)
        log_exit("ip 版本不匹配");

    if (policy != policy_file) {
        read_bin(length_header, vector, length_vector, db);
        if (policy == policy_content) {
            fseek(db, 0, SEEK_END);
            int size = ftell(db) - length_vector - length_header;
            content  = (char *)malloc(size);
            read_bin(length_vector + length_header, content, size, db);
        }
    }
}

search_t::~search_t() {
    fclose(db);
    if (policy == policy_content)
        free(content);
}

int search_t::get_io_count() {
    return io_count;
}

int search_t::get_cost_time() {
    return cost_time;
}

char const *search_t::get_content_index_help(int index) {
    if (policy != policy_file)
        return vector + index;

    ++io_count;
    static char v[8];
    read_bin(length_header + index, v, sizeof(v), db);
    return v;
}

void search_t::get_content_index(const ip_t &ip, int &left, int &right) {
    int index = ((unsigned char)ip.p[0] * 256 + (unsigned char)ip.p[1]) * 8;

    const char *p = get_content_index_help(index);
    left          = to_uint(p);
    right         = to_uint(p + 4);
}

char const *search_t::get_content_help(int index) {
    if (policy == policy_content)
        return content + index - length_header - length_vector;
    ++io_count;
    static char v[16 + 16 + 2 + 4];
    read_bin(index, v, content_size, db);
    return v;
}

string search_t::get_region(int index, int len) {
    if (policy == policy_content)
        return string(content + index - length_header - length_vector, len);
    ++io_count;
    char *p = (char *)malloc(sizeof(char) * len);
    read_bin(index, p, len, db);
    string res(p, len);
    free(p);
    return res;
}

void search_t::get_content(int   index,
                           ip_t &ip_left,
                           ip_t &ip_right,
                           int  &region_len,
                           int  &region_index) {
    const char *p = get_content_help(index);

    ip_left.from_xdb(p);
    ip_right.from_xdb(p + ip_size);

    region_len   = to_ushort(p + ip_size * 2);
    region_index = to_uint(p + ip_size * 2 + 2);
}

string search_t::search(const ip_t &ip) {
    io_count = 0;

    int content_left, content_right;
    get_content_index(ip, content_left, content_right);

    if (content_left == 0 || content_right == 0)
        return "";

    ip_t ip_left, ip_right;
    int  region_len;
    int  region_index;

    int left  = 0;
    int right = (content_right - content_left) / content_size;

    for (;;) {
        int mid       = left + (right - left) / 2;
        int mid_index = content_left + mid * content_size;
        get_content(mid_index, ip_left, ip_right, region_len, region_index);

        // ip ip_left ip_right
        if (ip < ip_left)
            right = mid - 1;
        // ip_left ip_right ip
        else if (ip_right < ip)
            left = mid + 1;
        else
            return get_region(region_index, region_len);
    }
}

string search_t::search(const string &str) {
    unsigned long long t1 = get_time();

    ip_t ip;
    if (ip.from_str(str) == false)
        return "invalid ipv" + std::to_string(ip_version) + ": " + str;
    string region = search(ip);

    unsigned long long t2 = get_time();
    cost_time             = t2 - t1;
    return region;
}

}  // namespace xdb
