
#include "make.h"

namespace xdb {

void make_t::vector_index_push_back(int row, int col, const node_t &node) {
    vector_index[row][col].push_back(
        std::make_pair<string, string>(node.to_bit(), string(node.region)));
}

void make_t::vector_index_push_back(node_t &node) {
    ip_t ip1 = node.ip1;
    ip_t ip2 = node.ip2;

    unsigned ip1_1 = ip1.p[0];
    unsigned ip1_2 = ip1.p[1];
    unsigned ip2_1 = ip2.p[0];
    unsigned ip2_2 = ip2.p[1];

    if (ip1_1 == ip2_1 && ip1_2 == ip2_2) {
        vector_index_push_back(ip1_1, ip1_2, node);
        return;
    }

    node.ip1 = ip1;
    node.ip2 = ip_t(ip1, 255);
    vector_index_push_back(ip1_1, ip1_2, node);

    node.ip1 = ip_t(ip2, 0);
    node.ip2 = ip2;
    vector_index_push_back(ip2_1, ip2_2, node);

    for (;;) {
        ++ip1_2;
        if (ip1_2 == 256) {
            ++ip1_1;
            ip1_2 = 0;
        }
        if (ip1_1 == ip2_1 && ip1_2 == ip2_2)
            break;
        ip1.p[0] = ip1_1;
        ip1.p[1] = ip1_2;
        node.ip1 = ip_t(ip1, 0);
        node.ip2 = ip_t(ip1, 255);
        vector_index_push_back(ip1_1, ip1_2, node);
    }
}

void make_t::handle_input_help(char *buf) {
    // 去掉多余的空
    unsigned int buf_len = strlen(buf);
    while (buf_len > 0 && isspace(buf[buf_len - 1]))
        --buf_len;
    if (buf_len == 0)
        return;
    buf[buf_len] = '\0';

    node_t node(buf);

    if (node.ip1 < next_ip) {
        log_exit("ip 未排序: " + node.ip1.to_string() + ", " +
                 next_ip.to_string());
    }

    next_ip = node.ip2 + 1;

    if (region.find(node.region) == region.end()) {
        region[node.region] = region_index;
        region_index += node.region.size();
    }

    vector_index_push_back(node);
}

void make_t::handle_input(const std::string &file_name) {
    FILE *src = fopen(file_name.data(), "r");
    if (src == NULL)
        log_exit("can't open " + file_name);

    char buf[1024];
    while (fgets(buf, sizeof(buf), src) != NULL)
        handle_input_help(buf);
    fclose(src);
}

void make_t::handle_header() {
    char buf[length_header];
    memset(buf, 0, length_header);
    write_ushort(3, buf);             // 版本号
    write_ushort(1, buf + 2);         // 缓存策略
    write_uint(time(NULL), buf + 4);  // 时间
    // 索引
    unsigned int content_left = length_header + length_vector;
    for (auto &d : region)
        content_left += d.first.size();
    unsigned int content_right = content_left;

    for (int i = 0; i < 256; ++i)
        for (int j = 0; j < 256; ++j)
            content_right += vector_index[i][j].size() * content_size;
    content_right -= content_size;
    write_uint(content_left, buf + 8);
    write_uint(content_right, buf + 12);
    write_ushort(ip_version, buf + 16);  // IP
    write_ushort(4, buf + 18);           // 指针数

    write_string(buf, length_header, db);
}

void make_t::handle_vector_index() {
    unsigned index = length_header + length_vector;
    for (auto &d : region)
        index += d.first.size();
    for (unsigned i = 0; i < 256; ++i)
        for (unsigned j = 0; j < 256; ++j)
            if (vector_index[i][j].size() == 0) {
                write_uint(0, db);
                write_uint(0, db);
            } else {
                write_uint(index, db);
                index += content_size * vector_index[i][j].size();
                write_uint(index, db);
            }
}

void make_t::handle_region() {
    for (auto &d : region) {
        fseek(db, d.second, SEEK_SET);
        write_string(d.first.data(), d.first.size(), db);
    }
}

void make_t::handle_content() {
    fseek(db, 0, SEEK_END);
    for (unsigned i = 0; i < 256; ++i)
        for (unsigned j = 0; j < 256; ++j)
            for (auto d : vector_index[i][j]) {
                write_string(d.first.data(), d.first.size(), db);
                write_ushort(d.second.size(), db);
                write_uint(region[d.second], db);
            }
}

make_t::make_t(const string &src, const string &dst, int version)
    : region_index(length_vector + length_header) {
    unsigned long long tv1 = get_time();

    init_xdb(version);

    handle_input(src);

    db = fopen(dst.data(), "w");
    if (db == NULL)
        log_exit("can't open " + dst);

    handle_header();
    handle_vector_index();
    handle_region();
    handle_content();

    fclose(db);

    unsigned long long tv2 = get_time();
    printf("took: %.2fs\n", (tv2 - tv1) * 1.0 / 1000 / 1000);
}

}  // namespace xdb
