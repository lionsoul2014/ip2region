
#include "xdb_edit.h"

#include <arpa/inet.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#include <algorithm>
#include <iostream>

static void log_exit(const std::string& msg) {
    std::cout << msg << std::endl;
    exit(-1);
}

static unsigned long long get_time() {
    struct timeval tv1;
    gettimeofday(&tv1, NULL);
    return (unsigned long long)tv1.tv_sec * 1000 * 1000 + tv1.tv_usec;
}

static bool ip2uint(const char* buf, unsigned int& ip) {
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

static void handle_ip_txt(const std::string&     file_name,
                          std::list<xdb_node_t>& regions) {
    FILE* f = fopen(file_name.data(), "r");
    if (f == NULL)
        log_exit("can't open " + file_name);

    char buf[1024];
    while (fgets(buf, sizeof(buf), f) != NULL) {
        unsigned int buf_len = strlen(buf);
        // 去掉多余的空
        while (buf_len > 0 && isspace(buf[buf_len - 1]))
            --buf_len;
        if (buf_len == 0)
            continue;
        buf[buf_len] = '\0';
        regions.push_back(xdb_node_t(buf));
    }

    fclose(f);
}

// xdb_node_t
xdb_node_t::xdb_node_t() {
}

xdb_node_t::xdb_node_t(char* buf) {
    char* pos1 = strchr(buf, '|');

    if (pos1 == NULL)
        log_exit("invalid data: " + std::string(buf));
    char* pos2 = strchr(pos1 + 1, '|');
    if (pos2 == NULL)
        log_exit("invalid data: " + std::string(buf));
    *pos1 = '\0';
    *pos2 = '\0';

    region = pos2 + 1;
    if (!ip2uint(buf, ip1) || !ip2uint(pos1 + 1, ip2) || ip1 > ip2 ||
        region.empty()) {
        *pos1 = *pos2 = '|';
        log_exit(std::string("invalid data: ") + buf);
    }
}

bool xdb_node_t::operator<(const xdb_node_t& rhs) const {
    if (ip1 < rhs.ip1)
        return true;
    if (ip1 > rhs.ip1)
        return false;
    return ip2 < rhs.ip2;
}

std::string xdb_node_t::to_string() const {
    return uint2ip(ip1) + "|" + uint2ip(ip2) + "|" + region;
}

void xdb_edit_t::handle_new_file(const std::string& file_name) {
    // 输入
    handle_ip_txt(file_name, new_regions);

    // 排序
    new_regions.sort();
    // 检验及其去重
    auto it = new_regions.begin();

    for (;;) {
        if (it == new_regions.end())
            break;
        auto next = it;
        ++next;
        if (next == new_regions.end())
            break;
        if (it->ip1 == next->ip1 || it->ip2 >= next->ip1) {
            // 数据重叠
            if (it->region != next->region)
                log_exit("数据有二义性: " + it->to_string() + ", " +
                         next->to_string());
            it->ip2 = std::max(it->ip2, next->ip2);
            new_regions.erase(next);
        } else if (it->ip2 + 1 == next->ip1 && it->region == next->region) {
            // 数据连接
            it->ip2 = next->ip2;
            new_regions.erase(next);
        } else {
            ++it;
        }
    }
}

void xdb_edit_t::handle_old_file(const std::string& file_name) {
    handle_ip_txt(file_name, old_regions);
}

void xdb_edit_t::merge() {
    auto it1 = old_regions.begin();
    auto it2 = new_regions.begin();

    for (;;) {
        if (it2 == new_regions.end())
            break;
        if (it2->ip1 > it2->ip2) {
            // 失效数据
            ++it2;
            continue;
        }
        while (it1->ip2 < it2->ip1)
            ++it1;
        if (it1->ip2 <= it2->ip2) {
            xdb_node_t node;
            node.ip1    = it2->ip1;
            node.ip2    = it1->ip2;
            node.region = it2->region;

            it1->ip2 = node.ip1 - 1;
            it2->ip1 = node.ip2 + 1;

            //            std::cout << "insert: " << node.to_string() <<
            //            std::endl;
            ++it1;
            it1 = old_regions.insert(it1, node);
            ++it1;
        } else {
            xdb_node_t node;
            node.ip1    = it2->ip2 + 1;
            node.ip2    = it1->ip2;
            node.region = it1->region;

            it1->ip2 = it2->ip1 - 1;

            //          std::cout << "insert: " << it2->to_string() <<
            //          std::endl;
            ++it1;
            it1 = old_regions.insert(it1, *it2);

            ++it1;
            it1 = old_regions.insert(it1, node);

            ++it2;
        }
    }
}

void xdb_edit_t::write_old_file(const std::string& file_name) {
    FILE* f = fopen(file_name.data(), "w");
    if (f == NULL)
        log_exit("can't open " + file_name);

    auto it = old_regions.begin();

    // 删除非法的数据
    for (;;) {
        if (it == old_regions.end())
            break;
        if (it->ip1 > it->ip2)
            it = old_regions.erase(it);
        else
            ++it;
    }

    // 合并数据域相同的相邻数据
    it = old_regions.begin();
    for (;;) {
        if (it == old_regions.end())
            break;
        auto next = it;
        ++next;
        if (next == old_regions.end())
            break;
        if (it->region == next->region) {
            it->ip2 = next->ip2;
            old_regions.erase(next);
        } else {
            ++it;
        }
    }

    for (auto& d : old_regions) {
        std::string res =
            uint2ip(d.ip1) + "|" + uint2ip(d.ip2) + "|" + d.region + "\n";
        fputs(res.data(), f);
    }

    fclose(f);
}

xdb_edit_t::xdb_edit_t(const std::string& file_name_old,
                       const std::string& file_name_new) {
    unsigned long long tv1 = get_time();

    handle_new_file(file_name_new);
    handle_old_file(file_name_old);
    merge();
    write_old_file(file_name_old);

    unsigned long long tv2 = get_time();

    double took = (tv2 - tv1) * 1.0 / 1000 / 1000;

    printf("took: %.2fs\n", took);
}
