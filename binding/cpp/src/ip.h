#ifndef IP_H
#define IP_H

#include "base.h"

namespace xdb {

struct ip_t {
    unsigned char p[16];

    ip_t();
    ip_t(const char* p);
    // val 为 0 或 255 时, 将 ip 的后几位置为 val
    ip_t(const ip_t& rhs, int val = -1);

    bool from_str(const string& str);
    void from_xdb(const char str[16]);

    ip_t& operator=(const ip_t& rhs);

    int  compare(const ip_t& rhs) const;
    bool operator<(const ip_t& rhs) const;
    bool operator<=(const ip_t& rhs) const;
    bool operator>(const ip_t& rhs) const;
    bool operator>=(const ip_t& rhs) const;
    bool operator==(const ip_t& rhs) const;
    bool operator!=(const ip_t& rhs) const;

    string to_string() const;
    string to_bit() const;
};

ip_t operator+(const ip_t& lhs, int v);
ip_t operator-(const ip_t& lhs, int v);

struct node_t {
    ip_t   ip1;
    ip_t   ip2;
    string region;

    node_t();
    node_t(char* buf);

    bool operator<(const node_t& rhs) const;

    string to_string() const;
    string to_bit() const;
};

}  // namespace xdb

#endif
