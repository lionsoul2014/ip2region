
#include "ip.h"

#ifdef _WIN32
#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0600
#endif
#include <winsock2.h>
#include <ws2tcpip.h>
#else
#include <arpa/inet.h>
#endif

namespace {

bool parse_ip_address(int family, const std::string& address, void* destination) {
#ifdef _WIN32
    return InetPtonA(family, address.c_str(), destination) == 1;
#else
    return inet_pton(family, address.c_str(), destination) == 1;
#endif
}

const char* format_ip_address(int family,
                              const void* address,
                              char* destination,
                              size_t destination_size) {
#ifdef _WIN32
    return InetNtopA(family,
                     const_cast<void*>(address),
                     destination,
                     static_cast<DWORD>(destination_size));
#else
    return inet_ntop(family, address, destination, destination_size);
#endif
}

}  // namespace

namespace xdb {

ip_t::ip_t() {
    memset(p, '\0', sizeof(p));
}

ip_t::ip_t(const ip_t& rhs, int val) {
    memcpy(p, rhs.p, ip_size);

    if (val == 0 || val == 255)
        for (int i = 2; i < ip_size; ++i)
            p[i] = val;
}

ip_t::ip_t(const char* p) {
    from_xdb(p);
}

bool ip_t::from_str(const string& str) {
    int af_inet = ip_version == ipv4 ? AF_INET : AF_INET6;
    return parse_ip_address(af_inet, str, p);
}

void ip_t::from_xdb(const char str[16]) {
    for (int i = 0; i < ip_size; ++i)
        if (ip_version == ipv6)
            p[i] = str[i];
        else
            p[i] = str[ip_size - 1 - i];
}

ip_t& ip_t::operator=(const ip_t& rhs) {
    memcpy(p, rhs.p, ip_size);
    return *this;
}

int ip_t::compare(const ip_t& rhs) const {
    for (int i = 0; i < ip_size; ++i) {
        if ((unsigned char)p[i] > (unsigned char)rhs.p[i])
            return 1;
        if ((unsigned char)p[i] < (unsigned char)rhs.p[i])
            return -1;
    }
    return 0;
}

bool ip_t::operator<(const ip_t& rhs) const {
    return compare(rhs) < 0;
}

bool ip_t::operator<=(const ip_t& rhs) const {
    return compare(rhs) <= 0;
}

bool ip_t::operator>(const ip_t& rhs) const {
    return compare(rhs) > 0;
}

bool ip_t::operator>=(const ip_t& rhs) const {
    return compare(rhs) >= 0;
}

bool ip_t::operator==(const ip_t& rhs) const {
    return compare(rhs) == 0;
}

bool ip_t::operator!=(const ip_t& rhs) const {
    return compare(rhs) != 0;
}

string ip_t::to_string() const {
    char buf[INET6_ADDRSTRLEN + 1];
    int  af_inet = ip_version == ipv4 ? AF_INET : AF_INET6;
    if (format_ip_address(af_inet, p, buf, sizeof(buf)) == NULL)
        log_exit("failed to format ipv" + std::to_string(ip_version));
    return string(buf);
}

string ip_t::to_bit() const {
    string str;
    for (int i = 0; i < ip_size; ++i)
        if (ip_version == ipv6)
            str.push_back(p[i]);
        else
            str.push_back(p[ip_size - 1 - i]);
    return str;
}

ip_t operator+(const ip_t& lhs, int v) {
    ip_t ip;

    int i = ip_size;
    while (--i >= 0) {
        v += lhs.p[i];
        ip.p[i] = v % 256;
        v /= 256;
    }
    return ip;
}

ip_t operator-(const ip_t& lhs, int v) {
    ip_t ip;

    int i = ip_size;
    v     = -v;
    while (--i >= 0) {
        v += lhs.p[i];
        if (v == -1)
            ip.p[i] = 255;
        else {
            ip.p[i] = v;
            v       = 0;
        }
    }
    return ip;
}

// node_t
node_t::node_t() {
}

node_t::node_t(char* buf) {
    char* pos1 = strchr(buf, '|');

    if (pos1 == NULL)
        log_exit("invalid data: " + std::string(buf));
    char* pos2 = strchr(pos1 + 1, '|');
    if (pos2 == NULL)
        log_exit("invalid data: " + std::string(buf));
    *pos1 = '\0';
    *pos2 = '\0';

    region = pos2 + 1;

    if (!ip1.from_str(buf) || !ip2.from_str(pos1 + 1) || ip2 < ip1 ||
        region.empty()) {
        *pos1 = *pos2 = '|';
        log_exit(string("invalid data: ") + buf);
    }
}

bool node_t::operator<(const node_t& rhs) const {
    if (ip1 < rhs.ip1)
        return true;
    return ip2 < rhs.ip2;
}

string node_t::to_string() const {
    return ip1.to_string() + "|" + ip2.to_string() + "|" + region;
}

string node_t::to_bit() const {
    return ip1.to_bit() + ip2.to_bit();
}

}  // namespace xdb
