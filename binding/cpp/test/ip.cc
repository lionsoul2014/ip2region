#include "../src/ip.h"

void expect_round_trip(int version, const std::string& address) {
    xdb::init_xdb(version);

    xdb::ip_t ip;
    if (!ip.from_str(address))
        xdb::log_exit("failed to parse " + address);
    if (ip.to_string() != address)
        xdb::log_exit("round trip mismatch for " + address);
}

int main() {
    expect_round_trip(xdb::ipv4, "192.0.2.1");
    expect_round_trip(xdb::ipv6, "2001:db8::1");

    xdb::init_xdb(xdb::ipv4);
    xdb::ip_t invalid;
    if (invalid.from_str("not-an-ip"))
        xdb::log_exit("accepted an invalid IPv4 address");

    return 0;
}
