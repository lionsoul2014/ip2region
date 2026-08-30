#include "../src/ip.h"

void expect_round_trip(int version, const std::string& address) {
    xdb::init_xdb(version);

    xdb::ip_t ip;
    if (!ip.from_str(address))
        xdb::log_exit("failed to parse " + address);
    if (ip.to_string() != address)
        xdb::log_exit("round trip mismatch for " + address);
}

void expect_binary_decoding() {
    const char uint_bytes[] = {
        static_cast<char>(0x89),
        static_cast<char>(0xAB),
        static_cast<char>(0xCD),
        static_cast<char>(0xEF),
    };
    const char ushort_bytes[] = {
        static_cast<char>(0xCD),
        static_cast<char>(0xEF),
    };

    if (xdb::to_uint(uint_bytes) != 0xEFCDAB89U)
        xdb::log_exit("failed to decode a 32-bit value with high bits");
    if (xdb::to_ushort(ushort_bytes) != 0xEFCDU)
        xdb::log_exit("failed to decode a 16-bit value with high bits");
}

int main() {
    expect_binary_decoding();
    expect_round_trip(xdb::ipv4, "192.0.2.1");
    expect_round_trip(xdb::ipv6, "2001:db8::1");

    xdb::init_xdb(xdb::ipv4);
    xdb::ip_t invalid;
    if (invalid.from_str("not-an-ip"))
        xdb::log_exit("accepted an invalid IPv4 address");

    return 0;
}
