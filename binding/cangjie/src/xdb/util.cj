package ip2region.xdb

import std.net.IPAddress

// --- IP address parsing and formatting

// Parse IP string to byte array
// For IPv4, returns 4 bytes in big-endian order (network byte order)
// For IPv6, returns 16 bytes in big-endian order
public func parseIP(ip: String): Array<Byte> {
    let addr = IPAddress.parse(ip)
    return addr.getAddressBytes()
}

// Convert IP byte array back to string
public func ipToString(ip: Array<Byte>): String {
    if (ip.size == 4) {
        return "${Int64(ip[0])}.${Int64(ip[1])}.${Int64(ip[2])}.${Int64(ip[3])}"
    }
    // IPv6 simplified format
    var result = ""
    for (i in 0..ip.size) {
        if (i > 0 && i % 2 == 0) {
            result = result + ":"
        }
        result = result + "${Int64(ip[i])}"
    }
    return result
}

// Compare two IP byte arrays.
// Returns: -1 if ip1 < ip2, 0 if equal, 1 if ip1 > ip2
public func ipCompare(ip1: Array<Byte>, ip2: Array<Byte>): Int64 {
    var len = ip1.size
    if (ip2.size < len) {
        len = ip2.size
    }
    for (i in 0..len) {
        if (ip1[i] < ip2[i]) {
            return -1
        } else if (ip1[i] > ip2[i]) {
            return 1
        }
    }
    if (ip1.size < ip2.size) {
        return -1
    } else if (ip1.size > ip2.size) {
        return 1
    }
    return 0
}

// --- Binary data reading helpers for xdb format

// Read a little-endian UInt32 from a byte buffer at the given offset
public func readLEUint32(buf: Array<Byte>, offset: Int64): Int64 {
    let b0 = Int64(buf[offset])
    let b1 = Int64(buf[offset + 1]) << 8
    let b2 = Int64(buf[offset + 2]) << 16
    let b3 = Int64(buf[offset + 3]) << 24
    return b0 | b1 | b2 | b3
}

// Read a little-endian UInt16 from a byte buffer at the given offset
public func readLEUint16(buf: Array<Byte>, offset: Int64): Int64 {
    let b0 = Int64(buf[offset])
    let b1 = Int64(buf[offset + 1]) << 8
    return b0 | b1
}

// Compare IPv4 address (big-endian) with xdb stored IPv4 (little-endian)
// xdb stores IPv4 in little-endian, so we compare ip[i] with xdbEntry[3-i]
public func compareIPv4(ip: Array<Byte>, xdbEntry: Array<Byte>, entryOffset: Int64): Int64 {
    for (i in 0..4) {
        let ipByte = Int64(ip[i])
        let xdbByte = Int64(xdbEntry[entryOffset + 3 - i])
        if (ipByte < xdbByte) {
            return -1
        } else if (ipByte > xdbByte) {
            return 1
        }
    }
    return 0
}

// Load the vector index from a content buffer
// The vector index starts at HeaderInfoLength and has VectorIndexRows * VectorIndexCols * VectorIndexSize bytes
public func loadVectorIndex(cBuff: Array<Byte>): Array<Byte> {
    let start = HeaderInfoLength
    let len = VectorIndexRows * VectorIndexCols * VectorIndexSize
    let vi = Array<Byte>(len, repeat: 0)
    for (i in 0..len) {
        vi[i] = cBuff[start + i]
    }
    return vi
}

// Compare IPv6 address (big-endian) with xdb stored IPv6 (big-endian)
public func compareIPv6(ip: Array<Byte>, xdbEntry: Array<Byte>, entryOffset: Int64): Int64 {
    for (i in 0..16) {
        if (ip[i] < xdbEntry[entryOffset + i]) {
            return -1
        } else if (ip[i] > xdbEntry[entryOffset + i]) {
            return 1
        }
    }
    return 0
}
