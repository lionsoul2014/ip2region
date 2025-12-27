// Copyright 2025 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package org.lionsoul.ip2region.xdb;

// IP version abstract manager (IPv4 & IPv6)
// @Author Lion <chenxin619315@gmail.com>
// @Date   2025/09/10

public abstract class Version {
    public static final int IPv4VersionNo = 4;
    public static final int IPv6VersionNo = 6;

    public static final IPv4 IPv4 = new IPv4();
    public static final IPv6 IPv6 = new IPv6();

    // version id and name
    public final int id;
    public final String name;
    
    // the numbers of bytes for one IP
    public final int bytes;

    // segment index size (bytes)
    public final int segmentIndexSize;

    public Version(int id, String name, int bytes, int segmentIndexSize) {
        this.id = id;
        this.name = name;
        this.bytes = bytes;
        this.segmentIndexSize = segmentIndexSize;
    }

    // encode the specified IP bytes to the specified buffer
    public abstract int putBytes(byte[] buff, int offset, byte[] ip);

    // compare the two IPs with the current version.
    // Returns: -1 if ip1 < ip2, 0 if ip1 == ip2, 1 if ip1 > ip2
    public int ipCompare(byte[] ip1, byte[] ip2) {
        return ipSubCompare(ip1, ip2, 0);
    }

    // @see ipCompare
    public abstract int ipSubCompare(byte[] ip1, byte[] buff, int offset);

    // parse the version from an name
    public static final Version fromName(String name) throws Exception {
        final String n = name.toUpperCase();
        if (n.equals("V4") || n.equals("IPV4")) {
            return IPv4;
        } else if (n.equals("V6") || n.equals("IPV6")) {
            return IPv6;
        } else {
            throw new Exception("invalid version name `"+name+"`");
        }
    }

    // parse the version from header
    public static final Version fromHeader(Header header) throws XdbException {
        // Old 2.0 structure with IPv4 supports ONLY.
        if (header.version == Searcher.STRUCTURE_20) {
            return IPv4;
        }

        // structure 3.0 after IPv6 supporting
        if (header.version != Searcher.STRUCTURE_30) {
            throw new XdbException("invalid xdb structure version `"+header.version+"`");
        }

        if (header.ipVersion == IPv4VersionNo) {
            return IPv4;
        } else if (header.ipVersion == IPv6VersionNo) {
            return IPv6;
        } else {
            throw new XdbException("invalid ip version number `" + header.ipVersion + "`");
        }
    }

    @Override public String toString() {
        return String.format("{Id:%d, Name:%s, Bytes:%d, IndexSize: %d}", id, name, bytes, segmentIndexSize);
    }
}