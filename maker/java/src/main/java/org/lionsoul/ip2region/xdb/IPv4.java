// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package org.lionsoul.ip2region.xdb;

// IPv4 version implementation
// @Author Lion <chenxin619315@gmail.com>
// @Date   2025/09/10

public class IPv4 extends Version {
    public IPv4() {
        // segmentIndex: 4 + 4 + 2 + 4
        super(4, "IPv4", 4, 14);
    }

    @Override
    public int putBytes(byte[] buff, int offset, byte[] ip) {
        // use the Little endian byte order to compatible with the old searcher implementation
        buff[offset++] = ip[3];
        buff[offset++] = ip[2];
        buff[offset++] = ip[1];
        buff[offset  ] = ip[0];
        return ip.length;
    }
}
