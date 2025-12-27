// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package org.lionsoul.ip2region.xdb;

// IPv4 version implementation
// @Author Lion <chenxin619315@gmail.com>
// @Date   2025/09/10

public class IPv6 extends Version {

    public IPv6() {
        // segmentIndex: 16 + 16 + 2 + 4
        super(6, "IPv6", 16, 38);
    }

    @Override
    public int putBytes(byte[] buff, int offset, byte[] ip) {
        System.arraycopy(ip, 0, buff, offset, ip.length);
        return ip.length;
    }

}
