// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/23

package org.lionsoul.ip2region.xdb;

public class Header {
    public final int version;
    public final int indexPolicy;
    public final long createdAt;
    public final long startIndexPtr;
    public final long endIndexPtr;

    // since xdb 3.0 with IPv6 supporting
    public final int ipVersion;
    public final int runtimePtrBytes;

    public final byte[] buffer;

    public Header(byte[] buff) {
        assert buff.length >= 16;
        version = LittleEndian.getUint16(buff, 0);
        indexPolicy = LittleEndian.getUint16(buff, 2);
        createdAt = LittleEndian.getUint32(buff, 4);
        startIndexPtr = LittleEndian.getUint32(buff, 8);
        endIndexPtr = LittleEndian.getUint32(buff, 12);
        ipVersion = LittleEndian.getUint16(buff, 16);
        runtimePtrBytes = LittleEndian.getUint16(buff, 18);
        buffer = buff;
    }

    @Override public String toString() {
        return "{" +
            "Version: " + version + ',' +
            "IndexPolicy: " + indexPolicy + ',' +
            "CreatedAt: " + createdAt + ',' +
            "StartIndexPtr: " + startIndexPtr + ',' +
            "EndIndexPtr: " + endIndexPtr + ',' + 
            "IPVersion: " + ipVersion + ',' + 
            "RuntimePtrBytes: " + runtimePtrBytes +
        '}';
    }
}
