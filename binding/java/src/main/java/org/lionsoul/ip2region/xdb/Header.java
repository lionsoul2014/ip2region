// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/23

package org.lionsoul.ip2region.xdb;


// Internal class
class Header {
    public final int version;
    public final int indexPolicy;
    public final int createdAt;
    public final int startIndexPtr;
    public final int endIndexPtr;
    public final byte[] buffer;

    public Header(byte[] buff) {
        assert buff.length >= 16;
        version = InternalUtil.getInt2(buff, 0);
        indexPolicy = InternalUtil.getInt2(buff, 2);
        createdAt = InternalUtil.getInt(buff, 4);
        startIndexPtr = InternalUtil.getInt(buff, 8);
        endIndexPtr = InternalUtil.getInt(buff, 12);
        buffer = buff;
    }

    @Override
    public String toString() {
        return "{" +
                "Version: " + version + ',' +
                "IndexPolicy: " + indexPolicy + ',' +
                "CreatedAt: " + createdAt + ',' +
                "StartIndexPtr: " + startIndexPtr + ',' +
                "EndIndexPtr: " + endIndexPtr +
                '}';
    }
}
