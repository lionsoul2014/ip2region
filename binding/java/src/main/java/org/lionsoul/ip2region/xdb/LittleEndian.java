// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package org.lionsoul.ip2region.xdb;

// Little Endian basic data type decode and encode.
// @Author Lion <chenxin619315@gmail.com>
// @Date   2025/09/10

public class LittleEndian {

    public final static int[] shiftIndex = {0, 8, 16, 24, 32, 40, 48, 56};
    
    // put specified bytes to the buffer started from the offset
    public static void put(final byte[] buff, int offset, long value, int bytes) {
        if (bytes > 8) {
            throw new IndexOutOfBoundsException("bytes should be <= 8");
        }

        for (int i = 0; i < bytes; i++) {
            buff[offset++] = (byte)((value >>> shiftIndex[i]) & 0xFF);
        }
    }

    // put an uint32 (4 bytes long) to the buffer from the offset
    public static void putUint32(final byte[] buff, int offset, long value) {
        buff[offset++] = (byte) (value & 0xFF);
        buff[offset++] = (byte) ((value >>  8) & 0xFF);
        buff[offset++] = (byte) ((value >> 16) & 0xFF);
        buff[offset  ] = (byte) ((value >> 24) & 0xFF);
    }

    // put a 2-bytes int to the buffer from the specified offset
    public static void putInt2(final byte[] buff, int offset, int value) {
        buff[offset++] = (byte) (value & 0xFF);
        buff[offset  ] = (byte) ((value >>  8) & 0xFF);
    }

    // get an uint32 from a byte array from the specified offset
    public static long getUint32(final byte[] buff, int offset) {
        return (
            ((buff[offset++] & 0x000000FFL)) |
            ((buff[offset++] <<  8) & 0x0000FF00L) |
            ((buff[offset++] << 16) & 0x00FF0000L) |
            ((buff[offset  ] << 24) & 0xFF000000L)
        );
    }

    // get an 2 bytes int from a byte array from the specified offset
    public static int getInt2(final byte[] buff, int offset) {
        return (
            ((buff[offset++]) & 0x000000FF) |
            ((buff[offset  ] << 8) & 0x0000FF00)
        );
    }

}
