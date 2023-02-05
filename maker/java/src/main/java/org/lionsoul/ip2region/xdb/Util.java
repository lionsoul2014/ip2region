// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
//
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/07/14

package org.lionsoul.ip2region.xdb;

public class Util
{
    // write specified bytes into a byte array start from offset
    public static void write( byte[] b, int offset, long v, int bytes) {
        for ( int i = 0; i < bytes; i++ ) {
            b[offset++] = (byte)((v >>> (8 * i)) & 0xFF);
        }
    }

    // write a int to a byte array
    public static void writeIntLong(byte[] b, int offset, long v) {
        b[offset++] = (byte)((v      ) & 0xFF);
        b[offset++] = (byte)((v >>  8) & 0xFF);
        b[offset++] = (byte)((v >> 16) & 0xFF);
        b[offset  ] = (byte)((v >> 24) & 0xFF);
    }

    // get an int from a byte array start from the specified offset
    public static long getIntLong(byte[] b, int offset) {
        return (
            ((b[offset++] & 0x000000FFL)) |
            ((b[offset++] <<  8) & 0x0000FF00L) |
            ((b[offset++] << 16) & 0x00FF0000L) |
            ((b[offset  ] << 24) & 0xFF000000L)
        );
    }

    public static int getInt2(byte[] b, int offset) {
        return (
            ((b[offset++]) & 0x000000FF) |
            ((b[offset  ] << 8) & 0x0000FF00)
        );
    }

    /* long int to ip string */
    public static String long2ip( long ip ) {
        return String.valueOf((ip >> 24) & 0xFF) + '.' +
                ((ip >> 16) & 0xFF) + '.' + ((ip >> 8) & 0xFF) + '.' + ((ip) & 0xFF);
    }

    public static final byte[] shiftIndex = {24, 16, 8, 0};

    /* check the specified ip address */
    public static long checkIP(String ip) throws Exception {
        final String[] ps = ip.split("\\.");
        if (ps.length != 4) {
            throw new Exception("invalid ip address `" + ip + "`");
        }

        long ipDst = 0;
        for (int i = 0; i < ps.length; i++) {
            int val = Integer.parseInt(ps[i]);
            if (val > 255) {
                throw new Exception("ip part `"+ps[i]+"` should be less then 256");
            }

            ipDst |= ((long) val << shiftIndex[i]);
        }

        return ipDst & 0xFFFFFFFFL;
    }

}
