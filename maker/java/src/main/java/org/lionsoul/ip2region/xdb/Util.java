// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
//
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/07/14

package org.lionsoul.ip2region.xdb;

import java.net.InetAddress;
import java.net.UnknownHostException;

public class Util
{

    // parse the specified IP address and return its bytes.
    // returns: byte[4] for IPv6 and byte[16] for IPv6 and the bytes should be in Big endian order.
    public static byte[] parseIP(String ip) throws InvalidInetAddressException {
        try {
            return InetAddress.getByName(ip).getAddress();
        } catch (UnknownHostException e) {
            throw new InvalidInetAddressException("invalid ip address `"+ip+"`");
        }
    }

    // print the ip in bytes
    public static String ipToString(final byte[] ip) throws InvalidInetAddressException {
        if (ip.length != 4 && ip.length != 16) {
            throw new InvalidInetAddressException("invalid ip address length `"+ip.length+"`");
        }

        try {
            return InetAddress.getByAddress(ip).getHostAddress();
        } catch (UnknownHostException e) {
            throw new InvalidInetAddressException("invalid ip address `"+ipArrayString(ip)+"`");
        }
    }

    // implode the byte[] ip with its byte value.
    public static String ipArrayString(byte[] ip) {
        final StringBuffer sb = new StringBuffer();
        sb.append("[");
        for (int i = 0; i < ip.length; i++) {
            if (i > 0) {
                sb.append(',');
            }
            sb.append((ip[i] & 0xFF));
        }
        sb.append("]");
        return sb.toString();
    }

    // compare two byte ip
    // Returns: -1 if ip1 < ip2, 0 if ip1 == ip2, 1 if ip1 > ip2
    public static int ipCompare(byte[] ip1, byte[] ip2) {
        for (int i = 0; i < ip1.length; i++) {
            // covert the byte to int to sure the uint8 attribute
            final int i1 = (int)(ip1[i] & 0xFF);
            final int i2 = (int)(ip2[i] & 0xFF);
            if (i1 < i2) {
                return -1;
            }

            if (i1 > i2) {
                return 1;
            }
        }

        return 0;
    }

    public static byte[] ipAddOne(byte[] ip) {
        final byte[] r = new byte[ip.length];
        System.arraycopy(ip, 0, r, 0, ip.length);
        for (int i = ip.length - 1; i >= 0; i--) {
            final int v = (int)(r[i] & 0xFF);
            if (v < 255) {    // No overflow
                r[i]++;
                break;
            }

            r[i] = 0;
        }

        return r;
    }

    public static byte[] ipSubOne(byte[] ip) {
        final byte[] r = new byte[ip.length];
        System.arraycopy(ip, 0, r, 0, ip.length);
        for (int i = ip.length - 1; i >= 0; i--) {
            final int v = (int)(r[i] & 0xFF);
            if (v > 0) {    // No borrow needed
                r[i]--;
                break;
            }

            r[i] = (byte) 0xFF; // borrow from the next byte
        }

        return r;
    }


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
