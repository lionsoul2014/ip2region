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
    // returns: byte[4] for IPv4 and byte[16] for IPv6 and the bytes should be in Big endian order.
    public static byte[] parseIP(String ip) throws InvalidInetAddressException {
        try {
            return InetAddress.getByName(ip).getAddress();
        } catch (UnknownHostException e) {
            throw new InvalidInetAddressException("invalid ip address `"+ip+"`");
        }
    }

    // convert the byte[] ip to string ip address
    public static String ipToString(final byte[] ip) {
        if (ip.length != 4 && ip.length != 16) {
            return String.format("invalid-ip-address-length: %d", ip.length);
        }

        try {
            return InetAddress.getByAddress(ip).getHostAddress();
        } catch (UnknownHostException e) {
            return String.format("invalid-ip-address `%s`", ipArrayString(ip));
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

    // region filtering
    public static String regionFiltering(String region, int[] fields) {
        if (fields.length == 0) {
            return region;
        }

        final String[] fs = region.split("\\|", -1);
        final StringBuilder sb = new StringBuilder();
        final int tailing = fields.length - 1;
        for (int i = 0; i < fields.length; i++) {
            final int idx = fields[i];
            if (idx >= fs.length) {
                throw new IllegalArgumentException("field index `"
                        + idx + "` exceeded the max length `" + fs.length + "`");
            }

            sb.append(fs[idx]);
            if (i < tailing) {
                sb.append("|");
            }
        }

        return sb.toString();
    }

}
