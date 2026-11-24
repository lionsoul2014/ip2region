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
    public static byte[] parseIP(String ip) throws InetAddressException {
        // InetAddress.getByName resolves anything that is not an address literal through DNS
        if (!isAddressLiteral(ip)) {
            throw new InetAddressException("invalid ip address `"+ip+"`");
        }

        try {
            return InetAddress.getByName(ip).getAddress();
        } catch (UnknownHostException e) {
            throw new InetAddressException("invalid ip address `"+ip+"`");
        }
    }

    // check for a dotted IPv4 address or a string getByName parses as an IPv6 literal
    private static boolean isAddressLiteral(String ip) {
        if (ip == null || ip.isEmpty()) {
            return false;
        }

        if (ip.indexOf(':') > -1) {
            final char c = ip.charAt(0);
            return c == ':' || c == '[' || Character.digit(c, 16) != -1;
        }

        final String[] parts = ip.split("\\.", -1);
        if (parts.length != 4) {
            return false;
        }

        for (String p : parts) {
            if (p.isEmpty() || p.length() > 3) {
                return false;
            }
            for (int i = 0; i < p.length(); i++) {
                if (p.charAt(i) < '0' || p.charAt(i) > '9') {
                    return false;
                }
            }
            if (Integer.parseInt(p) > 255) {
                return false;
            }
        }

        return true;
    }

    // convert the byte[] ip to string ip address
    public static String ipToString(final byte[] ip) {
        if (ip.length != 4 && ip.length != 16) {
            return String.format("invalid-ip-address-length: %d", ip.length);
        }

        try {
            return InetAddress.getByAddress(ip).getHostAddress();
        } catch (UnknownHostException e) {
            return String.format("invalid-ip-address `%s`", ipJoin(ip));
        }
    }

    // implode the byte[] ip with its byte value.
    public static String ipJoin(byte[] ip) {
        return bytesToString(ip, 0, ip.length);
    }

    public static String bytesToString(byte[] buff, int offset, int length) {
        final StringBuffer sb = new StringBuffer();
        sb.append("[");
        for (int i = 0; i < length; i++) {
            if (i > 0) {
                sb.append(',');
            }
            sb.append((buff[offset+i] & 0xFF));
        }
        sb.append("]");
        return sb.toString();
    }

    // compare two byte ip
    // Returns: -1 if ip1 < ip2, 0 if ip1 == ip2, 1 if ip1 > ip2
    public static int ipCompare(byte[] ip1, byte[] ip2) {
        return ipSubCompare(ip1, ip2, 0);
    }

    // compare the ip with the ip in the buffer start from offset
    // Returns: -1 if ip < buff[offset], 0 if ip == buff[offset], 1 if ip > buff[offset]
    public static int ipSubCompare(byte[] ip, byte[] buff, int offset) {
        for (int i = 0; i < ip.length; i++) {
            // covert the byte to int to sure the uint8 attribute
            final int i1 = ip[i] & 0xFF;
            final int i2 = buff[offset+i] & 0xFF;
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
            final int v = r[i] & 0xFF;
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
            final int v = r[i] & 0xFF;
            if (v > 0) {    // No borrow needed
                r[i]--;
                break;
            }

            r[i] = (byte) 0xFF; // borrow from the next byte
        }

        return r;
    }

}
