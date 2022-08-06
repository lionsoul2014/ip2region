package org.lionsoul.ip2region.xdb;

/**
 * Internal Util
 */
public class InternalUtil {
    private static final byte[] SHIFT_INDEX = {24, 16, 8, 0};

    /* get an int from a byte array start from the specified offset */
    protected static long getIntLong(byte[] b, int offset) {
        return (
                ((b[offset++] & 0x000000FFL)) |
                        ((b[offset++] << 8) & 0x0000FF00L) |
                        ((b[offset++] << 16) & 0x00FF0000L) |
                        ((b[offset] << 24) & 0xFF000000L)
        );
    }

    protected static int getInt(byte[] b, int offset) {
        return (
                ((b[offset++] & 0x000000FF)) |
                        ((b[offset++] << 8) & 0x0000FF00) |
                        ((b[offset++] << 16) & 0x00FF0000) |
                        ((b[offset] << 24) & 0xFF000000)
        );
    }

    protected static int getInt2(byte[] b, int offset) {
        return (
                (b[offset++] & 0x000000FF) |
                        (b[offset] & 0x0000FF00)
        );
    }

    /* long int to ip string */
    public static String long2ip(long ip) {
        return String.valueOf((ip >> 24) & 0xFF) + '.' +
                ((ip >> 16) & 0xFF) + '.' + ((ip >> 8) & 0xFF) + '.' + ((ip) & 0xFF);
    }

    /* check the specified ip address */
    public static long ip2long(String ip) {
        String[] ps = ip.split("\\.");
        if (ps.length != 4) {
            throw new SearcherException("invalid ip address `" + ip + "`");
        }

        long ipDst = 0;
        for (int i = 0; i < ps.length; i++) {
            int val = Integer.parseInt(ps[i]);
            if (val > 255) {
                throw new SearcherException("ip part `" + ps[i] + "` should be less then 256");
            }
            ipDst |= ((long) val << SHIFT_INDEX[i]);
        }
        return ipDst & 0xFFFFFFFFL;
    }
}
