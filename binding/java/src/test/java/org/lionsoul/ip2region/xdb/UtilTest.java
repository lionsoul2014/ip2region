package org.lionsoul.ip2region.xdb;

import org.junit.Test;

public class UtilTest {

    private static final Log log = Log.getLogger(UtilTest.class).setLevel(Log.DEBUG);

    @Test
    public void testCheckIP() throws InetAddressException {
        final String[] ips = new String[]{
            "192.168.1.102",
            "219.133.111.87",
            "::",
            "3000::",
            "::1001:ffff",
            "2001:2:0:ffff:ffff:ffff:ffff:ffff",
            "::ffff:114.114.114.114"
        };

        for (String ip : ips) {
            final byte[] ipBytes = Util.parseIP(ip);
            log.debugf("%s(v=%s) => %s", ip, Util.ipArrayString(ipBytes), Util.ipToString(ipBytes));
        }
    }

    @Test
    public void testIpCompare() throws InetAddressException {
        final String[][] ipPairs = new String[][]{
            {"1.0.0.0", "1.0.0.1"},
            {"192.168.1.101", "192.168.1.90"},
            {"219.133.111.87", "114.114.114.114"},
            {"2000::", "2000:ffff:ffff:ffff:ffff:ffff:ffff:ffff"},
            {"2001:4:112::", "2001:4:112:ffff:ffff:ffff:ffff:ffff"},
            {"ffff::", "2001:4:ffff:ffff:ffff:ffff:ffff:ffff"}
        };

        for (String[] ips : ipPairs) {
            final byte[] ip1 = Util.parseIP(ips[0]);
            final byte[] ip2 = Util.parseIP(ips[1]);
            log.debugf("compare(%s, %s): %d", ips[0], ips[1], Util.ipCompare(ip1, ip2));
        }
    }

}
