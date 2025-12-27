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
            log.debugf("%s(v=%s) => %s", ip, Util.ipJoin(ipBytes), Util.ipToString(ipBytes));
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

    @Test
    public void testIpSubCompare() throws InetAddressException {
        final String[][] ipPairs = new String[][] {
            {"1.0.0.0", "1.0.0.1"},
            {"192.168.1.100", "192.168.2.100"},
            {"10.100.1.10", "11.100.2.10"},
            {"11.100.1.10", "10.100.2.10"}
        };

        for (final String[] ips : ipPairs) {
            final byte[] ip1 = Util.parseIP(ips[0]);
            final byte[] ip2 = Util.parseIP(ips[1]);
            log.debugf("ipSubCompare(%s, %s): %d", Util.ipToString(ip1), Util.ipToString(ip2), Util.ipSubCompare(ip1, ip2, 0));
        }
    }

}
