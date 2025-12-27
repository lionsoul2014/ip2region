package org.lionsoul.ip2region.xdb;

import org.junit.Test;

public class IPv4Test {

    private static final Log log = Log.getLogger(IPv4Test.class);

    @Test
    public void testIpSubCompare() throws InetAddressException {
        final byte[] sip = Util.parseIP("0.255.255.255");
        final byte[] eip = Util.parseIP("1.0.0.2");
        final byte[] buff = new byte[Version.IPv4.segmentIndexSize];
        Version.IPv4.putBytes(buff, 0, sip);
        Version.IPv4.putBytes(buff, 4, eip);
        log.infof("bytesToString(buff): %s", Util.bytesToString(buff, 0, 8));

        final byte[] ip = Util.parseIP("1.0.0.0");

        // compare the sip
        log.infof("ipSubCompare(%s, %s): %d", 
            Util.ipToString(ip), 
            Util.bytesToString(buff, 0, 4), 
            Util.ipSubCompare(ip, buff, 0)
        );
        log.infof("IPv4.ipSubCompare(%s, %s): %d", 
            Util.ipToString(ip), 
            Util.bytesToString(buff, 0, 4), 
            Version.IPv4.ipSubCompare(ip, buff, 0)
        );

        // compare the eip
        log.infof("ipSubCompare(%s, %s): %d", 
            Util.ipToString(ip), 
            Util.bytesToString(buff, 4, 4), 
            Util.ipSubCompare(ip, buff, 4)
        );
        log.infof("IPv4.ipSubCompare(%s, %s): %d", 
            Util.ipToString(ip), 
            Util.bytesToString(buff, 4, 4), 
            Version.IPv4.ipSubCompare(ip, buff, 4)
        );
    }
}
