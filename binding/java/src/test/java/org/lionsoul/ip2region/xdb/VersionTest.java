package org.lionsoul.ip2region.xdb;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class VersionTest {

    private static final Log log = Log.getLogger(VersionTest.class).setLevel(Log.DEBUG);

    @Test
    public void testFromName() throws Exception {
        final String[] vers = new String[]{"IPv4", "IPv6"};
        final Version v4 = Version.fromName(vers[0]);
        assertEquals(v4.name, vers[0]);
        final Version v6 = Version.fromName(vers[1]);
        assertEquals(v6.name, vers[1]);

        log.debugf("v4: %s", v4);
        log.debugf("v6: %s", v6);
    }

    @Test
    public void testFromHeader() throws XdbException {
        // create the header buffer
        final byte[] buff1 = new byte[Searcher.HeaderInfoLength];
        // structure version
        LittleEndian.put(buff1, 0, 2, 2);
        LittleEndian.put(buff1, 16, Version.IPv4VersionNo, 2);

        final byte[] buff2 = new byte[Searcher.HeaderInfoLength];
        LittleEndian.put(buff2, 0, 3, 2);
        LittleEndian.put(buff2, 16, Version.IPv6VersionNo, 2);

        final Version ver1 = Version.fromHeader(new Header(buff1));
        final Version ver2 = Version.fromHeader(new Header(buff2));
        log.debugf("ver1: %s", ver1.toString());
        log.debugf("ver2: %s", ver2.toString());
    }

}
