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
}
