package org.lionsoul.ip2region.xdb;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class VersionTest {
    @Test
    public void testFromName() throws Exception {
        final String[] vers = new String[]{"IPv4", "IPv6"};
        final Version v4 = Version.fromName(vers[0]);
        assertEquals(v4.name, vers[0]);
        final Version v6 = Version.fromName(vers[1]);
        assertEquals(v6.name, vers[1]);

        System.out.printf("v4: %s\n", v4);
        System.out.printf("v6: %s\n", v6);
    }
}
