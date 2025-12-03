package org.lionsoul.ip2region;
import java.io.IOException;
import java.security.CodeSource;

import org.junit.Test;
import org.lionsoul.ip2region.xdb.Log;
import org.lionsoul.ip2region.xdb.XdbException;

public class ConfigTest {

    private static final Log log = Log.getLogger(ConfigTest.class).setLevel(Log.DEBUG);

    public static final String getDataPath(String xdbFile) {
        final CodeSource cs = ConfigTest.class.getProtectionDomain().getCodeSource();
        if (cs != null) {
            // log.debugf("code path: %s", cs.getLocation().getPath().concat("../../../../data/"));
            return cs.getLocation().getPath().concat("../../../../data/").concat(xdbFile);
        } else {
            return "../../../../data/".concat(xdbFile);
        }
    }

    @Test
    public void testBuildV4Config() throws IOException, XdbException {
        final Config v4Config = Config.custom()
            .setCachePolicy(Config.BufferCache)
            .setXdbPath(getDataPath("ip2region_v4.xdb"))
            .setSearchers(20)
            .asV4();
        log.debugf("builded config: %s", v4Config);
    }

    @Test
    public void testBuildV6Config() throws IOException, XdbException {
        final Config v4Config = Config.custom()
            .setCachePolicy(Config.VIndexCache)
            .setXdbPath(getDataPath("ip2region_v6.xdb"))
            .setSearchers(20)
            .asV6();
        log.debugf("builded config: %s", v4Config);
    }
}
