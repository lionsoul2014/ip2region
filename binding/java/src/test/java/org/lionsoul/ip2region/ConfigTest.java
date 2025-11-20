package org.lionsoul.ip2region;
import java.io.IOException;
import java.security.CodeSource;

import org.junit.Test;
import org.lionsoul.ip2region.xdb.Log;
import org.lionsoul.ip2region.xdb.XdbException;

public class ConfigTest {

    private static final Log log = Log.getLogger(ConfigTest.class).setLevel(Log.DEBUG);

    public String getDataPath(String xdbFile) {
        final CodeSource cs = this.getClass().getProtectionDomain().getCodeSource();
        if (cs != null) {
            // log.debugf("code path: %s", cs.getLocation().getPath().concat("../../../../data/"));
            return cs.getLocation().getPath().concat("../../../../data/").concat(xdbFile);
        } else {
            return "../../../../data/".concat(xdbFile);
        }
    }

    @Test
    public void testConfig() throws IOException, XdbException {
        final Config config = new Config(Config.VIndexCache, getDataPath("ip2region_v4.xdb"), 5, 10);
        log.debugf("config: %s", config);
    }

    @Test
    public void testBuildConfig() throws IOException, XdbException {
        final Config config = Config.custom()
            .setCachePolicy(Config.BufferCache)
            .setXdbPath(getDataPath("ip2region_v6.xdb"))
            .setMinSearchers(10)
            .setMaxSearchers(30)
            .build();
        log.debugf("builded config: %s", config);
    }
}
