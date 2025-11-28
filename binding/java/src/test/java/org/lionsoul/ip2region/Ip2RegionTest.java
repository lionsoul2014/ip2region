package org.lionsoul.ip2region;

import java.io.IOException;

import org.junit.Test;
import org.lionsoul.ip2region.xdb.InetAddressException;
import org.lionsoul.ip2region.xdb.Log;
import org.lionsoul.ip2region.xdb.Util;
import org.lionsoul.ip2region.xdb.XdbException;

public class Ip2RegionTest {

    private static final Log log = Log.getLogger(Ip2RegionTest.class).setLevel(Log.DEBUG);

    @Test
    public void TestConfigCreate() throws IOException, XdbException, InetAddressException, InterruptedException {
        final Config v4Config = Config.custom()
            .setCachePolicy(Config.NoCache)
            .setSeachers(10)
            .setXdbPath(ConfigTest.getDataPath("ip2region_v4.xdb"))
            .asV4();

        final Config v6Config = Config.custom()
            .setCachePolicy(Config.VIndexCache)
            .setSeachers(10)
            .setXdbPath(ConfigTest.getDataPath("ip2region_v6.xdb"))
            .asV6();

        byte[] v4Bytes = Util.parseIP("1.2.3.4");
        byte[] v6Bytes = Util.parseIP("240e:3b7:3272:d8d0:db09:c067:8d59:539e");
        final Ip2Region ip2Region = new Ip2Region(v4Config, v6Config);
        for (int i = 0; i < 50; i++) {
            v4Bytes = Util.ipAddOne(v4Bytes);
            v6Bytes = Util.ipAddOne(v6Bytes);
            final String v4Region = ip2Region.search(v4Bytes);
            final String v6Region = ip2Region.search(v6Bytes);
            log.debugf("search(%s)=%s, search(%s)=%s", Util.ipToString(v4Bytes), v4Region, Util.ipToString(v6Bytes), v6Region);
        }

        ip2Region.close();
        log.debugf("ip2region closed gracefully");
    }

    @Test
    public void TestPathCreate() {

    }

    @Test
    public void TestInMemSearch() {

    }

}
