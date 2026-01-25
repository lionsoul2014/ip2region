package org.lionsoul.ip2region.service;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Test;
import org.lionsoul.ip2region.xdb.InetAddressException;
import org.lionsoul.ip2region.xdb.Log;
import org.lionsoul.ip2region.xdb.Util;
import org.lionsoul.ip2region.xdb.XdbException;

public class Ip2RegionTest {

    private static final Log log = Log.getLogger(Ip2RegionTest.class).setLevel(Log.DEBUG);

    @Test
    public void TestConfigCreate() throws IOException, XdbException, InetAddressException, InterruptedException, InvalidConfigException {
        final Config v4Config = Config.custom()
            .setCachePolicy(Config.NoCache)
            .setSearchers(10)
            .setXdbPath(ConfigTest.getDataPath("ip2region_v4.xdb"))
            .asV4();

        final Config v6Config = Config.custom()
            .setCachePolicy(Config.VIndexCache)
            .setSearchers(10)
            .setXdbPath(ConfigTest.getDataPath("ip2region_v6.xdb"))
            .asV6();

        byte[] v4Bytes = Util.parseIP("113.92.157.29");
        byte[] v6Bytes = Util.parseIP("240e:3b7:3272:d8d0:db09:c067:8d59:539e");
        final Ip2Region ip2Region = Ip2Region.create(v4Config, v6Config);
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
    public void TestPathCreate() throws InetAddressException, IOException, XdbException, InterruptedException, InvalidConfigException {
        byte[] v4Bytes = Util.parseIP("113.92.157.29");
        byte[] v6Bytes = Util.parseIP("240e:3b7:3272:d8d0:db09:c067:8d59:539e");
        final Ip2Region ip2Region = Ip2Region.create(ConfigTest.getDataPath("ip2region_v4.xdb"), ConfigTest.getDataPath("ip2region_v6.xdb"));
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
    public void TestInMemSearch() throws IOException, XdbException, InetAddressException, InterruptedException, InvalidConfigException {
        final Config v4Config = Config.custom()
            .setCachePolicy(Config.BufferCache)
            .setXdbPath(ConfigTest.getDataPath("ip2region_v4.xdb"))
            .asV4();

        final Config v6Config = Config.custom()
            .setCachePolicy(Config.BufferCache)
            .setXdbPath(ConfigTest.getDataPath("ip2region_v6.xdb"))
            .asV6();

        byte[] v4Bytes = Util.parseIP("113.92.157.29");
        byte[] v6Bytes = Util.parseIP("240e:3b7:3272:d8d0:db09:c067:8d59:539e");
        final Ip2Region ip2Region = Ip2Region.create(v4Config, v6Config);
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
    public void TestConcurrentCall() throws IOException, XdbException, InetAddressException, InterruptedException, InvalidConfigException {
        final Config v4Config = Config.custom()
            .setCachePolicy(Config.VIndexCache)
            .setSearchers(15)
            .setXdbPath(ConfigTest.getDataPath("ip2region_v4.xdb"))
            .asV4();

        final Config v6Config = Config.custom()
            .setCachePolicy(Config.VIndexCache)
            .setSearchers(15)
            .setXdbPath(ConfigTest.getDataPath("ip2region_v6.xdb"))
            .asV6();

        byte[] v4Bytes = Util.parseIP("113.92.157.29");
        byte[] v6Bytes = Util.parseIP("240e:3b7:3272:d8d0:db09:c067:8d59:539e");
        final int threads = 100;
        final Ip2Region ip2Region = Ip2Region.create(v4Config, v6Config);
        final CountDownLatch latch = new CountDownLatch(threads);
        final AtomicInteger count = new AtomicInteger(0);
        final long tStart = System.nanoTime();
        for (int i = 0; i < threads; i++) {
            final Runnable t = new Runnable() {
                @Override
                public void run() {
                    for (int i = 0; i < 5000; i++) {
                        final byte[] ipBytes = i % 2 == 0 ? v4Bytes : v6Bytes;
                        try {
                            final String region = ip2Region.search(ipBytes);
                            if (ipBytes.length == 4) {
                                assertEquals("v4 region not equals", region, "中国|广东省|深圳市|电信|CN");
                            } else {
                                assertEquals("v6 region not equals", region, "中国|广东省|深圳市|电信|CN");
                            }
                        } catch (InetAddressException | IOException | InterruptedException e) {
                            log.errorf("failed to search(%s): %s", Util.ipToString(ipBytes), e.getMessage());
                        }

                        count.incrementAndGet();
                    }

                    latch.countDown();
                }
            };
            t.run();
        }

        latch.await();
        final long costs = System.nanoTime() - tStart;
        log.debugf("%d searches finished in %dms, avg took: %dµs", count.get(), costs / 1000_000, costs / count.get() / 1000);
        ip2Region.close();
        log.debugf("ip2region closed gracefully");
    }

    @Test
    public void TestV4Only() throws IOException, XdbException, InetAddressException, InterruptedException, InvalidConfigException {
        final Config v4Config = Config.custom()
            .setCachePolicy(Config.NoCache)
            .setXdbPath(ConfigTest.getDataPath("ip2region_v4.xdb"))
            .asV4();

        byte[] v4Bytes = Util.parseIP("113.92.157.29");
        byte[] v6Bytes = Util.parseIP("240e:3b7:3272:d8d0:db09:c067:8d59:539e");
        final Ip2Region ip2Region = Ip2Region.create(v4Config, null);
        for (int i = 0; i < 10; i++) {
            v4Bytes = Util.ipAddOne(v4Bytes);
            final String v4Region = ip2Region.search(v4Bytes);
            final String v6Region = ip2Region.search(v6Bytes);
            log.debugf("search(%s)=%s, search(%s)=%s", Util.ipToString(v4Bytes), v4Region, Util.ipToString(v6Bytes), v6Region);
        }

        ip2Region.close();
        log.debugf("ip2region closed gracefully");
    }

}
