package org.lionsoul.ip2region.service;

import org.junit.Test;
import org.lionsoul.ip2region.xdb.Log;
import org.lionsoul.ip2region.xdb.Searcher;

public class SearcherPoolTest {

    private static final Log log = Log.getLogger(SearcherPoolTest.class).setLevel(Log.DEBUG);

    @Test
    public void testV4SearcherPool() throws Exception {
        final Config v4Config = Config.custom()
            .setCachePolicy(Config.VIndexCache)
            .setSearchers(5)
            .setXdbPath(ConfigTest.getDataPath("ip2region_v4.xdb"))
            .asV4();


        final String ipStr = "58.250.36.41";
        final SearcherPool v4Pool = SearcherPool.create(v4Config);
        for (int i = 0; i < 20; i++) {
            final Searcher searcher = v4Pool.borrowSearcher();
            log.debugf("borrowed searcher %d: %s", i, searcher.toString());
            final String region = searcher.search(ipStr);
            log.debugf("search(%s)=%s", ipStr, region);
            v4Pool.returnSearcher(searcher);
            log.debugf("return searcher %d", i);
        }

        v4Pool.close();
        log.debugf("v4 searcher pool closed gracefully");
    }

    @Test
    public void testV6SearcherPool() throws Exception {
        final Config v6Config = Config.custom()
            .setCachePolicy(Config.VIndexCache)
            .setSearchers(5)
            .setXdbPath(ConfigTest.getDataPath("ip2region_v6.xdb"))
            .asV6();


        final String ipStr = "240e:3b7:3272:d8d0:db09:c067:8d59:539e";
        final SearcherPool v4Pool = SearcherPool.create(v6Config);
        for (int i = 0; i < 20; i++) {
            final Searcher searcher = v4Pool.borrowSearcher();
            log.debugf("borrowed searcher %d: %s", i, searcher.toString());
            final String region = searcher.search(ipStr);
            log.debugf("search(%s)=%s", ipStr, region);
            v4Pool.returnSearcher(searcher);
            log.debugf("return searcher %d", i);
        }

        v4Pool.close();
        log.debugf("v6 searcher pool closed gracefully");
    }
}
