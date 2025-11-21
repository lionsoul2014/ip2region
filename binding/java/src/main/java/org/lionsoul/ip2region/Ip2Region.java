// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package org.lionsoul.ip2region;

import java.io.IOException;

import org.lionsoul.ip2region.xdb.InetAddressException;
import org.lionsoul.ip2region.xdb.Searcher;
import org.lionsoul.ip2region.xdb.Util;

/**
 * ip2region searcher manager service to provider:
 * 1. Unified query interface for IPv4 and IPv6 address.
 * 2. Concurrency search support.
 * 
 * @Author Lion <chenxin619315@gmail.com>
 * Date    2025/11/21
*/
public class Ip2Region {

    /* v4 pool */
    private final SearcherPool v4Pool;

    /* v6 pool */
    private final SearcherPool v6Pool;

    public Ip2Region(Config v4Config, Config v6Config) {
        this.v4Pool = new SearcherPool(v4Config);
        this.v6Pool = new SearcherPool(v6Config); 
    }

    public String search(String ipString) throws InetAddressException, IOException {
        return search(Util.parseIP(ipString));
    }

    public String search(byte[] ipBytes) throws InetAddressException, IOException {
        // 1, define the pool with the input ip
        final SearcherPool pool;
        if (ipBytes.length == 4) {
            pool = v4Pool;
        } else if (ipBytes.length == 16) {
            pool = v6Pool;
        } else {
            throw new InetAddressException("invalid byte ip address with length=" + ipBytes.length);
        }

        // 2, get a searcher from the pool
        final Searcher searcher = pool.getSearcher();

        try {
            // 3, do the search
            final String region = searcher.search(ipBytes);
            return region;
        } catch (InetAddressException e) {
            // for the inet address error and we should return the searcher
            throw e;
        } catch (IOException e) {
            // for the IOException usually means something is wrong with the read operation to the xdb file
            // and we choose to keep the searcher and destory it right now
            // so we will create a new one for the next search
            try {searcher.close();} catch (IOException e1) {}
            throw e;
        }
    }

}