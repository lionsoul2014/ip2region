// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package org.lionsoul.ip2region.service;

import java.io.IOException;

import org.lionsoul.ip2region.xdb.InetAddressException;
import org.lionsoul.ip2region.xdb.Searcher;
import org.lionsoul.ip2region.xdb.Util;
import org.lionsoul.ip2region.xdb.XdbException;

/**
 * ip2region searcher manager service to provider:
 * 1. Unified query interface for IPv4 and IPv6 address.
 * 2. Concurrency search support.
 * 
 * @Author Lion <chenxin619315@gmail.com>
 * Date    2025/11/21
*/
public class Ip2Region {

    /* v4 pool for cache policy vIndex or NoCache */
    private final SearcherPool v4Pool;

    /* v4 xdb searcher for cache policy cBuffer */
    private final Searcher v4InMemSearcher;

    /* v6 pool for cache policy vIndex or NoCache*/
    private final SearcherPool v6Pool;

    /* v6 xdb searcher for cache policy cBuffer */
    private final Searcher v6InMemSearcher;

    public static final Ip2Region create(final Config v4Config, final Config v6Config) throws IOException {
        return new Ip2Region(v4Config, v6Config).init();
    }

    public static final Ip2Region create(final String v4XdbPath, final String v6XdbPath) throws IOException, XdbException {
        return new Ip2Region(v4XdbPath, v6XdbPath).init();
    }

    /**
     * init the ip2reigon with two xdb file path and default cachePolicy vIndex.
     * set it to null to disabled the search for specified version
     * 
     * @param v4XdbPath
     * @param v6XdbPath
     * @throws XdbException 
     * @throws IOException 
    */
    protected Ip2Region(String v4XdbPath, String v6XdbPath) throws IOException, XdbException {
        this(
            v4XdbPath == null ? null : Config.custom().setXdbPath(v4XdbPath).asV4(), 
            v6XdbPath == null ? null : Config.custom().setXdbPath(v6XdbPath).asV6()
        );
    }

    /**
     * init the ip2region with specified config.
     * set it to null for disabled the search for specified version
     * 
     * @param v4Config
     * @param v6Config
     * @throws IOException
    */ 
    protected Ip2Region(Config v4Config, Config v6Config) throws IOException {
        if (v4Config == null) {
            // @Note: with IPv4 disabled ?
            this.v4InMemSearcher = null;
            this.v4Pool = null;
        } else if (v4Config.cachePolicy == Config.BufferCache) {
            this.v4InMemSearcher = Searcher.newWithBuffer(v4Config.ipVersion, v4Config.cBuffer);
            this.v4Pool = null;
        } else {
            this.v4InMemSearcher = null;
            this.v4Pool = new SearcherPool(v4Config);
        }

        if (v6Config == null) {
            // @Note: with IPv6 disabled ?
            this.v6InMemSearcher = null;
            this.v6Pool = null;
        } else if (v6Config.cachePolicy == Config.BufferCache) {
            this.v6InMemSearcher = Searcher.newWithBuffer(v6Config.ipVersion, v6Config.cBuffer);
            this.v6Pool = null;
        } else {
            this.v6InMemSearcher = null;
            this.v6Pool = new SearcherPool(v6Config);
        }
    }

    // init the current ip2region service
    protected Ip2Region init() throws IOException {
        if (v4Pool != null) {
            v4Pool.init();
        }

        if (v6Pool != null) {
            v6Pool.init();
        }

        return this;
    }

    public String search(String ipString) throws InetAddressException, IOException, InterruptedException {
        return search(Util.parseIP(ipString));
    }

    public String search(byte[] ipBytes) throws InetAddressException, IOException, InterruptedException {
        if (ipBytes.length == 4) {
            return v4Search(ipBytes);
        } else if (ipBytes.length == 16) {
            return v6Search(ipBytes);
        } else {
            throw new InetAddressException("invalid byte ip address with length=" + ipBytes.length);
        }
    }

    protected String v4Search(final byte[] ipBytes) throws IOException, InetAddressException, InterruptedException {
        if (v4InMemSearcher != null) {
            return v4InMemSearcher.search(ipBytes);
        }

        // IPv4 search is disabled
        if (v4Pool == null) {
            return null;
        }

        final Searcher searcher = v4Pool.borrowSearcher();
        try {
            return searcher.search(ipBytes);
        } finally {
            v4Pool.returnSearcher(searcher);
        }
    }

    protected String v6Search(final byte[] ipBytes) throws IOException, InetAddressException, InterruptedException {
        if (v6InMemSearcher != null) {
            return v6InMemSearcher.search(ipBytes);
        }

        // IPv6 search is disabled
        if (v6Pool == null) {
            return null;
        }

        final Searcher searcher = v6Pool.borrowSearcher();
        try {
            return searcher.search(ipBytes);
        } finally {
            v6Pool.returnSearcher(searcher);
        }
    }

    public void close() throws InterruptedException {
        close(10000);
    }

    public void close(long timeoutMillis) throws InterruptedException {
        if (v4Pool != null) {
            v4Pool.close(timeoutMillis);
        }

        if (v6Pool != null) {
            v6Pool.close(timeoutMillis);
        }
    }

}