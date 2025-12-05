// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package org.lionsoul.ip2region.service;

import java.io.IOException;
import java.io.RandomAccessFile;

import org.lionsoul.ip2region.xdb.Header;
import org.lionsoul.ip2region.xdb.LongByteArray;
import org.lionsoul.ip2region.xdb.Searcher;
import org.lionsoul.ip2region.xdb.Version;
import org.lionsoul.ip2region.xdb.XdbException;

/**
 * ip2region config builder
 * @Author Lion <chenxin619315@gmail.com>
 * @Date   2025/11/20
*/
public class ConfigBuilder {

    // cache policy
    private int cachePolicy = Config.VIndexCache;

    // xdb file path
    private String xdbPath = null;

    // searchers
    private int searchers = 20;

    public ConfigBuilder() {}

    public ConfigBuilder(String xdbPath) {
        this.xdbPath = xdbPath;
    }

    public ConfigBuilder setCachePolicy(int cachePolicy) {
        this.cachePolicy = cachePolicy;
        return this;
    }

    public ConfigBuilder setXdbPath(String xdbPath) {
        this.xdbPath = xdbPath;
        return this;
    }

    public ConfigBuilder setSeachers(int searchers) {
        this.searchers = searchers;
        return this;
    }

    private Config build(Version ipVersion) throws IOException, XdbException {
        // load the header and the cache buffer
        final RandomAccessFile raf = new RandomAccessFile(xdbPath, "r");

        // 1, verify the xdb
        Searcher.verify(raf);

        // 2, load the header
        final Header header = Searcher.loadHeader(raf);

        // 3, check and load the vector index buffer
        final byte[] vIndex = cachePolicy == Config.VIndexCache ? Searcher.loadVectorIndex(raf) : null;

        // 4, check and load the content buffer
        final LongByteArray cBuffer = cachePolicy == Config.BufferCache ? Searcher.loadContent(raf) : null;

        raf.close();
        return new Config(cachePolicy, ipVersion, xdbPath, header, vIndex, cBuffer, searchers);
    }

    // build the final #Config instance for IPv4
    public Config asV4() throws IOException, XdbException {
        return build(Version.IPv4);
    }

    // build the final #Config instance for IPv6
    public Config asV6() throws IOException, XdbException {
        return build(Version.IPv6);
    }

}