// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
package org.lionsoul.ip2region;

import java.io.IOException;

import org.lionsoul.ip2region.xdb.Header;
import org.lionsoul.ip2region.xdb.Searcher;
import org.lionsoul.ip2region.xdb.Version;
import org.lionsoul.ip2region.xdb.XdbException;

/**
 * ip2region config class
 * @Author Lion <chenxin619315@gmail.com>
 * @Date   2025/11/20
*/
public class Config {
    // cache policy consts
    public static final int NoCache = 0;
    public static final int VIndexCache = 1;
    public static final int BufferCache = 2;

    // search cache policy
    public final int cachePolicy;

    // xdb file path
    public final String xdbPath;
    public final Header header;
    public final Version ipVersion;

    // searcher pool limitation
    public final int minSearchers;
    public final int maxSearchers;

    // config builder
    public static ConfigBuilder custom() {
        return new ConfigBuilder();
    }

    public Config(int cachePolicy, Version ipVersion, String xdbPath) throws IOException, XdbException {
        this(cachePolicy, ipVersion, xdbPath, 10, 50);
    }

    public Config(int cachePolicy, Version ipVersion, String xdbPath, int minSearchers, int maxSearchers) throws IOException, XdbException {
        this.cachePolicy = cachePolicy;
        this.xdbPath = xdbPath;

        // load the header from the xdb path
        final Header header = Searcher.loadHeaderFromFile(xdbPath);
        this.header = header;
        final Version xVersion = Version.fromHeader(header);

        // this.ipVersion = Version.fromHeader(header);
        this.ipVersion = ipVersion;

        // verify the ip version (ipVersion and the version of the xdb file should be the same)
        if (header.ipVersion != ipVersion.id) {
            throw new XdbException("ip verison not match: xdb file " + xdbPath + " (" + xVersion.name + "), as " + ipVersion.name + " expected");
        }

        this.minSearchers = minSearchers;
        this.maxSearchers = maxSearchers;
    }

    @Override public String toString() {
        final StringBuffer sb = new StringBuffer();
        sb.append('{');
        sb.append("cache_policy:").append(cachePolicy).append(',');
        sb.append("xdb_path:").append(xdbPath).append(',');
        sb.append("header:").append(header.toString()).append(',');
        sb.append("version:").append(ipVersion.toString()).append(',');
        sb.append("min_searchers:").append(minSearchers).append(',');
        sb.append("max_searchers:").append(maxSearchers);
        sb.append('}');
        return sb.toString();
    }
}