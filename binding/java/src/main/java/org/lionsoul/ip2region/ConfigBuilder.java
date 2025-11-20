// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package org.lionsoul.ip2region;

import java.io.IOException;

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

    // min searchers
    private int minSearchers = 10;

    // max searchers
    private int maxSearchers = 30;

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

    public ConfigBuilder setMinSearchers(int minSearchers) {
        this.minSearchers = minSearchers;
        return this;
    }

    public ConfigBuilder setMaxSearchers(int maxSearchers) {
        this.maxSearchers = maxSearchers;
        return this;
    }

    // build the final #Config instance with the current config items
    public Config build() throws IOException, XdbException {
        return new Config(cachePolicy, xdbPath, minSearchers, maxSearchers);
    }
}