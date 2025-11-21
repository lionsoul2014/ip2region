// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package org.lionsoul.ip2region;

import java.util.LinkedList;
import java.util.Queue;

import org.lionsoul.ip2region.xdb.Searcher;

/**
 * ip2region searcher pool manager to provider Concurrency search support.
 * 
 * @author Leon<chenxin619315@gmail.com>
 * Date 2025/11/21
*/
public class SearcherPool {
    // config instance
    private final Config config;

    // searcher pool
    private final Queue<Searcher> pool;

    public SearcherPool(Config config) {
        this.config = config;
        this.pool = new LinkedList<>();
    }

    public Searcher getSearcher() {
        return null;
    }
}