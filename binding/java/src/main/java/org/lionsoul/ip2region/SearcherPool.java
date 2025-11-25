// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package org.lionsoul.ip2region;

import java.io.IOException;
import java.util.LinkedList;
import java.util.Queue;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.ReentrantLock;

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

    // searcher lock
    private final ReentrantLock lock;

    // searcher numbers that was loaned out
    private final AtomicInteger loanCount;

    public SearcherPool(Config config) {
        this(config, false);
    }

    public SearcherPool(Config config, boolean fair) {
        this.config = config;
        this.pool = new LinkedList<>();
        this.lock = new ReentrantLock(fair);
        this.loanCount = new AtomicInteger(0);
    }

    // init the searcher pool 
    public void init() throws IOException {
        for (int i = 0; i < config.searchers; i++) {
            final Searcher searcher = new Searcher(config.ipVersion, config.xdbPath, config.vIndex, config.cBuffer);
            pool.add(searcher);
        }
    }

    public Searcher getSearcher() {
        return null;
    }

    // close the searcher pool
    public void close() {
        this.loanCount.set(config.searchers);
    }
}