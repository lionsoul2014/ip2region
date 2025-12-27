// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package org.lionsoul.ip2region.service;

import java.io.IOException;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Queue;
import java.util.concurrent.locks.Condition;
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
    public final Config config;

    // searcher pool
    private final Queue<Searcher> pool;

    // lock & conditions
    private final ReentrantLock lock;
    private final Condition emptyCondition;
    private final Condition fullCondition;

    // searcher numbers that was loaned out
    private int loanCount;

    // static method to create and init the searcher pool
    public static final SearcherPool create(final Config config) throws IOException {
        return new SearcherPool(config).init();
    }

    public static final SearcherPool create(final Config config, boolean fair) throws IOException {
        return new SearcherPool(config, fair).init();
    }

    protected SearcherPool(Config config) throws IOException {
        this(config, false);
    }

    protected SearcherPool(Config config, boolean fair) {
        assert config.searchers > 0;
        this.config = config;
        this.pool = new LinkedList<>();
        this.lock = new ReentrantLock(fair);
        this.emptyCondition = this.lock.newCondition();
        this.fullCondition = this.lock.newCondition();
        this.loanCount = 0;
    }

    protected SearcherPool init() throws IOException {
        // create the searchers
        for (int i = pool.size(); i < config.searchers; i++) {
            final Searcher searcher = new Searcher(config.ipVersion, config.xdbFile, config.vIndex, config.cBuffer);
            pool.add(searcher);
        }

        return this;
    }

    public int getLoanCount() {
        lock.lock();
        int lc = this.loanCount;
        lock.unlock();
        return lc;
    }

    public Searcher borrowSearcher() throws InterruptedException {
        lock.lock();
        try {
            while (pool.isEmpty()) {
                emptyCondition.await();
            }

            loanCount++;
            return pool.poll();
        } finally {
            lock.unlock();
        }
    }

    public void returnSearcher(final Searcher searcher) {
        lock.lock();
        try {
            pool.add(searcher);
            loanCount--;
            emptyCondition.signal();

            // check and signal the full condition.
            // pool close
            if (loanCount == 0) {
                fullCondition.signal();
            }
        } finally {
            lock.unlock();
        }
    }

    // close the searcher pool
    public void close() throws InterruptedException {
        close(10000);
    }

    public void close(long timeoutMillis) throws InterruptedException {
        lock.lock();
        try {
            while (loanCount > 0) {
                fullCondition.wait(timeoutMillis);
            }

            final Iterator<Searcher> it = pool.iterator();
            while (it.hasNext()) {
                final Searcher searcher = it.next();
                try {searcher.close();} catch (IOException e) {}
                it.remove();
            }
        } finally {
            lock.unlock();
        }
    }
}