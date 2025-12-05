// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package service

import (
	"fmt"
	"sync/atomic"
	"time"

	"github.com/lionsoul2014/ip2region/binding/golang/xdb"
)

// ---
// ip2region searcher pool
//
// @Author Lion <chenxin619315@gmail.com>
// @Date   2025/12/03

type SearcherPool struct {
	// config
	config *Config

	// searcher pool
	pool chan *xdb.Searcher

	// for pool close
	closing chan struct{}

	// searcher number that was loaned out
	loanCount int32
}

func NewSearcherPool(config *Config) (*SearcherPool, error) {
	if config.searchers < 1 {
		return nil, fmt.Errorf("config.searchers must > 0")
	}

	pool := make(chan *xdb.Searcher, config.searchers+1)
	// check and create all the searchers
	for i := 0; i < config.searchers; i++ {
		searcher, err := xdb.NewSearcher(config.ipVersion, config.xdbPath, config.vIndex, config.cBuffer)
		if err != nil {
			return nil, fmt.Errorf("failed to create the %dth searcher: %w", i+1, err)
		}

		// push the search to the pool
		pool <- searcher
	}

	return &SearcherPool{
		config: config,
		pool:   pool,

		closing:   make(chan struct{}, 1),
		loanCount: 0,
	}, nil
}

// get the loaned count
func (sp *SearcherPool) LoanCount() int {
	return int(atomic.LoadInt32(&sp.loanCount))
}

func (sp *SearcherPool) BorrowSearcher() *xdb.Searcher {
	// @Note: still accept searcher borrow while closing
	s := <-sp.pool
	atomic.AddInt32(&sp.loanCount, 1)
	return s
}

func (sp *SearcherPool) ReturnSearcher(searcher *xdb.Searcher) {
	select {
	case <-sp.closing:
		// manually close the searcher
		searcher.Close()

		// decrease the loan count
		atomic.AddInt32(&sp.loanCount, -1)
	default:
		// return the searcher
		sp.pool <- searcher
		atomic.AddInt32(&sp.loanCount, -1)
	}
}

func (sp *SearcherPool) Close() {
	sp.CloseTimeout(time.Second * 10)
}

func (sp *SearcherPool) CloseTimeout(d time.Duration) {
	close(sp.closing)
	for {
		timeout := false
		select {
		case s := <-sp.pool:
			s.Close()
		case <-time.After(d):
			// check if all the loaned searchers was closed
			timeout = true
		}

		lc, left := sp.LoanCount(), len(sp.pool)
		if left == 0 && lc == 0 {
			break
		}

		if timeout {
			break
		}
	}
}
