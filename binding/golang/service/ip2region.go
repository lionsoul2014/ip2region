// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package service

import (
	"fmt"
	"time"

	"github.com/lionsoul2014/ip2region/binding/golang/xdb"
)

// ---
// Ip2Region service
// 1. Unified query interface to IPv4 and IPv6 address.
// 2. Concurrency search support.
//
// @Author Lion <chenxin619315@gmail.com>
// @Date   2025/12/05

type Ip2Region struct {
	// v4 pool for cache policy vIndex or NoCache
	v4Pool *SearcherPool

	// v4 xdb searcher for full in-memeory search
	v4InMemSearcher *xdb.Searcher

	// v6 pool for cache policy vIndex or NoCache:w
	v6Pool *SearcherPool

	// v6 xdb searcher for full in-memeory search
	v6InMemSearcher *xdb.Searcher
}

// create a new Ip2Region service with specified v4 and v6 config.
// set it to nil to disabled the specified search for the specified version.
func NewIp2Region(v4Config *Config, v6Config *Config) (*Ip2Region, error) {
	var err error

	// check and init the v4 pool or in-memory searcher
	var v4Pool *SearcherPool
	var v4InMemSearcher *xdb.Searcher
	if v4Config == nil {
		// with IPv4 disabled ?
		v4Pool = nil
		v4InMemSearcher = nil
	} else if v4Config.cachePolicy == BufferCache {
		v4Pool = nil
		v4InMemSearcher, err = xdb.NewWithBuffer(v4Config.ipVersion, v4Config.cBuffer)
		if err != nil {
			return nil, fmt.Errorf("failed to create v4 in-memory searcher: %w", err)
		}
	} else {
		v4InMemSearcher = nil
		v4Pool, err = NewSearcherPool(v4Config)
		if err != nil {
			return nil, fmt.Errorf("failed to create v4 searcher pool: %w", err)
		}
	}

	// check and init the v6 pool or in-memory searcher
	var v6Pool *SearcherPool
	var v6InMemSearcher *xdb.Searcher
	if v6Config == nil {
		v6Pool = nil
		v6InMemSearcher = nil
	} else if v6Config.cachePolicy == BufferCache {
		v6Pool = nil
		v6InMemSearcher, err = xdb.NewWithBuffer(v6Config.ipVersion, v6Config.cBuffer)
		if err != nil {
			return nil, fmt.Errorf("failed to create v6 in-memory searcher: %w", err)
		}
	} else {
		v6InMemSearcher = nil
		v6Pool, err = NewSearcherPool(v6Config)
		if err != nil {
			return nil, fmt.Errorf("failed to create v6 in-memeory searcher pool: %w", err)
		}
	}

	return &Ip2Region{
		v4Pool:          v4Pool,
		v4InMemSearcher: v4InMemSearcher,

		v6Pool:          v6Pool,
		v6InMemSearcher: v6InMemSearcher,
	}, nil
}

// create the ip2region search service with the specified v4 & v6 xdb path.
// with default cache policy VIndexCache and default searchers = 20
func NewIp2RegionWithPath(v4XdbPath string, v6XdbPath string) (*Ip2Region, error) {
	var err error

	// create v4 config with default config items
	var v4Config *Config
	if v4XdbPath == "" {
		v4Config = nil
	} else {
		v4Config, err = NewV4Config(VIndexCache, v4XdbPath, 20)
		if err != nil {
			return nil, fmt.Errorf("failed to create v4 config: %w", err)
		}
	}

	// create v6 config with default config items
	var v6Config *Config
	if v6XdbPath == "" {
		v6Config = nil
	} else {
		v6Config, err = NewV6Config(VIndexCache, v6XdbPath, 20)
		if err != nil {
			return nil, fmt.Errorf("failed to create v6 config: %w", err)
		}
	}

	return NewIp2Region(v4Config, v6Config)
}

func (ip2r *Ip2Region) SearchByStr(ipStr string) (string, error) {
	ipBytes, err := xdb.ParseIP(ipStr)
	if err != nil {
		return "", err
	}

	return ip2r.Search(ipBytes)
}

func (ip2r *Ip2Region) Search(ipBytes []byte) (string, error) {
	if l := len(ipBytes); l == 4 {
		return ip2r.v4Search(ipBytes)
	} else if l == 16 {
		return ip2r.v6Search(ipBytes)
	} else {
		return "", fmt.Errorf("invalid byte ip address with len=%d", l)
	}
}

func (ip2r *Ip2Region) v4Search(ipBytes []byte) (string, error) {
	if ip2r.v4InMemSearcher != nil {
		return ip2r.v4InMemSearcher.Search(ipBytes)
	}

	// v4 search is disabled
	if ip2r.v4Pool == nil {
		return "", nil
	}

	v4Searcher := ip2r.v4Pool.BorrowSearcher()
	defer ip2r.v4Pool.ReturnSearcher(v4Searcher)
	return v4Searcher.Search(ipBytes)
}

func (ip2r *Ip2Region) v6Search(ipBytes []byte) (string, error) {
	if ip2r.v6InMemSearcher != nil {
		return ip2r.v6InMemSearcher.Search(ipBytes)
	}

	// v6 search is disabled
	if ip2r.v6Pool == nil {
		return "", nil
	}

	v6Searcher := ip2r.v6Pool.BorrowSearcher()
	defer ip2r.v6Pool.ReturnSearcher(v6Searcher)
	return v6Searcher.Search(ipBytes)
}

func (ip2r *Ip2Region) Close() {
	ip2r.CloseTimeout(time.Second * 10)
}

func (ip2r *Ip2Region) CloseTimeout(d time.Duration) {
	if ip2r.v4Pool != nil {
		ip2r.v4Pool.CloseTimeout(d)
	}

	if ip2r.v6Pool != nil {
		ip2r.v6Pool.CloseTimeout(d)
	}
}
