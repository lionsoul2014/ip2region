// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// ---
// Ip2Region service config
//
// @Author Lion <chenxin619315@gmail.com>
// @Date   2025/12/03

package service

import (
	"fmt"
	"os"

	"github.com/lionsoul2014/ip2region/binding/golang/xdb"
)

const (
	NoCache     = 0
	VIndexCache = 1
	BufferCache = 2
)

type Config struct {
	cachePolicy int
	ipVersion   *xdb.Version

	// xdb file path
	xdbPath string
	header  *xdb.Header

	// buffers
	vIndex  []byte
	cBuffer []byte

	searchers int
}

func NewV4Config(cachePolicy int, xdbPath string, searchers int) (*Config, error) {
	return newConfig(cachePolicy, xdb.IPv4, xdbPath, searchers)
}

func NewV6Config(cachePolicy int, xdbPath string, searchers int) (*Config, error) {
	return newConfig(cachePolicy, xdb.IPv6, xdbPath, searchers)
}

func newConfig(cachePolicy int, ipVersion *xdb.Version, xdbPath string, searchers int) (*Config, error) {
	if searchers < 1 {
		return nil, fmt.Errorf("searchers=%d, > 0 expected", searchers)
	}

	// open the xdb binary file
	handle, err := os.OpenFile(xdbPath, os.O_RDONLY, 0600)
	if err != nil {
		return nil, err
	}

	// 1, verify the xdb
	err = xdb.Verify(handle)
	if err != nil {
		return nil, err
	}

	// 2, load the header
	header, err := xdb.LoadHeader(handle)
	if err != nil {
		return nil, err
	}

	// verify the ip version
	xIpVersion, err := xdb.VersionFromHeader(header)
	if err != nil {
		return nil, err
	}

	if xIpVersion.Id != ipVersion.Id {
		return nil, fmt.Errorf("ip verison not match: xdb file %s with ip version=%s, as %s expected", xdbPath, xIpVersion.Name, ipVersion.Name)
	}

	// 3, check and load the vector index buffer
	var vIndex []byte = nil
	if cachePolicy == VIndexCache {
		vIndex, err = xdb.LoadVectorIndex(handle)
		if err != nil {
			return nil, err
		}
	}

	// 4, check and load the content buffer
	var cBuffer []byte = nil
	if cachePolicy == BufferCache {
		cBuffer, err = xdb.LoadContent(handle)
		if err != nil {
			return nil, err
		}
	}

	return &Config{
		cachePolicy: cachePolicy,
		ipVersion:   ipVersion,

		xdbPath: xdbPath,
		header:  header,

		vIndex:  vIndex,
		cBuffer: cBuffer,

		searchers: searchers,
	}, nil
}

func (c *Config) String() string {
	vIndex := "null"
	if c.vIndex != nil {
		vIndex = fmt.Sprintf("{bytes:%d}", len(c.vIndex))
	}

	cBuffer := "null"
	if c.cBuffer != nil {
		cBuffer = fmt.Sprintf("{bytes:%d}", len(c.cBuffer))
	}

	return fmt.Sprintf(
		"{cache_policy:%d, version:%s, xdb_path:%s, header:%s, v_index:%s, c_buffer:%s}",
		c.cachePolicy, c.ipVersion.String(), c.xdbPath, c.header.String(), vIndex, cBuffer,
	)
}

func (c *Config) CachePolicy() int {
	return c.cachePolicy
}

func (c *Config) IPVersion() *xdb.Version {
	return c.ipVersion
}

func (c *Config) Header() *xdb.Header {
	return c.header
}

func (c *Config) VIndex() []byte {
	return c.vIndex
}

func (c *Config) CBuffer() []byte {
	return c.cBuffer
}

func (c *Config) Searchers() int {
	return c.searchers
}
