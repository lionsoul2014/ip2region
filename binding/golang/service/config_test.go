// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package service

import (
	"fmt"
	"testing"
)

func TestV4Config(t *testing.T) {
	v4Config, err := NewV4Config(VIndexCache, "../../../data/ip2region_v4.xdb", 10)
	if err != nil {
		t.Errorf("failed to new v4 config: %s", err)
		return
	}

	v4BufferConfig, err := NewV4Config(BufferCache, "../../../data/ip2region_v4.xdb", 10)
	if err != nil {
		t.Errorf("failed to new v4 config: %s", err)
		return
	}

	fmt.Printf("v4Config: %s\n", v4Config)
	fmt.Printf("v4BufferConfig: %s\n", v4BufferConfig)
}

func TestV6Config(t *testing.T) {
	v6Config, err := NewV6Config(NoCache, "../../../data/ip2region_v6.xdb", 10)
	if err != nil {
		t.Errorf("failed to new v6 config: %s", err)
		return
	}

	fmt.Printf("v6Config: %s\n", v6Config)
}
