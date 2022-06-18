// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package xdb

import (
	"fmt"
	"testing"
)

func TestLoadVectorIndex(t *testing.T) {
	vIndex, err := LoadVectorIndexFromFile("../../../data/ip2region.xdb")
	if err != nil {
		fmt.Printf("failed to load vector index: %s\n", err)
		return
	}

	fmt.Printf("vIndex length: %d\n", len(vIndex))
}

func TestLoadContent(t *testing.T) {
	buff, err := LoadContentFromFile("../../../data/ip2region.xdb")
	if err != nil {
		fmt.Printf("failed to load xdb content: %s\n", err)
		return
	}

	fmt.Printf("buff length: %d\n", len(buff))
}

func TestLoadHeader(t *testing.T) {
	buff, err := LoadHeaderFromFile("../../../data/ip2region.xdb")
	if err != nil {
		fmt.Printf("failed to load xdb header info: %s\n", err)
		return
	}

	fmt.Printf("buff length: %d\n", len(buff))
}
