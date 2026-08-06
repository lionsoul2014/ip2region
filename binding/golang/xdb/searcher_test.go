// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package xdb

import (
	"testing"
)

// truncated content buffer (header only) must return an error instead of panicking
func TestSearchTruncatedContentBuffer(t *testing.T) {
	cBuff := make([]byte, HeaderInfoLength)
	s, err := NewWithBuffer(IPv4, cBuff)
	if err != nil {
		t.Fatalf("failed to new searcher: %s", err)
	}
	defer s.Close()

	if _, err := s.Search("1.2.3.4"); err == nil {
		t.Fatal("expected error with truncated content buffer, got nil")
	}
}

// content buffer truncated inside the vector index area must not panic
func TestSearchContentBufferTruncatedInVectorIndex(t *testing.T) {
	cBuff := make([]byte, HeaderInfoLength+100)
	s, err := NewWithBuffer(IPv4, cBuff)
	if err != nil {
		t.Fatalf("failed to new searcher: %s", err)
	}
	defer s.Close()

	if _, err := s.Search("1.2.3.4"); err == nil {
		t.Fatal("expected error with content buffer truncated in vector index, got nil")
	}
}

// too-short vector index must return an error instead of panicking
func TestSearchTruncatedVectorIndex(t *testing.T) {
	vIndex := make([]byte, 100)
	s, err := NewWithVectorIndex(IPv4, "../../../data/ip2region_v4.xdb", vIndex)
	if err != nil {
		t.Fatalf("failed to new searcher: %s", err)
	}
	defer s.Close()

	if _, err := s.Search("1.2.3.4"); err == nil {
		t.Fatal("expected error with truncated vector index, got nil")
	}
}

// sanity check: a full content buffer still searches correctly
func TestSearchFullContentBuffer(t *testing.T) {
	cBuff, err := LoadContentFromFile("../../../data/ip2region_v4.xdb")
	if err != nil {
		t.Fatalf("failed to load content: %s", err)
	}

	s, err := NewWithBuffer(IPv4, cBuff)
	if err != nil {
		t.Fatalf("failed to new searcher: %s", err)
	}
	defer s.Close()

	region, err := s.Search("219.133.110.197")
	if err != nil {
		t.Fatalf("failed to search: %s", err)
	}
	if region == "" {
		t.Fatal("empty region returned for a known ip")
	}
}
