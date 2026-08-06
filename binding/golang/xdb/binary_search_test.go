// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package xdb

import (
	"encoding/binary"
	"testing"
)

// buildTestXdbContent builds a minimal IPv4 xdb content in memory:
//
//	- 256-byte header (Structure20)
//	- a full vector index block, only cell (1,1) is filled
//	- one segment index entry: [1.1.1.0, 1.1.1.255] -> region "test"
//	- the region data right after the segment index
//
// The total content is sized so that reading past the last segment index
// (i.e. at the region area) with a full segment-index-sized buffer would
// fail, which lets the tests catch the binary-search upper-bound bug.
func buildTestXdbContent(t *testing.T) []byte {
	t.Helper()

	segIndexSize := IPv4.SegmentIndexSize
	segStart := HeaderInfoLength + VectorIndexRows*VectorIndexCols*VectorIndexSize
	segEnd := segStart + segIndexSize
	region := "test"

	content := make([]byte, segEnd+len(region))

	// header
	binary.LittleEndian.PutUint16(content[0:], Structure20)
	binary.LittleEndian.PutUint16(content[2:], uint16(VectorIndexPolicy))
	binary.LittleEndian.PutUint32(content[4:], 0)
	binary.LittleEndian.PutUint32(content[8:], uint32(segStart))
	binary.LittleEndian.PutUint32(content[12:], uint32(segEnd))
	binary.LittleEndian.PutUint16(content[16:], uint16(IPv4VersionNo))
	binary.LittleEndian.PutUint16(content[18:], 4)

	// vector index cell (1,1) -> segment index area
	idx := 1*VectorIndexCols*VectorIndexSize + 1*VectorIndexSize
	binary.LittleEndian.PutUint32(content[HeaderInfoLength+idx:], uint32(segStart))
	binary.LittleEndian.PutUint32(content[HeaderInfoLength+idx+4:], uint32(segEnd))

	// segment index: start 1.1.1.0, end 1.1.1.255, stored little-endian
	binary.LittleEndian.PutUint32(content[segStart:], binary.BigEndian.Uint32([]byte{1, 1, 1, 0}))
	binary.LittleEndian.PutUint32(content[segStart+4:], binary.BigEndian.Uint32([]byte{1, 1, 1, 255}))
	binary.LittleEndian.PutUint16(content[segStart+8:], uint16(len(region)))
	binary.LittleEndian.PutUint32(content[segStart+10:], uint32(segEnd))

	// region data
	copy(content[segEnd:], region)

	return content
}

// an ip inside the segment must be found
func TestBinarySearchHit(t *testing.T) {
	s, err := NewWithBuffer(IPv4, buildTestXdbContent(t))
	if err != nil {
		t.Fatalf("failed to new searcher: %s", err)
	}
	defer s.Close()

	region, err := s.Search("1.1.1.100")
	if err != nil {
		t.Fatalf("failed to search: %s", err)
	}
	if region != "test" {
		t.Fatalf("unexpected region `%s`, `test` expected", region)
	}
}

// an ip inside the same vector index cell but outside any segment
// must return an empty region without error. Before the fix, the binary
// search upper bound was N (one past the last index), so it read past
// the segment index area and failed on the tiny region buffer.
func TestBinarySearchMissInCell(t *testing.T) {
	s, err := NewWithBuffer(IPv4, buildTestXdbContent(t))
	if err != nil {
		t.Fatalf("failed to new searcher: %s", err)
	}
	defer s.Close()

	region, err := s.Search("1.1.2.1")
	if err != nil {
		t.Fatalf("expected empty region for a gap ip, got error: %s", err)
	}
	if region != "" {
		t.Fatalf("expected empty region, got `%s`", region)
	}
}
