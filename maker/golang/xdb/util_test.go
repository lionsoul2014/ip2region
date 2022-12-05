// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package xdb

import (
	"encoding/binary"
	"fmt"
	"net"
	"os"
	"testing"
)

func TestCheckIP(t *testing.T) {
	var str = "29.34.191.255"
	ip, err := CheckIP(str)
	if err != nil {
		t.Errorf("check ip `%s`: %s\n", str, err)
	}

	netIP := net.ParseIP(str).To4()
	if netIP == nil {
		t.Fatalf("parse ip `%s` failed", str)
	}

	u32 := binary.BigEndian.Uint32(netIP)
	fmt.Printf("checkip: %d, parseip: %d, isEqual: %v\n", ip, u32, ip == u32)
}

func TestLong2IP(t *testing.T) {
	var str = "29.34.191.255"
	netIP := net.ParseIP(str).To4()
	if netIP == nil {
		t.Fatalf("parse ip `%s` failed", str)
	}

	u32 := binary.BigEndian.Uint32(netIP)
	ipStr := Long2IP(u32)
	fmt.Printf("originIP: %s, Long2IP: %s, isEqual: %v\n", str, ipStr, ipStr == str)
}

func TestSplitSegment(t *testing.T) {
	// var str = "1.1.0.0|1.3.3.24|中国|广东|深圳|电信"
	// var str = "0.0.0.0|1.255.225.254|0|0|0|内网IP|内网IP"
	var str = "28.201.224.0|29.34.191.255|美国|0|0|0|0"
	seg, err := SegmentFrom(str)
	if err != nil {
		t.Fatalf("failed to parser segment '%s': %s", str, err)
	}

	fmt.Printf("idx: src, seg: %s\n", seg.String())
	var segList = seg.Split()
	err = CheckSegments(segList)
	if err != nil {
		t.Fatalf("check segments: %s", err.Error())
	}

	for i, s := range segList {
		fmt.Printf("idx: %3d, seg: %s\n", i, s.String())
	}
}

func TestIterateSegments(t *testing.T) {
	handle, err := os.OpenFile("../../../data/segments.tests", os.O_RDONLY, 0600)
	if err != nil {
		t.Fatalf("failed to open tests file: %s", err)
	}

	err = IterateSegments(handle, func(l string) {
		// fmt.Printf("load segment: `%s`\n", l)
	}, func(seg *Segment) error {
		fmt.Printf("get segment: `%s`\n", seg)
		return nil
	})
}
