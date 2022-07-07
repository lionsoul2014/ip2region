// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package xdb

import (
	"encoding/binary"
	"fmt"
	"net"
	"testing"
)

func TestCheckIP(t *testing.T) {
	var str = "29.34.191.255"
	ip, err := CheckIP(str)
	if err != nil {
		t.Errorf("check ip `%s`: %s\n", str, err)
	}
	netip := net.ParseIP(str).To4()
	if netip == nil {
		t.Fatalf("parse ip `%s` failed", str)
	}
	u32 := binary.BigEndian.Uint32(netip)
	fmt.Printf("checkip: %d, parseip: %d, isequal: %v", ip, u32, ip == u32)
}

func TestLong2IP(t *testing.T) {
	var str = "29.34.191.255"
	netip := net.ParseIP(str).To4()
	if netip == nil {
		t.Fatalf("parse ip `%s` failed", str)
	}
	u32 := binary.BigEndian.Uint32(netip)
	ipstr := Long2IP(u32)
	fmt.Printf("originIP: %s, Long2IP: %s, isequal: %v", str, ipstr, ipstr == str)
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
