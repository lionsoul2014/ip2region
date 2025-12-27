// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package xdb

import (
	"fmt"
	"os"
	"testing"
)

func TestParseIP(t *testing.T) {
	var ips = []string{"29.34.191.255", "2c0f:fff0::", "2fff:ffff:ffff:ffff:ffff:ffff:ffff:ffff"}
	for _, ip := range ips {
		bytes, err := ParseIP(ip)
		if err != nil {
			t.Errorf("check ip `%s`: %s\n", IP2String(bytes), err)
		}

		nip := IP2String(bytes)
		fmt.Printf("checkip: (%s / %s), isEqual: %v\n", ip, nip, ip == nip)
	}
}

func TestIPCompare(t *testing.T) {
	var ipPairs = [][]string{
		{"1.2.3.4", "1.2.3.5"},
		{"58.250.36.41", "58.250.30.41"},
		{"2c10::", "2e00::"},
		{"fdff:ffff:ffff:ffff:ffff:ffff:ffff:ffff", "febf:ffff:ffff:ffff:ffff:ffff:ffff:ffff"},
		{"fe7f:ffff:ffff:ffff:ffff:ffff:ffff:ffff", "fe00::"},
	}

	for _, pairs := range ipPairs {
		fmt.Printf("IPCompare(%s, %s): %d\n", pairs[0], pairs[1], IPCompare([]byte(pairs[0]), []byte(pairs[1])))
	}
}

func TestIPAddOne(t *testing.T) {
	var ipPairs = [][]string{
		{"1.2.3.4", "1.2.3.5"},
		{"2.3.4.5", "2.3.4.6"},
		{"fdff:ffff:ffff:ffff:ffff:ffff:ffff:ffff", "fe00::"},
		{"2fff:ffff:ffff:ffff:ffff:ffff:ffff:ffff", "3000::"},
		{"2fff:ffff:ffff:ffff:ffff:ffff:ffff:ffff", "3000::1"},
	}

	for _, pairs := range ipPairs {
		sip, err := ParseIP(pairs[0])
		if err != nil {
			t.Errorf("parse ip `%s`: %s\n", pairs[0], err)
		}

		eip, err := ParseIP(pairs[1])
		if err != nil {
			t.Errorf("parse ip `%s`: %s\n", pairs[1], err)
		}

		fmt.Printf("IPAddOne(%s) = %s ? %d\n",
			pairs[0], pairs[1], IPCompare(IPAddOne(sip), eip))
	}
}

func TestIPAddOne2(t *testing.T) {
	var ip = []byte{0, 1, 2, 3}
	nip := IPAddOne(ip)
	fmt.Printf("nip: %+v, ip:%+v", ip, nip)
}

func TestIPSubOne(t *testing.T) {
	var ipPairs = [][]string{
		{"1.2.3.4", "1.2.3.5"},
		{"2.3.4.5", "2.3.4.6"},
		{"fdff:ffff:ffff:ffff:ffff:ffff:ffff:ffff", "fe00::"},
		{"2fff:ffff:ffff:ffff:ffff:ffff:ffff:ffff", "3000::"},
		{"2fff:ffff:ffff:ffff:ffff:ffff:ffff:ffff", "3000::1"},
	}

	for _, pairs := range ipPairs {
		sip, err := ParseIP(pairs[0])
		if err != nil {
			t.Errorf("parse ip `%s`: %s\n", pairs[0], err)
		}

		eip, err := ParseIP(pairs[1])
		if err != nil {
			t.Errorf("parse ip `%s`: %s\n", pairs[1], err)
		}

		fmt.Printf("IPSubOne(%s) = %s ? %d\n",
			pairs[1], pairs[0], IPCompare(IPSubOne(eip), sip))
	}
}

func TestIPSubOne2(t *testing.T) {
	var ip = []byte{0, 1, 2, 3}
	nip := IPSubOne(ip)
	fmt.Printf("nip: %+v, ip:%+v", ip, nip)
}

func TestSplitSegmentV4(t *testing.T) {
	// var str = "1.1.0.0|1.3.3.24|中国|广东|深圳|电信"
	// var str = "0.0.0.0|1.255.225.254|0|0|0|内网IP|内网IP"
	// var str = "29.0.0.0|29.34.191.255|美国|0|0|0|0"
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

func TestRegionFiltering(t *testing.T) {
	var line = "2001:1203:31:8000::|2001:1203:31:bfff:ffff:ffff:ffff:ffff||墨西哥|瓜纳华托州||||专线用户|"
	seg, err := SegmentFrom(line)
	if err != nil {
		t.Fatalf("failed to parse segment '%s': %s", line, err)
	}

	fReg, err := RegionFiltering(seg.Region, []int{1, 2, 4, 6})
	if err != nil {
		t.Fatalf("failed to filter region '%s': %s", seg.Region, err)
	}

	fmt.Printf("region: %s, filtered: %s\n", seg.Region, fReg)
}

func TestSplitSegmentV6(t *testing.T) {
	var str = "fec0::|ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff||瑞士|弗里堡州||||专线用户|IANA"
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
	handle, err := os.OpenFile("../../../data/segments.tests.mixed", os.O_RDONLY, 0600)
	if err != nil {
		t.Fatalf("failed to open tests file: %s", err)
	}

	_ = IterateSegments(handle, func(l string) {
		// fmt.Printf("load segment: `%s`\n", l)
	}, nil, func(seg *Segment) error {
		fmt.Printf("get segment: `%s`\n", seg)
		return nil
	})
}
