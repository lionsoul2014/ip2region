// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// ---
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/16

package xdb

import (
	"fmt"
	"testing"
	"time"
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
	header, err := LoadHeaderFromFile("../../../data/ip2region.xdb")
	if err != nil {
		fmt.Printf("failed to load xdb header info: %s\n", err)
		return
	}

	fmt.Printf("Version        : %d\n", header.Version)
	fmt.Printf("IndexPolicy    : %s\n", header.IndexPolicy.String())
	fmt.Printf("CreatedAt      : %d(%s)\n", header.CreatedAt, time.Unix(int64(header.CreatedAt), 0).Format(time.RFC3339))
	fmt.Printf("StartIndexPtr  : %d\n", header.StartIndexPtr)
	fmt.Printf("EndIndexPtr    : %d\n", header.EndIndexPtr)
}
