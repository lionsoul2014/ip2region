// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package xdb

import (
	"encoding/binary"
	"fmt"
	"os"
	"strings"
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

func TestIPSub(t *testing.T) {
	var strToSub = "1.2.3.4"
	bytesToSub, err := ParseIP(strToSub)
	if err != nil {
		t.Fatalf("failed to parse ip %s", strToSub)
	}
	var intToSub = int(binary.BigEndian.Uint32(bytesToSub))
	t.Logf("to sub ip: %d -> %s", intToSub, strToSub)

	counter := 0
	buf := make([]byte, 4)
	for i := 0; i < 0x2FFFFFFF; i++ {
		binary.BigEndian.PutUint32(buf, uint32(i))
		subVal, err := IPSub(buf, bytesToSub)
		if err != nil {
			t.Fatalf("failed to IPSub(%s,%s): %s", IP2String(buf), strToSub, err)
		}

		// do it as two integers
		byteSub := int(binary.BigEndian.Uint32(subVal))
		intSub := i + intToSub
		if byteSub != intSub {
			t.Fatal("byte and int sub value are not the same")
		}

		counter++
	}

	t.Logf("test done with %d ips", counter)
}

func TestIPHalf(t *testing.T) {
	var buf = make([]byte, 4)
	for i := 0; i < 0xFFFFFFFF; i++ {
		binary.BigEndian.PutUint32(buf, uint32(i))
		half := IPHalf(buf)

		// do it as two integers
		byteMiddle := binary.BigEndian.Uint32(half)
		intMidle := i >> 1
		if byteMiddle != uint32(intMidle) {
			t.Fatal("byte middle and int middle are not the same")
		}
	}
}

func TestSubOverflow(t *testing.T) {
	var ip1Str = "255.255.255.250"
	ip1Bytes, err := ParseIP(ip1Str)
	if err != nil {
		t.Fatalf("failed to ParseIP(%s): %s", ip1Str, err)
	}

	var buff = make([]byte, 4)
	for i := 0; i < 10; i++ {
		binary.BigEndian.PutUint32(buff, uint32(i))
		ipSub, err := IPSub(ip1Bytes, buff)
		if err != nil {
			t.Fatalf("failed to IPSub(%s, %s): %s", ip1Str, IP2String(buff), err)
		}

		t.Logf("IPSub(%s, %s) = %+v", ip1Str, IP2String(buff), ipSub)
	}
}

func TestIPMiddle(t *testing.T) {
	var sIPStr = "0.0.0.0"
	sBytes, err := ParseIP(sIPStr)
	if err != nil {
		t.Fatalf("failed to parse ip %s", sIPStr)
	}
	var sInt = int(binary.BigEndian.Uint32(sBytes))
	t.Logf("start ip: %d -> %s", sInt, sIPStr)

	counter := 0
	buf := make([]byte, 4)
	for i := 0; i < 0x0FFFFFFF; i++ {
		binary.BigEndian.PutUint32(buf, uint32(i))
		midVal, err := IPMiddle(sBytes, buf)
		if err != nil {
			t.Fatalf("failed to IPMiddle(%s,%s): %s", sIPStr, IP2String(buf), err)
		}

		// do it as two integers
		byteMid := int(binary.BigEndian.Uint32(midVal))
		intMid := (sInt + i) >> 1
		if byteMid != intMid {
			t.Fatal("byte and int middle value are not the same")
		}

		counter++
	}

	t.Logf("test done with %d ips", counter)
}

func TestSegmentFromt(t *testing.T) {
	var lines = []string{
		// ipv4 range
		"2.10.222.0|2.10.223.255|France|Brittany|0|Orange S.A.|FR",
		"8.35.35.0|8.35.159.255|United States|Colorado|0|ZSCALER, INC.|US",
		"223.104.64.128|223.104.64.159|中国|广东省|深圳市|移动|CN",

		// ipv4 CIDR
		"1.0.4.0/24|2497 6453 7545 2764 38803|IGP",
		"2.18.209.0/24|14537 3356 1299 34164 34164|IGP",
		"14.32.46.0/24|2497 4766 9696|IGP",

		// IPv6 range
		"2001:200:17c::|2001:200:180:ffff:ffff:ffff:ffff:ffff|Japan|Tokyo|Tokyo|WIDE Project|JP",
		"2001:506:100:226e::8|2001:506:100:226f::7|United States|Michigan|Detroit|Transact Ltd.|US",
		"2a13:aac4:1000::|2a13:aac4:ffff:ffff:ffff:ffff:ffff:ffff|中国|广东省|深圳市|MLGT|CN",

		// IPv6 CIDR
		"2c0f:fc89:8081::/48|32590 9002 3257 8452 36992 36992 36992 36992 36992 36992 36992 36992 36992|EGP",
		"2c0f:fe08:20a::/48|2497 6939 36914|IGP",
		"2c0f:ffc8::/32|14537 23764 37468 22355|IGP",
	}

	for _, str := range lines {
		seg, err := SegmentFrom(str, NewRegion)
		if err != nil {
			t.Fatalf("parse segment: %s", err)
		}

		fmt.Printf("seg={%s, %s, %s}\n", IP2String(seg.StartIP), IP2String(seg.EndIP), seg.Region.Str)
	}
}

func TestSplitSegmentV4(t *testing.T) {
	// var str = "1.1.0.0|1.3.3.24|中国|广东|深圳|电信"
	// var str = "0.0.0.0|1.255.225.254|0|0|0|内网IP|内网IP"
	// var str = "29.0.0.0|29.34.191.255|美国|0|0|0|0"
	var str = "28.201.224.0|29.34.191.255|美国|0|0|0|0"
	seg, err := SegmentFrom(str, NewRegion)
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
	seg, err := SegmentFrom(line, NewRegion)
	if err != nil {
		t.Fatalf("failed to parse segment '%s': %s", line, err)
	}

	fReg, err := seg.Region.Filtering([]int{1, 2, 4, 6})
	if err != nil {
		t.Fatalf("failed to filter region '%s': %s", seg.Region, err)
	}

	fmt.Printf("region: %s, filtered: %s\n", seg.Region, fReg)
}

func TestSplitSegmentV6(t *testing.T) {
	var str = "fec0::|ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff||瑞士|弗里堡州||||专线用户|IANA"
	seg, err := SegmentFrom(str, NewRegion)
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
	handle, err := os.OpenFile("../../../data/sample/segments.tests.mixed", os.O_RDONLY, 0600)
	if err != nil {
		t.Fatalf("failed to open tests file: %s", err)
	}

	total, merged, err := IterateSegments(handle, true, func(l string) {
		// fmt.Printf("load segment: `%s`\n", l)
	}, nil, NewRegion, func(seg *Segment) error {
		fmt.Printf("{%s, %s} -> `%s`\n", IP2String(seg.StartIP), IP2String(seg.EndIP), seg.Region)
		return nil
	})
	if err != nil {
		t.Fatalf("iterate error: %s", err)
	}

	fmt.Printf("done iterate -> total:%d, merged:%d\n", total, merged)
}

func TestStringTokenizer(t *testing.T) {
	var strList = []string{
		"24.231.126.0/24|14537 2914 29866|IGP",
		"24.231.126.0|24.231.126.255|14537 2914 29866|IGP",
	}

	var counter = 0
	for _, str := range strList {
		tokens := StringTokenizer(str, "|", func(s string, start int) bool {
			// fmt.Printf("%s[idx=%d, |]=%s\n", str, start, s)
			if counter == 0 {
				if strings.Index(s, "/") > 0 {
					return false
				}
			}

			counter++
			return counter < 2
		})
		fmt.Printf("%d tokens: %s\n", len(tokens), strings.Join(tokens, ", "))
	}
}

func TestCIDR2Range(t *testing.T) {
	var strList = [][3]string{
		{"43.247.92.0/22", "43.247.92.0", "43.247.95.255"},
		{"64.252.86.39/29", "64.252.86.32", "64.252.86.39"},
		{"103.37.44.0/22", "103.37.44.0", "103.37.47.255"},
		{"111.223.12.0/22", "111.223.12.0", "111.223.15.255"},
		{"43.248.80.0/20", "43.248.80.0", "43.248.95.255"},
		{"192.168.100.0/22", "192.168.100.0", "192.168.103.255"},
		{"2403:3380::/32", "2403:3380::", "2403:3380:ffff:ffff:ffff:ffff:ffff:ffff"},
		{"2001:db8:85a3::/64", "2001:db8:85a3::", "2001:db8:85a3:0:ffff:ffff:ffff:ffff"},
		{"2001:db8:abcd::/48", "2001:db8:abcd::", "2001:db8:abcd:ffff:ffff:ffff:ffff:ffff"},
	}

	for _, item := range strList {
		sip, eip, err := CIDR2Range(item[0])
		if err != nil {
			t.Fatalf("CIDR2Range: %s", err)
		}

		tSip, err := ParseIP(item[1])
		if err != nil {
			t.Fatalf("parse start ip: %s", err)
		} else if IPCompare(sip, tSip) != 0 {
			t.Fatalf("start ip %s != %s", IP2String(sip), item[1])
		}

		tEip, err := ParseIP(item[2])
		if err != nil {
			t.Fatalf("parse end ip: %s", err)
		} else if IPCompare(eip, tEip) != 0 {
			t.Fatalf("end ip %s != %s", IP2String(eip), item[2])
		}

		fmt.Printf("cidr=%s: {sip=%s, eip=%s}\n", item[0], item[1], item[2])
	}
}
