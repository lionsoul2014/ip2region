// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// ---
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/16

package xdb

import (
	"encoding/binary"
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

func TestLoadVectorIndex(t *testing.T) {
	vIndex, err := LoadVectorIndexFromFile("../../../data/ip2region_v4.xdb")
	if err != nil {
		fmt.Printf("failed to load vector index: %s\n", err)
		return
	}

	fmt.Printf("vIndex length: %d\n", len(vIndex))
}

func TestLoadContent(t *testing.T) {
	buff, err := LoadContentFromFile("../../../data/ip2region_v4.xdb")
	if err != nil {
		fmt.Printf("failed to load xdb content: %s\n", err)
		return
	}

	fmt.Printf("buff length: %d\n", len(buff))
}

func TestLoadHeader(t *testing.T) {
	header, err := LoadHeaderFromFile("../../../data/ip2region_v4.xdb")
	if err != nil {
		fmt.Printf("failed to load xdb header info: %s\n", err)
		return
	}

	fmt.Printf("Version         : %d\n", header.Version)
	fmt.Printf("IndexPolicy     : %s\n", header.IndexPolicy.String())
	fmt.Printf("CreatedAt       : %d(%s)\n", header.CreatedAt, time.Unix(int64(header.CreatedAt), 0).Format(time.RFC3339))
	fmt.Printf("StartIndexPtr   : %d\n", header.StartIndexPtr)
	fmt.Printf("EndIndexPtr     : %d\n", header.EndIndexPtr)
	fmt.Printf("IPVersion       : %d\n", header.IPVersion)
	fmt.Printf("RuntimePtrBytes : %d\n", header.RuntimePtrBytes)
}
