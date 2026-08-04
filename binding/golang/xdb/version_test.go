// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package xdb

import (
	"bytes"
	"testing"
)

func TestIPv4IPCompareNoSideEffect(t *testing.T) {
	// ip2 is little-endian as stored in the xdb index, it must NOT be mutated
	ip1 := []byte{1, 2, 3, 4}
	ip2 := []byte{4, 3, 2, 1}
	orig := append([]byte(nil), ip2...)

	if r := IPv4.IPCompare(ip1, ip2); r != 0 {
		t.Fatalf("IPv4.IPCompare(%v, %v) = %d, 0 expected", ip1, ip2, r)
	}
	if !bytes.Equal(ip2, orig) {
		t.Fatalf("IPv4.IPCompare mutated its input: got %v, want %v", ip2, orig)
	}
}

func TestIPv4IPCompare(t *testing.T) {
	// ip1 is big-endian, ip2 is little-endian
	cases := []struct {
		ip1 []byte
		ip2 []byte
		exp int
	}{
		{[]byte{1, 2, 3, 4}, []byte{4, 3, 2, 1}, 0},   // 1.2.3.4 == 1.2.3.4
		{[]byte{1, 2, 3, 5}, []byte{4, 3, 2, 1}, 1},   // 1.2.3.5 > 1.2.3.4
		{[]byte{1, 2, 3, 3}, []byte{4, 3, 2, 1}, -1},  // 1.2.3.3 < 1.2.3.4
		{[]byte{58, 250, 36, 41}, []byte{41, 30, 250, 58}, 1}, // 58.250.36.41 > 58.250.30.41
	}

	for _, c := range cases {
		r := IPv4.IPCompare(c.ip1, c.ip2)
		if (r < 0 && c.exp >= 0) || (r == 0 && c.exp != 0) || (r > 0 && c.exp <= 0) {
			t.Errorf("IPv4.IPCompare(%v, %v) = %d, sign %d expected", c.ip1, c.ip2, r, c.exp)
		}
	}
}
