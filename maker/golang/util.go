// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package main

import (
	"encoding/binary"
	"fmt"
	"strconv"
	"strings"
)

// Util function

func CheckIP(ip string) (uint32, error) {
	var ps = strings.Split(ip, ".")
	if len(ps) != 4 {
		return 0, fmt.Errorf("invalid ip address `%s`", ip)
	}

	var buff = make([]byte, 4)
	for i, s := range ps {
		d, err := strconv.Atoi(s)
		if err != nil {
			return 0, fmt.Errorf("the %dth part `%s` is not an integer", i, s)
		}

		if d < 0 || d > 255 {
			return 0, fmt.Errorf("the %dth part `%s` should be an integer bettween 0 and 255", i, s)
		}

		buff[i] = byte(d)
	}

	// convert the ip to integer

	return binary.BigEndian.Uint32(buff), nil
}

func Long2IP(ip uint32) string {
	var buff = make([]string, 4)
	buff[0] = fmt.Sprintf("%d", (ip>>24)&0xFF)
	buff[1] = fmt.Sprintf("%d", (ip>>16)&0xFF)
	buff[2] = fmt.Sprintf("%d", (ip>>8)&0xFF)
	buff[3] = fmt.Sprintf("%d", (ip>>0)&0xFF)
	return strings.Join(buff, ".")
}

func MidIP(sip uint32, eip uint32) uint32 {
	return uint32((uint64(sip) + uint64(eip)) >> 1)
}

func CheckSegments(segList []*Segment) error {
	var last *Segment
	for _, seg := range segList {
		// sip must <= eip
		if seg.StartIP > seg.EndIP {
			return fmt.Errorf("segment `%s`: start ip should not be greater than end ip", seg.String())
		}

		// check the continuity of the data segment
		if last != nil {
			if last.EndIP+1 != seg.StartIP {
				return fmt.Errorf("discontinuous segment `%s`: last.eip+1 != cur.sip", seg.String())
			}
		}

		last = seg
	}

	return nil
}
