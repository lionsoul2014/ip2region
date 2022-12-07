// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package xdb

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

// Util function

var shiftIndex = []int{24, 16, 8, 0}

func CheckIP(ip string) (uint32, error) {
	var ps = strings.Split(ip, ".")
	if len(ps) != 4 {
		return 0, fmt.Errorf("invalid ip address `%s`", ip)
	}

	var val uint32
	for i, s := range ps {
		d, err := strconv.Atoi(s)
		if err != nil {
			return 0, fmt.Errorf("the %dth part `%s` is not an integer", i, s)
		}

		if d < 0 || d > 255 {
			return 0, fmt.Errorf("the %dth part `%s` should be an integer bettween 0 and 255", i, s)
		}

		val |= uint32(d) << shiftIndex[i]
	}

	return val, nil
}

func Long2IP(ip uint32) string {
	return fmt.Sprintf("%d.%d.%d.%d", (ip>>24)&0xFF, (ip>>16)&0xFF, (ip>>8)&0xFF, (ip>>0)&0xFF)
}

func MidIP(sip uint32, eip uint32) uint32 {
	return uint32((uint64(sip) + uint64(eip)) >> 1)
}

func IterateSegments(handle *os.File, before func(l string), cb func(seg *Segment) error) error {
	var last *Segment = nil
	var scanner = bufio.NewScanner(handle)
	scanner.Split(bufio.ScanLines)
	for scanner.Scan() {
		var l = strings.TrimSpace(strings.TrimSuffix(scanner.Text(), "\n"))
		if len(l) < 1 { // ignore empty line
			continue
		}

		if l[0] == '#' { // ignore the comment line
			continue
		}

		if before != nil {
			before(l)
		}

		var ps = strings.SplitN(l, "|", 3)
		if len(ps) != 3 {
			return fmt.Errorf("invalid ip segment line `%s`", l)
		}

		sip, err := CheckIP(ps[0])
		if err != nil {
			return fmt.Errorf("check start ip `%s`: %s", ps[0], err)
		}

		eip, err := CheckIP(ps[1])
		if err != nil {
			return fmt.Errorf("check end ip `%s`: %s", ps[1], err)
		}

		if sip > eip {
			return fmt.Errorf("start ip(%s) should not be greater than end ip(%s)", ps[0], ps[1])
		}

		if len(ps[2]) < 1 {
			return fmt.Errorf("empty region info in segment line `%s`", l)
		}

		var seg = &Segment{
			StartIP: sip,
			EndIP:   eip,
			Region:  ps[2],
		}

		// check and automatic merging the Consecutive Segments which means:
		// 1, region info is the same
		// 2, last.eip+1 = cur.sip
		if last == nil {
			last = seg
			continue
		} else if last.Region == seg.Region {
			if err = seg.AfterCheck(last); err == nil {
				last.EndIP = seg.EndIP
				continue
			}
		}

		if err = cb(last); err != nil {
			return err
		}

		// reset the last
		last = seg
	}

	// process the last segment
	if last != nil {
		return cb(last)
	}

	return nil
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
