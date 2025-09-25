// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package xdb

import (
	"bufio"
	"bytes"
	"fmt"
	"math/big"
	"net"
	"os"
	"strings"
)

// Util function

func ParseIP(ip string) ([]byte, error) {
	parsedIP := net.ParseIP(ip)
	if parsedIP == nil {
		return nil, fmt.Errorf("invalid ip address: %s", ip)
	}

	v4 := parsedIP.To4()
	if v4 != nil {
		return v4, nil
	}

	v6 := parsedIP.To16()
	if v6 != nil {
		return v6, nil
	}

	return nil, fmt.Errorf("invalid ip address: %s", ip)
}

func IP2String(ip []byte) string {
	return net.IP(ip[:]).String()
}

func IP2Long(ip []byte) *big.Int {
	return big.NewInt(0).SetBytes(ip)
}

// IPCompare compares two IP addresses
// Returns: -1 if ip1 < ip2, 0 if ip1 == ip2, 1 if ip1 > ip2
func IPCompare(ip1, ip2 []byte) int {
	// for i := 0; i < len(ip1); i++ {
	// 	if ip1[i] < ip2[i] {
	// 		return -1
	// 	}

	// 	if ip1[i] > ip2[i] {
	// 		return 1
	// 	}
	// }

	// return 0
	return bytes.Compare(ip1, ip2)
}

func IPAddOne(ip []byte) []byte {
	var r = make([]byte, len(ip))
	copy(r, ip)
	for i := len(ip) - 1; i >= 0; i-- {
		r[i]++
		if r[i] != 0 { // No overflow
			break
		}
	}

	return r
}

func IPSubOne(ip []byte) []byte {
	var r = make([]byte, len(ip))
	copy(r, ip)
	for i := len(ip) - 1; i >= 0; i-- {
		if r[i] != 0 { // No borrow needed
			r[i]--
			break
		}
		r[i] = 0xFF // borrow from the next byte
	}

	return r
}

func IPMiddle(sip, eip []byte) []byte {
	var result = make([]byte, len(sip))
	var carry uint16 = 0

	// Add the two addresses with carry
	for i := len(sip) - 1; i >= 0; i-- {
		sum := uint16(sip[i]) + uint16(eip[i]) + carry
		result[i] = byte(sum >> 0x01) // Divide by 2
		carry = (sum & 0x01) << 7     // Carry for next byte (shift to MSB)
	}

	return result
}

func IterateSegments(handle *os.File, before func(l string), filter func(region string) (string, error), done func(seg *Segment) error) error {
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

		sip, err := ParseIP(ps[0])
		if err != nil {
			return fmt.Errorf("check start ip `%s`: %s", ps[0], err)
		}

		eip, err := ParseIP(ps[1])
		if err != nil {
			return fmt.Errorf("check end ip `%s`: %s", ps[1], err)
		}

		if len(sip) != len(eip) {
			return fmt.Errorf("invalid ip segment line `%s`, sip/eip version not match", l)
		}

		if IPCompare(sip, eip) > 0 {
			return fmt.Errorf("start ip(%s) should not be greater than end ip(%s)", ps[0], ps[1])
		}

		// Allow empty region info since 2024/09/24
		// if len(ps[2]) < 1 {
		// 	return fmt.Errorf("empty region info in segment line `%s`", l)
		// }

		// check and do the region filter
		var region = ps[2]
		if filter != nil {
			region, err = filter(ps[2])
			if err != nil {
				return fmt.Errorf("failed to filter region `%s`: %s", ps[2], err)
			}
		}

		var seg = &Segment{
			StartIP: sip,
			EndIP:   eip,
			Region:  region,
		}

		// check and automatic merging the Consecutive Segments, which means:
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

		if err = done(last); err != nil {
			return err
		}

		// reset the last
		last = seg
	}

	// process the last segment
	if last != nil {
		return done(last)
	}

	return nil
}

func CheckSegments(segList []*Segment) error {
	var last *Segment
	for _, seg := range segList {
		// sip must <= eip
		if IPCompare(seg.StartIP, seg.EndIP) > 0 {
			return fmt.Errorf("segment `%s`: start ip should not be greater than end ip", seg.String())
		}

		// check the continuity of the data segment
		if last != nil {
			if IPCompare(IPAddOne(last.EndIP), seg.StartIP) != 0 {
				return fmt.Errorf("discontinuous segment `%s`: last.eip+1 != cur.sip", seg.String())
			}
		}

		last = seg
	}

	return nil
}

func RegionFiltering(region string, fields []int) (string, error) {
	if len(fields) == 0 {
		return region, nil
	}

	fs := strings.Split(region, "|")
	var sb []string
	for _, idx := range fields {
		if idx < 0 {
			return "", fmt.Errorf("negative filter index %d", idx)
		}

		if idx >= len(fs) {
			return "", fmt.Errorf("field index %d exceeded the max length of %d", idx, len(fs))
		}

		sb = append(sb, fs[idx])
	}

	return strings.Join(sb, "|"), nil
}
