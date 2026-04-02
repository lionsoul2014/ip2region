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

// IPSub Sub the spcecified two byte ip
func IPSub(sip, eip []byte) ([]byte, error) {
	if len(sip) != len(eip) {
		return []byte{}, fmt.Errorf("length of the two ips are not the same")
	}

	var carry uint16 = 0
	var result = make([]byte, len(sip)+1)

	for i := len(sip) - 1; i >= 0; i-- {
		sum := uint16(sip[i]) + uint16(eip[i]) + carry
		result[i+1] = byte(sum) // Store standard 8-bit result
		carry = sum >> 8        // Extract the 1-bit carry for the next byte
	}

	// check and append the carry
	if carry > 0 {
		result[0] = byte(carry)
		return result, nil
	} else {
		return result[1:], nil
	}
}

// IPHalf get the half value of an input byte ip
func IPHalf(ip []byte) []byte {
	var length = len(ip)
	var result = make([]byte, length)
	// Tracks the bit falling off from the previous byte
	var carry byte = 0

	for i := 0; i < length; i++ {
		// 1. Shift current byte right by 1
		// 2. Or (|) with the carry from the previous byte (shifted to the MSB position)
		result[i] = (ip[i] >> 1) | (carry << 7)

		// 3. Capture the Least Significant Bit (LSB) to use as carry for the next byte
		carry = ip[i] & 1
	}

	return result
}

// IPMiddle get the middle value of two input ip address
func IPMiddle(sip, eip []byte) ([]byte, error) {
	buf, err := IPSub(sip, eip)
	if err != nil {
		return []byte{}, fmt.Errorf("IPSub(%s, %s): %w", IP2String(sip), IP2String(eip), err)
	}

	return IPHalf(buf), nil
}

func IterateSegments(handle *os.File, autoMerge bool, before func(l string), filter func(region string) (string, error), done func(seg *Segment) error) (int, int, error) {
	var last *Segment = nil
	var totalCount, mergeCount = 0, 0
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

		totalCount++
		if before != nil {
			before(l)
		}

		var ps = strings.SplitN(l, "|", 3)
		if len(ps) != 3 {
			return totalCount, mergeCount, fmt.Errorf("invalid ip segment line `%s`", l)
		}

		sip, err := ParseIP(ps[0])
		if err != nil {
			return totalCount, mergeCount, fmt.Errorf("check start ip `%s`: %s", ps[0], err)
		}

		eip, err := ParseIP(ps[1])
		if err != nil {
			return totalCount, mergeCount, fmt.Errorf("check end ip `%s`: %s", ps[1], err)
		}

		if len(sip) != len(eip) {
			return totalCount, mergeCount, fmt.Errorf("invalid ip segment line `%s`, sip/eip version not match", l)
		}

		if IPCompare(sip, eip) > 0 {
			return totalCount, mergeCount, fmt.Errorf("start ip(%s) should not be greater than end ip(%s)", ps[0], ps[1])
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
				return totalCount, mergeCount, fmt.Errorf("failed to filter region `%s`: %s", ps[2], err)
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
		} else if autoMerge && last.Region == seg.Region {
			if err = seg.RightBehind(last); err == nil {
				mergeCount++
				last.EndIP = seg.EndIP
				continue
			}
		}

		if err = done(last); err != nil {
			return totalCount, mergeCount, err
		}

		// reset the last
		last = seg
	}

	// process the last segment
	if last != nil {
		return totalCount, mergeCount, done(last)
	}

	return totalCount, mergeCount, nil
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
