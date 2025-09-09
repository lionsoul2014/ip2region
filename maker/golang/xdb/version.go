// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package xdb

import (
	"bytes"
	"fmt"
	"strings"
)

const (
	IPv4VersionNo = 4
	IPv6VersionNo = 6
)

type Version struct {
	Id               int
	Name             string
	Bytes            int
	SegmentIndexSize int

	// bytes encode
	PutBytes func([]byte, []byte) int

	// ip compares
	IPCompare func([]byte, []byte) int
}

func (v *Version) String() string {
	return fmt.Sprintf("{Id:%d, Name:%s, Bytes:%d, IndexSize: %d}", v.Id, v.Name, v.Bytes, v.SegmentIndexSize)
}

var (
	IPvx = &Version{}
	IPv4 = &Version{
		Id:               4,
		Name:             "IPv4",
		Bytes:            4,
		SegmentIndexSize: 14, // 4 + 4 + 2 + 4,
		PutBytes: func(buff []byte, ip []byte) int {
			// binary.LittleEndian.PutUint32(buff, binary.BigEndian.Uint32(ip))
			// Little Endian byte order for compatible with the old searcher implementation
			buff[0] = ip[3]
			buff[1] = ip[2]
			buff[2] = ip[1]
			buff[3] = ip[0]
			return len(ip)
		},
		IPCompare: func(ip1 []byte, ip2 []byte) int {
			// ip1 - with Bit endian parsed from an input
			// ip2 - with Little endian read from the xdb index
			ip2[0], ip2[3] = ip2[3], ip2[0]
			ip2[1], ip2[2] = ip2[2], ip2[1]
			return bytes.Compare(ip1, ip2)
		},
	}
	IPv6 = &Version{
		Id:               6,
		Name:             "IPv6",
		Bytes:            16,
		SegmentIndexSize: 38, // 16 + 16 + 2 + 4,
		// Big Endian byte order to follow the network byte order
		PutBytes: func(buff []byte, ip []byte) int {
			return copy(buff, ip)
		},
		IPCompare: func(ip1, ip2 []byte) int {
			return bytes.Compare(ip1, ip2)
		},
	}
)

func VersionFromIP(ip string) (*Version, error) {
	r, err := ParseIP(ip)
	if err != nil {
		return IPvx, fmt.Errorf("parse ip fail: %w", err)
	}

	if len(r) == 4 {
		return IPv4, nil
	}

	return IPv6, nil
}

func VersionFromName(name string) (*Version, error) {
	switch strings.ToUpper(name) {
	case "V4", "IPV4":
		return IPv4, nil
	case "V6", "IPV6":
		return IPv6, nil
	default:
		return IPvx, fmt.Errorf("invalid version name `%s`", name)
	}
}
