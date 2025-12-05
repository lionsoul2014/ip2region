// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package xdb

import (
	"bytes"
	"fmt"
	"strings"
)

type Version struct {
	Id               int
	Name             string
	Bytes            int
	SegmentIndexSize int

	// function to compare two ips
	IPCompare func([]byte, []byte) int
}

func (v *Version) String() string {
	return fmt.Sprintf(
		"{id:%d, name:%s, bytes:%d, segment_index_size:%d}",
		v.Id, v.Name, v.Bytes, v.SegmentIndexSize,
	)
}

const (
	IPv4VersionNo = 4
	IPv6VersionNo = 6
)

var (
	IPvx = &Version{}
	IPv4 = &Version{
		Id:               IPv4VersionNo,
		Name:             "IPv4",
		Bytes:            4,
		SegmentIndexSize: 14, // 4 + 4 + 2 + 4,
		IPCompare: func(ip1, ip2 []byte) int {
			// ip1 - with Big endian byte order parsed from an input
			// ip2 - with Little endian byte order read from the xdb index
			ip2[0], ip2[3] = ip2[3], ip2[0]
			ip2[1], ip2[2] = ip2[2], ip2[1]
			return bytes.Compare(ip1, ip2)
		},
	}
	IPv6 = &Version{
		Id:               IPv6VersionNo,
		Name:             "IPv6",
		Bytes:            16,
		SegmentIndexSize: 38, // 16 + 16 + 2 + 4,
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

func VersionFromHeader(header *Header) (*Version, error) {
	// old structure with IPv4 supports ONLY
	if header.Version == Structure20 {
		return IPv4, nil
	}

	// structure 3.0 after IPv6 supporting
	if header.Version != Structure30 {
		return IPvx, fmt.Errorf("invalid version `%d`", header.IPVersion)
	}

	switch header.IPVersion {
	case IPv4VersionNo:
		return IPv4, nil
	case IPv6VersionNo:
		return IPv6, nil
	default:
		return IPvx, fmt.Errorf("invalid version `%d`", header.Version)
	}
}
