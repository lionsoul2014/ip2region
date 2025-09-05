// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package xdb

import (
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
}

var (
	IPvx = &Version{}
	IPv4 = &Version{
		Id:               4,
		Name:             "IPv4",
		Bytes:            4,
		SegmentIndexSize: 14, // 4 + 4 + 2 + 4
	}
	IPv6 = &Version{
		Id:               6,
		Name:             "IPv6",
		Bytes:            16,
		SegmentIndexSize: 38, // 16 + 16 + 2 + 4
	}
)

func VersionFromIP(ip string) (*Version, error) {
	bytes, err := ParseIP(ip)
	if err != nil {
		return IPvx, fmt.Errorf("parse ip fail: %w", err)
	}

	if len(bytes) == 4 {
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
