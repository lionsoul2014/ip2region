// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package xdb

import (
	"fmt"
	"strings"
)

type Version struct {
	Id               int
	SegmentIndexSize int
}

var (
	VX = &Version{}
	V4 = &Version{
		Id:               4,
		SegmentIndexSize: 14, // 4 + 4 + 2 + 4
	}
	V6 = &Version{
		Id:               6,
		SegmentIndexSize: 38, // 16 + 16 + 2 + 4
	}
)

func VersionFromData(ip string) (*Version, error) {
	bytes, err := ParseIP(ip)
	if err != nil {
		return VX, fmt.Errorf("parse ip fail: %w", err)
	}

	if len(bytes) == 4 {
		return V4, nil
	}

	return V6, nil
}

func VersionFromName(name string) (*Version, error) {
	switch strings.ToUpper(name) {
	case "V4", "IPV4":
		return V4, nil
	case "V6", "IPV6":
		return V6, nil
	default:
		return VX, fmt.Errorf("invalid version name `%s`", name)
	}
}
