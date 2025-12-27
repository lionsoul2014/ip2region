// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package xdb

import (
	"fmt"
	"strings"
)

type Segment struct {
	StartIP []byte
	EndIP   []byte
	Region  string
}

func SegmentFrom(seg string) (*Segment, error) {
	var ps = strings.SplitN(strings.TrimSpace(seg), "|", 3)
	if len(ps) != 3 {
		return nil, fmt.Errorf("invalid ip segment `%s`", seg)
	}

	sip, err := ParseIP(ps[0])
	if err != nil {
		return nil, fmt.Errorf("check start ip `%s`: %s", ps[0], err)
	}

	eip, err := ParseIP(ps[1])
	if err != nil {
		return nil, fmt.Errorf("check end ip `%s`: %s", ps[1], err)
	}

	if IPCompare(sip, eip) > 0 {
		return nil, fmt.Errorf("start ip(%s) should not be greater than end ip(%s)", ps[0], ps[1])
	}

	return &Segment{
		StartIP: sip,
		EndIP:   eip,
		Region:  ps[2],
	}, nil
}

// AfterCheck check the current segment is the one just after the specified one
func (s *Segment) AfterCheck(last *Segment) error {
	if last != nil {
		if IPCompare(IPAddOne(last.EndIP), s.StartIP) != 0 {
			return fmt.Errorf(
				"discontinuous data segment: last.eip(%s)+1 != seg.sip(%s, %s)",
				IP2String(last.EndIP), IP2String(s.StartIP), s.Region,
			)
		}
	}

	return nil
}

// Split the segment based on the pre-two bytes
func (s *Segment) Split() []*Segment {
	// 1, split the segment with the first byte
	var tList []*Segment
	var sByte1, eByte1 = int(s.StartIP[0]), int(s.EndIP[0])
	// var nSip = s.StartIP
	for i := sByte1; i <= eByte1; i++ {
		// Make and init the new start & end IP
		sip := make([]byte, len(s.StartIP))
		eip := make([]byte, len(s.StartIP))

		if i == sByte1 {
			sip = s.StartIP
		} else {
			sip[0] = byte(i)
		}

		if i == eByte1 {
			eip = s.EndIP
		} else {
			// set the first byte
			eip[0] = byte(i)
			// fill the buffer with 0xFF
			for j := 1; j < len(eip); j++ {
				eip[j] = 0xFF
			}
		}

		// sip := (i << 24) | (nSip & 0xFFFFFF)
		// eip := (i << 24) | 0xFFFFFF
		// if eip < s.EndIP {
		// 	nSip = (i + 1) << 24
		// } else {
		// 	eip = s.EndIP
		// }

		// fmt.Printf("sip:%+v, eip: %+v\n", sip, eip)
		// append the new segment (maybe)
		tList = append(tList, &Segment{
			StartIP: sip,
			EndIP:   eip,
			// @Note: don't bother to copy the region
			/// Region: s.Region,
		})
	}

	// 2, split the segments with the second byte
	var segList []*Segment
	for _, seg := range tList {
		// base := seg.StartIP & 0xFF000000
		// nSip := seg.StartIP
		// sb2, eb2 := (seg.StartIP>>16)&0xFF, (seg.EndIP>>16)&0xFF
		sb2, eb2 := int(seg.StartIP[1]), int(seg.EndIP[1])
		// fmt.Printf("seg: %s, sb2: %d, eb2: %d\n", seg.String(), sb2, eb2)
		for i := sb2; i <= eb2; i++ {
			// sip := base | (i << 16) | (nSip & 0xFFFF)
			// eip := base | (i << 16) | 0xFFFF
			// if eip < seg.EndIP {
			// 	nSip = 0
			// } else {
			// 	eip = seg.EndIP
			// }

			sip := make([]byte, len(s.StartIP))
			eip := make([]byte, len(s.StartIP))
			sip[0] = seg.StartIP[0]
			eip[0] = seg.StartIP[0]

			if i == sb2 {
				sip = seg.StartIP
			} else {
				sip[1] = byte(i)
			}

			if i == eb2 {
				eip = seg.EndIP
			} else {
				eip[1] = byte(i)
				for j := 2; j < len(eip); j++ {
					eip[j] = 0xFF
				}
			}

			// fmt.Printf("i=%d, sip:%+v, eip: %+v\n", i, sip, eip)
			segList = append(segList, &Segment{
				StartIP: sip,
				EndIP:   eip,
				Region:  s.Region,
			})
		}
	}

	return segList
}

func (s *Segment) String() string {
	return fmt.Sprintf("%s|%s|%s", IP2String(s.StartIP), IP2String(s.EndIP), s.Region)
}

// Contains checks if an IP address is within this segment
func (s *Segment) Contains(ip []byte) bool {
	return IPCompare(s.StartIP, ip) <= 0 && IPCompare(ip, s.EndIP) <= 0
}
