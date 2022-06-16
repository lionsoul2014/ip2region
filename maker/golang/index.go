// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package main

import (
	"encoding/binary"
	"fmt"
	"strings"
)

type IndexPolicy int

const (
	VectorIndexPolicy IndexPolicy = 1
	BTreeIndexPolicy  IndexPolicy = 2
)

func IndexPolicyFromString(str string) (IndexPolicy, error) {
	switch strings.ToLower(str) {
	case "vector":
		return VectorIndexPolicy, nil
	case "btree":
		return BTreeIndexPolicy, nil
	default:
		return VectorIndexPolicy, fmt.Errorf("invalid policy '%s'", str)
	}
}

const SegmentIndexBlockSize = 14

type SegmentIndexBlock struct {
	StartIP uint32
	EndIP   uint32
	DataLen uint16
	DataPtr uint32
}

func SegmentIndexDecode(input []byte) (*SegmentIndexBlock, error) {
	if len(input) < 14 {
		return nil, fmt.Errorf("input is less than 14 bytes")
	}

	return &SegmentIndexBlock{
		StartIP: binary.LittleEndian.Uint32(input),
		EndIP:   binary.LittleEndian.Uint32(input[4:]),
		DataLen: binary.LittleEndian.Uint16(input[8:]),
		DataPtr: binary.LittleEndian.Uint32(input[10:]),
	}, nil
}

func (s *SegmentIndexBlock) Encode() []byte {
	var buff = make([]byte, 14)
	binary.LittleEndian.PutUint32(buff, s.StartIP)
	binary.LittleEndian.PutUint32(buff[4:], s.EndIP)
	binary.LittleEndian.PutUint16(buff[8:], s.DataLen)
	binary.LittleEndian.PutUint32(buff[10:], s.DataPtr)
	return buff
}

func (s *SegmentIndexBlock) String() string {
	return fmt.Sprintf("{sip: %d, eip: %d, len: %d, ptr: %d}", s.StartIP, s.EndIP, s.DataLen, s.DataPtr)
}

// ------------

type VectorIndexBlock struct {
	FirstPtr uint32
	LastPtr  uint32
}

func VectorIndexBlockDecode(input []byte) (*VectorIndexBlock, error) {
	if len(input) < 8 {
		return nil, fmt.Errorf("input should be not less then 8 bytes")
	}

	return &VectorIndexBlock{
		FirstPtr: binary.LittleEndian.Uint32(input),
		LastPtr:  binary.LittleEndian.Uint32(input[4:]),
	}, nil
}

func (v VectorIndexBlock) Encode() []byte {
	var buff = make([]byte, 8)
	binary.LittleEndian.PutUint32(buff, v.FirstPtr)
	binary.LittleEndian.PutUint32(buff[4:], v.LastPtr)
	return buff
}

func (v VectorIndexBlock) String() string {
	return fmt.Sprintf("{FristPtr: %d, LastPtr: %d}", v.FirstPtr, v.LastPtr)
}
