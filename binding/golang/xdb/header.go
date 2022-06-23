// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/16

package xdb

import (
	"encoding/binary"
	"fmt"
)

type Header struct {
	// data []byte
	Version       uint16
	IndexPolicy   IndexPolicy
	CreatedAt     uint32
	StartIndexPtr uint32
	EndIndexPtr   uint32
}

func NewHeader(input []byte) (*Header, error) {
	if len(input) < 16 {
		return nil, fmt.Errorf("invalid input buffer")
	}

	return &Header{
		Version:       binary.LittleEndian.Uint16(input),
		IndexPolicy:   IndexPolicy(binary.LittleEndian.Uint16(input[2:])),
		CreatedAt:     binary.LittleEndian.Uint32(input[4:]),
		StartIndexPtr: binary.LittleEndian.Uint32(input[8:]),
		EndIndexPtr:   binary.LittleEndian.Uint32(input[12:]),
	}, nil
}
