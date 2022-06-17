// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// ---
// ip2region database v2.0 searcher.
// @Note this is a Not thread safe implementation.

package ip2region

import (
	"encoding/binary"
	"fmt"
	"os"
)

const (
	HeaderInfoLength = 256
	VectorIndexRows  = 256
	VectorIndexCols  = 256
	VectorIndexSize  = 8
)

type Searcher struct {
	handle *os.File

	// header info
	header []byte

	// use it only when this feature enabled.
	// Preload the vector index will reduce the number of IO operations
	// thus speedup the search process
	vectorIndex [][]*VectorIndexBlock
}

func New(dbFile string) (*Searcher, error) {
	handle, err := os.OpenFile(dbFile, os.O_RDONLY, 0600)
	if err != nil {
		return nil, err
	}

	return &Searcher{
		handle: handle,
		header: nil,

		vectorIndex: nil,
	}, nil
}

func (s *Searcher) Close() {
	err := s.handle.Close()
	if err != nil {
		return
	}
}

// LoadVectorIndex load and cache the vector index for search speedup.
// this will take up VectorIndexRows x VectorIndexCols x VectorIndexSize bytes memory.
func (s *Searcher) LoadVectorIndex() error {
	// loaded already
	if s.vectorIndex != nil {
		return nil
	}

	// load all the vector index block
	_, err := s.handle.Seek(HeaderInfoLength, 0)
	if err != nil {
		return fmt.Errorf("seek to vector index: %w", err)
	}

	var buff = make([]byte, VectorIndexRows*VectorIndexCols*VectorIndexSize)
	rLen, err := s.handle.Read(buff)
	if err != nil {
		return err
	}

	if rLen != len(buff) {
		return fmt.Errorf("incomplete read: readed bytes should be %d", len(buff))
	}

	// decode the vector index blocks
	var vectorIndex = make([][]*VectorIndexBlock, VectorIndexRows)
	for r := 0; r < VectorIndexRows; r++ {
		vectorIndex[r] = make([]*VectorIndexBlock, VectorIndexCols)
		for c := 0; c < VectorIndexCols; c++ {
			offset := r*VectorIndexCols*VectorIndexSize + c*VectorIndexSize
			vectorIndex[r][c], err = VectorIndexBlockDecode(buff[offset:])
			if err != nil {
				return fmt.Errorf("decode vector index at [%d][%d]: %w", r, c, err)
			}
		}
	}

	s.vectorIndex = vectorIndex
	return nil
}

// ClearVectorIndex clear preloaded vector index cache
func (s *Searcher) ClearVectorIndex() {
	s.vectorIndex = nil
}

// Search find the region for the specified ip address
func (s *Searcher) Search(ip uint32) (string, error) {
	// locate the segment index block based on the vector index
	var vIndex *VectorIndexBlock
	if s.vectorIndex != nil {
		vIndex = s.vectorIndex[(ip>>24)&0xFF][(ip>>16)&0xFF]
	} else {
		l0, l1 := (ip>>24)&0xFF, (ip>>16)&0xFF
		offset := l0*VectorIndexCols*VectorIndexSize + l1*VectorIndexSize
		pos, err := s.handle.Seek(int64(HeaderInfoLength+offset), 0)
		if err != nil {
			return "", fmt.Errorf("seek to vector index[%d][%d]: %w", l0, l1, err)
		}

		var buff = make([]byte, 8)
		rLen, err := s.handle.Read(buff)
		if err != nil {
			return "", fmt.Errorf("read vector index at %d: %w", pos, err)
		}

		if rLen != len(buff) {
			return "", fmt.Errorf("incomplete read: readed bytes should be %d", len(buff))
		}

		vIndex, err = VectorIndexBlockDecode(buff)
		if err != nil {
			return "", fmt.Errorf("invalid vector index block at %d: %w", pos, err)
		}
	}

	//log.Printf("vIndex=%s", vIndex)
	// binary search the segment index to get the region
	var dataLen, dataPtr = 0, uint32(0)
	var buff = make([]byte, SegmentIndexBlockSize)
	var l, h = 0, int((vIndex.LastPtr - vIndex.FirstPtr) / SegmentIndexBlockSize)
	for l <= h {
		m := (l + h) >> 1
		p := vIndex.FirstPtr + uint32(m*SegmentIndexBlockSize)
		_, err := s.handle.Seek(int64(p), 0)
		if err != nil {
			return "", fmt.Errorf("seek to segment block at %d: %w", p, err)
		}

		rLen, err := s.handle.Read(buff)
		if err != nil {
			return "", fmt.Errorf("read segment index at %d: %w", p, err)
		}

		if rLen != len(buff) {
			return "", fmt.Errorf("incomplete read: readed bytes should be %d", len(buff))
		}

		// segIndex, err := SegmentIndexDecode(buff)
		// if err != nil {
		// 	return "", fmt.Errorf("invalid segment index block at %d: %w", p, err)
		// }
		// decode the data step by step to reduce the unnecessary calculations
		sip := binary.LittleEndian.Uint32(buff)
		if ip < sip {
			h = m - 1
		} else {
			eip := binary.LittleEndian.Uint32(buff[4:])
			if ip > eip {
				l = m + 1
			} else {
				dataLen = int(binary.LittleEndian.Uint16(buff[8:]))
				dataPtr = binary.LittleEndian.Uint32(buff[10:])
				break
			}
		}
	}

	if dataLen == 0 {
		return "", nil
	}

	// load and return the region data
	_, err := s.handle.Seek(int64(dataPtr), 0)
	if err != nil {
		return "", fmt.Errorf("seek to data block at %d: %w", dataPtr, err)
	}

	var regionBuff = make([]byte, dataLen)
	rLen, err := s.handle.Read(regionBuff)
	if err != nil {
		return "", fmt.Errorf("read region data at %d: %w", dataPtr, err)
	}

	if rLen != dataLen {
		return "", fmt.Errorf("incomplete read: readed bytes should be %d", dataLen)
	}

	return string(regionBuff), nil
}
