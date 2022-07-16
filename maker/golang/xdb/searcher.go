// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// ---
// ip2region database v2.0 searcher.
// this is part of the maker for testing and validate.
// please use the searcher in binding/golang for production use.
// And this is a Not thread safe implementation.

package xdb

import (
	"encoding/binary"
	"fmt"
	"os"
)

type Searcher struct {
	handle *os.File

	// header info
	header []byte

	// use it only when this feature enabled.
	// Preload the vector index will reduce the number of IO operations
	// thus speedup the search process
	vectorIndex []byte
}

func NewSearcher(dbFile string) (*Searcher, error) {
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
	if s.handle != nil {
		err := s.handle.Close()
		if err != nil {
			return
		}
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

	var buff = make([]byte, VectorIndexLength)
	rLen, err := s.handle.Read(buff)
	if err != nil {
		return err
	}

	if rLen != len(buff) {
		return fmt.Errorf("incomplete read: readed bytes should be %d", len(buff))
	}

	s.vectorIndex = buff
	return nil
}

// ClearVectorIndex clear preloaded vector index cache
func (s *Searcher) ClearVectorIndex() {
	s.vectorIndex = nil
}

// Search find the region for the specified ip address
func (s *Searcher) Search(ip uint32) (string, int, error) {
	// locate the segment index block based on the vector index
	var ioCount = 0
	var il0 = (ip >> 24) & 0xFF
	var il1 = (ip >> 16) & 0xFF
	var idx = il0*VectorIndexCols*VectorIndexSize + il1*VectorIndexSize
	var sPtr, ePtr = uint32(0), uint32(0)
	if s.vectorIndex != nil {
		sPtr = binary.LittleEndian.Uint32(s.vectorIndex[idx:])
		ePtr = binary.LittleEndian.Uint32(s.vectorIndex[idx+4:])
	} else {
		pos, err := s.handle.Seek(int64(HeaderInfoLength+idx), 0)
		if err != nil {
			return "", ioCount, fmt.Errorf("seek to vector index %d: %w", HeaderInfoLength+idx, err)
		}

		ioCount++
		var buff = make([]byte, VectorIndexSize)
		rLen, err := s.handle.Read(buff)
		if err != nil {
			return "", ioCount, fmt.Errorf("read vector index at %d: %w", pos, err)
		}

		if rLen != len(buff) {
			return "", ioCount, fmt.Errorf("incomplete read: readed bytes should be %d", len(buff))
		}

		sPtr = binary.LittleEndian.Uint32(buff)
		ePtr = binary.LittleEndian.Uint32(buff[4:])
	}

	//log.Printf("vIndex=%s", vIndex)
	// binary search the segment index to get the region
	var dataLen, dataPtr = 0, uint32(0)
	var buff = make([]byte, SegmentIndexSize)
	var l, h = 0, int((ePtr - sPtr) / SegmentIndexSize)
	for l <= h {
		// log.Printf("l=%d, h=%d", l, h)
		m := (l + h) >> 1
		p := sPtr + uint32(m*SegmentIndexSize)
		// log.Printf("m=%d, p=%d", m, p)
		_, err := s.handle.Seek(int64(p), 0)
		if err != nil {
			return "", ioCount, fmt.Errorf("seek to segment block at %d: %w", p, err)
		}

		ioCount++
		rLen, err := s.handle.Read(buff)
		if err != nil {
			return "", ioCount, fmt.Errorf("read segment index at %d: %w", p, err)
		}

		if rLen != len(buff) {
			return "", ioCount, fmt.Errorf("incomplete read: readed bytes should be %d", len(buff))
		}

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
		return "", ioCount, nil
	}

	// load and return the region data
	_, err := s.handle.Seek(int64(dataPtr), 0)
	if err != nil {
		return "", ioCount, fmt.Errorf("seek to data block at %d: %w", dataPtr, err)
	}

	ioCount++
	var regionBuff = make([]byte, dataLen)
	rLen, err := s.handle.Read(regionBuff)
	if err != nil {
		return "", ioCount, fmt.Errorf("read region data at %d: %w", dataPtr, err)
	}

	if rLen != dataLen {
		return "", ioCount, fmt.Errorf("incomplete read: readed bytes should be %d", dataLen)
	}

	return string(regionBuff), ioCount, nil
}
