// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// ---
// Ip2Region database v2.0 searcher.
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
	version *Version

	handle *os.File

	// header info
	header []byte

	// use it only when this feature enabled.
	// Preload the vector index will reduce the number of IO operations
	// thus speedup the search process
	vectorIndex []byte
}

func NewSearcher(version *Version, dbFile string) (*Searcher, error) {
	handle, err := os.OpenFile(dbFile, os.O_RDONLY, 0600)
	if err != nil {
		return nil, err
	}

	return &Searcher{
		version: version,

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
func (s *Searcher) Search(ip []byte) (string, int, error) {
	// version check
	if len(ip) != s.version.Bytes {
		return "", 0, fmt.Errorf("invalid ip address(%s expected)", s.version.Name)
	}

	// locate the segment index block based on the vector index
	var ioCount = 0
	var il0, il1, bytes, tBytes = int(ip[0]), int(ip[1]), len(ip), len(ip) << 1
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
	var segIndexSize = uint32(s.version.SegmentIndexSize)
	var dataLen, dataPtr = 0, uint32(0)
	var buff = make([]byte, segIndexSize)
	var l, h = 0, int((ePtr - sPtr) / segIndexSize)
	for l <= h {
		// log.Printf("l=%d, h=%d", l, h)
		m := (l + h) >> 1
		p := sPtr + uint32(m)*segIndexSize
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
		if s.version.IPCompare(ip, buff[0:bytes]) < 0 {
			h = m - 1
		} else if s.version.IPCompare(ip, buff[bytes:tBytes]) > 0 {
			l = m + 1
		} else {
			dataLen = int(binary.LittleEndian.Uint16(buff[tBytes:]))
			dataPtr = binary.LittleEndian.Uint32(buff[tBytes+2:])
			break
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

// LoadXdbHeader load the header info from the specified handle
func LoadXdbHeader(handle *os.File) ([]byte, error) {
	_, err := handle.Seek(0, 0)
	if err != nil {
		return nil, fmt.Errorf("seek to the header: %w", err)
	}

	var buff = make([]byte, HeaderInfoLength)
	rLen, err := handle.Read(buff)
	if err != nil {
		return nil, err
	}

	if rLen != len(buff) {
		return nil, fmt.Errorf("incomplete read: readed bytes should be %d", len(buff))
	}

	return buff, nil
}

// LoadXdbHeaderFromFile load header info from the specified db file path
func LoadXdbHeaderFromFile(dbFile string) ([]byte, error) {
	handle, err := os.OpenFile(dbFile, os.O_RDONLY, 0600)
	if err != nil {
		return nil, fmt.Errorf("open xdb file `%s`: %w", dbFile, err)
	}

	defer func(handle *os.File) {
		_ = handle.Close()
	}(handle)

	header, err := LoadXdbHeader(handle)
	if err != nil {
		return nil, err
	}

	return header, nil
}
