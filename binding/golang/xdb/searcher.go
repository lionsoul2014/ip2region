// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// ---
// Ip2Region database v2.0 searcher.
// @Note this is a Not thread safe implementation.
//
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/16

package xdb

import (
	"encoding/binary"
	"fmt"
	"os"
)

type Searcher struct {
	version *Version
	handle  *os.File

	ioCount int

	// use it only when this feature enabled.
	// Preload the vector index will reduce the number of IO operations
	// thus speedup the search process
	vectorIndex []byte

	// content buffer.
	// running with the whole xdb file cached
	contentBuff []byte
}

func NewWithFileOnly(version *Version, dbFile string) (*Searcher, error) {
	return NewSearcher(version, dbFile, nil, nil)
}

func NewWithVectorIndex(version *Version, dbFile string, vIndex []byte) (*Searcher, error) {
	return NewSearcher(version, dbFile, vIndex, nil)
}

func NewWithBuffer(version *Version, cBuff []byte) (*Searcher, error) {
	return NewSearcher(version, "", nil, cBuff)
}

func NewSearcher(version *Version, dbFile string, vIndex []byte, cBuff []byte) (*Searcher, error) {
	var err error

	// content buff first
	if cBuff != nil {
		return &Searcher{
			version:     version,
			vectorIndex: nil,
			contentBuff: cBuff,
		}, nil
	}

	// open the xdb binary file
	handle, err := os.OpenFile(dbFile, os.O_RDONLY, 0600)
	if err != nil {
		return nil, err
	}

	return &Searcher{
		version:     version,
		handle:      handle,
		vectorIndex: vIndex,
	}, nil
}

func (s *Searcher) Close() {
	if s.handle != nil {
		err := s.handle.Close()
		if err != nil {
			// do error log here ?
		}
	}
}

// IPVersion return the ip version
func (s *Searcher) IPVersion() *Version {
	return s.version
}

// GetIOCount return the global io count for the last search
func (s *Searcher) GetIOCount() int {
	return s.ioCount
}

// Search the region for the specified string or bytes ip address
func (s *Searcher) Search(ip any) (string, error) {
	var err error
	var ipBytes []byte
	switch v := ip.(type) {
	case string:
		ipBytes, err = ParseIP(v)
		if err != nil {
			return "", fmt.Errorf("parse ip %s: %w", v, err)
		}
	case []byte:
		ipBytes = v
	default:
		return "", fmt.Errorf("invalid ip value type %s", v)
	}

	// ip version check
	if len(ipBytes) != s.version.Bytes {
		return "", fmt.Errorf("invalid ip address(%s expected)", s.version.Name)
	}

	// reset the global ioCount
	s.ioCount = 0

	// locate the segment index block based on the vector index
	var il0, il1 = int(ipBytes[0]), int(ipBytes[1])
	var idx = il0*VectorIndexCols*VectorIndexSize + il1*VectorIndexSize
	var sPtr, ePtr = uint32(0), uint32(0)
	if s.vectorIndex != nil {
		sPtr = binary.LittleEndian.Uint32(s.vectorIndex[idx:])
		ePtr = binary.LittleEndian.Uint32(s.vectorIndex[idx+4:])
	} else if s.contentBuff != nil {
		sPtr = binary.LittleEndian.Uint32(s.contentBuff[HeaderInfoLength+idx:])
		ePtr = binary.LittleEndian.Uint32(s.contentBuff[HeaderInfoLength+idx+4:])
	} else {
		// read the vector index block
		var buff = make([]byte, VectorIndexSize)
		err := s.read(int64(HeaderInfoLength+idx), buff)
		if err != nil {
			return "", fmt.Errorf("read vector index block at %d: %w", HeaderInfoLength+idx, err)
		}

		sPtr = binary.LittleEndian.Uint32(buff)
		ePtr = binary.LittleEndian.Uint32(buff[4:])
	}

	// fmt.Printf("sPtr=%d, ePtr=%d\n", sPtr, ePtr)
	// @Note: ptr validate, zero ptr means source data missing
	// so we could just stop here and return an empty string.
	if sPtr == 0 || ePtr == 0 {
		return "", nil
	}

	// binary search the segment index to get the region
	var bytes, dBytes = len(ipBytes), len(ipBytes) << 1
	var segIndexSize = uint32(s.version.SegmentIndexSize)
	var dataLen, dataPtr = 0, uint32(0)
	var buff = make([]byte, segIndexSize)
	var l, h = 0, int((ePtr - sPtr) / segIndexSize)
	for l <= h {
		m := (l + h) >> 1
		p := sPtr + uint32(m)*segIndexSize
		err := s.read(int64(p), buff)
		if err != nil {
			return "", fmt.Errorf("read segment index at %d: %w", p, err)
		}

		// decode the data step by step to reduce the unnecessary operations
		if s.version.IPCompare(ipBytes, buff[0:bytes]) < 0 {
			h = m - 1
		} else if s.version.IPCompare(ipBytes, buff[bytes:dBytes]) > 0 {
			l = m + 1
		} else {
			dataLen = int(binary.LittleEndian.Uint16(buff[dBytes:]))
			dataPtr = binary.LittleEndian.Uint32(buff[dBytes+2:])
			break
		}
	}

	// fmt.Printf("dataLen: %d, dataPtr: %d\n", dataLen, dataPtr)
	if dataLen == 0 {
		return "", nil
	}

	// load and return the region data
	var regionBuff = make([]byte, dataLen)
	err = s.read(int64(dataPtr), regionBuff)
	if err != nil {
		return "", fmt.Errorf("read region at %d: %w", dataPtr, err)
	}

	return string(regionBuff), nil
}

// do the data read operation based on the setting.
// content buffer first or will read from the file.
// this operation will invoke the Seek for file based read.
func (s *Searcher) read(offset int64, buff []byte) error {
	if s.contentBuff != nil {
		cLen := copy(buff, s.contentBuff[offset:])
		if cLen != len(buff) {
			return fmt.Errorf("incomplete read: readed bytes should be %d", len(buff))
		}
	} else {
		_, err := s.handle.Seek(offset, 0)
		if err != nil {
			return fmt.Errorf("seek to %d: %w", offset, err)
		}

		s.ioCount++
		rLen, err := s.handle.Read(buff)
		if err != nil {
			return fmt.Errorf("handle read: %w", err)
		}

		if rLen != len(buff) {
			return fmt.Errorf("incomplete read: readed bytes should be %d", len(buff))
		}
	}

	return nil
}
