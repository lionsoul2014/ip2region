// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// ---
// ip2region database v2.0 searcher.
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

const (
	HeaderInfoLength = 256
	VectorIndexRows  = 256
	VectorIndexCols  = 256
	VectorIndexSize  = 8
)

type Searcher struct {
	handle *os.File

	// header info
	header  []byte
	ioCount int

	// use it only when this feature enabled.
	// Preload the vector index will reduce the number of IO operations
	// thus speedup the search process
	vectorIndex [][]*VectorIndexBlock

	// content buffer.
	// running with the whole xdb file cached
	contentBuff []byte
}

func baseNew(dbFile string, vIndex [][]*VectorIndexBlock, cBuff []byte) (*Searcher, error) {
	var err error

	// content buff first
	if cBuff != nil {
		// check and autoload the vector index
		if vIndex == nil {
			vIndex, err = LoadVectorIndexFromBuff(cBuff)
			if err != nil {
				return nil, fmt.Errorf("load vector index from buff: %w", err)
			}
		}

		return &Searcher{
			vectorIndex: vIndex,
			contentBuff: cBuff,
		}, nil
	}

	// open the xdb binary file
	handle, err := os.OpenFile(dbFile, os.O_RDONLY, 0600)
	if err != nil {
		return nil, err
	}

	return &Searcher{
		handle:      handle,
		vectorIndex: vIndex,
	}, nil
}

func NewWithFileOnly(dbFile string) (*Searcher, error) {
	return baseNew(dbFile, nil, nil)
}

func NewWithVectorIndex(dbFile string, vIndex [][]*VectorIndexBlock) (*Searcher, error) {
	return baseNew(dbFile, vIndex, nil)
}

func NewWithBuffer(cBuff []byte) (*Searcher, error) {
	vIndex, err := LoadVectorIndexFromBuff(cBuff)
	if err != nil {
		return nil, fmt.Errorf("load vector index from buff: %w", err)
	}

	return baseNew("", vIndex, cBuff)
}

func (s *Searcher) Close() {
	if s.handle != nil {
		err := s.handle.Close()
		if err != nil {
			return
		}
	}
}

// GetIOCount return the global io count for the last search
func (s *Searcher) GetIOCount() int {
	return s.ioCount
}

// SearchByStr find the region for the specified ip string
func (s *Searcher) SearchByStr(str string) (string, error) {
	ip, err := CheckIP(str)
	if err != nil {
		return "", err
	}

	return s.Search(ip)
}

// Search find the region for the specified long ip
func (s *Searcher) Search(ip uint32) (string, error) {
	// reset the global ioCount
	s.ioCount = 0

	// locate the segment index block based on the vector index
	var vIndex *VectorIndexBlock
	if s.vectorIndex != nil {
		vIndex = s.vectorIndex[(ip>>24)&0xFF][(ip>>16)&0xFF]
	} else {
		l0, l1 := (ip>>24)&0xFF, (ip>>16)&0xFF
		offset := HeaderInfoLength + l0*VectorIndexCols*VectorIndexSize + l1*VectorIndexSize

		// read the vector index block
		var vIndexBuff = make([]byte, 8)
		err := s.read(int64(offset), vIndexBuff)
		vIndex, err = VectorIndexBlockDecode(vIndexBuff)
		if err != nil {
			return "", fmt.Errorf("read vector index block at %d: %w", offset, err)
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
		err := s.read(int64(p), buff)
		if err != nil {
			return "", fmt.Errorf("read segment index at %d: %w", p, err)
		}

		// decode the data step by step to reduce the unnecessary operations
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
	var regionBuff = make([]byte, dataLen)
	err := s.read(int64(dataPtr), regionBuff)
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
