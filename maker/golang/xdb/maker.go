// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// ----
// Ip2Region database v2.0 structure
//
// +----------------+-------------------+---------------+--------------+
// | header space   | speed up index    |  data payload | block index  |
// +----------------+-------------------+---------------+--------------+
// | 256 bytes      | 512 KiB (fixed)   | dynamic size  | dynamic size |
// +----------------+-------------------+---------------+--------------+
//
// 1. padding space : for header info like block index ptr, version, release date eg ... or any other temporary needs.
// -- 2bytes: version number, different version means structure update,
//			it fixed to 2 before IPv6 supporting, then updated to 3 since IPv6 supporting
// -- 2bytes: index algorithm code.
// -- 4bytes: generate unix timestamp (version)
// -- 4bytes: index block start ptr
// -- 4bytes: index block end ptr
// -- 2bytes: ip version number (4/6 since IPv6 supporting)
// -- 2bytes: runtime ptr bytes
//
//
// 2. data block: region or whatever data info.
// 3. segment index block: binary index block.
// 4. vector index block: fixed index info for block index search speedup.
// space structure table:
// -- 0   -> | 1rt super block | 2nd super block | 3rd super block | ... | 255th super block
// -- 1   -> | 1rt super block | 2nd super block | 3rd super block | ... | 255th super block
// -- 2   -> | 1rt super block | 2nd super block | 3rd super block | ... | 255th super block
// -- ...
// -- 255 -> | 1rt super block | 2nd super block | 3rd super block | ... | 255th super block
//
//
// super block structure:
// +-----------------------+----------------------+
// | first index block ptr | last index block ptr |
// +-----------------------+----------------------+
//
// data entry structure:
// +--------------------+-----------------------+
// | 2bytes (for desc)	| dynamic length        |
// +--------------------+-----------------------+
//  data length   whatever in bytes
//
// index entry structure
// +------------+-----------+---------------+------------+
// | 4bytes     | 4bytes    | 2bytes        | 4 bytes    |
// +------------+-----------+---------------+------------+
//  start ip 	  end ip	  data length     data ptr

package xdb

import (
	"encoding/binary"
	"fmt"
	"log/slog"
	"math"
	"os"
	"time"
)

const (
	VersionNo         = 3 // since 2025/09/01 (IPv6 supporting)
	HeaderInfoLength  = 256
	VectorIndexRows   = 256
	VectorIndexCols   = 256
	VectorIndexSize   = 8 // in bytes
	RuntimePtrSize    = 4 // in bytes
	VectorIndexLength = VectorIndexRows * VectorIndexCols * VectorIndexSize
)

type Maker struct {
	version *Version

	srcHandle *os.File
	dstHandle *os.File

	// self-define field index
	fields []int

	indexPolicy IndexPolicy
	segments    []*Segment
	regionPool  map[string]uint32
	vectorIndex []byte
}

func NewMaker(version *Version, policy IndexPolicy, srcFile string, dstFile string, fields []int) (*Maker, error) {
	// open the source file with READONLY mode
	srcHandle, err := os.OpenFile(srcFile, os.O_RDONLY, 0600)
	if err != nil {
		return nil, fmt.Errorf("open source file `%s`: %w", srcFile, err)
	}

	// open the destination file with Read/Write mode
	dstHandle, err := os.OpenFile(dstFile, os.O_RDWR|os.O_CREATE|os.O_TRUNC, 0666)
	if err != nil {
		return nil, fmt.Errorf("open target file `%s`: %w", dstFile, err)
	}

	return &Maker{
		version: version,

		srcHandle: srcHandle,
		dstHandle: dstHandle,

		// fields filter index
		fields: fields,

		indexPolicy: policy,
		segments:    []*Segment{},
		regionPool:  map[string]uint32{},
		vectorIndex: make([]byte, VectorIndexLength),
	}, nil
}

func (m *Maker) initDbHeader() error {
	slog.Info("try to init the db header ... ")

	_, err := m.dstHandle.Seek(0, 0)
	if err != nil {
		return err
	}

	// make and write the header space
	var header = make([]byte, 256)

	// 1, data version number
	binary.LittleEndian.PutUint16(header, uint16(VersionNo))

	// 2, index policy code
	binary.LittleEndian.PutUint16(header[2:], uint16(m.indexPolicy))

	// 3, generate unix timestamp
	binary.LittleEndian.PutUint32(header[4:], uint32(time.Now().Unix()))

	// 4, index block start ptr
	binary.LittleEndian.PutUint32(header[8:], uint32(0))

	// 5, index block end ptr
	binary.LittleEndian.PutUint32(header[12:], uint32(0))

	// 6, ip version
	binary.LittleEndian.PutUint16(header[16:], uint16(m.version.Id))

	// 7, runtime ptr bytes
	binary.LittleEndian.PutUint16(header[18:], uint16(RuntimePtrSize))

	_, err = m.dstHandle.Write(header)
	if err != nil {
		return err
	}

	return nil
}

func (m *Maker) loadSegments() error {
	slog.Info("try to load the segments ... ")
	var last *Segment = nil
	var tStart = time.Now()

	var iErr = IterateSegments(m.srcHandle, func(l string) {
		slog.Debug("loaded", "segment", l)
	}, func(region string) (string, error) {
		// apply the field filter
		return RegionFiltering(region, m.fields)
	}, func(seg *Segment) error {
		// ip version check
		if len(seg.StartIP) != m.version.Bytes {
			return fmt.Errorf("invalid ip segment(%s expected)", m.version.Name)
		}

		// check the continuity of the data segment
		if err := seg.AfterCheck(last); err != nil {
			return err
		}

		m.segments = append(m.segments, seg)
		last = seg
		return nil
	})
	if iErr != nil {
		return fmt.Errorf("failed to load segments: %s", iErr)
	}

	slog.Info("all segments loaded", "length", len(m.segments), "elapsed", time.Since(tStart))
	return nil
}

// Init the db binary file
func (m *Maker) Init() error {
	// init the db header
	err := m.initDbHeader()
	if err != nil {
		return fmt.Errorf("init db header: %w", err)
	}

	// load all the segments
	err = m.loadSegments()
	if err != nil {
		return fmt.Errorf("load segments: %w", err)
	}

	return nil
}

// refresh the vector index of the specified ip
func (m *Maker) setVectorIndex(ip []byte, ptr uint32) {
	var segIdxSize = uint32(m.version.SegmentIndexSize)
	var il0, il1 = int(ip[0]), int(ip[1])
	var idx = il0*VectorIndexCols*VectorIndexSize + il1*VectorIndexSize
	var sPtr = binary.LittleEndian.Uint32(m.vectorIndex[idx:])
	if sPtr == 0 {
		binary.LittleEndian.PutUint32(m.vectorIndex[idx:], ptr)
		binary.LittleEndian.PutUint32(m.vectorIndex[idx+4:], ptr+segIdxSize)
	} else {
		binary.LittleEndian.PutUint32(m.vectorIndex[idx+4:], ptr+segIdxSize)
	}
}

// Start to make the binary file
func (m *Maker) Start() error {
	if len(m.segments) < 1 {
		return fmt.Errorf("empty segment list")
	}

	// 1, write all the region/data to the binary file
	_, err := m.dstHandle.Seek(int64(HeaderInfoLength+VectorIndexLength), 0)
	if err != nil {
		return fmt.Errorf("seek to data first ptr: %w", err)
	}

	slog.Info("try to write the data block ... ")
	for _, seg := range m.segments {
		slog.Debug("try to write", "region", seg.Region)
		ptr, has := m.regionPool[seg.Region]
		if has {
			slog.Debug(" --[Cached]", "ptr=", ptr)
			continue
		}

		var region = []byte(seg.Region)
		if len(region) > 0xFFFF {
			return fmt.Errorf("too long region info `%s`: should be less than %d bytes", seg.Region, 0xFFFF)
		}

		// get the first ptr of the next region
		pos, err := m.dstHandle.Seek(0, 1)
		if err != nil {
			return fmt.Errorf("seek to current ptr: %w", err)
		}

		// @TODO: remove this if the long ptr operation were supported
		if pos >= math.MaxUint32 {
			return fmt.Errorf("region ptr exceed the max length of %d", math.MaxUint32)
		}

		_, err = m.dstHandle.Write(region)
		if err != nil {
			return fmt.Errorf("write region '%s': %w", seg.Region, err)
		}

		m.regionPool[seg.Region] = uint32(pos)
		slog.Debug(" --[Added] with", "ptr", pos)
	}

	// 2, write the index block and cache the super index block
	slog.Info("try to write the segment index block ... ")
	var indexBuff = make([]byte, m.version.SegmentIndexSize)
	var counter, startIndexPtr, endIndexPtr = 0, int64(-1), int64(-1)
	for _, seg := range m.segments {
		dataPtr, has := m.regionPool[seg.Region]
		if !has {
			return fmt.Errorf("missing ptr cache for region `%s`", seg.Region)
		}

		// @Note: data length should be the length of bytes.
		// this works fine because of the string feature (byte sequence) of golang.
		var dataLen = len(seg.Region)
		if dataLen < 1 {
			// @TODO: could this even be a case ?
			// 	return fmt.Errorf("empty region info for segment '%s'", seg)
			// Allow empty region info since 2024/09/24
		}

		var _offset = 0
		var segList = seg.Split()
		slog.Debug("try to index segment", "length", len(segList), "splits", seg.String())
		for _, s := range segList {
			pos, err := m.dstHandle.Seek(0, 1)
			if err != nil {
				return fmt.Errorf("seek to segment index block: %w", err)
			}

			// @TODO: remove this if the long ptr operation were supported
			if pos >= math.MaxUint32 {
				return fmt.Errorf("segment index ptr exceed the max length of %d", math.MaxUint32)
			}

			// encode the segment index.
			// @Note by Leon at 2025/09/05:
			// This is a tough decision since the directly copy of the bytes will make everything simpler.
			// But in order to compatible with the old searcher implementation we had to keep encoding the IPv4 bytes with little endian.
			// @TODO: we may choose to use the big-endian byte order in the future.
			// But now compatibility is the most important !!!

			m.version.PutBytes(indexBuff[0:], s.StartIP)
			m.version.PutBytes(indexBuff[len(s.StartIP):], s.EndIP)
			_offset = len(s.StartIP) + len(s.EndIP)
			binary.LittleEndian.PutUint16(indexBuff[_offset:], uint16(dataLen))
			binary.LittleEndian.PutUint32(indexBuff[_offset+2:], dataPtr)
			_, err = m.dstHandle.Write(indexBuff)
			if err != nil {
				return fmt.Errorf("write segment index for '%s': %w", s.String(), err)
			}

			slog.Debug("|-segment index", "counter", counter, "ptr", pos, "segment", s.String())
			m.setVectorIndex(s.StartIP, uint32(pos))
			counter++

			// check and record the start index ptr
			if startIndexPtr == -1 {
				startIndexPtr = pos
			}

			endIndexPtr = pos
		}
	}

	// synchronized the vector index block
	slog.Info("try to write the vector index block ... ")
	_, err = m.dstHandle.Seek(int64(HeaderInfoLength), 0)
	if err != nil {
		return fmt.Errorf("seek vector index first ptr: %w", err)
	}
	_, err = m.dstHandle.Write(m.vectorIndex)
	if err != nil {
		return fmt.Errorf("write vector index: %w", err)
	}

	// synchronized the segment index info
	slog.Info("try to write the segment index ptr ... ")
	binary.LittleEndian.PutUint32(indexBuff, uint32(startIndexPtr))
	binary.LittleEndian.PutUint32(indexBuff[4:], uint32(endIndexPtr))
	_, err = m.dstHandle.Seek(8, 0)
	if err != nil {
		return fmt.Errorf("seek segment index ptr: %w", err)
	}

	_, err = m.dstHandle.Write(indexBuff[:8])
	if err != nil {
		return fmt.Errorf("write segment index ptr: %w", err)
	}

	slog.Info("write done", "dataBlocks", len(m.regionPool), "indexBlocks", len(m.segments),
		"counter", counter, "startIndexPtr", startIndexPtr, "endIndexPtr", endIndexPtr)

	return nil
}

func (m *Maker) End() error {
	err := m.dstHandle.Close()
	if err != nil {
		return err
	}

	err = m.srcHandle.Close()
	if err != nil {
		return err
	}

	return nil
}
