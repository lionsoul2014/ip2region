// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// original source ip editor

package xdb

import (
	"container/list"
	"fmt"
	"os"
	"path/filepath"
	"time"
)

type Editor struct {
	// source ip file
	srcHandle *os.File
	dstHandle *os.File

	// region info
	// @Note: 2^32 items at most
	region map[uint32]string
	rIndex uint32

	// segments list
	segments *list.List
}

func NewEditor(srcFile string, dstFile string) (*Editor, error) {
	// check the src and dst file
	srcPath, err := filepath.Abs(srcFile)
	if err != nil {
		return nil, err
	}

	dstPath, err := filepath.Abs(dstFile)
	if err != nil {
		return nil, err
	}

	if srcPath == dstPath {
		return nil, fmt.Errorf("src_path(%s) = dst_path(%s)", srcFile, dstFile)
	}

	srcHandle, err := os.OpenFile(srcPath, os.O_RDONLY, 0600)
	if err != nil {
		return nil, err
	}

	dstHandle, err := os.OpenFile(dstPath, os.O_CREATE|os.O_RDWR, 0644)
	if err != nil {
		return nil, err
	}

	e := &Editor{
		srcHandle: srcHandle,
		dstHandle: dstHandle,

		region:   map[uint32]string{},
		rIndex:   uint32(0),
		segments: list.New(),
	}

	// load the segments
	if err = e.loadSegments(); err != nil {
		return nil, fmt.Errorf("failed load segments: %s", err)
	}

	return e, nil
}

// Load all the segments from the source file
func (e *Editor) loadSegments() error {
	var last *Segment = nil
	var tStart = time.Now()

	var err = IterateSegments(e.srcHandle, nil, func(sip uint32, eip uint32, region *string) error {
		var str = *region
		var seg = &Segment{
			StartIP: sip,
			EndIP:   eip,
			Region:  str,
		}

		// check the continuity of the data segment
		if last != nil {
			if last.EndIP+1 != seg.StartIP {
				return fmt.Errorf("discontinuous data segment: last.eip+1(%d) != seg.sip(%d, %s)", sip, eip, str)
			}
		}

		last = seg
		return nil
	})
	if err != nil {
		return fmt.Errorf("failed to load segments: %s", err)
	}

	fmt.Printf("all segments loaded, length: %d, elapsed: %s\n", e.segments.Len(), time.Since(tStart))
	return nil
}

func (e *Editor) Put(ip string) error {
	return nil
}

func (e *Editor) PutFile(src string) error {
	return nil
}

func (e *Editor) Save() error {
	return nil
}

func (e *Editor) Close() error {
	return nil
}
