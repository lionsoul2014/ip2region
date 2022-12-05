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
)

type Editor struct {
	// source ip file
	srcPath   string
	srcHandle *os.File

	// segments list
	segments *list.List
}

func NewEditor(srcFile string) (*Editor, error) {
	// check the src and dst file
	srcPath, err := filepath.Abs(srcFile)
	if err != nil {
		return nil, err
	}

	srcHandle, err := os.OpenFile(srcPath, os.O_RDONLY, 0600)
	if err != nil {
		return nil, err
	}

	e := &Editor{
		srcPath:   srcPath,
		srcHandle: srcHandle,
		segments:  list.New(),
	}

	// load the segments
	if err = e.loadSegments(); err != nil {
		return nil, fmt.Errorf("failed to load segments: %s", err)
	}

	return e, nil
}

// Load all the segments from the source file
func (e *Editor) loadSegments() error {
	var last *Segment = nil

	var iErr = IterateSegments(e.srcHandle, func(l string) {
		// do nothing here
	}, func(seg *Segment) error {
		// check the continuity of the data segment
		if err := seg.AfterCheck(last); err != nil {
			return err
		}

		e.segments.PushBack(seg)
		last = seg
		return nil
	})
	if iErr != nil {
		return iErr
	}

	return nil
}

func (e *Editor) SegLen() int {
	return e.segments.Len()
}

func (e *Editor) Put(ip string) error {
	seg, err := SegmentFrom(ip)
	if err != nil {
		return err
	}

	return e.PutSegment(seg)
}

func (e *Editor) PutSegment(seg *Segment) error {
	return nil
}

func (e *Editor) PutFile(src string) error {
	handle, err := os.OpenFile(src, os.O_RDONLY, 0600)
	if err != nil {
		return err
	}

	iErr := IterateSegments(handle, func(l string) {
		// do nothing here
	}, func(seg *Segment) error {
		return e.PutSegment(seg)
	})
	if iErr != nil {
		return iErr
	}

	_ = handle.Close()
	return nil
}

func (e *Editor) Save() error {
	dstHandle, err := os.OpenFile(e.srcPath, os.O_WRONLY|os.O_TRUNC, 0644)
	if err != nil {
		return err
	}

	// loop and flush all the segments to the dstHandle
	var next *list.Element
	for e := e.segments.Front(); e != nil; e = next {
		next = e.Next()
		s, ok := e.Value.(*Segment)
		if !ok {
			// could this even a case ?
			continue
		}

		var l = s.String()
		_, err = dstHandle.WriteString(fmt.Sprintf("%s\n", l))
		if err != nil {
			return err
		}
	}

	// close the handle
	_ = dstHandle.Close()

	return nil
}

func (e *Editor) Close() {
	_ = e.srcHandle.Close()
}
