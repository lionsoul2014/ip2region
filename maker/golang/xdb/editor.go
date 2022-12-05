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
	toSave    bool

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
		toSave:    false,
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

func (e *Editor) NeedSave() bool {
	return e.toSave
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

// PutSegment put the specified segment into the current segment list with
// the following position relationships.
// 1, fully contained like:
// StartIP------seg.StartIP--------seg.EndIP----EndIP
//                 |------------------|
// 2, intersect like:
// StartIP------seg.StartIP------EndIP------|
//                 |---------------------seg.EndIP
//
func (e *Editor) PutSegment(seg *Segment) error {
	var tOne *list.Element
	var next *list.Element
	for ele := e.segments.Front(); ele != nil; ele = next {
		next = ele.Next()
		s, ok := ele.Value.(*Segment)
		if !ok {
			// could this even be a case ?
			continue
		}

		// find the related segment
		if seg.StartIP >= s.StartIP && seg.StartIP <= s.EndIP {
			tOne = ele
			break
		}
	}

	if tOne == nil {
		// could this even be a case ?
		// if the loaded segments contains all the segments we have
		// from 0 to 0xffffffff
		return fmt.Errorf("failed to find the related segment")
	}

	s, ok := tOne.Value.(*Segment)
	if !ok {
		return fmt.Errorf("internal error: invalid segment type")
	}

	fmt.Printf("tOne: %s\n", s)

	// open the to save flag
	e.toSave = true

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
	// check the to-save flag
	if e.toSave == false {
		return fmt.Errorf("nothing changed")
	}

	dstHandle, err := os.OpenFile(e.srcPath, os.O_WRONLY|os.O_TRUNC, 0644)
	if err != nil {
		return err
	}

	// loop and flush all the segments to the dstHandle
	var next *list.Element
	for ele := e.segments.Front(); ele != nil; ele = next {
		next = ele.Next()
		s, ok := ele.Value.(*Segment)
		if !ok {
			// could this even be a case ?
			continue
		}

		var l = s.String()
		_, err = dstHandle.WriteString(fmt.Sprintf("%s\n", l))
		if err != nil {
			return err
		}
	}

	// close the handle
	// and close the to-save flag
	_ = dstHandle.Close()
	e.toSave = false

	return nil
}

func (e *Editor) Close() {
	_ = e.srcHandle.Close()
}
