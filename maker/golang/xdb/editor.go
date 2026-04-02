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
	"sort"
)

type Editor struct {
	verison *Version

	// source ip file
	srcPath   string
	srcHandle *os.File
	toSave    bool

	// segments list
	segments *list.List
}

func NewEditor(version *Version, srcFile string) (*Editor, error) {
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
		verison:   version,
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
	var segments []*Segment
	var sorting = false

	_, _, iErr := IterateSegments(e.srcHandle, true, func(l string) {
		// do nothing here
	}, nil, func(seg *Segment) error {
		// version check
		if len(seg.StartIP) != e.verison.Bytes {
			return fmt.Errorf("invalid ip segment(%s expected)", e.verison.Name)
		}

		// check the order of the data segment
		// if err := seg.RightBehind(last); err != nil {
		if err := seg.After(last); err != nil {
			// return err
			// @Note: If the continuity is disrupted,
			// we will sort all these segments later.
			sorting = true
		}

		// e.segments.PushBack(seg)
		segments = append(segments, seg)
		last = seg
		return nil
	})
	if iErr != nil {
		return iErr
	}

	// check and do the sorting
	if sorting {
		sort.Slice(segments, func(i, j int) bool {
			return IPCompare(segments[i].StartIP, segments[j].StartIP) < 0
		})

		// open the to save
		e.toSave = true
	}

	// check and fill in the discontinuous segments
	// to Keep the entire data continuous.
	last = nil
	for _, seg := range segments {
		if err := seg.After(last); err != nil {
		}

		if last == nil {
			if IPCompare(seg.StartIP, e.verison.Min) > 0 {
				e.segments.PushBack(&Segment{
					StartIP: e.verison.Min,
					EndIP:   IPSubOne(seg.StartIP),
					Region:  "",
				})
			}
		} else if err := seg.RightBehind(last); err == nil {
			// Do nothing here since it just right behind the last
		} else if err := seg.After(last); err != nil {
			// segments overlap
			return fmt.Errorf("overlap checking: %w", err)
		} else {
			// push the padding segments
			e.segments.PushBack(&Segment{
				StartIP: IPAddOne(last.EndIP),
				EndIP:   IPSubOne(seg.StartIP),
				Region:  "",
			})
		}

		// push the current segment
		e.segments.PushBack(seg)

		// reset the last
		last = seg
	}

	// check and padding the tailing segmnet
	if back := e.segments.Back(); back != nil {
		if IPCompare(e.verison.Max, back.Value.(*Segment).EndIP) > 0 {
			e.segments.PushBack(&Segment{
				StartIP: IPAddOne(back.Value.(*Segment).EndIP),
				EndIP:   e.verison.Max,
				Region:  "",
			})
		}
	}

	segments = nil // let GC do it work
	return nil
}

func (e *Editor) NeedSave() bool {
	return e.toSave
}

func (e *Editor) SegLen() int {
	return e.segments.Len()
}

func (e *Editor) Slice(offset int, size int) []*Segment {
	var index = -1
	var out []*Segment
	var next *list.Element
	for ele := e.segments.Front(); ele != nil; ele = next {
		next = ele.Next()
		s, ok := ele.Value.(*Segment)
		if !ok {
			continue
		}

		// offset match
		index++
		if index < offset {
			continue
		}

		out = append(out, s)
		if len(out) >= size {
			break
		}
	}

	return out
}

func (e *Editor) Put(ip string) (int, int, error) {
	seg, err := SegmentFrom(ip)
	if err != nil {
		return 0, 0, err
	}

	return e.PutSegment(seg)
}

// PutSegment put the specified segment into the current segment list with
// the following position relationships.
// 1, A - fully contained like:
// StartIP------seg.StartIP--------seg.EndIP----EndIP
//
//	|------------------|
//
// 2, B - intersect like:
// StartIP------seg.StartIP------EndIP------|
//
//	|---------------------seg.EndIP
func (e *Editor) PutSegment(seg *Segment) (int, int, error) {
	var next *list.Element
	var eList []*list.Element
	var found, counter = false, 0
	for ele := e.segments.Front(); ele != nil; ele = next {
		next = ele.Next()
		s, ok := ele.Value.(*Segment)
		if !ok {
			// could this even be a case ?
			return 0, 0, fmt.Errorf("type error: ele not a Segment ptr")
		}

		counter++

		// found the related segment
		if found {
			// just keep going
		} else if IPCompare(seg.StartIP, s.StartIP) >= 0 &&
			IPCompare(seg.StartIP, s.EndIP) <= 0 {
			found = true
		} else {
			continue
		}

		eList = append(eList, ele)
		if IPCompare(seg.EndIP, s.EndIP) <= 0 {
			break
		}
	}

	if len(eList) == 0 {
		// could this even be a case ?
		return 0, 0, fmt.Errorf("failed to find the related segment")
	}

	// print for debug
	// for i, s := range eList {
	// 	fmt.Printf("ele %d: %s\n", i, s.Value.(*Segment))
	// }

	// segment split
	var sList []*Segment
	var head = eList[0].Value.(*Segment)
	if IPCompare(seg.StartIP, head.StartIP) > 0 {
		sList = append(sList, &Segment{
			StartIP: head.StartIP,
			EndIP:   IPSubOne(seg.StartIP),
			Region:  head.Region,
		})
	}

	// append the new segment
	sList = append(sList, seg)

	// check and do the tailing segment append
	var tail = eList[len(eList)-1].Value.(*Segment)
	if IPCompare(seg.EndIP, tail.EndIP) < 0 {
		sList = append(sList, &Segment{
			StartIP: IPAddOne(seg.EndIP),
			EndIP:   tail.EndIP,
			Region:  tail.Region,
		})
	}

	// print for debug
	// for i, s := range sList {
	// 	fmt.Printf("%d: %s\n", i, s)
	// }

	// delete all the in-range segments and
	var base *list.Element
	var oldRows, newRows = len(eList), len(sList)
	for _, ele := range eList {
		base = ele.Next()
		e.segments.Remove(ele)
	}

	// add all the new segments
	if base == nil {
		for _, s := range sList {
			e.segments.PushBack(s)
		}
	} else {
		for _, s := range sList {
			e.segments.InsertBefore(s, base)
		}
	}

	// open the to save flag
	e.toSave = true

	return oldRows, newRows, nil
}

func (e *Editor) PutFile(src string) (int, int, error) {
	handle, err := os.OpenFile(src, os.O_RDONLY, 0600)
	if err != nil {
		return 0, 0, err
	}

	var oldRows, newRows = 0, 0
	_, _, iErr := IterateSegments(handle, true, func(l string) {
		// do nothing here
	}, nil, func(seg *Segment) error {
		o, n, err := e.PutSegment(seg)
		if err == nil {
			oldRows += o
			newRows += n
		}

		return err
	})
	if iErr != nil {
		return oldRows, newRows, iErr
	}

	_ = handle.Close()
	return oldRows, newRows, nil
}

func (e *Editor) Save() error {
	// check the to-save flag
	if !e.toSave {
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

		// ignore the padded or empty segment
		if s.Region == "" {
			continue
		}

		// var l = s.String()
		// _, err = dstHandle.WriteString(fmt.Sprintf("%s\n", l))
		_, err = fmt.Fprintln(dstHandle, s.String())
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
