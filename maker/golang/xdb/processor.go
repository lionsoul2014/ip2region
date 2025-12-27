// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// original source ip processor

package xdb

import (
	"fmt"
	"log/slog"
	"os"
	"sort"
	"time"
)

type Processor struct {
	srcHandle *os.File
	dstHandle *os.File

	fields   []int
	segments []*Segment
}

func NewProcessor(srcFile string, dstFile string, fields []int) (*Processor, error) {
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

	return &Processor{
		srcHandle: srcHandle,
		dstHandle: dstHandle,

		// filter fields index
		fields: fields,

		segments: []*Segment{},
	}, nil
}

func (p *Processor) loadSegments() error {
	slog.Info("try to load the segments ... ")
	var tStart = time.Now()

	var iErr = IterateSegments(p.srcHandle, func(l string) {
		slog.Debug("loaded", "segment", l)
	}, func(region string) (string, error) {
		return RegionFiltering(region, p.fields)
	}, func(seg *Segment) error {
		// check the continuity of the data segment
		// if err := seg.AfterCheck(last); err != nil {
		// 	return err
		// }

		// slog.Info("filtered", "source", seg.Region, "filtered", region)
		p.segments = append(p.segments, seg)
		return nil
	})
	if iErr != nil {
		return fmt.Errorf("failed to load segments: %s", iErr)
	}

	slog.Info("all segments loaded", "length", len(p.segments), "elapsed", time.Since(tStart))
	return nil
}

// Init the db binary file
func (p *Processor) Init() error {
	// load all the segments
	err := p.loadSegments()
	if err != nil {
		return fmt.Errorf("load segments: %w", err)
	}

	return nil
}

func (p *Processor) Start() error {
	slog.Info("try to sort all the segments based on its start ip ...")
	sort.Slice(p.segments, func(i, j int) bool {
		return IPCompare(p.segments[i].StartIP, p.segments[j].StartIP) < 0
	})

	slog.Info("try to write all segments to target file ...")
	for _, seg := range p.segments {
		_, err := fmt.Fprintln(p.dstHandle, seg.String())
		if err != nil {
			return fmt.Errorf("write segment index for '%s': %w", seg.String(), err)
		}
	}

	slog.Info("process done", "segments", len(p.segments))
	return nil
}

func (p *Processor) End() error {
	err := p.dstHandle.Close()
	if err != nil {
		return err
	}

	err = p.srcHandle.Close()
	if err != nil {
		return err
	}

	return nil
}
