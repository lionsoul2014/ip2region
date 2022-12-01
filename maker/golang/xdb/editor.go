// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// original source ip editor

package xdb

import "container/list"

type Editor struct {
	// source ip file
	srcFile string
	dstFile string

	// region info
	region map[int]string

	// segments list
	segments *list.List
}

func NewEditor(srcFile string, dstFile string) (*Editor, error) {
	e := &Editor{
		srcFile: srcFile,
		dstFile: dstFile,

		region:   map[int]string{},
		segments: list.New(),
	}
	return e, nil
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
