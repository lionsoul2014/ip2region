// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// ---
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/16

package xdb

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

var shiftIndex = []int{24, 16, 8, 0}

func CheckIP(ip string) (uint32, error) {
	var ps = strings.Split(strings.TrimSpace(ip), ".")
	if len(ps) != 4 {
		return 0, fmt.Errorf("invalid ip address `%s`", ip)
	}

	var val = uint32(0)
	for i, s := range ps {
		d, err := strconv.Atoi(s)
		if err != nil {
			return 0, fmt.Errorf("the %dth part `%s` is not an integer", i, s)
		}

		if d < 0 || d > 255 {
			return 0, fmt.Errorf("the %dth part `%s` should be an integer bettween 0 and 255", i, s)
		}

		val |= uint32(d) << shiftIndex[i]
	}

	// convert the ip to integer
	return val, nil
}

func Long2IP(ip uint32) string {
	return fmt.Sprintf("%d.%d.%d.%d", (ip>>24)&0xFF, (ip>>16)&0xFF, (ip>>8)&0xFF, ip&0xFF)
}

func MidIP(sip uint32, eip uint32) uint32 {
	return uint32((uint64(sip) + uint64(eip)) >> 1)
}

// LoadHeader load the header info from the specified handle
func LoadHeader(handle *os.File) (*Header, error) {
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

	return NewHeader(buff)
}

// LoadHeaderFromFile load header info from the specified db file path
func LoadHeaderFromFile(dbFile string) (*Header, error) {
	handle, err := os.OpenFile(dbFile, os.O_RDONLY, 0600)
	if err != nil {
		return nil, fmt.Errorf("open xdb file `%s`: %w", dbFile, err)
	}

	defer func(handle *os.File) {
		_ = handle.Close()
	}(handle)

	header, err := LoadHeader(handle)
	if err != nil {
		return nil, err
	}

	return header, nil
}

// LoadHeaderFromBuff wrap the header info from the content buffer
func LoadHeaderFromBuff(cBuff []byte) (*Header, error) {
	return NewHeader(cBuff[0:256])
}

// LoadVectorIndex util function to load the vector index from the specified file handle
func LoadVectorIndex(handle *os.File) ([]byte, error) {
	// load all the vector index block
	_, err := handle.Seek(HeaderInfoLength, 0)
	if err != nil {
		return nil, fmt.Errorf("seek to vector index: %w", err)
	}

	var buff = make([]byte, VectorIndexRows*VectorIndexCols*VectorIndexSize)
	rLen, err := handle.Read(buff)
	if err != nil {
		return nil, err
	}

	if rLen != len(buff) {
		return nil, fmt.Errorf("incomplete read: readed bytes should be %d", len(buff))
	}

	return buff, nil
}

// LoadVectorIndexFromFile load vector index from a specified file path
func LoadVectorIndexFromFile(dbFile string) ([]byte, error) {
	handle, err := os.OpenFile(dbFile, os.O_RDONLY, 0600)
	if err != nil {
		return nil, fmt.Errorf("open xdb file `%s`: %w", dbFile, err)
	}

	defer func() {
		_ = handle.Close()
	}()

	vIndex, err := LoadVectorIndex(handle)
	if err != nil {
		return nil, err
	}

	return vIndex, nil
}

// LoadContent load the whole xdb content from the specified file handle
func LoadContent(handle *os.File) ([]byte, error) {
	// get file size
	fi, err := handle.Stat()
	if err != nil {
		return nil, fmt.Errorf("stat: %w", err)
	}

	size := fi.Size()

	// seek to the head of the file
	_, err = handle.Seek(0, 0)
	if err != nil {
		return nil, fmt.Errorf("seek to get xdb file length: %w", err)
	}

	var buff = make([]byte, size)
	rLen, err := handle.Read(buff)
	if err != nil {
		return nil, err
	}

	if rLen != len(buff) {
		return nil, fmt.Errorf("incomplete read: readed bytes should be %d", len(buff))
	}

	return buff, nil
}

// LoadContentFromFile load the whole xdb content from the specified db file path
func LoadContentFromFile(dbFile string) ([]byte, error) {
	handle, err := os.OpenFile(dbFile, os.O_RDONLY, 0600)
	if err != nil {
		return nil, fmt.Errorf("open xdb file `%s`: %w", dbFile, err)
	}

	defer func() {
		_ = handle.Close()
	}()

	cBuff, err := LoadContent(handle)
	if err != nil {
		return nil, err
	}

	return cBuff, nil
}
