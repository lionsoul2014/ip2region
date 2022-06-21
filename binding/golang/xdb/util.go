// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// ---
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/16

package xdb

import (
	"encoding/binary"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func CheckIP(ip string) (uint32, error) {
	var ps = strings.Split(ip, ".")
	if len(ps) != 4 {
		return 0, fmt.Errorf("invalid ip address `%s`", ip)
	}

	var buff = make([]byte, 4)
	for i, s := range ps {
		d, err := strconv.Atoi(s)
		if err != nil {
			return 0, fmt.Errorf("the %dth part `%s` is not an integer", i, s)
		}

		if d < 0 || d > 255 {
			return 0, fmt.Errorf("the %dth part `%s` should be an integer bettween 0 and 255", i, s)
		}

		buff[i] = byte(d)
	}

	// convert the ip to integer
	return binary.BigEndian.Uint32(buff), nil
}

func Long2IP(ip uint32) string {
	var buff = make([]string, 4)
	buff[0] = fmt.Sprintf("%d", (ip>>24)&0xFF)
	buff[1] = fmt.Sprintf("%d", (ip>>16)&0xFF)
	buff[2] = fmt.Sprintf("%d", (ip>>8)&0xFF)
	buff[3] = fmt.Sprintf("%d", (ip>>0)&0xFF)
	return strings.Join(buff, ".")
}

func MidIP(sip uint32, eip uint32) uint32 {
	return uint32((uint64(sip) + uint64(eip)) >> 1)
}

// LoadVectorIndex util function to load the vector index from the specified file handle
func LoadVectorIndex(handle *os.File) ([][]*VectorIndexBlock, error) {
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

	// decode the vector index blocks
	var vectorIndex = make([][]*VectorIndexBlock, VectorIndexRows)
	for r := 0; r < VectorIndexRows; r++ {
		vectorIndex[r] = make([]*VectorIndexBlock, VectorIndexCols)
		for c := 0; c < VectorIndexCols; c++ {
			offset := r*VectorIndexCols*VectorIndexSize + c*VectorIndexSize
			vectorIndex[r][c], err = VectorIndexBlockDecode(buff[offset:])
			if err != nil {
				return nil, fmt.Errorf("decode vector index at [%d][%d]: %w", r, c, err)
			}
		}
	}

	return vectorIndex, nil
}

// LoadVectorIndexFromFile load vector index from a specified file path
func LoadVectorIndexFromFile(dbFile string) ([][]*VectorIndexBlock, error) {
	handle, err := os.OpenFile(dbFile, os.O_RDONLY, 0600)
	if err != nil {
		return nil, fmt.Errorf("open xdb file `%s`: %w", dbFile, err)
	}

	return LoadVectorIndex(handle)
}

// LoadVectorIndexFromBuff load vector index from content buffer
func LoadVectorIndexFromBuff(cBuff []byte) ([][]*VectorIndexBlock, error) {
	var err error
	var vectorIndex = make([][]*VectorIndexBlock, VectorIndexRows)
	for r := 0; r < VectorIndexRows; r++ {
		vectorIndex[r] = make([]*VectorIndexBlock, VectorIndexCols)
		for c := 0; c < VectorIndexCols; c++ {
			offset := HeaderInfoLength + r*VectorIndexCols*VectorIndexSize + c*VectorIndexSize
			vectorIndex[r][c], err = VectorIndexBlockDecode(cBuff[offset:])
			if err != nil {
				return nil, fmt.Errorf("decode vector index at [%d][%d]: %w", r, c, err)
			}
		}
	}

	return vectorIndex, nil
}

// LoadHeader load the header info from the specified handle
func LoadHeader(handle *os.File) ([]byte, error) {
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

	return buff, nil
}

// LoadHeaderFromFile load header info from the specified db file path
func LoadHeaderFromFile(dbFile string) ([]byte, error) {
	handle, err := os.OpenFile(dbFile, os.O_RDONLY, 0600)
	if err != nil {
		return nil, fmt.Errorf("open xdb file `%s`: %w", dbFile, err)
	}

	return LoadHeader(handle)
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

	return LoadContent(handle)
}
