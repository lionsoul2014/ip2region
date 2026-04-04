// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// ---
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/16

package xdb

import (
	"bytes"
	"embed"
	"fmt"
	"io"
	"net"
	"os"
)

func ParseIP(ip string) ([]byte, error) {
	parsedIP := net.ParseIP(ip)
	if parsedIP == nil {
		return nil, fmt.Errorf("invalid ip address: %s", ip)
	}

	v4 := parsedIP.To4()
	if v4 != nil {
		return v4, nil
	}

	v6 := parsedIP.To16()
	if v6 != nil {
		return v6, nil
	}

	return nil, fmt.Errorf("invalid ip address: %s", ip)
}

func IP2String(ip []byte) string {
	return net.IP(ip[:]).String()
}

// IPCompare compares two IP addresses
// Returns: -1 if ip1 < ip2, 0 if ip1 == ip2, 1 if ip1 > ip2
func IPCompare(ip1, ip2 []byte) int {
	// for i := 0; i < len(ip1); i++ {
	// 	if ip1[i] < ip2[i] {
	// 		return -1
	// 	}
	// 	if ip1[i] > ip2[i] {
	// 		return 1
	// 	}
	// }
	// return 0
	return bytes.Compare(ip1, ip2)
}

func IPAddOne(ip []byte) []byte {
	var r = make([]byte, len(ip))
	copy(r, ip)
	for i := len(ip) - 1; i >= 0; i-- {
		r[i]++
		if r[i] != 0 { // No overflow
			break
		}
	}

	return r
}

func IPSubOne(ip []byte) []byte {
	var r = make([]byte, len(ip))
	copy(r, ip)
	for i := len(ip) - 1; i >= 0; i-- {
		if r[i] != 0 { // No borrow needed
			r[i]--
			break
		}
		r[i] = 0xFF // borrow from the next byte
	}

	return r
}

// IPSub Sub the spcecified two byte ip
func IPSub(sip, eip []byte) ([]byte, error) {
	if len(sip) != len(eip) {
		return []byte{}, fmt.Errorf("length of the two ips are not the same")
	}

	var carry uint16 = 0
	var result = make([]byte, len(sip)+1)

	for i := len(sip) - 1; i >= 0; i-- {
		sum := uint16(sip[i]) + uint16(eip[i]) + carry
		result[i+1] = byte(sum) // Store standard 8-bit result
		carry = sum >> 8        // Extract the 1-bit carry for the next byte
	}

	// check and append the carry
	if carry > 0 {
		result[0] = byte(carry)
		return result, nil
	} else {
		return result[1:], nil
	}
}

// IPHalf get the half value of an input byte ip
func IPHalf(ip []byte) []byte {
	var length = len(ip)
	var result = make([]byte, length)
	// Tracks the bit falling off from the previous byte
	var carry byte = 0

	for i := 0; i < length; i++ {
		// 1. Shift current byte right by 1
		// 2. Or (|) with the carry from the previous byte (shifted to the MSB position)
		result[i] = (ip[i] >> 1) | (carry << 7)

		// 3. Capture the Least Significant Bit (LSB) to use as carry for the next byte
		carry = ip[i] & 1
	}

	return result
}

// IPMiddle get the middle value of two input ip address
func IPMiddle(sip, eip []byte) ([]byte, error) {
	buf, err := IPSub(sip, eip)
	if err != nil {
		return []byte{}, fmt.Errorf("IPSub(%s, %s): %w", IP2String(sip), IP2String(eip), err)
	}

	return IPHalf(buf), nil
}

// Verify if the current Searcher could be used to search the specified xdb file.
// Why do we need this check ?
// The future features of the xdb impl may cause the current searcher not able to work properly.
//
// @Note: You Just need to check this ONCE when the service starts
// Or use another process (eg, A command) to check once Just to confirm the suitability.
func Verify(handle *os.File) error {
	header, err := LoadHeader(handle)
	if err != nil {
		return fmt.Errorf("loading header: %w", err)
	}

	// get the runtime ptr bytes
	runtimePtrBytes := 0
	switch header.Version {
	case Structure20:
		runtimePtrBytes = 4
	case Structure30:
		runtimePtrBytes = header.RuntimePtrBytes
	default:
		return fmt.Errorf("invalid version: %d", header.Version)
	}

	// 1, confirm the xdb file size.
	// to sure that the MaxFilePointer does no overflow
	stat, err := handle.Stat()
	if err != nil {
		return fmt.Errorf("file stat: %w", err)
	}

	maxFilePtr := int64(1<<(runtimePtrBytes*8) - 1)
	if stat.Size() > maxFilePtr {
		return fmt.Errorf("xdb file exceeds the maximum supported bytes: %d", maxFilePtr)
	}

	return nil
}

// VerifyFromFile check Verify for details
func VerifyFromFile(dbFile string) error {
	handle, err := os.OpenFile(dbFile, os.O_RDONLY, 0600)
	if err != nil {
		return fmt.Errorf("open xdb file `%s`: %w", dbFile, err)
	}
	defer handle.Close()

	return Verify(handle)
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
	defer handle.Close()

	header, err := LoadHeader(handle)
	if err != nil {
		return nil, err
	}

	return header, nil
}

// LoadHeaderFromBuff wrap the header info from the content buffer
func LoadHeaderFromBuff(cBuff []byte) (*Header, error) {
	return NewHeader(cBuff[0:HeaderInfoLength])
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
	defer handle.Close()

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
	defer handle.Close()

	cBuff, err := LoadContent(handle)
	if err != nil {
		return nil, err
	}

	return cBuff, nil
}

// LoadContentFromFS load the whole xdb binary from embed.FS
func LoadContentFromFS(fs embed.FS, filePath string) ([]byte, error) {
	file, err := fs.Open(filePath)
	if err != nil {
		return nil, fmt.Errorf("failed to open embedded file `%s`: %w", filePath, err)
	}
	defer file.Close()

	var cBuff []byte
	cBuff, err = io.ReadAll(file)
	if err != nil {
		return nil, fmt.Errorf("failed to read embedded file `%s`: %w", filePath, err)
	}

	return cBuff, nil
}
