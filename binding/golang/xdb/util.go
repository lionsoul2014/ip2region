package xdb

import (
	"encoding/binary"
	"fmt"
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
