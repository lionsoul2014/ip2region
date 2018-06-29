package ip2region

import (
	"testing"
)

func BenchmarkBtreeSearch(B *testing.B) {
	region, err := New("../../data/ip2region.db ")
	if err != nil {
		B.Error(err)
	}
	for i:=0;i<B.N;i++{
		region.BtreeSearch("127.0.0.1")
	}

}

func BenchmarkMemorySearch(B *testing.B) {
	region, err := New("../../data/ip2region.db ")
	if err != nil {
		B.Error(err)
	}
	for i:=0;i<B.N;i++{
		region.MemorySearch("127.0.0.1")
	}

}

func BenchmarkBinarySearch(B *testing.B) {
	region, err := New("../../data/ip2region.db ")
	if err != nil {
		B.Error(err)
	}
	for i:=0;i<B.N;i++{
		region.BinarySearch("127.0.0.1")
	}

}

func TestIp2long(t *testing.T) {
	ip, err := ip2long("127.0.0.1")
	if err != nil {
		t.Error(err)
	}
	if ip != 2130706433 {
		t.Error("result error")
	}
	t.Log(ip)
}
