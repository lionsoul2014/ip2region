package ip2region

import (
	"testing"
)

func TestGetLong(t *testing.T) {
	//region, err := New("../../../data/ip2region.db ")
	//if err != nil {
	//	t.Error(err)
	//}
	//region.BtreeSearch("222.46.20.201")
	//region.MemorySearch("222.46.20.201")

	//getLong([]byte("abcd"), 0)

}

func BenchmarkBtreeSearch(B *testing.B) {
	region, err := New("../../data/ip2region.db ")
	if err != nil {
		B.Error(err)
	}
	for i:=0;i<B.N;i++{
		region.BtreeSearch("222.46.20.201")
	}

}

func BenchmarkMemorySearch(B *testing.B) {
	region, err := New("../../data/ip2region.db ")
	if err != nil {
		B.Error(err)
	}
	for i:=0;i<B.N;i++{
		region.MemorySearch("251.1.12.1")
	}

}

func BenchmarkBinarySearch(B *testing.B) {
	region, err := New("../../data/ip2region.db ")
	if err != nil {
		B.Error(err)
	}
	for i:=0;i<B.N;i++{
		region.BinarySearch("222.46.20.201")
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
