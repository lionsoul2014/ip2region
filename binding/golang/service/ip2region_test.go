// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package service

import (
	"fmt"
	"sync"
	"sync/atomic"
	"testing"
	"time"

	"github.com/lionsoul2014/ip2region/binding/golang/xdb"
)

func TestConfigCreate(t *testing.T) {
	v4Config, err := NewV4Config(VIndexCache, "../../../data/ip2region_v4.xdb", 10)
	if err != nil {
		t.Fatalf("failed to create v4 config: %s", err)
	}

	v6Config, err := NewV6Config(VIndexCache, "../../../data/ip2region_v6.xdb", 10)
	if err != nil {
		t.Fatalf("failed to create v6 config: %s", err)
	}

	ip2region, err := NewIp2Region(v4Config, v6Config)
	if err != nil {
		t.Fatalf("failed to create ip2region service: %s", err)
	}

	v4Bytes, err := xdb.ParseIP("219.133.110.197")
	if err != nil {
		t.Fatal("invalid ipv4 address")
	}

	v6Bytes, err := xdb.ParseIP("240e:3b7:3275:f090:d2a3:7d1a:dd90:c3b6")
	if err != nil {
		t.Fatalf("invalid ipv6 address")
	}

	for i := 0; i < 20; i++ {
		v4Bytes = xdb.IPAddOne(v4Bytes)
		v6Bytes = xdb.IPAddOne(v6Bytes)
		v4Region, err := ip2region.Search(v4Bytes)
		if err != nil {
			t.Fatalf("failed to search(%s): %s", xdb.IP2String(v4Bytes), err)
		}

		v6Region, err := ip2region.Search(v6Bytes)
		if err != nil {
			t.Fatalf("failed to search(%s): %s", xdb.IP2String(v6Bytes), err)
		}

		fmt.Printf(
			"%2d->search(%s)=%s, search(%s)=%s\n",
			i, xdb.IP2String(v4Bytes), v4Region, xdb.IP2String(v6Bytes), v6Region,
		)
	}

	ip2region.Close()
	fmt.Print("ip2region closed gracefully")
}

func TestPathCreate(t *testing.T) {
	ip2region, err := NewIp2RegionWithPath("../../../data/ip2region_v4.xdb", "../../../data/ip2region_v6.xdb")
	if err != nil {
		t.Fatalf("failed to create ip2region with path: %s", err)
	}

	v4Bytes, err := xdb.ParseIP("219.133.110.197")
	if err != nil {
		t.Fatal("invalid ipv4 address")
	}

	v6Bytes, err := xdb.ParseIP("240e:3b7:3275:f090:d2a3:7d1a:dd90:c3b6")
	if err != nil {
		t.Fatalf("invalid ipv6 address")
	}

	for i := 0; i < 20; i++ {
		v4Bytes = xdb.IPAddOne(v4Bytes)
		v6Bytes = xdb.IPAddOne(v6Bytes)
		v4Region, err := ip2region.Search(v4Bytes)
		if err != nil {
			t.Fatalf("failed to search(%s): %s", xdb.IP2String(v4Bytes), err)
		}

		v6Region, err := ip2region.Search(v6Bytes)
		if err != nil {
			t.Fatalf("failed to search(%s): %s", xdb.IP2String(v6Bytes), err)
		}

		fmt.Printf(
			"%2d->search(%s)=%s, search(%s)=%s\n",
			i, xdb.IP2String(v4Bytes), v4Region, xdb.IP2String(v6Bytes), v6Region,
		)
	}

	ip2region.Close()
	fmt.Print("ip2region closed gracefully")
}

func TestInMemSearch(t *testing.T) {
	v4Config, err := NewV4Config(BufferCache, "../../../data/ip2region_v4.xdb", 10)
	if err != nil {
		t.Fatalf("failed to create v4 config: %s", err)
	}

	v6Config, err := NewV6Config(BufferCache, "../../../data/ip2region_v6.xdb", 10)
	if err != nil {
		t.Fatalf("failed to create v6 config: %s", err)
	}

	ip2region, err := NewIp2Region(v4Config, v6Config)
	if err != nil {
		t.Fatalf("failed to create ip2region service: %s", err)
	}

	v4Bytes, err := xdb.ParseIP("219.133.110.197")
	if err != nil {
		t.Fatal("invalid ipv4 address")
	}

	v6Bytes, err := xdb.ParseIP("240e:3b7:3275:f090:d2a3:7d1a:dd90:c3b6")
	if err != nil {
		t.Fatalf("invalid ipv6 address")
	}

	for i := 0; i < 20; i++ {
		v4Bytes = xdb.IPAddOne(v4Bytes)
		v6Bytes = xdb.IPAddOne(v6Bytes)
		v4Region, err := ip2region.Search(v4Bytes)
		if err != nil {
			t.Fatalf("failed to search(%s): %s", xdb.IP2String(v4Bytes), err)
		}

		v6Region, err := ip2region.Search(v6Bytes)
		if err != nil {
			t.Fatalf("failed to search(%s): %s", xdb.IP2String(v6Bytes), err)
		}

		fmt.Printf(
			"%2d->search(%s)=%s, search(%s)=%s\n",
			i, xdb.IP2String(v4Bytes), v4Region, xdb.IP2String(v6Bytes), v6Region,
		)
	}

	ip2region.Close()
	fmt.Print("ip2region closed gracefully")
}

func TestV4Only(t *testing.T) {
	v4Config, err := NewV4Config(NoCache, "../../../data/ip2region_v4.xdb", 10)
	if err != nil {
		t.Fatalf("failed to create v4 config: %s", err)
	}

	ip2region, err := NewIp2Region(v4Config, nil)
	if err != nil {
		t.Fatalf("failed to create ip2region service: %s", err)
	}

	v4Bytes, err := xdb.ParseIP("219.133.110.197")
	if err != nil {
		t.Fatal("invalid ipv4 address")
	}

	v6Bytes, err := xdb.ParseIP("240e:3b7:3275:f090:d2a3:7d1a:dd90:c3b6")
	if err != nil {
		t.Fatalf("invalid ipv6 address")
	}

	for i := 0; i < 10; i++ {
		v4Bytes = xdb.IPAddOne(v4Bytes)
		v6Bytes = xdb.IPAddOne(v6Bytes)
		v4Region, err := ip2region.Search(v4Bytes)
		if err != nil {
			t.Fatalf("failed to search(%s): %s", xdb.IP2String(v4Bytes), err)
		}

		v6Region, err := ip2region.Search(v6Bytes)
		if err != nil {
			t.Fatalf("failed to search(%s): %s", xdb.IP2String(v6Bytes), err)
		}

		fmt.Printf(
			"%2d->search(%s)=%s, search(%s)=%s\n",
			i, xdb.IP2String(v4Bytes), v4Region, xdb.IP2String(v6Bytes), v6Region,
		)
	}

	ip2region.Close()
	fmt.Print("ip2region closed gracefully")
}

func TestConcurrentCall(t *testing.T) {
	v4Config, err := NewV4Config(VIndexCache, "../../../data/ip2region_v4.xdb", 15)
	if err != nil {
		t.Fatalf("failed to create v4 config: %s", err)
	}

	v6Config, err := NewV6Config(VIndexCache, "../../../data/ip2region_v6.xdb", 15)
	if err != nil {
		t.Fatalf("failed to create v6 config: %s", err)
	}

	v4Bytes, err := xdb.ParseIP("219.133.110.197")
	if err != nil {
		t.Fatal("invalid ipv4 address")
	}

	v6Bytes, err := xdb.ParseIP("240e:3b7:3275:f090:d2a3:7d1a:dd90:c3b6")
	if err != nil {
		t.Fatalf("invalid ipv6 address")
	}

	fmt.Printf("v4Config: %s\n", v4Config)
	fmt.Printf("v6Config: %s\n", v6Config)
	ip2region, err := NewIp2Region(v4Config, v6Config)
	if err != nil {
		t.Fatalf("failed to create ip2region service: %s", err)
	}

	coroutines := 100
	var wg sync.WaitGroup
	var count int64 = 0
	tStart := time.Now()
	for i := 0; i < coroutines; i++ {
		wg.Add(1)
		go func() {
			var ipBytes []byte
			for i := 0; i < 5000; i++ {
				if i%2 == 0 {
					ipBytes = v4Bytes
				} else {
					ipBytes = v6Bytes
				}

				region, err := ip2region.Search(ipBytes)
				if err != nil {
					fmt.Printf("Error: failed to search(%s): %s", xdb.IP2String(ipBytes), err)
					break
				}

				if l := len(ipBytes); l == 4 {
					if region != "中国|广东省|深圳市|电信" {
						fmt.Print("Error: region not equals")
						break
					}
				} else {
					if region != "中国|广东省|深圳市|家庭宽带" {
						fmt.Print("Error: region not equals")
						break
					}
				}

				atomic.AddInt64(&count, 1)
			}

			// mark all searches finished for this coroutine
			wg.Done()
		}()
	}

	// wait for all the searches to finished
	wg.Wait()
	costs := time.Since(tStart)
	fmt.Printf("%d searches finished in %s, avg took: %s\n", count, costs, costs/time.Duration(count))
	ip2region.Close()
	fmt.Print("ip2region closed gracefully")
}
