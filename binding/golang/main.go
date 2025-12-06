// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// ---
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/16

package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/lionsoul2014/ip2region/binding/golang/service"
	"github.com/lionsoul2014/ip2region/binding/golang/xdb"
	"github.com/mitchellh/go-homedir"
)

func getXdbPath(fileName string) (string, error) {
	binPath, err := os.Executable()
	if err != nil {
		return "", fmt.Errorf("failed to get executale: %w", err)
	}

	xdbPath := filepath.Join(filepath.Dir(filepath.Dir(filepath.Dir(binPath))), "/data/", fileName)
	_, err = os.Stat(xdbPath)
	if err != nil {
		return "", nil
	}

	// fmt.Printf("xdbPath=%s\n", xdbPath)
	return xdbPath, nil
}

func createService(v4XdbPath string, v4CachePolicy string, v6XdbPath string, v6CachePolicy string) (*service.Ip2Region, error) {
	// try to create v4 config
	v4CPolicy, err := service.CachePolicyFromName(v4CachePolicy)
	if err != nil {
		return nil, fmt.Errorf("parse v4 cache policy: %w", err)
	}

	v4DbPath, err := homedir.Expand(v4XdbPath)
	if err != nil {
		return nil, fmt.Errorf("Expand(`%s`): %s", v4XdbPath, err)
	}
	v4Config, err := service.NewV4Config(v4CPolicy, v4DbPath, 1)
	if err != nil {
		return nil, fmt.Errorf("NewV4Config: %w", err)
	}

	// try to create v6 config
	v6Policy, err := service.CachePolicyFromName(v6CachePolicy)
	if err != nil {
		return nil, fmt.Errorf("parse v6 cache policy: %w", err)
	}

	v6DbPath, err := homedir.Expand(v6XdbPath)
	if err != nil {
		return nil, fmt.Errorf("Expand(`%s`): %s", v6XdbPath, err)
	}
	v6Config, err := service.NewV6Config(v6Policy, v6DbPath, 1)
	if err != nil {
		return nil, fmt.Errorf("NewV6Config: %w", err)
	}

	return service.NewIp2Region(v4Config, v6Config)
}

func createSearcher(dbPath string, cachePolicy string) (*xdb.Searcher, error) {
	handle, err := os.OpenFile(dbPath, os.O_RDONLY, 0600)
	if err != nil {
		return nil, fmt.Errorf("open xdb file `%s`: %w", dbPath, err)
	}

	defer handle.Close()

	// verify the xdb file
	// @Note: do NOT call it every time you create a searcher since this will slow down the search response.
	// @see the util.Verify function for details.
	err = xdb.Verify(handle)
	if err != nil {
		return nil, fmt.Errorf("xdb verify: %w", err)
	}

	// auto-detect the ip version from the xdb header
	header, err := xdb.LoadHeader(handle)
	if err != nil {
		return nil, fmt.Errorf("failed to load header from `%s`: %s", dbPath, err)
	}

	version, err := xdb.VersionFromHeader(header)
	if err != nil {
		return nil, fmt.Errorf("failed to detect IP version from `%s`: %s", dbPath, err)
	}

	switch cachePolicy {
	case "nil", "file":
		return xdb.NewWithFileOnly(version, dbPath)
	case "vectorIndex":
		vIndex, err := xdb.LoadVectorIndexFromFile(dbPath)
		if err != nil {
			return nil, fmt.Errorf("failed to load vector index from `%s`: %w", dbPath, err)
		}

		return xdb.NewWithVectorIndex(version, dbPath, vIndex)
	case "content":
		cBuff, err := xdb.LoadContentFromFile(dbPath)
		if err != nil {
			return nil, fmt.Errorf("failed to load content from '%s': %w", dbPath, err)
		}

		return xdb.NewWithBuffer(version, cBuff)
	default:
		return nil, fmt.Errorf("invalid cache policy `%s`, options: file/vectorIndex/content", cachePolicy)
	}
}

func printHelp() {
	fmt.Printf("ip2region xdb searcher\n")
	fmt.Printf("%s [command] [command options]\n", os.Args[0])
	fmt.Printf("Command: \n")
	fmt.Printf("  search    search input test\n")
	fmt.Printf("  bench     search bench test\n")
}

func testSearch() {
	var err error
	var help = ""
	var v4DbPath, v4CachePolicy = "", "vectorIndex"
	var v6DbPath, v6CachePolicy = "", "vectorIndex"
	for i := 2; i < len(os.Args); i++ {
		r := os.Args[i]
		if len(r) < 5 {
			continue
		}

		if strings.Index(r, "--") != 0 {
			continue
		}

		var key, val = "", ""
		var sIdx = strings.Index(r, "=")
		if sIdx < 0 {
			// fmt.Printf("missing = for args pair '%s'\n", r)
			// return
			key = r[2:]
		} else {
			key = r[2:sIdx]
			val = r[sIdx+1:]
		}

		switch key {
		case "help":
			if val == "" {
				help = "true"
			} else {
				help = val
			}
		case "v4-db":
			v4DbPath = val
		case "v4-cache-policy":
			v4CachePolicy = val
		case "v6-db":
			v6DbPath = val
		case "v6-cache-policy":
			v6CachePolicy = val
		default:
			fmt.Printf("undefined option `%s`\n", r)
			return
		}
	}

	// check and get the get the default v4 xdb path
	if v4DbPath == "" {
		v4DbPath, err = getXdbPath("ip2region_v4.xdb")
		if err != nil {
			fmt.Printf("failed to get v4 xdb path: %s", err)
			return
		}
	}

	// check and get the get the default v6 xdb path
	if v6DbPath == "" {
		v6DbPath, err = getXdbPath("ip2region_v6.xdb")
		if err != nil {
			fmt.Printf("failed to get v6 xdb path: %s", err)
			return
		}
	}

	if v4DbPath == "" || v6DbPath == "" || help == "true" {
		fmt.Printf("%s search [command options]\n", os.Args[0])
		fmt.Printf("options:\n")
		fmt.Printf(" --v4-db string            ip2region v4 binary xdb file path\n")
		fmt.Printf(" --v4-cache-policy string  v4 cache policy, default vectorIndex, options: file/vectorIndex/content\n")
		fmt.Printf(" --v6-db string            ip2region v6 binary xdb file path\n")
		fmt.Printf(" --v6-cache-policy string  v6 cache policy, default vectorIndex, options: file/vectorIndex/content\n")
		fmt.Printf(" --help                    print this help menu\n")
		return
	}

	// create the search service with the xdb paths and cache policies
	ip2region, err := createService(v4DbPath, v4CachePolicy, v6DbPath, v6CachePolicy)
	if err != nil {
		fmt.Printf("failed to create ip2region service: %s\n", err.Error())
		return
	}
	defer func() {
		ip2region.Close()
		fmt.Printf("searcher test program exited, thanks for trying\n")
	}()

	fmt.Printf(`ip2region search service test program
+-v4 db: %s (%s)
+-v6 db: %s (%s)
type 'quit' to exit
`, v4DbPath, v4CachePolicy, v6DbPath, v6CachePolicy)
	reader := bufio.NewReader(os.Stdin)
	for {
		fmt.Print("ip2region>> ")
		str, err := reader.ReadString('\n')
		if err != nil {
			log.Fatalf("failed to read string: %s", err)
		}

		line := strings.TrimSpace(strings.TrimSuffix(str, "\n"))
		if len(line) == 0 {
			continue
		}

		if line == "quit" {
			break
		}

		tStart := time.Now()
		region, err := ip2region.SearchByStr(line)
		if err != nil {
			fmt.Printf("\x1b[0;31m{err: %s}\x1b[0m\n", err.Error())
		} else {
			fmt.Printf("\x1b[0;32m{region: %s, took: %s}\x1b[0m\n", region, time.Since(tStart))
		}
	}
}

func testBench() {
	var err error
	var dbFile, srcFile, cachePolicy = "", "", "vectorIndex"
	for i := 2; i < len(os.Args); i++ {
		r := os.Args[i]
		if len(r) < 5 {
			continue
		}

		if strings.Index(r, "--") != 0 {
			continue
		}

		var sIdx = strings.Index(r, "=")
		if sIdx < 0 {
			fmt.Printf("missing = for args pair '%s'\n", r)
			return
		}

		switch r[2:sIdx] {
		case "db":
			dbFile = r[sIdx+1:]
		case "src":
			srcFile = r[sIdx+1:]
		case "cache-policy":
			cachePolicy = r[sIdx+1:]
		default:
			fmt.Printf("undefined option `%s`\n", r)
			return
		}
	}

	if dbFile == "" || srcFile == "" {
		fmt.Printf("%s bench [command options]\n", os.Args[0])
		fmt.Printf("options:\n")
		fmt.Printf(" --db string              ip2region binary xdb file path\n")
		fmt.Printf(" --src string             source ip text file path\n")
		fmt.Printf(" --cache-policy string    cache policy: file/vectorIndex/content\n")
		return
	}

	dbPath, err := homedir.Expand(dbFile)
	if err != nil {
		fmt.Printf("invalid xdb file path `%s`: %s", dbFile, err)
		return
	}

	searcher, err := createSearcher(dbPath, cachePolicy)
	if err != nil {
		fmt.Printf("failed to create searcher: %s\n", err.Error())
		return
	}
	defer func() {
		searcher.Close()
	}()

	handle, err := os.OpenFile(srcFile, os.O_RDONLY, 0600)
	if err != nil {
		fmt.Printf("failed to open source text file: %s\n", err)
		return
	}
	defer handle.Close()

	var count, tStart, costs = int64(0), time.Now(), int64(0)
	var scanner = bufio.NewScanner(handle)
	scanner.Split(bufio.ScanLines)
	for scanner.Scan() {
		var l = strings.TrimSpace(strings.TrimSuffix(scanner.Text(), "\n"))
		var ps = strings.SplitN(l, "|", 3)
		if len(ps) != 3 {
			fmt.Printf("invalid ip segment line `%s`\n", l)
			return
		}

		sip, err := xdb.ParseIP(ps[0])
		if err != nil {
			fmt.Printf("check start ip `%s`: %s\n", ps[0], err)
			return
		}

		eip, err := xdb.ParseIP(ps[1])
		if err != nil {
			fmt.Printf("check end ip `%s`: %s\n", ps[1], err)
			return
		}

		if xdb.IPCompare(sip, eip) > 0 {
			fmt.Printf("start ip(%s) should not be greater than end ip(%s)\n", ps[0], ps[1])
			return
		}

		for _, ip := range [][]byte{sip, eip} {
			sTime := time.Now()
			region, err := searcher.Search(ip)
			if err != nil {
				fmt.Printf("failed to search ip '%s': %s\n", xdb.IP2String(ip), err)
				return
			}

			costs += time.Since(sTime).Nanoseconds()

			// check the region info
			if region != ps[2] {
				fmt.Printf("failed Search(%s) with (%s != %s)\n", xdb.IP2String(ip), region, ps[2])
				return
			}

			count++
		}
	}

	cost := time.Since(tStart)
	fmt.Printf("Bench finished, {cachePolicy: %s, total: %d, took: %s, cost: %d μs/op}\n",
		cachePolicy, count, cost, costs/count/1000)
}

func main() {
	if len(os.Args) < 2 {
		printHelp()
		return
	}

	// set the log flag
	log.SetFlags(log.Ldate | log.Ltime | log.Lshortfile)
	switch strings.ToLower(os.Args[1]) {
	case "search":
		testSearch()
	case "bench":
		testBench()
	default:
		printHelp()
	}
}
