// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package cmd

import (
	"bufio"
	"encoding/binary"
	"fmt"
	"log/slog"
	"os"
	"strings"
	"time"

	"github.com/lionsoul2014/ip2region/maker/golang/xdb"
)

// xdb searcher test

func Search() {
	var err error
	var dbFile = ""
	var fErr = iterateFlags(func(key string, val string) error {
		if key == "db" {
			dbFile = val
		} else {
			return fmt.Errorf("undefined option '%s=%s'", key, val)
		}
		return nil
	})
	if fErr != nil {
		fmt.Printf("failed to parse flags: %s", fErr)
		return
	}

	if dbFile == "" {
		fmt.Printf("%s search [command options]\n", os.Args[0])
		fmt.Printf("options:\n")
		fmt.Printf(" --db string         ip2region binary xdb file path\n")
		return
	}

	// detect the version from the xdb header
	header, err := xdb.LoadXdbHeaderFromFile(dbFile)
	if err != nil {
		slog.Error("failed to load xdb header", "error", err)
		return
	}

	var version *xdb.Version = nil
	versionNo := binary.LittleEndian.Uint16(header[0:])
	if versionNo == 2 {
		// old xdb file
		version = xdb.IPv4
	} else if versionNo == 3 {
		ipNo := int(binary.LittleEndian.Uint16(header[16:]))
		if ipNo == xdb.IPv4.Id {
			version = xdb.IPv4
		} else if ipNo == xdb.IPv6.Id {
			version = xdb.IPv6
		} else {
			slog.Error("invalid ip version", "id", ipNo)
			return
		}
	} else {
		slog.Error("invalid xdb version", "versionNo", versionNo, "xdbFile", dbFile)
		return
	}

	searcher, err := xdb.NewSearcher(version, dbFile)
	if err != nil {
		fmt.Printf("failed to create searcher with `%s`: %s\n", dbFile, err.Error())
		return
	}
	defer func() {
		searcher.Close()
		fmt.Printf("test program exited, thanks for trying\n")
	}()

	fmt.Printf(`ip2region xdb search test program,
source xdb: %s (%s)
commands:
  loadIndex : load the vector index for search speedup.
  clearIndex: clear the vector index.
  quit      : exit the test program
`, dbFile, version.Name)
	reader := bufio.NewReader(os.Stdin)
	for {
		fmt.Print("ip2region>> ")
		str, err := reader.ReadString('\n')
		if err != nil {
			slog.Error("failed to read string", "error", err)
			return
		}

		line := strings.TrimSpace(strings.TrimSuffix(str, "\n"))
		if len(line) == 0 {
			continue
		}

		// command interception and execution
		if line == "loadIndex" {
			err = searcher.LoadVectorIndex()
			if err != nil {
				slog.Error("failed to load vector index", "error", err)
				return
			}
			fmt.Printf("vector index cached\n")
			continue
		} else if line == "clearIndex" {
			searcher.ClearVectorIndex()
			fmt.Printf("vector index cleared\n")
			continue
		} else if line == "quit" {
			break
		}

		ip, err := xdb.ParseIP(line)
		if err != nil {
			fmt.Printf("invalid ip address `%s`\n", line)
			continue
		}

		tStart := time.Now()
		region, ioCount, err := searcher.Search(ip)
		if err != nil {
			fmt.Printf("\x1b[0;31m{err:%s, iocount:%d}\x1b[0m\n", err.Error(), ioCount)
		} else {
			fmt.Printf("\x1b[0;32m{region:%s, iocount:%d, took:%s}\x1b[0m\n", region, ioCount, time.Since(tStart))
		}
	}
}
