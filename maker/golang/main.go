// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package main

import (
	"bufio"
	"fmt"
	"github.com/lionsoul2014/ip2region/maker/golang/xdb"
	"log"
	"os"
	"strings"
	"time"
)

func printHelp() {
	fmt.Printf("ip2region xdb maker\n")
	fmt.Printf("%s [command] [command options]\n", os.Args[0])
	fmt.Printf("Command: \n")
	fmt.Printf("  gen      generate the binary db file\n")
	fmt.Printf("  search   binary xdb search test\n")
	fmt.Printf("  bench    binary xdb bench test\n")
	fmt.Printf("  edit     edit the source ip data\n")
}

// Iterate the cli flags
func iterateFlags(cb func(key string, val string) error) error {
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
			return fmt.Errorf("missing = for args pair '%s'", r)
		}

		if err := cb(r[2:sIdx], r[sIdx+1:]); err != nil {
			return err
		}
	}

	return nil
}

func genDb() {
	var err error
	var srcFile, dstFile = "", ""
	var indexPolicy = xdb.VectorIndexPolicy
	var fErr = iterateFlags(func(key string, val string) error {
		switch key {
		case "src":
			srcFile = val
		case "dst":
			dstFile = val
		case "index":
			indexPolicy, err = xdb.IndexPolicyFromString(val)
			if err != nil {
				return fmt.Errorf("parse policy: %w", err)
			}
		default:
			return fmt.Errorf("undefine option `%s=%s`\n", key, val)
		}

		return nil
	})
	if fErr != nil {
		fmt.Printf("failed to parse flags: %s", fErr)
		return
	}

	if srcFile == "" || dstFile == "" {
		fmt.Printf("%s gen [command options]\n", os.Args[0])
		fmt.Printf("options:\n")
		fmt.Printf(" --src string    source ip text file path\n")
		fmt.Printf(" --dst string    destination binary xdb file path\n")
		return
	}

	// make the binary file
	tStart := time.Now()
	maker, err := xdb.NewMaker(indexPolicy, srcFile, dstFile)
	if err != nil {
		fmt.Printf("failed to create %s\n", err)
		return
	}

	err = maker.Init()
	if err != nil {
		fmt.Printf("failed Init: %s\n", err)
		return
	}

	err = maker.Start()
	if err != nil {
		fmt.Printf("failed Start: %s\n", err)
		return
	}

	err = maker.End()
	if err != nil {
		fmt.Printf("failed End: %s\n", err)
	}

	log.Printf("Done, elapsed: %s\n", time.Since(tStart))
}

func testSearch() {
	var err error
	var dbFile = ""
	var fErr = iterateFlags(func(key string, val string) error {
		if key == "db" {
			dbFile = val
		} else {
			return fmt.Errorf("undefined option '%s=%s'\n", key, val)
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
		fmt.Printf(" --db string    ip2region binary xdb file path\n")
		return
	}

	searcher, err := xdb.NewSearcher(dbFile)
	if err != nil {
		fmt.Printf("failed to create searcher with `%s`: %s\n", dbFile, err.Error())
		return
	}
	defer func() {
		searcher.Close()
		fmt.Printf("test program exited, thanks for trying\n")
	}()

	fmt.Println(`ip2region xdb search test program, commands:
loadIndex : load the vector index for search speedup.
clearIndex: clear the vector index.
quit      : exit the test program`)
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

		// command interception and execution
		if line == "loadIndex" {
			err = searcher.LoadVectorIndex()
			if err != nil {
				log.Fatalf("failed to load vector index: %s", err)
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

		ip, err := xdb.CheckIP(line)
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

func testBench() {
	var err error
	var dbFile, srcFile = "", ""
	var ignoreError = false
	var fErr = iterateFlags(func(key string, val string) error {
		switch key {
		case "db":
			dbFile = val
		case "src":
			srcFile = val
		case "ignore-error":
			if val == "true" || val == "1" {
				ignoreError = true
			} else if val == "false" || val == "0" {
				ignoreError = false
			} else {
				return fmt.Errorf("invalid value for ignore-error option, could be false/0 or true/1\n")
			}
		default:
			return fmt.Errorf("undefined option '%s=%s'\n", key, val)
		}
		return nil
	})
	if fErr != nil {
		fmt.Printf("failed to parse flags: %s", fErr)
		return
	}

	if dbFile == "" || srcFile == "" {
		fmt.Printf("%s bench [command options]\n", os.Args[0])
		fmt.Printf("options:\n")
		fmt.Printf(" --db string            ip2region binary xdb file path\n")
		fmt.Printf(" --src string           source ip text file path\n")
		fmt.Printf(" --ignore-error bool    keep going if bench failed\n")
		return
	}

	searcher, err := xdb.NewSearcher(dbFile)
	if err != nil {
		fmt.Printf("failed to create searcher with `%s`: %s\n", dbFile, err)
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

	var count, errCount, tStart = 0, 0, time.Now()
	var scanner = bufio.NewScanner(handle)
	scanner.Split(bufio.ScanLines)
	for scanner.Scan() {
		var l = strings.TrimSpace(strings.TrimSuffix(scanner.Text(), "\n"))
		var ps = strings.SplitN(l, "|", 3)
		if len(ps) != 3 {
			fmt.Printf("invalid ip segment line `%s`\n", l)
			return
		}

		sip, err := xdb.CheckIP(ps[0])
		if err != nil {
			fmt.Printf("check start ip `%s`: %s\n", ps[0], err)
			return
		}

		eip, err := xdb.CheckIP(ps[1])
		if err != nil {
			fmt.Printf("check end ip `%s`: %s\n", ps[1], err)
			return
		}

		if sip > eip {
			fmt.Printf("start ip(%s) should not be greater than end ip(%s)\n", ps[0], ps[1])
			return
		}

		fmt.Printf("try to bench segment: `%s`\n", l)
		mip := xdb.MidIP(sip, eip)
		for _, ip := range []uint32{sip, xdb.MidIP(sip, mip), mip, xdb.MidIP(mip, eip), eip} {
			fmt.Printf("|-try to bench ip '%s' ... ", xdb.Long2IP(ip))
			region, _, err := searcher.Search(ip)
			if err != nil {
				fmt.Printf("failed to search ip '%s': %s\n", xdb.Long2IP(ip), err)
				return
			}

			// check the region info
			count++
			if region != ps[2] {
				errCount++
				fmt.Printf(" --[Failed] (%s != %s)\n", region, ps[2])
				if ignoreError == false {
					return
				}
			} else {
				fmt.Printf(" --[Ok]\n")
			}
		}
	}

	fmt.Printf("Bench finished, {count: %d, failed: %d, took: %s}\n", count, errCount, time.Since(tStart))
}

func edit() {
	var err error
	var srcFile, dstFile = "", ""
	var fErr = iterateFlags(func(key string, val string) error {
		switch key {
		case "src":
			srcFile = val
		case "dst":
			dstFile = val
		default:
			return fmt.Errorf("undefined option '%s=%s'\n", key, val)
		}
		return nil
	})
	if fErr != nil {
		fmt.Printf("failed to parse flags: %s", fErr)
		return
	}

	if dstFile == "" || srcFile == "" {
		fmt.Printf("%s edit [command options]\n", os.Args[0])
		fmt.Printf("options:\n")
		fmt.Printf(" --src string    source ip text file path\n")
		fmt.Printf(" --dst string    destination source file path\n")
		return
	}

	fmt.Printf("init the editor from source @ `%s` ... \n", srcFile)
	editor, err := xdb.NewEditor(srcFile)
	if err != nil {
		fmt.Printf("failed to init editor: %s", err)
		return
	}

	var help = func() {
		fmt.Printf("command list: \n")
		fmt.Printf("  put [segment]   : put the specifield segment\n")
		fmt.Printf("  put_file [file] : put all the segments from the specified file\n")
		fmt.Printf("  save            : save all the changes to the destination source file\n")
		fmt.Printf("  exit            : exit the program\n")
		fmt.Printf("  help            : print this help menu\n")
	}

	help()
	var reader = bufio.NewReader(os.Stdin)
	for {
		fmt.Printf(">> ")
		line, err := reader.ReadString('\n')
		if err != nil {
			fmt.Printf("failed to read line from cli: %s\n", err)
			break
		}

		cmd := strings.TrimSpace(line)
		if cmd == "help" {
			help()
		} else if cmd == "exit" {
			break
		} else if cmd == "save" {
			err = editor.Save()
			if err != nil {
				fmt.Printf("failed to save the changes: %s", err)
				continue
			}
			fmt.Printf("Changes saved\n")
		} else if strings.HasPrefix(cmd, "put ") {
			seg := cmd[len("put "):]
			err = editor.Put(seg)
			if err != nil {
				fmt.Printf("failed to Put(%s): %s\n", seg, err)
				continue
			}
			fmt.Printf("Put(%s): Ok\n", seg)
		} else if strings.HasPrefix(cmd, "put_file ") {
			file := cmd[len("put_file "):]
			err = editor.PutFile(file)
			if err != nil {
				fmt.Printf("failed to PutFile(%s): %s\n", file, err)
				continue
			}
			fmt.Printf("PutFile(%s): Ok\n", file)
		}
	}
}

func main() {
	if len(os.Args) < 2 {
		printHelp()
		return
	}

	// set the log flag
	log.SetFlags(log.Ldate | log.Ltime | log.Lshortfile)
	switch strings.ToLower(os.Args[1]) {
	case "gen":
		genDb()
	case "search":
		testSearch()
	case "bench":
		testBench()
	case "edit":
		edit()
	default:
		printHelp()
	}
}
