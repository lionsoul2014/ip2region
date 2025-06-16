// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package main

import (
	"bufio"
	"fmt"
	"github.com/lionsoul2014/ip2region/maker/golang/xdb"
	"log"
	"log/slog"
	"os"
	"regexp"
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

func applyLogLevel(logLevel string) error {
	// check and apply the log level
	var levelLog = slog.LevelInfo
	switch strings.ToLower(logLevel) {
	case "debug":
		levelLog = slog.LevelDebug
	case "info":
		levelLog = slog.LevelInfo
	case "warn":
		levelLog = slog.LevelWarn
	case "error":
		levelLog = slog.LevelError
	case "":
		// ignore the empty value
		// and default it to LevelInfo
	default:
		return fmt.Errorf("invalid log level %s", logLevel)
	}

	slog.SetLogLoggerLevel(levelLog)
	return nil
}

func genDb() {
	var err error
	var srcFile, dstFile = "", ""
	var fieldList, logLevel = "", "info"
	var indexPolicy = xdb.VectorIndexPolicy
	var fErr = iterateFlags(func(key string, val string) error {
		switch key {
		case "src":
			srcFile = val
		case "dst":
			dstFile = val
		case "log-level":
			logLevel = val
		case "field-list":
			fieldList = val
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
		fmt.Printf(" --src string           source ip text file path\n")
		fmt.Printf(" --dst string           destination binary xdb file path\n")
		fmt.Printf(" --field-list string    field index list imploded with ',' eg: 0,1,2,3-6,7\n")
		fmt.Printf(" --log-level string     set the log level, options: debug/info/warn/error\n")
		return
	}

	// check and apply the log level
	err = applyLogLevel(logLevel)
	if err != nil {
		slog.Error("failed to apply log level", "error", err)
		return
	}

	slog.Info("field-list", "value", fieldList)

	// make the binary file
	tStart := time.Now()
	maker, err := xdb.NewMaker(indexPolicy, srcFile, dstFile)
	if err != nil {
		fmt.Printf("failed to create %s\n", err)
		return
	}

	slog.Info("Generating xdb with", "src", srcFile, "dst", dstFile, "logLevel", logLevel)
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

	slog.Info("make done", "elapsed", time.Since(tStart))
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
	var dbFile, srcFile, logLevel = "", "", ""
	var ignoreError = false
	var fErr = iterateFlags(func(key string, val string) error {
		switch key {
		case "db":
			dbFile = val
		case "src":
			srcFile = val
		case "log-level":
			logLevel = val
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
		fmt.Printf(" --log-level string     set the log level, options: debug/info/warn/error\n")
		fmt.Printf(" --ignore-error bool    keep going if bench failed\n")
		return
	}

	// check and apply the log level
	err = applyLogLevel(logLevel)
	if err != nil {
		slog.Error("failed to apply log level", "error", err)
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
	slog.Info("Bench start", "xdbPath", dbFile, "srcPath", srcFile)
	var iErr = xdb.IterateSegments(handle, nil, func(seg *xdb.Segment) error {
		var l = fmt.Sprintf("%d|%d|%s", seg.StartIP, seg.EndIP, seg.Region)
		slog.Debug("try to bench", "segment", l)
		mip := xdb.MidIP(seg.StartIP, seg.EndIP)
		for _, ip := range []uint32{seg.StartIP, xdb.MidIP(seg.EndIP, mip), mip, xdb.MidIP(mip, seg.EndIP), seg.EndIP} {
			slog.Debug("|-try to bench", "ip", xdb.Long2IP(ip))
			r, _, err := searcher.Search(ip)
			if err != nil {
				return fmt.Errorf("failed to search ip '%s': %s\n", xdb.Long2IP(ip), err)
			}

			// check the region info
			count++
			if r != seg.Region {
				errCount++
				slog.Error(" --[Failed] region not match", "src", r, "dst", seg.Region)
				if ignoreError == false {
					return fmt.Errorf("")
				}
			} else {
				slog.Debug(" --[Ok]")
			}
		}
		return nil
	})
	if iErr != nil {
		fmt.Printf("%s", err)
		return
	}

	slog.Info("Bench finished", "count", count, "failed", errCount, "elapsed", time.Since(tStart))
}

func edit() {
	var err error
	var srcFile = ""
	var fErr = iterateFlags(func(key string, val string) error {
		switch key {
		case "src":
			srcFile = val
		default:
			return fmt.Errorf("undefined option '%s=%s'\n", key, val)
		}
		return nil
	})
	if fErr != nil {
		fmt.Printf("failed to parse flags: %s", fErr)
		return
	}

	if srcFile == "" {
		fmt.Printf("%s edit [command options]\n", os.Args[0])
		fmt.Printf("options:\n")
		fmt.Printf(" --src string    source ip text file path\n")
		return
	}

	rExp, err := regexp.Compile("\\s+")
	if err != nil {
		fmt.Printf("failed to compile regexp: %s\n", err)
		return
	}

	fmt.Printf("init the editor from source @ `%s` ... \n", srcFile)
	var tStart = time.Now()
	editor, err := xdb.NewEditor(srcFile)
	if err != nil {
		fmt.Printf("failed to init editor: %s", err)
		return
	}

	fmt.Printf("all segments loaded, length: %d, elapsed: %s\n", editor.SegLen(), time.Since(tStart))
	var help = func() {
		fmt.Printf("command list: \n")
		fmt.Printf("  put [segment]        : put the specifield $segment\n")
		fmt.Printf("  put_file [file]      : put all the segments from the specified $file\n")
		fmt.Printf("  list [offset] [size] : list the first $size segments start from $offset\n")
		fmt.Printf("  save                 : save all the changes to the destination source file\n")
		fmt.Printf("  quit                 : exit the program\n")
		fmt.Printf("  help                 : print this help menu\n")
	}

	help()
	var sTip = ""
	var reader = bufio.NewReader(os.Stdin)
	for {
		if editor.NeedSave() {
			sTip = "*"
		} else {
			sTip = ""
		}

		fmt.Printf("%seditor>> ", sTip)
		line, err := reader.ReadString('\n')
		if err != nil {
			fmt.Printf("failed to read line from cli: %s\n", err)
			break
		}

		cmd := strings.TrimSpace(line)
		if cmd == "help" {
			help()
		} else if cmd == "quit" {
			if editor.NeedSave() {
				fmt.Printf("there are changes that need to save, type 'quit!' to force quit\n")
			} else {
				break
			}
		} else if cmd == "quit!" {
			// quit directly
			break
		} else if cmd == "save" {
			err = editor.Save()
			if err != nil {
				fmt.Printf("failed to save the changes: %s\n", err)
				continue
			}
			fmt.Printf("all segments saved to %s\n", srcFile)
		} else if strings.HasPrefix(cmd, "list") {
			var sErr error
			off, size, l := 0, 10, len("list")
			str := strings.TrimSpace(cmd)
			if len(str) > l {
				sets := rExp.Split(cmd, 3)
				switch len(sets) {
				case 2:
					_, sErr = fmt.Sscanf(cmd, "%s %d", &str, &off)
				case 3:
					_, sErr = fmt.Sscanf(cmd, "%s %d %d", &str, &off, &size)
				}
			}

			if sErr != nil {
				fmt.Printf("failed to parse the offset and size: %s\n", sErr)
				continue
			}

			fmt.Printf("+-slice(%d,%d): \n", off, size)
			for _, s := range editor.Slice(off, size) {
				fmt.Printf("%s\n", s)
			}
		} else if strings.HasPrefix(cmd, "put ") {
			seg := strings.TrimSpace(cmd[len("put "):])
			o, n, err := editor.Put(seg)
			if err != nil {
				fmt.Printf("failed to Put(%s): %s\n", seg, err)
				continue
			}
			fmt.Printf("Put(%s): Ok, with %d deletes and %d additions\n", seg, o, n)
		} else if strings.HasPrefix(cmd, "put_file ") {
			file := strings.TrimSpace(cmd[len("put_file "):])
			o, n, err := editor.PutFile(file)
			if err != nil {
				fmt.Printf("failed to PutFile(%s): %s\n", file, err)
				continue
			}
			fmt.Printf("PutFile(%s): Ok, with %d deletes and %d additions\n", file, o, n)
		} else if len(cmd) > 0 {
			help()
		}
	}
}

func main() {
	if len(os.Args) < 2 {
		printHelp()
		return
	}

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
