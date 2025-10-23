// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package cmd

import (
	"bufio"
	"fmt"
	"log/slog"
	"os"
	"regexp"
	"strings"
	"time"

	"github.com/lionsoul2014/ip2region/maker/golang/xdb"
)

// source ip data editor

func Edit() {
	var err error
	var srcFile, ipVersion = "", ""
	var fErr = iterateFlags(func(key string, val string) error {
		switch key {
		case "src":
			srcFile = val
		case "version":
			ipVersion = val
		default:
			return fmt.Errorf("undefined option '%s=%s'", key, val)
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
		fmt.Printf(" --src string        source ip text file path\n")
		fmt.Printf(" --version string    IP version, options: ipv4/ipv6, specify this flag so you don't get confused \n")
		return
	}

	// check and define the IP version
	var version *xdb.Version = nil
	if len(ipVersion) < 2 {
		slog.Error("please specify the ip version with flag --version, ipv4 or ipv6 ?")
		return
	} else if v, err := xdb.VersionFromName(ipVersion); err != nil {
		slog.Error("failed to parse version name", "error", err)
		return
	} else {
		version = v
	}

	rExp, err := regexp.Compile(`\s+`)
	if err != nil {
		fmt.Printf("failed to compile regexp: %s\n", err)
		return
	}

	fmt.Printf("init the editor from source @ `%s` ... \n", srcFile)
	var tStart = time.Now()
	editor, err := xdb.NewEditor(version, srcFile)
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
