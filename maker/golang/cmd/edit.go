// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package cmd

import (
	"bufio"
	"fmt"
	"github.com/lionsoul2014/ip2region/maker/golang/xdb"
	"os"
	"regexp"
	"strings"
	"time"

	"github.com/spf13/cobra"
)

var srcFileEdit string

// editCmd represents the edit command
var editCmd = &cobra.Command{
	Use:   "edit",
	Short: "edit the source ip data",
	Run: func(cmd *cobra.Command, args []string) {
		edit()
	},
}

func init() {
	rootCmd.AddCommand(editCmd)

	editCmd.Flags().StringVarP(&srcFileEdit, "src", "s", "", "source ip text file path (required)")
	// Required flags
	_ = editCmd.MarkFlagRequired("src")
}

func edit() {
	rExp, err := regexp.Compile("\\s+")
	if err != nil {
		fmt.Printf("failed to compile regexp: %s\n", err)
		return
	}

	fmt.Printf("init the editor from source @ `%s` ... \n", srcFileEdit)
	var tStart = time.Now()
	editor, err := xdb.NewEditor(srcFileEdit)
	if err != nil {
		fmt.Printf("failed to init editor: %s", err)
		return
	}

	fmt.Printf("all segments loaded, length: %d, elapsed: %s\n", editor.SegLen(), time.Since(tStart))
	var help = func() {
		fmt.Println("command list:")
		fmt.Println("  put [segment]        : put the specifield $segment")
		fmt.Println("  put_file [file]      : put all the segments from the specified $file")
		fmt.Println("  list [offset] [size] : list the first $size segments start from $offset")
		fmt.Println("  save                 : save all the changes to the destination source file")
		fmt.Println("  quit                 : exit the program")
		fmt.Println("  help                 : print this help menu")
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
			fmt.Printf("all segments saved to %s\n", srcFileEdit)
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
