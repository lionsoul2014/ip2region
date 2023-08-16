// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package cmd

import (
	"bufio"
	"fmt"
	"github.com/lionsoul2014/ip2region/maker/golang/xdb"
	"log"
	"os"
	"strings"
	"time"

	"github.com/spf13/cobra"
)

var dbFileSearch string

// searchCmd represents the search command
var searchCmd = &cobra.Command{
	Use:   "search",
	Short: "binary xdb search test",
	Run: func(cmd *cobra.Command, args []string) {
		search()
	},
}

func init() {
	rootCmd.AddCommand(searchCmd)

	searchCmd.Flags().StringVarP(&dbFileSearch, "db", "d", "", "ip2region binary xdb file path (required)")
	// Required flags
	_ = searchCmd.MarkFlagRequired("db")
}

func search() {
	searcher, err := xdb.NewSearcher(dbFileSearch)
	if err != nil {
		fmt.Printf("failed to create searcher with `%s`: %s\n", dbFileSearch, err.Error())
		return
	}
	defer func() {
		searcher.Close()
		fmt.Println("test program exited, thanks for trying")
	}()

	fmt.Println("ip2region xdb search test program, commands:")
	fmt.Println("loadIndex : load the vector index for search speedup.")
	fmt.Println("clearIndex: clear the vector index.")
	fmt.Println("quit      : exit the test program.")

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
