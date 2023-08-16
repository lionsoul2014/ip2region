// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package cmd

import (
	"bufio"
	"fmt"
	"github.com/mitchellh/go-homedir"
	"github.com/spf13/cobra"
	"log"
	"os"
	"strings"
	"time"
)

var (
	dbFileSearch      string
	cachePolicySearch string
)

// searchCmd represents the search command
var searchCmd = &cobra.Command{
	Use:   "search",
	Short: "search input test",
	Run: func(cmd *cobra.Command, args []string) {
		searcher()
	},
}

func init() {
	rootCmd.AddCommand(searchCmd)

	searchCmd.Flags().StringVarP(&dbFileSearch, "db", "d", "", "ip2region binary xdb file path (required)")
	searchCmd.Flags().StringVarP(&cachePolicySearch, "cache-policy", "c", "vectorIndex", "cache policy (file|vectorIndex|content)")
	// Required flags
	_ = searchCmd.MarkFlagRequired("db")

}

func searcher() {
	dbPath, err := homedir.Expand(dbFileSearch)
	if err != nil {
		fmt.Printf("invalid xdb file path `%s`: %s", dbFileSearch, err)
		return
	}
	// create the searcher with the cache policy setting
	searcher, err := createSearcher(dbPath, "vectorIndex")
	if err != nil {
		fmt.Printf("failed to create searcher: %s\n", err.Error())
		return
	}
	defer func() {
		searcher.Close()
		fmt.Println("searcher test program exited, thanks for trying")
	}()

	fmt.Printf("ip2region xdb searcher test program, cachePolicy: %s \n type 'quit' to exit \n ", cachePolicySearch)
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
		region, err := searcher.SearchByStr(line)
		if err != nil {
			fmt.Printf("\x1b[0;31m{err: %s, ioCount: %d}\x1b[0m\n", err.Error(), searcher.GetIOCount())
		} else {
			fmt.Printf("\x1b[0;32m{region: %s, ioCount: %d, took: %s}\x1b[0m\n", region, searcher.GetIOCount(), time.Since(tStart))
		}
	}

}
