// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package cmd

import (
	"bufio"
	"fmt"
	"github.com/lionsoul2014/ip2region/binding/golang/xdb"
	"github.com/mitchellh/go-homedir"
	"os"
	"strings"
	"time"

	"github.com/spf13/cobra"
)

var (
	cachePolicyBench string
	dbFileBench      string
	srcFileBench     string
)

// benchCmd represents the bench command
var benchCmd = &cobra.Command{
	Use:   "bench",
	Short: "search bench test",
	Run: func(cmd *cobra.Command, args []string) {
		bench()
	},
}

func init() {
	rootCmd.AddCommand(benchCmd)

	benchCmd.Flags().StringVarP(&dbFileBench, "db", "d", "", "ip2region binary xdb file path (required)")
	benchCmd.Flags().StringVarP(&srcFileBench, "src", "s", "", "source ip text file path (required)")
	benchCmd.Flags().StringVarP(&cachePolicyBench, "cache-policy", "c", "vectorIndex", "cache policy (file|vectorIndex|content)")

	// Required flags
	_ = benchCmd.MarkFlagRequired("db")
	_ = benchCmd.MarkFlagRequired("src")
}

func bench() {
	dbPath, err := homedir.Expand(dbFileBench)
	if err != nil {
		fmt.Printf("invalid xdb file path `%s`: %s", dbFileBench, err)
		return
	}

	searcher, err := createSearcher(dbPath, cachePolicyBench)
	if err != nil {
		fmt.Printf("failed to create searcher: %s\n", err.Error())
		return
	}
	defer func() {
		searcher.Close()
	}()

	handle, err := os.OpenFile(srcFileBench, os.O_RDONLY, 0600)
	if err != nil {
		fmt.Printf("failed to open source text file: %s\n", err)
		return
	}

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

		mip := xdb.MidIP(sip, eip)
		for _, ip := range []uint32{sip, xdb.MidIP(sip, mip), mip, xdb.MidIP(mip, eip), eip} {
			sTime := time.Now()
			region, err := searcher.Search(ip)
			if err != nil {
				fmt.Printf("failed to search ip '%s': %s\n", xdb.Long2IP(ip), err)
				return
			}

			costs += time.Since(sTime).Nanoseconds()

			// check the region info
			if region != ps[2] {
				fmt.Printf("failed Search(%s) with (%s != %s)\n", xdb.Long2IP(ip), region, ps[2])
				return
			}

			count++
		}
	}

	cost := time.Since(tStart)
	fmt.Printf("Bench finished, {cachePolicy: %s, total: %d, took: %s, cost: %d μs/op}\n",
		cachePolicyBench, count, cost, costs/count/1000)
}
