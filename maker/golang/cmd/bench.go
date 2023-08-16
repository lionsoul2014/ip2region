// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package cmd

import (
	"fmt"
	"github.com/lionsoul2014/ip2region/maker/golang/xdb"
	"os"
	"time"

	"github.com/spf13/cobra"
)

var (
	ignoreErrorBench bool
	dbFileBench      string
	srcFileBench     string
)

// benchCmd represents the bench command
var benchCmd = &cobra.Command{
	Use:   "bench",
	Short: "binary xdb bench test",
	Run: func(cmd *cobra.Command, args []string) {
		bench()
	},
}

func init() {
	rootCmd.AddCommand(benchCmd)

	benchCmd.Flags().StringVarP(&dbFileBench, "db", "d", "", "ip2region binary xdb file path (required)")
	benchCmd.Flags().StringVarP(&srcFileBench, "src", "s", "", "source ip text file path (required)")
	benchCmd.Flags().BoolVarP(&ignoreErrorBench, "ignore-error", "i", false, "keep going if bench failed (default \"false\")")

	// Required flags
	_ = benchCmd.MarkFlagRequired("db")
	_ = benchCmd.MarkFlagRequired("src")

}

func bench() {
	searcher, err := xdb.NewSearcher(dbFileBench)
	if err != nil {
		fmt.Printf("failed to create searcher with `%s`: %s\n", dbFileBench, err)
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

	var count, errCount, tStart = 0, 0, time.Now()
	var iErr = xdb.IterateSegments(handle, nil, func(seg *xdb.Segment) error {
		var l = fmt.Sprintf("%d|%d|%s", seg.StartIP, seg.EndIP, seg.Region)
		fmt.Printf("try to bench segment: `%s`\n", l)
		mip := xdb.MidIP(seg.StartIP, seg.EndIP)
		for _, ip := range []uint32{seg.StartIP, xdb.MidIP(seg.EndIP, mip), mip, xdb.MidIP(mip, seg.EndIP), seg.EndIP} {
			fmt.Printf("|-try to bench ip '%s' ... ", xdb.Long2IP(ip))
			r, _, err := searcher.Search(ip)
			if err != nil {
				return fmt.Errorf("failed to search ip '%s': %s\n", xdb.Long2IP(ip), err)
			}
			// check the region info
			count++
			if r != seg.Region {
				errCount++
				fmt.Printf(" --[Failed] (%s != %s)\n", r, seg.Region)
				if !ignoreErrorBench {
					return fmt.Errorf("")
				}
			} else {
				fmt.Println(" --[Ok]")
			}
		}
		return nil
	})
	if iErr != nil {
		fmt.Printf("%s", err)
		return
	}

	fmt.Printf("Bench finished, {count: %d, failed: %d, took: %s}\n", count, errCount, time.Since(tStart))
}
