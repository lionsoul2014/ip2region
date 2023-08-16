// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package cmd

import (
	"fmt"
	"github.com/lionsoul2014/ip2region/maker/golang/xdb"
	"log"
	"time"

	"github.com/spf13/cobra"
)

var (
	dstFileGen     string
	srcFileGen     string
	indexPolicyGen string
)

// genCmd represents the gen command
var genCmd = &cobra.Command{
	Use:   "gen",
	Short: "generate the binary db file",
	Run: func(cmd *cobra.Command, args []string) {
		genDb()
	},
}

func init() {
	rootCmd.AddCommand(genCmd)

	genCmd.Flags().StringVarP(&dstFileGen, "dst", "d", "", "destination binary xdb file path (required)")
	genCmd.Flags().StringVarP(&srcFileGen, "src", "s", "", "source ip text file path (required)")
	genCmd.Flags().StringVarP(&indexPolicyGen, "index-policy", "i", "vector", "generate index policy (vector|btree)")

	// Required flags
	_ = genCmd.MarkFlagRequired("dst")
	_ = genCmd.MarkFlagRequired("src")

}

func genDb() {
	indexPolicy, err := xdb.IndexPolicyFromString(indexPolicyGen)
	if err != nil {
		fmt.Println(err)
	}
	// make the binary file
	tStart := time.Now()
	maker, err := xdb.NewMaker(indexPolicy, srcFileGen, dstFileGen)
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
