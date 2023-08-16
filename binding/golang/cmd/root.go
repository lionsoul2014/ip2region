// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package cmd

import (
	"fmt"
	"github.com/lionsoul2014/ip2region/binding/golang/xdb"
	"os"

	"github.com/spf13/cobra"
)

// rootCmd represents the base command when called without any subcommands
var rootCmd = &cobra.Command{
	Use:   "xdb_searcher",
	Short: "Go Implementation For Ip2region",
	Long: `Ip2region (2.0 - xdb) is a offline IP address manager framework and locator,
support billions of data segments, ten microsecond searching performance.`,
}

func createSearcher(dbPath string, cachePolicy string) (*xdb.Searcher, error) {
	switch cachePolicy {
	case "nil", "file":
		return xdb.NewWithFileOnly(dbPath)
	case "vectorIndex":
		vIndex, err := xdb.LoadVectorIndexFromFile(dbPath)
		if err != nil {
			return nil, fmt.Errorf("failed to load vector index from `%s`: %w", dbPath, err)
		}

		return xdb.NewWithVectorIndex(dbPath, vIndex)
	case "content":
		cBuff, err := xdb.LoadContentFromFile(dbPath)
		if err != nil {
			return nil, fmt.Errorf("failed to load content from '%s': %w", dbPath, err)
		}

		return xdb.NewWithBuffer(cBuff)
	default:
		return nil, fmt.Errorf("invalid cache policy `%s`, options: file/vectorIndex/content", cachePolicy)
	}
}

// Execute adds all child commands to the root command and sets flags appropriately.
// This is called by main.main(). It only needs to happen once to the rootCmd.
func Execute() {
	err := rootCmd.Execute()
	if err != nil {
		os.Exit(1)
	}
}
