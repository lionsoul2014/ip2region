// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package cmd

import (
	"fmt"
	"log/slog"
	"os"
	"time"

	"github.com/lionsoul2014/ip2region/maker/golang/xdb"
)

// source data process, sort, de-duplicate, merge

func Process() {
	var err error
	var srcFile, dstFile = "", ""
	var fieldList, logLevel = "", ""
	var fErr = iterateFlags(func(key string, val string) error {
		switch key {
		case "src":
			srcFile = val
		case "dst":
			dstFile = val
		case "field-list":
			fieldList = val
		case "log-level":
			logLevel = val
		default:
			return fmt.Errorf("undefined option '%s=%s'", key, val)
		}
		return nil
	})
	if fErr != nil {
		fmt.Printf("failed to parse flags: %s", fErr)
		return
	}

	if srcFile == "" || dstFile == "" {
		fmt.Printf("%s process [command options]\n", os.Args[0])
		fmt.Printf("options:\n")
		fmt.Printf(" --src string           source ip text file path\n")
		fmt.Printf(" --dst string           target ip text file path\n")
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

	fields, err := getFilterFields(fieldList)
	if err != nil {
		slog.Error("failed to get filter fields", "error", err)
		return
	}

	// make the binary file
	tStart := time.Now()
	processor, err := xdb.NewProcessor(srcFile, dstFile, fields)
	if err != nil {
		fmt.Printf("failed to create %s\n", err)
		return
	}

	err = processor.Init()
	if err != nil {
		fmt.Printf("failed Init: %s\n", err)
		return
	}

	slog.Info("Processing", "src", srcFile, "dst", dstFile, "logLevel", logLevel)
	err = processor.Start()
	if err != nil {
		fmt.Printf("failed Start: %s\n", err)
		return
	}

	err = processor.End()
	if err != nil {
		fmt.Printf("failed End: %s\n", err)
	}

	slog.Info("processor done", "elapsed", time.Since(tStart))
}
