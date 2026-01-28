// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package cmd

import (
	"fmt"
	"log/slog"
	"os"
	"strconv"
	"time"

	"github.com/lionsoul2014/ip2region/maker/golang/xdb"
)

// source data process, sort, de-duplicate, merge

func Process() {
	var err error
	var srcFile, dstFile = "", ""
	var fieldList, logLevel = "", ""
	var clearBasedIndex = -1
	var clearValueEqual, clearValueExcept = "", ""
	var fErr = iterateFlags(func(key string, val string) error {
		switch key {
		case "src":
			srcFile = val
		case "dst":
			dstFile = val
		case "field-list":
			fieldList = val
		case "clear-based-index":
			num, err := strconv.Atoi(val)
			if err != nil {
				return fmt.Errorf("invalid clear-based-index '%s=%s', integer expected", key, val)
			}

			clearBasedIndex = num
		case "clear-value-equal":
			clearValueEqual = val
		case "clear-value-except":
			clearValueExcept = val
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
		fmt.Printf(" --src string                 source ip text file path\n")
		fmt.Printf(" --dst string                 target ip text file path\n")
		fmt.Printf(" --field-list string          field index list imploded with ',' eg: 0,1,2,3-6,7\n")
		fmt.Printf(" --clear-based-index integer  clear based index eg: 3\n")
		fmt.Printf(" --clear-value-equal string   clear value equal to the specified one\n")
		fmt.Printf(" --clear-value-except string  clear value except the specified one\n")
		fmt.Printf(" --log-level string           set the log level, options: debug/info/warn/error\n")
		return
	}

	if clearBasedIndex > -1 {
		if len(clearValueEqual) > 0 && len(clearValueExcept) > 0 {
			fmt.Print("Only one can be specified besides clear-value-equal and clear-value-except")
			return
		}

		if len(clearValueEqual) == 0 && len(clearValueExcept) == 0 {
			fmt.Print("At least one must be specified for clear-value-equal and clear-value-except")
			return
		}
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
	processor, err := xdb.NewProcessor(srcFile, dstFile, fields, clearBasedIndex, clearValueEqual, clearValueExcept)
	if err != nil {
		fmt.Printf("failed to create %s\n", err)
		return
	}

	err = processor.Init()
	if err != nil {
		fmt.Printf("failed Init: %s\n", err)
		return
	}

	slog.Info("Processing", "src", srcFile, "dst", dstFile, "fields", fields, "clearBasedIndex",
		clearBasedIndex, "clearValueEqual", clearValueEqual, "clearValueExcept", clearValueExcept, "logLevel", logLevel)
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
