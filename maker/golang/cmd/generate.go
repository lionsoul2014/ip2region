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

// script to do the xdb generate

func Generate() {
	var err error
	var srcFile, dstFile = "", ""
	var ipVersion, fieldList, logLevel = "", "", "info"
	var indexPolicy = xdb.VectorIndexPolicy
	var fErr = iterateFlags(func(key string, val string) error {
		switch key {
		case "src":
			srcFile = val
		case "dst":
			dstFile = val
		case "version":
			ipVersion = val
		case "log-level":
			logLevel = val
		case "field-list":
			fieldList = val
		case "index":
			indexPolicy, err = xdb.IndexPolicyFromString(val)
			if err != nil {
				return fmt.Errorf("parse policy: %w", err)
			}
		default:
			return fmt.Errorf("undefine option `%s=%s`", key, val)
		}

		return nil
	})
	if fErr != nil {
		fmt.Printf("failed to parse flags: %s", fErr)
		return
	}

	if srcFile == "" || dstFile == "" {
		fmt.Printf("%s gen [command options]\n", os.Args[0])
		fmt.Printf("options:\n")
		fmt.Printf(" --src string           source ip text file path\n")
		fmt.Printf(" --dst string           destination binary xdb file path\n")
		fmt.Printf(" --version string       IP version, options: ipv4/ipv6, specify this flag so you don't get confused \n")
		fmt.Printf(" --field-list string    field index list imploded with ',' eg: 0,1,2,3-6,7\n")
		fmt.Printf(" --log-level string     set the log level, options: debug/info/warn/error\n")
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
	maker, err := xdb.NewMaker(version, indexPolicy, srcFile, dstFile, fields)
	if err != nil {
		fmt.Printf("failed to create %s\n", err)
		return
	}

	slog.Info("Generating xdb with", "src", srcFile, "dst", dstFile, "logLevel", logLevel)
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

	slog.Info("make done", "elapsed", time.Since(tStart))
}
