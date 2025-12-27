package cmd

import (
	"fmt"
	"log/slog"
	"os"
	"time"

	"github.com/lionsoul2014/ip2region/maker/golang/xdb"
)

func Bench() {
	var err error
	var dbFile, srcFile, ipVersion, logLevel = "", "", "", ""
	var ignoreError = false
	var fErr = iterateFlags(func(key string, val string) error {
		switch key {
		case "db":
			dbFile = val
		case "src":
			srcFile = val
		case "version":
			ipVersion = val
		case "log-level":
			logLevel = val
		case "ignore-error":
			if val == "true" || val == "1" {
				ignoreError = true
			} else if val == "false" || val == "0" {
				ignoreError = false
			} else {
				return fmt.Errorf("invalid value for ignore-error option, could be false/0 or true/1")
			}
		default:
			return fmt.Errorf("undefined option '%s=%s'", key, val)
		}
		return nil
	})
	if fErr != nil {
		fmt.Printf("failed to parse flags: %s", fErr)
		return
	}

	if dbFile == "" || srcFile == "" {
		fmt.Printf("%s bench [command options]\n", os.Args[0])
		fmt.Printf("options:\n")
		fmt.Printf(" --db string            ip2region binary xdb file path\n")
		fmt.Printf(" --src string           source ip text file path\n")
		fmt.Printf(" --version string       IP version, options: ipv4/ipv6, specify this flag so you don't get confused \n")
		fmt.Printf(" --log-level string     set the log level, options: debug/info/warn/error\n")
		fmt.Printf(" --ignore-error bool    keep going if bench failed\n")
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

	searcher, err := xdb.NewSearcher(version, dbFile)
	if err != nil {
		fmt.Printf("failed to create searcher with `%s`: %s\n", dbFile, err)
		return
	}
	defer func() {
		searcher.Close()
	}()

	handle, err := os.OpenFile(srcFile, os.O_RDONLY, 0600)
	if err != nil {
		fmt.Printf("failed to open source text file: %s\n", err)
		return
	}

	defer handle.Close()

	var count, errCount, tStart = 0, 0, time.Now()
	slog.Info("Bench start", "xdbPath", dbFile, "srcPath", srcFile)
	var iErr = xdb.IterateSegments(handle, nil, nil, func(seg *xdb.Segment) error {
		var l = fmt.Sprintf("%d|%d|%s", seg.StartIP, seg.EndIP, seg.Region)
		slog.Debug("try to bench", "segment", l)
		// mip := xdb.IPMiddle(seg.StartIP, seg.EndIP)
		// for _, ip := range [][]byte{seg.StartIP, xdb.IPMiddle(seg.EndIP, mip), mip, xdb.IPMiddle(mip, seg.EndIP), seg.EndIP} {
		for _, ip := range [][]byte{seg.StartIP, seg.EndIP} {
			slog.Debug("|-try to bench", "ip", xdb.IP2String(ip))
			r, _, err := searcher.Search(ip)
			if err != nil {
				return fmt.Errorf("failed to search ip '%s': %s", xdb.IP2Long(ip), err)
			}

			// check the region info
			count++
			if r != seg.Region {
				errCount++
				slog.Error(" --[Failed] region not match", "src", r, "dst", seg.Region)
				if !ignoreError {
					return fmt.Errorf("")
				}
			} else {
				slog.Debug(" --[Ok]")
			}
		}
		return nil
	})
	if iErr != nil {
		fmt.Printf("%s", err)
		return
	}

	slog.Info("Bench finished", "count", count, "failed", errCount, "elapsed", time.Since(tStart))
}
