// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package main

import (
	"log"
	"os"
	"strings"

	"github.com/lionsoul2014/ip2region/maker/golang/cmd"
)

func main() {
	if len(os.Args) < 2 {
		cmd.PrintHelp()
		return
	}

	log.SetFlags(log.Ldate | log.Ltime | log.Lshortfile)
	switch strings.ToLower(os.Args[1]) {
	case "gen":
		cmd.Generate()
	case "search":
		cmd.Search()
	case "bench":
		cmd.Bench()
	case "edit":
		cmd.Edit()
	case "process":
		cmd.Process()
	case "stat":
		cmd.Stats()
	default:
		cmd.PrintHelp()
	}
}
