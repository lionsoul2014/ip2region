// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package main

import (
	"fmt"
	"log"
	"os"
	"strings"

	"github.com/lionsoul2014/ip2region/maker/golang/cmd"
)

func printHelp() {
	fmt.Printf("ip2region xdb maker\n")
	fmt.Printf("%s [command] [command options]\n", os.Args[0])
	fmt.Printf("Command: \n")
	fmt.Printf("  gen      generate the binary xdb file\n")
	fmt.Printf("  search   binary xdb search test\n")
	fmt.Printf("  bench    binary xdb bench test\n")
	fmt.Printf("  edit     edit the source ip data\n")
	fmt.Printf("  process  process the source ip data\n")
}

func main() {
	if len(os.Args) < 2 {
		printHelp()
		return
	}

	log.SetFlags(log.Ldate | log.Ltime | log.Lshortfile)
	switch sCmd := strings.ToLower(os.Args[1]); sCmd {
	case "gen":
		cmd.Generate(sCmd)
	case "search":
		cmd.Search(sCmd)
	case "bench":
		cmd.Bench(sCmd)
	case "edit":
		cmd.Edit(sCmd)
	case "process":
		cmd.Process(sCmd)
	default:
		printHelp()
	}
}
