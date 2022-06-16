package main

import (
	"os"
	"github.com/lionsoul2014/ip2region/binding/golang/ip2region"
	"bufio"
	"fmt"
	"strings"
	"errors"
	"time"

)

func main() {

	db := os.Args[1]

	_,err:= os.Stat(db)
	if os.IsNotExist(err){
		panic("not found db " + db)
	}

	region, err := ip2region.New(db)
	defer region.Close()
	fmt.Println(`initializing
+-------------------------------------------------------+
| ip2region test script                                 |
| format 'ip type'                                      |
| type option 'b-tree','binary','memory' default b-tree |
| Type 'quit' to exit program                           |
+-------------------------------------------------------+`)

	reader := bufio.NewReader(os.Stdin)
	for {
		fmt.Print("ip2reginon >> ")
		data, _, _ := reader.ReadLine()
		begin:= time.Now()
		commands := strings.Fields(string(data))
		ip := ip2region.IpInfo{}
		len := len(commands)
		if len == 0{
			continue
		}

		if commands[0] == "quit"{
			break
		}

		if !(len > 1) {
			commands = append(commands, "b-tree")
		}
		switch commands[1] {
		case "b-tree":
			ip, err = region.BtreeSearch(commands[0])
		case "binary":
			ip, err = region.BinarySearch(commands[0])
		case "memory":
			ip, err = region.MemorySearch(commands[0])
		default:
			err = errors.New("parameter error")
		}

		if err != nil {

			fmt.Println( fmt.Sprintf("\x1b[0;31m%s\x1b[0m",err.Error()))
		}else{
			fmt.Println( fmt.Sprintf("\x1b[0;32m%s  %s\x1b[0m",ip.String(),time.Since(begin).String()))
		}
	}
}
