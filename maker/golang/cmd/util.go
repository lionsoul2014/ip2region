package cmd

import (
	"fmt"
	"log/slog"
	"os"
	"regexp"
	"sort"
	"strconv"
	"strings"
)

func PrintHelp() {
	fmt.Printf("ip2region xdb maker\n")
	fmt.Printf("%s [command] [command options]\n", os.Args[0])
	fmt.Printf("Command: \n")
	fmt.Printf("  gen      generate the binary xdb file\n")
	fmt.Printf("  search   binary xdb search test\n")
	fmt.Printf("  bench    binary xdb bench test\n")
	fmt.Printf("  edit     edit the source ip data\n")
	fmt.Printf("  process  process the source ip data\n")
}

// Iterate the cli flags
func iterateFlags(cb func(key string, val string) error) error {
	for i := 2; i < len(os.Args); i++ {
		r := os.Args[i]
		if len(r) < 5 {
			continue
		}

		if strings.Index(r, "--") != 0 {
			continue
		}

		var sIdx = strings.Index(r, "=")
		if sIdx < 0 {
			return fmt.Errorf("missing = for args pair '%s'", r)
		}

		if err := cb(r[2:sIdx], r[sIdx+1:]); err != nil {
			return err
		}
	}

	return nil
}

func applyLogLevel(logLevel string) error {
	// check and apply the log level
	var levelLog = slog.LevelInfo
	switch strings.ToLower(logLevel) {
	case "debug":
		levelLog = slog.LevelDebug
	case "info":
		levelLog = slog.LevelInfo
	case "warn":
		levelLog = slog.LevelWarn
	case "error":
		levelLog = slog.LevelError
	case "":
		// ignore the empty value
		// and default it to LevelInfo
	default:
		return fmt.Errorf("invalid log level %s", logLevel)
	}

	slog.SetLogLoggerLevel(levelLog)
	return nil
}

var pattern = regexp.MustCompile(`^(\d+(-\d+)?)$`)

func getFilterFields(fieldList string) ([]int, error) {
	if len(fieldList) == 0 {
		return []int{}, nil
	}

	var fields []int
	var mapping = make(map[string]string)
	fList := strings.Split(fieldList, ",")
	for _, f := range fList {
		f = strings.TrimSpace(f)
		if len(f) == 0 {
			return nil, fmt.Errorf("empty field index value `%s`", f)
		}

		ms := pattern.FindString(f)
		if len(ms) == 0 {
			return nil, fmt.Errorf("field `%s` is not a number or number range", f)
		}

		// if strings.Index(ms, "-") == -1 {
		if !strings.Contains(ms, "-") {
			if _, ok := mapping[ms]; ok {
				return nil, fmt.Errorf("duplicate option `%s`", f)
			}

			idx, err := strconv.Atoi(ms)
			if err != nil {
				return nil, fmt.Errorf("field index `%s` not an integer", f)
			}

			mapping[ms] = ms
			fields = append(fields, idx)
			continue
		}

		ra := strings.Split(ms, "-")
		if len(ra) != 2 {
			return nil, fmt.Errorf("invalid field index range `%s`", ms)
		}

		start, err := strconv.Atoi(ra[0])
		if err != nil {
			return nil, fmt.Errorf("range start `%s` not an integer", ra[0])
		}

		end, err := strconv.Atoi(ra[1])
		if err != nil {
			return nil, fmt.Errorf("range end `%s` not an integer", ra[1])
		}

		if start > end {
			return nil, fmt.Errorf("index range start(%d) should <= end(%d)", start, end)
		}

		for i := start; i <= end; i++ {
			s := strconv.Itoa(i)
			if _, ok := mapping[s]; ok {
				return nil, fmt.Errorf("duplicate option `%s`", s)
			}

			mapping[s] = s
			fields = append(fields, i)
		}
	}

	// sort the fields
	sort.Ints(fields)
	// fmt.Printf("%+v\n", fields)
	return fields, nil
}
