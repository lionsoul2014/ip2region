// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package xdb

import (
	"fmt"
	"strings"
)

type IndexPolicy int

const (
	VectorIndexPolicy IndexPolicy = 1
	BTreeIndexPolicy  IndexPolicy = 2
)

func IndexPolicyFromString(str string) (IndexPolicy, error) {
	switch strings.ToLower(str) {
	case "vector":
		return VectorIndexPolicy, nil
	case "btree":
		return BTreeIndexPolicy, nil
	default:
		return VectorIndexPolicy, fmt.Errorf("invalid policy '%s'", str)
	}
}
