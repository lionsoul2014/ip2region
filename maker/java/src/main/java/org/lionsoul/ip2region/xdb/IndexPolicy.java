// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
//
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/07/14

package org.lionsoul.ip2region.xdb;

public class IndexPolicy {
    public static final int Vector = 1;
    public static final int BTree = 2;

    // parser the index policy from string
    public static int parse(String policy) throws Exception {
        String v = policy.toLowerCase();
        if ("vector".equals(v)) {
            return Vector;
        } else if ("btree".equals(v)) {
            return BTree;
        } else {
            throw new Exception("unknown index policy `"+policy+"`");
        }
    }
}