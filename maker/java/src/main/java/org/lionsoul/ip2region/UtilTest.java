// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
//
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/07/15

package org.lionsoul.ip2region;

import org.lionsoul.ip2region.xdb.IndexPolicy;
import org.lionsoul.ip2region.xdb.Log;
import org.lionsoul.ip2region.xdb.Segment;
import org.lionsoul.ip2region.xdb.Util;

public class UtilTest {

    public static final Log log = Log.getLogger(UtilTest.class);

    public static void testIndexPolicy() {
        final String[] inputs = {"vector", "btree", "VecTor", "BTree", "abc"};
        for (String str : inputs) {
            try {
                int policy = IndexPolicy.parse(str);
                log.infof("parse(%s)=%d", str, policy);
            } catch (Exception e) {
                log.errorf("parse index policy `%s`: %s", str, e.getMessage());
            }
        }
    }

    public static void testIPUtil() {
        final String ipStr = "1.2.3.4";
        try {
            long ip = Util.checkIP(ipStr);
            log.infof("checkIP(%s)=%d, validate: %s, long2ip(%d)=%s", ipStr, ip, ip == 16909060 ? "true" : "false", ip, Util.long2ip(ip));
        } catch (Exception e) {
            log.errorf("failed to check ip `%s`: %s", ipStr, e.getMessage());
        }
    }

    public static void testSegment() {
        final String[] t_segs = {
            "1.1.0.0|1.3.3.24|中国|广东|深圳|电信",
            "0.0.0.0|1.255.225.254|0|0|0|内网IP|内网IP",
            "28.201.224.0|29.34.191.255|美国|0|0|0|0"
        };

        for (String str : t_segs) {
            try {
                final Segment seg = Segment.parse(str);
                log.infof("segment(%s)=%s, split: ", str, seg.toString());
                for (Segment s : seg.split()) {
                    log.debugf(s.toString());
                }
            } catch (Exception e) {
                log.errorf("failed to parse segment `%s`: %s", str, e.getMessage());
            }
        }
    }

    public static void main(String[] args) {
        System.out.println("+-- testing index policy");
        testIndexPolicy();
        System.out.println();

        System.out.println("+-- testing ip util");
        testIPUtil();
        System.out.println();

        System.out.println("+-- testing segments");
        testSegment();
        System.out.println();
    }

}
