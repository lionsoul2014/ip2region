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

import java.util.regex.Matcher;

public class UtilTest {

    public static final Log log = Log.getLogger(UtilTest.class);

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

    public static void testFieldFilter() {
        final String reg = "0|16777215|保留|保留|保留|保留|||||||||||";
        final String[] fs = reg.split("\\|", -1);
        log.infof("length: %d", fs.length);
        for (String s : fs) {
            log.infof("%s", s);
        }
    }

    public static void testIndexParse() {
        final String[] ss = new String[]{"1", "2-3", "23", "3-", "x-", "3-x", "4x"};
        for (String s : ss) {
            log.infof("parse: %s", s);
            final Matcher m = MakerTest.p.matcher(s);
            if (m.matches()) {
                for (int i = 0; i < m.groupCount(); i++) {
                    log.infof("match: %d/%d, str: %s", i, m.groupCount(), m.group(i));
                }
            } else {
                log.infof("no match for: %s", s);
            }
        }
    }

    public static void main(String[] args) {
        System.out.println("+-- testing segments");
        testSegment();
        System.out.println();

        System.out.println("+-- testing field filter");
        testFieldFilter();
        System.out.println();

        System.out.println("+-- testing index parse");
        testIndexParse();
        System.out.println();
    }

}
