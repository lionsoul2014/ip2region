// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/23

package org.lionsoul.ip2region;

import org.lionsoul.ip2region.xdb.Searcher;

public class UtilTest {

    public static void testIP2Long() {
        String ip = "1.2.3.4";
        long ipAddr = 0;
        try {
            ipAddr = Searcher.checkIP(ip);
        } catch (Exception e) {
            System.out.printf("failed to check ip: %s\n", e);
            return;
        }

        if (ipAddr != 16909060) {
            System.out.print("failed ip2long\n");
            return;
        }

        String ip2 = Searcher.long2ip(ipAddr);
        if (!ip.equals(ip2)) {
            System.out.print("failed long2ip\n");
            return;
        }

        System.out.printf("passed: ip=%s, ipAddr=%d, ip2=%s\n", ip, ipAddr, ip2);
    }

    public static void main(String[] args) {
        System.out.print("testing IP2Long ... \n");
        testIP2Long();
    }

}
