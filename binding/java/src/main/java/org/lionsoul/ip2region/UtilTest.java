// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/23

package org.lionsoul.ip2region;

import org.lionsoul.ip2region.xdb.LongByteArray;
import org.lionsoul.ip2region.xdb.Searcher;

import java.util.Arrays;

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

    public static void testLongByteArray() {
        final LongByteArray byteArray = new LongByteArray();
        byteArray.append(new byte[]{0,0,0,0,0});
        byteArray.append(new byte[]{1,1,1,1,1});
        int counter = 2;
        for (int i = 0; i < 100; i++) {
            final byte[] buff = new byte[10];
            Arrays.fill(buff, (byte) counter);
            byteArray.append(buff);
            counter++;
        }

        System.out.printf("1, byteArray.length: %d\n", byteArray.length());
        System.out.println("2, length copy test...");
        int[] length = new int[]{5, 10, 15, 20, 21, 22, 23, 25, 28, 29, 30, 40, 42, 44, 50, 60};
        for (int j : length) {
            final byte[] destBuff = new byte[j];
            byteArray.copy(0, destBuff, 0, destBuff.length);
            System.out.printf("copy(0,%d): \n", destBuff.length);
            for (byte b : destBuff) {
                System.out.print(b + " ");
            }
            System.out.println();
        }

        System.out.println("3, offset copy test...");
        int[] offset = new int[]{0, 5, 10, 15, 20, 21, 22, 23, 25, 28, 29, 30, 40, 42, 44, 50, 60};
        for (int j : offset) {
            final byte[] destBuff = new byte[11];
            byteArray.copy(j, destBuff, 0, destBuff.length);
            System.out.printf("copy(%d,%d): \n", j, destBuff.length);
            for (byte b : destBuff) {
                System.out.print(b + " ");
            }
            System.out.println();
        }
    }

    public static void main(String[] args) {
        System.out.print("testing IP2Long ... \n");
        testIP2Long();
        System.out.print("testing LongByteArray ... \n");
        testLongByteArray();
    }

}
