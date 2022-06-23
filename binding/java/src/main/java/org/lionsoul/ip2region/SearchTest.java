// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/23

package org.lionsoul.ip2region;

import org.lionsoul.ip2region.xdb.Searcher;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.concurrent.TimeUnit;

public class SearchTest {

    public static void printHelp(String[] args) {
        System.out.print("ip2region xdb searcher\n");
        System.out.print("java -jar ip2region-{version}.jar [command] [command options]\n");
        System.out.print("Command: \n");
        System.out.print("  search    search input test\n");
        System.out.print("  bench     search bench test\n");
    }

    public static Searcher createSearcher(String dbPath, String cachePolicy) throws IOException {
        if ("file".equals(cachePolicy)) {
            return Searcher.newWithFileOnly(dbPath);
        } else if ("vectorIndex".equals(cachePolicy)) {
            byte[] vIndex = Searcher.loadVectorIndexFromFile(dbPath);
            return Searcher.newWithVectorIndex(dbPath, vIndex);
        } else if ("content".equals(cachePolicy)) {
            byte[] cBuff = Searcher.loadContentFromFile(dbPath);
            return Searcher.newWithBuffer(cBuff);
        } else {
            throw new IOException("invalid cache policy `" + cachePolicy + "`, options: file/vectorIndex/content");
        }
    }

    public static void searchTest(String[] args) throws IOException {
        String dbPath = "", cachePolicy = "vectorIndex";
        for (final String r : args) {
            if (r.length() < 5) {
                continue;
            }

            if (r.indexOf("--") != 0) {
                continue;
            }

            int sIdx = r.indexOf('=');
            if (sIdx < 0) {
                System.out.printf("missing = for args pair `%s`\n", r);
                return;
            }

            String key = r.substring(2, sIdx);
            String val = r.substring(sIdx + 1);
            System.out.printf("key=%s, val=%s\n", key, val);
            if ("db".equals(key)) {
                dbPath = val;
            } else if ("cache-policy".equals(key)) {
                cachePolicy = val;
            } else {
                System.out.printf("undefined option `%s`", r);
                return;
            }
        }

        if (dbPath.length() < 1) {
            System.out.print("java -jar ip2region-{version}.jar search [command options]\n");
            System.out.print("options:\n");
            System.out.print(" --db string              ip2region binary xdb file path\n");
            System.out.print(" --cache-policy string    cache policy: file/vectorIndex/content\n");
            return;
        }

        Searcher searcher = createSearcher(dbPath, cachePolicy);
        final BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        System.out.printf("ip2region xdb searcher test program, cachePolicy: %s\ntype 'quit' to exit\n", cachePolicy);
        while ( true ) {
            System.out.print("ip2region>> ");
            String line = reader.readLine().trim();
            if ( line.length() < 2 ) {
                continue;
            }

            if ( line.equalsIgnoreCase("quit") ) {
                break;
            }

            try {
                double sTime = System.nanoTime();
                String region = searcher.searchByStr(line);
                long cost = TimeUnit.NANOSECONDS.toMicros((long) (System.nanoTime() - sTime));
                System.out.printf("{region: %s, ioCount: %d, took: %d μs}\n", region, searcher.getIOCount(), cost);
            } catch (Exception e) {
                System.out.printf("{err: %s, ioCount: %d}\n", e, searcher.getIOCount());
            }
        }

        reader.close();
        searcher.close();
        System.out.println("searcher test program exited, thanks for trying");
    }

    public static void benchTest(String[] args) {

    }

    public static void main(String[] args) {
        if (args.length < 1) {
            printHelp(args);
            return;
        }

        if ("search".equals(args[0])) {
            try {
                searchTest(args);
            } catch (IOException e) {
                System.out.printf("failed running search test: %s", e);
            }
        } else if ("bench".equals(args[0])) {
            benchTest(args);
        } else {
            printHelp(args);
        }
    }

}
