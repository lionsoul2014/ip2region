// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
//
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/07/12

package org.lionsoul.ip2region;

import org.lionsoul.ip2region.xdb.IndexPolicy;
import org.lionsoul.ip2region.xdb.Log;
import org.lionsoul.ip2region.xdb.Maker;

public class MakerTest {

    public final static Log log = Log.getLogger(MakerTest.class);

    public static void printHelp(String[] args) {
        System.out.println("ip2region xdb maker");
        System.out.println("java -jar ip2region-maker-{version}.jar [command options]");
        System.out.println("options:");
        System.out.println(" --src string        source ip text file path");
        System.out.println(" --dst string        destination binary xdb file path");
        System.out.println(" --log-level string  set the log level, options: debug/info/warn/error");
    }

    public static void genDb(String[] args) throws Exception {
        String srcFile = "", dstFile = "", logLevel = "info";
        int indexPolicy = IndexPolicy.Vector;
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
            // System.out.printf("key=%s, val=%s\n", key, val);
            if ("src".equals(key)) {
                srcFile = val;
            } else if ("dst".equals(key)) {
                dstFile = val;
            } else if ("log-level".equals(key)) {
                logLevel = val;
            } else {
                System.out.printf("undefined option `%s`\n", r);
                return;
            }
        }

        if (srcFile.isEmpty() || dstFile.isEmpty()) {
            printHelp(args);
            return;
        }

        long tStart = System.currentTimeMillis();
        final Maker maker = new Maker(indexPolicy, srcFile, dstFile);
        log.infof("Generating xdb with src=%s, dst=%s, logLevel=%s\n", srcFile, dstFile, logLevel);
        Maker.log.setLevel(logLevel);
        maker.init();
        maker.start();
        maker.end();

        log.infof("Done, elapsed: %d s", (System.currentTimeMillis() - tStart) / 1000);
    }

    public static void main(String[] args) {
        if (args.length < 1) {
            printHelp(args);
            return;
        }

        try {
            genDb(args);
        } catch (Exception e) {
            System.out.printf("failed running genDb: %s\n", e);
        }
    }

}
