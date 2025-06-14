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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public class MakerTest {

    public final static Log log = Log.getLogger(MakerTest.class);

    public static void printHelp(String[] args) {
        System.out.println("ip2region xdb maker");
        System.out.println("java -jar ip2region-maker-{version}.jar [command options]");
        System.out.println("options:");
        System.out.println(" --src string        source ip text file path");
        System.out.println(" --dst string        destination binary xdb file path");
        System.out.println(" --field-list string field index list imploded with ',' eg: 0,1,2,3,4");
        System.out.println(" --log-level string  set the log level, options: debug/info/warn/error");
    }

    private static int[] getFieldList(String fieldList) {
        final ArrayList<Integer> list = new ArrayList<Integer>();
        final Map<String, String> map = new HashMap<String, String>();
        if (!fieldList.isEmpty()) {
            String[] fList =  fieldList.split(",");
            for (String f : fList) {
                final String s = f.trim();
                if (s.isEmpty()) {
                    log.infof("undefined option `%s`", f);
                    return null;
                }

                if (!s.matches("^\\d+$")) {
                    log.infof("field `%s` is not a number", f);
                    return null;
                }

                if (map.containsKey(s)) {
                    log.infof("duplicate field index `%s`", s);
                    return null;
                }

                map.put(s, s);
                final int idx = Integer.parseInt(s);
                if (idx < 0) {
                    log.infof("field index `%s` is negative", s);
                    return null;
                }

                list.add(idx);
            }
        }

        // let's keep it a complex way so the old JDK could run these pieces.
        final int[] fields = new int[list.size()];
        for (int i = 0; i < list.size(); i++) {
            fields[i] = list.get(i);
        }

        return fields;
    }

    public static void genDb(String[] args) throws Exception {
        String srcFile = "", dstFile = "";
        String fieldList = "", logLevel = "info";
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
            } else if ("field-list".equals(key)) {
                    fieldList = val;
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

        final int[] fields = getFieldList(fieldList);
        if (fields == null) {
            return;
        }

        // check and make the field list
        long tStart = System.currentTimeMillis();
        final Maker maker = new Maker(indexPolicy, srcFile, dstFile, fields);
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
