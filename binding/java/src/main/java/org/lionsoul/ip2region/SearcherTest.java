// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/23

package org.lionsoul.ip2region;

import org.lionsoul.ip2region.service.Config;
import org.lionsoul.ip2region.service.InvalidConfigException;
import org.lionsoul.ip2region.service.Ip2Region;
import org.lionsoul.ip2region.xdb.InetAddressException;
import org.lionsoul.ip2region.xdb.XdbException;
import org.lionsoul.ip2region.xdb.LongByteArray;
import org.lionsoul.ip2region.xdb.Searcher;
import org.lionsoul.ip2region.xdb.Util;
import org.lionsoul.ip2region.xdb.Version;

import java.io.*;
import java.nio.charset.Charset;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.CodeSource;
import java.util.concurrent.TimeUnit;

public class SearcherTest {

    public static void printHelp(String[] args) {
        System.out.print("ip2region xdb searcher\n");
        System.out.print("java -jar ip2region-{version}.jar [command] [command options]\n");
        System.out.print("Command: \n");
        System.out.print("  search    search input test\n");
        System.out.print("  bench     search bench test\n");
    }

    public static final String getXdbPath(String fileName) throws IOException {
        String xdbPath;
        final CodeSource cs = SearcherTest.class.getProtectionDomain().getCodeSource();
        if (cs != null) {
            // log.debugf("code path: %s", cs.getLocation().getPath().concat("../../../../data/"));
            final Path jarPath = Paths.get(cs.getLocation().getPath());
            xdbPath = jarPath.getParent().toString().concat("/../../../data/").concat(fileName);
        } else {
            xdbPath = "../../../data/".concat(fileName);
        }

        final File xdbFile = new File(xdbPath);
        return xdbFile.exists() ? xdbFile.getCanonicalPath() : "";
    }

    public static final Ip2Region createService(
        String v4XdbPath, String v4CachePolicy, String v6XdbPath, String v6CachePolicy) throws IOException, XdbException, InvalidConfigException {
        final Config v4Config = Config.custom()
            .setCachePolicy(Config.cachePolicyFromName(v4CachePolicy))
            .setSearchers(1)
            .setXdbPath(v4XdbPath).asV4();

        final Config v6Config = Config.custom()
            .setCachePolicy(Config.cachePolicyFromName(v6CachePolicy))
            .setSearchers(1)
            .setXdbPath(v6XdbPath).asV6();

        return Ip2Region.create(v4Config, v6Config);
    }

    public static Searcher createSearcher(String dbPath, String cachePolicy) throws IOException, XdbException {
        final RandomAccessFile handle = new RandomAccessFile(dbPath, "r");

        // verify the xdb file
        // @Note: do NOT call it every time you create a searcher since this will slow
        // down the search response.
        // @see the util.Verify function for details.
        Searcher.verify(handle);

        // get the ip version from header
        final Version version = Version.fromHeader(Searcher.loadHeader(handle));

        // create the final searcher
        if ("file".equals(cachePolicy)) {
            return Searcher.newWithFileOnly(version, dbPath);
        } else if ("vectorIndex".equals(cachePolicy)) {
            final byte[] vIndex = Searcher.loadVectorIndexFromFile(dbPath);
            return Searcher.newWithVectorIndex(version, dbPath, vIndex);
        } else if ("content".equals(cachePolicy)) {
            final LongByteArray cBuff = Searcher.loadContentFromFile(dbPath);
            return Searcher.newWithBuffer(version, cBuff);
        } else {
            throw new IOException("invalid cache policy `" + cachePolicy + "`, options: file/vectorIndex/content");
        }
    }

    public static void searchTest(String[] args) throws IOException, XdbException, InterruptedException, InvalidConfigException {
        String help = "";
        String v4DbPath = "", v4CachePolicy = "vectorIndex";
        String v6DbPath = "", v6CachePolicy = "vectorIndex";
        for (final String r : args) {
            if (r.length() < 5) {
                continue;
            }

            if (r.indexOf("--") != 0) {
                continue;
            }

            String key = "", val = "";
            int sIdx = r.indexOf('=');
            if (sIdx < 0) {
                key = r.substring(2);
                // System.out.printf("missing = for args pair `%s`\n", r);
                // return;
            } else {
                key = r.substring(2, sIdx);
                val = r.substring(sIdx + 1);
            }

            // System.out.printf("key=%s, val=%s\n", key, val);
            if ("help".equals(key)) {
                help = val == "" ? "true" : val;
            } else if ("v4-db".equals(key)) {
                v4DbPath = val;
            } else if ("v4-cache-policy".equals(key)) {
                v4CachePolicy = val;
            } else if ("v6-db".equals(key)) {
                v6DbPath = val;
            } else if ("v6-cache-policy".equals(key)) {
                v6CachePolicy = val;
            } else {
                System.out.printf("undefined option `%s`\n", r);
                return;
            }
        }

        
        // check and set the default path for v4
        if (v4DbPath.isEmpty()) {
            v4DbPath = getXdbPath("ip2region_v4.xdb");
        }

        // check and set the default path for 6
        if (v6DbPath.isEmpty()) {
            v6DbPath = getXdbPath("ip2region_v6.xdb");
        }

        if (v4DbPath.isEmpty() || v6DbPath.isEmpty() || help.equals("true")) {
            System.out.print("java -jar ip2region-{version}.jar search [command options]\n");
            System.out.print("options:\n");
            System.out.print(" --v4-db string            ip2region ipv4 binary xdb file path\n");
            System.out.print(" --v4-cache-policy string  v4 cache policy, default vectorIndex, options: file/vectorIndex/content\n");
            System.out.print(" --v6-db string            ip2region ipv6 binary xdb file path\n");
            System.out.print(" --v6-cache-policy string  v6 cache policy, default vectorIndex, options: file/vectorIndex/content\n");
            System.out.print(" --help                    print this help menu\n");
            return;
        }

        final Ip2Region ip2region = createService(v4DbPath, v4CachePolicy, v6DbPath, v6CachePolicy);
        final BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        System.out.printf("ip2region search service test program\n" 
+ "+-v4 xdb: %s (%s)\n"
+ "+-v6 xdb: %s (%s)\n"
+ "type 'quit' to exit\n", v4DbPath, v4CachePolicy, v6DbPath, v6CachePolicy);
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
                String region = ip2region.search(line);
                long cost = TimeUnit.NANOSECONDS.toMicros((long) (System.nanoTime() - sTime));
                System.out.printf("{region: %s, took: %d μs}\n", region, cost);
            } catch (Exception e) {
                System.out.printf("{region: , err: %s}\n", e);
            }
        }

        reader.close();
        ip2region.close();
        System.out.println("ip2region test program exited, thanks for trying");
    }

    public static void benchTest(String[] args) throws IOException, XdbException, InetAddressException {
        String dbPath = "", srcPath = "", cachePolicy = "vectorIndex";
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
            if ("db".equals(key)) {
                dbPath = val;
            } else if ("src".equals(key)) {
                srcPath = val;
            } else if ("cache-policy".equals(key)) {
                cachePolicy = val;
            } else {
                System.out.printf("undefined option `%s`\n", r);
                return;
            }
        }

        if (dbPath.length() < 1 || srcPath.length() < 1) {
            System.out.print("java -jar ip2region-{version}.jar bench [command options]\n");
            System.out.print("options:\n");
            System.out.print(" --db string              ip2region binary xdb file path\n");
            System.out.print(" --src string             source ip text file path\n");
            System.out.print(" --cache-policy string    cache policy: file/vectorIndex/content\n");
            return;
        }

        Searcher searcher = createSearcher(dbPath, cachePolicy);
        long count = 0, costs = 0, tStart = System.nanoTime();
        String line;
        final Charset charset = Charset.forName("utf-8");
        final FileInputStream fis = new FileInputStream(srcPath);
        final BufferedReader reader = new BufferedReader(new InputStreamReader(fis, charset));
        while ((line = reader.readLine()) != null) {
            String l = line.trim();
            String[] ps = l.split("\\|", 3);
            if (ps.length != 3) {
                reader.close();
                System.out.printf("invalid ip segment `%s`\n", l);
                return;
            }

            // mark the start time
            long sTime = System.nanoTime();

            byte[] sip;
            try {
                sip = Util.parseIP(ps[0]);
            } catch (Exception e) {
                reader.close();
                System.out.printf("check start ip `%s`: %s\n", ps[0], e);
                return;
            }

            byte[] eip;
            try {
                eip = Util.parseIP(ps[1]);
            } catch (Exception e) {
                reader.close();
                System.out.printf("check end ip `%s`: %s\n", ps[1], e);
                return;
            }

            if (Util.ipCompare(sip, eip) > 0) {
                reader.close();
                System.out.printf("start ip(%s) should not be greater than end ip(%s)\n", ps[0], ps[1]);
                return;
            }

            for (final byte[] ip : new byte[][]{sip, eip}) {
                String region = searcher.search(ip);

                // check the region info
                if (!ps[2].equals(region)) {
                    System.out.printf("failed search(%s) with (%s != %s)\n", Util.ipToString(ip), region, ps[2]);
                    reader.close();
                    return;
                }

                count++;
            }

            costs += System.nanoTime() - sTime;
        }

        reader.close();
        searcher.close();
        long took = System.nanoTime() - tStart;
        System.out.printf("Bench finished, {cachePolicy: %s, total: %d, took: %ds, cost: %d μs/op}\n",
                cachePolicy, count, TimeUnit.NANOSECONDS.toSeconds(took),
                count == 0 ? 0 : TimeUnit.NANOSECONDS.toMicros(costs/count));
    }

    public static void main(String[] args) {
        if (args.length < 1) {
            printHelp(args);
            return;
        }

        if ("search".equals(args[0])) {
            try {
                searchTest(args);
            } catch (Exception e) {
                System.out.printf("failed running search test: %s\n", e);
            }
        } else if ("bench".equals(args[0])) {
            try {
                benchTest(args);
            } catch (Exception e) {
                System.out.printf("fwailed running bench test: %s\n", e);
            }
        } else {
            printHelp(args);
        }
    }

}
