// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

package org.lionsoul.ip2region.xdb;

// xdb searcher (Not thread safe implementation)
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/23


import lombok.Builder;
import lombok.Data;

/**
 * @see FileSearcher
 * @see VectorIndexSearcher
 * @see BufferSearcher
 */
public interface Searcher extends AutoCloseable {

    // FileSearcher
    static Searcher newWithFileOnly(String dbPath) {
        return new FileSearcher(dbPath);
    }

    // IndexSearcher
    static Searcher newWithVectorIndex(String dbPath) {
        return new VectorIndexSearcher(dbPath);
    }

    // IndexSearcher
    static Searcher newWithVectorIndex(String dbPath, byte[] vectorIndex) {
        return new VectorIndexSearcher(dbPath, vectorIndex);
    }

    // BufferSearcher
    static Searcher newWithBuffer(String dbPath) {
        return new BufferSearcher(dbPath);
    }

    // BufferSearcher
    static Searcher newWithBuffer(byte[] cBuff) {
        return new BufferSearcher(cBuff);
    }

    String search(String ip);

    Region searchRegion(String ip);

    @Data
    @Builder
    class Region {
        private int ioCount;
        private String region;

        public RegionMsg toRegionMsg() {
            if (region == null) {
                return null;
            }

            final String[] ss = region.split("\\|");
            return RegionMsg.builder()
                    .country(ss[0])
                    .region(ss[1])
                    .province(ss[2])
                    .city(ss[3])
                    .isp(ss[4])
                    .build();
        }
    }

    // Country|Region|Province|City|ISP
    // 国家|区域|省份|城市|ISP
    @Data
    @Builder
    class RegionMsg {
        private String country;
        private String region;
        private String province;
        private String city;
        private String isp;
    }
}