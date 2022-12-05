// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
//
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/07/14

package org.lionsoul.ip2region.xdb;

import java.util.ArrayList;
import java.util.List;

public class Segment {
    public final long startIP;
    public final long endIP;
    public final String region;

    // parser the Segment from an input string
    public static Segment parse(String input) throws Exception {
        final String[] ps = input.trim().split("\\|", 3);
        if (ps.length != 3) {
            throw new Exception("invalid ip segment `"+input+"`");
        }

        long sip = Util.checkIP(ps[0]);
        long eip = Util.checkIP(ps[1]);
        if (sip > eip) {
            throw new Exception("start ip `"+ps[0]+"` should not be greater than end ip `"+ps[1]+"`");
        }

        return new Segment(sip, eip, ps[2]);
    }

    public Segment(long startIP, long endIP, String region) {
        this.startIP = startIP;
        this.endIP = endIP;
        this.region = region;
    }

    // split the current segment for vector index
    public List<Segment> split() {
        final long sByte1 = (int) ((startIP >> 24) & 0xFF);
        final long eByte1 = (int) ((endIP >> 24) & 0xFF);
        long nSip = startIP;
        final List<Segment> tList =  new ArrayList<Segment>();
        for (long i = sByte1; i <= eByte1; i++) {
            long sip = (i << 24) | (nSip & 0xFFFFFF);
            long eip = (i << 24) | 0xFFFFFF;
            if (eip < endIP) {
                nSip = (i + 1) << 24;
            } else {
                eip = endIP;
            }

            // append the new segment
            tList.add(new Segment(sip, eip, null));
        }

        // 2, split the segments with the second byte
        final List<Segment> segList = new ArrayList<Segment>();
        for (Segment seg : tList) {
            final long base = seg.startIP & 0xFF000000;
            final long sb2 = (seg.startIP >> 16) & 0xFF;
            final long eb2 = (seg.endIP >> 16) & 0xFF;
            long tSip = seg.startIP;
            for (long i = sb2; i <= eb2; i++) {
                long sip = base | (i << 16) | (tSip & 0xFFFF);
                long eip = base | (i << 16) | 0xFFFF;
                if (eip < seg.endIP) {
                    tSip = 0;
                } else {
                    eip = seg.endIP;
                }

                segList.add(new Segment(sip, eip, region));
            }
        }

        return segList;
    }

    @Override public String toString() {
        return Util.long2ip(startIP) + "|" + Util.long2ip(endIP) + "|" + region;
    }
}
